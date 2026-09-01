# Verifying the Bayesian toolchain by fitting a bayesnec model is a poor test.
# It is slow, it exercises a dozen models at once, and when it fails the error
# arrives from deep inside Stan with nothing to say which layer is broken. The
# checks below are the ones the cmdstanr and rstan installation pages recommend,
# run in the order the layers stack, so a failure names the layer that failed:
#
#   1 packages   the R packages are installed
#   2 toolchain  a C++ compiler and make are present and usable
#   3 cmdstan    CmdStan itself is installed and its version readable
#   4 stan       a minimal Stan model compiles and samples
#   5 brms       brms can translate, compile and fit a model
#
# Only when all five pass is a failed bayesnec fit a statement about the model
# rather than about the installation.

#' Check the Stan toolchain before fitting
#'
#' Runs the installation checks that the `cmdstanr` and `rstan` documentation
#' recommend, in the order the layers depend on each other, and reports which
#' layer fails. This is a better first test than fitting a `bayesnec` model: it
#' is faster, and it says which part of the installation is at fault instead of
#' failing somewhere inside Stan.
#'
#' The last two stages compile a Stan model, which takes some tens of seconds
#' the first time. That compilation is the point of the check: a toolchain can
#' look correctly configured and still fail to build a model.
#'
#' @param backend Which Stan interface to check, `"cmdstanr"` or `"rstan"`.
#' @param stages Which stages to run. Later stages are skipped automatically
#'   once an earlier one has failed, because their result would not be
#'   interpretable.
#' @param quiet Whether to suppress compiler and sampler output.
#' @return An object of class `cr_stan_check`, a data frame with one row per
#'   stage and columns `stage`, `status` (`"pass"`, `"fail"` or `"skip"`),
#'   `detail` and `seconds`.
#' @export
#' @examples
#' \donttest{
#' check_stan_toolchain()
#' check_stan_toolchain("rstan", stages = c("packages", "toolchain"))
#' }
check_stan_toolchain <- function(backend = c("cmdstanr", "rstan"),
                                 stages = c("packages", "toolchain", "cmdstan",
                                            "stan", "brms"),
                                 quiet = TRUE) {
  backend <- match.arg(backend)
  stages <- match.arg(stages, several.ok = TRUE)
  # The CmdStan installation stage has no meaning for rstan, which carries its
  # own copy of the Stan library.
  if (backend == "rstan") stages <- setdiff(stages, "cmdstan")

  rows <- list()
  failed <- FALSE
  for (s in stages) {
    if (failed) {
      rows[[s]] <- stan_row(s, "skip", "Skipped: an earlier stage failed.", 0)
      next
    }
    t0 <- Sys.time()
    res <- switch(s,
      packages = stan_stage_packages(backend),
      toolchain = stan_stage_toolchain(backend),
      cmdstan = stan_stage_cmdstan(),
      stan = stan_stage_stan(backend, quiet),
      brms = stan_stage_brms(backend, quiet)
    )
    rows[[s]] <- stan_row(
      s, res$status, res$detail,
      as.numeric(difftime(Sys.time(), t0, units = "secs"))
    )
    if (res$status == "fail") failed <- TRUE
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  structure(out, class = c("cr_stan_check", "data.frame"), backend = backend)
}

stan_row <- function(stage, status, detail, seconds) {
  # Messages from the Stan tooling arrive with embedded newlines, which would
  # break the aligned one-line-per-stage report.
  detail <- gsub("[[:space:]]+", " ", trimws(detail))
  data.frame(
    stage = stage, status = status, detail = detail,
    seconds = round(seconds, 1), stringsAsFactors = FALSE
  )
}

stan_stage_packages <- function(backend) {
  need <- c("brms", backend)
  have <- vapply(need, requireNamespace, logical(1), quietly = TRUE)
  if (all(have)) {
    vers <- vapply(need, function(p) as.character(utils::packageVersion(p)), character(1))
    list(status = "pass", detail = paste(need, vers, collapse = ", "))
  } else {
    list(
      status = "fail",
      detail = paste0(
        "Not installed: ", paste(need[!have], collapse = ", "),
        if ("cmdstanr" %in% need[!have]) {
          ". cmdstanr is not on CRAN: install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))"
        } else {
          ""
        }
      )
    )
  }
}

stan_stage_toolchain <- function(backend) {
  if (backend == "cmdstanr") {
    # check_cmdstan_toolchain() reports through messages and throws on a
    # toolchain it cannot use, so both are captured.
    msg <- character(0)
    ok <- tryCatch(
      withCallingHandlers(
        {
          cmdstanr::check_cmdstan_toolchain(fix = FALSE, quiet = TRUE)
          TRUE
        },
        message = function(m) {
          msg <<- c(msg, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) e
    )
    if (isTRUE(ok)) {
      return(list(status = "pass", detail = "cmdstanr reports a usable toolchain."))
    }
    return(list(
      status = "fail",
      detail = paste0(
        if (inherits(ok, "error")) conditionMessage(ok) else "Toolchain not usable.",
        " Try cmdstanr::check_cmdstan_toolchain(fix = TRUE)."
      )
    ))
  }

  if (!requireNamespace("pkgbuild", quietly = TRUE)) {
    return(list(status = "fail", detail = "Package 'pkgbuild' is needed for the rstan toolchain check."))
  }
  # has_build_tools() prints its own warning when tools are missing, which is
  # duplicated by the detail below. It can also throw outright. A diagnostic
  # whose purpose is to report a broken toolchain must not itself fail on one,
  # so the verdict is carried out of the tryCatch rather than read afterwards
  # from a variable that the failing branch never assigns.
  ok <- tryCatch(
    {
      suppressWarnings(utils::capture.output(
        val <- pkgbuild::has_build_tools(debug = FALSE),
        type = "message"
      ))
      isTRUE(val)
    },
    error = function(e) FALSE
  )
  if (isTRUE(ok)) {
    list(status = "pass", detail = "pkgbuild reports build tools available.")
  } else {
    list(
      status = "fail",
      detail = paste(
        "No usable build tools. On Windows install the Rtools matching this R",
        "version; an earlier Rtools will not substitute. Diagnose with",
        "pkgbuild::has_build_tools(debug = TRUE)."
      )
    )
  }
}

stan_stage_cmdstan <- function() {
  path <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) NULL)
  if (is.null(path) || !dir.exists(path)) {
    return(list(
      status = "fail",
      detail = "CmdStan not found. Install it with cmdstanr::install_cmdstan()."
    ))
  }
  ver <- tryCatch(as.character(cmdstanr::cmdstan_version()), error = function(e) NA_character_)
  if (is.na(ver)) {
    return(list(
      status = "fail",
      detail = paste0("CmdStan at ", path, " but its version could not be read.")
    ))
  }
  list(status = "pass", detail = paste0("CmdStan ", ver, " at ", path))
}

# The minimal model is the bernoulli example the Stan projects ship for exactly
# this purpose. Compiling and sampling it is what separates a toolchain that is
# configured from one that actually works.
stan_stage_stan <- function(backend, quiet) {
  code <- paste(
    "data { int<lower=0> N; array[N] int<lower=0,upper=1> y; }",
    "parameters { real<lower=0,upper=1> theta; }",
    "model { y ~ bernoulli(theta); }",
    sep = "\n"
  )
  dat <- list(N = 10L, y = c(0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L))

  res <- tryCatch(
    stan_quietly(quiet, {
      if (backend == "cmdstanr") {
        f <- file.path(cmdstanr::cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
        if (!file.exists(f)) {
          f <- tempfile(fileext = ".stan")
          writeLines(code, f)
        }
        mod <- cmdstanr::cmdstan_model(f)
        fit <- mod$sample(
          data = dat, chains = 1, iter_warmup = 200, iter_sampling = 200,
          refresh = 0, show_messages = FALSE, show_exceptions = FALSE
        )
        as.numeric(fit$summary("theta")$mean)
      } else {
        mod <- rstan::stan_model(model_code = code)
        fit <- rstan::sampling(mod,
          data = dat, chains = 1, iter = 400, refresh = 0
        )
        as.numeric(rstan::summary(fit)$summary["theta", "mean"])
      }
    }),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    return(list(
      status = "fail",
      detail = paste0("A minimal Stan model failed to compile or sample: ",
        stan_first_line(conditionMessage(res)))
    ))
  }
  if (!is.finite(res)) {
    return(list(status = "fail", detail = "The model sampled but returned no finite estimate."))
  }
  list(
    status = "pass",
    detail = sprintf("A minimal Stan model compiled and sampled (theta = %.2f).", res)
  )
}

# brms sits on top of Stan and does its own translation, so it can fail where a
# hand-written model succeeds. This is the last layer below bayesnec.
stan_stage_brms <- function(backend, quiet) {
  d <- data.frame(y = c(2.1, 1.8, 2.6, 2.2, 1.9, 2.4, 2.0, 2.3, 1.7, 2.5))
  res <- tryCatch(
    stan_quietly(quiet, {
      fit <- brms::brm(y ~ 1,
        data = d, backend = backend, chains = 1,
        iter = 400, warmup = 200, refresh = 0, silent = 2
      )
      as.numeric(brms::fixef(fit)[1, "Estimate"])
    }),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    return(list(
      status = "fail",
      detail = paste0("brms could not fit a one-parameter model: ",
        stan_first_line(conditionMessage(res)))
    ))
  }
  list(
    status = "pass",
    detail = sprintf("brms compiled and fitted a model (intercept = %.2f).", res)
  )
}

stan_quietly <- function(quiet, expr) {
  if (!quiet) {
    return(force(expr))
  }
  utils::capture.output(
    suppressMessages(suppressWarnings(val <- force(expr))),
    type = "output"
  )
  val
}

stan_first_line <- function(x) {
  lines <- trimws(strsplit(x, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  if (!length(lines)) {
    return("no message")
  }
  paste(utils::head(lines, 3), collapse = " | ")
}

#' @export
print.cr_stan_check <- function(x, ...) {
  cat("Stan toolchain check (", attr(x, "backend"), ")\n", sep = "")
  mark <- c(pass = "PASS", fail = "FAIL", skip = "skip")
  for (i in seq_len(nrow(x))) {
    cat(sprintf(
      "  %-4s %-10s %5.1fs  %s\n",
      mark[[x$status[i]]], x$stage[i], x$seconds[i], x$detail[i]
    ))
  }
  if (any(x$status == "fail")) {
    cat("\n  The first failing stage is the one to fix; later stages were skipped.\n")
  } else if (all(x$status == "pass")) {
    cat("\n  All stages passed. A bayesnec fit that now fails is a problem with the\n")
    cat("  model or the data, not with the installation.\n")
  }
  invisible(x)
}
