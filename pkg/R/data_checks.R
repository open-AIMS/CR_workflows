#' Validate a concentration-response dataset against its test type
#'
#' Applies the checks a laboratory analyst would otherwise make by eye before
#' fitting: that the expected columns are present, that concentrations and
#' responses fall within admissible ranges for the response type, that a control
#' is present, that replication is adequate, and that the response declines with
#' concentration over the tested range. Problems that make a fit impossible are
#' raised as errors. Problems that only threaten reliability are returned in the
#' result so that the analyst can record and override them, rather than being
#' silently corrected.
#'
#' @param data A data frame holding one test.
#' @param test_type Test-type identifier, one of `cr_test_types()$id`.
#' @param min_conc_levels Minimum number of distinct non-control concentrations
#'   below which an issue is recorded. Five is the usual guideline minimum for
#'   regression-based estimation.
#' @param min_reps Minimum replicates per concentration below which an issue is
#'   recorded.
#' @return An object of class `cr_check`: a list with elements `test_type`,
#'   `label`, `n_rows`, `n_conc`, `issues` and `design`.
#' @export
#' @examples
#' data(algal_growth)
#' check_cr_data(algal_growth, "algal_growth")
check_cr_data <- function(data, test_type, min_conc_levels = 5, min_reps = 3) {
  tt <- cr_test_type(test_type)
  issues <- character(0)

  needed <- c(tt$x_var, tt$y_var, if (!is.na(tt$trials_var)) tt$trials_var)
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols)) {
    stop("Dataset is missing required column(s) for test type '", test_type,
      "': ", paste(missing_cols, collapse = ", "),
      ". Expected: ", paste(needed, collapse = ", "),
      call. = FALSE
    )
  }

  x <- data[[tt$x_var]]
  y <- data[[tt$y_var]]
  if (!is.numeric(x)) {
    stop("Concentration column '", tt$x_var, "' must be numeric.", call. = FALSE)
  }
  if (!is.numeric(y)) {
    stop("Response column '", tt$y_var, "' must be numeric.", call. = FALSE)
  }
  if (anyNA(x)) {
    stop("Concentration column contains missing values.", call. = FALSE)
  }
  if (any(x < 0)) {
    stop("Concentration column contains negative values.", call. = FALSE)
  }
  if (anyNA(y)) {
    issues <- c(issues, "Response column contains missing values; these rows will be dropped by the fitting engine.")
  }

  if (!any(x == 0)) {
    issues <- c(issues, "No zero-concentration control rows found. Confirm that the control was recorded and that a solvent control has not been coded as a low concentration.")
  }

  issues <- c(issues, check_response_range(y, data, tt))

  n_conc <- length(unique(x))
  n_exposed <- n_conc - as.integer(any(x == 0))
  if (n_exposed < min_conc_levels) {
    issues <- c(issues, sprintf(
      "Only %d non-control concentrations. Regression-based estimates are poorly determined below %d.",
      n_exposed, min_conc_levels
    ))
  }

  design <- summarise_design(data, test_type)
  if (any(design$n < min_reps)) {
    issues <- c(issues, sprintf(
      "Fewer than %d replicates at %d concentration(s).",
      min_reps, sum(design$n < min_reps)
    ))
  }
  issues <- c(issues, check_response_direction(y, data, tt, test_type))

  structure(
    list(
      test_type = test_type, label = tt$label, n_rows = nrow(data),
      n_conc = n_conc, issues = issues, design = design
    ),
    class = "cr_check"
  )
}

# Range checks depend on the statistical form of the response, so they are kept
# separate from the structural checks above and dispatched on response_type.
check_response_range <- function(y, data, tt) {
  w <- character(0)
  switch(tt$response_type,
    continuous_positive = {
      if (any(y <= 0, na.rm = TRUE)) {
        w <- c(w, "Response contains zero or negative values. A Gamma likelihood cannot accommodate these; either use a Gaussian likelihood or apply a documented offset.")
      }
    },
    count = {
      if (any(y != round(y), na.rm = TRUE)) {
        w <- c(w, "Count response contains non-integer values.")
      }
      if (any(y < 0, na.rm = TRUE)) {
        w <- c(w, "Count response contains negative values.")
      }
    },
    proportion = {
      if (any(y < 0 | y > 1, na.rm = TRUE)) {
        w <- c(w, "Proportion response falls outside the interval 0 to 1.")
      } else if (any(y %in% c(0, 1), na.rm = TRUE)) {
        w <- c(w, "Proportion response contains exact zeros or ones, which a Beta likelihood excludes. Either use the underlying counts with a binomial likelihood, or apply a documented transformation.")
      }
    },
    binomial_trials = {
      n <- data[[tt$trials_var]]
      if (any(y > n, na.rm = TRUE)) {
        w <- c(w, "Successes exceed trials in at least one row.")
      }
      if (any(n <= 0, na.rm = TRUE)) {
        w <- c(w, "Trials column contains non-positive values.")
      }
      if (any(y < 0, na.rm = TRUE)) {
        w <- c(w, "Success column contains negative values.")
      }
    }
  )
  w
}

# Two problems are read off the same quantity: the mean response at the highest
# tested concentration as a fraction of the control.
#
# A fraction at or above one means the response rises with concentration. Every
# response column in the registry is oriented to fall, so this is almost always
# the wrong column having been supplied -- the affected count instead of its
# complement, which the standard operating procedure names as one of the
# decisions that are hard to correct later. Left undetected it fits an
# increasing curve and the ECx values are meaningless, so it is reported here
# rather than surfacing later as an unrelated-looking failure in the estimates.
#
# A fraction below one but above the largest effect the test type reports means
# that effect was never observed, so an ECx at that level extrapolates beyond
# the data. The threshold is taken from the registry rather than fixed at a
# half, so that a test type reporting only EC10 and EC20 is judged against what
# it actually reports.
# The mean response at the control and at the highest tested concentration, on
# the proportion scale for binomial test types so the two are comparable. These
# are compared directly rather than as a ratio, because an inverted response
# often has a control mean of exactly zero -- the affected count in an
# undamaged control -- and a ratio is then undefined for the very case the
# direction check exists to catch.
response_extremes <- function(y, data, tt) {
  if (tt$response_type == "binomial_trials") y <- y / data[[tt$trials_var]]
  x <- data[[tt$x_var]]
  top <- max(x, na.rm = TRUE)
  c(
    control = mean(y[x == 0], na.rm = TRUE),
    highest = mean(y[x == top], na.rm = TRUE)
  )
}

check_response_direction <- function(y, data, tt, test_type) {
  e <- response_extremes(y, data, tt)
  if (!all(is.finite(e))) {
    return(character(0))
  }
  if (e[["highest"]] >= e[["control"]]) {
    return(sprintf(
      paste0(
        "The mean response at the highest tested concentration (%s) is not below the ",
        "control (%s), so the response does not decline over the tested range. Every ",
        "test type expects a response that falls with concentration: check that column ",
        "'%s' holds the unaffected count or measurement and not its complement. Fitted ",
        "as they stand these data give an increasing curve and ECx values that cannot ",
        "be interpreted."
      ),
      format(signif(e[["highest"]], 4)), format(signif(e[["control"]], 4)), tt$y_var
    ))
  }
  if (e[["control"]] <= 0) {
    return(character(0))
  }
  ratio <- e[["highest"]] / e[["control"]]
  max_ecx <- max(cr_ecx_targets(test_type))
  if (ratio > 1 - max_ecx / 100) {
    return(sprintf(
      paste0(
        "The largest observed effect is a reduction to %.0f%% of the control, which does ",
        "not reach the EC%g this test type reports. ECx values beyond the observed effect ",
        "range are extrapolations and must be reported as such."
      ),
      100 * ratio, max_ecx
    ))
  }
  character(0)
}

#' Summarise the design of a concentration-response dataset
#'
#' @inheritParams check_cr_data
#' @return A data frame with one row per concentration giving the number of
#'   replicates and the mean and standard deviation of the response. For
#'   binomial test types the response is summarised as a proportion.
#' @export
#' @examples
#' data(algal_growth)
#' summarise_design(algal_growth, "algal_growth")
summarise_design <- function(data, test_type) {
  tt <- cr_test_type(test_type)
  y <- data[[tt$y_var]]
  if (tt$response_type == "binomial_trials") y <- y / data[[tt$trials_var]]
  x <- data[[tt$x_var]]
  parts <- lapply(split(y, x), function(v) {
    data.frame(
      n = length(v), mean = mean(v, na.rm = TRUE),
      sd = stats::sd(v, na.rm = TRUE)
    )
  })
  out <- do.call(rbind, parts)
  out <- cbind(conc = as.numeric(names(parts)), out)
  rownames(out) <- NULL
  out[order(out$conc), , drop = FALSE]
}

#' @export
print.cr_check <- function(x, ...) {
  cat("Concentration-response data check\n")
  cat("  test type      : ", x$test_type, " (", x$label, ")\n", sep = "")
  cat("  rows           : ", x$n_rows, "\n", sep = "")
  cat("  concentrations : ", x$n_conc, "\n", sep = "")
  if (length(x$issues) == 0) {
    cat("  status         : no issues identified\n")
  } else {
    cat("  status         : ", length(x$issues), " item(s) to review\n", sep = "")
    for (w in x$issues) cat("    - ", w, "\n", sep = "")
  }
  invisible(x)
}
