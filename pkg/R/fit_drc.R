#' Candidate drc mean functions for a test type
#'
#' Returns the set of `drc` mean functions considered for a test type. The
#' candidate set is deliberately small. A wide search over `drc` functions
#' inflates the chance of selecting a curve that fits the sample rather than the
#' response, and the selected function has to be defensible in a report.
#'
#' @inheritParams check_cr_data
#' @return A named list of `drc` mean function objects.
#' @export
#' @examples
#' names(drc_candidates("algal_growth"))
drc_candidates <- function(test_type) {
  require_engine("drc")
  tt <- cr_test_type(test_type)
  nm <- if (tt$hormesis) {
    # Brain-Cousens functions carry the low-dose stimulation term; the
    # log-logistic pair is retained so that a non-hormetic fit can win.
    c("BC.4", "BC.5", "LL.3", "LL.4")
  } else if (tt$response_type == "binomial_trials") {
    # Two-parameter forms fix the upper and lower asymptotes at 1 and 0, which
    # is the usual assumption for a survival or success endpoint.
    c("LL.2", "LL.3", "W1.2", "W2.2")
  } else {
    c("LL.3", "LL.4", "W1.3", "W2.3")
  }
  stats::setNames(lapply(nm, drc_fct), nm)
}

#' Fit a concentration-response model with the drc engine
#'
#' Wraps [drc::drm()] with the mean function and error structure recorded for
#' the test type. For binomial test types the response is supplied as a
#' proportion with the number of trials as weights, which is the form `drc`
#' expects for `type = "binomial"`.
#'
#' @inheritParams check_cr_data
#' @param fct A `drc` mean function object. Defaults to the registry value for
#'   the test type.
#' @param validate Whether to run [check_cr_data()] first.
#' @param ... Further arguments passed to [drc::drm()].
#' @return A `drc` object, with the test type recorded in the `cr_test_type`
#'   attribute so that downstream helpers do not need it passed again.
#' @export
fit_cr_drc <- function(data, test_type, fct = NULL, validate = TRUE, ...) {
  require_engine("drc")
  tt <- cr_test_type(test_type)
  if (validate) print(check_cr_data(data, test_type))
  if (is.null(fct)) fct <- drc_fct(tt$drc_fct)

  d <- drc_frame(data, tt)
  args <- list(
    formula = stats::as.formula(paste(".y ~", tt$x_var)),
    data = d, fct = fct, type = tt$drc_type
  )
  if (tt$response_type == "binomial_trials") args$weights <- d$.w
  fit <- do.call(drc::drm, c(args, list(...)))
  attr(fit, "cr_test_type") <- test_type
  fit
}

# drc takes a single response column, so binomial data are converted to a
# proportion plus weights and count data are passed through unchanged.
drc_frame <- function(data, tt) {
  d <- data
  d$.y <- switch(tt$response_type,
    binomial_trials = data[[tt$y_var]] / data[[tt$trials_var]],
    data[[tt$y_var]]
  )
  d$.w <- if (tt$response_type == "binomial_trials") data[[tt$trials_var]] else 1
  d
}

#' Compare candidate drc mean functions
#'
#' Fits every function returned by [drc_candidates()] and returns a comparison
#' table. Selection is by AIC, with the residual standard error and the lack-of-
#' fit test reported alongside so that a fit chosen on AIC alone can be
#' questioned.
#'
#' @inheritParams fit_cr_drc
#' @return A data frame ordered by AIC, with columns `fct`, `converged`,
#'   `logLik`, `AIC`, `n_par` and `lof_p` (the p-value of the lack-of-fit test,
#'   `NA` where it cannot be computed).
#' @export
compare_drc_models <- function(data, test_type, validate = FALSE, ...) {
  require_engine("drc")
  cands <- drc_candidates(test_type)
  rows <- lapply(names(cands), function(nm) {
    fit <- tryCatch(
      fit_cr_drc(data, test_type, fct = cands[[nm]], validate = validate, ...),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      return(data.frame(
        fct = nm, converged = FALSE, logLik = NA_real_, AIC = NA_real_,
        n_par = NA_integer_, lof_p = NA_real_, stringsAsFactors = FALSE
      ))
    }
    # modelFit() fails for some designs (for example a single replicate at a
    # concentration), which is not a reason to drop the candidate.
    lof <- tryCatch(drc::modelFit(fit)[["p value"]][2], error = function(e) NA_real_)
    data.frame(
      fct = nm, converged = TRUE,
      logLik = as.numeric(stats::logLik(fit)),
      AIC = stats::AIC(fit),
      n_par = length(stats::coef(fit)),
      lof_p = as.numeric(lof), stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$AIC, na.last = TRUE), , drop = FALSE]
}

#' Construct a drc mean function by name
#'
#' drc mean functions read their own name out of `match.call()`, so they must be
#' constructed by an ordinary named call rather than with `do.call()` on the
#' function object, which leaves `match.call()[[1]]` holding a closure and fails.
#' This helper exists so that a workflow can turn the name selected by
#' [compare_drc_models()] back into the object [fit_cr_drc()] needs.
#'
#' @param name Name of a drc mean function, for example `"LL.3"`.
#' @return A drc mean function object.
#' @export
#' @examples
#' drc_fct("LL.3")$name
drc_fct <- function(name) {
  require_engine("drc")
  eval(str2lang(paste0("drc::", name, "()")))
}
