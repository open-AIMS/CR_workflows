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
    # Three-parameter forms lead, because they estimate the control response
    # rather than fixing it at 1. The two-parameter forms assume every control
    # individual survives or succeeds, which no guideline requires and few tests
    # achieve: on the shipped example data, wherever the control proportion is
    # below 1, LL.3 beats LL.2 by between 90 and 1900 AIC units. LL.2 is
    # retained because it wins where the control response really is 100 per
    # cent, and it is the more parsimonious model in that case.
    c("LL.3", "LL.2", "W1.3", "W2.3")
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
    lof <- drc_lack_of_fit(fit)
    ll <- cr_drc_loglik(fit, test_type)
    k <- cr_drc_npar(fit, test_type)
    data.frame(
      fct = nm, converged = TRUE,
      logLik = ll,
      AIC = -2 * ll + 2 * k,
      n_par = k,
      lof_p = lof, stringsAsFactors = FALSE
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

# drc's own log-likelihood cannot be used for the count test types. For
# type = "Poisson" it returns a value of the wrong sign and magnitude: on
# daphnia_reproduction it reports +21538 where the Poisson log-likelihood of the
# same fit is -269.25, giving an AIC of -42942. The parameter estimates are
# unaffected -- the fitted EC50 is sensible -- but an AIC built from that number
# cannot rank candidates. The likelihood is therefore recomputed from the fitted
# means for count responses. drc's value is used for the continuous and binomial
# types, where it was checked against a direct calculation and agrees.
cr_drc_loglik <- function(fit, test_type) {
  tt <- cr_test_type(test_type)
  if (tt$response_type != "count") {
    return(as.numeric(stats::logLik(fit)))
  }
  mu <- pmax(stats::fitted(fit), .Machine$double.eps)
  # The response is recovered from the fit rather than read from a data slot,
  # whose name has varied between drc versions.
  y <- mu + stats::residuals(fit)
  sum(stats::dpois(round(y), mu, log = TRUE))
}

# Number of estimated parameters, for AIC. A Gaussian fit estimates a scale
# parameter in addition to the mean-function coefficients; binomial and Poisson
# fits do not. drc's own AIC counts the scale parameter the same way, so this
# keeps the AIC reported here on the same footing as drc's, rather than two
# units below it. Akaike weights are unaffected either way, because within one
# candidate set the offset is the same for every model and cancels.
cr_drc_npar <- function(fit, test_type) {
  tt <- cr_test_type(test_type)
  k <- length(stats::coef(fit))
  if (tt$response_type %in% c("binomial_trials", "count")) k else k + 1L
}

# drc::modelFit() returns NULL rather than raising an error for some fits,
# including every type = "Poisson" fit. Subscripting the absent p-value then
# gives a zero-length value, which reaches data.frame() as a "differing number
# of rows" error rather than as a missing lack-of-fit test. Both the NULL and
# the error case are reduced to NA here.
drc_lack_of_fit <- function(fit) {
  tryCatch(
    {
      mf <- drc::modelFit(fit)
      p <- if (is.null(mf)) NULL else mf[["p value"]]
      if (length(p) >= 2L) as.numeric(p[[2]]) else NA_real_
    },
    error = function(e) NA_real_
  )
}
