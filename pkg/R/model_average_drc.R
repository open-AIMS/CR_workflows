#' Fit every candidate mean function and average them by Akaike weight
#'
#' Fits each function returned by [drc_candidates()] and combines them into one
#' model-averaged fit, so that the reported uncertainty includes the choice of
#' curve form rather than being conditional on one function having been picked
#' correctly. This is the `drc` counterpart of what `bayesnec` does with
#' stacking weights.
#'
#' Weights are Akaike weights, `exp(-delta AIC / 2)` normalised to sum to one,
#' computed from the log-likelihood returned by the internal `cr_drc_loglik()`.
#' That matters for the count test types: `drc` reports a log-likelihood of the
#' wrong sign and magnitude for `type = "Poisson"`, so weights taken from its
#' AIC would be meaningless, and [drc::maED()] fails on those fits outright.
#'
#' @inheritParams check_cr_data
#' @param validate Whether to run [check_cr_data()] first.
#' @param ... Passed to [fit_cr_drc()].
#' @return An object of class `cr_drc_ma`: a list with elements `fits` (the
#'   converged fits, named by mean function), `weights`, `comparison`,
#'   `test_type` and `data`.
#' @export
#' @examples
#' \donttest{
#' ma <- fit_cr_drc_ma(algal_growth, "algal_growth", validate = FALSE)
#' cr_model_weights(ma)
#' }
fit_cr_drc_ma <- function(data, test_type, validate = TRUE, ...) {
  require_engine("drc")
  if (validate) print(check_cr_data(data, test_type))

  cands <- drc_candidates(test_type)
  fits <- lapply(names(cands), function(nm) {
    tryCatch(
      fit_cr_drc(data, test_type, fct = cands[[nm]], validate = FALSE, ...),
      error = function(e) NULL
    )
  })
  names(fits) <- names(cands)
  fits <- fits[!vapply(fits, is.null, logical(1))]
  if (!length(fits)) {
    stop("No candidate mean function converged for test type '", test_type,
      "', so there is nothing to average.",
      call. = FALSE
    )
  }

  ll <- vapply(fits, cr_drc_loglik, numeric(1), test_type = test_type)
  k <- vapply(fits, cr_drc_npar, numeric(1), test_type = test_type)
  aic <- -2 * ll + 2 * k
  w <- akaike_weights(aic)

  comparison <- data.frame(
    model = names(fits), logLik = ll, AIC = aic,
    delta = aic - min(aic), n_par = k, weight = w,
    stringsAsFactors = FALSE
  )
  comparison <- comparison[order(-comparison$weight), , drop = FALSE]
  rownames(comparison) <- NULL

  structure(
    list(
      fits = fits, weights = w, comparison = comparison,
      test_type = test_type, data = data
    ),
    class = "cr_drc_ma"
  )
}

#' Akaike weights from a vector of AIC values
#'
#' @param aic Numeric vector of AIC values.
#' @return A numeric vector of weights summing to one.
#' @export
#' @examples
#' akaike_weights(c(100, 102, 110))
akaike_weights <- function(aic) {
  if (all(is.na(aic))) {
    return(rep(NA_real_, length(aic)))
  }
  d <- aic - min(aic, na.rm = TRUE)
  w <- exp(-d / 2)
  w[is.na(w)] <- 0
  w / sum(w)
}

# Buckland et al. (1997) unconditional combination. The averaged estimate is the
# weighted mean; the standard error adds, to each model's own variance, the
# squared distance of that model's estimate from the average, so that
# disagreement between candidates widens the interval rather than being lost.
# This is what makes a model-averaged interval wider than a selected model's.
buckland_combine <- function(estimate, se, weight) {
  keep <- is.finite(estimate) & is.finite(se) & is.finite(weight)
  if (!any(keep)) {
    return(c(estimate = NA_real_, se = NA_real_, n_models = 0))
  }
  estimate <- estimate[keep]
  se <- se[keep]
  weight <- weight[keep] / sum(weight[keep])
  theta <- sum(weight * estimate)
  c(
    estimate = theta,
    se = sum(weight * sqrt(se^2 + (estimate - theta)^2)),
    n_models = length(estimate)
  )
}

#' @export
cr_ecx.cr_drc_ma <- function(fit, ecx_val = NULL, test_type = NULL,
                             level = 0.95, reference = c("control", "range"),
                             ...) {
  reference <- match.arg(reference)
  test_type <- test_type %||% fit$test_type
  ecx_val <- ecx_val %||% cr_ecx_targets(test_type)
  z <- stats::qnorm(1 - (1 - level) / 2)

  # Each candidate's ECx is taken on the control-referenced scale first, so the
  # quantity being averaged is the same one the single-model path reports.
  per_model <- lapply(names(fit$fits), function(nm) {
    tryCatch(
      suppressWarnings(cr_ecx(fit$fits[[nm]],
        ecx_val = ecx_val, test_type = test_type,
        level = level, reference = reference, ...
      )),
      error = function(e) NULL
    )
  })
  names(per_model) <- names(fit$fits)
  ok <- !vapply(per_model, is.null, logical(1))
  if (!any(ok)) {
    stop("No candidate produced an ECx estimate for test type '", test_type, "'.",
      call. = FALSE
    )
  }

  rows <- lapply(seq_along(ecx_val), function(i) {
    est <- vapply(per_model[ok], function(p) p$estimate[i], numeric(1))
    se <- vapply(per_model[ok], function(p) p$se[i], numeric(1))
    w <- fit$weights[names(per_model)[ok]]
    comb <- buckland_combine(est, se, w)
    data.frame(
      engine = "drc", estimate_type = "ECx", level = ecx_val[i],
      estimate = unname(comb[["estimate"]]),
      se = unname(comb[["se"]]),
      lower = unname(comb[["estimate"]] - z * comb[["se"]]),
      upper = unname(comb[["estimate"]] + z * comb[["se"]]),
      interval = sprintf(
        "%.0f%% model-averaged interval (Buckland) over %d candidate(s), referenced to the fitted %s",
        100 * level, comb[["n_models"]],
        if (reference == "control") "control response" else "response range"
      ),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  # A lower confidence limit below zero is not a concentration. It is clamped
  # rather than reported, because the Buckland interval is symmetric on the
  # concentration scale and can cross zero for a poorly determined low ECx.
  out$lower <- pmax(out$lower, 0)
  out
}

#' @export
print.cr_drc_ma <- function(x, ...) {
  cat("Model-averaged drc fit\n")
  cat("  test type : ", x$test_type, "\n", sep = "")
  cat("  candidates: ", length(x$fits), " converged\n", sep = "")
  print(format(x$comparison, digits = 4), row.names = FALSE)
  invisible(x)
}

#' @export
cr_model_weights.cr_drc_ma <- function(fit) {
  data.frame(
    model = fit$comparison$model,
    weight = fit$comparison$weight,
    delta_AIC = fit$comparison$delta,
    stringsAsFactors = FALSE
  )
}
