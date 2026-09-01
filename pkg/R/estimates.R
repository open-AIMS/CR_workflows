#' Extract ECx estimates from a fit of either engine
#'
#' Returns ECx estimates in one common format regardless of which engine
#' produced the fit, so that a results table has the same columns whether the
#' laboratory runs `bayesnec` or `drc`.
#'
#' The interval reported is not the same quantity in the two engines and the
#' `interval` column records which was used. For `bayesnec` it is a credible
#' interval from the posterior of the model-averaged prediction. For `drc` it is
#' a delta-method confidence interval. They are not interchangeable and should
#' not be presented in a single column without that label.
#'
#' @param fit A `bayesnecfit`, `bayesmanecfit` or `drc` object.
#' @param ecx_val ECx levels as percentages. Defaults to the registry value for
#'   the test type.
#' @param test_type Test-type identifier. For [cr_ecx()] this is only needed for
#'   `drc` fits not created by [fit_cr_drc()], which records it as an attribute;
#'   [cr_results_table()] always requires it, because the identifying columns it
#'   adds are read from the registry.
#' @param level Interval width. Defaults to 0.95.
#' @param ... Passed to the underlying engine function. The `drc` methods also
#'   accept `reference`, which selects what the ECx is referenced to: `"control"`
#'   (the default) references it to the fitted control response, which is the
#'   convention in ecotoxicological reporting and what `bayesnec::ecx()` returns;
#'   `"range"` references it to the fitted response range, which is drc's own
#'   convention. The two coincide wherever the fitted lower limit is zero.
#' @return A data frame with one row per requested level and columns `engine`,
#'   `estimate_type`, `level`, `estimate`, `se`, `lower`, `upper` and `interval`.
#'   `se` is `NA` for `bayesnec` fits, whose interval is a posterior quantile
#'   rather than a standard error, and is present so that both engines return one
#'   schema. A level whose target lies outside the range of the fitted curve has
#'   no solution; it is returned as `NA` with the reason in its `interval`, and a
#'   warning is raised, so that the levels that were estimated are still
#'   available.
#' @export
cr_ecx <- function(fit, ecx_val = NULL, test_type = NULL, level = 0.95, ...) {
  UseMethod("cr_ecx")
}

#' @export
cr_ecx.default <- function(fit, ecx_val = NULL, test_type = NULL, level = 0.95, ...) {
  stop("No cr_ecx() method for an object of class ",
    paste(class(fit), collapse = "/"), ".",
    call. = FALSE
  )
}

#' @export
cr_ecx.drc <- function(fit, ecx_val = NULL, test_type = NULL, level = 0.95,
                       reference = c("control", "range"), ...) {
  require_engine("drc")
  reference <- match.arg(reference)
  test_type <- test_type %||% attr(fit, "cr_test_type")
  ecx_val <- ecx_val %||% cr_ecx_targets(test_type)

  # ECx is referenced to the fitted control response by default, because that is
  # the convention in ecotoxicological reporting and it is what bayesnec::ecx()
  # returns, so estimates from the two engines answer the same question.
  #
  # The control-referenced target is converted to a percentage of the fitted
  # range and passed to drc's relative calculation, rather than being passed
  # straight to type = "absolute". drc's absolute branch does not solve the
  # fitted curve for Brain-Cousens mean functions: on the hormetic example data
  # it returns near-identical concentrations for EC10, EC20 and EC50, and it
  # fails outright in uniroot() for others. The relative branch is exercised by
  # every drc example and gives a delta-method interval, so the conversion is
  # done here instead. Where the fitted lower limit is zero, which holds for all
  # the three-parameter defaults in the registry, the converted level equals the
  # nominal level and the two definitions coincide.
  respLev <- if (reference == "control") {
    ecx_to_relative(fit, test_type, ecx_val)
  } else {
    ecx_val
  }
  # A level whose target lies outside the fitted range has no solution. That is
  # a statement about that level, not about the fit: a curve reaching a 45 per
  # cent effect determines EC10 and EC20 perfectly well and only leaves EC50
  # unreachable. Raising an error would withhold the levels that were estimated
  # and, in a workflow, produce no report at all. The level is therefore
  # returned as NA with the reason in its own interval column, which is the same
  # treatment the package gives every other problem that threatens one result
  # rather than making the analysis impossible.
  #
  # The reason states what was found rather than a conclusion. The target can
  # fall outside the range because the curve never declines that far, in which
  # case the ECx is above the tested range; it can equally happen because the
  # fitted curve rises with concentration, which means the wrong response column
  # was supplied. check_cr_data() distinguishes the two, so the analyst is sent
  # there rather than being handed one of the two explanations as established.
  ref_label <- if (reference == "control") "control response" else "response range"
  unreachable <- !is.finite(respLev)
  if (all(unreachable)) {
    warning("No ECx level could be estimated for this fit: every target lies ",
      "outside the range of the fitted curve. Run check_cr_data() on these data.",
      call. = FALSE
    )
    return(ecx_na_rows(ecx_val, ref_label))
  }

  # drc::ED() fails inside uniroot() whenever the Brain-Cousens hormesis term is
  # weakly determined: the root-finding interval it chooses does not bracket the
  # target, and the whole call errors rather than returning that one level as
  # NA. That is a numerical failure of the solver, not a statement that the
  # estimate does not exist, and it stops a workflow that has otherwise
  # succeeded. Where it happens the curve is solved directly instead and the
  # estimate is returned without an interval, labelled as such, so that the
  # analyst sees the point estimate and knows the interval is missing.
  est <- tryCatch(
    drc::ED(fit,
      respLev = respLev[!unreachable], type = "relative", interval = "delta",
      level = level, display = FALSE, ...
    ),
    error = function(e) NULL
  )

  if (!is.null(est)) {
    out <- ecx_na_rows(ecx_val, ref_label)
    out$estimate[!unreachable] <- as.numeric(est[, "Estimate"])
    out$se[!unreachable] <- as.numeric(est[, "Std. Error"])
    out$lower[!unreachable] <- as.numeric(est[, "Lower"])
    out$upper[!unreachable] <- as.numeric(est[, "Upper"])
    out$interval[!unreachable] <- sprintf(
      "%.0f%% delta-method confidence interval, referenced to the fitted %s",
      100 * level, ref_label
    )
    if (any(unreachable)) {
      warning("ECx level(s) ", paste(ecx_val[unreachable], collapse = ", "),
        " lie outside the range of the fitted curve and are reported as NA. ",
        "Run check_cr_data() on these data.",
        call. = FALSE
      )
    }
    return(out)
  }

  warning("drc::ED() did not converge for this fit; ECx values were obtained by ",
    "solving the fitted curve directly and are reported without a confidence ",
    "interval. Check the fitted model, and consider compare_drc_models().",
    call. = FALSE
  )
  out <- ecx_na_rows(ecx_val, ref_label)
  out$estimate[!unreachable] <- cr_solve_ecx(fit, test_type, ecx_val[!unreachable])
  out$interval[!unreachable] <- sprintf(
    "no interval available, referenced to the fitted %s", ref_label
  )
  out
}

# The result skeleton, one row per requested level, filled in where a level
# could be estimated. A level that could not keeps its NA estimate and carries
# the reason in the column that already labels what every other interval is.
ecx_na_rows <- function(ecx_val, ref_label) {
  data.frame(
    engine = "drc", estimate_type = "ECx", level = ecx_val,
    estimate = NA_real_, se = NA_real_, lower = NA_real_, upper = NA_real_,
    interval = paste0(
      "not estimable: the target lies outside the range of the fitted curve, ",
      "referenced to the fitted ", ref_label
    ),
    stringsAsFactors = FALSE
  )
}

# Solve the fitted curve for the concentration giving an x% reduction from the
# fitted control response. Used only as the fallback above.
cr_solve_ecx <- function(fit, test_type, ecx_val) {
  tt <- cr_test_type(test_type)
  d0 <- cr_control_prediction(fit, test_type)
  xs <- fit$dataList$dose
  hi <- max(xs, na.rm = TRUE)
  lo <- min(xs[xs > 0], na.rm = TRUE)
  fx <- function(x) {
    nd <- data.frame(x)
    names(nd) <- tt$x_var
    as.numeric(cr_drc_predict(fit, nd)[1])
  }
  vapply(ecx_val, function(v) {
    target <- d0 * (1 - v / 100)
    g <- function(x) fx(x) - target
    # Widen the bracket beyond the tested range so that an ECx just outside it
    # is still returned, and flagged as an extrapolation by check_cr_data().
    tryCatch(stats::uniroot(g, interval = c(lo / 100, hi * 10))$root,
      error = function(e) NA_real_
    )
  }, numeric(1))
}

# Convert an ECx level referenced to the fitted control response into the
# percentage of the fitted range that drc's relative calculation expects.
# Returns NA where the target lies outside the fitted range, which happens when
# the lower asymptote sits above the requested effect level.
ecx_to_relative <- function(fit, test_type, ecx_val) {
  d <- cr_control_prediction(fit, test_type)
  cf <- stats::coef(fit)
  lower <- cf[grep("^c:", names(cf))]
  lower <- if (length(lower)) unname(lower[1]) else 0
  rng <- d - lower
  if (!is.finite(rng) || rng <= 0) {
    return(rep(NA_real_, length(ecx_val)))
  }
  target <- d * (1 - ecx_val / 100)
  rel <- 100 * (d - target) / rng
  ifelse(rel > 0 & rel < 100, rel, NA_real_)
}

# Fitted response at zero concentration, used as the reference for ECx.
cr_control_prediction <- function(fit, test_type) {
  tt <- cr_test_type(test_type)
  nd <- data.frame(0)
  names(nd) <- tt$x_var
  as.numeric(cr_drc_predict(fit, nd)[1])
}

# drc's predict method triggers a deprecation warning once per row under
# R >= 4.5 ("Recycling array of length 1 in array-vector arithmetic"). It comes
# from drc internals, not from the model, and drowns real warnings, so it is
# muffled here by message rather than by suppressWarnings().
cr_drc_predict <- function(fit, newdata, ...) {
  withCallingHandlers(
    stats::predict(fit, newdata = newdata, ...),
    warning = function(w) {
      if (grepl("Recycling array of length 1", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

#' @export
cr_ecx.bayesnecfit <- function(fit, ecx_val = NULL, test_type = NULL,
                               level = 0.95, ...) {
  bnec_ecx_table(fit, ecx_val, test_type, level, ...)
}

#' @export
cr_ecx.bayesmanecfit <- function(fit, ecx_val = NULL, test_type = NULL,
                                 level = 0.95, ...) {
  bnec_ecx_table(fit, ecx_val, test_type, level, ...)
}

bnec_ecx_table <- function(fit, ecx_val, test_type, level, ...) {
  require_engine("bayesnec")
  ecx_val <- ecx_val %||% if (!is.null(test_type)) cr_ecx_targets(test_type) else c(10, 20, 50)
  rows <- lapply(ecx_val, function(v) {
    e <- bayesnec::ecx(fit, ecx_val = v, prob_vals = c(0.5, (1 - level) / 2, 1 - (1 - level) / 2), ...)
    data.frame(
      engine = "bayesnec", estimate_type = "ECx", level = v,
      estimate = unname(e[1]),
      # A credible interval is not built from a standard error; the column
      # exists so that both engines return the same schema.
      se = NA_real_,
      lower = unname(e[2]), upper = unname(e[3]),
      interval = sprintf("%.0f%% credible interval", 100 * level),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Extract the threshold estimate from a bayesnec fit
#'
#' Returns the no-effect concentration and labels it with the quantity that was
#' actually estimated. Three labels are possible and they are not
#' interchangeable:
#'
#' \describe{
#'   \item{`NEC`}{Every model in the fit carries the `nec` threshold parameter,
#'     so the estimate is that parameter.}
#'   \item{`NSEC`}{No model carries a threshold, so the estimate is derived from
#'     the smooth fitted curve.}
#'   \item{`N(S)EC`}{The candidate set mixes threshold and smooth models, so the
#'     model-averaged estimate is a weighted mixture of `nec` and NSEC draws. It
#'     is not a NEC, and reporting it as one overstates what was estimated.}
#' }
#'
#' The distinction was found by `bayesnec` itself: fitting the `decline` model
#' group produces a mixed candidate set, and `bayesnec::nec()` warns that its
#' return value is a mixture. Labelling that result `NEC` in a report would be
#' wrong.
#'
#' @inheritParams cr_ecx
#' @param type One of `"auto"`, `"nec"` or `"nsec"`. `"auto"` chooses the label
#'   from the models in the fit, as described above. The explicit values force
#'   the corresponding `bayesnec` function and label.
#' @return A data frame in the same format as [cr_ecx()], with `estimate_type`
#'   one of `"NEC"`, `"NSEC"` or `"N(S)EC"`.
#' @export
cr_nec <- function(fit, type = c("auto", "nec", "nsec"), level = 0.95, ...) {
  require_engine("bayesnec")
  type <- match.arg(type)
  probs <- c(0.5, (1 - level) / 2, 1 - (1 - level) / 2)

  if (type == "auto") {
    label <- bnec_threshold_label(fit)
    # bayesnec::nec() returns the mixture for a mixed set, so it is the right
    # function to call in both the all-threshold and the mixed case; only the
    # label differs.
    fn <- if (label == "NSEC") "nsec" else "nec"
  } else {
    label <- toupper(type)
    fn <- type
  }

  e <- if (fn == "nec") {
    bayesnec::nec(fit, prob_vals = probs, ...)
  } else {
    bayesnec::nsec(fit, prob_vals = probs, ...)
  }
  data.frame(
    engine = "bayesnec", estimate_type = label, level = NA_real_,
    estimate = unname(e[1]), se = NA_real_,
    lower = unname(e[2]), upper = unname(e[3]),
    interval = sprintf("%.0f%% credible interval", 100 * level),
    stringsAsFactors = FALSE
  )
}

bnec_model_names <- function(fit) {
  if (inherits(fit, "bayesmanecfit")) names(fit$mod_fits) else fit$model
}

# Which threshold quantity a fit actually estimates, from the models it holds.
# "NEC" only where every model carries the nec parameter; "N(S)EC" where the
# candidate set mixes threshold and smooth models, because the model-averaged
# estimate is then a weighted mixture of nec and NSEC draws.
bnec_threshold_label <- function(fit) {
  models_in_fit <- bnec_model_names(fit)
  n_threshold <- sum(models_in_fit %in% bayesnec::models()$nec)
  if (n_threshold == length(models_in_fit)) {
    "NEC"
  } else if (n_threshold == 0) {
    "NSEC"
  } else {
    "N(S)EC"
  }
}

#' Assemble the reporting table for a fit
#'
#' Combines the ECx estimates with, for `bayesnec` fits, the NEC or NSEC
#' estimate, and adds the identifying columns a laboratory report requires.
#'
#' @inheritParams cr_ecx
#' @param sample_id Identifier for the sample or test, carried into the table.
#' @return A data frame.
#' @export
cr_results_table <- function(fit, test_type, ecx_val = NULL, sample_id = NA_character_,
                             level = 0.95, ...) {
  tt <- cr_test_type(test_type)
  out <- cr_ecx(fit, ecx_val = ecx_val, test_type = test_type, level = level, ...)
  if (inherits(fit, c("bayesnecfit", "bayesmanecfit"))) {
    out <- rbind(out, cr_nec(fit, level = level))
  }
  cbind(
    sample_id = sample_id,
    test_type = test_type,
    label = tt$label,
    guideline = tt$guideline,
    endpoint = tt$endpoint,
    units = tt$conc_units,
    out,
    stringsAsFactors = FALSE
  )
}
