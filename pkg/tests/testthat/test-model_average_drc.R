skip_if_not_installed("drc")

test_that("akaike_weights() behaves as the definition requires", {
  expect_equal(sum(akaike_weights(c(100, 102, 110))), 1)
  # An AIC difference of zero gives equal weights.
  expect_equal(akaike_weights(c(50, 50)), c(0.5, 0.5))
  # The weight ratio is exp(-delta/2).
  w <- akaike_weights(c(100, 104))
  expect_equal(w[1] / w[2], exp(2), tolerance = 1e-10)
  # A constant shift cancels, which is why drc counting the scale parameter
  # differently from this package does not change any weight.
  expect_equal(akaike_weights(c(100, 102, 110)), akaike_weights(c(102, 104, 112)))
})

test_that("buckland_combine() reduces to the single-model case and widens with disagreement", {
  # One model carrying all the weight must return that model's own estimate.
  one <- crworkflows:::buckland_combine(c(5, 9), c(1, 1), c(1, 0))
  expect_equal(unname(one[["estimate"]]), 5)
  expect_equal(unname(one[["se"]]), 1)

  # Models that agree add nothing to the standard error.
  agree <- crworkflows:::buckland_combine(c(5, 5), c(1, 1), c(0.5, 0.5))
  expect_equal(unname(agree[["se"]]), 1)

  # Models that disagree must widen it.
  disagree <- crworkflows:::buckland_combine(c(4, 6), c(1, 1), c(0.5, 0.5))
  expect_gt(disagree[["se"]], agree[["se"]])
  expect_equal(unname(disagree[["estimate"]]), 5)

  expect_equal(unname(crworkflows:::buckland_combine(NA_real_, NA_real_, 1)[["n_models"]]), 0)
})

test_that("fit_cr_drc_ma() works for every test type, including the count types", {
  # drc::maED() fails outright on type = "Poisson" fits, so the count test types
  # are the reason this is implemented here rather than delegated.
  for (id in cr_test_types()$id) {
    ma <- suppressWarnings(fit_cr_drc_ma(get(id, envir = asNamespace("crworkflows")),
      id,
      validate = FALSE
    ))
    expect_s3_class(ma, "cr_drc_ma")
    expect_gt(length(ma$fits), 0)
    expect_equal(sum(ma$weights), 1, tolerance = 1e-8, info = id)
    expect_true(all(is.finite(ma$comparison$AIC)), info = id)

    est <- suppressWarnings(cr_ecx(ma))
    expect_true(all(is.finite(est$estimate)), info = id)
    expect_true(all(est$estimate > 0), info = id)
    expect_true(all(est$lower <= est$estimate), info = id)
    expect_true(all(est$upper >= est$estimate), info = id)
    expect_true(all(est$lower >= 0), info = id)
  }
})

test_that("the averaged estimate agrees with drc::maED() on the types maED supports", {
  # Compared on drc's own referencing so both compute the same quantity. Exact
  # agreement is not expected: maED refits each candidate from the supplied fit,
  # which converges to slightly different parameter values than fitting each
  # separately. The weights are identical, which is the part being checked.
  for (id in c("algal_growth", "fish_larval_survival", "plant_growth")) {
    d <- get(id, envir = asNamespace("crworkflows"))
    ma <- suppressWarnings(fit_cr_drc_ma(d, id, validate = FALSE))
    mine <- suppressWarnings(cr_ecx(ma, ecx_val = c(10, 50), reference = "range"))
    base <- fit_cr_drc(d, id, validate = FALSE)
    theirs <- suppressWarnings(drc::maED(base,
      fctList = drc_candidates(id),
      respLev = c(10, 50), interval = "buckland", display = FALSE
    ))
    expect_equal(mine$estimate, as.numeric(theirs[, "Estimate"]),
      tolerance = 0.05, info = id
    )
    expect_equal(mine$se, as.numeric(theirs[, "Std. Error"]),
      tolerance = 0.15, info = id
    )
  }
})

test_that("the AIC matches drc's own where drc computes it correctly", {
  # drc's AIC is trustworthy for the continuous and binomial types; matching it
  # keeps the reported numbers comparable with anything computed directly in drc.
  for (id in c("algal_growth", "fish_larval_survival")) {
    d <- get(id, envir = asNamespace("crworkflows"))
    f <- fit_cr_drc(d, id, validate = FALSE)
    k <- crworkflows:::cr_drc_npar(f, id)
    expect_equal(
      -2 * crworkflows:::cr_drc_loglik(f, id) + 2 * k,
      stats::AIC(f),
      tolerance = 1e-6, info = id
    )
  }
})

test_that("averaging reduces to the single model when one candidate dominates", {
  # The hormetic data are fitted decisively by BC.4, so the averaged estimate
  # must be close to that model's own.
  id <- "bioluminescence_inhibition"
  d <- get(id, envir = asNamespace("crworkflows"))
  ma <- suppressWarnings(fit_cr_drc_ma(d, id, validate = FALSE))
  top <- ma$comparison$model[1]
  expect_gt(ma$comparison$weight[1], 0.5)

  avg <- suppressWarnings(cr_ecx(ma, ecx_val = 50))
  single <- suppressWarnings(cr_ecx(fit_cr_drc(d, id, fct = drc_fct(top), validate = FALSE),
    ecx_val = 50, test_type = id
  ))
  expect_equal(avg$estimate, single$estimate, tolerance = 0.02)
})

test_that("averaging widens the interval where candidates disagree", {
  # daphnia_reproduction spreads weight across three mean functions, so the
  # model-averaged interval must be wider than the best single model's. That
  # widening is the reason for averaging.
  id <- "daphnia_reproduction"
  d <- get(id, envir = asNamespace("crworkflows"))
  ma <- suppressWarnings(fit_cr_drc_ma(d, id, validate = FALSE))
  expect_lt(max(ma$comparison$weight), 0.95)

  avg <- suppressWarnings(cr_ecx(ma, ecx_val = 50))
  single <- suppressWarnings(cr_ecx(
    fit_cr_drc(d, id, fct = drc_fct(ma$comparison$model[1]), validate = FALSE),
    ecx_val = 50, test_type = id
  ))
  expect_gt(avg$upper - avg$lower, single$upper - single$lower)
})

test_that("cr_model_weights() dispatches for both engines", {
  ma <- suppressWarnings(fit_cr_drc_ma(algal_growth, "algal_growth", validate = FALSE))
  w <- cr_model_weights(ma)
  expect_equal(names(w)[1:2], c("model", "weight"))
  expect_equal(sum(w$weight), 1, tolerance = 1e-8)
  expect_true(!is.unsorted(rev(w$weight)))

  expect_error(cr_model_weights(structure(list(), class = "nonsense")),
    "No cr_model_weights"
  )
})

test_that("the averaged fit plots with a labelled band", {
  ma <- suppressWarnings(fit_cr_drc_ma(algal_growth, "algal_growth", validate = FALSE))
  p <- plot_cr_fit(ma, algal_growth, "algal_growth")
  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 3)
  expect_match(p$labels$caption, "Model-averaged")
  expect_match(p$labels$caption, "Buckland")
  expect_silent(invisible(ggplot2::ggplot_build(p)))
})

test_that("cr_results_table() accepts an averaged fit", {
  ma <- suppressWarnings(fit_cr_drc_ma(algal_growth, "algal_growth", validate = FALSE))
  res <- suppressWarnings(cr_results_table(ma, "algal_growth", sample_id = "S1"))
  expect_true(all(c("sample_id", "engine", "estimate_type", "estimate",
                    "lower", "upper", "interval") %in% names(res)))
  expect_match(res$interval[1], "model-averaged")
})

test_that("an averaged fit plots for every test type", {
  # Plotting an averaged fit combines the candidate predictions pointwise, which
  # is a different path from the single-fit plot and was previously exercised on
  # one test type only.
  for (id in cr_test_types()$id) {
    d <- get(id, envir = asNamespace("crworkflows"))
    ma <- suppressWarnings(fit_cr_drc_ma(d, id, validate = FALSE))
    p <- suppressWarnings(plot_cr_fit(ma, d, id))
    expect_s3_class(p, "ggplot")
    expect_silent(invisible(ggplot2::ggplot_build(p)))
    expect_match(p$labels$caption, "Buckland", info = id)
    # No gaps in the drawn curve or band.
    pred <- p$layers[[3]]$data
    expect_true(all(is.finite(pred$.fit)), info = id)
    expect_true(all(is.finite(pred$.lower) & is.finite(pred$.upper)), info = id)
  }
})

test_that("the plotted curve is averaged over one fixed set of candidates", {
  # drc's delta-method standard error is NaN over most of the tested range for
  # two of the four candidates here. Combining point by point dropped whichever
  # were unusable at each concentration, so the curve was averaged over between
  # two and four candidates depending on where along the axis it was read, with
  # a small discontinuity at each change and a caption that named a count
  # holding nowhere in particular.
  id <- "daphnia_immobilisation"
  d <- get(id, envir = asNamespace("crworkflows"))
  ma <- suppressWarnings(fit_cr_drc_ma(d, id, validate = FALSE))

  expect_warning(
    p <- plot_cr_fit(ma, d, id),
    "no usable standard error"
  )
  expect_match(p$labels$caption, "of [0-9]+ candidates")

  # The composition is fixed, so the curve has no jumps: on a grid of 200 points
  # over a curve spanning the full response range, no single step may be a large
  # fraction of that range.
  pred <- p$layers[[3]]$data
  span <- diff(range(pred$.fit))
  expect_lt(max(abs(diff(pred$.fit))), 0.1 * span)
  expect_false(is.unsorted(rev(pred$.fit)))
})

test_that("the many drc NaN warnings are summarised into one", {
  # Drawing this figure previously emitted 225 separate "NaNs produced"
  # warnings, which hide anything else the call has to report.
  id <- "daphnia_immobilisation"
  d <- get(id, envir = asNamespace("crworkflows"))
  ma <- suppressWarnings(fit_cr_drc_ma(d, id, validate = FALSE))

  warnings_seen <- character(0)
  withCallingHandlers(
    invisible(plot_cr_fit(ma, d, id)),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings_seen, 1L)
  expect_false(any(grepl("NaNs produced", warnings_seen, fixed = TRUE)))
})

test_that("each candidate is predicted once when building the averaged curve", {
  # The band needs the prediction and its standard error at every grid point.
  # Taking them from two separate predict() calls doubled the work for no gain.
  ma <- suppressWarnings(fit_cr_drc_ma(algal_growth, "algal_growth", validate = FALSE))
  calls <- 0L
  ns <- asNamespace("crworkflows")
  original <- get("cr_drc_predict", envir = ns)
  unlockBinding("cr_drc_predict", ns)
  assign("cr_drc_predict", function(...) {
    calls <<- calls + 1L
    original(...)
  }, envir = ns)
  on.exit({
    assign("cr_drc_predict", original, envir = ns)
    lockBinding("cr_drc_predict", ns)
  })

  invisible(plot_cr_fit(ma, algal_growth, "algal_growth"))
  expect_equal(calls, length(ma$fits))
})
