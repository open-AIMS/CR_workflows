skip_if_not_installed("drc")

test_that("every test type fits and returns usable ECx estimates", {
  # The generating parameters are known, so this checks that the whole path from
  # the registry through the fit to the estimate table produces a number in the
  # right region, not only that it runs without error.
  for (id in cr_test_types()$id) {
    d <- get(id, envir = asNamespace("crworkflows"))
    fit <- fit_cr_drc(d, id, validate = FALSE)
    expect_s3_class(fit, "drc")
    expect_identical(attr(fit, "cr_test_type"), id)

    res <- suppressWarnings(cr_results_table(fit, id, sample_id = "test"))
    expect_true(all(c("sample_id", "engine", "estimate_type", "level",
                      "estimate", "lower", "upper", "interval") %in% names(res)))
    expect_true(all(res$engine == "drc"))
    expect_true(all(is.finite(res$estimate)), info = paste(id, "gave a non-finite ECx"))
    expect_true(all(res$estimate > 0), info = paste(id, "gave a non-positive ECx"))

    # ECx must increase with x: a larger effect needs a larger concentration.
    ecx <- res[res$estimate_type == "ECx", ]
    ecx <- ecx[order(ecx$level), ]
    expect_true(all(diff(ecx$estimate) > 0), info = paste(id, "ECx not monotonic in x"))
  }
})

test_that("the fitted EC50 is near the concentration generating half the control response", {
  # Checked against the data rather than against the generating parameter,
  # because for the hormetic test types the generating `e` parameter is not the
  # EC50: the curve peaks above the control.
  for (id in c("algal_growth", "fish_larval_survival", "earthworm_survival")) {
    d <- get(id, envir = asNamespace("crworkflows"))
    design <- summarise_design(d, id)
    ctrl <- design$mean[design$conc == 0]
    below <- design$conc[design$mean < ctrl / 2 & design$conc > 0]
    above <- design$conc[design$mean > ctrl / 2 & design$conc > 0]
    bracket <- c(max(above), min(below))

    fit <- fit_cr_drc(d, id, validate = FALSE)
    ec50 <- cr_ecx(fit, ecx_val = 50)$estimate
    expect_gt(ec50, bracket[1] / 2)
    expect_lt(ec50, bracket[2] * 2)
  }
})

test_that("compare_drc_models() ranks the hormetic form first on hormetic data", {
  cmp <- compare_drc_models(bioluminescence_inhibition, "bioluminescence_inhibition")
  expect_true(all(c("fct", "converged", "AIC") %in% names(cmp)))
  expect_true(any(cmp$converged))
  expect_true(!is.unsorted(cmp$AIC, na.rm = TRUE))
  expect_match(cmp$fct[1], "^BC")
})

test_that("compare_drc_models() records a failed candidate rather than aborting", {
  cmp <- compare_drc_models(algal_growth, "algal_growth")
  expect_s3_class(cmp, "data.frame")
  expect_equal(nrow(cmp), length(drc_candidates("algal_growth")))
})

test_that("ECx referencing options give the documented relationship", {
  # For a three-parameter mean function the lower limit is fixed at zero, so
  # control referencing and range referencing must agree exactly.
  fit <- fit_cr_drc(algal_growth, "algal_growth", validate = FALSE)
  ctrl <- cr_ecx(fit, ecx_val = c(10, 50), reference = "control")
  rng <- cr_ecx(fit, ecx_val = c(10, 50), reference = "range")
  expect_equal(ctrl$estimate, rng$estimate, tolerance = 1e-6)
})

test_that("drc_fct() builds a usable mean function by name", {
  f <- drc_fct("LL.3")
  expect_type(f, "list")
  expect_true("fct" %in% names(f))
  expect_error(drc_fct("NOT.A.FUNCTION"))
})

test_that("binomial data are fitted on the proportion scale with trials as weights", {
  fit <- fit_cr_drc(fish_larval_survival, "fish_larval_survival", validate = FALSE)
  expect_true(all(fit$data$.y >= 0 & fit$data$.y <= 1))
  expect_equal(
    sort(unique(fit$dataList$weights)),
    sort(unique(fish_larval_survival$total))
  )

  # The conversion itself, independent of how drc stores it.
  frame <- crworkflows:::drc_frame(
    fish_larval_survival, cr_test_type("fish_larval_survival")
  )
  expect_equal(frame$.y, fish_larval_survival$alive / fish_larval_survival$total)
  expect_equal(frame$.w, fish_larval_survival$total)
})

test_that("compare_drc_models() returns a usable table for every test type", {
  # The original tests covered only algal_growth and the hormetic data, so a
  # failure confined to the count test types went unnoticed until a workflow was
  # rendered. Every test type is exercised here.
  for (id in cr_test_types()$id) {
    d <- get(id, envir = asNamespace("crworkflows"))
    # drc emits "NaNs produced" from its optimiser while trying parameter values
    # that put the mean function out of domain. The fits still converge, which
    # is asserted below; the warnings are muffled only to keep the suite output
    # readable. They are left visible in the package itself.
    cmp <- suppressWarnings(compare_drc_models(d, id))
    expect_s3_class(cmp, "data.frame")
    expect_equal(nrow(cmp), length(drc_candidates(id)), info = id)
    expect_true(any(cmp$converged), info = paste(id, "no candidate converged"))

    ok <- cmp[cmp$converged, , drop = FALSE]
    expect_true(all(is.finite(ok$logLik)), info = paste(id, "non-finite logLik"))
    expect_true(all(is.finite(ok$AIC)), info = paste(id, "non-finite AIC"))
    expect_true(!is.unsorted(cmp$AIC, na.rm = TRUE), info = paste(id, "not AIC-ordered"))
  }
})

test_that("the log-likelihood is the one implied by the response type", {
  # drc reports a positive log-likelihood of the wrong magnitude for
  # type = "Poisson", so the count types are recomputed. This checks the
  # recomputation against a direct calculation, and checks that the other types
  # still agree with drc's own value.
  count_fit <- fit_cr_drc(daphnia_reproduction, "daphnia_reproduction", validate = FALSE)
  mu <- stats::fitted(count_fit)
  y <- daphnia_reproduction$offspring
  expect_equal(
    crworkflows:::cr_drc_loglik(count_fit, "daphnia_reproduction"),
    sum(stats::dpois(y, mu, log = TRUE)),
    tolerance = 1e-6
  )
  # The guard that matters: a count log-likelihood must be negative.
  expect_lt(crworkflows:::cr_drc_loglik(count_fit, "daphnia_reproduction"), 0)

  for (id in c("algal_growth", "fish_larval_survival")) {
    d <- get(id, envir = asNamespace("crworkflows"))
    f <- fit_cr_drc(d, id, validate = FALSE)
    expect_equal(
      crworkflows:::cr_drc_loglik(f, id),
      as.numeric(stats::logLik(f)),
      info = id
    )
  }
})

test_that("a missing lack-of-fit test becomes NA rather than an error", {
  # drc::modelFit() returns NULL for Poisson fits; subscripting it gives a
  # zero-length value that crashes data.frame() rather than yielding NA.
  count_fit <- fit_cr_drc(daphnia_reproduction, "daphnia_reproduction", validate = FALSE)
  lof <- crworkflows:::drc_lack_of_fit(count_fit)
  expect_length(lof, 1L)
  expect_true(is.na(lof) || is.finite(lof))

  binom_fit <- fit_cr_drc(fish_larval_survival, "fish_larval_survival", validate = FALSE)
  expect_length(crworkflows:::drc_lack_of_fit(binom_fit), 1L)
})
