skip_if_not_installed("bayesnec")

# Fitting a bayesnec model needs a Stan toolchain and minutes of sampling, so
# these tests build stub objects carrying the structure the real fit classes
# have and check the code that reads them. The structure asserted here was taken
# from real fits of algal_growth: a bayesnecfit stores predictions in
# `pred_vals`, and a bayesmanecfit stores model-averaged predictions in
# `w_pred_vals` and has no `pred_vals` at all. Reading the wrong slot returns
# NULL and fails later inside ggplot, which is how it went unnoticed until a
# model-averaged fit was run.

pred_data <- function(n = 50) {
  x <- exp(seq(log(0.1), log(32), length.out = n))
  data.frame(
    x = x,
    Estimate = 1.35 / (1 + (x / 4.2)^2.1),
    Q2.5 = 1.35 / (1 + (x / 4.2)^2.1) * 0.8,
    Q97.5 = 1.35 / (1 + (x / 4.2)^2.1) * 1.2
  )
}

stub_bayesnecfit <- function() {
  structure(
    list(model = "nec3param", pred_vals = list(data = pred_data())),
    class = c("bayesnecfit", "bnecfit")
  )
}

stub_bayesmanecfit <- function(models = c("nec3param", "ecxexp")) {
  structure(
    list(
      mod_fits = stats::setNames(vector("list", length(models)), models),
      success_models = models,
      # wi is an unnamed weights object in a real fit, not a named vector; the
      # model names live in the `model` column beside it.
      mod_stats = data.frame(
        model = models,
        waic = seq_along(models) * -10,
        wi = rev(seq_along(models)) / sum(seq_along(models)),
        stringsAsFactors = FALSE
      ),
      w_pred_vals = list(data = pred_data())
    ),
    class = c("bayesmanecfit", "bnecfit")
  )
}

test_that("predictions are read from the slot the fit class actually uses", {
  single <- crworkflows:::bnec_pred_frame(stub_bayesnecfit())
  expect_true(all(c("x", "Estimate", "Q2.5", "Q97.5") %in% names(single)))
  expect_equal(nrow(single), 50)

  averaged <- crworkflows:::bnec_pred_frame(stub_bayesmanecfit())
  expect_true(all(c("x", "Estimate", "Q2.5", "Q97.5") %in% names(averaged)))
  expect_equal(nrow(averaged), 50)
})

test_that("a bayesmanecfit is not read through the single-model slot", {
  # The regression guard: a bayesmanecfit has no pred_vals, so a helper reading
  # that slot would return NULL rather than the averaged predictions.
  fit <- stub_bayesmanecfit()
  expect_null(fit$pred_vals)
  expect_silent(crworkflows:::bnec_pred_frame(fit))
})

test_that("a fit carrying no predictions is reported directly", {
  broken <- structure(list(model = "nec3param"), class = c("bayesnecfit", "bnecfit"))
  expect_error(crworkflows:::bnec_pred_frame(broken), "carries no fitted")

  short <- structure(
    list(pred_vals = list(data = data.frame(x = 1:3, Estimate = 1:3))),
    class = c("bayesnecfit", "bnecfit")
  )
  expect_error(crworkflows:::bnec_pred_frame(short), "missing column")
})

test_that("plot_cr_fit() draws a bayesnec fit of either class", {
  for (fit in list(stub_bayesnecfit(), stub_bayesmanecfit())) {
    p <- plot_cr_fit(fit, algal_growth, "algal_growth")
    expect_s3_class(p, "ggplot")
    expect_equal(length(p$layers), 3)
    expect_match(p$labels$caption, "credible interval")
    expect_silent(invisible(ggplot2::ggplot_build(p)))
  }
})

test_that("the model-averaged plot says so in its caption", {
  p <- plot_cr_fit(stub_bayesmanecfit(), algal_growth, "algal_growth")
  expect_match(p$labels$caption, "Model-averaged")

  p1 <- plot_cr_fit(stub_bayesnecfit(), algal_growth, "algal_growth")
  expect_false(grepl("Model-averaged", p1$labels$caption))
})

test_that("cr_model_weights() names the models beside their weights", {
  w <- cr_model_weights(stub_bayesmanecfit(c("nec3param", "ecxexp", "ecxwb1")))
  expect_equal(names(w), c("model", "weight"))
  expect_setequal(w$model, c("nec3param", "ecxexp", "ecxwb1"))
  expect_false(anyNA(w$model))
  expect_equal(sum(w$weight), 1, tolerance = 1e-8)
  expect_true(!is.unsorted(rev(w$weight)))

  single <- cr_model_weights(stub_bayesnecfit())
  expect_equal(single$model, "nec3param")
  expect_equal(single$weight, 1)
})

test_that("the threshold estimate is labelled with the quantity estimated", {
  # NEC, NSEC and N(S)EC are different quantities. A candidate set mixing
  # threshold and smooth models yields a weighted mixture of nec and NSEC
  # draws, which bayesnec itself warns about; reporting it as a NEC would
  # overstate what was estimated.
  nec_models <- bayesnec::models()$nec
  ecx_models <- bayesnec::models()$ecx

  all_threshold <- stub_bayesmanecfit(nec_models[1:2])
  none_threshold <- stub_bayesmanecfit(setdiff(ecx_models, nec_models)[1:2])
  mixed <- stub_bayesmanecfit(c(nec_models[1], setdiff(ecx_models, nec_models)[1]))

  expect_equal(crworkflows:::bnec_threshold_label(all_threshold), "NEC")
  expect_equal(crworkflows:::bnec_threshold_label(none_threshold), "NSEC")
  expect_equal(crworkflows:::bnec_threshold_label(mixed), "N(S)EC")

  # The registry default for the non-hormetic test types is the decline group,
  # which is mixed, so the routine workflow must not report a bare NEC.
  decline <- stub_bayesmanecfit(bayesnec::models()$decline)
  expect_equal(crworkflows:::bnec_threshold_label(decline), "N(S)EC")
})

test_that("cr_bnec_formula() keeps every model when given a vector", {
  # sprintf() vectorises, so interpolating a vector directly produces one
  # formula string per model and as.formula() silently keeps only the first.
  models <- c("nec3param", "ecxexp", "ecxwb1")
  txt <- paste(deparse(stats::formula(
    cr_bnec_formula("algal_growth", model = models)
  )), collapse = "")
  for (m in models) {
    expect_true(grepl(m, txt, fixed = TRUE), info = paste(m, "missing from", txt))
  }

  single <- paste(deparse(stats::formula(
    cr_bnec_formula("algal_growth", model = "nec3param")
  )), collapse = "")
  expect_true(grepl('model = "nec3param"', single, fixed = TRUE))
})
