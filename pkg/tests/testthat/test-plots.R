test_that("plot_cr_data() builds for every test type", {
  for (id in cr_test_types()$id) {
    d <- get(id, envir = asNamespace("crworkflows"))
    p <- plot_cr_data(d, id)
    expect_s3_class(p, "ggplot")
    expect_silent(invisible(ggplot2::ggplot_build(p)))
  }
})

test_that("controls are moved onto the log axis rather than dropped", {
  # A zero cannot be drawn on a log axis. Silently dropping the control would
  # remove the most important group from the figure, so it is repositioned.
  p <- plot_cr_data(algal_growth, "algal_growth", log_x = TRUE)
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[1]]), nrow(algal_growth))

  lowest <- min(algal_growth$conc[algal_growth$conc > 0])
  frame <- crworkflows:::cr_plot_frame(
    algal_growth, cr_test_type("algal_growth"), log_x = TRUE
  )
  expect_equal(min(frame$.x), lowest / 10)
  expect_equal(sum(frame$.control == "control"), sum(algal_growth$conc == 0))
})

test_that("binomial responses are plotted as proportions", {
  frame <- crworkflows:::cr_plot_frame(
    fish_larval_survival, cr_test_type("fish_larval_survival"), log_x = TRUE
  )
  expect_true(all(frame$.y >= 0 & frame$.y <= 1))
})

test_that("plot_cr_fit() adds the curve and labels the interval it drew", {
  skip_if_not_installed("drc")
  fit <- fit_cr_drc(algal_growth, "algal_growth", validate = FALSE)
  p <- plot_cr_fit(fit, algal_growth, "algal_growth")
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$caption, "confidence interval")
  expect_match(p$labels$caption, "drc")
  # points, ribbon, line
  expect_equal(length(p$layers), 3)
})

test_that("the drc predict wrapper does not emit the drc recycling warning", {
  skip_if_not_installed("drc")
  fit <- fit_cr_drc(algal_growth, "algal_growth", validate = FALSE)
  expect_silent(plot_cr_fit(fit, algal_growth, "algal_growth"))
})

test_that("a grid of one concentration does not break the averaged plot", {
  # drc's predict method drops the dimensions of its result for a single row,
  # which every column operation downstream then fails on.
  skip_if_not_installed("drc")
  ma <- suppressWarnings(fit_cr_drc_ma(algal_growth, "algal_growth", validate = FALSE))
  for (n in c(1L, 2L)) {
    p <- suppressWarnings(plot_cr_fit(ma, algal_growth, "algal_growth", n_grid = n))
    pred <- p$layers[[3]]$data
    expect_equal(nrow(pred), n)
    expect_true(all(is.finite(pred$.fit)))
  }
})
