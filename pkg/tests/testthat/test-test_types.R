test_that("the registry is internally consistent", {
  reg <- cr_test_types()

  expect_gt(nrow(reg), 0)
  expect_false(anyDuplicated(reg$id) > 0)
  expect_true(all(reg$response_type %in%
    c("continuous_positive", "binomial_trials", "count", "proportion")))
  expect_true(all(reg$x_var == "conc"))
  expect_true(all(nzchar(reg$label)))
  expect_true(all(nzchar(reg$conc_units)))
  expect_type(reg$hormesis, "logical")

  # A trials column is required for binomial test types and meaningless
  # otherwise; getting this wrong silently changes the likelihood.
  binom <- reg$response_type == "binomial_trials"
  expect_true(all(!is.na(reg$trials_var[binom])))
  expect_true(all(is.na(reg$trials_var[!binom])))
})

test_that("every registry row has a matching dataset with the expected columns", {
  reg <- cr_test_types()
  for (i in seq_len(nrow(reg))) {
    r <- reg[i, , drop = FALSE]
    d <- get(r$id, envir = asNamespace("crworkflows"))
    expect_s3_class(d, "data.frame")
    needed <- c(r$x_var, r$y_var, if (!is.na(r$trials_var)) r$trials_var)
    expect_true(all(needed %in% names(d)),
      info = paste(r$id, "is missing", paste(setdiff(needed, names(d)), collapse = ", "))
    )
    expect_true(any(d[[r$x_var]] == 0), info = paste(r$id, "has no control"))
  }
})

test_that("cr_test_type() rejects an unknown identifier", {
  expect_error(cr_test_type("not_a_test"), "Unknown test type")
  expect_error(cr_test_type(c("algal_growth", "plant_growth")), "Unknown test type")
})

test_that("cr_ecx_targets() parses the registry string", {
  expect_equal(cr_ecx_targets("algal_growth"), c(10, 20, 50))
  expect_type(cr_ecx_targets("daphnia_immobilisation"), "double")
})
