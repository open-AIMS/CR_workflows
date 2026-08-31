test_that("the shipped example data pass their own checks", {
  for (id in cr_test_types()$id) {
    d <- get(id, envir = asNamespace("crworkflows"))
    chk <- check_cr_data(d, id)
    expect_s3_class(chk, "cr_check")
    expect_equal(chk$issues, character(0),
      info = paste(id, "reported:", paste(chk$issues, collapse = "; "))
    )
  }
})

test_that("a missing required column is an error, not a warning", {
  d <- algal_growth
  d$growth_rate <- NULL
  expect_error(check_cr_data(d, "algal_growth"), "missing required column")
})

test_that("structural problems in the concentration column are errors", {
  d <- algal_growth
  d$conc[1] <- -1
  expect_error(check_cr_data(d, "algal_growth"), "negative values")

  d <- algal_growth
  d$conc[1] <- NA
  expect_error(check_cr_data(d, "algal_growth"), "missing values")

  d <- algal_growth
  d$conc <- as.character(d$conc)
  expect_error(check_cr_data(d, "algal_growth"), "must be numeric")
})

test_that("a missing control is reported rather than silently accepted", {
  d <- algal_growth[algal_growth$conc > 0, ]
  chk <- check_cr_data(d, "algal_growth")
  expect_match(paste(chk$issues, collapse = " "), "No zero-concentration control")
})

test_that("thin designs are reported", {
  d <- algal_growth[algal_growth$conc %in% c(0, 1, 32), ]
  chk <- check_cr_data(d, "algal_growth")
  expect_match(paste(chk$issues, collapse = " "), "non-control concentrations")
})

test_that("a Gamma-incompatible response is reported", {
  d <- algal_growth
  d$growth_rate[1] <- 0
  chk <- check_cr_data(d, "algal_growth")
  expect_match(paste(chk$issues, collapse = " "), "Gamma likelihood")
})

test_that("successes exceeding trials is reported", {
  d <- fish_larval_survival
  d$alive[1] <- d$total[1] + 5
  chk <- check_cr_data(d, "fish_larval_survival")
  expect_match(paste(chk$issues, collapse = " "), "Successes exceed trials")
})

test_that("a response that has not reached a plateau is flagged as extrapolation", {
  # Truncating to the lower half of the series removes the plateau, so the
  # highest tested concentration is still above half the control response.
  d <- algal_growth[algal_growth$conc <= 2, ]
  chk <- check_cr_data(d, "algal_growth", min_conc_levels = 1)
  expect_match(paste(chk$issues, collapse = " "), "extrapolations")
})

test_that("summarise_design() puts binomial responses on the proportion scale", {
  s <- summarise_design(fish_larval_survival, "fish_larval_survival")
  expect_true(all(s$mean >= 0 & s$mean <= 1))
  expect_equal(s$conc, sort(unique(fish_larval_survival$conc)))
  expect_true(all(s$n > 0))
})

test_that("cr_check prints its issues", {
  d <- algal_growth[algal_growth$conc > 0, ]
  expect_output(print(check_cr_data(d, "algal_growth")), "control")
  expect_output(print(check_cr_data(algal_growth, "algal_growth")), "no issues identified")
})
