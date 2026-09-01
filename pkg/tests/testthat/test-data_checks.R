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

test_that("a response that rises with concentration is reported", {
  # The standard operating procedure names supplying the affected count instead
  # of its complement as one of three decisions that are hard to correct later,
  # and it is the commonest of the three. Before this check the inverted data
  # passed every check in silence and the failure surfaced much later, as an
  # error from the estimate stage that named an unrelated cause.
  d <- daphnia_immobilisation
  d$mobile <- d$immobile
  chk <- check_cr_data(d, "daphnia_immobilisation")
  expect_match(paste(chk$issues, collapse = " "), "does not decline")
  expect_match(paste(chk$issues, collapse = " "), "mobile", fixed = TRUE)

  # The control mean of the inverted data is exactly zero, so the comparison
  # cannot be made as a ratio. That is the ordinary case, not an edge case: an
  # undamaged control has an affected count of zero.
  e <- crworkflows:::response_extremes(
    d$mobile, d, cr_test_type("daphnia_immobilisation")
  )
  expect_equal(unname(e[["control"]]), 0)

  # It must not fire on data that decline, including the hormetic test types
  # whose response rises above the control at low concentrations before falling.
  for (id in cr_test_types()$id) {
    d <- get(id, envir = asNamespace("crworkflows"))
    issues <- check_cr_data(d, id)$issues
    expect_false(any(grepl("does not decline", issues)), info = id)
  }
})

test_that("the extrapolation threshold comes from the registry, not a constant", {
  # The check asks whether the largest observed effect reaches the largest ECx
  # the test type reports. Fixing it at fifty per cent would be wrong for a test
  # type reporting only EC10 and EC20.
  tt <- cr_test_type("algal_growth")
  ratio_of <- function(d) {
    e <- crworkflows:::response_extremes(d$growth_rate, d, tt)
    e[["highest"]] / e[["control"]]
  }

  truncated <- algal_growth[algal_growth$conc <= 2, ]
  expect_gt(ratio_of(truncated), 0.5)
  chk <- check_cr_data(truncated, "algal_growth", min_conc_levels = 1)
  expect_match(paste(chk$issues, collapse = " "), "does not reach the EC50")

  full <- check_cr_data(algal_growth, "algal_growth")
  expect_false(any(grepl("does not reach the EC", full$issues)))
})
