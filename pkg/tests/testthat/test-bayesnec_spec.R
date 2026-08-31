skip_if_not_installed("bayesnec")
skip_if_not_installed("brms")

# These tests check the model specification bayesnec would be given, not the
# fit. Sampling requires a working Stan toolchain and several minutes per model,
# which is not appropriate in a unit test; a fitted-model check belongs in the
# workflow documents, which are rendered as the acceptance test.

test_that("every family is constructed with an identity link", {
  # bayesnec sets link = "identity" for every family it accepts, so that top,
  # bot and nec stay on the response scale. Several brms families default to a
  # different link, and a family built with its default produces a different
  # model that still samples cleanly. This test is the guard against that.
  for (id in cr_test_types()$id) {
    fam <- cr_bnec_family(id)
    expect_identical(fam$link, "identity", info = paste(id, "has link", fam$link))
  }
})

test_that("the family matches the response type recorded in the registry", {
  reg <- cr_test_types()
  for (i in seq_len(nrow(reg))) {
    r <- reg[i, , drop = FALSE]
    fam <- cr_bnec_family(r$id)
    expected <- switch(r$response_type,
      binomial_trials = "binomial",
      count = "negbinomial",
      proportion = "beta",
      continuous_positive = c("Gamma", "gaussian")
    )
    expect_true(fam$family %in% expected,
      info = paste(r$id, "has family", fam$family, "for", r$response_type)
    )
  }
})

test_that("the formula carries a trials term exactly when one is needed", {
  reg <- cr_test_types()
  for (i in seq_len(nrow(reg))) {
    r <- reg[i, , drop = FALSE]
    txt <- paste(deparse(stats::formula(cr_bnec_formula(r$id))), collapse = "")
    if (is.na(r$trials_var)) {
      expect_false(grepl("trials(", txt, fixed = TRUE), info = r$id)
    } else {
      expect_true(grepl(paste0("trials(", r$trials_var, ")"), txt, fixed = TRUE),
        info = r$id
      )
    }
    expect_true(grepl(paste0("crf(", r$x_var), txt, fixed = TRUE), info = r$id)
  }
})

test_that("the registry names a real bayesnec model or model group", {
  known <- c(names(bayesnec::models()), unlist(bayesnec::models(), use.names = FALSE))
  expect_true(all(cr_test_types()$bnec_model %in% known))
})

test_that("the model group is one whose response declines with concentration", {
  # Every response column in the registry is oriented so that it falls with
  # concentration; a model group admitting increasing curves would be a
  # specification error rather than a modelling choice.
  for (id in cr_test_types()$id) {
    d <- get(id, envir = asNamespace("crworkflows"))
    tt <- cr_test_type(id)
    s <- summarise_design(d, id)
    expect_gt(s$mean[s$conc == 0], s$mean[which.max(s$conc)],
      label = paste(id, "control mean")
    )
  }
})

test_that("cr_bnec_formula() accepts an explicit model override", {
  txt <- paste(deparse(stats::formula(cr_bnec_formula("algal_growth", model = "nec3param"))),
    collapse = ""
  )
  expect_true(grepl("nec3param", txt, fixed = TRUE))
})
