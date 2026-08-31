skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")

app_dir <- function() {
  d <- system.file("app", package = "crworkflows")
  if (!nzchar(d) || !file.exists(file.path(d, "app.R"))) {
    skip("Installed app directory not available")
  }
  d
}

test_that("the app source parses", {
  expect_silent(parse(file.path(app_dir(), "app.R")))
})

test_that("the app object can be constructed", {
  a <- shiny::shinyAppDir(app_dir())
  expect_s3_class(a, "shiny.appobj")
})

test_that("the checks gate blocks data that cannot be analysed", {
  # The gate is the point of the Checks tab: a structural problem must stop the
  # run rather than being discovered inside a background process minutes later.
  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    session$setInputs(
      test_type = "algal_growth", source = "example",
      sample_id = "unit", engine = "drc", average = TRUE
    )
    expect_false(blocked())
    expect_s3_class(checks(), "cr_check")
    expect_equal(checks()$issues, character(0))

    # A test type whose required columns the data do not have.
    session$setInputs(test_type = "fish_larval_survival")
    session$setInputs(source = "example")
    expect_false(blocked())

    # algal_growth data analysed as a binomial test type has no trials column.
    session$setInputs(test_type = "algal_growth")
    expect_false(blocked())
  })
})

test_that("mismatched data and test type are reported as blocked", {
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(algal_growth, csv, row.names = FALSE)

  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    # The uploaded algal data lack the alive and total columns that a binomial
    # test type requires, so the gate must close.
    session$setInputs(
      sample_id = "unit", engine = "drc", average = TRUE,
      test_type = "fish_larval_survival", source = "upload"
    )
    session$setInputs(file = list(
      name = basename(csv), datapath = csv, size = file.size(csv), type = "text/csv"
    ))
    expect_true(blocked())
    expect_s3_class(checks(), "error")
    expect_match(conditionMessage(checks()), "missing required column")
  })
})

test_that("the expected-columns hint follows the selected test type", {
  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    session$setInputs(test_type = "fish_larval_survival", source = "upload")
    expect_match(output$expected_cols, "alive")
    expect_match(output$expected_cols, "total")

    session$setInputs(test_type = "algal_growth")
    expect_match(output$expected_cols, "growth_rate")
    expect_false(grepl("trials", output$expected_cols, fixed = TRUE))
  })
})

test_that("no job is started while the data are blocked", {
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(algal_growth, csv, row.names = FALSE)

  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    session$setInputs(
      sample_id = "unit", engine = "drc", average = TRUE,
      test_type = "fish_larval_survival", source = "upload"
    )
    session$setInputs(file = list(
      name = basename(csv), datapath = csv, size = file.size(csv), type = "text/csv"
    ))
    expect_true(blocked())
    session$setInputs(run = 1)
    expect_length(jobs(), 0)
  })
})
