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

test_that("the shipped example data pass the gate for every test type", {
  # The gate is the point of the Checks tab: a structural problem must stop the
  # run rather than being discovered inside a background process minutes later.
  # With the example source the data always match the selected test type, so
  # what this establishes is the other half: the gate must not close on data
  # that are correct.
  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    session$setInputs(source = "example", sample_id = "unit",
                      engine = "drc", average = TRUE)
    for (id in cr_test_types()$id) {
      session$setInputs(test_type = id)
      expect_false(blocked(), label = paste(id, "blocked"))
      expect_s3_class(checks(), "cr_check")
      expect_equal(checks()$issues, character(0), info = id)
    }
  })
})

test_that("choosing upload before a file is not reported as a fault", {
  # Switching the source to upload leaves the app with nothing to check until a
  # file is chosen. That is an ordinary intermediate state: reporting it as data
  # that cannot be analysed sends the analyst looking for a problem in a file
  # they have not yet supplied.
  shiny::testServer(shiny::shinyAppDir(app_dir()), {
    session$setInputs(test_type = "algal_growth", sample_id = "unit",
                      engine = "drc", average = TRUE, source = "upload")
    expect_true(awaiting_file())
    expect_false(blocked())
    # renderUI returns a list of html and dependencies, so the markup is taken
    # from the html element rather than from the list as a whole.
    expect_match(output$checks$html, "Choose a csv")
    expect_false(grepl("alert-danger", output$checks$html, fixed = TRUE))
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
    expect_false(awaiting_file())
    expect_true(blocked())
    expect_s3_class(checks(), "error")
    expect_match(conditionMessage(checks()), "missing required column")
    expect_match(output$checks$html, "cannot be analysed")
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
