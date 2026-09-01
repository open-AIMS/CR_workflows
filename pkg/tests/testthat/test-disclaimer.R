test_that("cr_disclaimer() states what it has to state", {
  plain <- cr_disclaimer()
  expect_type(plain, "character")
  expect_length(plain, 1L)
  # The three claims the disclaimer exists to make. Asserted individually so
  # that a rewording cannot quietly drop one of them.
  expect_match(plain, "generative AI", fixed = TRUE)
  expect_match(plain, "not been thoroughly checked", fixed = TRUE)
  expect_match(plain, "work in progress", fixed = TRUE)
  expect_match(plain, "treated with caution", fixed = TRUE)
  expect_false(grepl("\n", plain, fixed = TRUE))
})

test_that("the markdown form is a Quarto callout wrapping the same text", {
  md <- cr_disclaimer("markdown")
  expect_match(md, "::: {.callout-warning}", fixed = TRUE)
  expect_match(md, "## Disclaimer", fixed = TRUE)
  expect_true(grepl(cr_disclaimer(), md, fixed = TRUE))
  # The callout must be closed, or everything after it in the report is absorbed
  # into the warning block.
  expect_match(md, ":::\n$")
})

test_that("cr_disclaimer() rejects an unknown style", {
  expect_error(cr_disclaimer("shouty"), "should be one of")
})

test_that("every workflow document carries the disclaimer", {
  # The report is the record of the analysis, so it is the one place the
  # statement must not be missing. Both the shipped copies and, where the project
  # tree is present, the editable ones are checked.
  for (id in cr_test_types()$id) {
    for (engine in c("drc", "bayesnec")) {
      rel <- file.path("workflows", engine, cr_test_type(id)$group,
                       paste0(id, ".qmd"))
      p <- system.file(rel, package = "crworkflows")
      skip_if(!nzchar(p) || !file.exists(p), "workflow documents not installed")
      txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
      expect_match(txt, 'cr_disclaimer("markdown")', fixed = TRUE,
        info = paste("no disclaimer in", rel)
      )
    }
  }
})

test_that("the application displays the disclaimer", {
  skip_if_not_installed("shiny")
  d <- system.file("app", package = "crworkflows")
  skip_if(!nzchar(d) || !file.exists(file.path(d, "app.R")),
    "installed app directory not available")
  txt <- paste(readLines(file.path(d, "app.R"), warn = FALSE), collapse = "\n")
  expect_match(txt, "cr_disclaimer()", fixed = TRUE)
  expect_match(txt, "disclaimer_banner", fixed = TRUE)
})
