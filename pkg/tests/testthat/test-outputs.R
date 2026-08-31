test_that("cr_output_path() creates the containing directory", {
  root <- withr::local_tempdir()
  p <- cr_output_path("figures", "x.png", root = root)
  expect_true(dir.exists(dirname(p)))
  expect_equal(basename(p), "x.png")
})

test_that("cr_project_root() finds the marker and errors above it", {
  root <- withr::local_tempdir()
  file.create(file.path(root, ".crproject"))
  deep <- file.path(root, "a", "b")
  dir.create(deep, recursive = TRUE)
  expect_equal(
    normalizePath(cr_project_root(deep), "/"),
    normalizePath(root, "/")
  )

  bare <- withr::local_tempdir()
  expect_error(cr_project_root(bare), "No .crproject marker")
})

test_that("write_cr_outputs() writes the full output set under one stem", {
  skip_if_not_installed("drc")
  root <- withr::local_tempdir()
  fit <- fit_cr_drc(algal_growth, "algal_growth", validate = FALSE)
  results <- cr_results_table(fit, "algal_growth", sample_id = "S1")
  p <- plot_cr_fit(fit, algal_growth, "algal_growth")

  paths <- write_cr_outputs(fit, p, results, stem = "S1_algal_growth_drc", root = root)
  expect_setequal(names(paths), c("figure", "table", "fit"))
  expect_true(all(file.exists(paths)))

  back <- utils::read.csv(paths[["table"]])
  expect_equal(nrow(back), nrow(results))
  expect_equal(back$sample_id, results$sample_id)
})

test_that("save_fit = FALSE omits the model object", {
  skip_if_not_installed("drc")
  root <- withr::local_tempdir()
  fit <- fit_cr_drc(algal_growth, "algal_growth", validate = FALSE)
  results <- cr_results_table(fit, "algal_growth")
  p <- plot_cr_data(algal_growth, "algal_growth")
  paths <- write_cr_outputs(fit, p, results, stem = "S2", root = root, save_fit = FALSE)
  expect_false("fit" %in% names(paths))
})

test_that("cr_session_record() reports the versions needed to reproduce a fit", {
  rec <- cr_session_record(seed = 7, engine = "drc")
  expect_equal(nrow(rec), 1)
  expect_true(all(c("r_version", "platform", "engine", "seed", "date") %in% names(rec)))
  expect_equal(rec$seed, 7)
  expect_equal(rec$engine, "drc")
  expect_true("crworkflows" %in% names(rec))
})

test_that("require_engine() names the missing package", {
  expect_error(require_engine("nonsense"), "'arg' should be one of")
  expect_true(require_engine("drc"))
})
