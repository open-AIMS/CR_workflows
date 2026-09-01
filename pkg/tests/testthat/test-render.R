test_that("cr_project_root() honours the override before searching", {
  # The override exists so that a document rendered from a scratch directory,
  # which contains no marker, still writes to the project's outputs tree.
  fake <- withr::local_tempdir()
  withr::with_envvar(c(CRWORKFLOWS_PROJECT_ROOT = fake), {
    expect_equal(
      normalizePath(cr_project_root(tempdir()), "/"),
      normalizePath(fake, "/")
    )
  })

  other <- withr::local_tempdir()
  withr::with_options(list(crworkflows.project_root = other), {
    expect_equal(
      normalizePath(cr_project_root(tempdir()), "/"),
      normalizePath(other, "/")
    )
  })

  # With no override and no marker, it must still fail rather than guess.
  bare <- withr::local_tempdir()
  withr::with_envvar(c(CRWORKFLOWS_PROJECT_ROOT = ""), {
    withr::with_options(list(crworkflows.project_root = NULL), {
      expect_error(cr_project_root(bare), "No .crproject marker")
    })
  })
})

test_that("cr_workflow_path() points at the document for each engine and group", {
  root <- withr::local_tempdir()
  for (id in cr_test_types()$id) {
    for (engine in c("drc", "bayesnec")) {
      p <- cr_workflow_path(engine, id, root = root)
      expect_match(p, paste0(engine, "/"), fixed = TRUE)
      expect_match(p, paste0(id, ".qmd"), fixed = TRUE)
      expect_match(p, cr_test_type(id)$group, fixed = TRUE)
    }
  }
  expect_error(cr_workflow_path("drc", "not_a_test", root = root), "Unknown test type")
})

test_that("cr_output_files() names the whole output set under one stem", {
  root <- withr::local_tempdir()
  f <- cr_output_files("drc", "algal_growth", "S1", root = root)
  expect_setequal(names(f), c("report", "figure", "table", "weights", "fit"))
  expect_true(all(grepl("S1_algal_growth_drc", f, fixed = TRUE)))
  expect_match(f[["report"]], "outputs/reports", fixed = TRUE)
  expect_match(f[["figure"]], "outputs/figures", fixed = TRUE)
  expect_match(f[["fit"]], "outputs/fits", fixed = TRUE)
})

test_that("write_cr_outputs() archives the weights only when averaging happened", {
  skip_if_not_installed("drc")
  root <- withr::local_tempdir()

  ma <- suppressWarnings(fit_cr_drc_ma(algal_growth, "algal_growth", validate = FALSE))
  res <- suppressWarnings(cr_results_table(ma, "algal_growth"))
  p <- plot_cr_data(algal_growth, "algal_growth")
  paths <- write_cr_outputs(ma, p, res, stem = "MA", root = root, save_fit = FALSE)
  expect_true("weights" %in% names(paths))
  w <- utils::read.csv(paths[["weights"]])
  expect_equal(names(w)[1:2], c("model", "weight"))
  expect_equal(sum(w$weight), 1, tolerance = 1e-8)

  single <- fit_cr_drc(algal_growth, "algal_growth", validate = FALSE)
  res1 <- suppressWarnings(cr_results_table(single, "algal_growth"))
  paths1 <- write_cr_outputs(single, p, res1, stem = "ONE", root = root, save_fit = FALSE)
  expect_false("weights" %in% names(paths1))
})

test_that("every workflow document is shipped inside the package", {
  # The package has to be able to run an analysis without the project being
  # cloned, which means the documents must be installed with it.
  for (id in cr_test_types()$id) {
    for (engine in c("drc", "bayesnec")) {
      rel <- file.path(
        "workflows", engine, cr_test_type(id)$group, paste0(id, ".qmd")
      )
      p <- system.file(rel, package = "crworkflows")
      expect_true(nzchar(p) && file.exists(p),
        info = paste("not shipped:", rel)
      )
    }
  }
})

test_that("the document is taken from the installed package when no project is present", {
  sandbox <- withr::local_tempdir()
  withr::with_dir(sandbox, {
    withr::with_envvar(c(CRWORKFLOWS_PROJECT_ROOT = ""), {
      p <- cr_workflow_path("drc", "algal_growth")
      expect_true(file.exists(p))
      expect_equal(
        normalizePath(p, "/"),
        normalizePath(
          system.file("workflows/drc/aquatic/algal_growth.qmd",
            package = "crworkflows"
          ), "/"
        )
      )
    })
  })
})

test_that("a project copy takes precedence over the installed one", {
  # A laboratory that has adapted a document must get its own version.
  root <- withr::local_tempdir()
  rel <- file.path("workflows", "drc", "aquatic")
  dir.create(file.path(root, rel), recursive = TRUE)
  local_doc <- file.path(root, rel, "algal_growth.qmd")
  writeLines("edited locally", local_doc)

  p <- cr_workflow_path("drc", "algal_growth", root = root)
  expect_equal(normalizePath(p, "/"), normalizePath(local_doc, "/"))
  expect_identical(readLines(p), "edited locally")
})

test_that("cr_output_root() falls back to the working directory", {
  sandbox <- withr::local_tempdir()
  withr::with_dir(sandbox, {
    withr::with_envvar(c(CRWORKFLOWS_PROJECT_ROOT = ""), {
      withr::with_options(list(crworkflows.project_root = NULL), {
        expect_equal(
          normalizePath(cr_output_root(), "/"),
          normalizePath(sandbox, "/")
        )
      })
    })
  })

  marked <- withr::local_tempdir()
  file.create(file.path(marked, ".crproject"))
  deep <- file.path(marked, "a")
  dir.create(deep)
  withr::with_dir(deep, {
    withr::with_envvar(c(CRWORKFLOWS_PROJECT_ROOT = ""), {
      expect_equal(
        normalizePath(cr_output_root(), "/"),
        normalizePath(marked, "/")
      )
    })
  })
})

test_that("an unknown test type is still an error, not a silent fallback", {
  expect_error(cr_workflow_path("drc", "not_a_test"), "Unknown test type")
})
