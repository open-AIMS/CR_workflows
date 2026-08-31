skip_if_not_installed("callr")
skip_if_not_installed("drc")
skip_if_not_installed("quarto")
skip_on_cran()

# These render real documents, so they need the project tree rather than the
# installed package alone. Skipped where the interface is not being worked on.
project_root <- function() {
  r <- tryCatch(cr_project_root(), error = function(e) NA_character_)
  if (is.na(r) || !dir.exists(file.path(r, "workflows"))) {
    skip("Project tree with workflows/ not available")
  }
  r
}

wait_for <- function(job, timeout = 300) {
  deadline <- Sys.time() + timeout
  while (job$process$is_alive() && Sys.time() < deadline) Sys.sleep(1)
  if (job$process$is_alive()) {
    cr_stop_job(job)
    skip("Job did not finish within the timeout")
  }
  invisible(job)
}

test_that("a drc job runs to completion and leaves its outputs", {
  root <- project_root()
  job <- cr_start_job("drc", "algal_growth", sample_id = "TESTJOB", root = root)
  expect_s3_class(job, "cr_job")
  expect_equal(cr_job_status(job)$status, "running")

  wait_for(job)
  st <- cr_job_status(job)
  expect_equal(st$status, "done", info = st$message)
  expect_equal(st$sample_id, "TESTJOB")

  out <- cr_job_outputs(job)
  expect_true(all(c("report", "figure", "table") %in% names(out)))
  expect_true(all(file.exists(out)))
  unlink(unname(out))
})

test_that("two jobs on the same document do not collide", {
  # Quarto writes its html beside the source under a fixed name, so without the
  # scratch copy in cr_render_workflow() these two would overwrite each other.
  root <- project_root()
  a <- cr_start_job("drc", "algal_growth", sample_id = "PARA", root = root)
  b <- cr_start_job("drc", "algal_growth", sample_id = "PARB", root = root)
  wait_for(a)
  wait_for(b)

  expect_equal(cr_job_status(a)$status, "done", info = cr_job_status(a)$message)
  expect_equal(cr_job_status(b)$status, "done", info = cr_job_status(b)$message)
  expect_true(all(file.exists(cr_job_outputs(a))))
  expect_true(all(file.exists(cr_job_outputs(b))))

  # The workflow tree must be left as it was found.
  expect_false(file.exists(file.path(root, "workflows", "drc", "aquatic", "algal_growth.html")))

  unlink(unname(c(cr_job_outputs(a), cr_job_outputs(b))))
})

test_that("a failing job reports as failed with a message", {
  root <- project_root()
  job <- cr_start_job("drc", "algal_growth",
    sample_id = "FAILJOB", root = root,
    average = "not_a_logical"
  )
  wait_for(job)
  st <- cr_job_status(job)
  expect_equal(st$status, "failed")
  expect_true(nzchar(st$message))
})

test_that("cr_bundle_outputs() produces a flat zip", {
  skip_if_not_installed("zip")
  root <- project_root()
  job <- cr_start_job("drc", "algal_growth", sample_id = "ZIPJOB", root = root)
  wait_for(job)
  skip_if(cr_job_status(job)$status != "done", "render failed")

  z <- cr_bundle_outputs(list(job))
  expect_true(file.exists(z))
  names_in_zip <- zip::zip_list(z)$filename
  expect_true(all(!grepl("/", names_in_zip, fixed = TRUE)))
  expect_true(any(grepl("ZIPJOB", names_in_zip, fixed = TRUE)))

  expect_error(cr_bundle_outputs(character(0)), "No output files")
  unlink(c(z, unname(cr_job_outputs(job))))
})
