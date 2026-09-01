# The check is a diagnostic, so its own correctness matters in two places: it
# must not report a pass it did not establish, and it must not let a later stage
# be read as meaningful when an earlier one failed. The compiling stages are not
# run here; they take tens of seconds and depend on the machine's toolchain,
# which is what the function exists to report on.

test_that("the fast stages run and return the documented shape", {
  res <- check_stan_toolchain("cmdstanr", stages = c("packages", "toolchain"))
  expect_s3_class(res, "cr_stan_check")
  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("stage", "status", "detail", "seconds"))
  expect_true(all(res$status %in% c("pass", "fail", "skip")))
  expect_true(all(nzchar(res$detail)))
  expect_type(res$seconds, "double")
  expect_identical(attr(res, "backend"), "cmdstanr")
})

test_that("the cmdstan stage is dropped for rstan", {
  # rstan carries its own copy of the Stan library, so a CmdStan installation
  # says nothing about whether rstan will work.
  res <- check_stan_toolchain("rstan", stages = c("packages", "toolchain", "cmdstan"))
  expect_false("cmdstan" %in% res$stage)

  res2 <- check_stan_toolchain("cmdstanr", stages = c("packages", "toolchain", "cmdstan"))
  expect_true("cmdstan" %in% res2$stage)
})

test_that("stages after a failure are skipped rather than reported", {
  res <- check_stan_toolchain("cmdstanr")
  first_fail <- which(res$status == "fail")[1]
  if (is.na(first_fail)) {
    expect_true(all(res$status == "pass"))
  } else {
    later <- res$status[seq_len(nrow(res)) > first_fail]
    expect_true(all(later == "skip"),
      info = "a stage after a failure was run rather than skipped"
    )
  }
})

test_that("the stages are ordered by dependency", {
  # The order is the point of the check: a compiler failure must be reported
  # before a Stan failure, which must be reported before a brms failure.
  res <- check_stan_toolchain("cmdstanr")
  expected <- c("packages", "toolchain", "cmdstan", "stan", "brms")
  expect_equal(res$stage, expected[expected %in% res$stage])
})

test_that("an unknown backend or stage is rejected", {
  expect_error(check_stan_toolchain("nonsense"), "should be one of")
  expect_error(check_stan_toolchain("cmdstanr", stages = "nonsense"), "should be one of")
})

test_that("detail text is a single line", {
  # Messages from the Stan tooling arrive with embedded newlines, which would
  # break the aligned report.
  res <- check_stan_toolchain("cmdstanr")
  expect_false(any(grepl("\n", res$detail, fixed = TRUE)))
})

test_that("the report prints the status of every stage", {
  res <- check_stan_toolchain("cmdstanr", stages = c("packages", "toolchain"))
  out <- utils::capture.output(print(res))
  expect_match(out[1], "Stan toolchain check")
  expect_true(any(grepl("packages", out, fixed = TRUE)))
  expect_true(any(grepl("toolchain", out, fixed = TRUE)))
})
