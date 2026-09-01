# The check is a diagnostic, so its own correctness matters in two places: it
# must not report a pass it did not establish, and it must not let a later stage
# be read as meaningful when an earlier one failed. The compiling stages are not
# run here; they take tens of seconds and depend on the machine's toolchain,
# which is what the function exists to report on.
#
# "Not run here" has to be enforced rather than assumed. Calling the function
# with its default stages compiles and samples a Stan model and then fits a brms
# model, so on a machine whose toolchain works the suite would compile Stan
# models on every run. Every call below therefore names the stages it wants, and
# the ordering and skipping logic is exercised against a stubbed stage runner
# instead of against a real toolchain, which also means the result does not
# depend on whether the machine running the tests happens to have one.

# Replaces the stage functions with ones returning a scripted status, so the
# control flow of check_stan_toolchain() can be tested without any of the work.
with_stubbed_stages <- function(statuses, expr) {
  ns <- asNamespace("crworkflows")
  names <- c("stan_stage_packages", "stan_stage_toolchain", "stan_stage_cmdstan",
             "stan_stage_stan", "stan_stage_brms")
  keys <- c("packages", "toolchain", "cmdstan", "stan", "brms")
  originals <- lapply(names, get, envir = ns)
  for (i in seq_along(names)) {
    local({
      key <- keys[[i]]
      unlockBinding(names[[i]], ns)
      assign(names[[i]], function(...) {
        list(status = statuses[[key]], detail = paste("stub", key))
      }, envir = ns)
    })
  }
  on.exit({
    for (i in seq_along(names)) {
      assign(names[[i]], originals[[i]], envir = ns)
      lockBinding(names[[i]], ns)
    }
  })
  force(expr)
}

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
  # A failure in the middle of the sequence: everything after it must be
  # reported as skipped, because its result would not be interpretable.
  res <- with_stubbed_stages(
    list(packages = "pass", toolchain = "fail", cmdstan = "pass",
         stan = "pass", brms = "pass"),
    check_stan_toolchain("cmdstanr")
  )
  expect_equal(res$status, c("pass", "fail", "skip", "skip", "skip"))
  expect_true(all(grepl("earlier stage failed", res$detail[res$status == "skip"])))
})

test_that("every stage runs when none fails", {
  res <- with_stubbed_stages(
    list(packages = "pass", toolchain = "pass", cmdstan = "pass",
         stan = "pass", brms = "pass"),
    check_stan_toolchain("cmdstanr")
  )
  expect_true(all(res$status == "pass"))
  expect_equal(nrow(res), 5L)
})

test_that("the stages are ordered by dependency", {
  # The order is the point of the check: a compiler failure must be reported
  # before a Stan failure, which must be reported before a brms failure.
  res <- with_stubbed_stages(
    list(packages = "pass", toolchain = "pass", cmdstan = "pass",
         stan = "pass", brms = "pass"),
    check_stan_toolchain("cmdstanr")
  )
  expect_equal(res$stage, c("packages", "toolchain", "cmdstan", "stan", "brms"))
})

test_that("the rstan toolchain stage reports a failure instead of raising one", {
  # The regression guard. When pkgbuild::has_build_tools() throws rather than
  # returning FALSE, the stage used to read a variable that the failing branch
  # never assigned and abort with "object 'val' not found", so the diagnostic
  # whose purpose is to report a broken toolchain failed on exactly the machines
  # it was written for.
  skip_if_not_installed("pkgbuild")
  ns <- asNamespace("pkgbuild")
  original <- get("has_build_tools", envir = ns)
  unlockBinding("has_build_tools", ns)
  assign("has_build_tools", function(...) stop("no toolchain"), envir = ns)
  on.exit({
    assign("has_build_tools", original, envir = ns)
    lockBinding("has_build_tools", ns)
  })

  res <- crworkflows:::stan_stage_toolchain("rstan")
  expect_equal(res$status, "fail")
  expect_match(res$detail, "No usable build tools")
})

test_that("an unknown backend or stage is rejected", {
  expect_error(check_stan_toolchain("nonsense"), "should be one of")
  expect_error(check_stan_toolchain("cmdstanr", stages = "nonsense"), "should be one of")
})

test_that("detail text is a single line", {
  # Messages from the Stan tooling arrive with embedded newlines, which would
  # break the aligned report.
  res <- check_stan_toolchain("cmdstanr", stages = c("packages", "toolchain"))
  expect_false(any(grepl("\n", res$detail, fixed = TRUE)))
  expect_false(any(grepl("\n", crworkflows:::stan_row(
    "stan", "fail", "first line\nsecond line", 1
  )$detail, fixed = TRUE)))
})

test_that("the report prints the status of every stage", {
  res <- check_stan_toolchain("cmdstanr", stages = c("packages", "toolchain"))
  out <- utils::capture.output(print(res))
  expect_match(out[1], "Stan toolchain check")
  expect_true(any(grepl("packages", out, fixed = TRUE)))
  expect_true(any(grepl("toolchain", out, fixed = TRUE)))
})
