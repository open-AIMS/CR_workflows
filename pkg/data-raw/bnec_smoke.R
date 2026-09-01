# Fit one short bayesnec model per response family, to confirm that the model
# specification the registry produces actually samples.
#
# Not part of the test suite: each fit compiles a Stan model and samples, which
# takes minutes rather than the milliseconds a unit test should. The unit tests
# check the specification (family, link, formula, model group); this script
# checks that the specification fits.
#
# Requires a working Stan toolchain.
#   Rscript pkg/data-raw/bnec_smoke.R

library(crworkflows)

# One test type per response family, rather than all fourteen, because the
# failure modes being checked for are properties of the family and link rather
# than of the individual test type.
cases <- c(
  algal_growth = "Gamma",
  fish_larval_survival = "binomial",
  daphnia_reproduction = "negbinomial",
  coral_bleaching = "Beta"
)

results <- list()

for (id in names(cases)) {
  cat("\n=== ", id, " (", cases[[id]], ") ===\n", sep = "")
  d <- get(id, envir = asNamespace("crworkflows"))

  # A single model rather than the full candidate set, and short chains: this
  # asks whether the specification samples, not whether the estimate is final.
  t0 <- Sys.time()
  fit <- fit_cr_bayesnec(
    d, id,
    model = "nec3param", validate = FALSE,
    seed = 42, chains = 2, iter = 2000, warmup = 1000, refresh = 0
  )
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  rh <- max(brms::rhat(fit$fit), na.rm = TRUE)
  ess <- min(brms::neff_ratio(fit$fit), na.rm = TRUE)
  res <- cr_results_table(fit, id, sample_id = "smoke")

  cat(sprintf("  fitted in %.0f s | max R-hat %.3f | min ESS ratio %.2f\n",
              elapsed, rh, ess))
  print(res[, c("estimate_type", "level", "estimate", "lower", "upper")])

  truth <- attr(d, "truth")
  cat("  generating parameters: ",
      paste(names(truth), unlist(truth), sep = " = ", collapse = ", "), "\n")

  results[[id]] <- list(elapsed = elapsed, rhat = rh, ess = ess, results = res)
}

cat("\n=== summary ===\n")
for (id in names(results)) {
  r <- results[[id]]
  cat(sprintf("%-24s %-12s %6.0f s  R-hat %.3f  %s\n", id, cases[[id]],
              r$elapsed, r$rhat,
              if (r$rhat < 1.05) "converged" else "NOT CONVERGED"))
}
