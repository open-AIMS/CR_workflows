# Run the bayesnec analysis path for one test type without Quarto.
#
# The bayesnec workflow documents need both a Stan toolchain and the Quarto CLI.
# Where only the toolchain is available, this script runs the same sequence of
# calls the template makes -- checks, fit, convergence, weights, estimates,
# figure, outputs -- so the analysis path can be verified before the documents
# are rendered elsewhere.
#
# It is a check, not a substitute for the workflow: it produces the figure, the
# table and the fit, but not the report.
#
# Usage, from the project root:
#   Rscript workflows/_check_bayesnec.R algal_growth
#   Rscript workflows/_check_bayesnec.R algal_growth nec3param   # single model

library(crworkflows)

args <- commandArgs(trailingOnly = TRUE)
test_type <- if (length(args) >= 1) args[[1]] else "algal_growth"
model <- if (length(args) >= 2) args[[2]] else NULL
seed <- 20260831

options(crworkflows.output_root = file.path(cr_output_root(), "outputs"))
set.seed(seed)

tt <- cr_test_type(test_type)
cat("test type      : ", tt$id, " (", tt$label, ")\n", sep = "")
cat("family         : ", tt$bnec_family, "\n", sep = "")
cat("candidate models: ", model %||% tt$bnec_model, "\n\n", sep = "")

cr_data <- get(test_type, envir = asNamespace("crworkflows"))

cat("--- data checks ---\n")
print(check_cr_data(cr_data, test_type))

cat("\n--- fitting ---\n")
t0 <- Sys.time()
fit <- fit_cr_bayesnec(
  cr_data, test_type,
  model = model, validate = FALSE,
  seed = seed, chains = 4, iter = 4000, warmup = 2000, refresh = 0
)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
cat(sprintf("fitted in %.1f minutes\n", elapsed))

cat("\n--- convergence ---\n")
if (inherits(fit, "bayesmanecfit")) {
  rhats <- vapply(fit$mod_fits, function(m) max(brms::rhat(m$fit), na.rm = TRUE), numeric(1))
  print(round(rhats, 4))
  cat("worst R-hat across models:", round(max(rhats), 4),
      if (max(rhats) < 1.05) "(converged)" else "(NOT CONVERGED)", "\n")
  cat("\n--- model weights ---\n")
  print(cr_model_weights(fit), row.names = FALSE, digits = 3)
} else {
  cat("single model:", fit$model, " max R-hat",
      round(max(brms::rhat(fit$fit), na.rm = TRUE), 4), "\n")
}

cat("\n--- estimates ---\n")
results <- cr_results_table(fit, test_type, sample_id = "check")
print(results[, c("estimate_type", "level", "estimate", "lower", "upper")])

truth <- attr(cr_data, "truth")
cat("\ngenerating parameters: ",
    paste(names(truth), unlist(truth), sep = " = ", collapse = ", "), "\n")

cat("\n--- outputs ---\n")
paths <- write_cr_outputs(
  fit = fit,
  plot = plot_cr_fit(fit, cr_data, test_type),
  results = results,
  stem = paste("check", test_type, "bayesnec", sep = "_")
)
print(paths)
cat("\ndone\n")
