# Render the bayesnec workflow documents under WSL using the cmdstanr backend.
#
# The bayesnec documents need a Stan toolchain and the Quarto CLI. Where Windows
# has no Rtools matching the installed R, both are available under WSL, and
# `C:/Rworking` and `/home/rfisher/Rworking_wsl` are the same location, so no
# files are copied.
#
# These renders are slow: each document fits a whole candidate set, which took
# about 16 minutes for algal_growth at twelve models. The hormetic test types
# use the larger `all` group and take longer. Run the whole set only when there
# is time for it.
#
# Usage, from the project root inside WSL:
#   Rscript workflows/_render_bayesnec_wsl.R                   # every test type
#   Rscript workflows/_render_bayesnec_wsl.R algal_growth      # one test type
#   Rscript workflows/_render_bayesnec_wsl.R algal_growth coral_bleaching

source("workflows/_render.R")
source("pkg/R/test_types.R")

args <- commandArgs(trailingOnly = TRUE)
ids <- if (length(args)) args else cr_test_types()$id

cat("rendering", length(ids), "bayesnec document(s) with the cmdstanr backend\n\n")

results <- data.frame(
  test_type = ids, minutes = NA_real_, status = NA_character_,
  stringsAsFactors = FALSE
)

for (i in seq_along(ids)) {
  id <- ids[[i]]
  cat("[", i, "/", length(ids), "] ", id, " ... ", sep = "")
  t0 <- Sys.time()
  # One failure must not abandon the rest of the batch; the status is recorded
  # and the summary at the end says which documents need attention.
  out <- tryCatch(
    {
      # The serialised model object is not kept. A model-averaged bayesnec fit
      # is around 400 MB, so rendering the whole set with save_fit on writes
      # roughly 5 GB. These are demonstration renders of the example data; the
      # report records the seed and versions needed to refit. Set save_fit to
      # TRUE for a real sample whose fit has to be archived.
      render_workflow("bayesnec", id, backend = "cmdstanr", save_fit = FALSE)
      "ok"
    },
    error = function(e) paste("FAILED:", conditionMessage(e))
  )
  results$minutes[i] <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  results$status[i] <- out
  cat(sprintf("%s (%.1f min)\n", out, results$minutes[i]))
}

cat("\n=== summary ===\n")
print(results, row.names = FALSE)
cat("\nrendered", sum(results$status == "ok"), "of", nrow(results),
    "in", sprintf("%.1f", sum(results$minutes)), "minutes\n")
