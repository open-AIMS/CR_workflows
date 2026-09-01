# Render the bayesnec workflow documents in bulk, using the cmdstanr backend.
#
# Needs a working Stan toolchain and the Quarto CLI.
#
# These renders are slow: each document fits a whole candidate set, which takes
# eight to fifteen minutes per test type. The hormetic test types use the larger
# `all` model group and take longest. Run the whole set only when there is time
# for it.
#
# Usage, from the project root:
#   Rscript workflows/_render_bayesnec.R                   # every test type
#   Rscript workflows/_render_bayesnec.R algal_growth      # one test type
#   Rscript workflows/_render_bayesnec.R algal_growth coral_bleaching

source("workflows/_render.R")
library(crworkflows)

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
