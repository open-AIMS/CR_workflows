# Generate docs/test-type-catalogue.md from the registry and the shipped data.
#
# Generated rather than written by hand so that the catalogue cannot describe a
# test type differently from the registry that drives the analysis.
#
# Run from the project root:  source("docs/_build_catalogue.R")

# Small cardinals read better written out in this prose. Anything larger than
# the list is given as a numeral, which is the usual convention and avoids
# maintaining a table of number words for counts that will not arise.
spell <- function(n) {
  words <- c("one", "two", "three", "four", "five", "six", "seven", "eight",
             "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
             "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
  if (n >= 1 && n <= length(words)) words[[n]] else as.character(n)
}

build_catalogue <- function(root = ".") {
  source(file.path(root, "pkg", "R", "test_types.R"), local = TRUE)
  reg <- cr_test_types()

  section <- function(r) {
    d <- utils::read.csv(
      file.path(root, "pkg", "inst", "extdata", paste0(r$id, ".csv"))
    )
    concs <- sort(unique(d$conc))
    exposed <- concs[concs > 0]
    reps <- table(d$conc)

    c(
      paste0("## `", r$id, "`"),
      "",
      paste0("**", r$label, "** — ", r$guideline, "."),
      "",
      paste0("Endpoint: ", r$endpoint, ", recorded in column `", r$y_var, "`",
        if (!is.na(r$trials_var)) paste0(" out of `", r$trials_var, "`") else "",
        ". Concentration in ", r$conc_units, "."
      ),
      "",
      "| | |",
      "|---|---|",
      paste0("| Required columns | ", paste0("`", c(r$x_var, r$y_var,
        if (!is.na(r$trials_var)) r$trials_var), "`", collapse = ", "), " |"),
      paste0("| Response type | ", r$response_type, " |"),
      paste0("| Example design | ", length(exposed), " exposed concentrations plus a control, ",
        min(reps), if (min(reps) == max(reps)) "" else paste0(" to ", max(reps)),
        " replicates each |"),
      paste0("| Concentration range | ", format(min(exposed)), " to ",
        format(max(exposed)), " ", r$conc_units, " |"),
      paste0("| bayesnec family | `", r$bnec_family, "(link = \"identity\")` |"),
      paste0("| bayesnec candidate models | `", r$bnec_model, "` |"),
      paste0("| drc default mean function | `", r$drc_fct, "`, `type = \"", r$drc_type, "\"` |"),
      paste0("| Hormesis admitted | ", if (r$hormesis) "yes" else "no", " |"),
      paste0("| ECx reported | ", gsub(",", ", ", r$ecx_targets), " |"),
      "",
      paste0("Workflows: `workflows/bayesnec/", r$group, "/", r$id, ".qmd`, ",
        "`workflows/drc/", r$group, "/", r$id, ".qmd`"),
      ""
    )
  }

  groups <- unique(reg$group)
  body <- unlist(lapply(groups, function(g) {
    rows <- reg[reg$group == g, , drop = FALSE]
    c(
      paste0("# ", tools::toTitleCase(g)),
      "",
      unlist(lapply(seq_len(nrow(rows)), function(i) section(rows[i, , drop = FALSE])))
    )
  }))

  header <- c(
    "# Test type catalogue",
    "",
    "Generated from the registry in `pkg/R/test_types.R` and the shipped example",
    "datasets by `docs/_build_catalogue.R`. Do not edit by hand: edit the registry",
    "and re-run that script.",
    "",
    "The design given for each test type is the design of the shipped example",
    "dataset, which follows the guideline but is simulated, not measured. It is a",
    "guide to the expected input format, not a specification of the test.",
    "",
    # Written out in words to match the register of the surrounding prose, and
    # counted from the registry so it cannot say something the registry does not.
    paste0(spell(nrow(reg)), " test types across ", spell(length(groups)),
      " groups: ", paste(groups, collapse = ", "), "."),
    ""
  )

  # LF on every platform, so that running this on Windows and on Linux produces
  # the same file rather than one that differs only in its line endings.
  out <- file.path(root, "docs", "test-type-catalogue.md")
  con <- file(out, open = "wb")
  writeLines(c(header, body), con, sep = "\n")
  close(con)
  message("wrote ", out)
  invisible(out)
}

build_catalogue()
