# Build one workflow document per test type per engine from the templates.
#
# The 28 workflow documents are generated rather than written by hand so that a
# change to the analysis procedure is made once, in the template, and reaches
# every test type. A laboratory that needs to depart from the standard procedure
# for one test type should edit that generated document and record the departure
# in it; re-running this script would overwrite that edit, so the script refuses
# to overwrite a document that has been modified unless `force = TRUE`.
#
# Run from the project root:  source("workflows/_build_workflows.R")

build_workflows <- function(root = ".", force = FALSE) {
  source(file.path(root, "pkg", "R", "test_types.R"), local = TRUE)
  reg <- cr_test_types()

  templates <- c(
    bayesnec = file.path(root, "workflows", "_templates", "bayesnec-workflow-template.qmd"),
    drc = file.path(root, "workflows", "_templates", "drc-workflow-template.qmd")
  )
  manifest_path <- file.path(root, "workflows", "_manifest.csv")
  manifest <- if (file.exists(manifest_path)) {
    utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  } else {
    data.frame(
      path = character(0), engine = character(0), test_type = character(0),
      template_hash = character(0), file_hash = character(0),
      stringsAsFactors = FALSE
    )
  }

  rows <- list()
  for (engine in names(templates)) {
    tmpl <- readLines(templates[[engine]], warn = FALSE)
    tmpl_hash <- digest_lines(tmpl)
    for (i in seq_len(nrow(reg))) {
      r <- reg[i, , drop = FALSE]
      out_dir <- file.path(root, "workflows", engine, r$group)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      out_path <- file.path(out_dir, paste0(r$id, ".qmd"))

      content <- tmpl
      for (key in c("ID", "LABEL", "GUIDELINE", "ENDPOINT", "GROUP")) {
        value <- switch(key,
          ID = r$id, LABEL = r$label, GUIDELINE = r$guideline,
          ENDPOINT = r$endpoint, GROUP = r$group
        )
        content <- gsub(paste0("{{", key, "}}"), value, content, fixed = TRUE)
      }

      if (file.exists(out_path) && !force) {
        prev <- manifest[manifest$path == relative_path(out_path, root), , drop = FALSE]
        current_hash <- digest_lines(readLines(out_path, warn = FALSE))
        if (nrow(prev) == 1 && !identical(prev$file_hash, current_hash)) {
          message("skipped (edited since generation): ", out_path)
          rows[[length(rows) + 1]] <- prev
          next
        }
      }

      writeLines(content, out_path)
      rows[[length(rows) + 1]] <- data.frame(
        path = relative_path(out_path, root), engine = engine,
        test_type = r$id, template_hash = tmpl_hash,
        file_hash = digest_lines(content), stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(rbind, rows)
  utils::write.csv(out, manifest_path, row.names = FALSE)

  # The documents are copied into the package as well as written to the editable
  # tree. Shipping them means an installed package can run an analysis on its
  # own, without the project being cloned; copying them here rather than by hand
  # means the shipped copies cannot fall behind the ones that were just built.
  ship_workflows(root)

  message("built ", nrow(out), " workflow documents")
  invisible(out)
}

ship_workflows <- function(root = ".") {
  src <- file.path(root, "workflows")
  dest <- file.path(root, "pkg", "inst", "workflows")
  unlink(dest, recursive = TRUE)
  for (engine in c("drc", "bayesnec")) {
    for (d in list.dirs(file.path(src, engine), recursive = FALSE)) {
      target <- file.path(dest, engine, basename(d))
      dir.create(target, recursive = TRUE, showWarnings = FALSE)
      file.copy(list.files(d, pattern = "[.]qmd$", full.names = TRUE), target,
        overwrite = TRUE
      )
    }
  }
  n <- length(list.files(dest, pattern = "[.]qmd$", recursive = TRUE))
  message("shipped ", n, " workflow documents into the package")
  invisible(dest)
}

# A content hash, used only to detect hand edits. tools::md5sum() works on files
# rather than character vectors, so the content is written to a temporary file.
digest_lines <- function(x) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeLines(x, tmp)
  unname(tools::md5sum(tmp))
}

relative_path <- function(path, root) {
  sub(paste0("^", gsub("\\\\", "/", normalizePath(root, "/", TRUE)), "/"), "",
      gsub("\\\\", "/", normalizePath(path, "/", TRUE)))
}

# Sourcing this file builds the documents. It is a build script, not a library.
build_workflows()
