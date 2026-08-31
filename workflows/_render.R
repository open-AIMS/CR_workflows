# Render workflow documents and collect the reports into outputs/reports.
#
# Quarto writes its html next to the source document. A laboratory needs the
# reports in one place, alongside the figures and tables the same run produced,
# so the rendered file is moved after rendering rather than left in the
# workflows tree.
#
# Examples, run from the project root:
#   source("workflows/_render.R")
#   render_workflow("drc", "algal_growth")
#   render_workflow("drc", "algal_growth", sample_id = "S2026-0142",
#                   data_file = "path/to/sample.csv")
#   render_all("drc")

render_workflow <- function(engine = c("drc", "bayesnec"), test_type,
                            sample_id = "example", data_file = NULL,
                            root = ".", ...) {
  engine <- match.arg(engine)
  qmd <- workflow_path(engine, test_type, root)
  if (!file.exists(qmd)) {
    stop("No ", engine, " workflow found for test type '", test_type, "'.",
      call. = FALSE
    )
  }

  execute_params <- c(
    list(sample_id = sample_id, test_type = test_type),
    if (!is.null(data_file)) list(data_file = normalizePath(data_file, "/")),
    list(...)
  )

  quarto::quarto_render(qmd, execute_params = execute_params, quiet = TRUE)

  report_dir <- file.path(root, "outputs", "reports")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  produced <- sub("[.]qmd$", ".html", qmd)
  target <- file.path(report_dir, paste(sample_id, test_type, engine,
    sep = "_"
  ))
  target <- paste0(target, ".html")
  ok <- file.rename(produced, target)
  if (!ok) {
    # file.rename fails across volumes; fall back to copy and remove.
    file.copy(produced, target, overwrite = TRUE)
    unlink(produced)
  }
  message("report: ", target)
  invisible(target)
}

render_all <- function(engine = c("drc", "bayesnec"), root = ".", ...) {
  engine <- match.arg(engine)
  source(file.path(root, "pkg", "R", "test_types.R"), local = TRUE)
  ids <- cr_test_types()$id
  vapply(ids, function(id) render_workflow(engine, id, root = root, ...),
    character(1)
  )
}

workflow_path <- function(engine, test_type, root = ".") {
  source(file.path(root, "pkg", "R", "test_types.R"), local = TRUE)
  tt <- cr_test_type(test_type)
  file.path(root, "workflows", engine, tt$group, paste0(test_type, ".qmd"))
}
