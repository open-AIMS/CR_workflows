# Render workflow documents from a clone and collect the reports into
# outputs/reports.
#
# These are thin wrappers over crworkflows::cr_render_workflow(), which is the
# single implementation of the render path: it copies the document to a scratch
# directory before rendering, so that two renders of the same test type cannot
# overwrite each other's working files, and it files the report alongside the
# figure, table and fit that the same run produced. The Shiny interface calls
# the same function, so the console, the interface and this script cannot drift
# apart.
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
  target <- crworkflows::cr_render_workflow(
    engine = engine, test_type = test_type, sample_id = sample_id,
    data_file = data_file, root = root, ...
  )
  message("report: ", target)
  invisible(target)
}

render_all <- function(engine = c("drc", "bayesnec"), root = ".", ...) {
  engine <- match.arg(engine)
  ids <- crworkflows::cr_test_types()$id
  vapply(ids, function(id) render_workflow(engine, id, root = root, ...),
    character(1)
  )
}
