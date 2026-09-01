#' Launch the concentration-response analysis interface
#'
#' Starts a Shiny application for running the standard workflows without writing
#' R code. The application is a front end to the same Quarto workflow documents
#' the command-line functions render, not a separate implementation: it sets the
#' parameters, renders the document, and collects what it produced. The report
#' remains the record of the analysis.
#'
#' Analyses run in a background process, so the interface stays usable while a
#' fit is running and several samples can run at once. A `drc` analysis takes a
#' few seconds; a model-averaged `bayesnec` analysis takes eight to fifteen
#' minutes, and is submitted as a job to be collected when it finishes.
#'
#' The project does not have to be cloned. The workflow documents ship inside
#' the package, so an installed package is enough to run the app; where a
#' project tree is present its own `workflows/` directory is used instead, so a
#' laboratory that has adapted a document gets its version.
#'
#' @param root Directory the outputs are written under, in an `outputs/`
#'   subdirectory. `NULL` uses the project containing the working directory if
#'   there is one, and the working directory otherwise.
#' @param ... Passed to [shiny::runApp()], for example `port` or `launch.browser`.
#' @return Invisibly `NULL`. Called for the side effect of running the app.
#' @export
run_cr_app <- function(root = NULL, ...) {
  for (p in c("shiny", "bslib", "DT", "callr")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' is required to run the interface.", call. = FALSE)
    }
  }
  app_dir <- system.file("app", package = "crworkflows")
  if (!nzchar(app_dir)) {
    stop("The application directory was not found in the installed package.",
      call. = FALSE
    )
  }
  if (is.null(root)) root <- cr_output_root()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root <- normalizePath(root, "/", mustWork = TRUE)
  shiny::shinyOptions(cr_root = root)
  shiny::runApp(app_dir, ...)
}
