#' Locate a workflow document
#'
#' @param engine Either `"drc"` or `"bayesnec"`.
#' @inheritParams check_cr_data
#' @param root Project root. Defaults to [cr_project_root()].
#' @return The path to the workflow document.
#' @export
cr_workflow_path <- function(engine = c("drc", "bayesnec"), test_type,
                             root = cr_project_root()) {
  engine <- match.arg(engine)
  tt <- cr_test_type(test_type)
  file.path(root, "workflows", engine, tt$group, paste0(test_type, ".qmd"))
}

#' Render a workflow document and file its report
#'
#' Renders the workflow for one sample and moves the resulting html into
#' `outputs/reports/`, so that the report sits alongside the figure, table and
#' fit that the same run produced.
#'
#' The document is rendered in a scratch copy rather than in place. Quarto
#' writes its html beside the source and uses a fixed intermediate name, so two
#' renders of the same test type at once would overwrite each other's working
#' files. Copying first makes concurrent renders safe, which the app relies on,
#' and leaves the `workflows/` tree untouched by a run.
#'
#' @inheritParams cr_workflow_path
#' @param sample_id Identifier for the sample, used in the output file names.
#' @param data_file Path to a csv of the sample data, or `NULL` to use the
#'   shipped example dataset for the test type.
#' @param backend For the bayesnec engine, the Stan interface. `"cmdstanr"` is
#'   the default because it is faster than `rstan` and does not need a matching
#'   Rtools on Windows.
#' @param quiet Whether to suppress Quarto's progress output.
#' @param ... Further parameters passed to the document, for example
#'   `average = FALSE` or `save_fit = FALSE`.
#' @return Invisibly, the path to the filed report.
#' @export
cr_render_workflow <- function(engine = c("drc", "bayesnec"), test_type,
                               sample_id = "example", data_file = NULL,
                               root = cr_project_root(),
                               backend = "cmdstanr", quiet = TRUE, ...) {
  engine <- match.arg(engine)
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("Package 'quarto' and the Quarto CLI are required to render a workflow.",
      call. = FALSE
    )
  }
  root <- normalizePath(root, "/", mustWork = TRUE)
  qmd <- cr_workflow_path(engine, test_type, root)
  if (!file.exists(qmd)) {
    stop("No ", engine, " workflow document found for test type '", test_type,
      "'. Expected: ", qmd,
      call. = FALSE
    )
  }

  work_dir <- file.path(tempdir(), paste0("cr_", engine, "_", test_type, "_", as.integer(Sys.time()), "_", sample(1e6, 1)))
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  local_qmd <- file.path(work_dir, basename(qmd))
  file.copy(qmd, local_qmd, overwrite = TRUE)

  execute_params <- c(
    list(sample_id = sample_id, test_type = test_type),
    if (!is.null(data_file)) {
      list(data_file = normalizePath(data_file, "/", mustWork = TRUE))
    },
    if (engine == "bayesnec" && !is.null(backend)) list(backend = backend),
    list(...)
  )

  # The rendered document resolves the project root from this variable rather
  # than by walking up from the scratch directory, which contains no marker.
  old <- Sys.getenv("CRWORKFLOWS_PROJECT_ROOT", unset = NA)
  Sys.setenv(CRWORKFLOWS_PROJECT_ROOT = root)
  on.exit(
    if (is.na(old)) {
      Sys.unsetenv("CRWORKFLOWS_PROJECT_ROOT")
    } else {
      Sys.setenv(CRWORKFLOWS_PROJECT_ROOT = old)
    },
    add = TRUE
  )

  quarto::quarto_render(local_qmd, execute_params = execute_params, quiet = quiet)

  produced <- sub("[.]qmd$", ".html", local_qmd)
  if (!file.exists(produced)) {
    stop("Quarto reported success but produced no html for '", test_type, "'.",
      call. = FALSE
    )
  }
  report_dir <- file.path(root, "outputs", "reports")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(
    report_dir,
    paste0(paste(sample_id, test_type, engine, sep = "_"), ".html")
  )
  # file.rename cannot cross volumes, and the scratch directory often is one.
  if (!file.rename(produced, target)) {
    file.copy(produced, target, overwrite = TRUE)
    unlink(produced)
  }
  invisible(target)
}

#' Paths to the outputs of one analysis
#'
#' Returns the four files an analysis produces, whether or not each exists, so
#' that a caller can collect what a run left behind.
#'
#' @inheritParams cr_render_workflow
#' @return A named character vector with elements `report`, `figure`, `table`
#'   and `fit`.
#' @export
cr_output_files <- function(engine, test_type, sample_id = "example",
                            root = cr_project_root()) {
  stem <- paste(sample_id, test_type, engine, sep = "_")
  c(
    report = file.path(root, "outputs", "reports", paste0(stem, ".html")),
    figure = file.path(root, "outputs", "figures", paste0(stem, ".png")),
    table = file.path(root, "outputs", "tables", paste0(stem, "_results.csv")),
    weights = file.path(root, "outputs", "tables", paste0(stem, "_weights.csv")),
    fit = file.path(root, "outputs", "fits", paste0(stem, ".rds"))
  )
}
