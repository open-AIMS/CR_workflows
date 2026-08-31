#' Check that an optional fitting engine is installed
#'
#' @param engine Either `"bayesnec"` or `"drc"`.
#' @return Invisibly `TRUE`, or an error if the package is unavailable.
#' @export
require_engine <- function(engine = c("bayesnec", "drc")) {
  engine <- match.arg(engine)
  if (!requireNamespace(engine, quietly = TRUE)) {
    stop("Package '", engine, "' is required for this function but is not ",
      "installed. Install it, or use the other fitting engine.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Find the project root
#'
#' Walks up from `path` looking for the `.crproject` marker file at the root of
#' the CR_workflows project. Workflow documents call this so that they write to
#' one `outputs/` tree regardless of which subdirectory they are rendered from:
#' Quarto sets the working directory to the document's own folder, so a relative
#' path would otherwise create an `outputs/` directory beside every document.
#'
#' The search can be short-circuited by the `crworkflows.project_root` option or
#' the `CRWORKFLOWS_PROJECT_ROOT` environment variable. Both exist so that a
#' document can be rendered from a scratch directory outside the project, which
#' is what the app does to keep concurrent renders from colliding, while still
#' writing to the project's own `outputs/` tree.
#'
#' @param path Directory to start from. Defaults to the working directory.
#' @return The project root as a character string, or an error if no marker is
#'   found above `path` and no override is set.
#' @export
cr_project_root <- function(path = getwd()) {
  override <- getOption(
    "crworkflows.project_root",
    Sys.getenv("CRWORKFLOWS_PROJECT_ROOT", "")
  )
  if (nzchar(override)) {
    return(normalizePath(override, "/", mustWork = FALSE))
  }
  path <- normalizePath(path, "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(path, ".crproject"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("No .crproject marker found above '", getwd(),
        "'. Workflow documents must be rendered from inside the project.",
        call. = FALSE
      )
    }
    path <- parent
  }
}

#' Path to a project output file
#'
#' Builds a path under the project `outputs/` directory and creates the
#' containing folder if needed. Workflows use this so that every document writes
#' to the same predictable location regardless of where it is rendered from.
#'
#' @param ... Path components appended to the output root.
#' @param root Output root directory. Defaults to the `crworkflows.output_root`
#'   option, or `"outputs"` relative to the working directory.
#' @return The full file path, as a character string.
#' @export
#' @examples
#' cr_output_path("figures", "example.png", root = tempdir())
cr_output_path <- function(...,
                           root = getOption("crworkflows.output_root", "outputs")) {
  path <- file.path(root, ...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}

# Null-coalescing helper, used where an argument may legitimately be absent.
`%||%` <- function(x, y) if (is.null(x)) y else x
