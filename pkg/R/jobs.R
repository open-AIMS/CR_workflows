# Analyses are run in a separate R process rather than in the caller's. A
# model-averaged bayesnec fit takes eight to fifteen minutes, which would freeze
# a Shiny session for the whole of it. Running the render in a background
# process keeps the interface responsive, lets several samples run at once, and
# means a fit that fails takes its own process down rather than the app.

#' Start an analysis in a background process
#'
#' @inheritParams cr_render_workflow
#' @return An object of class `cr_job`.
#' @export
cr_start_job <- function(engine = c("drc", "bayesnec"), test_type,
                         sample_id = "example", data_file = NULL,
                         root = NULL,
                         backend = "cmdstanr", ...) {
  engine <- match.arg(engine)
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("Package 'callr' is required to run an analysis in the background.",
      call. = FALSE
    )
  }
  if (is.null(root)) root <- cr_output_root()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root <- normalizePath(root, "/", mustWork = TRUE)
  if (!is.null(data_file)) data_file <- normalizePath(data_file, "/", mustWork = TRUE)

  proc <- callr::r_bg(
    func = function(engine, test_type, sample_id, data_file, root, backend, extra) {
      library(crworkflows)
      do.call(cr_render_workflow, c(
        list(
          engine = engine, test_type = test_type, sample_id = sample_id,
          data_file = data_file, root = root, backend = backend, quiet = TRUE
        ),
        extra
      ))
    },
    args = list(
      engine = engine, test_type = test_type, sample_id = sample_id,
      data_file = data_file, root = root, backend = backend,
      extra = list(...)
    ),
    supervise = TRUE,
    stdout = "|", stderr = "|"
  )

  structure(
    list(
      process = proc, engine = engine, test_type = test_type,
      sample_id = sample_id, root = root,
      started = Sys.time(), id = paste(sample_id, test_type, engine, sep = "_")
    ),
    class = "cr_job"
  )
}

#' Status of a background analysis
#'
#' @param job A `cr_job` from [cr_start_job()].
#' @return A one-row data frame with columns `id`, `sample_id`, `test_type`,
#'   `engine`, `status`, `minutes` and `message`. `status` is one of `"running"`,
#'   `"done"` or `"failed"`.
#' @export
cr_job_status <- function(job) {
  stopifnot(inherits(job, "cr_job"))
  alive <- job$process$is_alive()
  status <- if (alive) {
    "running"
  } else if (identical(job$process$get_exit_status(), 0L)) {
    "done"
  } else {
    "failed"
  }
  msg <- if (status == "failed") cr_job_error(job) else ""
  data.frame(
    id = job$id, sample_id = job$sample_id, test_type = job$test_type,
    engine = job$engine, status = status,
    minutes = as.numeric(difftime(Sys.time(), job$started, units = "mins")),
    message = msg, stringsAsFactors = FALSE
  )
}

# The failure reason is read from the process's stderr. It is the only place a
# Quarto or fitting error surfaces once the process has exited, and without it
# the interface can only report that something failed.
cr_job_error <- function(job) {
  txt <- tryCatch(job$process$read_all_error(), error = function(e) "")
  if (!nzchar(txt)) {
    return("The analysis process exited without a message.")
  }
  lines <- utils::tail(strsplit(txt, "\n", fixed = TRUE)[[1]], 15)
  paste(trimws(lines[nzchar(trimws(lines))]), collapse = " | ")
}

#' Collect the outputs of a finished analysis
#'
#' @inheritParams cr_job_status
#' @return A named character vector of the files that exist, as returned by
#'   [cr_output_files()].
#' @export
cr_job_outputs <- function(job) {
  stopifnot(inherits(job, "cr_job"))
  f <- cr_output_files(job$engine, job$test_type, job$sample_id, job$root)
  f[file.exists(f)]
}

#' @export
print.cr_job <- function(x, ...) {
  s <- cr_job_status(x)
  cat("Analysis job: ", s$id, "\n", sep = "")
  cat("  status : ", s$status, sprintf(" (%.1f min)", s$minutes), "\n", sep = "")
  if (nzchar(s$message)) cat("  message: ", s$message, "\n", sep = "")
  invisible(x)
}

#' Stop a running analysis
#'
#' @inheritParams cr_job_status
#' @return Invisibly `TRUE`.
#' @export
cr_stop_job <- function(job) {
  stopifnot(inherits(job, "cr_job"))
  if (job$process$is_alive()) job$process$kill()
  invisible(TRUE)
}

#' Bundle the outputs of one or more analyses into a zip file
#'
#' @param jobs A list of `cr_job` objects, or a character vector of file paths.
#' @param zipfile Path of the zip file to create.
#' @return The path to the zip file.
#' @export
cr_bundle_outputs <- function(jobs, zipfile = tempfile(fileext = ".zip")) {
  files <- if (is.character(jobs)) {
    jobs
  } else {
    unique(unlist(lapply(jobs, cr_job_outputs), use.names = FALSE))
  }
  files <- files[file.exists(files)]
  if (!length(files)) {
    stop("No output files to bundle.", call. = FALSE)
  }
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop("Package 'zip' is required to bundle outputs.", call. = FALSE)
  }
  # The outputs of one analysis live in four different directories. They are
  # staged into one flat folder first so the zip opens to a readable list rather
  # than a deep outputs/figures/... tree, and so that zip() has a single root.
  stage <- file.path(tempdir(), paste0("cr_bundle_", as.integer(Sys.time())))
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  file.copy(files, file.path(stage, basename(files)), overwrite = TRUE)

  zip::zip(zipfile,
    files = list.files(stage), root = stage, mode = "cherry-pick"
  )
  zipfile
}
