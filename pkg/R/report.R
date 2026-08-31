#' Write the standard output set for one analysis
#'
#' Saves the figure, the results table and the fitted model object to the
#' project `outputs/` tree under a common stem, so that every analysis leaves
#' the same set of files behind and a report can be traced back to the fit that
#' produced it.
#'
#' @param fit A fitted model object.
#' @param plot A `ggplot` object.
#' @param results A results table, as returned by [cr_results_table()].
#' @param stem File stem, typically `"<sample_id>_<test_type>_<engine>"`.
#' @param root Output root directory.
#' @param width,height Figure dimensions in inches.
#' @param save_fit Whether to save the fitted model object. Model-averaged
#'   Bayesian fits are very large: the `algal_growth` example, twelve models at
#'   four chains of 4000 iterations, serialises to 423 MB. A laboratory
#'   archiving many tests should set this to `FALSE` and re-fit from the
#'   recorded seed and package versions when a fit is needed again. `drc` fits
#'   are a few hundred kilobytes and can be kept without difficulty.
#' @return Invisibly, a named character vector of the paths written.
#' @export
write_cr_outputs <- function(fit, plot, results, stem,
                             root = getOption("crworkflows.output_root", "outputs"),
                             width = 6, height = 4.5, save_fit = TRUE) {
  paths <- c(
    figure = cr_output_path("figures", paste0(stem, ".png"), root = root),
    table = cr_output_path("tables", paste0(stem, "_results.csv"), root = root)
  )
  ggplot2::ggsave(paths[["figure"]], plot, width = width, height = height, dpi = 300)
  utils::write.csv(results, paths[["table"]], row.names = FALSE)
  if (save_fit) {
    paths["fit"] <- cr_output_path("fits", paste0(stem, ".rds"), root = root)
    saveRDS(fit, paths[["fit"]])
  }
  invisible(paths)
}

#' Record the software versions used for an analysis
#'
#' Returns the R and package versions, the platform and the random seed in a
#' one-row data frame for inclusion in a report. Reproducing a Bayesian fit
#' requires all of these, not only the seed.
#'
#' @param seed The seed used for the analysis.
#' @param engine Which fitting engine was used.
#' @param backend For the bayesnec engine, the Stan interface used, either
#'   `"rstan"` or `"cmdstanr"`. Recorded because the two are different
#'   implementations of the same sampler and do not reproduce each other draw
#'   for draw from one seed.
#' @return A one-row data frame.
#' @export
#' @examples
#' cr_session_record(seed = 42, engine = "drc")
cr_session_record <- function(seed = NA_integer_, engine = c("bayesnec", "drc"),
                              backend = NULL) {
  engine <- match.arg(engine)
  # Both Stan interfaces are recorded when either is in use: rstan and cmdstanr
  # are different implementations of the same sampler and do not reproduce each
  # other draw for draw from one seed, so the version of the one actually used
  # has to be recoverable from the report.
  pkgs <- c(
    "crworkflows", engine,
    if (engine == "bayesnec") c("brms", "rstan", "cmdstanr")
  )
  vers <- vapply(pkgs, function(p) {
    if (requireNamespace(p, quietly = TRUE)) {
      as.character(utils::packageVersion(p))
    } else {
      NA_character_
    }
  }, character(1))
  out <- data.frame(
    r_version = R.version.string,
    platform = R.version$platform,
    engine = engine,
    backend = backend %||% NA_character_,
    seed = seed,
    date = format(Sys.Date()),
    stringsAsFactors = FALSE
  )
  if (engine == "bayesnec" && identical(backend, "cmdstanr")) {
    out$cmdstan_version <- tryCatch(as.character(cmdstanr::cmdstan_version()),
      error = function(e) NA_character_
    )
  }
  cbind(out, as.data.frame(t(vers), stringsAsFactors = FALSE))
}
