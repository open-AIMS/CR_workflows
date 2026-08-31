#' Validate a concentration-response dataset against its test type
#'
#' Applies the checks a laboratory analyst would otherwise make by eye before
#' fitting: that the expected columns are present, that concentrations and
#' responses fall within admissible ranges for the response type, that a control
#' is present, and that replication is adequate. Problems that make a fit
#' impossible are raised as errors. Problems that only threaten reliability are
#' returned in the result so that the analyst can record and override them,
#' rather than being silently corrected.
#'
#' @param data A data frame holding one test.
#' @param test_type Test-type identifier, one of `cr_test_types()$id`.
#' @param min_conc_levels Minimum number of distinct non-control concentrations
#'   below which an issue is recorded. Five is the usual guideline minimum for
#'   regression-based estimation.
#' @param min_reps Minimum replicates per concentration below which an issue is
#'   recorded.
#' @return An object of class `cr_check`: a list with elements `test_type`,
#'   `label`, `n_rows`, `n_conc`, `issues` and `design`.
#' @export
#' @examples
#' data(algal_growth)
#' check_cr_data(algal_growth, "algal_growth")
check_cr_data <- function(data, test_type, min_conc_levels = 5, min_reps = 3) {
  tt <- cr_test_type(test_type)
  issues <- character(0)

  needed <- c(tt$x_var, tt$y_var, if (!is.na(tt$trials_var)) tt$trials_var)
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols)) {
    stop("Dataset is missing required column(s) for test type '", test_type,
      "': ", paste(missing_cols, collapse = ", "),
      ". Expected: ", paste(needed, collapse = ", "),
      call. = FALSE
    )
  }

  x <- data[[tt$x_var]]
  y <- data[[tt$y_var]]
  if (!is.numeric(x)) {
    stop("Concentration column '", tt$x_var, "' must be numeric.", call. = FALSE)
  }
  if (!is.numeric(y)) {
    stop("Response column '", tt$y_var, "' must be numeric.", call. = FALSE)
  }
  if (anyNA(x)) {
    stop("Concentration column contains missing values.", call. = FALSE)
  }
  if (any(x < 0)) {
    stop("Concentration column contains negative values.", call. = FALSE)
  }
  if (anyNA(y)) {
    issues <- c(issues, "Response column contains missing values; these rows will be dropped by the fitting engine.")
  }

  if (!any(x == 0)) {
    issues <- c(issues, "No zero-concentration control rows found. Confirm that the control was recorded and that a solvent control has not been coded as a low concentration.")
  }

  issues <- c(issues, check_response_range(y, data, tt))

  n_conc <- length(unique(x))
  n_exposed <- n_conc - as.integer(any(x == 0))
  if (n_exposed < min_conc_levels) {
    issues <- c(issues, sprintf(
      "Only %d non-control concentrations. Regression-based estimates are poorly determined below %d.",
      n_exposed, min_conc_levels
    ))
  }

  design <- summarise_design(data, test_type)
  if (any(design$n < min_reps)) {
    issues <- c(issues, sprintf(
      "Fewer than %d replicates at %d concentration(s).",
      min_reps, sum(design$n < min_reps)
    ))
  }
  if (partial_response(y, data, tt)) {
    issues <- c(issues, "The response does not reach a lower plateau within the tested range. ECx values beyond the observed effect range are extrapolations and must be reported as such.")
  }

  structure(
    list(
      test_type = test_type, label = tt$label, n_rows = nrow(data),
      n_conc = n_conc, issues = issues, design = design
    ),
    class = "cr_check"
  )
}

# Range checks depend on the statistical form of the response, so they are kept
# separate from the structural checks above and dispatched on response_type.
check_response_range <- function(y, data, tt) {
  w <- character(0)
  switch(tt$response_type,
    continuous_positive = {
      if (any(y <= 0, na.rm = TRUE)) {
        w <- c(w, "Response contains zero or negative values. A Gamma likelihood cannot accommodate these; either use a Gaussian likelihood or apply a documented offset.")
      }
    },
    count = {
      if (any(y != round(y), na.rm = TRUE)) {
        w <- c(w, "Count response contains non-integer values.")
      }
      if (any(y < 0, na.rm = TRUE)) {
        w <- c(w, "Count response contains negative values.")
      }
    },
    proportion = {
      if (any(y < 0 | y > 1, na.rm = TRUE)) {
        w <- c(w, "Proportion response falls outside the interval 0 to 1.")
      } else if (any(y %in% c(0, 1), na.rm = TRUE)) {
        w <- c(w, "Proportion response contains exact zeros or ones, which a Beta likelihood excludes. Either use the underlying counts with a binomial likelihood, or apply a documented transformation.")
      }
    },
    binomial_trials = {
      n <- data[[tt$trials_var]]
      if (any(y > n, na.rm = TRUE)) {
        w <- c(w, "Successes exceed trials in at least one row.")
      }
      if (any(n <= 0, na.rm = TRUE)) {
        w <- c(w, "Trials column contains non-positive values.")
      }
      if (any(y < 0, na.rm = TRUE)) {
        w <- c(w, "Success column contains negative values.")
      }
    }
  )
  w
}

# A response that has not reached a plateau yields ECx estimates that
# extrapolate beyond the data. Detected here by comparing the mean response at
# the highest concentration with the control, rather than being left to be
# noticed downstream in the estimate table.
partial_response <- function(y, data, tt) {
  if (tt$response_type == "binomial_trials") y <- y / data[[tt$trials_var]]
  x <- data[[tt$x_var]]
  ctrl <- mean(y[x == 0], na.rm = TRUE)
  top <- max(x, na.rm = TRUE)
  hi <- mean(y[x == top], na.rm = TRUE)
  if (!is.finite(ctrl) || !is.finite(hi) || ctrl == 0) {
    return(FALSE)
  }
  (hi / ctrl) > 0.5
}

#' Summarise the design of a concentration-response dataset
#'
#' @inheritParams check_cr_data
#' @return A data frame with one row per concentration giving the number of
#'   replicates and the mean and standard deviation of the response. For
#'   binomial test types the response is summarised as a proportion.
#' @export
#' @examples
#' data(algal_growth)
#' summarise_design(algal_growth, "algal_growth")
summarise_design <- function(data, test_type) {
  tt <- cr_test_type(test_type)
  y <- data[[tt$y_var]]
  if (tt$response_type == "binomial_trials") y <- y / data[[tt$trials_var]]
  x <- data[[tt$x_var]]
  parts <- lapply(split(y, x), function(v) {
    data.frame(
      n = length(v), mean = mean(v, na.rm = TRUE),
      sd = stats::sd(v, na.rm = TRUE)
    )
  })
  out <- do.call(rbind, parts)
  out <- cbind(conc = as.numeric(names(parts)), out)
  rownames(out) <- NULL
  out[order(out$conc), , drop = FALSE]
}

#' @export
print.cr_check <- function(x, ...) {
  cat("Concentration-response data check\n")
  cat("  test type      : ", x$test_type, " (", x$label, ")\n", sep = "")
  cat("  rows           : ", x$n_rows, "\n", sep = "")
  cat("  concentrations : ", x$n_conc, "\n", sep = "")
  if (length(x$issues) == 0) {
    cat("  status         : no issues identified\n")
  } else {
    cat("  status         : ", length(x$issues), " item(s) to review\n", sep = "")
    for (w in x$issues) cat("    - ", w, "\n", sep = "")
  }
  invisible(x)
}
