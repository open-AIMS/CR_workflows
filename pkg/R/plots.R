#' Plot the observed concentration-response data
#'
#' Draws the raw data before any model is fitted. Binomial responses are plotted
#' as proportions. Zero concentrations are drawn at a position one decade below
#' the lowest tested concentration and labelled, because a control cannot be
#' placed on a logarithmic axis but should still be visible.
#'
#' @inheritParams check_cr_data
#' @param log_x Whether to use a logarithmic concentration axis.
#' @return A `ggplot` object.
#' @export
#' @examples
#' data(algal_growth)
#' plot_cr_data(algal_growth, "algal_growth")
plot_cr_data <- function(data, test_type, log_x = TRUE) {
  tt <- cr_test_type(test_type)
  d <- cr_plot_frame(data, tt, log_x)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$.x, y = .data$.y)) +
    ggplot2::geom_point(
      ggplot2::aes(shape = .data$.control),
      size = 2, alpha = 0.8
    ) +
    ggplot2::scale_shape_manual(
      values = c("exposed" = 16, "control" = 1),
      name = NULL
    ) +
    ggplot2::labs(
      x = paste0("Concentration (", tt$conc_units, ")"),
      y = cr_y_label(tt),
      title = tt$label,
      subtitle = tt$guideline
    ) +
    ggplot2::theme_bw()
  if (log_x) p <- p + ggplot2::scale_x_log10()
  p
}

#' Plot a fitted concentration-response curve over the data
#'
#' Accepts a fit from either engine and draws the fitted curve with its
#' interval. The interval is a credible interval for `bayesnec` fits and a
#' delta-method confidence interval for `drc` fits; the caption records which.
#'
#' @param fit A `bayesnecfit`, `bayesmanecfit` or `drc` object.
#' @inheritParams check_cr_data
#' @param log_x Whether to use a logarithmic concentration axis.
#' @param n_grid Number of concentrations at which the curve is evaluated.
#' @return A `ggplot` object.
#' @export
plot_cr_fit <- function(fit, data, test_type, log_x = TRUE, n_grid = 200) {
  tt <- cr_test_type(test_type)
  pred <- cr_predict_grid(fit, data, tt, n_grid, log_x)
  plot_cr_data(data, test_type, log_x = log_x) +
    ggplot2::geom_ribbon(
      data = pred,
      ggplot2::aes(x = .data$.x, ymin = .data$.lower, ymax = .data$.upper),
      inherit.aes = FALSE, alpha = 0.2
    ) +
    ggplot2::geom_line(
      data = pred,
      ggplot2::aes(x = .data$.x, y = .data$.fit),
      inherit.aes = FALSE, linewidth = 0.7
    ) +
    ggplot2::labs(caption = attr(pred, "interval_label"))
}

# Shared data preparation: puts every response on one plotting scale and pins
# the control to a visible position on a log axis.
cr_plot_frame <- function(data, tt, log_x) {
  y <- data[[tt$y_var]]
  if (tt$response_type == "binomial_trials") y <- y / data[[tt$trials_var]]
  x <- data[[tt$x_var]]
  ctrl <- x == 0
  x_plot <- x
  if (log_x && any(ctrl)) {
    lowest <- min(x[!ctrl], na.rm = TRUE)
    x_plot[ctrl] <- lowest / 10
  }
  data.frame(
    .x = x_plot, .y = y,
    .control = ifelse(ctrl, "control", "exposed"),
    stringsAsFactors = FALSE
  )
}

cr_y_label <- function(tt) {
  if (tt$response_type == "binomial_trials") {
    paste0("Proportion (", tt$endpoint, ")")
  } else {
    tt$endpoint
  }
}

# Prediction grid, built per engine. Returned with an attribute naming the kind
# of interval so that the plot caption cannot drift from the calculation.
cr_predict_grid <- function(fit, data, tt, n_grid, log_x) {
  x <- data[[tt$x_var]]
  lo <- min(x[x > 0], na.rm = TRUE)
  hi <- max(x, na.rm = TRUE)
  grid <- if (log_x) {
    exp(seq(log(lo / 10), log(hi), length.out = n_grid))
  } else {
    seq(0, hi, length.out = n_grid)
  }

  if (inherits(fit, "cr_drc_ma")) {
    require_engine("drc")
    nd <- data.frame(grid)
    names(nd) <- tt$x_var
    # The averaged curve and its band are formed pointwise: at each
    # concentration the candidate predictions are combined by the same Buckland
    # rule used for the estimates, so the band widens where the candidates
    # disagree about the shape of the curve.
    #
    # Each candidate is predicted once and both columns taken from that result;
    # predicting twice doubles the work over a grid of n_grid concentrations.
    # drc's delta-method standard error is not always computable: on the
    # daphnia_immobilisation example it is NaN over most of the tested range for
    # two of the four candidates, and each NaN arrives as a separate "NaNs
    # produced" warning. Those are collected here and summarised once, because
    # two hundred identical warnings from drawing one figure hide anything else
    # the call had to say.
    per_fit <- withCallingHandlers(
      lapply(fit$fits, function(f) {
        p <- cr_drc_predict(f, nd, se.fit = TRUE)
        # drc's predict method drops the dimensions of its result for a grid of
        # one concentration, so it is restored before anything indexes columns.
        if (is.null(dim(p))) matrix(p, nrow = 1L) else p
      }),
      warning = function(w) {
        if (grepl("NaNs produced", conditionMessage(w), fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
    # matrix() rather than vapply(), which returns a bare vector rather than a
    # one-row matrix for a single-concentration grid; every column operation
    # below would then fail on it.
    as_grid_matrix <- function(column) {
      matrix(
        unlist(lapply(per_fit, function(p) as.numeric(p[, column])), use.names = FALSE),
        nrow = length(grid), dimnames = list(NULL, names(per_fit))
      )
    }
    preds <- as_grid_matrix(1L)
    ses <- as_grid_matrix(2L)
    w <- fit$weights[colnames(preds)]

    # buckland_combine() drops a candidate whose standard error is not finite.
    # Applied point by point that silently changes which candidates the curve is
    # averaged over as x moves along the axis -- five times over the grid on the
    # daphnia_immobilisation example -- so the drawn curve has small
    # discontinuities where the composition changes and the caption names a
    # candidate count that holds nowhere in particular. The set is therefore
    # fixed for the whole curve: a candidate usable everywhere is kept, one that
    # is not is dropped throughout and reported.
    usable <- apply(ses, 2, function(s) all(is.finite(s)))
    n_used <- sum(usable)
    if (any(usable) && !all(usable)) {
      dropped <- colnames(ses)[!usable]
      warning("drc returned no usable standard error at some concentrations for ",
        "candidate mean function(s) ", paste(dropped, collapse = ", "),
        " (combined weight ", format(signif(sum(w[dropped]), 3)), "). They are ",
        "excluded from the plotted curve and band, which is averaged over ",
        n_used, " of ", length(usable), " candidates. The estimates in the ",
        "results table are computed separately and are unaffected.",
        call. = FALSE
      )
      preds <- preds[, usable, drop = FALSE]
      ses <- ses[, usable, drop = FALSE]
      w <- w[usable]
    } else if (!any(usable)) {
      # Nothing is usable throughout, so the pointwise combination is all there
      # is. It is drawn, and the caption says the composition varies.
      n_used <- NA_integer_
    }

    comb <- t(vapply(seq_along(grid), function(i) {
      buckland_combine(preds[i, ], ses[i, ], w)[c("estimate", "se")]
    }, numeric(2)))
    z <- stats::qnorm(0.975)
    out <- data.frame(
      .x = grid, .fit = comb[, 1],
      .lower = comb[, 1] - z * comb[, 2],
      .upper = comb[, 1] + z * comb[, 2]
    )
    attr(out, "interval_label") <- if (is.na(n_used)) {
      sprintf(paste0(
        "Model-averaged curve with 95%% Buckland interval (drc). drc gave no ",
        "usable standard error across the whole range for any of the %d ",
        "candidates, so the interval is combined from whichever are usable at ",
        "each concentration and its composition varies along the curve."
      ), length(fit$fits))
    } else if (n_used < length(fit$fits)) {
      sprintf(paste0(
        "Model-averaged curve over %d of %d candidates with 95%% Buckland ",
        "interval (drc); the rest gave no usable standard error"
      ), n_used, length(fit$fits))
    } else {
      sprintf(
        "Model-averaged curve over %d candidates with 95%% Buckland interval (drc)",
        n_used
      )
    }
    return(out)
  }

  if (inherits(fit, "drc")) {
    require_engine("drc")
    nd <- data.frame(grid)
    names(nd) <- tt$x_var
    pr <- cr_drc_predict(fit, nd, interval = "confidence")
    out <- data.frame(.x = grid, .fit = pr[, 1], .lower = pr[, 2], .upper = pr[, 3])
    attr(out, "interval_label") <- "Fitted curve with 95% delta-method confidence interval (drc)"
    return(out)
  }

  require_engine("bayesnec")
  pd <- bnec_pred_frame(fit)
  out <- data.frame(.x = pd$x, .fit = pd$Estimate, .lower = pd$Q2.5, .upper = pd$Q97.5)
  attr(out, "interval_label") <- if (inherits(fit, "bayesmanecfit")) {
    "Model-averaged curve with 95% credible interval (bayesnec)"
  } else {
    "Fitted curve with 95% credible interval (bayesnec)"
  }
  out
}

# The two bayesnec fit classes hold their predictions in differently named
# slots: a single-model bayesnecfit in `pred_vals`, and a model-averaged
# bayesmanecfit in `w_pred_vals`, which has no `pred_vals` at all. Reading
# `pred_vals` from a bayesmanecfit therefore yields NULL and produces an empty
# prediction frame, which fails later inside ggplot with a message about a
# missing column rather than about the fit. The slot is selected on class here,
# and a fit carrying neither is reported directly.
bnec_pred_frame <- function(fit) {
  pd <- if (inherits(fit, "bayesmanecfit")) fit$w_pred_vals$data else fit$pred_vals$data
  if (is.null(pd)) {
    stop("This ", paste(class(fit), collapse = "/"), " object carries no fitted ",
      "predictions. Expected ",
      if (inherits(fit, "bayesmanecfit")) "w_pred_vals$data" else "pred_vals$data",
      ".",
      call. = FALSE
    )
  }
  needed <- c("x", "Estimate", "Q2.5", "Q97.5")
  if (!all(needed %in% names(pd))) {
    stop("Fitted predictions are missing column(s): ",
      paste(setdiff(needed, names(pd)), collapse = ", "), ".",
      call. = FALSE
    )
  }
  as.data.frame(pd)
}

#' Model weights from an averaged fit of either engine
#'
#' Returns the weights that combined the candidate models, in one format for
#' both engines: `bayesnec` stacking weights and `drc` Akaike weights.
#'
#' The two are not the same quantity. Stacking weights are chosen to optimise
#' the predictive performance of the combination. Akaike weights are a
#' transformation of the relative AIC of each model on its own. Both say how
#' much each candidate contributed, and neither is a probability that a model is
#' correct.
#'
#' @param fit A `bayesnecfit`, `bayesmanecfit` or `cr_drc_ma` object.
#' @return A data frame whose first two columns are `model` and `weight`,
#'   ordered by decreasing weight. A single-model fit returns one row with
#'   weight 1.
#' @export
cr_model_weights <- function(fit) {
  UseMethod("cr_model_weights")
}

#' @export
cr_model_weights.default <- function(fit) {
  stop("No cr_model_weights() method for an object of class ",
    paste(class(fit), collapse = "/"), ".",
    call. = FALSE
  )
}

#' @export
cr_model_weights.bayesnecfit <- function(fit) {
  data.frame(model = fit$model, weight = 1, stringsAsFactors = FALSE)
}

# The weights live in mod_stats$wi, which is an unnamed weights object rather
# than a named vector, so the model names have to be taken from the `model`
# column beside it.
#' @export
cr_model_weights.bayesmanecfit <- function(fit) {
  out <- data.frame(
    model = fit$mod_stats$model,
    weight = as.numeric(fit$mod_stats$wi),
    stringsAsFactors = FALSE
  )
  out[order(-out$weight), , drop = FALSE]
}
