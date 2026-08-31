#' Response family for the bayesnec engine
#'
#' Builds the `brms` family object used by [fit_cr_bayesnec()] for a given test
#' type, with the link set explicitly.
#'
#' The link is always `"identity"`. This is not a stylistic choice: `bayesnec`
#' sets `link = "identity"` for every family it accepts, so that `top`, `bot`
#' and `nec` remain on the natural response scale. Several `brms` families
#' default to something else (`Beta()` and `binomial()` to logit, `Gamma()` to
#' inverse), and a family constructed with its default link produces a different
#' model that still samples cleanly and reports healthy convergence diagnostics.
#' Stating the link here keeps any hand-written `brms` comparison honest.
#'
#' @inheritParams check_cr_data
#' @return A `brms` family object.
#' @export
cr_bnec_family <- function(test_type) {
  require_engine("bayesnec")
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Package 'brms' is required alongside 'bayesnec'.", call. = FALSE)
  }
  fam <- cr_test_type(test_type)$bnec_family
  switch(fam,
    Gamma = stats::Gamma(link = "identity"),
    gaussian = stats::gaussian(link = "identity"),
    binomial = stats::binomial(link = "identity"),
    poisson = stats::poisson(link = "identity"),
    Beta = brms::Beta(link = "identity"),
    negbinomial = brms::negbinomial(link = "identity"),
    stop("No family mapping defined for '", fam, "'.", call. = FALSE)
  )
}

#' Model formula for the bayesnec engine
#'
#' @inheritParams check_cr_data
#' @param model `bayesnec` model name or model-group name. Defaults to the value
#'   in the test-type registry.
#' @return A `bayesnecformula` object.
#' @export
cr_bnec_formula <- function(test_type, model = NULL) {
  require_engine("bayesnec")
  tt <- cr_test_type(test_type)
  model <- model %||% tt$bnec_model
  lhs <- if (is.na(tt$trials_var)) {
    tt$y_var
  } else {
    paste(tt$y_var, "| trials(", tt$trials_var, ")")
  }
  # `model` may be a single model or model-group name, or a vector of model
  # names. sprintf() vectorises over it, so building the formula with the vector
  # interpolated directly produces one formula string per element; as.formula()
  # then takes the first and silently discards the rest, giving a single-model
  # fit where a candidate set was asked for. The vector is therefore deparsed
  # into a c(...) call inside the formula text.
  model_txt <- if (length(model) == 1L) {
    paste0('"', model, '"')
  } else {
    paste0("c(", paste0('"', model, '"', collapse = ", "), ")")
  }
  txt <- sprintf("%s ~ crf(%s, model = %s)", lhs, tt$x_var, model_txt)
  bayesnec::bayesnecformula(stats::as.formula(txt))
}

#' Fit a concentration-response model with the bayesnec engine
#'
#' Wraps [bayesnec::bnec()] with the family, link and candidate model set
#' recorded for the test type, so that every laboratory fitting the same test
#' type starts from the same specification.
#'
#' @inheritParams check_cr_data
#' @param model `bayesnec` model or model-group name. Defaults to the registry
#'   value for the test type; `"decline"` for monotonic endpoints and `"all"`
#'   where a hormetic response is plausible.
#' @param validate Whether to run [check_cr_data()] first and stop on a
#'   structural error. Set to `FALSE` only when the data have already been
#'   checked in the same session.
#' @param ... Further arguments passed to [bayesnec::bnec()], for example
#'   `iter`, `chains`, `seed` or `control`.
#' @return A `bayesnecfit` or `bayesmanecfit` object.
#' @export
fit_cr_bayesnec <- function(data, test_type, model = NULL, validate = TRUE, ...) {
  require_engine("bayesnec")
  if (validate) print(check_cr_data(data, test_type))
  bayesnec::bnec(
    formula = cr_bnec_formula(test_type, model),
    data = data,
    family = cr_bnec_family(test_type),
    ...
  )
}
