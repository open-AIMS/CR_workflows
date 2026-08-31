#' Registry of supported test types
#'
#' Returns the metadata table that drives every other function in the package.
#' Each row describes one routine ecotoxicological test type: the column names
#' its data are expected to use, the statistical form of the response, and the
#' default model specification for each fitting engine.
#'
#' The registry is the single place where a test type is defined. Adding a new
#' test type means adding a row here, a simulation block in
#' `data-raw/generate_datasets.R`, and a workflow document under `workflows/`.
#'
#' @return A data frame with one row per test type and the following columns:
#'   \describe{
#'     \item{id}{Short identifier, also the name of the example dataset.}
#'     \item{group}{Exposure matrix or organism group used to organise workflows.}
#'     \item{label}{Human-readable test name.}
#'     \item{guideline}{Test guideline the design is based on.}
#'     \item{endpoint}{The measured endpoint.}
#'     \item{response_type}{One of `"continuous_positive"`, `"binomial_trials"`,
#'       `"count"` or `"proportion"`. Determines validation rules and defaults.}
#'     \item{x_var}{Name of the concentration column.}
#'     \item{y_var}{Name of the response column.}
#'     \item{trials_var}{Name of the trials column, or `NA` where not applicable.}
#'     \item{conc_units}{Units of the concentration column.}
#'     \item{bnec_family}{Name of the `brms` family used by the `bayesnec` engine.}
#'     \item{bnec_model}{Default `bayesnec` model or model group.}
#'     \item{drc_fct}{Default `drc` mean function.}
#'     \item{drc_type}{Value passed to the `type` argument of [drc::drm()].}
#'     \item{hormesis}{Whether a hormetic (low-dose stimulation) response is
#'       plausible for this endpoint and should be admitted to the candidate set.}
#'     \item{ecx_targets}{Comma-separated ECx levels reported by default.}
#'   }
#' @export
#' @examples
#' cr_test_types()[, c("id", "group", "response_type")]
cr_test_types <- function() {
  rbind(
    tt_row("algal_growth", "aquatic",
      "Algal growth inhibition (72 h)", "OECD 201", "specific growth rate",
      "continuous_positive", "growth_rate", NA_character_, "mg/L",
      "Gamma", "decline", "LL.3", "continuous", FALSE, "10,20,50"),
    tt_row("daphnia_immobilisation", "aquatic",
      "Daphnid acute immobilisation (48 h)", "OECD 202", "mobile individuals",
      "binomial_trials", "mobile", "total", "mg/L",
      "binomial", "decline", "LL.2", "binomial", FALSE, "10,50"),
    tt_row("fish_larval_survival", "aquatic",
      "Fish early life stage survival (7 d)", "OECD 210", "surviving larvae",
      "binomial_trials", "alive", "total", "mg/L",
      "binomial", "decline", "LL.2", "binomial", FALSE, "10,20,50"),
    tt_row("daphnia_reproduction", "aquatic",
      "Daphnid reproduction (21 d)", "OECD 211", "offspring per surviving adult",
      "count", "offspring", NA_character_, "mg/L",
      "negbinomial", "decline", "LL.3", "Poisson", FALSE, "10,20,50"),
    tt_row("fertilisation_success", "sublethal",
      "Fertilisation success (1 h)", "ASTM E1563", "fertilised ova",
      "binomial_trials", "fertilised", "total", "ug/L",
      "binomial", "decline", "LL.2", "binomial", FALSE, "10,20,50"),
    tt_row("larval_development", "sublethal",
      "Larval development (48 h)", "ASTM E724", "proportion normal larvae",
      "binomial_trials", "normal", "total", "ug/L",
      "binomial", "decline", "LL.2", "binomial", FALSE, "10,20,50"),
    tt_row("coral_bleaching", "sublethal",
      "Coral symbiont retention (10 d)", "no standard guideline",
      "symbiont density relative to control",
      "proportion", "prop_symbiont", NA_character_, "ug/L",
      "Beta", "decline", "LL.4", "continuous", FALSE, "10,20,50"),
    tt_row("earthworm_survival", "terrestrial",
      "Earthworm acute survival (14 d)", "OECD 207", "surviving adults",
      "binomial_trials", "alive", "total", "mg/kg dry soil",
      "binomial", "decline", "LL.2", "binomial", FALSE, "10,50"),
    tt_row("earthworm_reproduction", "terrestrial",
      "Earthworm reproduction (56 d)", "OECD 222", "juveniles per vessel",
      "count", "juveniles", NA_character_, "mg/kg dry soil",
      "negbinomial", "decline", "LL.3", "Poisson", FALSE, "10,20,50"),
    tt_row("plant_emergence", "terrestrial",
      "Seedling emergence (21 d)", "OECD 208", "emerged seedlings",
      "binomial_trials", "emerged", "sown", "mg/kg dry soil",
      "binomial", "decline", "LL.2", "binomial", FALSE, "10,20,50"),
    tt_row("plant_growth", "terrestrial",
      "Seedling shoot biomass (21 d)", "OECD 208", "shoot dry weight",
      "continuous_positive", "dry_weight", NA_character_, "mg/kg dry soil",
      "Gamma", "decline", "LL.3", "continuous", FALSE, "10,20,50"),
    tt_row("amphipod_survival", "sediment",
      "Sediment amphipod survival (10 d)", "USEPA 600/R-94/025",
      "surviving amphipods",
      "binomial_trials", "alive", "total", "mg/kg dry sediment",
      "binomial", "decline", "LL.2", "binomial", FALSE, "10,50"),
    tt_row("bioluminescence_inhibition", "microbial",
      "Bacterial bioluminescence inhibition (15 min)", "ISO 11348",
      "relative light units",
      "continuous_positive", "rlu", NA_character_, "mg/L",
      "Gamma", "all", "BC.4", "continuous", TRUE, "10,20,50"),
    tt_row("nitrification_inhibition", "microbial",
      "Soil nitrogen transformation (28 d)", "OECD 216",
      "nitrate formation rate",
      "continuous_positive", "nitrate_rate", NA_character_, "mg/kg dry soil",
      "Gamma", "all", "BC.4", "continuous", TRUE, "10,20,50")
  )
}

# Internal constructor, kept separate so the registry above reads as a table.
tt_row <- function(id, group, label, guideline, endpoint, response_type,
                   y_var, trials_var, conc_units,
                   bnec_family, bnec_model, drc_fct, drc_type, hormesis,
                   ecx_targets) {
  data.frame(
    id = id, group = group, label = label, guideline = guideline,
    endpoint = endpoint, response_type = response_type,
    x_var = "conc", y_var = y_var, trials_var = trials_var,
    conc_units = conc_units, bnec_family = bnec_family,
    bnec_model = bnec_model, drc_fct = drc_fct, drc_type = drc_type,
    hormesis = hormesis, ecx_targets = ecx_targets,
    stringsAsFactors = FALSE
  )
}

#' Look up a single test type
#'
#' @param id Test-type identifier, one of `cr_test_types()$id`.
#' @return A one-row data frame from [cr_test_types()].
#' @export
#' @examples
#' cr_test_type("algal_growth")
cr_test_type <- function(id) {
  reg <- cr_test_types()
  if (length(id) != 1L || !id %in% reg$id) {
    stop("Unknown test type '", paste(id, collapse = ", "), "'. Available: ",
         paste(reg$id, collapse = ", "), call. = FALSE)
  }
  reg[reg$id == id, , drop = FALSE]
}

#' ECx levels reported by default for a test type
#'
#' @inheritParams cr_test_type
#' @return A numeric vector of ECx levels, as percentages.
#' @export
cr_ecx_targets <- function(id) {
  as.numeric(strsplit(cr_test_type(id)$ecx_targets, ",", fixed = TRUE)[[1]])
}
