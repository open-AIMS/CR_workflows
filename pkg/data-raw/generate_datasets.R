# Generate the example datasets shipped with crworkflows.
#
# Every dataset is simulated, not measured. They exist so that a laboratory can
# run each workflow end to end before substituting its own data, and so that the
# package tests have data with known properties. Each is simulated from a
# specified curve with a specified error structure, and the true parameters are
# stored in the attributes of the object so a workflow result can be compared
# against what generated it.
#
# Run with:  source("pkg/data-raw/generate_datasets.R")
# Writes:    pkg/data/*.rda and pkg/inst/extdata/*.csv

library(dplyr)
library(tidyr)
library(purrr)

# Base seed. Each dataset uses base_seed + an offset fixed per test type, so
# every dataset is reproducible on its own.
base_seed <- 20260831

pkg_root <- if (basename(getwd()) == "pkg") "." else "pkg"

# Concentration series ---------------------------------------------------------

# A geometric dilution series with a control, which is the design almost all of
# the guidelines specify. The factor of 2 is the usual choice for a definitive
# test; a factor of 3.2 is used for the wider-range screening designs.
dilution_series <- function(top, n = 6, factor = 2, control = TRUE) {
  s <- top / factor^(seq_len(n) - 1)
  if (control) c(0, rev(s)) else rev(s)
}

# Mean functions ---------------------------------------------------------------

# Three-parameter log-logistic on the natural concentration scale. Used for the
# monotonic endpoints. `ec50` is the concentration giving half the control
# response, and `slope` is the Hill coefficient.
ll3 <- function(x, top, ec50, slope) top / (1 + (x / ec50)^slope)

# Brain-Cousens hormetic form: a log-logistic decline with a linear stimulation
# term, used for the two microbial endpoints where low-dose stimulation is
# commonly observed.
bc <- function(x, top, ec50, slope, hormesis) {
  (top + hormesis * x) / (1 + (x / ec50)^slope)
}

# Simulation helpers -----------------------------------------------------------

# Each simulator sets its own seed from its `seed` argument rather than drawing
# from one stream shared by the whole script. Without this, adding or editing a
# dataset shifts the random draws of every dataset defined after it, so a change
# to one test type silently changes the answers in every workflow below it.

sim_binomial <- function(conc, reps, trials, top, ec50, slope, seed) {
  set.seed(seed)
  tidyr::expand_grid(conc = conc, replicate = seq_len(reps)) |>
    dplyr::mutate(
      p_true = ll3(.data$conc, top, ec50, slope),
      total = trials,
      successes = stats::rbinom(dplyr::n(), .data$total, .data$p_true)
    )
}

sim_gamma <- function(conc, reps, top, ec50, slope, cv, hormesis = 0, seed) {
  set.seed(seed)
  tidyr::expand_grid(conc = conc, replicate = seq_len(reps)) |>
    dplyr::mutate(
      mu_true = bc(.data$conc, top, ec50, slope, hormesis),
      shape = 1 / cv^2,
      value = stats::rgamma(dplyr::n(), shape = .data$shape, rate = .data$shape / .data$mu_true)
    )
}

sim_negbin <- function(conc, reps, top, ec50, slope, size, seed) {
  set.seed(seed)
  tidyr::expand_grid(conc = conc, replicate = seq_len(reps)) |>
    dplyr::mutate(
      mu_true = ll3(.data$conc, top, ec50, slope),
      value = stats::rnbinom(dplyr::n(), mu = .data$mu_true, size = size)
    )
}

sim_beta <- function(conc, reps, top, ec50, slope, phi, seed) {
  set.seed(seed)
  tidyr::expand_grid(conc = conc, replicate = seq_len(reps)) |>
    dplyr::mutate(
      mu_true = ll3(.data$conc, top, ec50, slope),
      value = stats::rbeta(dplyr::n(), .data$mu_true * phi, (1 - .data$mu_true) * phi)
    )
}

# Attach the generating parameters so that a workflow can state what the true
# value was. Without this the datasets are only useful as shapes.
tag <- function(d, ...) {
  d <- as.data.frame(d)
  attr(d, "truth") <- list(...)
  d
}

datasets <- list()

# Aquatic ----------------------------------------------------------------------

datasets$algal_growth <- sim_gamma(
  dilution_series(top = 32, n = 6), reps = 3,
  top = 1.35, ec50 = 4.2, slope = 2.1, cv = 0.08, seed = base_seed + 1
) |>
  dplyr::transmute(
    conc,
    replicate,
    growth_rate = round(.data$value, 4)
  ) |>
  tag(model = "LL.3", ec50 = 4.2, top = 1.35, slope = 2.1, error = "Gamma, CV 0.08")

# Both the recorded endpoint (immobile) and its complement (mobile) are kept.
# The bench sheet records immobilised individuals, but the analysis is run on
# the mobile count, because both engines fit a declining response: bayesnec's
# decline model group and the drc log-logistic candidates all assume the
# response falls with concentration.
datasets$daphnia_immobilisation <- sim_binomial(
  dilution_series(top = 10, n = 6), reps = 4, trials = 5,
  top = 0.99, ec50 = 1.1, slope = 3.0, seed = base_seed + 2
) |>
  dplyr::transmute(
    conc,
    replicate,
    total,
    immobile = .data$total - .data$successes,
    mobile = .data$successes
  ) |>
  tag(model = "LL.3 on survival", ec50 = 1.1, slope = 3.0, error = "binomial, 5 per vessel")

datasets$fish_larval_survival <- sim_binomial(
  dilution_series(top = 64, n = 6), reps = 4, trials = 20,
  top = 0.96, ec50 = 9.5, slope = 2.4, seed = base_seed + 3
) |>
  dplyr::transmute(conc, replicate, total, alive = .data$successes) |>
  tag(model = "LL.3", ec50 = 9.5, slope = 2.4, error = "binomial, 20 per vessel")

datasets$daphnia_reproduction <- sim_negbin(
  dilution_series(top = 3.2, n = 6), reps = 10,
  top = 92, ec50 = 0.42, slope = 1.9, size = 18, seed = base_seed + 4
) |>
  dplyr::transmute(conc, replicate, offspring = .data$value) |>
  tag(model = "LL.3", ec50 = 0.42, top = 92, slope = 1.9, error = "negative binomial, size 18")

# Sublethal --------------------------------------------------------------------

datasets$fertilisation_success <- sim_binomial(
  dilution_series(top = 320, n = 6), reps = 4, trials = 100,
  top = 0.94, ec50 = 41, slope = 2.8, seed = base_seed + 5
) |>
  dplyr::transmute(conc, replicate, total, fertilised = .data$successes) |>
  tag(model = "LL.3", ec50 = 41, slope = 2.8, error = "binomial, 100 ova counted")

datasets$larval_development <- sim_binomial(
  dilution_series(top = 100, n = 6), reps = 4, trials = 100,
  top = 0.91, ec50 = 12.5, slope = 3.4, seed = base_seed + 6
) |>
  dplyr::transmute(conc, replicate, total, normal = .data$successes) |>
  tag(model = "LL.3", ec50 = 12.5, slope = 3.4, error = "binomial, 100 larvae scored")

# Held as a proportion rather than counts, because symbiont density is measured
# by cell count per unit area and expressed relative to the control. This is the
# one dataset that exercises the Beta likelihood.
datasets$coral_bleaching <- sim_beta(
  dilution_series(top = 32, n = 6), reps = 5,
  top = 0.95, ec50 = 6.8, slope = 2.2, phi = 45, seed = base_seed + 7
) |>
  # Rounded to five places, and the series stopped short of the concentration
  # that drives the mean to zero. A Beta likelihood excludes exact zeros and
  # ones, so an example dataset containing either would fail its own check.
  dplyr::transmute(conc, replicate, prop_symbiont = round(.data$value, 5)) |>
  tag(model = "LL.3", ec50 = 6.8, slope = 2.2, error = "Beta, phi 45")

# Terrestrial ------------------------------------------------------------------

datasets$earthworm_survival <- sim_binomial(
  dilution_series(top = 1000, n = 6, factor = 1.8), reps = 4, trials = 10,
  top = 0.98, ec50 = 190, slope = 3.5, seed = base_seed + 8
) |>
  dplyr::transmute(conc, replicate, total, alive = .data$successes) |>
  tag(model = "LL.3", ec50 = 190, slope = 3.5, error = "binomial, 10 per vessel")

datasets$earthworm_reproduction <- sim_negbin(
  dilution_series(top = 320, n = 6), reps = 4,
  top = 64, ec50 = 34, slope = 2.0, size = 12, seed = base_seed + 9
) |>
  dplyr::transmute(conc, replicate, juveniles = .data$value) |>
  tag(model = "LL.3", ec50 = 34, top = 64, slope = 2.0, error = "negative binomial, size 12")

datasets$plant_emergence <- sim_binomial(
  dilution_series(top = 500, n = 6), reps = 4, trials = 20,
  top = 0.93, ec50 = 78, slope = 2.6, seed = base_seed + 10
) |>
  dplyr::transmute(conc, replicate, sown = .data$total, emerged = .data$successes) |>
  tag(model = "LL.3", ec50 = 78, slope = 2.6, error = "binomial, 20 seeds sown")

datasets$plant_growth <- sim_gamma(
  dilution_series(top = 500, n = 6), reps = 4,
  top = 420, ec50 = 55, slope = 1.7, cv = 0.18, seed = base_seed + 11
) |>
  dplyr::transmute(conc, replicate, dry_weight = round(.data$value, 1)) |>
  tag(model = "LL.3", ec50 = 55, top = 420, slope = 1.7, error = "Gamma, CV 0.18")

# Sediment ---------------------------------------------------------------------

datasets$amphipod_survival <- sim_binomial(
  dilution_series(top = 800, n = 6), reps = 5, trials = 20,
  top = 0.95, ec50 = 105, slope = 2.9, seed = base_seed + 12
) |>
  dplyr::transmute(conc, replicate, total, alive = .data$successes) |>
  tag(model = "LL.3", ec50 = 105, slope = 2.9, error = "binomial, 20 per chamber")

# Microbial --------------------------------------------------------------------

# Both microbial datasets are generated with a positive hormesis term, so that
# the hormetic candidate models in both engines have something to detect. A
# laboratory using these to validate a workflow should recover a fit that
# prefers a hormetic form over a plain log-logistic.
datasets$bioluminescence_inhibition <- sim_gamma(
  dilution_series(top = 200, n = 8), reps = 3,
  top = 100, ec50 = 15, slope = 2.5, cv = 0.06, hormesis = 5, seed = base_seed + 13
) |>
  dplyr::transmute(conc, replicate, rlu = round(.data$value, 2)) |>
  # For a hormetic curve the `e` parameter is not the EC50: the curve peaks
  # above the control, so the concentration giving a 50 per cent reduction from
  # the control lies above `e`. It is recorded as `e` rather than `ec50` so that
  # a workflow does not compare an ECx estimate against the wrong quantity.
  tag(model = "BC.4", e = 15, top = 100, slope = 2.5, hormesis = 5, error = "Gamma, CV 0.06")

datasets$nitrification_inhibition <- sim_gamma(
  dilution_series(top = 200, n = 6), reps = 4,
  top = 3.4, ec50 = 24, slope = 1.8, cv = 0.12, hormesis = 0.012, seed = base_seed + 14
) |>
  dplyr::transmute(conc, replicate, nitrate_rate = round(.data$value, 4)) |>
  tag(model = "BC.4", ec50 = 24, top = 3.4, slope = 1.8, hormesis = 0.012, error = "Gamma, CV 0.12")

# Write ------------------------------------------------------------------------

# Datasets are written both as .rda for package use and as .csv in inst/extdata,
# because the csv is the format a laboratory recognises and will match its own
# export against.
# Fail early if the registry and this script have drifted apart.
source(file.path(pkg_root, "R", "test_types.R"))
stopifnot(setequal(names(datasets), cr_test_types()$id))

for (nm in names(datasets)) {
  d <- datasets[[nm]]
  assign(nm, d)
  save(list = nm, file = file.path(pkg_root, "data", paste0(nm, ".rda")),
       compress = "bzip2", version = 3)
  utils::write.csv(d, file.path(pkg_root, "inst", "extdata", paste0(nm, ".csv")),
                   row.names = FALSE)
  message("wrote ", nm, ": ", nrow(d), " rows")
}
