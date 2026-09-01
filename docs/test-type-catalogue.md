# Test type catalogue

Generated from the registry in `pkg/R/test_types.R` and the shipped example
datasets by `docs/_build_catalogue.R`. Do not edit by hand: edit the registry
and re-run that script.

The design given for each test type is the design of the shipped example
dataset, which follows the guideline but is simulated, not measured. It is a
guide to the expected input format, not a specification of the test.

14 test types across 5 groups: aquatic, sublethal, terrestrial, sediment, microbial.

# Aquatic

## `algal_growth`

**Algal growth inhibition (72 h)** — OECD 201.

Endpoint: specific growth rate, recorded in column `growth_rate`. Concentration in mg/L.

| | |
|---|---|
| Required columns | `conc`, `growth_rate` |
| Response type | continuous_positive |
| Example design | 6 exposed concentrations plus a control, 3 replicates each |
| Concentration range | 1 to 32 mg/L |
| bayesnec family | `Gamma(link = "identity")` |
| bayesnec candidate models | `decline` |
| drc default mean function | `LL.3`, `type = "continuous"` |
| Hormesis admitted | no |
| ECx reported | 10, 20, 50 |

Workflows: `workflows/bayesnec/aquatic/algal_growth.qmd`, `workflows/drc/aquatic/algal_growth.qmd`

## `daphnia_immobilisation`

**Daphnid acute immobilisation (48 h)** — OECD 202.

Endpoint: mobile individuals, recorded in column `mobile` out of `total`. Concentration in mg/L.

| | |
|---|---|
| Required columns | `conc`, `mobile`, `total` |
| Response type | binomial_trials |
| Example design | 6 exposed concentrations plus a control, 4 replicates each |
| Concentration range | 0.3125 to 10 mg/L |
| bayesnec family | `binomial(link = "identity")` |
| bayesnec candidate models | `decline` |
| drc default mean function | `LL.3`, `type = "binomial"` |
| Hormesis admitted | no |
| ECx reported | 10, 50 |

Workflows: `workflows/bayesnec/aquatic/daphnia_immobilisation.qmd`, `workflows/drc/aquatic/daphnia_immobilisation.qmd`

## `fish_larval_survival`

**Fish early life stage survival (7 d)** — OECD 210.

Endpoint: surviving larvae, recorded in column `alive` out of `total`. Concentration in mg/L.

| | |
|---|---|
| Required columns | `conc`, `alive`, `total` |
| Response type | binomial_trials |
| Example design | 6 exposed concentrations plus a control, 4 replicates each |
| Concentration range | 2 to 64 mg/L |
| bayesnec family | `binomial(link = "identity")` |
| bayesnec candidate models | `decline` |
| drc default mean function | `LL.3`, `type = "binomial"` |
| Hormesis admitted | no |
| ECx reported | 10, 20, 50 |

Workflows: `workflows/bayesnec/aquatic/fish_larval_survival.qmd`, `workflows/drc/aquatic/fish_larval_survival.qmd`

## `daphnia_reproduction`

**Daphnid reproduction (21 d)** — OECD 211.

Endpoint: offspring per surviving adult, recorded in column `offspring`. Concentration in mg/L.

| | |
|---|---|
| Required columns | `conc`, `offspring` |
| Response type | count |
| Example design | 6 exposed concentrations plus a control, 10 replicates each |
| Concentration range | 0.1 to 3.2 mg/L |
| bayesnec family | `negbinomial(link = "identity")` |
| bayesnec candidate models | `decline` |
| drc default mean function | `LL.3`, `type = "Poisson"` |
| Hormesis admitted | no |
| ECx reported | 10, 20, 50 |

Workflows: `workflows/bayesnec/aquatic/daphnia_reproduction.qmd`, `workflows/drc/aquatic/daphnia_reproduction.qmd`

# Sublethal

## `fertilisation_success`

**Fertilisation success (1 h)** — ASTM E1563.

Endpoint: fertilised ova, recorded in column `fertilised` out of `total`. Concentration in ug/L.

| | |
|---|---|
| Required columns | `conc`, `fertilised`, `total` |
| Response type | binomial_trials |
| Example design | 6 exposed concentrations plus a control, 4 replicates each |
| Concentration range | 10 to 320 ug/L |
| bayesnec family | `binomial(link = "identity")` |
| bayesnec candidate models | `decline` |
| drc default mean function | `LL.3`, `type = "binomial"` |
| Hormesis admitted | no |
| ECx reported | 10, 20, 50 |

Workflows: `workflows/bayesnec/sublethal/fertilisation_success.qmd`, `workflows/drc/sublethal/fertilisation_success.qmd`

## `larval_development`

**Larval development (48 h)** — ASTM E724.

Endpoint: proportion normal larvae, recorded in column `normal` out of `total`. Concentration in ug/L.

| | |
|---|---|
| Required columns | `conc`, `normal`, `total` |
| Response type | binomial_trials |
| Example design | 6 exposed concentrations plus a control, 4 replicates each |
| Concentration range | 3.125 to 100 ug/L |
| bayesnec family | `binomial(link = "identity")` |
| bayesnec candidate models | `decline` |
| drc default mean function | `LL.3`, `type = "binomial"` |
| Hormesis admitted | no |
| ECx reported | 10, 20, 50 |

Workflows: `workflows/bayesnec/sublethal/larval_development.qmd`, `workflows/drc/sublethal/larval_development.qmd`

## `coral_bleaching`

**Coral symbiont retention (10 d)** — no standard guideline.

Endpoint: symbiont density relative to control, recorded in column `prop_symbiont`. Concentration in ug/L.

| | |
|---|---|
| Required columns | `conc`, `prop_symbiont` |
| Response type | proportion |
| Example design | 6 exposed concentrations plus a control, 5 replicates each |
| Concentration range | 1 to 32 ug/L |
| bayesnec family | `Beta(link = "identity")` |
| bayesnec candidate models | `decline` |
| drc default mean function | `LL.4`, `type = "continuous"` |
| Hormesis admitted | no |
| ECx reported | 10, 20, 50 |

Workflows: `workflows/bayesnec/sublethal/coral_bleaching.qmd`, `workflows/drc/sublethal/coral_bleaching.qmd`

# Terrestrial

## `earthworm_survival`

**Earthworm acute survival (14 d)** — OECD 207.

Endpoint: surviving adults, recorded in column `alive` out of `total`. Concentration in mg/kg dry soil.

| | |
|---|---|
| Required columns | `conc`, `alive`, `total` |
| Response type | binomial_trials |
| Example design | 6 exposed concentrations plus a control, 4 replicates each |
| Concentration range | 52.92215 to 1000 mg/kg dry soil |
| bayesnec family | `binomial(link = "identity")` |
| bayesnec candidate models | `decline` |
| drc default mean function | `LL.3`, `type = "binomial"` |
| Hormesis admitted | no |
| ECx reported | 10, 50 |

Workflows: `workflows/bayesnec/terrestrial/earthworm_survival.qmd`, `workflows/drc/terrestrial/earthworm_survival.qmd`

## `earthworm_reproduction`

**Earthworm reproduction (56 d)** — OECD 222.

Endpoint: juveniles per vessel, recorded in column `juveniles`. Concentration in mg/kg dry soil.

| | |
|---|---|
| Required columns | `conc`, `juveniles` |
| Response type | count |
| Example design | 6 exposed concentrations plus a control, 4 replicates each |
| Concentration range | 10 to 320 mg/kg dry soil |
| bayesnec family | `negbinomial(link = "identity")` |
| bayesnec candidate models | `decline` |
| drc default mean function | `LL.3`, `type = "Poisson"` |
| Hormesis admitted | no |
| ECx reported | 10, 20, 50 |

Workflows: `workflows/bayesnec/terrestrial/earthworm_reproduction.qmd`, `workflows/drc/terrestrial/earthworm_reproduction.qmd`

## `plant_emergence`

**Seedling emergence (21 d)** — OECD 208.

Endpoint: emerged seedlings, recorded in column `emerged` out of `sown`. Concentration in mg/kg dry soil.

| | |
|---|---|
| Required columns | `conc`, `emerged`, `sown` |
| Response type | binomial_trials |
| Example design | 6 exposed concentrations plus a control, 4 replicates each |
| Concentration range | 15.625 to 500 mg/kg dry soil |
| bayesnec family | `binomial(link = "identity")` |
| bayesnec candidate models | `decline` |
| drc default mean function | `LL.3`, `type = "binomial"` |
| Hormesis admitted | no |
| ECx reported | 10, 20, 50 |

Workflows: `workflows/bayesnec/terrestrial/plant_emergence.qmd`, `workflows/drc/terrestrial/plant_emergence.qmd`

## `plant_growth`

**Seedling shoot biomass (21 d)** — OECD 208.

Endpoint: shoot dry weight, recorded in column `dry_weight`. Concentration in mg/kg dry soil.

| | |
|---|---|
| Required columns | `conc`, `dry_weight` |
| Response type | continuous_positive |
| Example design | 6 exposed concentrations plus a control, 4 replicates each |
| Concentration range | 15.625 to 500 mg/kg dry soil |
| bayesnec family | `Gamma(link = "identity")` |
| bayesnec candidate models | `decline` |
| drc default mean function | `LL.3`, `type = "continuous"` |
| Hormesis admitted | no |
| ECx reported | 10, 20, 50 |

Workflows: `workflows/bayesnec/terrestrial/plant_growth.qmd`, `workflows/drc/terrestrial/plant_growth.qmd`

# Sediment

## `amphipod_survival`

**Sediment amphipod survival (10 d)** — USEPA 600/R-94/025.

Endpoint: surviving amphipods, recorded in column `alive` out of `total`. Concentration in mg/kg dry sediment.

| | |
|---|---|
| Required columns | `conc`, `alive`, `total` |
| Response type | binomial_trials |
| Example design | 6 exposed concentrations plus a control, 5 replicates each |
| Concentration range | 25 to 800 mg/kg dry sediment |
| bayesnec family | `binomial(link = "identity")` |
| bayesnec candidate models | `decline` |
| drc default mean function | `LL.3`, `type = "binomial"` |
| Hormesis admitted | no |
| ECx reported | 10, 50 |

Workflows: `workflows/bayesnec/sediment/amphipod_survival.qmd`, `workflows/drc/sediment/amphipod_survival.qmd`

# Microbial

## `bioluminescence_inhibition`

**Bacterial bioluminescence inhibition (15 min)** — ISO 11348.

Endpoint: relative light units, recorded in column `rlu`. Concentration in mg/L.

| | |
|---|---|
| Required columns | `conc`, `rlu` |
| Response type | continuous_positive |
| Example design | 8 exposed concentrations plus a control, 3 replicates each |
| Concentration range | 1.5625 to 200 mg/L |
| bayesnec family | `Gamma(link = "identity")` |
| bayesnec candidate models | `all` |
| drc default mean function | `BC.4`, `type = "continuous"` |
| Hormesis admitted | yes |
| ECx reported | 10, 20, 50 |

Workflows: `workflows/bayesnec/microbial/bioluminescence_inhibition.qmd`, `workflows/drc/microbial/bioluminescence_inhibition.qmd`

## `nitrification_inhibition`

**Soil nitrogen transformation (28 d)** — OECD 216.

Endpoint: nitrate formation rate, recorded in column `nitrate_rate`. Concentration in mg/kg dry soil.

| | |
|---|---|
| Required columns | `conc`, `nitrate_rate` |
| Response type | continuous_positive |
| Example design | 6 exposed concentrations plus a control, 4 replicates each |
| Concentration range | 6.25 to 200 mg/kg dry soil |
| bayesnec family | `Gamma(link = "identity")` |
| bayesnec candidate models | `all` |
| drc default mean function | `BC.4`, `type = "continuous"` |
| Hormesis admitted | yes |
| ECx reported | 10, 20, 50 |

Workflows: `workflows/bayesnec/microbial/nitrification_inhibition.qmd`, `workflows/drc/microbial/nitrification_inhibition.qmd`

