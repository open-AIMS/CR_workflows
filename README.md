# CR_workflows

Standardised concentration-response analysis workflows for commercial
ecotoxicology laboratories.

The project supplies, for each of fourteen routine test types: an example
dataset, the analysis functions that test type needs, and a complete
end-to-end Quarto document that takes a laboratory data file and produces the
figure, the estimate table and the archived model fit that a report requires.

Two fitting engines are supported and each has its own set of workflow
documents: Bayesian multi-model averaging via
[`bayesnec`](https://open-aims.github.io/bayesnec/), and frequentist non-linear
regression via [`drc`](https://cran.r-project.org/package=drc). Neither engine
is required to install the package; install the one in use.

## Layout

```
CR_workflows/
├── pkg/                  installable R package `crworkflows`
│   ├── R/                analysis functions and the test-type registry
│   ├── data/             fourteen example datasets, one per test type
│   ├── data-raw/         the scripts that generate the datasets and their docs
│   ├── inst/extdata/     the same datasets as csv, as input format templates
│   ├── inst/app/         the Shiny interface
│   ├── inst/workflows/   the workflow documents, shipped with the package
│   └── tests/testthat/   unit tests
├── workflows/            end-to-end Quarto documents, edited in place
│   ├── _templates/       one template per engine; the documents are built from these
│   ├── _build_workflows.R  generates the 28 documents from the templates
│   ├── _render.R         renders a document and files the report
│   ├── _render_bayesnec.R  renders the bayesnec documents in bulk
│   ├── _check_bayesnec.R   runs the bayesnec path without Quarto
│   ├── bayesnec/<group>/<test_type>.qmd
│   └── drc/<group>/<test_type>.qmd
├── outputs/              everything a render produces; not tracked by git
├── docs/                 the test-type catalogue, engine comparison and procedures
├── prompts/              session logs
└── superceded/           safety-net backups of untracked files before editing
```

The workflow documents live in `workflows/` so that a laboratory can edit them
for a particular test type without rebuilding anything. `_build_workflows.R`
also copies them into `pkg/inst/workflows/`, so an installed package carries
its own copy and can run an analysis without the project being cloned. Where
both are present the project copy wins, so a local edit is what gets used.

## Setting up

The workflow documents are shipped inside the package, so installing it is
enough to run an analysis or the interface. Cloning the project is only needed
in order to edit the documents.

```r
install.packages(c("ggplot2", "rlang", "dplyr", "readr", "quarto", "knitr"))
install.packages("drc")                     # for the drc workflows

# for the bayesnec workflows. cmdstanr is the default backend and is not on
# CRAN, so it comes from the Stan project's own repository.
install.packages("bayesnec")
install.packages("cmdstanr",
  repos = c("https://stan-dev.r-universe.dev", getOption("repos"))
)
cmdstanr::install_cmdstan()                 # installs CmdStan itself

install.packages("remotes")
remotes::install_github("open-AIMS/CR_workflows", subdir = "pkg")
```

The repository is currently **private**, so `install_github()` needs a GitHub
token with access to it. Set one with `usethis::create_github_token()` and
`gitcreds::gitcreds_set()`, or install from a local clone instead:

```r
devtools::install("pkg")   # from the project root
```

Rendering a report also needs the [Quarto CLI](https://quarto.org/docs/get-started/),
which is a separate installation from the `quarto` R package.

Outputs are written to an `outputs/` directory. Inside a clone that is the
project's own `outputs/`; elsewhere it is created under the working directory,
or wherever `root` points:

```r
crworkflows::cr_render_workflow("drc", "algal_growth", root = "~/cr_analyses")
```

The `drc` workflows need nothing beyond a plain R installation. The `bayesnec`
workflows compile Stan models, so they need a working C++ toolchain.

### Checking the Stan toolchain

Before the first Bayesian fit, run:

```r
crworkflows::check_stan_toolchain()          # cmdstanr, the default backend
crworkflows::check_stan_toolchain("rstan")   # if using rstan instead
```

This runs the checks the `cmdstanr` and `rstan` installation pages recommend, in
the order the layers depend on each other, and names the layer that fails:

| Stage | What it establishes |
|---|---|
| `packages` | `brms` and the backend package are installed |
| `toolchain` | a compiler and `make` are present and usable |
| `cmdstan` | CmdStan itself is installed and its version readable |
| `stan` | a minimal Stan model compiles and samples |
| `brms` | `brms` can translate, compile and fit a model |

Later stages are skipped once one fails, so the first `FAIL` is the thing to
fix. A passing run looks like this:

```
Stan toolchain check (cmdstanr)
  PASS packages     0.0s  brms 2.23.0, cmdstanr 0.9.0
  PASS toolchain    0.0s  cmdstanr reports a usable toolchain.
  PASS cmdstan      0.0s  CmdStan 2.39.0 at ~/.cmdstan/cmdstan-2.39.0
  PASS stan         0.6s  A minimal Stan model compiled and sampled (theta = 0.26).
  PASS brms        11.8s  brms compiled and fitted a model (intercept = 2.14).
```

A failing run names the layer instead:

```
Stan toolchain check (cmdstanr)
  PASS packages     8.0s  brms 2.23.0, cmdstanr 0.8.0
  FAIL toolchain    0.0s  Rtools44 installation found but the toolchain was not
                          installed. Run cmdstanr::check_cmdstan_toolchain(fix = TRUE).
  skip cmdstan      0.0s  Skipped: an earlier stage failed.
```

The last two stages compile a model. The timings above are for a machine that
has compiled these models before; a first run takes tens of seconds longer. That
compilation is the point: a toolchain can look correctly configured and still
fail to build a model, and only an actual build proves otherwise.

**Prefer this to running a `bayesnec` model as a first test.** A `bayesnec` fit
compiles a dozen models and takes minutes, and when it fails the error surfaces
from inside Stan with nothing to say which layer is at fault. Once all stages
pass, a `bayesnec` fit that still fails is a problem with the model or the data
rather than with the installation.

On Windows the toolchain means the Rtools matching the installed R: **R 4.5.x
requires Rtools 4.5**, and an earlier Rtools will not substitute for it. Where
`cmdstanr` reports the toolchain as missing, `cmdstanr::check_cmdstan_toolchain(fix = TRUE)`
will attempt to repair it, and `cmdstanr::install_cmdstan()` installs CmdStan
itself.

Once the toolchain passes, `workflows/_check_bayesnec.R` runs the analysis path
for one test type end to end without needing the Quarto CLI:

```bash
Rscript workflows/_check_bayesnec.R algal_growth
```

## The interface

For routine use there is a Shiny application:

```r
crworkflows::run_cr_app()
```

It takes a csv, shows the data checks before anything is fitted, runs the
analysis, and hands back the report and outputs. It is a front end to the same
Quarto workflow documents described below, not a second implementation: every
run renders the same document, so there is one analysis path and the report
remains the record.

A `drc` analysis returns in seconds. A `bayesnec` analysis takes eight to
fifteen minutes and is submitted as a background job, so the interface stays
usable and several samples can run at once. Bayesian fits use the `cmdstanr`
backend, which is the faster of the two Stan interfaces. It still needs a C++
toolchain: CmdStan compiles each model.

The interface does not hide the decisions the analysis makes. The data checks
gate the run, the model weights are shown beside the estimates, and the
threshold estimate keeps its `NEC`, `NSEC` or `N(S)EC` label.

## Running an analysis from the console

```r
library(crworkflows)

# the shipped example data, to confirm the installation works
cr_render_workflow("drc", "algal_growth")

# a real sample
cr_render_workflow(
  "drc", "algal_growth",
  sample_id = "S2026-0142",
  data_file = "data/S2026-0142_algae.csv"
)

# a Bayesian analysis of the same sample
cr_render_workflow(
  "bayesnec", "algal_growth",
  sample_id = "S2026-0142",
  data_file = "data/S2026-0142_algae.csv"
)
```

Analyses can also be run in the background, which is what the interface does and
what a batch of samples needs:

```r
job <- cr_start_job("bayesnec", "algal_growth", sample_id = "S2026-0142")
cr_job_status(job)
cr_job_outputs(job)
```

Each render writes four files under `outputs/`, sharing the stem
`<sample_id>_<test_type>_<engine>`: the figure, the estimate table, the
serialised fit and the rendered report.

The input csv must carry the columns the test type expects. The shipped csv in
`pkg/inst/extdata/` is the template for each: for example
`pkg/inst/extdata/fish_larval_survival.csv` has `conc`, `replicate`, `total`
and `alive`. Column names and units come from the registry, which is printed
at the top of every report:

```r
crworkflows::cr_test_types()
crworkflows::cr_test_type("fish_larval_survival")
```

## Test types

| Group | Test type | Guideline | Response |
|---|---|---|---|
| aquatic | `algal_growth` | OECD 201 | continuous, positive |
| aquatic | `daphnia_immobilisation` | OECD 202 | binomial with trials |
| aquatic | `fish_larval_survival` | OECD 210 | binomial with trials |
| aquatic | `daphnia_reproduction` | OECD 211 | count |
| sublethal | `fertilisation_success` | ASTM E1563 | binomial with trials |
| sublethal | `larval_development` | ASTM E724 | binomial with trials |
| sublethal | `coral_bleaching` | none | proportion |
| terrestrial | `earthworm_survival` | OECD 207 | binomial with trials |
| terrestrial | `earthworm_reproduction` | OECD 222 | count |
| terrestrial | `plant_emergence` | OECD 208 | binomial with trials |
| terrestrial | `plant_growth` | OECD 208 | continuous, positive |
| sediment | `amphipod_survival` | USEPA 600/R-94/025 | binomial with trials |
| microbial | `bioluminescence_inhibition` | ISO 11348 | continuous, hormetic |
| microbial | `nitrification_inhibition` | OECD 216 | continuous, hormetic |

`docs/test-type-catalogue.md` gives the design, the endpoint and the model
specification for each. `docs/engine-comparison.md` records where the two
engines answer the same question and where they do not.

## The example datasets are simulated

Every shipped dataset is simulated from a stated curve with a stated error
structure, not measured. The generating parameters are held in the `truth`
attribute of each object, so a workflow result can be compared against the
values that produced it:

```r
attr(crworkflows::algal_growth, "truth")
```

They exist so that each workflow can be run end to end before a laboratory
substitutes its own data, and so that the tests have data with known
properties. They are not reference data and must not be cited as such.

## Changing the analysis

The 28 workflow documents are generated from two templates, so a change to the
analysis procedure is made once and reaches every test type:

```r
# edit workflows/_templates/<engine>-workflow-template.qmd, then
source("workflows/_build_workflows.R")
```

The build refuses to overwrite a document that has been edited by hand since it
was generated, so a departure from the standard procedure for one test type is
not silently lost. Pass `force = TRUE` to `build_workflows()` to overwrite
anyway.

Adding a test type means adding a row to the registry in
`pkg/R/test_types.R`, a simulation block in
`pkg/data-raw/generate_datasets.R`, and then re-running the generators:

```r
source("pkg/data-raw/generate_datasets.R")
source("pkg/data-raw/document_datasets.R")
devtools::document("pkg")
source("workflows/_build_workflows.R")
```
