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
│   └── tests/testthat/   unit tests
├── workflows/            end-to-end Quarto documents, edited in place
│   ├── _templates/       one template per engine; the documents are built from these
│   ├── _build_workflows.R  generates the 28 documents from the templates
│   ├── _render.R         renders a document and files the report
│   ├── bayesnec/<group>/<test_type>.qmd
│   └── drc/<group>/<test_type>.qmd
├── outputs/              everything a render produces; not tracked by git
├── docs/                 the test-type catalogue, engine comparison and procedures
├── prompts/              session logs
└── superceded/           safety-net backups of untracked files before editing
```

The workflow documents live outside the package so that a laboratory can edit
them directly for a particular sample without rebuilding or reinstalling
anything.

## Setting up

```r
# from the project root
install.packages(c("ggplot2", "rlang", "dplyr", "readr", "quarto", "knitr"))
install.packages("drc")                     # for the drc workflows
install.packages("bayesnec")                # for the bayesnec workflows

install.packages("devtools")
devtools::install("pkg")
```

The `bayesnec` workflows compile Stan models and therefore need a working C++
toolchain. On Windows this means the version of Rtools matching the installed R:
**R 4.5.x requires Rtools 4.5**, and an earlier Rtools will not substitute for
it. Check with `pkgbuild::has_build_tools(debug = TRUE)` before the first fit.
The `drc` workflows have no such requirement and run on a plain R installation.

Where Windows has no matching Rtools, the Bayesian fits can be run under WSL,
which has an ordinary `gcc` toolchain. `C:/Rworking` and
`/home/rfisher/Rworking_wsl` are the same location, so no files need copying:

```bash
wsl -d Debian
cd /home/rfisher/Rworking_wsl/CR_workflows
R CMD INSTALL pkg
Rscript workflows/_check_bayesnec.R algal_growth
```

`workflows/_check_bayesnec.R` runs the same sequence of calls the workflow
document makes and writes the same figure, table and fit, but it does not
render the report, which needs the Quarto CLI. Use it to verify the Bayesian
analysis path where only the toolchain is available.

## Running an analysis

```r
source("workflows/_render.R")

# the shipped example data, to confirm the installation works
render_workflow("drc", "algal_growth")

# a real sample
render_workflow(
  "drc", "algal_growth",
  sample_id = "S2026-0142",
  data_file = "data/S2026-0142_algae.csv"
)
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
