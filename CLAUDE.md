# CR_workflows — project notes

Project-specific context, additional to `C:/Rworking/CLAUDE.md`. That file's
conventions apply here; this one records only what is particular to this
repository.

## Repo type

Research compendium with an installable R package inside it. `pkg/` is the
package `crworkflows` and follows package conventions, including `testthat`
tests for every new function and `R CMD check`. Everything outside `pkg/` is
project material: the editable workflow documents, the generators, and the
documentation.

Not an analysis project: there is no `packages.R`. `pkg/DESCRIPTION` is the
canonical dependency list, `Suggests` included, and it covers the scripts under
`workflows/` and `pkg/data-raw/` as well as the package itself.

## Generated files: do not edit by hand

Four things are generated, and editing the output rather than the source is
silently undone the next time the generator runs:

| Output | Generated from | By |
|---|---|---|
| `workflows/<engine>/<group>/*.qmd` and their copies in `pkg/inst/workflows/` | `workflows/_templates/*.qmd` | `workflows/_build_workflows.R` |
| `pkg/R/data.R` | the registry and the csv templates | `pkg/data-raw/document_datasets.R` |
| `pkg/data/*.rda`, `pkg/inst/extdata/*.csv` | the simulation blocks | `pkg/data-raw/generate_datasets.R` |
| `docs/test-type-catalogue.md` | the registry and the shipped data | `docs/_build_catalogue.R` |

A change to the analysis procedure goes in the template, not in the 28
documents. `_build_workflows.R` refuses to overwrite a document whose content
hash no longer matches `workflows/_manifest.csv`, so a deliberate local edit is
not lost; `build_workflows(force = TRUE)` overrides that.

**Generators must write LF explicitly.** `writeLines()` and `write.csv()` use
the platform line ending, and `eol = "\n"` alone is not enough on Windows
because both open text connections that translate it back. Use a binary
connection. This is not cosmetic: the manifest hashes are computed from written
content, so a platform-dependent write made the hashes Windows-specific and the
build script reported all 28 documents as hand-edited when run from WSL.
`.gitattributes` normalises the repository to LF as a second line of defence.

## The registry drives everything

`pkg/R/test_types.R` is the single definition of a test type. Adding one means a
row there, a simulation block in `pkg/data-raw/generate_datasets.R`, and a
re-run of every generator above. Nothing else should hard-code a test type, a
column name, or an ECx level; several defects found in review were constants
that had drifted from the registry.

## Two engines, and where they differ

`docs/engine-comparison.md` is the record of where `bayesnec` and `drc` answer
the same question and where they do not. Read it before comparing numbers from
the two, and add to it rather than to a code comment when a new divergence is
found. The two that bite hardest are recorded there and in the global
`CLAUDE.md`: `drc` fits the count endpoints with a Poisson whose intervals are
too narrow under overdispersion, and its delta-method standard error is
sometimes unavailable over part of the range.

## Testing

`pkg/tests/testthat/test-jobs.R` renders real documents in a background R
process, which loads the **installed** `crworkflows`, not the working tree. It
is the one group of tests that does not exercise the code under development. A
change touching both the package and the templates fails there until the package
is reinstalled; the file guards against that and skips with the reason named, so
reinstall before reading such a skip as anything else.

Bayesian fits are not exercised in the suite: they need a Stan toolchain and
minutes per model. The tests check the specification `bayesnec` would be given —
family, link, formula, model group — against stub objects carrying the structure
the real fit classes have. `pkg/data-raw/bnec_smoke.R` and
`workflows/_check_bayesnec.R` fit for real when a toolchain is available.

`check_stan_toolchain()` compiles and samples a Stan model in its last two
stages. Never call it with default `stages` from a test.
