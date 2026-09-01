# workflows

One end-to-end Quarto document per test type per engine: 14 test types by two
engines, 28 documents. Each takes a laboratory data file and produces the
figure, the estimate table, the serialised fit and the rendered report that a
report requires.

## Running one

From the project root:

```r
source("workflows/_render.R")
render_workflow("drc", "algal_growth",
                sample_id = "S2026-0142",
                data_file = "path/to/sample.csv")
```

Omit `data_file` to run the shipped example dataset, which is the way to
confirm the installation before using real data.

Documents can also be rendered directly from Quarto or from the IDE. Doing so
runs the example dataset unless `params$data_file` is set in the file, and
leaves the html beside the document rather than filing it under
`outputs/reports/`.

## Structure

| Path | Purpose |
|---|---|
| `_templates/` | One template per engine. Every document is generated from these. |
| `_build_workflows.R` | Generates the 28 documents and copies them into `pkg/inst/workflows/`. Refuses to overwrite a document edited since it was generated. |
| `_render.R` | Renders a document and moves the report into `outputs/reports/`. |
| `_render_bayesnec.R` | Renders the bayesnec documents in bulk. Slow: eight to fifteen minutes per test type. |
| `_check_bayesnec.R` | Runs the bayesnec analysis path for one test type without Quarto, for use where only the Stan toolchain is available. |
| `_manifest.csv` | Records which document came from which template, and its content hash. Used to detect hand edits. |
| `<engine>/<group>/<test_type>.qmd` | The workflow documents. |

## Editing

Change the procedure for every test type by editing the template and
re-running `_build_workflows.R`.

Change it for one test type by editing that document directly. The build will
then skip it and report that it was skipped, so the departure is not silently
overwritten. Record the reason for the departure in the document itself.
