# Running a routine analysis

The procedure for analysing one sample of one test type and producing the
report. It assumes the package is installed and the engine in use is available;
see the README for setup.

## 1. Prepare the data file

Export the test as a csv with one row per replicate. The required columns are
listed for each test type in `docs/test-type-catalogue.md`, and the shipped
file in `pkg/inst/extdata/` is a working example of the layout.

Three points are decided here and are hard to correct later.

**The control is a concentration of zero, not a blank label.** A control row
must have `conc = 0`. A solvent control coded as a low concentration is treated
as an exposed group and shifts every estimate.

**Counts are recorded as the number affected out of the number exposed.** Both
columns are required for a binomial test type; a proportion alone discards the
number of individuals behind it, and the interval will be wrong.

**The response must decline with concentration.** Where the bench sheet records
the affected count, as it does for daphnid immobilisation, supply its
complement as well. The example dataset for that test type carries both
`immobile` and `mobile`, and the analysis uses `mobile`.

## 2. Render the workflow

```r
source("workflows/_render.R")
render_workflow(
  engine    = "drc",
  test_type = "algal_growth",
  sample_id = "S2026-0142",
  data_file = "path/to/S2026-0142.csv"
)
```

`docs/engine-comparison.md` records how to choose the engine. Where both are
run, both are reported; the engine is not selected after seeing the estimates.

## 3. Read the data checks before the estimates

The report's data-check section lists every issue found. Each must be resolved,
or recorded in the report as accepted, before the estimates are used. The
common ones and what they mean:

| Reported | What to do |
|---|---|
| No zero-concentration control | Confirm the control was exported and is coded as zero. Re-export. |
| Fewer than five non-control concentrations | The regression is poorly determined. Report the estimate with the limitation stated, or report a threshold result instead. |
| Fewer than three replicates at some concentrations | Check for rows lost in export before accepting. |
| Response does not reach a lower plateau | Any ECx beyond the observed effect range is an extrapolation and is reported as greater than the highest tested concentration. |
| Successes exceed trials | A transcription error. Correct at source. |
| Zero or negative values with a Gamma likelihood | Decide between a Gaussian likelihood and a documented offset, and record which. |
| Exact zeros or ones in a proportion | Use the underlying counts with a binomial likelihood if they exist. |

## 4. Check the fit before the estimates

**For a `drc` analysis**, read the candidate comparison table, the weights and
the residual plot. A lack-of-fit p-value below 0.05 means that candidate does
not describe the data regardless of its AIC rank, and must be commented on.
Systematic curvature or a fan shape in the residuals means the mean function or
the error structure is wrong. Where the weight is spread across candidates with
different shapes, the data do not determine the curve form; the model-averaged
interval already reflects that, and it belongs in the report.

**For a `bayesnec` analysis**, read the convergence summary first. An R-hat
above 1.05, or a bulk effective sample size below about 400, means the chains
have not mixed and the estimates are not usable; increase `params$iter` and
re-render. Then read the model weights: weight spread across models with
different shapes means the data do not determine the curve form, which belongs
in the report.

## 5. File the outputs

A render writes four files under `outputs/`, sharing the stem
`<sample_id>_<test_type>_<engine>`:

- `figures/<stem>.png` — the fitted curve over the observed data
- `tables/<stem>_results.csv` — the estimates
- `fits/<stem>.rds` — the serialised model
- `reports/<stem>.html` — the rendered document

The report carries the R version, the package versions and the seed. All of
these are needed to reproduce a Bayesian fit; the seed alone is not sufficient.

A model-averaged `bayesnec` fit serialises to hundreds of megabytes: the
`algal_growth` example, twelve models at four chains of 4000 iterations, is
423 MB. Where many samples are archived, set `params$save_fit` to `false` and
rely on the recorded seed and versions to re-fit. A `drc` fit is a few hundred
kilobytes and can always be kept.

## 6. Reporting the estimates

Report the interval with the label the results table gives it. The `bayesnec`
interval is a credible interval; the `drc` interval is a Buckland
model-averaged interval, or a delta-method confidence interval where averaging
was turned off. They are not interchangeable and must not be pooled into one
column without that label.

Report the `estimate_type` alongside any no-effect concentration, and use its
wording. `NEC` is a threshold parameter estimated by the model. `NSEC` is
derived from a smooth curve that has no threshold. `N(S)EC` is a weighted
mixture of the two, which is what the routine `bayesnec` workflow returns
because its default candidate set contains both kinds of model. Do not shorten
`N(S)EC` to `NEC` in a report.

An estimate with `NA` in `lower` and `upper` from a `drc` analysis of a
hormetic test type is the direct-solve fallback described in
`docs/engine-comparison.md`, not a convergence failure. Report the point
estimate and state that no interval was available.
