## Session: package review cycle
Date: 2026-09-01
Model: Claude Opus 5 (claude-opus-5[1m])

### Purpose

A full review of the `crworkflows` package and the project around it, followed by
repair of what the review found, run as repeated cycles of review and fix. This
entry records the defects that changed the science or the software behind it and
the reasoning behind each repair. Documentation and repository tidying done in
the same session is not recorded here.

### Data checks did not detect an inverted response

`check_cr_data()` applied no test of the direction of the response. Supplying the
affected count where its complement is expected -- `immobile` in place of
`mobile`, which the standard operating procedure names as one of three decisions
that are hard to correct later -- passed every check in silence. The failure then
surfaced from `cr_ecx()` as an error stating that the ECx values lay above the
highest tested concentration, which is a different fault from the one present.

A direction check was added. It compares the mean response at the highest tested
concentration with the mean at the control, and reports an issue where the former
is not below the latter.

The comparison is made on the two means rather than on their ratio. An inverted
binomial response has a control mean of exactly zero -- an undamaged control has
an affected count of zero -- so a ratio is undefined for the commonest instance of
the fault the check exists to catch. This was found by writing the check as a
ratio first and watching it fail to fire on the inverted `daphnia_immobilisation`
data.

The same comparison carries the extrapolation check that was previously a
separate function. That check asked whether the response at the highest
concentration was above half the control, with the half fixed in the code. The
quantity it is testing is whether the largest observed effect reaches the largest
ECx the test type reports, so the threshold is now read from `ecx_targets` in the
registry. Every current row reports EC50, so no shipped result changes; a row
reporting only EC10 and EC20 would previously have been judged against the wrong
threshold. The reported message was reworded to state what is tested, which is
the size of the largest observed effect, rather than describing it as a plateau.

The error raised by `cr_ecx.drc()` when a target falls outside the fitted range
was reworded for the same reason. It asserted that the ECx lay above the tested
range, which is one of two possible causes; the other is that the fitted curve
rises with concentration. It now states what was found and names both.

### The plotted model-averaged curve changed composition along the x axis

`drc`'s delta-method standard error is not always computable. On the
`daphnia_immobilisation` example it is `NaN` over most of the tested range for
`LL.3` and in scattered blocks for `W2.3`, two of the four candidates.

`buckland_combine()` drops a candidate whose standard error is not finite and
renormalises the weights over those remaining. Applied point by point along the
prediction grid, as `cr_predict_grid()` did, this silently changed which
candidates the curve was averaged over as x moved: measured on that dataset, the
composition changed at five of the 200 grid points, the curve was averaged over
between two and four candidates depending where it was read, and the caption
stated four throughout. The discontinuities at the changes reached 0.019 on a
response scale spanning 0 to 1.

The candidate set is now fixed for the whole curve. A candidate whose standard
error is usable at every grid point is kept; one that is not is dropped
throughout, and a warning names it, gives its combined weight, and states how
many candidates the drawn curve is averaged over. The caption carries the same
count. Where no candidate is usable throughout, the pointwise combination is
drawn and the caption says that its composition varies, since there is nothing
better available and silence would be worse.

The estimates are unaffected: `cr_ecx.cr_drc_ma()` combines per-candidate `ED()`
results rather than grid predictions, and already reports the number of
candidates that contributed in its `interval` column.

Each `NaN` also arrived as its own warning, 225 of them from one call to
`plot_cr_fit()`. These are now collected and muffled at the point of prediction,
because the summary warning above carries everything they say and two hundred
identical warnings hide anything else the call reports.

### The Stan toolchain check failed instead of reporting a failure

`stan_stage_toolchain()` read the result of `pkgbuild::has_build_tools()` from a
variable that the failing branch never assigned, so where that function threw
rather than returning `FALSE` the stage aborted with "object 'val' not found".
The diagnostic whose purpose is to report a broken toolchain therefore failed on
the machines it was written for. The verdict is now carried out of the
`tryCatch()`.

Three tests called `check_stan_toolchain()` with its default stages, which
compiles and samples a Stan model and then fits a `brms` model. On a machine
whose toolchain works the suite would compile Stan models three times on every
run. The ordering and skipping logic is now exercised against stubbed stage
functions, which also removes the dependence on whether the machine running the
tests has a toolchain at all.

### Reproducibility

`cr_render_workflow()` named its scratch directory using `sample(1e6, 1)`, which
draws from the caller's random number stream. Rendering a report therefore
changed the numbers a subsequent seeded analysis produced in the same session.
`tempfile()` is used instead, which also removes the collision between two
renders started in the same second. `cr_bundle_outputs()` had the same collision
in its staging directory, where the first bundle to finish deleted the second's
files.

---
## Session: package review cycle (continued)
Date: 2026-09-01
Model: Claude Opus 5 (claude-opus-5[1m])

### An unreachable ECx level no longer withholds the estimable ones

`cr_ecx.drc()` raised an error where any requested level had no solution on the
fitted curve. The condition is a statement about that level rather than about
the fit: a curve that plateaus above half the control determines EC10 and EC20
and leaves only EC50 unreachable. Raising an error withheld the two levels that
were estimated and, inside a workflow, produced no report at all.

The level is now returned with an `NA` estimate and the reason in its `interval`
column, which is where every other qualification of a result already lives, and
a warning names the levels affected. Where no level is reachable, which is what
an inverted response column gives, every row is `NA` and the warning says so.

The behaviour was checked on `coral_bleaching` rescaled to plateau at 62 per
cent of the control. `LL.4` estimates its lower limit rather than fixing it at
zero, so the plateau puts EC50 outside the fitted range: EC10 and EC20 are
returned with delta-method intervals, EC50 is `NA`, and the results table has
three rows as before. The three-parameter defaults in the registry fix the lower
limit at zero and always reach every level, so no shipped result changes.

The warning is raised before `drc::ED()` is called rather than beside either
result, so that what is reported does not depend on whether the solver went on
to converge.

`cr_ecx.drc()` also now names a missing test type as the problem. A fit made by
calling `drc::drm()` directly carries no `cr_test_type` attribute, and the
omission previously surfaced from `cr_test_type()` as an unknown identifier of
"".

---
## Session: package review cycle (continued)
Date: 2026-09-01
Model: Claude Opus 5 (claude-opus-5[1m])

### Uploaded files were shared between concurrent interface sessions

Shiny sessions in one R process share `tempdir()`. The interface copied each
uploaded file to `tempdir()/upload_<name>`, so two analysts who both uploaded a
file called `results.csv` wrote to the same path. The second upload overwrote
the first, and a background job started before it and still reading that path
would analyse the second analyst's data and file the result under the first
analyst's sample identifier.

This matters because the interface is documented as running several samples at
once, which is the case where it happens. Each session now gets its own upload
directory, removed when the session ends. The uploaded name is supplied by the
client, so it is reduced to its base name before a path is built from it.

---
## Session: package review cycle (continued)
Date: 2026-09-01
Model: Claude Opus 5 (claude-opus-5[1m])

### The drc intervals for the count test types are conditional on equidispersion

`drc` fits the two count test types with `type = "Poisson"`, which fixes the
variance equal to the mean. `bayesnec` fits them with a negative binomial, which
estimates it. Nothing in the package or its documentation recorded that
difference, although `docs/engine-comparison.md` exists to record exactly this
kind of divergence and already covered the Gaussian against Gamma case for the
continuous endpoints.

The difference is not small on the shipped data. Pearson dispersion of the
`drc` fit was measured as 2.77 on `daphnia_reproduction` and 3.32 on
`earthworm_reproduction`, against 0.63 to 1.60 for the binomial test types,
which is what the generator implies: those two datasets are simulated from a
negative binomial of size 18 and 12. The point estimates are unaffected, but the
standard errors and every interval built from them are too narrow by roughly the
square root of the dispersion. On `daphnia_reproduction` the EC50 interval is
0.309 to 0.391, a width of 0.082, where the dispersion-corrected width would be
0.137.

`cr_drc_dispersion()` was added to report the quantity, and the `drc` workflow
now prints it beside the residual plot with a statement of what it means for the
interval. It reports rather than corrects: rescaling an interval is a change to
the model and belongs in the model specification, which is the author's
decision, not something a diagnostic should apply silently. Whether the registry
should fit the count endpoints with a quasi-Poisson or a different error
structure is left open and recorded in `docs/engine-comparison.md`.

The dispersion is computed as the mean squared Pearson residual against the
variance the fitted likelihood assumes: the mean for a Poisson fit, and
`p(1 - p)/n` for a binomial fit, which is the variance of the fitted proportion
given that `drc` fits binomial data on the proportion scale with the trials as
weights. It returns `NA` for the continuous test types, whose Gaussian fit
estimates its own scale, so the ratio would be one by construction and would say
nothing.

---
## Session: package review cycle (continued)
Date: 2026-09-01
Model: Claude Opus 5 (claude-opus-5[1m])

### The workflow build script could not be run on Linux

`_build_workflows.R` refuses to overwrite a document that has been edited by
hand since it was generated, and decides that by comparing an md5 of the
document against the hash recorded in `workflows/_manifest.csv`.

The hash was computed by writing the content to a temporary file with
`writeLines()`, which uses the platform's line ending. The hashes in the
committed manifest were therefore Windows hashes. Running the script on Linux or
WSL recomputed them with LF, found all 28 mismatched, and reported every
document as edited by hand -- which is the one state in which the script refuses
to regenerate anything. The protection against losing a deliberate local edit
had become a bar on running the generator at all from the other half of the
working environment described in the repository conventions.

The hash is now computed with an explicit LF terminator, so it depends on the
content and not on the machine. The generators write LF as well, for the same
reason and so that a Windows working tree matches what is committed:
`_build_workflows.R`, `docs/_build_catalogue.R`, `pkg/data-raw/document_datasets.R`
and the csv templates written by `pkg/data-raw/generate_datasets.R`. A binary
connection is used rather than `eol = "\n"` alone, because on Windows
`write.csv()` opens a text connection and translates the separator back to CRLF.

The manifest was regenerated once with `force = TRUE` to move it onto the new
basis. A clean re-run now reports no skips, all 28 recorded hashes are the
platform-independent ones, and the shipped copies are identical to the project
copies.

A `.gitattributes` normalising text files to LF was added alongside, so that the
question cannot arise again from a file written by some other tool.

---
