# Choosing between the two engines

This document records what the `bayesnec` and `drc` workflows have in common,
where their results answer the same question, and where they do not. It is
written for someone deciding which engine to run for a given sample, and for
someone reading two reports that used different engines and needing to know
whether the numbers are comparable.

## What each engine does

**`drc`** fits one non-linear mean function by maximum likelihood. The mean
function is chosen from a small candidate set, and that choice is a decision
made from the data. Intervals are delta-method confidence intervals.

**`bayesnec`** fits every model in a candidate set, then averages their
predictions by stacking weights, so no single curve form has to be chosen.
Intervals are credible intervals from the posterior of the averaged prediction.

## What is the same

Both workflows read the same data, apply the same validation checks, produce
the same four output files under the same naming stem, and report ECx
referenced to the fitted control response. That last point is a deliberate
choice: the `drc` workflow converts the control-referenced target onto the
scale drc's calculation expects, rather than reporting an ECx referenced to the
fitted range, so that an EC50 from one engine means the same thing as an EC50
from the other.

Both record the software versions and the seed, and both save the fitted model
object, so any reported number can be traced back to the fit that produced it.

## What is not the same

**The intervals are different quantities.** A 95% credible interval is a
statement about where the parameter lies given the data and the priors. A 95%
delta-method confidence interval is a large-sample approximation with a
coverage interpretation. They are labelled distinctly in the `interval` column
of every results table and must not be pooled into one column of a summary
without that label.

**Only `bayesnec` reports a no-effect concentration**, and the quantity it
reports depends on the candidate set. The `estimate_type` column carries one of
three labels, which are not interchangeable:

| Label | Meaning |
|---|---|
| `NEC` | Every model in the fit carries the `nec` threshold parameter, so the estimate is that parameter. |
| `NSEC` | No model carries a threshold, so the estimate is derived from the smooth fitted curve. |
| `N(S)EC` | The candidate set mixes threshold and smooth models, so the model-averaged estimate is a weighted mixture of `nec` and NSEC draws. |

The registry default for the non-hormetic test types is the `decline` group,
which contains both kinds of model, so **the routine workflow reports
`N(S)EC`, not `NEC`**. Reporting that figure as a NEC would overstate what was
estimated. `bayesnec::nec()` warns about this itself when the set is mixed.
To obtain a NEC proper, restrict the candidate set to the `nec` group by
setting `params$model` to `"nec"`, and record that restriction in the report.

The `drc` workflow reports no threshold estimate of any kind.

**Both engines now average over their candidate set**, so both report intervals
that include the uncertainty in the choice of curve form. The weighting schemes
differ and are not the same quantity:

| | `drc` | `bayesnec` |
|---|---|---|
| Weights | Akaike weights, `exp(-delta AIC / 2)` normalised | Stacking weights |
| Basis | relative AIC of each model on its own | predictive performance of the combination |
| Interval | Buckland unconditional interval | posterior of the averaged prediction |

Neither set of weights is a probability that a model is correct.

Averaging changes the interval, not usually the point estimate. Where one
candidate carries almost all the weight, the averaged result is
indistinguishable from that candidate fitted alone: on the hormetic example
data, `BC.4` takes 72 per cent of the weight and the averaged EC50 differs from
the single-model value in the third significant figure. Where the candidates
disagree, the interval widens: on `daphnia_reproduction`, where the weight is
spread across three mean functions, the model-averaged EC50 interval is about
60 per cent wider than the best single model's. That widening is the
information averaging adds, and it is the reason to prefer it.

Single-model selection remains available in the `drc` workflow by setting
`params$average` to `false`. The interval is then a delta-method confidence
interval conditional on the selected function being the right one.

**The likelihood may differ.** `bayesnec` uses the family recorded in the
registry: Gamma for positive continuous responses, binomial for counts out of
trials, negative binomial for counts, Beta for proportions. `drc` fits the
error structure named in `drc_type`, which for the continuous test types is
Gaussian. Where the response variance grows with the mean, as it does for a
growth or biomass endpoint spanning two orders of magnitude, the Gaussian fit
is influenced more by the high-response observations than the Gamma fit is, and
the two engines will not return the same point estimate. This is a difference
in the model, not an error in either.

## The link function

`bayesnec` sets `link = "identity"` for every family it accepts, so that `top`,
`bot` and `nec` stay on the scale of the measured response. Several `brms`
families default to something else: `Beta()` and `binomial()` to logit, and
`Gamma()` to inverse. `cr_bnec_family()` therefore states the link explicitly
for every family.

This matters when writing raw `brms` code to compare against or prototype for
`bayesnec`. A family constructed with its default link fits a curve to the
transformed mean rather than to the mean, which is a different model. It still
samples cleanly and reports healthy convergence diagnostics, so the error is
not visible in the usual checks. The symptom is one family beating another by
an implausibly large `elpd_diff`; before believing such a result, confirm that
every model in the comparison uses the same link.

`pkg/tests/testthat/test-bayesnec_spec.R` asserts the identity link for every
test type in the registry, so this cannot regress unnoticed.

## Known numerical behaviour

`drc::ED()` fails inside `uniroot()` when a Brain-Cousens hormesis term is
weakly determined: the root-finding interval it chooses does not bracket the
target, and the call errors rather than returning that level as `NA`. This was
observed on the shipped hormetic example data. Because it is a failure of the
solver rather than a statement that the estimate does not exist,
`cr_ecx.drc()` catches it, solves the fitted curve directly, and returns the
point estimate with no interval and a warning saying so. A results table
showing `NA` in `lower` and `upper` for a hormetic test type is this fallback,
not a convergence failure of the model.

`drc`'s `type = "absolute"` branch was tested as an alternative and rejected:
on the hormetic example data it returned near-identical concentrations for
EC10, EC20 and EC50, which are not credible. The conversion to drc's relative
scale is used instead.

## Which to run

Run **`drc`** for routine throughput. It needs no compiler toolchain, fits in
under a second, and for a well-determined monotonic curve with adequate
replication it gives essentially the same ECx as the Bayesian fit.

Run **`bayesnec`** when the reported uncertainty has to account for the choice
of curve form, when a no-effect concentration is required rather than an ECx,
when the response distribution is not adequately described by a Gaussian error,
or when the data are sparse enough that the large-sample interval is
questionable.

Where both are run on the same sample, report both and label the intervals.
Do not select between them after seeing the estimates.
