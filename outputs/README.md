# outputs

Everything in this directory is produced by rendering a workflow and is not
tracked by git. Deleting it and re-rendering reproduces it.

| Directory | Contents |
|---|---|
| `figures/` | One png per analysis: the fitted curve over the observed data. |
| `tables/` | One csv per analysis: the ECx estimates, and the NEC or NSEC estimate for `bayesnec` fits. |
| `fits/` | The serialised model object, saved so that an estimate can be traced back to the fit that produced it. A drc fit is a few hundred kilobytes. A model-averaged bayesnec fit is very large: the `algal_growth` example, twelve models at four chains of 4000 iterations, is 423 MB. Set `params$save_fit` to `false` to skip it and re-fit from the recorded seed and versions instead. |
| `reports/` | The rendered html workflow document, moved here by `workflows/_render.R`. |

File names follow the stem `<sample_id>_<test_type>_<engine>`, so the four
products of one analysis sort together.
