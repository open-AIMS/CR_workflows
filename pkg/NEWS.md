# crworkflows 0.0.0.9000

* This package was written by a generative AI system and has not been
  thoroughly checked. `cr_disclaimer()` holds the statement; the Shiny interface
  displays it and both workflow templates print it at the top of every report.
* Initial project scaffold.
* Test-type registry covering fourteen routine test types across aquatic,
  sublethal, terrestrial, sediment and microbial exposures.
* Simulated example dataset for each test type, with the generating parameters
  held in the `truth` attribute.
* Data validation (`check_cr_data()`, `summarise_design()`), fitting wrappers
  for both engines (`fit_cr_bayesnec()`, `fit_cr_drc()`), estimate extraction
  in one common format (`cr_ecx()`, `cr_nec()`, `cr_results_table()`), plotting
  and output helpers.
* `check_cr_data()` reports a response that does not decline with
  concentration, which is what supplying the affected count in place of its
  complement produces. The extrapolation check it carries is now made against
  the largest ECx the test type reports rather than against a fixed fifty per
  cent.
* `cr_ecx()` returns an ECx level whose target lies outside the range of the
  fitted curve as `NA`, with the reason in its `interval` column and a warning,
  rather than raising an error that withheld the levels that were estimable.
* `plot_cr_fit()` averages a `cr_drc_ma` fit over one fixed set of candidates
  across the whole curve. Where `drc` gives no usable standard error for a
  candidate it is dropped throughout and named in a warning, and the figure
  caption records how many candidates the curve was averaged over.
