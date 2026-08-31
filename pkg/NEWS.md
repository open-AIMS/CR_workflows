# crworkflows 0.0.0.9000

* Initial project scaffold.
* Test-type registry covering fourteen routine test types across aquatic,
  sublethal, terrestrial, sediment and microbial exposures.
* Simulated example dataset for each test type, with the generating parameters
  held in the `truth` attribute.
* Data validation (`check_cr_data()`, `summarise_design()`), fitting wrappers
  for both engines (`fit_cr_bayesnec()`, `fit_cr_drc()`), estimate extraction
  in one common format (`cr_ecx()`, `cr_nec()`, `cr_results_table()`), plotting
  and output helpers.
