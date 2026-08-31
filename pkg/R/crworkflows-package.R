# Fitting engines are deliberately in Suggests rather than Imports. A commercial
# laboratory running only frequentist analyses should not be required to install
# brms and a Stan toolchain, and one running only Bayesian analyses should not be
# required to install drc. Every engine-specific function therefore checks
# availability with require_engine() before use.

#' @keywords internal
#' @importFrom rlang .data
"_PACKAGE"
