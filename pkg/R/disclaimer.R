# The disclaimer is held here, in one place, and read from here by the
# application and by both workflow templates. Written out separately in each it
# would drift, and a disclaimer that differs between the interface and the report
# it produced is worse than one that is merely repeated.

#' The provenance disclaimer carried by every analysis
#'
#' Returns the statement that this package was written by a generative AI system
#' and has not been thoroughly checked. The Shiny interface displays it, and both
#' workflow templates print it at the top of every report, so that a reader of a
#' result meets it whether or not they have read the README.
#'
#' @param style One of `"plain"`, which returns the text as one paragraph for a
#'   console or a plain-text context, or `"markdown"`, which returns it wrapped
#'   in a Quarto warning callout for inclusion in a rendered report.
#' @return A character string of length one.
#' @export
#' @examples
#' cat(cr_disclaimer())
cr_disclaimer <- function(style = c("plain", "markdown")) {
  style <- match.arg(style)
  text <- paste(
    "This package was written by a generative AI system. Its code,",
    "documentation and analysis workflows have not been thoroughly checked, and",
    "it is a work in progress. Any result derived from it must be treated with",
    "caution and verified independently before it is used, cited, or relied upon",
    "for a regulatory or commercial decision."
  )
  if (style == "plain") {
    return(text)
  }
  paste0(
    "::: {.callout-warning}\n",
    "## Disclaimer\n\n",
    text, "\n",
    ":::\n"
  )
}
