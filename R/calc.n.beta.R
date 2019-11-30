#' Audit Sample Size for any Sample
#'
#' This function calculates the required sample size for an audit, based on
#' the Beta distribution. The Beta distribution is not bound to a population
#' size, and can be used in any case. calc.n.beta improves on calc.n.binomial by
#' presenting the possibility that the mistakes are not complete mistakes.
#'
#' @usage calc.n.beta(expected.mistakes, materiality, mean.taint = 1, confidence = 0.95)
#'
#' @param expected.mistakes An integer representing the number of expected mistakes
#' in the sample.
#' @param materiality A value representing the materiality of the audit in percentages.
#' @param mean.taint A value representing the expected mean taint in the sample, defaults to 1.
#' @param confidence The amount of confidence desired from the bound
#' (on a scale from 0 to 1), defaults to 95\% confidence.
#'
#' @return A value indicating the required sample size for the audit.
#'
#' @section Details: EMPTY FOR NOW
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso
#'
#' @references
#'
#' @examples
#' # Calculate the required sample size for a materiality of 5\% when one mistake
#' # is expected to be found in the sample, which is expected to have a taint of 0.5.
#' calc.n.binomial(expected.mistakes = 1,
#'                 materiality = 0.05,
#'                 mean.taint = 0.5,
#'                 confidence = 0.95)
#'
#' @keywords sample size
#'
#' @export

calc.n.beta <- function (expected.mistakes, materiality, mean.taint = 1, confidence = 0.95) {
  alpha <- 1 - confidence
  for (i in expected.mistakes:2000) {
    x <- qbeta(1 + (expected.mistakes*mean.taint), 1 + (i - expected.mistakes*mean.taint), p = confidence)
    if (x < alpha)
      return(i)
  }
}
