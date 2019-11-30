#' Audit Sample Size for Small Samples
#'
#' This function calculates the required sample size for an audit, based on
#' the Hypergeometric distribution. The Hypergeometric distribution can be used
#' if the expected required sample is more than 10\% of the population.
#' \deqn{\frac{n}{N} > 0.10}{n / N > 10\%}
#'
#' @usage calc.n.hypergeometric(expected.mistakes, population.size, materiality, confidence = 0.95)
#'
#' @param expected.mistakes An integer representing the number of expected mistakes
#' in the sample.
#' @param population.size An integer representing the number of transactions in
#' in the audit population.
#' @param materiality A value representing the materiality of the audit in percentages.
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
#' # Calculate the required sample size for a materiality of 5% when one mistake
#' # is expected to be found in the sample from a population of 1000 transactions.
#' calc.n.hypergeometric(expected.mistakes = 1,
#'                       population.size = 1000,
#'                       materiality = 0.05,
#'                       confidence = 0.95)
#'
#' @keywords sample size
#'
#' @export

calc.n.hypergeometric <- function(expected.mistakes,
                                  population.size,
                                  materiality,
                                  confidence = 0.95){
  alpha <- 1 - confidence
  K <- ceiling(materiality * population.size)
  for( i in 1:2000){
    x <- choose(K, 0:expected.mistakes) *
          choose(population.size-K, i-(0:expected.mistakes)) /
          choose(population.size, i)
    if(sum(x) < alpha)
      return(i)
  }
}
