#' Audit confidence / credible bounds
#'
#' This function takes summary statistics about an evaluated audit sample and calculates
#' a confidence bound accordint to a specified method.
#'
#' @usage confidenceBound(sampleSize, sumErrors, confidence = 0.95, method = "binomial", 
#'                        materiality = NULL)
#'
#' @param sampleSize the number of observations in the sample.
#' @param sumErrors the sum of the errors found in the sample.
#' @param confidence the required confidence for the bound.
#' @param method can be either one of "binomial", "hypergeometric", "poisson".
#'
#' @return A list containing the confidence bound for the audit.
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso
#'
#' @references
#'
#' @examples
#' 
#' # Using the binomial distribution, calculates the upper confidence bound for a materiality of 5% 
#' # when 3 mistakes are found in a sample of 234.
#'
#' # Frequentist planning (n = 234)
#' jfaRes <- jfa::sampleSize(materiality = 0.05, confidence = 0.95,
#'                           expectedError = 0.025, likelihood = "binomial")
#' 
#' # Six errors are allowed in the sample. Three are found.
#' 
#' confidenceBound(sampleSize = jfaRes$sampleSize, sumErrors = 3, method = "binomial")
#' 
#' @keywords confidence bound
#'
#' @export 

confidenceBound <- function(sampleSize, sumErrors, confidence = 0.95, 
                            method = "binomial", materiality = NULL){
  if(sumErrors > sampleSize)
    stop("The sum of the errors is higher than the sample size")
  if(!(method %in% c("binomial")))
    stop("Specify a valid method for the confidence bound")
  n <- sampleSize
  k <- sumErrors
  if(!is.null(materiality)){
    mat <- materiality
  } else {
    mat <- 0
  }
  if(method == "binomial"){
    bound <- stats::binom.test(x = k, n = n, p = mat, alternative = "less", conf.level = confidence)$conf.int[2]
  }
  results <- list()
  results[["n"]] <- sampleSize
  results[["k"]] <- sumErrors
  results[["confidence"]] <- confidence
  results[["confBound"]] <- bound
  results[["method"]] <- method
  if(!is.null(materiality))
    results[["materiality"]] <- materiality
  results[["jfaType"]] <- "evaluation"
  class(results) <- "jfa"
  return(results)
}
