#' Frequentist and Bayesian planning for audit samples
#'
#' This function calculates the required sample size for an audit, based on the poisson, binomial, or hypergeometric likelihood. A prior can be specified to perform Bayesian planning. The returned object has a print() and a plot() method.
#'
#' @usage planning(materiality, confidence = 0.95, expectedError = 0, 
#'                likelihood = "poisson", N = NULL, maxSize = 5000, 
#'                prior = FALSE, kPrior = 0, nPrior = 0)
#'
#' @param materiality a value between 0 and 1 representing the materiality of the audit as a fraction of the total size or value.
#' @param confidence the confidence level desired from the confidence bound (on a scale from 0 to 1). Defaults to 0.95, or 95\% confidence.
#' @param expectedError a fraction representing the percentage of expected mistakes in the sample relative to the total size, or a number (> 1) that represents the number of expected mistakes.
#' @param likelihood can be one of \emph{binomial}, \emph{poisson}, or \emph{hypergeometric}.
#' @param N the population size (required for hypergeometric calculations).
#' @param maxSize the maximum sample size that is considered for calculations. Increase this value if the samle size cannot be found due to it being too large (e.g., for low materialities).
#' @param prior If TRUE, adds a prior distribution to be updated by the specified likelihood. Chooses a conjugate beta distribution for the binomial likelihood, a conjugate gamma distribution for the poisson likelihood, and a conjugate beta-binomial distribution for the hypergeometric likelihood. Defaults to FALSE for frequentist planning.
#' @param kPrior the prior parameter \eqn{\alpha} (errors in the assumed prior sample).
#' @param nPrior the prior parameter \eqn{\beta} (sample size of assumed prior sample).
#'
#' @return An object of class \emph{jfaPlanning}.
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso
#'
#' @references
#'
#' @examples
#' 
#' library(jfa)
#' 
#' # Using the binomial distribution, calculates the required sample size for a 
#' # materiality of 5% when 2.5% mistakes are expected to be found in the sample.
#' 
#' # Frequentist planning with binomial likelihood.
#' planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, 
#'          likelihood = "binomial")
#' 
#' # Bayesian planning with uninformed prior.
#' planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, 
#'          likelihood = "binomial", prior = TRUE)
#' 
#' # Bayesian planning with informed prior (based on 10 correct observations).
#' planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, 
#'          likelihood = "binomial", prior = TRUE, kPrior = 0, nPrior = 10)
#'
#' @keywords planning sample size
#'
#' @export

planning <- function(materiality, confidence = 0.95, expectedError = 0, likelihood = "poisson", 
                     N = NULL, maxSize = 5000, prior = FALSE, kPrior = 0, nPrior = 0){
  
  if(is.null(materiality))
    stop("Specify the materiality")
  if(!(likelihood %in% c("binomial", "hypergeometric", "poisson")))
    stop("Specify a valid distribution")
  # if(prior && is.null(kPrior) && is.null(nPrior))
  #   stop("When you specify a prior, both kPrior and nPrior should be specified")
  if(prior && (kPrior < 0 || nPrior < 0))
    stop("When you specify a prior, both kPrior and nPrior should be higher than zero")
  
  ss <- NULL
  
  if(expectedError >= 0 && expectedError < 1){
    errorType <- "percentage"
    if(expectedError >= materiality)
      stop("The expected errors are higher than materiality")
    startN <- 1
  } else {
    errorType <- "integer"
    startN <- expectedError
  }
  
  if(likelihood == "poisson"){
    for(i in startN:maxSize){
      if(errorType == "percentage"){
        implicitK <- ceiling(expectedError * i)
      } else if(errorType == "integer"){
        implicitK <- expectedError
      }
      if(prior){
        bound <- stats::qgamma(confidence, shape = 1 + kPrior + implicitK, rate = nPrior + i)
        if(bound < materiality){
          ss <- i
          break
        }
      } else {
        prob <- stats::pgamma(materiality, shape = 1 + implicitK, rate = i)
        if(prob > confidence){
          ss <- i
          break
        }
      }
    }
  } else if(likelihood == "binomial"){
    for(i in startN:maxSize){
      if(errorType == "percentage"){
        implicitK <- ceiling(expectedError * i)
      } else if(errorType == "integer"){
        implicitK <- expectedError
      }
      if(prior){
        bound <- stats::qbeta(confidence, shape1 = 1 + kPrior + implicitK, shape2 = 1 + nPrior - kPrior + i - implicitK)
        if(bound < materiality){
          ss <- i
          break
        }
      } else {
        prob <- stats::dbinom(0:implicitK, size = i, prob = materiality)
        if(sum(prob) < (1 - confidence)){
          ss <- i
          break
        }
      }
    }
  } else if(likelihood == "hypergeometric"){
    if(is.null(N))
      stop("Specify a population size N")
    populationK <- ceiling(materiality * N)
    for(i in startN:maxSize){
      if(errorType == "percentage"){
        implicitK <- ceiling(expectedError * i)
      } else if(errorType == "integer"){
        implicitK <- expectedError
      }
      if(prior){
        bound <- jfa:::.qBetaBinom(p = confidence, N = N - i, shape1 = 1 + kPrior + implicitK, shape2 = 1 + nPrior - kPrior + i - implicitK) / N
        if(bound < materiality){
          ss <- i
          break
        }
      } else {
        prob <- stats::dhyper(x = 0:implicitK, m = populationK, n = N - populationK, k = i)
        if(sum(prob) < (1 - confidence)){
          ss <- i
          break
        }
      }
    }
  }
  
  if(is.null(ss))
    stop("Sample size could not be calculated, please increase the maxSize argument")
  
  results <- list()
  results[["materiality"]]          <- as.numeric(materiality)
  results[["confidence"]]           <- as.numeric(confidence)
  results[["sampleSize"]]           <- as.numeric(ceiling(ss))
  results[["expectedSampleError"]]  <- as.numeric(implicitK)
  results[["expectedError"]]        <- as.numeric(expectedError)
  results[["likelihood"]]           <- as.character(likelihood)
  results[["errorType"]]            <- as.character(errorType)
  if(likelihood == "hypergeometric"){
    results[["N"]]                  <- as.numeric(N)
    results[["populationK"]]        <- as.numeric(populationK)
  }
  results[["prior"]]                <- list()
  results[["prior"]]$prior          <- as.logical(prior)
  if(prior){
    results[["prior"]]$priorD       <- switch(likelihood, "poisson" = "gamma", "binomial" = "beta", "hypergeometric" = "beta-binomial")
    results[["prior"]]$kPrior       <- as.numeric(kPrior)
    results[["prior"]]$nPrior       <- as.numeric(nPrior)
    results[["prior"]]$aPrior       <- 1 + results[["prior"]]$kPrior
    results[["prior"]]$bPrior       <- ifelse(likelihood == "poisson", yes = nPrior, no = 1 + nPrior - kPrior)
  }
  class(results)                    <- "jfaPlanning"
  return(results)
}