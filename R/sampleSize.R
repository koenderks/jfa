#' Audit sample sizes
#'
#' This function calculates the required sample size for an audit, based on
#' the poisson, binomial, or hypergeometric likelihood. A prior can be specified
#' to perform Bayesian planning.
#'
#' @usage sampleSize(materiality = NULL, confidence = 0.95, expectedError = 0, 
#'                   distribution = "poisson", errorType = "percentage", N = NULL, 
#'                   maxSize = 5000, prior = FALSE, priorK = NULL, priorN = NULL)
#'
#' @param materiality a value between 0 and 1 representing the materiality of the audit as a fraction of the total size or value.
#' @param confidence the amount of confidence desired from the resulting confidence bound
#' (on a scale from 0 to 1). Defaults to 0.95, or 95\% confidence.
#' @param expectedError a fraction representing the number of expected mistakes in the sample relative to the total size.
#' @param likelihood can be one of "binomial", "poisson", or "hypergeometric".
#' @param errorType whether the errors are a percentage ("percentage") of the total or an absolute value ("integer").
#' @param N the population size (required for hypergeometric calculations).
#' @param maxSize the maximum sample size that is considered for calculations (for efficiency). 
#' For low materialities, increasing this parameter may be wise.
#' @param prior if TRUE, add a prior distribution to be updated by the likelihood. Chooses a beta distribution
#' for the binomial likelihood, a gamma distribution for the poisson likelihood, and a beta-binomial distribution for the 
#' hypergeometric likelihood. Defaults to FALSE.
#' @param priorK the prior parameter alpha (errors in the assumed prior sample).
#' @param priorN the prior parameter beta (sample size of assumed prior sample).
#'
#' @return A list containing the required sample size for the audit.
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso
#'
#' @references
#'
#' @examples
#' # Using the binomial distribution, calculates the required sample size for a materiality of 5% 
#' # when 2.5% mistakes are expected to be found in the sample.
#' 
#' # Frequentist
#' sampleSize(materiality = 0.05, confidence = 0.95, expectedError = 0.025, 
#'            likelihood = "binomial")
#' 
#' # Bayesian (uninformative beta(1, 1) prior)
#' sampleSize(materiality = 0.05, confidence = 0.95, expectedError = 0.025, 
#'            likelihood = "binomial", prior = TRUE, priorK = 0, priorN = 0)
#' 
#' # Bayesian (informative prior of 10 assumed correct observatiosn)
#' sampleSize(materiality = 0.05, confidence = 0.95, expectedError = 0.025, 
#'            likelihood = "binomial", prior = TRUE, priorK = 0, priorN = 10)
#'
#' @keywords sample size
#'
#' @export

sampleSize <- function(materiality = NULL, confidence = 0.95, expectedError = 0, likelihood = "poisson", 
                       errorType = "percentage", N = NULL, maxSize = 5000, 
                       prior = FALSE, priorK = NULL, priorN = NULL){
  if(is.null(materiality))
    stop("Specify the materiality")
  if(errorType == "percentage" && expectedError >= materiality)
    stop("The expected errors are higher than materiality")
  if(!(distribution %in% c("binomial", "hypergeometric", "poisson")))
    stop("Specify a valid distribution")
  if(prior && is.null(priorK) && is.null(priorN))
    stop("When you specify a prior, both priorK and priorN should be specified")
  if(prior && (priorK < 0 || priorN < 0))
    stop("When you specify a prior, both priorK and priorN should be specified")
  alpha <- 1 - confidence
  ss <- NULL
  if(errorType == "integer"){
    startN <- expectedError
  } else {
    startN <- 1
  }
  if(likelihood == "poisson"){
    for(i in startN:maxSize){
      if(errorType == "percentage"){
        implicitK <- ceiling(expectedError * i)
      } else if(errorType == "integer"){
        implicitK <- expectedError
      }
      if(prior){
        bound <- stats::qgamma(confidence, shape = 1 + priorK + implicitK, rate = priorN + i)
        if(bound < materiality){
          ss <- i
          break
        }
      } else {
        prob <- stats::pgamma(materiality, shape = 1 + implicitK, rate = i)
        if(prob > (1 - alpha)){
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
        bound <- stats::qbeta(confidence, shape1 = 1 + priorK + implicitK, shape2 = 1 + priorN - implicitK + i)
        if(bound < materiality){
          ss <- i
          break
        }
      } else {
        prob <- stats::dbinom(0:implicitK, size = i, prob = materiality)
        if(sum(prob) < alpha){
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
        bound <- jfa:::.qBetaBinom(p = 1 - alpha, N = N - i, shape1 = 1 + priorK + implicitK, shape2 = 1 + priorN - priorK + (i - implicitK)) / N
        if(bound < materiality){
          ss <- i
          break
        }
      } else {
        prob <- stats::dhyper(x = 0:implicitK, m = populationK, n = N - populationK, k = i)
        if(sum(prob) < alpha){
          ss <- i
          break
        }
      }
    }
  }
  if(is.null(ss))
    stop("Sample size could not be calculated, please increase the maxSize argument")
  results <- list(materiality = as.numeric(materiality), 
                  confidence = as.numeric(confidence), 
                  sampleSize = as.numeric(ss), 
                  expectedSampleError = as.numeric(implicitK), 
                  expectedError = as.numeric(expectedError), 
                  likelihood = as.character(likelihood))
  if(prior){
    results[["priorK"]] <- as.numeric(priorK)
    results[["priorN"]] <- as.numeric(priorN)
  }
  return(results)
}

.dBetaBinom <- function (x, N, shape1, shape2){
  logval <- lbeta(x + shape1, N - x + shape2) - lbeta(shape1, shape2) + lchoose(N, x)
  ret <- exp(logval)
  return(ret)
}

.qBetaBinom <- function (p, N, shape1, shape2){
  pp <- cumsum(jfa:::.dBetaBinom(0:N, N, shape1, shape2))
  return(sapply(p, function(x) sum(pp < x)))
}