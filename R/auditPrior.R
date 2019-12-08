#' Create a Prior Distribution
#'
#' @description This function creates a prior distribution according to the audit risk model. The returned object is of class \code{jfaPrior} and can be used with associated \code{print()} and \code{plot()} methods. \code{jfaPrior} objects can be used as input argument for the \code{prior} argument in other functions.
#'
#' @usage auditPrior(materiality, confidence = 0.95, method = "arm", ir = 1, cr = 1, 
#'            expectedError = 0, likelihood = "binomial", N = NULL)
#' 
#' @param materiality     a value between 0 and 1 representing the materiality of the audit as a fraction of the total size or value.
#' @param confidence      the confidence level desired from the confidence bound (on a scale from 0 to 1). Defaults to 0.95, or 95\% confidence.
#' @param method          the method by which the prior distribution is constructed. Currently only supports the \code{arm} method, which uses the audit risk model (Derks et al., 2019).
#' @param ir              the inherent risk probability from the audit risk model. Defaults to 1 for 100\% risk.
#' @param cr              the inherent risk probability from the audit risk model. Defaults to 1 for 100\% risk.
#' @param expectedError   a fraction representing the percentage of expected mistakes in the sample relative to the total size, or a number (>= 1) that represents the number of expected mistakes.
#' @param likelihood      can be one of \code{binomial}, \code{poisson}, or \code{hypergeometric}.
#' @param N               the population size (required for hypergeometric calculations).
#' 
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#' 
#' @seealso \code{\link{planning}} \code{\link{sampling}} \code{\link{evaluation}}
#' 
#' @references Derks, K., de Swart, J., Wagenmakers, E.-J., Wille, J., & Wetzels, R. (2019). JASP for audit: Bayesian tools for the auditing practice.
#'
#' @return An object of class \code{jfaPrior} containing:
#' 
#' \item{method}{the method by which the prior distribution is constructed.}
#' \item{likelihood}{the likelihood by which the prior distribution is updated.}
#' \item{priorD}{the name of the probability density function of the prior distribution.}
#' \item{nPrior}{the prior assumed sample size.}
#' \item{kPrior}{the prior assumed sample errors}
#' \item{aPrior}{the prior parameter alpha.}
#' \item{bPrior}{the prior parameter beta.}
#' \item{materiality}{the materiality that was used to construct the prior distribution.}
#' \item{N}{if specified as input, the population size.}
#'
#' @keywords prior distribution audit
#'
#' @examples 
#' library(jfa)
#' set.seed(1)
#' 
#' # Generate some audit data (N = 1000).
#' population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), 
#'                          bookValue = runif(n = 1000, min = 700, max = 1000))
#' 
#' # Specify the materiality, confidence, and expected errors.
#' materiality   <- 0.05   # 5%
#' confidence    <- 0.95   # 95%
#' expectedError <- 0.025  # 2.5%
#' 
#' # Specify the inherent risk (ir) and control risk (cr).
#' ir <- 1     # 100%
#' cr <- 0.6   # 60%
#' 
#' # Create a beta prior distribution according to the Audit Risk Model (arm) 
#' # and a binomial likelihood.
#' prior <- auditPrior(materiality = materiality, confidence = confidence, 
#'                     method = "arm", ir = ir, cr = cr, 
#'                     expectedError = expectedError, likelihood = "binomial")
#' print(prior)
#' 
#' # jfa prior distribution for arm method:
#' #      
#' # Prior sample size:     51 
#' # Prior errors:          1.27 
#' # Prior:                 beta(2.275, 50.725)
#' 
#' # Calculate the sample size according to the binomial distribution with the specified prior.
#' sampleSize <- planning(materiality = materiality, confidence = confidence, 
#'                        expectedError = expectedError, prior = prior, likelihood = "binomial")
#' print(sampleSize)
#' 
#' # jfa planning results for beta prior with binomial likelihood:
#' #      
#' # Materiality:             5% 
#' # Confidence:              95% 
#' # Sample size:             169 
#' # Allowed sample errors:   4.23 
#' # Prior parameter alpha:   2.275 
#' # Prior parameter beta:    50.725
#' 
#' # Draw sample using random monetary unit sampling.
#' sampleResult <- sampling(population = population, sampleSize = sampleSize, 
#'                          algorithm = "random", units = "mus", seed = 1, bookValues = "bookValue")
#' print(sampleResult)
#' 
#' # jfa sampling results for random monetary unit sampling: 
#' #      
#' # Population size:         1000 
#' # Sample size:             169 
#' # Proportion n/N:          0.169 
#' # Precentage of value:     16.84%
#' 
#' # Isolate the sample.
#' sample <- sampleResult$sample
#' 
#' # For this example, we use the book values as audit values. Implies zero errors at this point.
#' sample$trueValue <- sample$bookValue
#' 
#' # One overstatement is found.
#' sample$trueValue[2] <- sample$trueValue[2] - 500
#' 
#' # Evaluate the sample with one partial error using the posterior distribution.
#' conclusion <- evaluation(sample = sample, bookValues = "bookValue", auditValues = "trueValue", 
#'                          prior = prior, materiality = 0.05)
#' print(conclusion)
#' 
#' # jfa evaluation results for binomial likelihood with prior:
#' #   
#' # Materiality:           5% 
#' # Confidence:            95% 
#' # Upper bound:           2.729% 
#' # Sample size:           169 
#' # Sample errors:         1 
#' # Conclusion:            Approve population
#'  
#' @export

auditPrior <- function(materiality, confidence = 0.95, method = "arm", ir = 1, cr = 1, 
                       expectedError = 0, likelihood = "binomial", N = NULL){
  
  if(!(method %in% c("arm")))
    stop("Currently only method = 'arm' is supported")
  
  if(method == "arm"){
    nPlus <- jfa::planning(materiality = materiality, confidence = confidence, expectedError = expectedError, likelihood = likelihood, prior = TRUE, N = N)$sampleSize
    alpha <- (1 - confidence) / (ir * cr)
    nMin <- planning(materiality = materiality, confidence = 1 - alpha, expectedError = expectedError, likelihood = likelihood, prior = TRUE, N = N)$sampleSize
    if(expectedError >= 0 && expectedError < 1){
      kPlus <- nPlus * expectedError
      kMin <-  nMin * expectedError
    } else {
      kPlus <- expectedError
      kMin <- expectedError
    }
    nPrior <- nPlus - nMin
    kPrior <- kPlus - kMin
  }
  
  result <- list()
  
  result$method       <- as.character(method)
  result$likelihood   <- as.character(likelihood)
  result$priorD       <- switch(likelihood, "poisson" = "gamma", "binomial" = "beta", "hypergeometric" = "beta-binomial")
  result$kPrior       <- as.numeric(round(kPrior, 3))
  result$nPrior       <- as.numeric(round(nPrior, 3))
  result$aPrior       <- as.numeric(1 + result$kPrior)
  result$bPrior       <- ifelse(likelihood == "poisson", yes = nPrior, no = 1 + nPrior - kPrior)
  result$materiality  <- as.numeric(materiality)
  result$N            <- N
  
  class(result)       <- "jfaPrior"
  return(result)
}
