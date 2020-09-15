#' Frequentist and Bayesian Planning for Audit Samples
#'
#' @description This function calculates the required sample size for an audit, based on the poisson, binomial, or hypergeometric likelihood. A prior can be specified to perform Bayesian planning. The returned object is of class \code{jfaPlanning} and can be used with associated \code{print()} and \code{plot()} methods.
#'
#' @usage planning(materiality = NULL, confidence = 0.95, expectedError = 0, minPrecision = NULL, 
#'          likelihood = "poisson", N = NULL, maxSize = 5000, increase = 1, 
#'          prior = FALSE, kPrior = 0, nPrior = 0)
#'
#' @param materiality   a value between 0 and 1 representing the materiality of the audit as a fraction of the total size or value. Can be \code{NULL}, but \code{minPrecision} should be specified in that case.
#' @param confidence    the confidence level desired from the confidence bound (on a scale from 0 to 1). Defaults to 0.95, or 95\% confidence.
#' @param expectedError a fraction representing the percentage of expected mistakes in the sample relative to the total size, or a number (>= 1) that represents the number of expected mistakes.
#' @param minPrecision  The minimum precision to be obtained. Can be \code{NULL}, but \code{materiality} should be specified in that case.
#' @param likelihood    can be one of \code{binomial}, \code{poisson}, or \code{hypergeometric}.
#' @param N             the population size (required for hypergeometric calculations).
#' @param maxSize       the maximum sample size that is considered for calculations. Defaults to 5000 for efficiency. Increase this value if the sample size cannot be found due to it being too large (e.g., for a low materiality).
#' @param increase      the desired increase step for the sample size calculation.
#' @param prior         whether to use a prior distribution when planning. Defaults to \code{FALSE} for frequentist planning. If \code{TRUE}, the prior distribution is updated by the specified likelihood. Chooses a conjugate gamma distribution for the Poisson likelihood, a conjugate beta distribution for the binomial likelihood, and a conjugate beta-binomial distribution for the hypergeometric likelihood.
#' @param kPrior        the prior parameter \eqn{\alpha} (number of errors in the assumed prior sample).
#' @param nPrior        the prior parameter \eqn{\beta} (total number of observations in the assumed prior sample).
#' 
#' @details This section elaborates on the available likelihoods and corresponding prior distributions for the \code{likelihood} argument.
#' 
#' \itemize{
#'  \item{\code{poisson}:          The Poisson likelihood is used as a likelihood for monetary unit sampling (MUS). Its likelihood function is defined as: \deqn{p(x) = \frac{\lambda^x e^{-\lambda}}{x!}} The conjugate \emph{gamma(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{\beta^\alpha x^{\alpha - 1} e^{-\beta x}}{\Gamma(\alpha)}}}
#'  \item{\code{binomial}:         The binomial likelihood is used as a likelihood for record sampling \emph{with} replacement. Its likelihood function is defined as: \deqn{p(x) = {n \choose k} p^k (1 - p)^{n - k}} The conjugate \emph{beta(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{1}{B(\alpha, \beta)} x^{\alpha - 1} (1 - x)^{\beta - 1}}}
#'  \item{\code{hypergeometric}:   The hypergeometric likelihood is used as a likelihood for record sampling \emph{without} replacement. Its likelihood function is defined as: \deqn{p(x = k) = \frac{{K \choose k} {N - K \choose n - k}}{{N \choose n}}} The conjugate \emph{beta-binomial(\eqn{\alpha, \beta})} prior (Dyer and Pierce, 1993) has probability density function: \deqn{f(k | n, \alpha, \beta) = {n \choose k} \frac{B(k + \alpha, n - k + \beta)}{B(\alpha, \beta)}} }
#' }
#'
#' @return An object of class \code{jfaPlanning} containing:
#' 
#' \item{materiality}{the value of the specified materiality. Can be \code{NULL}.}
#' \item{confidence}{the confidence level for the desired population statement.}
#' \item{sampleSize}{the resulting sample size.}
#' \item{expectedSampleError}{the number of full errors that are allowed to occur in the sample.}
#' \item{expectedError}{the specified number of errors as a fraction or as a number.}
#' \item{likelihood}{the specified likelihood.}
#' \item{errorType}{whether the expected errors where specified as a percentage or as an integer.}
#' \item{minPrecision}{The minimum precision to be obtained. Can be \code{NULL}.}
#' \item{N}{the population size (only returned in case of a hypergeometric likelihood).}
#' \item{populationK}{the assumed population errors (only returned in case of a hypergeometric likelihood).}
#' \item{prior}{a list containing information on the prior parameters.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}} \code{\link{sampling}} \code{\link{evaluation}}
#'
#' @references Dyer, D. and Pierce, R.L. (1993). On the Choice of the Prior Distribution in Hypergeometric Sampling. \emph{Communications in Statistics - Theory and Methods}, 22(8), 2125 - 2146.
#'
#' @examples
#' library(jfa)
#' 
#' # Using the binomial distribution, calculates the required sample size for a 
#' # materiality of 5% when 2.5% mistakes are expected to be found in the sample.
#' 
#' # Frequentist planning with binomial likelihood:
#' 
#' p1 <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, 
#'                likelihood = "binomial")
#' print(p1)
#' 
#' # ------------------------------------------------------------
#' #              jfa Planning Summary (Frequentist)
#' # ------------------------------------------------------------     
#' # Input:
#' # 
#' # Confidence:              95% 
#' # Materiality:             5% 
#' # Minimum precision:       100% 
#' # Likelihood:              binomial 
#' # Expected sample errors:  6 
#' # ------------------------------------------------------------
#' # Output:
#' # 
#' # Sample size:             234 
#' # ------------------------------------------------------------
#' 
#' # Bayesian planning with uninformed prior:
#' 
#' p2 <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, 
#'                likelihood = "binomial", prior = TRUE)
#' print(p2)
#' 
#' # ------------------------------------------------------------
#' #              jfa Planning Summary (Bayesian)
#' # ------------------------------------------------------------
#' # Input:
#' # 
#' # Confidence:              95%      
#' # Materiality:             5% 
#' # Minimum precision:       100% 
#' # Likelihood:              binomial 
#' # Prior:                   beta(1, 1)
#' # Expected sample errors:  5.5
#' # ------------------------------------------------------------
#' # Output:
#' #
#' # Sample size:            220
#' # ------------------------------------------------------------ 
#' 
#' # Bayesian planning with informed prior:
#' 
#' prior <- auditPrior(materiality = 0.05, confidence = 0.95, cr = 0.6, 
#'                     expectedError = 0.025, likelihood = "binomial", method = "arm")
#' 
#' p3 <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025,
#'                prior = prior)
#' print(p3)
#' 
#' # ------------------------------------------------------------
#' #              jfa Planning Summary (Bayesian)
#' # ------------------------------------------------------------
#' # Input:
#' # 
#' # Confidence:              95%      
#' # Materiality:             5% 
#' # Minimum precision:       100% 
#' # Likelihood:              binomial 
#' # Prior:                   beta(2.275, 50.725)
#' # Expected sample errors:  4.23
#' # ------------------------------------------------------------
#' # Output:
#' #
#' # Sample size:            169
#' # ------------------------------------------------------------
#'
#' @keywords planning sample size audit
#'
#' @export

planning <- function(materiality = NULL, confidence = 0.95, expectedError = 0, minPrecision = NULL,
                     likelihood = "poisson", N = NULL, maxSize = 5000, increase = 1,
                     prior = FALSE, kPrior = 0, nPrior = 0){
  
  if(class(prior) == "jfaPrior"){
    nPrior <- prior$nPrior
    kPrior <- prior$kPrior
    likelihood <- prior$likelihood
  }

  if(is.null(materiality) && is.null(minPrecision))
    stop("Specify the materiality or the minimum precision")
  if(!is.null(minPrecision) && minPrecision == 0)
    stop("The minimum required precision cannot be zero.")
  if(!(likelihood %in% c("binomial", "hypergeometric", "poisson")))
    stop("Specify a valid likelihood.")
  if((class(prior) == "logical" && prior == TRUE) && kPrior < 0 || nPrior < 0)
    stop("When you specify a prior, both kPrior and nPrior should be > 0.")

  ss <- NULL

  if(expectedError >= 0 && expectedError < 1){
    errorType <- "percentage"
    if(!is.null(materiality) && expectedError >= materiality)
      stop("The expected errors are higher than materiality")
  } else if(expectedError >= 1){
    errorType <- "integer"
    if(expectedError%%1 != 0 && likelihood %in% c("binomial", "hypergeometric"))
      stop("When expectedError > 1 and the likelihood is binomial or hypergeometric, the value must be an integer.")
  }

  if(is.null(minPrecision))
    minPrecision <- 1
  if(is.null(materiality))
    materiality <- 1

  samplingFrame <- seq(from = 0, to = maxSize, by = increase)
  samplingFrame[1] <- 1
  
  if(likelihood == "poisson"){
    for(i in samplingFrame){
      if(errorType == "percentage"){
        implicitK <- expectedError * i
      } else if(errorType == "integer"){
        implicitK <- expectedError
      }
      if(i <= implicitK)
        next
      if((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior"){
        bound <- stats::qgamma(confidence, shape = 1 + kPrior + implicitK, rate = nPrior + i)
        mle <- (1 + kPrior + implicitK - 1) / (nPrior + i)
        if(bound < materiality && (bound - mle) < minPrecision){
          ss <- i
          break
        }
      } else {
        prob <- stats::pgamma(materiality, shape = 1 + implicitK, rate = i)
        bound <- stats::qgamma(confidence, shape = 1 + implicitK, rate = i)
        mle <- implicitK / i
        if(prob >= confidence && (bound - mle) < minPrecision){
          ss <- i
          break
        }
      }
    }
  } else if(likelihood == "binomial"){
    for(i in samplingFrame){
      if(errorType == "percentage"){
        implicitK <- expectedError * i
      } else if(errorType == "integer"){
        implicitK <- expectedError
      }
      if(i <= implicitK)
        next
      if((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior"){
        bound <- stats::qbeta(confidence, shape1 = 1 + kPrior + implicitK, shape2 = 1 + nPrior - kPrior + i - implicitK)
        mle <- (1 + kPrior + implicitK - 1) / (1 + kPrior + implicitK + 1 + nPrior - kPrior + i - implicitK - 2)
        if(bound < materiality && (bound - mle) < minPrecision){
          ss <- i
          break
        }
      } else {
        implicitK <- ceiling(implicitK)
        prob <- stats::dbinom(0:implicitK, size = i, prob = materiality)
        bound <- stats::binom.test(x = implicitK, n = i, p = materiality, alternative = "less", conf.level = confidence)$conf.int[2]
        mle <- implicitK / i
        if(sum(prob) < (1 - confidence) && (bound - mle) < minPrecision){
          ss <- i
          break
        }
      }
    }
  } else if(likelihood == "hypergeometric"){
    if(is.null(N))
      stop("Specify a population size N")
    populationK <- ceiling(materiality * N)
    for(i in samplingFrame){
      if(errorType == "percentage"){
        implicitK <- ceiling(expectedError * i)
      } else if(errorType == "integer"){
        implicitK <- expectedError
      }
      if(i <= implicitK)
        next
      if((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior"){
        if(is.null(N))
          stop("The beta-binomial distribution requires that you specify the population size N")
        bound <- .qBetaBinom(p = confidence, N = N - i + implicitK, shape1 = 1 + kPrior + implicitK, shape2 = 1 + nPrior - kPrior + i - implicitK) / N
        mle <- (1 + kPrior + implicitK - 1) / (1 + kPrior + implicitK + 1 + nPrior + i - implicitK - 2)
        if(bound < materiality && (bound - mle) < minPrecision){
          ss <- i
          break
        }
      } else {
        prob <- stats::dhyper(x = 0:implicitK, m = populationK, n = N - populationK, k = i)
        bound <- stats::phyper(q = implicitK, m = populationK, n = N - populationK, k = i)
        mle <- (N * implicitK + implicitK) / i / N
        if(sum(prob) < (1 - confidence) && (bound - mle) < minPrecision){
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
  results[["expectedSampleError"]]  <- as.numeric(round(implicitK, 2))
  results[["expectedError"]]        <- as.numeric(expectedError)
  results[["likelihood"]]           <- as.character(likelihood)
  results[["errorType"]]            <- as.character(errorType)
  results[["minPrecision"]]         <- as.numeric(minPrecision)
  if(likelihood == "hypergeometric"){
    results[["N"]]                  <- as.numeric(N)
    results[["populationK"]]        <- as.numeric(populationK)
  }
  results[["prior"]]                <- list()
  if((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior"){
    results[["prior"]]$prior        <- as.logical(TRUE)
    results[["prior"]]$priorD       <- switch(likelihood, "poisson" = "gamma", "binomial" = "beta", "hypergeometric" = "beta-binomial")
    results[["prior"]]$kPrior       <- as.numeric(round(kPrior, 3))
    results[["prior"]]$nPrior       <- as.numeric(round(nPrior, 3))
    results[["prior"]]$aPrior       <- 1 + results[["prior"]]$kPrior
    results[["prior"]]$bPrior       <- ifelse(likelihood == "poisson", yes = results[["prior"]]$nPrior, no = 1 + results[["prior"]]$nPrior - results[["prior"]]$kPrior)
  } else {
    results[["prior"]]$prior          <- as.logical(FALSE)
  }
  class(results)                    <- "jfaPlanning"
  return(results)
}