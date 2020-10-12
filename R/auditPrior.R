#' Create a Prior Distribution with Audit Information
#'
#' @description This function creates a prior distribution for Bayesian audit sampling according to several methods discussed in Derks et al. (2020). The returned object is of class \code{jfaPrior} and can be used with associated \code{print()} and \code{plot()} methods. \code{jfaPrior} objects can be used as input for the \code{prior} argument in other functions.
#'
#' @usage auditPrior(materiality = NULL, confidence = 0.95, method = "arm", ir = 1, 
#'            cr = 1, expectedError = 0, likelihood = "binomial", N = NULL, 
#'            pHmin = NULL, pHplus = NULL, factor = 1, sampleN = 0, sampleK = 0)
#' 
#' @param materiality     a value between 0 and 1 representing the materiality of the audit as a fraction of the total size or value. Can be \code{NULL} for some methods.
#' @param confidence      the confidence level desired from the confidence bound (on a scale from 0 to 1). Defaults to 0.95, or 95\% confidence.
#' @param method          the method by which the prior distribution is constructed. Defaults to the \code{arm} method, which uses the audit risk model (Derks et al., 2019). Can be one of \code{none}, \code{median}, \code{hypotheses}, \code{arm}, \code{sample} or \code{factor}. See the Details section for more information.
#' @param ir              the inherent risk probability from the audit risk model. Defaults to 1 for 100\% risk.
#' @param cr              the inherent risk probability from the audit risk model. Defaults to 1 for 100\% risk.
#' @param expectedError   a fraction representing the percentage of expected mistakes in the sample relative to the total size, or a number (>= 1) that represents the number of expected mistakes.
#' @param likelihood      can be one of \code{binomial}, \code{poisson}, or \code{hypergeometric}. See the Details section for more information.
#' @param N               the population size (only required when \code{likelihood = 'hypergeometric'}).
#' @param pHmin           When using \code{method = 'hypotheses'}, the prior probability of the hypothesis \eqn{\theta <} materiality.
#' @param pHplus          When using \code{method = 'hypotheses'}, the prior probability of the hypothesis \eqn{\theta >} materiality.
#' @param factor          When using \code{method = 'factor'}, the value of the weighting factor for the results of the previous sample.
#' @param sampleN         When using method \code{sample} or \code{factor}, the number of transactions that were inspected in the previous sample.
#' @param sampleK         When using method \code{sample} or \code{factor}, the total taint in the previous sample.
#' 
#' @details This section elaborates on the available methods for constructing a prior distribution.
#' 
#' \itemize{
#'  \item{\code{none}:              This method constructs a prior distribution according to the principle of minimum information.}
#'  \item{\code{median}:            This method constructs a prior distribution so that the prior probabilities of tolerable and intolerable misstatement are equal.}
#'  \item{\code{hypotheses}:        This method constructs a prior distribution with specified prior probabilities for the hypotheses of tolerable and intolerable misstatement. Requires specification of the \code{pHmin} and \code{pHplus} arguments.}
#'  \item{\code{arm}:               This method constructs a prior distribution according to the assessed risks in the audit risk model. Requires specification of the \code{ir} and \code{cr} arguments.}
#'  \item{\code{sample}:            This method constructs a prior distribution on the basis of an earlier sample. Requires specification of the \code{sampleN} and \code{sampleK} arguments.}
#'  \item{\code{factor}:            This method constructs a prior distribution on the basis of last year's results and a weighting factor. Requires specification of the \code{factor}, \code{sampleN} and \code{sampleK} arguments.}
#' }
#'
#' @details This section elaborates on the available likelihoods and corresponding prior distributions for the \code{likelihood} argument.
#' 
#' \itemize{
#'  \item{\code{poisson}:          The Poisson likelihood is used as a likelihood for monetary unit sampling (MUS). Its likelihood function is defined as: \deqn{p(x) = \frac{\lambda^x e^{-\lambda}}{x!}} The conjugate \emph{gamma(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{\beta^\alpha x^{\alpha - 1} e^{-\beta x}}{\Gamma(\alpha)}}}
#'  \item{\code{binomial}:         The binomial likelihood is used as a likelihood for record sampling \emph{with} replacement. Its likelihood function is defined as: \deqn{p(x) = {n \choose k} p^k (1 - p)^{n - k}} The conjugate \emph{beta(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{1}{B(\alpha, \beta)} x^{\alpha - 1} (1 - x)^{\beta - 1}}}
#'  \item{\code{hypergeometric}:   The hypergeometric likelihood is used as a likelihood for record sampling \emph{without} replacement. Its likelihood function is defined as: \deqn{p(x = k) = \frac{{K \choose k} {N - K \choose n - k}}{{N \choose n}}} The conjugate \emph{beta-binomial(\eqn{\alpha, \beta})} prior (Dyer and Pierce, 1993) has probability density function: \deqn{f(k | n, \alpha, \beta) = {n \choose k} \frac{B(k + \alpha, n - k + \beta)}{B(\alpha, \beta)}} }
#' }
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
#' \item{pHmin}{For methods \code{median} and \code{hypotheses}, the prior probability of the hypothesis \eqn{\theta <} materiality.}
#' \item{pHmin}{For methods \code{median} and \code{hypotheses}, the prior probability of the hypothesis \eqn{\theta >} materiality.}
#' \item{sampleN}{For methods \code{sample} and \code{factor}, the total number of transactions in the earlier sample.}
#' \item{sampleK}{For methods \code{sample} and \code{factor}, the number of transactions that were misstated in the earlier sample.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#' 
#' @seealso \code{\link{planning}} \code{\link{sampling}} \code{\link{evaluation}}
#' 
#' @references Derks, K., de Swart, J., Wagenmakers, E.-J., Wille, J., & Wetzels, R. (2019). JASP for audit: Bayesian tools for the auditing practice.
#' @references Derks, K., de Swart, J., van Batenburg, P. Wagenmakers, E.-J., & Wetzels, R. (2020). Priors in a Bayesian Audit: How Integrating Information into the Prior Distribution can Improve Audit Transparency and Efficiency.
#'
#' @keywords prior distribution audit
#'
#' @examples 
#' library(jfa)
#' 
#' # Specify the materiality, confidence, and expected errors:
#' materiality   <- 0.05   # 5%
#' confidence    <- 0.95   # 95%
#' expectedError <- 0.025  # 2.5%
#' 
#' # Specify the inherent risk (ir) and control risk (cr):
#' ir <- 1     # 100%
#' cr <- 0.6   # 60%
#' 
#' # Create a beta prior distribution according to the Audit Risk Model (arm) 
#' # and a binomial likelihood:
#' prior <- auditPrior(materiality = materiality, confidence = confidence, 
#'                     method = "arm", ir = ir, cr = cr, 
#'                     expectedError = expectedError, likelihood = "binomial")
#' print(prior)
#' 
#' # ------------------------------------------------------------
#' #         jfa Prior Distribution Summary (Bayesian)
#' # ------------------------------------------------------------
#' # Input:
#' # 
#' # Confidence:              0.95    
#' # Expected sample errors:  0.025       
#' # Likelihood:              binomial 
#' # Specifics:               Inherent risk = 1; Internal control risk = 0.6; Detection risk = 0.083 
#' # ------------------------------------------------------------
#' # Output: 
#' # 
#' # Prior distribution:      beta(2.275, 50.725) 
#' # Implicit sample size:    51 
#' # Implicit errors:         1.275 
#' # ------------------------------------------------------------
#' @export

auditPrior <- function(materiality = NULL, confidence = 0.95, method = "arm", ir = 1, 
                       cr = 1, expectedError = 0, likelihood = "binomial", N = NULL,
                       pHmin = NULL, pHplus = NULL, factor = 1, sampleN = 0, sampleK = 0){
  
  if(!(method %in% c("none", "median", "hypotheses", "arm", "sample", "factor")))
    stop("Currently only method = 'none', 'median', 'hypotheses', 'arm', 'sample', and 'factor' are supported")
  
  if(is.null(materiality) && method %in% c("median", "hypotheses", "arm"))
    stop("The methods 'none', 'median', and 'hypotheses' require that you specify a value for the materiality.")

  if(method == "none"){
    nPrior <- 0
    kPrior <- 0
  } else if(method == "arm"){
    if(is.null(ir) || is.null(cr) || is.null(materiality))
      stop("Method = 'arm' requires non-null 'materiality', 'ir', and 'cr' arguments.")
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
  } else if(method == "median"){
    if(likelihood == "hypergeometric")
      stop("Method = 'median' is not supported for the hypergeometric likelihood.")
    probH1 <- probH0 <- 0.5
    if(expectedError == 0){
      nPrior <- switch(likelihood, 
                       "poisson" = -(log(probH1) / materiality), 
                       "binomial" = log(probH1) / log(1 - materiality) - 1)
      kPrior <- 0
    } else {
      if(likelihood == "binomial"){
        alpha <- (-4 * materiality * expectedError + 3 * materiality - expectedError) / (3 * (materiality - expectedError))
        beta <- (4 * materiality * expectedError - materiality - 5 * expectedError + 2) / (3 * (materiality - expectedError))
        kPrior <- alpha - 1
        nPrior <- beta + kPrior - 1
      } else if(likelihood == "poisson"){
          for(alpha in seq(1, 5, 0.001)){
            beta <- (alpha - 1) / expectedError
            median <- stats::qgamma(p = 0.5, shape = alpha, rate = beta)
            mode <- (alpha - 1) / beta
            if(round(median, 3) == materiality && round(mode, 3) == expectedError){
              break
            }
          }
        kPrior <- alpha - 1
        nPrior <- beta
      }
    }
  } else if(method == "hypotheses"){
    if(likelihood == "hypergeometric")
      stop("Method = 'hypotheses' is not supported for the hypergeometric likelihood.")
    if(is.null(pHplus) && is.null(pHmin))
      stop("Method = 'hypotheses' requires non-null 'materiality' and 'pHplus' or 'pHplus' arguments.")
    if((!is.null(pHplus) && !is.null(pHmin)) && pHplus + pHmin != 1)
      stop("The values for 'pHplus' and 'pHmin' should sum to one.")  
    if(is.null(pHplus) && !is.null(pHmin)){
      probH1 <- 1 - pHmin
    } else{
      probH1 <- pHplus
    }
    probH0 <- 1 - probH1
    if(expectedError != 0)
      stop("Expected errors are not supported for method = 'hypotheses'.")
    nPrior <- switch(likelihood, 
                      "poisson" = -(log(probH1) / materiality), 
                      "binomial" = log(probH1) / log(1 - materiality) - 1)
    kPrior <- 0
  } else if(method == "sample"){
    if(is.null(sampleN) || is.null(sampleK))
      stop("Method = 'sample' requires non-null 'sampleN', and 'sampleK' arguments.")
    nPrior <- sampleN
    kPrior <- sampleK
  } else if(method == "factor"){
    if(is.null(sampleN) || is.null(sampleK) || is.null(factor))
      stop("Method = 'factor' requires non-null 'factor', 'sampleN', and 'sampleN=K' arguments.")  
    nPrior <- sampleN * factor
    kPrior <- sampleK * factor 
  }
  
  result <- list()
  
  result$method       <- as.character(method)
  result$confidence   <- as.numeric(confidence)
  result$likelihood   <- as.character(likelihood)
  result$expectedError <- as.numeric(expectedError)
  result$priorD       <- switch(likelihood, 
                                "poisson" = "gamma", 
                                "binomial" = "beta",
                                "hypergeometric" = "beta-binomial")
  result$kPrior       <- as.numeric(round(kPrior, 3))
  result$nPrior       <- as.numeric(round(nPrior, 3))
  result$aPrior       <- as.numeric(round(1 + kPrior, 3))
  result$bPrior       <- switch(likelihood, 
                                "poisson" = round(nPrior, 3), 
                                "binomial" = round(1 + nPrior - kPrior, 3),
                                "hypergeometric" = round(1 + nPrior - kPrior, 3))
  result$materiality  <- ifelse(is.null(materiality), 
                                yes = 0, 
                                no = as.numeric(materiality))
  result$N            <- N
  if(method == "median" || method == "hypotheses"){
    result$pHmin        <- as.numeric(probH0)
    result$pHplus       <- as.numeric(probH1)
  }
  if(method == "sample" || method == "factor"){
    result$sampleN       <- as.numeric(sampleN)
    result$sampleK       <- as.numeric(sampleK)
    result$factor        <- as.numeric(factor)    
  }
  if(method == "arm"){
    result$ir           <- as.numeric(ir)
    result$cr           <- as.numeric(cr)  
  }
  
  class(result)       <- "jfaPrior"
  return(result)
}
