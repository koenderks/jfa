#' Create a prior distribution for audit sampling
#'
#' @description This function creates a prior distribution with audit information to be used in the \code{planning()} and \code{evaluation()} functions via their \code{prior} argument. The function returns an object of class \code{jfaPrior} which can be used with associated \code{print()} and \code{plot()} methods.
#'
#' For more details on how to use this function see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage auditPrior(confidence, likelihood = 'binomial', method = 'none', 
#'            expectedError = 0, N = NULL, materiality = NULL, ir = 1, cr = 1,
#'            pHmin = NULL, pHplus = NULL, factor = 1, sampleN = 0, sampleK = 0)
#' 
#' @param confidence      a numeric value between 0 and 1 specifying the confidence level desired for the sample planning.
#' @param likelihood      a character specifying the likelihood assumed when updating the prior distribution. This can be either \code{binomial} for the binomial likelihood and beta prior distribution, \code{poisson} for the Poisson likelihood and gamma prior distribution, or \code{hypergeometric} for the hypergeometric likelihood and beta-binomial prior distribution. See the details section for more information about the available likelihoods.
#' @param method          a character specifying the method by which the prior distribution is constructed. Defaults to the \code{none} method, which incorporates no existing information. Other options are \code{median}, \code{hypotheses}, \code{arm}, \code{sample} or \code{factor}. See the details section for more information about these methods.
#' @param expectedError   a numeric value between 0 and 1 specifying the expected errors in the sample relative to the total sample size, or a value (>= 1) that represents the sum of expected errors in the sample. It is advised to set this value conservatively to minimize the probability of the observed errors exceeding the expected errors, which would imply that insufficient work has been done in the end.
#' @param N               an numeric value larger than 0 specifying the total population size. Only required when \code{likelihood = 'hypergeometric'}.
#' @param materiality     a numeric value between 0 and 1 specifying the performance materiality (i.e., maximum upper limit) of the audit as a fraction of the total population size. Can be \code{NULL} for some methods.
#' @param ir              if \code{method = 'arm'}, a numeric value between 0 and 1 specifying the inherent risk in the audit risk model. Defaults to 1 for 100\% risk.
#' @param cr              if \code{method = 'arm'}, a numeric value between 0 and 1 specifying the internal control risk in the audit risk model. Defaults to 1 for 100\% risk.
#' @param pHmin           if \code{method = 'hypotheses'}, a numeric value between 0 and 1 specifying the prior probability of the hypothesis \eqn{\theta <} materiality.
#' @param pHplus          if \code{method = 'hypotheses'}, a numeric value between 0 and 1 specifying the prior probability of the hypothesis \eqn{\theta >} materiality.
#' @param factor          if \code{method = 'factor'}, a numeric value between 0 and 1 specifying the weighting factor for the results of the earlier sample.
#' @param sampleN         if \code{method = 'sample'} or \code{method = 'factor'}, an integer larger than, or equal to, 0 specifying the number of sampling units that were inspected in the earlier sample.
#' @param sampleK         if \code{sample} or \code{factor}, a numeric value larger than, or equal to, 0 specifying the sum of errors in the previous sample.
#' 
#' @details This section elaborates on the available likelihoods and corresponding prior distributions for the \code{likelihood} argument.
#' 
#' \itemize{
#'  \item{\code{poisson}:          The Poisson likelihood is often used as a likelihood for monetary unit sampling (MUS). The likelihood function is defined as: \deqn{p(x) = \frac{\lambda^x e^{-\lambda}}{x!}} The conjugate \emph{gamma(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{\beta^\alpha x^{\alpha - 1} e^{-\beta x}}{\Gamma(\alpha)}}}
#'  \item{\code{binomial}:         The binomial likelihood is often used as a likelihood for attributes sampling \emph{with} replacement. The likelihood function is defined as: \deqn{p(x) = {n \choose k} p^k (1 - p)^{n - k}} The conjugate \emph{beta(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{1}{B(\alpha, \beta)} x^{\alpha - 1} (1 - x)^{\beta - 1}}}
#'  \item{\code{hypergeometric}:   The hypergeometric likelihood is used as a likelihood for sampling \emph{without} replacement. The likelihood function is defined as: \deqn{p(x = k) = \frac{{K \choose k} {N - K \choose n - k}}{{N \choose n}}} The conjugate \emph{beta-binomial(\eqn{\alpha, \beta})} prior (Dyer and Pierce, 1993) has probability density function: \deqn{f(k | n, \alpha, \beta) = {n \choose k} \frac{B(k + \alpha, n - k + \beta)}{B(\alpha, \beta)}} }
#' }
#'
#' @details This section elaborates on the available methods for constructing a prior distribution.
#'
#' \itemize{
#'  \item{\code{none}:              This method constructs a prior distribution that incorporates negligible information about the values of the misstatement.}
#'  \item{\code{median}:            This method constructs a prior distribution so that the prior probability of tolerable misstatement (H-) is equal to the prior probability of intolerable misstatement (H+).}
#'  \item{\code{hypotheses}:        This method constructs a prior distribution with custom prior probabilities for the hypotheses of tolerable misstatement (H-) and intolerable misstatement (H+). This method requires specification of the \code{pHmin} and \code{pHplus} arguments.}
#'  \item{\code{arm}:               This method constructs a prior distribution by translating the risks of material misstatement (inherent risk and internal control risk) from the audit risk model to an implicit sample. The method requires specification of the \code{ir} (inherent risk) and \code{cr} (internal control risk) arguments.}
#'  \item{\code{sample}:            This method constructs a prior distribution on the basis of an earlier sample. This method requires specification of the \code{sampleN} and \code{sampleK} arguments.}
#'  \item{\code{factor}:            This method constructs a prior distribution on the basis of an earlier sample in combination with a weighting factor. This method requires specification of the \code{factor}, \code{sampleN} and \code{sampleK} arguments.}
#' }
#'
#' @return An object of class \code{jfaPrior} containing:
#' 
#' \item{confidence}{a numeric value between 0 and 1 indicating the confidence level.}
#' \item{likelihood}{a character indicating the specified likelihood.}
#' \item{method}{a character indicating the method by which the prior distribution is constructed.}
#' \item{expectedError}{a numeric value larger than 0 indicating the input for the number of expected errors.}
#' \item{N}{if \code{N} is specified, an integer larger than 0 indicating the population size.}
#' \item{materiality}{if \code{materiality} is specified, a numeric value between 0 and 1 indicating the materiality used to construct the prior distribution.}
#' \item{description}{a list containing a description of the prior distribution, including the parameters of the prior distribution and the implicit sample on which the prior distribution is based.}
#' \item{statistics}{a list containing statistics of the prior distribution, including the mean, mode, median, and upper bound of the prior distribution.}
#' \item{specifics}{a list containing specifics of the prior distribution that vary depending on the \code{method}.}
#' \item{hypotheses}{if \code{materiality} is specified, a list containing information about the hypotheses, including prior probabilities and odds for the hypothesis of tolerable misstatement (H-) and the hypothesis of intolerable misstatement (H+).}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#' 
#' @seealso \code{\link{planning}} \code{\link{selection}} \code{\link{evaluation}} \code{\link{report}}
#' 
#' @references Derks, K., de Swart, J., Wagenmakers, E.-J., Wille, J., & Wetzels, R. (2019). JASP for audit: Bayesian tools for the auditing practice.
#' @references Derks, K., de Swart, J., van Batenburg, P. Wagenmakers, E.-J., & Wetzels, R. (2020). Priors in a Bayesian audit: How integrating information into the prior distribution can improve audit transparency and efficiency.
#'
#' @keywords prior distribution audit
#'
#' @examples  
#' # Specify inherent risk (ir) and control risk (cr)
#' ir <- 1     # 100%
#' cr <- 0.6   # 60%
#' 
#' # Create the prior distribution
#' auditPrior(confidence = 0.95, likelihood = 'binomial', method = 'arm',
#'            expectedError = 0.025, materiality = 0.05, ir = ir, cr = cr)
#' @export

auditPrior <- function(confidence = 0.95, likelihood = 'binomial', method = 'none', expectedError = 0, 
                       N = NULL, materiality = NULL, ir = 1, cr = 1, pHmin = NULL, pHplus = NULL, 
                       factor = 1, sampleN = 0, sampleK = 0) {
  
  if (confidence >= 1 || confidence <= 0 || is.null(confidence)) # Check if the confidence has a valid input
    stop("Specify a value for the confidence likelihood. Possible values lie within the range of 0 to 1.")
  
  if (!(likelihood %in% c("poisson", "binomial", "hypergeometric"))) # Check if the likelihood has a valid input
    stop("Specify a valid likelihood. Possible options are 'poisson', 'binomial', and 'hypergeometric'.")
  
  if (!(method %in% c("none", "median", "hypotheses", "arm", "sample", "factor"))) # Check if the method has a valid input
    stop("Currently only method = 'none', 'median', 'hypotheses', 'arm', 'sample', and 'factor' are supported")
  
  if (is.null(materiality) && method %in% c("median", "hypotheses", "arm")) # Materiality is required for these methods
    stop("The methods 'arm', 'median', and 'hypotheses' require that you specify a value for the materiality.")
  
  if (likelihood == "hypergeometric" && (is.null(N) || N <= 0)) # Check if N is specified if hypergeometric is specified
    stop("The hypergeometric likelihood requires that you specify a positive value for the populatin size N.")
  
  if (expectedError < 0) # Check if the expected errors has a valid input
    stop("The expected errors must be zero or larger than zero.")
  
  if (expectedError >= 1 && method != "none") # Check if the expected errors are consistent with the method
    stop("The expected errors must be entered as a proportion to use this prior construction method.")
  
  if (method == "none") { # Method 1: Negligible prior information
    nPrior <- 0 # No earlier observations
    kPrior <- 0 # No earlier errors
  } else if (method == "arm") { # Method 2: Translate the risks from the audit risk model
    if (is.null(ir) || is.null(cr) || is.null(materiality))
      stop("Method = 'arm' requires non-null 'materiality', 'ir', and 'cr' arguments.")
    alpha 	<- (1 - confidence) / (ir * cr) # Calculate the required detection risk from the audit risk model
    nPlus 	<- planning(confidence = confidence, likelihood = likelihood, expectedError = expectedError, N = N, materiality = materiality, prior = TRUE)$sampleSize # Calculate the sample size for the full detection risk  
    nMin 	<- planning(confidence = 1 - alpha, likelihood = likelihood, expectedError = expectedError, N = N, materiality = materiality, prior = TRUE)$sampleSize # Calculated the sample size for the adjusted detection risk
    nPrior 	<- nPlus - nMin # Calculate the sample size equivalent to the increase in detection risk
    kPrior 	<- (nPlus * expectedError) - (nMin * expectedError) # Calculate errors equivalent 
  } else if (method == "median") { # Method 3: Equal prior probabilities
    if (likelihood == "hypergeometric") # Cannot use this method with the hypergeometric likelihood
      stop("Method = 'median' is not supported for the hypergeometric likelihood.")
    probH1 <- probH0 <- 0.5 # Set equal prior probabilities
    if (expectedError == 0) { # Formulas for zero expected errors
      nPrior <- switch(likelihood, "poisson" = -(log(probH1) / materiality), "binomial" = log(probH1) / log(1 - materiality) - 1)
      kPrior <- 0
    } else { # Formulas for expected errors > 0
      if (likelihood == "binomial") { # Approximation through closed formulas
        alpha <- (-4 * materiality * expectedError + 3 * materiality - expectedError) / (3 * (materiality - expectedError))
        beta <- (4 * materiality * expectedError - materiality - 5 * expectedError + 2) / (3 * (materiality - expectedError))
        nPrior <- beta + (alpha - 1) - 1 # Earlier sample
        kPrior <- alpha - 1 # Earlier errors
      } else if (likelihood == "poisson") { # Approximation through iteration
        for (alpha in seq(1, 5, 0.001)) { # Iterate over alpha
          beta <- (alpha - 1) / expectedError # Express beta in terms of alpha
          median <- stats::qgamma(p = 0.5, shape = alpha, rate = beta) # Calculate the median for the current parameters
          mode <- (alpha - 1) / beta # Calculate the mode for the current parameters
          if (round(median, 3) == materiality && round(mode, 3) == expectedError) {
            break # Return if these match
          }
        }
        nPrior <- beta # Earlier sample
        kPrior <- alpha - 1 # Earlier errors
      }
    }
  } else if (method == "hypotheses") { # Method 4: Custom prior probability
    if (likelihood == "hypergeometric") # Cannot use this method with the hypergeometric likelihood
      stop("Method = 'hypotheses' is not supported for the hypergeometric likelihood.")
    if ((is.null(pHplus) && is.null(pHmin)) || is.null(materiality)) # Must have the prior probabilities and materiality
      stop("Method = 'hypotheses' requires non-null 'materiality' and 'pHplus' or 'pHplus' arguments.")
    if ((!is.null(pHplus) && !is.null(pHmin)) && pHplus + pHmin != 1) # Check for valid prior probabilities
      stop("The values for 'pHplus' and 'pHmin' should sum to one.")  
    if (expectedError != 0) # Cannot use this method with expected errors
      stop("Expected errors are not supported for method = 'hypotheses'.")
    if (is.null(pHplus) && !is.null(pHmin)) { # Adjust p(H1) for user input
      probH1 <- 1 - pHmin
    } else {
      probH1 <- pHplus
    }
    probH0 <- 1 - probH1 # Calculate p(H0)
    nPrior <- switch(likelihood, "poisson" = -(log(probH1) / materiality), "binomial" = log(probH1) / log(1 - materiality) - 1) # Earlier sample size
    kPrior <- 0 # Earlier errors
  } else if (method == "sample") { # Method 5: Earlier sample
    if (is.null(sampleN) || is.null(sampleK)) # Check for valid arguments
      stop("Method = 'sample' requires non-null 'sampleN', and 'sampleK' arguments.")
    nPrior <- sampleN # Earlier sample size
    kPrior <- sampleK # Earlier errors
  } else if (method == "factor") {
    if (is.null(sampleN) || is.null(sampleK) || is.null(factor)) # Check for valid arguments
      stop("Method = 'factor' requires non-null 'factor', 'sampleN', and 'sampleN=K' arguments.")  
    nPrior <- sampleN * factor # Earlier sample size
    kPrior <- sampleK * factor # Earlier errors
  }
  
  # Create the main result object	
  result <- list()
  result[["confidence"]]   	<- as.numeric(confidence)
  result[["likelihood"]]   	<- as.character(likelihood)
  result[["method"]]       	<- as.character(method)
  result[["expectedError"]] <- as.numeric(expectedError)
  result[["N"]]            	<- as.numeric(N)
  if (!is.null(materiality))
    result[["materiality"]]  <- as.numeric(materiality)
  # Create the description section
  result[["description"]]			<- list()
  result[["description"]]$density  	<- switch(likelihood, "poisson" = "gamma", "binomial" = "beta", "hypergeometric" = "beta-binomial")
  result[["description"]]$implicitk <- as.numeric(kPrior)
  result[["description"]]$implicitn	<- as.numeric(nPrior)
  result[["description"]]$alpha   	<- as.numeric(1 + kPrior)
  result[["description"]]$beta   	<- switch(likelihood, "poisson" = nPrior, "binomial" = 1 + nPrior - kPrior, "hypergeometric" = 1 + nPrior - kPrior)
  # Create the statistics section
  result[["statistics"]] 			<- list()
  result[["statistics"]]$mode 		<- switch(likelihood, 
                                          "poisson" = (result[["description"]]$alpha - 1) / result[["description"]]$beta,
                                          "binomial" = (result[["description"]]$alpha - 1) / (result[["description"]]$alpha + result[["description"]]$beta - 2),
                                          "hypergeometric" = .modeBetaBinom(N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta))
  result[["statistics"]]$mean 		<- switch(likelihood, 
                                          "poisson" = result[["description"]]$alpha / result[["description"]]$beta,
                                          "binomial" = result[["description"]]$alpha / (result[["description"]]$alpha + result[["description"]]$beta),
                                          "hypergeometric" = result[["description"]]$alpha / (result[["description"]]$alpha + result[["description"]]$beta) * result[["N"]])
  result[["statistics"]]$median 	<- switch(likelihood, 
                                           "poisson" = stats::qgamma(0.5, shape = result[["description"]]$alpha, rate = result[["description"]]$beta),
                                           "binomial" = stats::qbeta(0.5, shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta),
                                           "hypergeometric" = .qBetaBinom(0.5, N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta))
  result[["statistics"]]$ub 		<- switch(likelihood, 
                                        "poisson" = stats::qgamma(confidence, shape = result[["description"]]$alpha, rate = result[["description"]]$beta),
                                        "binomial" = stats::qbeta(confidence, shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta),
                                        "hypergeometric" = .qBetaBinom(confidence, N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta))									
  result[["statistics"]]$precision 	<- result[["statistics"]]$ub - result[["statistics"]]$mode
  # Create the specifics section
  if (method != "none")
    result[["specifics"]] 			<- list()
  if (method == "median" || method == "hypotheses") {
    result[["specifics"]]$pHmin   	<- as.numeric(probH0)
    result[["specifics"]]$pHplus  	<- as.numeric(probH1)
  } else if (method == "sample" || method == "factor") {
    result[["specifics"]]$sampleN   <- as.numeric(sampleN)
    result[["specifics"]]$sampleK   <- as.numeric(sampleK)
    result[["specifics"]]$factor   	<- as.numeric(factor)    
  } else if (method == "arm") {
    result[["specifics"]]$ir       	<- as.numeric(ir)
    result[["specifics"]]$cr       	<- as.numeric(cr)  
  }
  # Create the hypotheses section
  if (!is.null(result[["materiality"]])) {
    result[["hypotheses"]] 				<- list()
    result[["hypotheses"]]$hypotheses 	<- c(paste0("H-: \u0398 < ", materiality), paste0("H+: \u0398 > ", materiality))
    result[["hypotheses"]]$pHmin 		<- switch(likelihood, 
                                             "poisson" = stats::pgamma(materiality, shape = result[["description"]]$alpha, rate = result[["description"]]$beta),
                                             "binomial" = stats::pbeta(materiality, shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta),
                                             "hypergeometric" = .pBetaBinom(ceiling(materiality * result[["N"]]), N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta))
    result[["hypotheses"]]$pHplus 		<- switch(likelihood, 
                                              "poisson" = stats::pgamma(materiality, shape = result[["description"]]$alpha, rate = result[["description"]]$beta, lower.tail = FALSE),
                                              "binomial" = stats::pbeta(materiality, shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta, lower.tail = FALSE),
                                              "hypergeometric" = .pBetaBinom(ceiling(materiality * result[["N"]]), N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta, lower.tail = FALSE))
    result[["hypotheses"]]$oddsHmin 	<- result[["hypotheses"]]$pHmin / result[["hypotheses"]]$pHplus
    result[["hypotheses"]]$oddsHplus 	<- 1 / result[["hypotheses"]]$oddsHmin
  }
  # Functional form of the prior distribution
  result[["prior"]] <- switch(likelihood, 
                              "poisson" = paste0("gamma(\u03B1 = ", round(result[["description"]]$alpha, 3), ", \u03B2 = ", round(result[["description"]]$beta, 3), ")"),
                              "binomial" = paste0("beta(\u03B1 = ", round(result[["description"]]$alpha, 3), ", \u03B2 = ", round(result[["description"]]$beta, 3), ")"),
                              "hypergeometric" = paste0("beta-binomial(N = ", result[["N"]], ", \u03B1 = ", round(result[["description"]]$alpha, 3), ", \u03B2 = ", round(result[["description"]]$beta, 3), ")"))	
  # Add class 'jfaPrior' to the result
  class(result) <- "jfaPrior"
  return(result)
}
