#' Create a Prior Distribution with Existing Audit Information
#'
#' @description This function creates a prior distribution for Bayesian audit sampling according to several methods discussed in Derks et al. (2020). The returned object is of class \code{jfaPrior} and can be used with associated \code{print()} and \code{plot()} methods. \code{jfaPrior} objects can be used as input for the \code{prior} argument in other functions.
#'
#' For more details on how to use this function see the package vignette:
#' \code{vignette("jfa", package = "jfa")}
#'
#' @usage auditPrior(confidence = 0.95, likelihood = "binomial", method = "none", 
#'            expectedError = 0, N = NULL, materiality = NULL, ir = 1, cr = 1,
#'            pHmin = NULL, pHplus = NULL, factor = 1, sampleN = 0, sampleK = 0)
#' 
#' @param confidence      the confidence level desired from the confidence bound (on a scale from 0 to 1). Defaults to 0.95, or 95\% confidence.
#' @param likelihood      can be one of \code{binomial}, \code{poisson}, or \code{hypergeometric}. See the Details section for more information.
#' @param method          the method by which the prior distribution is constructed. Defaults to the \code{none} method, which incorporates no prior information. Can be one of \code{none}, \code{median}, \code{hypotheses}, \code{arm}, \code{sample} or \code{factor}. See the Details section for more information.
#' @param expectedError   a fraction representing the percentage of expected mistakes in the sample relative to the total size, or a number (>= 1) that represents the number of expected mistakes.
#' @param N               the population size (only required when \code{likelihood = 'hypergeometric'}).
#' @param materiality     a value between 0 and 1 representing the materiality of the audit as a fraction of the total size or value. Can be \code{NULL} for some methods.
#' @param ir              the inherent risk probability from the audit risk model. Defaults to 1 for 100\% risk.
#' @param cr              the inherent risk probability from the audit risk model. Defaults to 1 for 100\% risk.
#' @param pHmin           When using \code{method = 'hypotheses'}, the prior probability of the hypothesis \eqn{\theta <} materiality.
#' @param pHplus          When using \code{method = 'hypotheses'}, the prior probability of the hypothesis \eqn{\theta >} materiality.
#' @param factor          When using \code{method = 'factor'}, the value of the weighting factor for the results of the previous sample.
#' @param sampleN         When using method \code{sample} or \code{factor}, the number of transactions that were inspected in the previous sample.
#' @param sampleK         When using method \code{sample} or \code{factor}, the total taint in the previous sample.
#' 
#' @details This section elaborates on the available likelihoods and corresponding prior distributions for the \code{likelihood} argument.
#' 
#' \itemize{
#'  \item{\code{poisson}:          The Poisson likelihood is used as a likelihood for monetary unit sampling (MUS). Its likelihood function is defined as: \deqn{p(x) = \frac{\lambda^x e^{-\lambda}}{x!}} The conjugate \emph{gamma(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{\beta^\alpha x^{\alpha - 1} e^{-\beta x}}{\Gamma(\alpha)}}}
#'  \item{\code{binomial}:         The binomial likelihood is used as a likelihood for record sampling \emph{with} replacement. Its likelihood function is defined as: \deqn{p(x) = {n \choose k} p^k (1 - p)^{n - k}} The conjugate \emph{beta(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{1}{B(\alpha, \beta)} x^{\alpha - 1} (1 - x)^{\beta - 1}}}
#'  \item{\code{hypergeometric}:   The hypergeometric likelihood is used as a likelihood for record sampling \emph{without} replacement. Its likelihood function is defined as: \deqn{p(x = k) = \frac{{K \choose k} {N - K \choose n - k}}{{N \choose n}}} The conjugate \emph{beta-binomial(\eqn{\alpha, \beta})} prior (Dyer and Pierce, 1993) has probability density function: \deqn{f(k | n, \alpha, \beta) = {n \choose k} \frac{B(k + \alpha, n - k + \beta)}{B(\alpha, \beta)}} }
#' }
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
#' @return An object of class \code{jfaPrior} containing:
#' 
#' \item{confidence}{the method by which the prior distribution is constructed.}
#' \item{likelihood}{the likelihood by which the prior distribution is updated.}
#' \item{method}{the method by which the prior distribution is constructed.}
#' \item{expectedError}{the expected error input.}
#' \item{N}{if specified as input, the population size.}
#' \item{materiality}{if specified, the materiality that was used to construct the prior distribution.}
#' \item{description}{a description of the prior distribution, including parameters and the implicit sample.}
#' \item{statistics}{a list of statistics of the prior distribution, including the mean, mode, median, and upper bound.}
#' \item{specifics}{a list of optional specifications of the prior distribution, these depend on the method used.}
#' \item{hypotheses}{if a materiality is specified, a list of statistics about the hypotheses including prior probabilities and odds.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#' 
#' @seealso \code{\link{planning}} \code{\link{selection}} \code{\link{evaluation}}
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
#' prior <- auditPrior(confidence = confidence, likelihood = "binomial", 
#'                     method = "arm", expectedError = expectedError, materiality = materiality, 
#'                     ir = ir, cr = cr)
#' print(prior)
#' 
#' # ------------------------------------------------------------
#' #         jfa Prior Distribution Summary (Bayesian)
#' # ------------------------------------------------------------
#' # Input:
#' #
#' # Confidence:              0.95    
#' # Expected sample errors:  0.02       
#' # Likelihood:              binomial 
#' # Specifics:               Inherent risk = 1; Internal control risk = 0.6; Detection risk = 0.08 
#' # ------------------------------------------------------------
#' # Output: 
#' #
#' # Prior distribution:      beta(2.275, 50.725) 
#' # Implicit sample size:    51 
#' # Implicit errors:         1.27 
#' # ------------------------------------------------------------
#' # Statistics: 
#' #
#' # Upper bound:             0.1 
#' # Precision:               7.1% 
#' # Mode:                    0.02 
#' # Mean:                    0.04 
#' # Median:                  0.04 
#' # ------------------------------------------------------------
#' @export

auditPrior <- function(confidence = 0.95, likelihood = "binomial", method = "none", expectedError = 0, 
						N = NULL, materiality = NULL, ir = 1, cr = 1, pHmin = NULL, pHplus = NULL, 
						factor = 1, sampleN = 0, sampleK = 0){

	# Perform error handling with respect to incompatible input options
	if(confidence >= 1 || confidence <= 0 || is.null(confidence))
		stop("Specify a value for the confidence likelihood. Possible values lie within the range of 0 to 1.")
	
	if(!(likelihood %in% c("poisson", "binomial", "hypergeometric")))
		stop("Specify a valid likelihood. Possible options are 'poisson', 'binomial', and 'hypergeometric'.")
	
	if(!(method %in% c("none", "median", "hypotheses", "arm", "sample", "factor")))
		stop("Currently only method = 'none', 'median', 'hypotheses', 'arm', 'sample', and 'factor' are supported")
		
	if(is.null(materiality) && method %in% c("median", "hypotheses", "arm"))
		stop("The methods 'arm', 'median', and 'hypotheses' require that you specify a value for the materiality.")

	if(likelihood == "hypergeometric" && (is.null(N) || N <= 0))
		stop("The hypergeometric likelihood requires that you specify a positive value for the populatin size N.")

	if(expectedError < 0)
		stop("The expected errors must be zero or larger than zero.")

	if(expectedError >= 1 && method != "none")
		stop("The expected errors must be entered as a proportion to use this prior construction method.")

	# Create the prior distribution depending on the specified method
	if(method == "none"){
		nPrior <- 0
		kPrior <- 0
	} else if(method == "arm"){
		if(is.null(ir) || is.null(cr) || is.null(materiality))
			stop("Method = 'arm' requires non-null 'materiality', 'ir', and 'cr' arguments.")
		nPlus <- planning(confidence = confidence, likelihood = likelihood, expectedError = expectedError, N = N, materiality = materiality, prior = TRUE)$sampleSize
		alpha <- (1 - confidence) / (ir * cr)
		nMin <- planning(confidence = 1 - alpha, likelihood = likelihood, expectedError = expectedError, N = N, materiality = materiality, prior = TRUE)$sampleSize
		kPlus <- nPlus * expectedError
		kMin <-  nMin * expectedError
		nPrior <- nPlus - nMin
		kPrior <- kPlus - kMin
	} else if(method == "median"){
		if(likelihood == "hypergeometric")
			stop("Method = 'median' is not supported for the hypergeometric likelihood.")
		probH1 <- probH0 <- 0.5
		if(expectedError == 0){
			nPrior <- switch(likelihood, "poisson" = -(log(probH1) / materiality), "binomial" = log(probH1) / log(1 - materiality) - 1)
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
		} else {
			probH1 <- pHplus
		}
		probH0 <- 1 - probH1
		if(expectedError != 0)
			stop("Expected errors are not supported for method = 'hypotheses'.")
		nPrior <- switch(likelihood, "poisson" = -(log(probH1) / materiality), "binomial" = log(probH1) / log(1 - materiality) - 1)
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

	# Create the main results object	
	result <- list()
	result[["confidence"]]   	<- as.numeric(confidence)
	result[["likelihood"]]   	<- as.character(likelihood)
	result[["method"]]       	<- as.character(method)
	result[["expectedError"]] 	<- as.numeric(expectedError)
	result[["N"]]            	<- as.numeric(N)
	if(!is.null(materiality))
		result[["materiality"]]  <- as.numeric(materiality)
	# Create the description section
	result[["description"]]				<- list()
	result[["description"]]$density  	<- switch(likelihood, "poisson" = "gamma", "binomial" = "beta", "hypergeometric" = "beta-binomial")
	result[["description"]]$implicitk  	<- as.numeric(kPrior)
	result[["description"]]$implicitn	<- as.numeric(nPrior)
	result[["description"]]$alpha   	<- as.numeric(1 + kPrior)
	result[["description"]]$beta   		<- switch(likelihood, "poisson" = nPrior, "binomial" = 1 + nPrior - kPrior, "hypergeometric" = 1 + nPrior - kPrior)
	# Create the statistics section
	result[["statistics"]] 				<- list()
	result[["statistics"]]$mode 		<- switch(likelihood, 
													"poisson" = (result[["description"]]$alpha - 1) / result[["description"]]$beta,
													"binomial" = (result[["description"]]$alpha - 1) / (result[["description"]]$alpha + result[["description"]]$beta - 2),
													"hypergeometric" = which.max(.dBetaBinom(x = 0:result[["N"]], N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta)) - 1)
	result[["statistics"]]$mean 		<- switch(likelihood, 
													"poisson" = result[["description"]]$alpha / result[["description"]]$beta,
													"binomial" = result[["description"]]$alpha / (result[["description"]]$alpha + result[["description"]]$beta),
													"hypergeometric" = result[["description"]]$alpha / (result[["description"]]$alpha + result[["description"]]$beta) * result[["N"]])
	result[["statistics"]]$median 		<- switch(likelihood, 
													"poisson" = stats::qgamma(0.5, shape = result[["description"]]$alpha, rate = result[["description"]]$beta),
													"binomial" = stats::qbeta(0.5, shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta),
													"hypergeometric" = .qBetaBinom(0.5, N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta))
	result[["statistics"]]$ub 			<- switch(likelihood, 
													"poisson" = stats::qgamma(confidence, shape = result[["description"]]$alpha, rate = result[["description"]]$beta),
													"binomial" = stats::qbeta(confidence, shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta),
													"hypergeometric" = .qBetaBinom(confidence, N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta))									
	result[["statistics"]]$precision 	<- ifelse(likelihood == "hypergeometric", 
													yes = (result[["statistics"]]$ub - result[["statistics"]]$mode) / result[["N"]],
													no = result[["statistics"]]$ub - result[["statistics"]]$mode)
	# Create the specifics section
	if(method != "none")
		result[["specifics"]] 			<- list()
	if(method == "median" || method == "hypotheses"){
		result[["specifics"]]$pHmin   	<- as.numeric(probH0)
		result[["specifics"]]$pHplus  	<- as.numeric(probH1)
	} else if(method == "sample" || method == "factor"){
		result[["specifics"]]$sampleN   <- as.numeric(sampleN)
		result[["specifics"]]$sampleK   <- as.numeric(sampleK)
		result[["specifics"]]$factor   	<- as.numeric(factor)    
	} else if(method == "arm"){
		result[["specifics"]]$ir       	<- as.numeric(ir)
		result[["specifics"]]$cr       	<- as.numeric(cr)  
	}
	# Create the hypotheses section
	if(!is.null(result$materiality)){
		result[["hypotheses"]] 				<- list()
		result[["hypotheses"]]$hypotheses 	<- c(paste0("H-: \u0398 < ", materiality), paste0("H+: \u0398 > ", materiality))
		result[["hypotheses"]]$pHmin 		<- switch(likelihood, 
														"poisson" = stats::pgamma(materiality, shape = result[["description"]]$alpha, rate = result[["description"]]$beta),
														"binomial" = stats::pbeta(materiality, shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta),
														"hypergeometric" = .pBetaBinom(ceiling(materiality * result[["N"]]), N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta))
		result[["hypotheses"]]$pHplus 		<- switch(likelihood, 
														"poisson" = stats::pgamma(materiality, shape = result[["description"]]$alpha, rate = result[["description"]]$beta, lower.tail = FALSE),
														"binomial" = stats::pbeta(materiality, shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta, lower.tail = FALSE),
														"hypergeometric" = 1 - result[["hypotheses"]]$pHmin)
		result[["hypotheses"]]$oddsHmin 	<- result[["hypotheses"]]$pHmin / result[["hypotheses"]]$pHplus
		result[["hypotheses"]]$oddsHplus 	<- 1 / result[["hypotheses"]]$oddsHmin
	}
	# Functional form of the prior distribution.
	result[["prior"]] <- switch(likelihood, 
								"poisson" = paste0("gamma(\u03B1 = ", round(result[["description"]]$alpha, 3), ", \u03B2 = ", round(result[["description"]]$beta, 3), ")"),
								"binomial" = paste0("beta(\u03B1 = ", round(result[["description"]]$alpha, 3), ", \u03B2 = ", round(result[["description"]]$beta, 3), ")"),
								"hypergeometric" = paste0("beta-binomial(N = ", result[["N"]], ", \u03B1 = ", round(result[["description"]]$alpha, 3), ", \u03B2 = ", round(result[["description"]]$beta, 3), ")"))	
	# Add class 'jfaPrior' to the result.
	class(result)       <- "jfaPrior"
	return(result)
}
