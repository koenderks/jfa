#' Frequentist and Bayesian Planning for Audit Sampling
#'
#' @description This function calculates the required sample size for an audit, based on the Poisson, binomial, or hypergeometric likelihood. A prior can be specified to perform Bayesian planning. The returned object is of class \code{jfaPlanning} and can be used with associated \code{print()} and \code{plot()} methods.
#'
#' For more details on how to use this function see the package vignette:
#' \code{vignette("jfa", package = "jfa")}
#'
#' @usage planning(confidence = 0.95, expectedError = 0, likelihood = "poisson", N = NULL, 
#'           materiality = NULL, minPrecision = NULL, 
#'           prior = FALSE, kPrior = 0, nPrior = 0,
#'           increase = 1, maxSize = 5000)
#'
#' @param confidence    the confidence level desired from the confidence bound (on a scale from 0 to 1). Defaults to 0.95, or 95\% confidence.
#' @param expectedError a fraction representing the percentage of expected mistakes in the sample relative to the total size, or a number (>= 1) that represents the number of expected mistakes.
#' @param likelihood    can be one of \code{binomial}, \code{poisson}, or \code{hypergeometric}.
#' @param N             the population size (required for hypergeometric calculations).
#' @param materiality   a value between 0 and 1 representing the materiality of the audit as a fraction of the total size or value. Can be \code{NULL}, but \code{minPrecision} should be specified in that case.
#' @param minPrecision  The minimum precision to be obtained. Can be \code{NULL}, but \code{materiality} should be specified in that case.
#' @param prior         whether to use a prior distribution when planning. Defaults to \code{FALSE} for frequentist planning. If \code{TRUE}, the prior distribution is updated by the specified likelihood. Chooses a conjugate gamma distribution for the Poisson likelihood, a conjugate beta distribution for the binomial likelihood, and a conjugate beta-binomial distribution for the hypergeometric likelihood.
#' @param kPrior        the prior parameter \eqn{\alpha} (number of errors in the assumed prior sample).
#' @param nPrior        the prior parameter \eqn{\beta} (total number of observations in the assumed prior sample).
#' @param increase      the desired increase step for the sample size calculation.
#' @param maxSize       the maximum sample size that is considered for calculations. Defaults to 5000 for efficiency. Increase this value if the sample size cannot be found due to it being too large (e.g., for a low materiality).
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
#' \item{confidence}{the confidence level for the desired population statement.}
#' \item{expectedError}{the specified number of errors as a fraction or as a number.}
#' \item{likelihood}{the specified likelihood.}
#' \item{N}{the population size (only returned in case of a hypergeometric likelihood).}
#' \item{materiality}{the value of the specified materiality. Can be \code{NULL}.}
#' \item{minPrecision}{The minimum precision to be obtained. Can be \code{NULL}.}
#' \item{sampleSize}{the resulting sample size.}
#' \item{errorType}{whether the expected errors where specified as a percentage or as an integer.}
#' \item{expectedSampleError}{the number of full errors that are allowed to occur in the sample.}
#' \item{expectedBound}{a value specifying the expected upper bound if the sample goes according to plan.}
#' \item{expectedPrecision}{a value specifying the expected precision if the sample goes according to plan.}
#' \item{populationK}{the assumed population errors (only returned in case of a hypergeometric likelihood).}
#' \item{prior}{an object of class \code{jfaPrior} that represents the prior distribution.}
#' \item{expectedPosterior}{an object of class \code{jfaPosterior} that represents the expected posterior distribution.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}} \code{\link{selection}} \code{\link{evaluation}}
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
#' p1 <- planning(confidence = 0.95, expectedError = 0.025, likelihood = "binomial",
#'                materiality = 0.05)
#' print(p1)
#' 
#' # ------------------------------------------------------------
#' #              jfa Planning Summary (Frequentist)
#' # ------------------------------------------------------------     
#' # Input:
#' # 
#' # Confidence:              95% 
#' # Materiality:             5% 
#' # Minimum precision:       Not specified 
#' # Likelihood:              binomial 
#' # Expected sample errors:  6 
#' # ------------------------------------------------------------
#' # Output:
#' #
#' # Sample size:             234 
#' # ------------------------------------------------------------
#' # Statistics:
#' #
#' # Expected upper bound:    5% 
#' # Expected precision:      2.43% 
#' # ------------------------------------------------------------ 
#' 
#' # Bayesian planning with prior:
#' 
#' prior <- auditPrior(confidence = 0.95, likelihood = "binomial", method = "arm", 
#'                     expectedError = 0.025, materiality = 0.05, cr = 0.6)
#' 
#' p3 <- planning(confidence = 0.95, expectedError = 0.025, materiality = 0.05,
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
#' # Minimum precision:       Not specified 
#' # Likelihood:              binomial 
#' # Prior distribution:      beta(2.275, 50.725) 
#' # Expected sample errors:  4.23 
#' # ------------------------------------------------------------
#' # Output:
#' #
#' # Sample size:             169 
#' # Posterior distribution:  beta(6.5, 215.5) 
#' # ------------------------------------------------------------
#' # Statistics:
#' #
#' # Expected upper bound:    4.99% 
#' # Expected precision:      2.49% 
#' # Expected Bayes factor-+: 9.32 
#' # ------------------------------------------------------------ 
#'
#' @keywords planning sample size audit
#'
#' @export

planning <- function(confidence = 0.95, expectedError = 0, likelihood = "poisson", N = NULL, 
						materiality = NULL, minPrecision = NULL, 
						prior = FALSE, kPrior = 0, nPrior = 0,
                     	increase = 1, maxSize = 5000){

	# Import an existing prior distribution with class 'jfaPrior'.
	if(class(prior) == "jfaPrior"){
		if(kPrior != 0 || nPrior != 0)
			warning("When the prior is of class 'jfaPrior', the arguments 'kPrior' and 'nPrior' will not be used.")
		nPrior 		<- prior$description$implicitn
		kPrior 		<- prior$description$implicitk
		likelihood 	<- prior$likelihood
	}

	# Perform error handling with respect to incompatible input options
	if(confidence >= 1 || confidence <= 0 || is.null(confidence))
		stop("Specify a value for the confidence likelihood. Possible values lie within the range of 0 to 1.")

	if(!(likelihood %in% c("poisson", "binomial", "hypergeometric")))
		stop("Specify a valid likelihood. Possible options are 'poisson', 'binomial', and 'hypergeometric'.")
		
	if(is.null(materiality) && is.null(minPrecision))
		stop("Specify the materiality or the minimum precision")

	if(!is.null(minPrecision) && (minPrecision <= 0 || minPrecision >= 1))
		stop("The minimum required precision must be a positive value lower than 1.")

	if((class(prior) == "logical" && prior == TRUE) && kPrior < 0 || nPrior < 0)
		stop("When you specify a prior, both kPrior and nPrior should be > 0.")

	# Define a placeholder for the sample size 
	ss <- NULL

	# Find out the type of expected errors (percentage vs. number)
	if(expectedError >= 0 && expectedError < 1){
		errorType <- "percentage"
		if(!is.null(materiality) && expectedError >= materiality)
			stop("This analysis is not possible: the expected errors are higher than materiality.")
	} else if(expectedError >= 1){
		errorType <- "integer"
		if(expectedError%%1 != 0 && likelihood %in% c("binomial", "hypergeometric") && !((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior"))
			stop("When expectedError > 1 and the likelihood is binomial or hypergeometric, its value must be an integer.")
	}

	# Set the materiality and the minimium precision to 1 if they are NULL
	if(is.null(materiality))
		materiality <- 1
	if(is.null(minPrecision))
		minPrecision <- 1
  
	# Calculate the sample size depending on the probability distribution

	if(likelihood == "hypergeometric"){
		if(is.null(N) || N <= 0)
			stop("The hypergeometric likelihood requires that you specify a population size N.")
		if(materiality == 1)
			stop("The hypergeometric likelihood requires that you specify a materiality.")
		populationK <- ceiling(materiality * N)
	}

	# Define the sampling frame (the possible sample sizes)
	samplingFrame <- seq(from = 0, to = maxSize, by = increase)
	samplingFrame[1] <- 1
	
	# Set up iterations
	iter <- 1
	sufficient <- FALSE

	# Start iterations
	while(!sufficient){

		i <- samplingFrame[iter]

		# Find the expected errors in the sample
		implicitK <- switch(errorType, "percentage" = expectedError * i, "integer" = expectedError)
		if(likelihood == "hypergeometric")
			implicitK <- ceiling(implicitK)

		while(i <= implicitK){ # Remove the number from the sampling frame and take the next one
			samplingFrame <- samplingFrame[-iter]
			i <- samplingFrame[iter]
		}

		if(!is.null(N) && i > N) # The sample size is too large
			stop("The resulting sample size is larger than the population size.")
			
		if((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior"){ # Bayesian planning
			
			bound <- switch(likelihood, 
							"poisson" = stats::qgamma(confidence, shape = 1 + kPrior + implicitK, rate = nPrior + i),
							"binomial" = stats::qbeta(confidence, shape1 = 1 + kPrior + implicitK, shape2 = 1 + nPrior - kPrior + i - implicitK),
							"hypergeometric" = .qBetaBinom(p = confidence, N = N - i + implicitK, shape1 = 1 + kPrior + implicitK, shape2 = 1 + nPrior - kPrior + i - implicitK) / N)
			mle <- switch(likelihood,
							"poisson" = (1 + kPrior + implicitK - 1) / (nPrior + i),
							"binomial" = (1 + kPrior + implicitK - 1) / (1 + kPrior + implicitK + 1 + nPrior - kPrior + i - implicitK - 2),
							"hypergeometric" = .modeBetaBinom(N = N - i + implicitK, shape1 = 1 + kPrior + implicitK, shape2 = 1 + nPrior - kPrior + i - implicitK))
			sufficient <- bound < materiality && (bound - mle) < minPrecision

		} else { # Classical planning

			if(likelihood == "binomial") 
				implicitK <- ceiling(implicitK)
			prob <- switch(likelihood, 
							"poisson" = stats::pgamma(materiality, shape = 1 + implicitK, rate = i),
							"binomial" = stats::dbinom(0:implicitK, size = i, prob = materiality),
							"hypergeometric" = stats::dhyper(x = 0:implicitK, m = populationK, n = ceiling(N - populationK), k = i))
			bound <- switch(likelihood, 
							"poisson" = stats::qgamma(confidence, shape = 1 + implicitK, rate = i),
							"binomial" = stats::binom.test(x = implicitK, n = i, p = materiality, alternative = "less", conf.level = confidence)$conf.int[2],
							"hypergeometric" = stats::qhyper(p = confidence, m = populationK, n = ceiling(N - populationK), k = i) / N)
			mle <- switch(likelihood, 
							"poisson" = implicitK / i,
							"binomial" = implicitK / i,
							"hypergeometric" = floor( ((i + 1) * (populationK + 1)) / (N + 2) ) / N) # = ceiling((((i + 1) * (ceiling(populationK) + 1)) / (N + 2)) - 1) / N
			sufficient <- switch(likelihood, 
							"poisson" = prob >= confidence && (bound - mle) < minPrecision,
							"binomial" = sum(prob) < (1 - confidence) && (bound - mle) < minPrecision,
							"hypergeometric" = sum(prob) < (1 - confidence) && (bound - mle) < minPrecision)
		}

		if(sufficient) # Sufficient work done
			ss <- i
		iter <- iter + 1
	}
  
	# No sample size could be calculated, throw an error
	if(is.null(ss))
		stop("Sample size could not be calculated, you may want to increase the maxSize argument.")

	# Create the main results object
	result <- list()
	result[["confidence"]]				<- as.numeric(confidence)
	result[["expectedError"]]			<- as.numeric(expectedError)
	result[["likelihood"]]   			<- as.character(likelihood)
	result[["N"]]						<- as.numeric(N)
	result[["materiality"]]  			<- as.numeric(materiality)
	result[["minPrecision"]]			<- as.numeric(minPrecision)
	result[["sampleSize"]]          	<- as.numeric(ceiling(ss))
	result[["errorType"]]				<- as.character(errorType)
	result[["expectedSampleError"]]  	<- as.numeric(implicitK)
	result[["expectedBound"]]        	<- as.numeric(bound)
	result[["expectedPrecision"]]    	<- as.numeric(bound - mle)
	if(likelihood == "hypergeometric")
		result[["populationK"]]        	<- as.numeric(populationK)
	# Create the prior distribution object	
	if(((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior")){
		if(class(prior) == "jfaPrior"){
			result[["prior"]] 			<- prior
		} else {
			result[["prior"]]           <- auditPrior(confidence = confidence, 
														likelihood = likelihood, 
														method = "sample", 
														expectedError = 0, 
														N = result[["N"]], 
														materiality = result[["materiality"]], 
														sampleN = nPrior, 
														sampleK = kPrior)
		}
	}
	# Create the expected posterior distribution object
	if(!is.null(result[["prior"]])){
		result[["expectedPosterior"]] <- list()
		# Functional form of the expected posterior
		result[["expectedPosterior"]]$posterior <- switch(likelihood, 
													"poisson" = paste0("gamma(\u03B1 = ", round(result[["prior"]]$description$alpha + result[["expectedSampleError"]], 3), ", \u03B2 = ", round(result[["prior"]]$description$beta + result[["sampleSize"]], 3), ")"),
													"binomial" = paste0("beta(\u03B1 = ", round(result[["prior"]]$description$alpha + result[["expectedSampleError"]], 3), ", \u03B2 = ", round(result[["prior"]]$description$beta + result[["sampleSize"]] - result[["expectedSampleError"]], 3), ")"),
													"hypergeometric" = paste0("beta-binomial(N = ", result[["N"]], ", \u03B1 = ", round(result[["prior"]]$description$alpha + result[["expectedSampleError"]], 3), ", \u03B2 = ", round(result[["prior"]]$description$beta + result[["sampleSize"]] - result[["expectedSampleError"]], 3), ")"))
		# Create the description section
		result[["expectedPosterior"]][["description"]]			<- list()
		result[["expectedPosterior"]][["description"]]$density 	<- switch(likelihood, "poisson" = "gamma", "binomial" = "beta", "hypergeometric" = "beta-binomial")
		result[["expectedPosterior"]][["description"]]$alpha   	<- result[["prior"]]$description$alpha + result[["expectedSampleError"]]
		result[["expectedPosterior"]][["description"]]$beta   	<- switch(likelihood, 
																			"poisson" = result[["prior"]]$description$beta + result[["sampleSize"]], 
																			"binomial" = result[["prior"]]$description$beta + result[["sampleSize"]] - result[["expectedSampleError"]], 
																			"hypergeometric" = result[["prior"]]$description$beta + result[["sampleSize"]] - result[["expectedSampleError"]])
		# Create the statistics section
		result[["expectedPosterior"]][["statistics"]] 			<- list()
		result[["expectedPosterior"]][["statistics"]]$mode 	<- switch(likelihood, 
																		"poisson" = (result[["expectedPosterior"]][["description"]]$alpha - 1) / result[["expectedPosterior"]][["description"]]$beta,
																		"binomial" = (result[["expectedPosterior"]][["description"]]$alpha - 1) / (result[["expectedPosterior"]][["description"]]$alpha + result[["expectedPosterior"]][["description"]]$beta - 2),
																		"hypergeometric" = which.max(.dBetaBinom(x = 0:result[["N"]], N = result[["N"]], shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta)) - 1)
		result[["expectedPosterior"]][["statistics"]]$mean 	<- switch(likelihood, 
																		"poisson" = result[["expectedPosterior"]][["description"]]$alpha / result[["expectedPosterior"]][["description"]]$beta,
																		"binomial" = result[["expectedPosterior"]][["description"]]$alpha / (result[["expectedPosterior"]][["description"]]$alpha + result[["expectedPosterior"]][["description"]]$beta),
																		"hypergeometric" = result[["expectedPosterior"]][["description"]]$alpha / (result[["expectedPosterior"]][["description"]]$alpha + result[["expectedPosterior"]][["description"]]$beta) * result[["N"]])
		result[["expectedPosterior"]][["statistics"]]$median 	<- switch(likelihood, 
																			"poisson" = stats::qgamma(0.5, shape = result[["expectedPosterior"]][["description"]]$alpha, rate = result[["expectedPosterior"]][["description"]]$beta),
																			"binomial" = stats::qbeta(0.5, shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta),
																			"hypergeometric" = .qBetaBinom(0.5, N = result[["N"]], shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta))
		result[["expectedPosterior"]][["statistics"]]$ub 	<- switch(likelihood, 
																		"poisson" = stats::qgamma(confidence, shape = result[["expectedPosterior"]][["description"]]$alpha, rate = result[["expectedPosterior"]][["description"]]$beta),
																		"binomial" = stats::qbeta(confidence, shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta),
																		"hypergeometric" = .qBetaBinom(confidence, N = result[["N"]], shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta))									
		result[["expectedPosterior"]][["statistics"]]$precision <- ifelse(likelihood == "hypergeometric", 
																			yes = (result[["expectedPosterior"]][["statistics"]]$ub - result[["expectedPosterior"]][["statistics"]]$mode) / result[["N"]],
																			no = result[["expectedPosterior"]][["statistics"]]$ub - result[["expectedPosterior"]][["statistics"]]$mode)
		# Create the hypotheses section
		if(result[["materiality"]] != 1){
			result[["expectedPosterior"]][["hypotheses"]] 				<- list()
			result[["expectedPosterior"]][["hypotheses"]]$hypotheses 	<- c(paste0("H-: \u0398 < ", materiality), paste0("H+: \u0398 > ", materiality))
			result[["expectedPosterior"]][["hypotheses"]]$pHmin 		<- switch(likelihood, 
																					"poisson" = stats::pgamma(materiality, shape = result[["expectedPosterior"]][["description"]]$alpha, rate = result[["expectedPosterior"]][["description"]]$beta),
																					"binomial" = stats::pbeta(materiality, shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta),
																					"hypergeometric" = .pBetaBinom(ceiling(materiality * result[["N"]]), N = result[["N"]], shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta))
			result[["expectedPosterior"]][["hypotheses"]]$pHplus 		<- switch(likelihood, 
																					"poisson" = stats::pgamma(materiality, shape = result[["expectedPosterior"]][["description"]]$alpha, rate = result[["expectedPosterior"]][["description"]]$beta, lower.tail = FALSE),
																					"binomial" = stats::pbeta(materiality, shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta, lower.tail = FALSE),
																					"hypergeometric" = 1 - result[["expectedPosterior"]][["hypotheses"]]$pHmin)
			result[["expectedPosterior"]][["hypotheses"]]$oddsHmin 		<- result[["expectedPosterior"]][["hypotheses"]]$pHmin / result[["expectedPosterior"]][["hypotheses"]]$pHplus
			result[["expectedPosterior"]][["hypotheses"]]$oddsHplus 	<- 1 / result[["expectedPosterior"]][["hypotheses"]]$oddsHmin
			result[["expectedPosterior"]][["hypotheses"]]$expectedBf	<- result[["expectedPosterior"]][["hypotheses"]]$oddsHmin / result[["prior"]][["hypotheses"]]$oddsHmin
		}
		result[["expectedPosterior"]][["N"]] <- result[["N"]]
		# Add class 'jfaPosterior' to the expected posterior distribution object.
		class(result[["expectedPosterior"]]) <- "jfaPosterior"
	}
	# Add class 'jfaPlanning' to the result.
	class(result) <- "jfaPlanning"
	return(result)
}