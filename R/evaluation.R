#' Evaluation of Audit Samples using Confidence / Credible Bounds
#'
#' @description This function takes a data frame (using \code{sample}, \code{bookValue}, and \code{auditValues}) or summary statistics (using \code{nSumstats} and \code{kSumstats}) and evaluates the audit sample according to the specified method. The returned object is of class \code{jfaEvaluation} and can be used with associated \code{print()} and \code{plot()} methods.
#'
#' @usage evaluation(confidence = 0.95, method = "binomial", N = NULL,
#'             sample = NULL, bookValues = NULL, auditValues = NULL, counts = NULL, 
#'             nSumstats = NULL, kSumstats = NULL, 
#'             materiality = NULL, minPrecision = NULL,
#'             prior = FALSE, nPrior = 0, kPrior = 0, 
#'             rohrbachDelta = 2.7, momentPoptype = "accounts", populationBookValue = NULL,
#'             csA = 1, csB = 3, csMu = 0.5) 
#'
#' @param confidence    the required confidence level for the bound. Default is 0.95 for 95\% confidence.
#' @param method        the method that is used to evaluate the sample. This can be either one of \code{poisson}, \code{binomial}, \code{hypergeometric}, \code{stringer}, \code{stringer-meikle}, \code{stringer-lta}, \code{stringer-pvz}, \code{rohrbach}, \code{moment}, \code{direct}, \code{difference}, \code{quotient}, or \code{regression}. 
#' @param N             an integer specifying the total number of units (transactions or monetary units) in the population.
#' @param sample        a data frame containing at least a column of Ist values and a column of Soll (true) values.
#' @param bookValues    a character specifying the column name for the Ist values in the sample.
#' @param auditValues   a character specifying the column name for the Soll values in the sample.
#' @param counts        a integer vector of the number of times each transaction in the sample is to be evaluated (due to it being selected multiple times for the sample).
#' @param nSumstats     an integer specifying the number of transactions in the sample. If specified, overrides the \code{sample}, \code{bookValues} and \code{auditValues} arguments and assumes that the data come from summary statistics specified by both \code{nSumstats} and \code{kSumstats}.
#' @param kSumstats     a value specifying the sum of taints (proportional errors) found in the sample. If specified, overrides the \code{sample}, \code{bookValues} and \code{auditValues} arguments and assumes that the data come from summary statistics specified by both \code{kSumstats} and \code{nSumstats}.
#' @param materiality   a value specifying the performance materiality as a fraction of the total value (or size) of the population (a value between 0 and 1). If specified, the function also returns the conclusion of the analysis with respect to the performance materiality. The value is discarded when \code{direct}, \code{difference}, \code{quotient}, or \code{regression} method is chosen.
#' @param minPrecision  a value specifying the required minimum precision. If specified, the function also returns the conclusion of the analysis with respect to the required minimum precision. This value must be specified as a fraction of the total value of the population (a value between 0 and 1).
#' @param prior         a logical indicating whether to use a prior distribution when evaluating. Defaults to \code{FALSE} for frequentist evaluation. If \code{TRUE}, the prior distribution is updated by the corresponding likelihood. Chooses a conjugate gamma distribution for the Poisson likelihood, a conjugate beta distribution for the binomial likelihood, and a conjugate beta-binomial distribution for the hypergeometric likelihood.
#' @param nPrior        a value for the prior parameter \eqn{\beta} (number of transactions in the assumed prior sample).
#' @param kPrior        a value for the prior parameter \eqn{\alpha} (total tainting in the assumed prior sample).
#' @param rohrbachDelta a value specifying \eqn{\Delta} in Rohrbach's augmented variance bound (Rohrbach, 1993).
#' @param momentPoptype a character specifying the type of population for the modified moment method (Dworin and Grimlund, 1986). Can be either one of \code{accounts} or \code{inventory}. Options result in different methods for calculating the central moments.
#' @param populationBookValue a value specifying the total value of the transactions in the population. Required when \code{method} is one of \code{direct}, \code{difference}, \code{quotient}, or \code{regression}, but optional otherwise.
#' @param csA           if \code{method = "coxsnell"}, the \eqn{\alpha} parameter of the prior distribution on the mean taint. Default is set to 1, as recommended by Cox and Snell (1979).
#' @param csB           if \code{method = "coxsnell"}, the \eqn{\beta} parameter of the prior distribution on the mean taint. Default is set to 3, as recommended by Cox and Snell (1979).
#' @param csMu          if \code{method = "coxsnell"}, the mean of the prior distribution on the mean taint. Default is set to 0.5, as recommended by Cox and Snell (1979).
#'
#' @details This section lists the available options for the \code{methods} argument.
#' 
#' \itemize{
#'  \item{\code{poisson}:          The confidence bound taken from the Poisson distribution. If combined with \code{prior = TRUE}, performs Bayesian evaluation using a \emph{gamma} prior and posterior.}
#'  \item{\code{binomial}:         The confidence bound taken from the binomial distribution. If combined with \code{prior = TRUE}, performs Bayesian evaluation using a \emph{beta} prior and posterior.}
#'  \item{\code{hypergeometric}:   The confidence bound taken from the hypergeometric distribution. If combined with \code{prior = TRUE}, performs Bayesian evaluation using a \emph{beta-binomial} prior and posterior.}
#'  \item{\code{stringer}:         The Stringer bound (Stringer, 1963).}
#'  \item{\code{stringer-meikle}:  Stringer bound with Meikle's correction for understatements (Meikle, 1972).}
#'  \item{\code{stringer-lta}:     Stringer bound with LTA correction for understatements (Leslie, Teitlebaum, and Anderson, 1979).}
#'  \item{\code{stringer-pvz}:     Stringer bound with Pap and van Zuijlen's correction for understatements (Pap and van Zuijlen, 1996).}
#'  \item{\code{rohrbach}:         Rohrbach's augmented variance bound (Rohrbach, 1993).}
#'  \item{\code{moment}:           Modified moment bound (Dworin and Grimlund, 1986).}
#'  \item{\code{coxsnell}:         Cox and Snell bound (Cox and Snell, 1979).}
#'  \item{\code{direct}:           Confidence interval using the direct method (Touw and Hoogduin, 2011).}
#'  \item{\code{difference}:       Confidence interval using the difference method (Touw and Hoogduin, 2011).}
#'  \item{\code{quotient}:         Confidence interval using the quotient method (Touw and Hoogduin, 2011).}
#'  \item{\code{regression}:       Confidence interval using the regression method (Touw and Hoogduin, 2011).}
#' }
#' 
#' @references Cox, D. and Snell, E. (1979). On sampling and the estimation of rare errors. \emph{Biometrika}, 66(1), 125-132. 
#' @references Dworin, L., and Grimlund, R. A. (1986). Dollar-unit sampling: A comparison of the quasi-Bayesian and moment bounds. \emph{Accounting Review}, 36-57.
#' @references Leslie, D. A., Teitlebaum, A. D., & Anderson, R. J. (1979). \emph{Dollar-unit sampling: a practical guide for auditors}. Copp Clark Pitman; Belmont, Calif.: distributed by Fearon-Pitman.
#' @references Meikle, G. R. (1972). \emph{Statistical Sampling in an Audit Context: An Audit Technique}. Canadian Institute of Chartered Accountants.
#' @references Pap, G., and van Zuijlen, M. C. (1996). On the asymptotic behavior of the Stringer bound 1. \emph{Statistica Neerlandica}, 50(3), 367-389.
#' @references Rohrbach, K. J. (1993). Variance augmentation to achieve nominal coverage probability in sampling from audit populations. \emph{Auditing}, 12(2), 79.
#' @references Stringer, K. W. (1963). Practical aspects of statistical sampling in auditing. \emph{In Proceedings of the Business and Economic Statistics Section} (pp. 405-411). American Statistical Association.
#' @references Touw, P., and Hoogduin, L. (2011). \emph{Statistiek voor Audit en Controlling}. Boom uitgevers Amsterdam.
#'
#' @return An object of class \code{jfaEvaluation} containing:
#' 
#' \item{confidence}{a value specifying the confidence level of the result.}
#' \item{method}{the evaluation method that was used.}
#' \item{N}{if \code{N} is specified, the population size that is used.}
#' \item{n}{an integer specifying the sample size used in the evaluation.}
#' \item{k}{an integer specifying the number of transactions that contained an error.}
#' \item{t}{a value specifying the sum of observed taints.}
#' \item{materiality}{if \code{materiality} is specified, the performance materiality used.}
#' \item{minPrecision}{if \code{minPrecision} is specified, the minimum required precision used.}
#' \item{mle}{a value specifying the most likely error in the population as a proportion.}
#' \item{precision}{a value specifying the difference between the mle and the upper confidence bound as a proportion.}
#' \item{popBookvalue}{if specified as input, the total Ist value of the population.}
#' \item{pointEstimate}{if method is one of \code{direct}, \code{difference}, \code{quotient}, or \code{regression}, the value of the point estimate.}
#' \item{lowerBound}{if method is one of \code{direct}, \code{difference}, \code{quotient}, or \code{regression}, the value of the lower bound of the interval.}
#' \item{upperBound}{if method is one of \code{direct}, \code{difference}, \code{quotient}, or \code{regression}, the value of the upper bound of the interval.}
#' \item{confBound}{the upper confidence bound on the error percentage. }
#' \item{conclusion}{if \code{materiality} is specified, the conclusion about whether to approve or not approve the population.}
#' \item{populationK}{the assumed total errors in the population. Used in inferences with \code{hypergeometric} method.}
#' \item{prior}{an ofject of class 'jfaPrior' to represents the prior distribution.}
#' \item{posterior}{an ofject of class 'jfaPosterior' to represents the posterior distribution.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}} \code{\link{planning}} \code{\link{sampling}}
#'
#' @examples
#' library(jfa)
#' set.seed(1)
#' 
#' # Generate some audit data (N = 1000):
#' data <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), 
#'                    bookValue = runif(n = 1000, min = 700, max = 1000))
#' 
#' # Using monetary unit sampling, draw a random sample from the population.
#' s1 <- sampling(population = data, sampleSize = 100, units = "mus", 
#'                bookValues = "bookValue", algorithm = "random")
#' s1_sample <- s1$sample
#' s1_sample$trueValue <- s1_sample$bookValue
#' s1_sample$trueValue[2] <- s1_sample$trueValue[2] - 500 # One overstatement is found
#' 
#' # Using summary statistics, calculate the upper confidence bound according
#' # to the binomial distribution:
#' 
#' e1 <- evaluation(nSumstats = 100, kSumstats = 1, method = "binomial", 
#'                  materiality = 0.05)
#' print(e1)
#' 
#' # ------------------------------------------------------------ 
#' #             jfa Evaluation Summary (Frequentist)
#' # ------------------------------------------------------------ 
#' # Input: 
#' #
#' # Confidence:               95%   
#' # Materiality:              5% 
#' # Minium precision:         Not specified 
#' # Sample size:              100 
#' # Sample errors:            1 
#' # Sum of taints:            1 
#' # Method:                   binomial 
#' # ------------------------------------------------------------
#' # Output:
#' #
#' # Most likely error:        1% 
#' # Upper bound:              4.66% 
#' # Precision:                3.66% 
#' # Conclusion:               Approve population
#' # ------------------------------------------------------------
#'
#' # Evaluate the raw sample using the stringer bound and the sample counts:
#' 
#' e2 <- evaluation(sample = s1_sample, bookValues = "bookValue", auditValues = "trueValue", 
#'                  method = "stringer", materiality = 0.05, counts = s1_sample$counts)
#' print(e2)
#' 
#' # ------------------------------------------------------------ 
#' #             jfa Evaluation Summary (Frequentist)
#' # ------------------------------------------------------------ 
#' # Input: 
#' #
#' # Confidence:               95%   
#' # Materiality:              5% 
#' # Minium precision:         Not specified 
#' # Sample size:              100 
#' # Sample errors:            1 
#' # Sum of taints:            1 
#' # Method:                   stringer 
#' # ------------------------------------------------------------
#' # Output:
#' #
#' # Most likely error:        0.69% 
#' # Upper bound:              4.12% 
#' # Precision:                3.44% 
#' # Conclusion:               Approve population 
#' # ------------------------------------------------------------  
#' 
#' @keywords evaluation confidence bound audit
#'
#' @export 

evaluation <- function(confidence = 0.95, method = "binomial", N = NULL,
						sample = NULL, bookValues = NULL, auditValues = NULL, counts = NULL, 
						nSumstats = NULL, kSumstats = NULL, 
						materiality = NULL, minPrecision = NULL,
                       	prior = FALSE, nPrior = 0, kPrior = 0, 
                       	rohrbachDelta = 2.7, momentPoptype = "accounts", populationBookValue = NULL,
                       	csA = 1, csB = 3, csMu = 0.5){

	# Import existing prior distribution from class 'jfaPrior'.
	if(class(prior) == "jfaPrior"){
		if(kPrior != 0 || nPrior != 0)
			warning("When the prior is of class 'jfaPrior', the arguments 'kPrior' and 'nPrior' will not be used.")
		nPrior 		<- prior$description$implicitn
		kPrior 		<- prior$description$implicitk
		method 		<- prior$likelihood
	}

	# Perform error handling with respect to incompatible input options
	if(is.null(materiality) && is.null(minPrecision))
		stop("Specify the materiality or the minimum precision")
	
	if(!is.null(minPrecision) && minPrecision == 0)
		stop("The minimum required precision cannot be zero.")
	
	if(!(method %in% c("poisson", "binomial", "hypergeometric", "stringer", "stringer-meikle", "stringer-lta", "stringer-pvz", "rohrbach", "moment", "coxsnell", "direct", "difference", "quotient", "regression")) || length(method) != 1)
		stop("Specify a valid method for the evaluation.")

	if(!is.null(counts) && any(counts < 1))
		stop("When specified, your 'counts' must all be equal to, or larger than, 1.")          

	if(((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior") && method %in% c("stringer", "stringer-meikle", "stringer-lta", "stringer-pvz", "rohrbach", "moment", "direct", "difference", "quotient", "regression"))
		stop("To use a prior distribution, you must use either the poisson, the binomial, or the hypergeometric method.")  

 	if((class(prior) == "logical" && prior == TRUE) && kPrior < 0 || nPrior < 0)
		stop("When you specify a prior, both kPrior and nPrior should be higher than zero")

  	if(!is.null(nSumstats) || !is.null(kSumstats)){
     	if(is.null(nSumstats) || is.null(kSumstats))
       		stop("When using summary statistics, both nSumstats and kSumstats must be defined")
      	if(nSumstats <= 0 || kSumstats < 0)
        	stop("When using summary statistics, both nSumstats and kSumstats must be positive")
    	if(length(nSumstats) != 1 || length(kSumstats) != 1)
      		stop("Specify one value for nSumstat and kSumstat")
    	if(kSumstats > nSumstats)
      		stop("The sum of the errors is higher than the sample size")
    	if(method %in% c("stringer", "stringer-meikle", "stringer-lta", "stringer-pvz", "coxsnell", "rohrbach", "moment", "direct", "difference", "quotient", "regression"))
      		stop("The selected method requires raw observations, and does not accomodate summary statistics")
    
		n <- nSumstats
		k <- kSumstats
		t <- kSumstats
  
  	} else if(!is.null(sample)){

		if(is.null(bookValues) || is.null(auditValues) || length(bookValues) != 1 || length(auditValues) != 1)
			stop("Specify a valid book value column name and a valid audit value column name when using a sample")
		
    	sample <- stats::na.omit(sample)
		n <- nrow(sample)
		if(!is.null(counts))
			n <- sum(counts)
		bv <- sample[, bookValues]
		av <- sample[, auditValues]
		taints <- (bv - av) / bv
		k <- length(which(taints != 0))

		if(!is.null(counts))
			taints <- taints * counts
		
		t <- sum(taints)
		
	}

	# Set the materiality and the minimium precision to 1 if they are NULL
	if(is.null(materiality))
		materiality <- 1
	if(is.null(minPrecision))
		minPrecision <- 1

	# Define placeholders for the most likely error and the precision  
	mle <- NULL
	precision <- NULL
  
	# Calculate the results depending on the specified method
	if(method == "poisson"){
		if((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior"){
			bound <- stats::qgamma(p = confidence, shape = 1 + kPrior + t, rate = 1 + nPrior + n)
			mle <- (1 + kPrior + t - 1) / (1 + nPrior + n)
			precision <- bound - mle
		} else {
			bound <- stats::poisson.test(x = k, T = n, r = materiality, alternative = "less", conf.level = confidence)$conf.int[2]
			mle <- k / n
			precision <- bound - mle
		}
	} else if(method == "binomial"){
		if((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior"){
			bound <- stats::qbeta(p = confidence, shape1 = 1 + kPrior + t, shape2 = 1 + nPrior - kPrior + n - t)
			mle <- (1 + kPrior + t - 1) / (1 + kPrior + t + 1 + nPrior - kPrior + n - t)
			precision <- bound - mle
		} else {
			bound <- stats::binom.test(x = k, n = n, p = materiality, alternative = "less", conf.level = confidence)$conf.int[2]
			mle <- k / n
			precision <- bound - mle
		}
	} else if(method == "hypergeometric"){
		if((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior"){
			if(is.null(N))
				stop("Evaluation with beta-binomial distribution requires that you specify the population size N")
			bound <- .qBetaBinom(p = confidence, N = N, shape1 = 1 + kPrior + k, shape2 = 1 + nPrior - kPrior + n - k) / N
			mle <- (which.max(.dBetaBinom(x = 0:N, N = N, shape1 = 1 + kPrior + k, shape2 = 1 + nPrior - kPrior + n - k)) - 1) / N
			precision <- bound - mle
		} else {
			if(materiality == 1)
				stop("Evaluation with the hypergeometric distribution requires that you specify the materiality")
			populationK <- materiality * N
			bound <- stats::phyper(q = k, m = populationK, n = N - populationK, k = n)
			mle <- k / n
			precision <- bound - mle
		}
	} else if(method == "stringer"){
		bound <- .stringerBound(taints, confidence, n)
		mle <- t / n
		precision <- bound - mle
	} else if(method == "stringer-meikle"){
		bound <- .stringerBound(taints, confidence, n, correction = "meikle")
		mle <- t / n
		precision <- bound - mle
	} else if(method == "stringer-lta"){
		bound <- .stringerBound(taints, confidence, n, correction = "lta")
		mle <- t / n
		precision <- bound - mle
	} else if(method == "stringer-pvz"){
		bound <- .stringerBound(taints, confidence, n, correction = "pvz")
		mle <- t / n
		precision <- bound - mle
	} else if(method == "rohrbach"){
		bound <- .rohrbachBound(taints, confidence, n, N, rohrbachDelta = rohrbachDelta)
		mle <- t / n
		precision <- bound - mle
	} else if(method == "moment"){
		bound <- .momentBound(taints, confidence, n, momentPoptype = momentPoptype)
		mle <- t / n
		precision <- bound - mle
	} else if(method == "coxsnell"){
		bound <- .coxAndSnellBound(taints, confidence, n, csA, csB, csMu, aPrior = 1 + kPrior, bPrior = 1 + nPrior - kPrior)
	} else if(method == "direct"){
		bound <- .directMethod(bv, av, confidence, N, n, populationBookValue)
		mle <- bound$pointEstimate
		precision <- (bound$upperBound - mle) / populationBookValue
	} else if(method == "difference"){
		bound <- .differenceMethod(bv, av, confidence, N, n, populationBookValue)
		mle <- bound$pointEstimate
		precision <- (bound$upperBound - mle) / populationBookValue
	} else if(method == "quotient"){
		bound <- .quotientMethod(bv, av, confidence, N, n, populationBookValue)
		mle <- bound$pointEstimate
		precision <- (bound$upperBound - mle) / populationBookValue
	} else if(method == "regression"){
		bound <- .regressionMethod(bv, av, confidence, N, n, populationBookValue)
		mle <- bound$pointEstimate
		precision <- (bound$upperBound - mle) / populationBookValue
	}
	# Add new methods here.
  
	# Create the main results object
	result <- list()
	result[["confidence"]]    <- as.numeric(confidence)
	result[["method"]]        <- as.character(method)
	result[["N"]]             <- as.numeric(N)
	result[["n"]]             <- as.numeric(n)
	result[["k"]]             <- as.numeric(k)
	result[["t"]]             <- as.numeric(t)
	result[["materiality"]]   <- as.numeric(materiality)
	result[["minPrecision"]]  <- as.numeric(minPrecision)
	if(!is.null(mle))
		result[["mle"]]			<- as.numeric(mle)
	if(!is.null(precision))
		result[["precision"]]	<- as.numeric(precision)
	if(method %in% c("direct", "difference", "quotient", "regression")){
		result[["popBookvalue"]]   <- as.numeric(populationBookValue)
		result[["pointEstimate"]]  <- as.numeric(bound[["pointEstimate"]])
		result[["lowerBound"]]     <- as.numeric(bound[["lowerBound"]])
		result[["upperBound"]]     <- as.numeric(bound[["upperBound"]])
	} else {
		if(method == "coxsnell"){
			result[["confBound"]]            <- as.numeric(bound[["bound"]])
			result[["multiplicationFactor"]] <- as.numeric(bound[["multiplicationFactor"]])
			result[["df1"]]                  <- as.numeric(bound[["df1"]])
			result[["df2"]]                  <- as.numeric(bound[["df2"]])
			result[["mle"]]					 <- result[["multiplicationFactor"]] * (((result[["df1"]]-2)/result[["df1"]]) * (result[["df2"]] / (result[["df2"]] + 2)))
			result[["precision"]]			 <- result[["confBound"]] - result[["mle"]]
		} else {
			result[["confBound"]]            <- as.numeric(bound) 
		}
	}
	if(method == "hypergeometric" && is.logical(prior) && prior == FALSE)
		result[["populationK"]]        		 <- as.numeric(populationK)
	# Produce relevant conclusions conditional on the analysis result
	approvePrecision <- TRUE
	if(minPrecision != 1)
		approvePrecision <- result[["precision"]] <= minPrecision
	approveMateriality <- TRUE
	if(materiality != 1){
		if(method %in% c("direct", "difference", "quotient", "regression")){
			approveMateriality <- populationBookValue <= result[["upperBound"]] && populationBookValue >= result[["lowerBound"]]
		} else {
			approveMateriality <- result[["confBound"]] < materiality
		}
	}
	# Provide the conclusion
	result[["conclusion"]] <- ifelse(approveMateriality && approvePrecision, 
										yes = "Approve population",
										no = "Do not approve population")
	# Create the prior distribution object	
	if(((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior")){
		if(class(prior) == "jfaPrior"){
			result[["prior"]] 			<- prior
		} else {
			result[["prior"]]           <- auditPrior(confidence = confidence, 
														likelihood = method, 
														method = "sample", 
														expectedError = 0, 
														N = result[["N"]], 
														materiality = result[["materiality"]], 
														sampleN = nPrior, 
														sampleK = kPrior)
		}
	}
	# Create the posterior distribution object
	if(!is.null(result[["prior"]])){
		result[["posterior"]] <- list()
		# Functional form of the posterior distribution
		result[["posterior"]]$posterior <- switch(method, 
													"poisson" = paste0("gamma(\u03B1 = ", round(result[["prior"]]$description$alpha + result[["t"]], 3), ", \u03B2 = ", round(result[["prior"]]$description$beta + result[["n"]], 3), ")"),
													"binomial" = paste0("beta(\u03B1 = ", round(result[["prior"]]$description$alpha + result[["t"]], 3), ", \u03B2 = ", round(result[["prior"]]$description$beta + result[["n"]] - result[["t"]], 3), ")"),
													"hypergeometric" = paste0("beta-binomial(N = ", result[["N"]], ", \u03B1 = ", round(result[["prior"]]$description$alpha + result[["k"]], 3), ", \u03B2 = ", round(result[["prior"]]$description$beta + result[["n"]] - result[["k"]], 3), ")"))
		# Create the description section
		result[["posterior"]][["description"]]			<- list()
		result[["posterior"]][["description"]]$density 	<- switch(method, "poisson" = "gamma", "binomial" = "beta", "hypergeometric" = "beta-binomial")
		result[["posterior"]][["description"]]$alpha   	<- switch(method, 
																	"poisson" = result[["prior"]]$description$alpha + result[["t"]],
																	"binomial" = result[["prior"]]$description$alpha + result[["t"]],
																	"hypergeometric" = result[["prior"]]$description$alpha + result[["k"]])
		result[["posterior"]][["description"]]$beta   	<- switch(method, 
																	"poisson" = result[["prior"]]$description$beta + result[["n"]], 
																	"binomial" = result[["prior"]]$description$beta + result[["n"]] - result[["t"]], 
																	"hypergeometric" = result[["prior"]]$description$beta + result[["n"]] - result[["k"]])
		# Create the statistics section
		result[["posterior"]][["statistics"]] 			<- list()
		result[["posterior"]][["statistics"]]$mode 		<- switch(method, 
																	"poisson" = (result[["posterior"]][["description"]]$alpha - 1) / result[["posterior"]][["description"]]$beta,
																	"binomial" = (result[["posterior"]][["description"]]$alpha - 1) / (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta - 2),
																	"hypergeometric" = which.max(.dBetaBinom(x = 0:result[["N"]], N = result[["N"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta)) - 1)
		result[["posterior"]][["statistics"]]$mean 		<- switch(method, 
																	"poisson" = result[["posterior"]][["description"]]$alpha / result[["posterior"]][["description"]]$beta,
																	"binomial" = result[["posterior"]][["description"]]$alpha / (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta),
																	"hypergeometric" = result[["posterior"]][["description"]]$alpha / (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta) * result[["N"]])
		result[["posterior"]][["statistics"]]$median 	<- switch(method, 
																	"poisson" = stats::qgamma(0.5, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
																	"binomial" = stats::qbeta(0.5, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
																	"hypergeometric" = .qBetaBinom(0.5, N = result[["N"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta))
		result[["posterior"]][["statistics"]]$ub 		<- switch(method, 
																	"poisson" = stats::qgamma(confidence, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
																	"binomial" = stats::qbeta(confidence, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
																	"hypergeometric" = .qBetaBinom(confidence, N = result[["N"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta))									
		result[["posterior"]][["statistics"]]$precision <- ifelse(method == "hypergeometric", 
																			yes = (result[["posterior"]][["statistics"]]$ub - result[["posterior"]][["statistics"]]$mode) / result[["N"]],
																			no = result[["posterior"]][["statistics"]]$ub - result[["posterior"]][["statistics"]]$mode)
		# Create the hypotheses section
		if(result[["materiality"]] != 1){
			result[["posterior"]][["hypotheses"]] 				<- list()
			result[["posterior"]][["hypotheses"]]$hypotheses 	<- c(paste0("H-: \u0398 < ", materiality), paste0("H+: \u0398 > ", materiality))
			result[["posterior"]][["hypotheses"]]$pHmin 		<- switch(method, 
																			"poisson" = stats::pgamma(materiality, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
																			"binomial" = stats::pbeta(materiality, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
																			"hypergeometric" = .pBetaBinom(ceiling(materiality * result[["N"]]), N = result[["N"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta))
			result[["posterior"]][["hypotheses"]]$pHplus 		<- switch(method, 
																			"poisson" = stats::pgamma(materiality, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta, lower.tail = FALSE),
																			"binomial" = stats::pbeta(materiality, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta, lower.tail = FALSE),
																			"hypergeometric" = 1 - result[["posterior"]][["hypotheses"]]$pHmin)
			result[["posterior"]][["hypotheses"]]$oddsHmin 		<- result[["posterior"]][["hypotheses"]]$pHmin / result[["posterior"]][["hypotheses"]]$pHplus
			result[["posterior"]][["hypotheses"]]$oddsHplus 	<- 1 / result[["posterior"]][["hypotheses"]]$oddsHmin
			result[["posterior"]][["hypotheses"]]$bf			<- result[["posterior"]][["hypotheses"]]$oddsHmin / result[["prior"]][["hypotheses"]]$oddsHmin
		}
		result[["posterior"]][["N"]] <- result[["N"]]
		# Add class 'jfaPosterior' to the posterior distribution object.
		class(result[["posterior"]]) <- "jfaPosterior"
	}
	# Add class 'jfaEvaluation' to the result.
	class(result) <- "jfaEvaluation"
	return(result)
}
