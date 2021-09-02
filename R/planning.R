#' Plan a statistical audit sample
#'
#' @description This function calculates the minimum sample size for a statistical audit sample based on the binomial, Poisson, or hypergeometric likelihood. The function returns an object of class \code{jfaPlanning} which can be used with associated \code{summary()} and \code{plot()} methods.
#'
#' For more details on how to use this function, see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage planning(materiality = NULL, minPrecision = NULL, expectedError = 0,
#'          likelihood = 'binomial', confidence = 0.95, N = NULL,
#'          prior = FALSE, nPrior = 0, kPrior = 0,
#'          increase = 1, maxSize = 5000)
#'
#' @param materiality   a numeric value between 0 and 1 specifying the performance materiality (i.e., maximum upper limit) as a fraction of the total population size. Can be \code{NULL}, but \code{minPrecision} should be specified in that case.
#' @param minPrecision  a numeric value between 0 and 1 specifying the minimum precision (i.e., upper bound minus most likely error) as a fraction of the total population size. Can be \code{NULL}, but \code{materiality} should be specified in that case.
#' @param expectedError a numeric value between 0 and 1 specifying the expected errors in the sample relative to the total sample size, or a number (>= 1) that represents the number of expected errors in the sample. It is advised to set this value conservatively to minimize the probability of the observed errors exceeding the expected errors, which would imply that insufficient work has been done in the end.
#' @param likelihood    a character specifying the likelihood assumed in the calculation. This can be either \code{binomial} for the binomial likelihood, \code{poisson} for the Poisson likelihood, or \code{hypergeometric} for the hypergeometric likelihood. See the details section for more information about the available likelihoods.
#' @param confidence    a numeric value between 0 and 1 specifying the confidence level used in the planning. Defaults to 0.95 for 95\% confidence.
#' @param N             an integer larger than 0 specifying the total population size. Only required when \code{likelihood = 'hypergeometric'}.
#' @param prior         a logical specifying whether to use a prior distribution when planning, or an object of class \code{jfaPrior} or \code{jfaPosterior} containing the prior distribution. Defaults to \code{FALSE} for frequentist planning. If \code{TRUE}, a negligible prior distribution is chosen by default, but can be adjusted using the \code{kPrior} and \code{nPrior} arguments. Chooses a conjugate gamma distribution for the Poisson likelihood, a conjugate beta distribution for the binomial likelihood, and a conjugate beta-binomial distribution for the hypergeometric likelihood.
#' @param nPrior        if \code{prior = TRUE}, a numeric value larger than, or equal to, 0 specifying the sample size of the sample equivalent to the prior information.
#' @param kPrior        if \code{prior = TRUE}, a numeric value larger than, or equal to, 0 specifying the sum of errors in the sample equivalent to the prior information.
#' @param increase      an integer larger than 0 specifying the desired increase step for the sample size calculation.
#' @param maxSize       an integer larger than 0 specifying the maximum sample size that is considered in the calculation. Defaults to 5000 for efficiency. Increase this value if the sample size cannot be found due to it being too large (e.g., for a low materiality).
#' 
#' @details This section elaborates on the available likelihoods and corresponding prior distributions for the \code{likelihood} argument.
#' 
#' \itemize{
#'  \item{\code{binomial}:         The binomial likelihood is often used as a likelihood for attributes sampling \emph{with} replacement. The likelihood function is defined as: \deqn{p(x) = {n \choose k} p^k (1 - p)^{n - k}} The conjugate \emph{beta(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{1}{B(\alpha, \beta)} x^{\alpha - 1} (1 - x)^{\beta - 1}}}
#'  \item{\code{poisson}:          The Poisson likelihood is often used as a likelihood for monetary unit sampling (MUS). The likelihood function is defined as: \deqn{p(x) = \frac{\lambda^x e^{-\lambda}}{x!}} The conjugate \emph{gamma(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{\beta^\alpha x^{\alpha - 1} e^{-\beta x}}{\Gamma(\alpha)}}}
#'  \item{\code{hypergeometric}:   The hypergeometric likelihood is used as a likelihood for sampling \emph{without} replacement. The likelihood function is defined as: \deqn{p(x = k) = \frac{{K \choose k} {N - K \choose n - k}}{{N \choose n}}} The conjugate \emph{beta-binomial(\eqn{\alpha, \beta})} prior (Dyer and Pierce, 1993) has probability density function: \deqn{f(k | n, \alpha, \beta) = {n \choose k} \frac{B(k + \alpha, n - k + \beta)}{B(\alpha, \beta)}} }
#' }
#'
#' @return An object of class \code{jfaPlanning} containing:
#' 
#' \item{confidence}{a numeric value between 0 and 1 indicating the confidence level.}
#' \item{materiality}{a numeric value between 0 and 1 indicating the specified materiality. Can be \code{NULL}.}
#' \item{minPrecision}{a numeric value between 0 and 1 indicating the minimum precision to be obtained. Can be \code{NULL}.}
#' \item{expectedError}{a numeric value larger than, or equal to, 0 indicating the expected errors input.}
#' \item{likelihood}{a character indicating the specified likelihood.}
#' \item{N}{an integer larger than 0 indicating the population size (only returned if \code{N} is specified).}
#' \item{sampleSize}{an integer larger than 0 indicating the required sample size.}
#' \item{errorType}{a character indicating whether the expected errors where specified as a percentage or as an integer.}
#' \item{expectedSampleError}{a numeric value larger than, or equal to, 0 indicating the number of errors that are allowed in the sample.}
#' \item{expectedBound}{a numeric value between 0 and 1 indicating the expected upper bound if the sample goes according to plan.}
#' \item{expectedPrecision}{a numeric value between 0 and 1 indicating the expected precision if the sample goes according to plan.}
#' \item{populationK}{if \code{likelihood = 'hypergeometric'}, an integer larger than 0 indicating the assumed population errors.}
#' \item{prior}{if a prior distribution is specified, an object of class \code{jfaPrior} that contains information about the prior distribution.}
#' \item{expectedPosterior}{if a prior distribution is specified, an object of class \code{jfaPosterior} that contains information about the expected posterior distribution.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}} \code{\link{selection}} \code{\link{evaluation}} \code{\link{report}} \code{\link{auditBF}}
#'
#' @references Derks, K., de Swart, J., van Batenburg, P., Wagenmakers, E.-J., & Wetzels, R. (2021). Priors in a Bayesian audit: How integration of existing information into the prior distribution can improve audit transparency and efficiency. \emph{International Journal of Auditing}, 1-16.
#' @references Dyer, D. and Pierce, R.L. (1993). On the choice of the prior distribution in hypergeometric sampling. \emph{Communications in Statistics - Theory and Methods}, 22(8), 2125 - 2146.
#'
#' @keywords planning sample size audit
#'
#' @examples
#' # Frequentist planning using a binomial likelihood
#' planning(materiality = 0.05, expectedError = 0.025, likelihood = 'binomial')
#' 
#' # Bayesian planning using a negligible beta prior
#' planning(materiality = 0.05, expectedError = 0.025, likelihood = 'binomial', 
#'          prior = TRUE)
#'
#' # Bayesian planning using an informed beta prior
#' planning(materiality = 0.05, expectedError = 0.025, likelihood = 'binomial', 
#'          prior = auditPrior(method = 'median', materiality = 0.05))
#'
#' @export

planning <- function(materiality = NULL, minPrecision = NULL, expectedError = 0,
                     likelihood = 'binomial', confidence = 0.95, N = NULL,  
                     prior = FALSE, nPrior = 0, kPrior = 0,
                     increase = 1, maxSize = 5000) {
  
  # Import existing prior distribution with class 'jfaPrior' or 'jfaPosterior'.
  if (class(prior) %in% c("jfaPrior", "jfaPosterior")) {
    
    if (kPrior != 0 || nPrior != 0)
      warning("When the prior is of class 'jfaPrior' or 'jfaPosterior', the arguments 'nPrior' and 'kPrior' will not be used.")
    
    nPrior      <- prior[["description"]]$implicitn
    kPrior      <- prior[["description"]]$implicitk
    likelihood  <- prior[["likelihood"]]
    
  }
  
  # Perform error handling with respect to incompatible input options
  if (is.null(materiality) && is.null(minPrecision))
    stop("You must specify your sampling objective(s) using the 'materiality' or 'minPrecision' argument(s).")

  if (confidence >= 1 || confidence <= 0 || is.null(confidence))
    stop("Specify a valid value for the 'confidence' argument. Possible values lie within the range of 0 to 1.")
  
  if (!(likelihood %in% c("poisson", "binomial", "hypergeometric")))
    stop("Specify a valid option for the 'likelihood' argument. Possible options are 'binomial', 'poisson', and 'hypergeometric'.")
  
  if (!is.null(minPrecision) && (minPrecision <= 0 || minPrecision >= 1))
    stop("The minimum required precision must be a positive value < 1.")
  
  if ((class(prior) == "logical" && prior == TRUE) && kPrior < 0 || nPrior < 0)
    stop("If you specify a prior, the values for the 'nPrior' and 'kPrior' arguments should be >= 0.")
  
  # Define a placeholder for the sample size 
  ss <- NULL
  
  # Find out the type of expected errors (percentage vs. number)
  if (expectedError >= 0 && expectedError < 1) {
    
    errorType <- "percentage"
    if (!is.null(materiality) && expectedError >= materiality)
      stop("The value for the 'expectedError' argument must be lower than the value for the 'materiality' argument.")
    
  } else if (expectedError >= 1) {
    
    errorType <- "integer"
    if (expectedError%%1 != 0 && likelihood %in% c("binomial", "hypergeometric") && !((class(prior) == "logical" && prior == TRUE) || class(prior) %in% c("jfaPrior", "jfaPosterior")))
      stop("When the 'expectedError' argument > 1 and the 'likelihood' argument is 'binomial' or 'hypergeometric', its value must be an integer.")
    
  }
  
  # Set the materiality and the minimium precision to 1 if they are NULL
  if (is.null(materiality))
    materiality <- 1
  if (is.null(minPrecision))
    minPrecision <- 1
  
  # Requirements for the hypergeometric distribution
  if (likelihood == 'hypergeometric') {
    if (is.null(N) || N <= 0)
      stop("The 'hypergeometric' likelihood requires a positive integer as input for the population size 'N'.")
    if (!is.null(materiality) && (expectedError / N) >= materiality)
      stop("The value of 'expectedError' / 'N' must be lower than the value for the 'materiality' argument.")
  }
  
  # Define the sampling frame (the possible sample sizes)
  samplingFrame <- seq(from = 0, to = maxSize, by = increase)
  samplingFrame[1] <- 1
  
  # Set up iterations
  iter <- 1
  sufficient <- FALSE
  
  # Start iterations
  while (!sufficient && iter < maxSize) {
    
    i <- samplingFrame[iter]
    
    # Find the expected errors in the sample
    implicitK <- switch(errorType, "percentage" = expectedError * i, "integer" = expectedError)
    
    if (likelihood == "hypergeometric") {
      implicitK   <- ceiling(implicitK) # Convert to an integer in the case of a hypergeometric likelihood
      populationK <- ceiling(materiality * N)
    }
    
    while (i <= implicitK) { # Remove redundant numbers from the sampling frame for more efficient sampling
      samplingFrame <- samplingFrame[-iter]
      i <- samplingFrame[iter]
    }
    
    if ((class(prior) == "logical" && prior == TRUE) || class(prior) %in% c("jfaPrior", "jfaPosterior")) { # Bayesian planning
      
      bound <- switch(likelihood, 
                      "poisson" = stats::qgamma(confidence, shape = (1 + kPrior) + implicitK, rate = nPrior + i),
                      "binomial" = stats::qbeta(confidence, shape1 = (1 + kPrior) + implicitK, shape2 = (1 + nPrior - kPrior) + i - implicitK),
                      "hypergeometric" = .qBetaBinom(p = confidence, N = N - i, shape1 = (1 + kPrior) + implicitK, shape2 = (1 + nPrior - kPrior) + i - implicitK) / N)
      mle <- switch(likelihood,
                    "poisson" = ((1 + kPrior + implicitK) - 1) / (nPrior + i),
                    "binomial" = ((1 + kPrior + implicitK) - 1) / ((1 + kPrior + implicitK) + (1 + nPrior - kPrior + i - implicitK) - 2),
                    "hypergeometric" = .modeBetaBinom(N = N - i, shape1 = (1 + kPrior) + implicitK, shape2 = (1 + nPrior - kPrior) + i - implicitK) / N)
      
    } else { # Classical planning
      
      if (likelihood == "binomial") 
        implicitK <- ceiling(implicitK) # Convert to an integer in the case of a binomial likelihood
      
      bound <- switch(likelihood, 
                      "poisson" = stats::qgamma(p = confidence, shape = 1 + implicitK, rate = i),
                      "binomial" = stats::qbeta(p = confidence, shape1 = 1 + implicitK, shape2 = i - implicitK),
                      "hypergeometric" = .qHyper(p = confidence, N = N, n = i, k = implicitK) / N)
      mle <- implicitK / i
      
    }
    
    sufficient <- bound < materiality && (bound - mle) < minPrecision # Sufficient work done?
    
    if (sufficient)
      ss <- i
    iter <- iter + 1
  }
  
  # No sample size could be calculated, throw an error
  if (is.null(ss))
    stop("Sample size could not be calculated, you may want to increase the 'maxSize' argument.")
  
  if (!is.null(N) && ss > N) # The sample size is too large
    warning("Sampling with replacement is required. The resulting sample size is larger than the population size 'N'.")
  
  # Create the main results object
  result <- list()
  result[["confidence"]]            <- as.numeric(confidence)
  result[["materiality"]]           <- as.numeric(materiality)
  result[["minPrecision"]]          <- as.numeric(minPrecision)
  result[["expectedError"]]         <- as.numeric(expectedError)
  result[["likelihood"]]            <- as.character(likelihood)
  result[["N"]]                     <- as.numeric(N)
  result[["sampleSize"]]            <- as.numeric(ceiling(ss))
  result[["errorType"]]             <- as.character(errorType)
  result[["expectedSampleError"]]   <- as.numeric(implicitK)
  result[["expectedBound"]]         <- as.numeric(bound)
  result[["expectedPrecision"]]     <- as.numeric(bound - mle)
  if (likelihood == "hypergeometric")
    result[["populationK"]]         <- as.numeric(populationK)
  result[["iterations"]]            <- as.numeric(iter)
  
  # Create the prior distribution object	
  if (((class(prior) == "logical" && prior == TRUE) || class(prior) %in% c("jfaPrior", "jfaPosterior"))) {
    if (class(prior) == "jfaPrior" && !is.null(prior[["hypotheses"]])) {
      result[["prior"]]             <- prior
    } else {
      result[["prior"]]             <- auditPrior(confidence = confidence,
                                                  materiality = result[["materiality"]],
                                                  expectedError = 0,
                                                  method = "sample",
                                                  likelihood = likelihood,
                                                  N = result[["N"]],
                                                  sampleN = nPrior, 
                                                  sampleK = kPrior)
    }
  }
  
  if (!is.null(result[["prior"]])) {
    # Create the expected posterior distribution object  
    result[["expectedPosterior"]] <- list()
    
    # Functional form of the expected posterior
    result[["expectedPosterior"]]$posterior <- switch(likelihood, 
                                                      "poisson" = paste0("gamma(\u03B1 = ", round(result[["prior"]]$description$alpha + result[["expectedSampleError"]], 3), ", \u03B2 = ", round(result[["prior"]]$description$beta + result[["sampleSize"]], 3), ")"),
                                                      "binomial" = paste0("beta(\u03B1 = ", round(result[["prior"]]$description$alpha + result[["expectedSampleError"]], 3), ", \u03B2 = ", round(result[["prior"]]$description$beta + result[["sampleSize"]] - result[["expectedSampleError"]], 3), ")"),
                                                      "hypergeometric" = paste0("beta-binomial(N = ", result[["N"]] - result[["sampleSize"]], ", \u03B1 = ", round(result[["prior"]]$description$alpha + result[["expectedSampleError"]], 3), ", \u03B2 = ", round(result[["prior"]]$description$beta + result[["sampleSize"]] - result[["expectedSampleError"]], 3), ")"))
    result[["expectedPosterior"]]$likelihood <- likelihood
    
    # Create the description section
    result[["expectedPosterior"]][["description"]]          <- list()
    result[["expectedPosterior"]][["description"]]$density  <- switch(likelihood, "poisson" = "gamma", "binomial" = "beta", "hypergeometric" = "beta-binomial")
    result[["expectedPosterior"]][["description"]]$n        <- result[["sampleSize"]]
    result[["expectedPosterior"]][["description"]]$k        <- result[["expectedSampleError"]]
    result[["expectedPosterior"]][["description"]]$alpha    <- result[["prior"]]$description$alpha + result[["expectedSampleError"]]
    result[["expectedPosterior"]][["description"]]$beta     <- switch(likelihood, 
                                                                      "poisson" = result[["prior"]]$description$beta + result[["sampleSize"]], 
                                                                      "binomial" = result[["prior"]]$description$beta + result[["sampleSize"]] - result[["expectedSampleError"]], 
                                                                      "hypergeometric" = result[["prior"]]$description$beta + result[["sampleSize"]] - result[["expectedSampleError"]])
    result[["expectedPosterior"]][["description"]]$implicitk <- result[["expectedPosterior"]][["description"]]$alpha - 1
    result[["expectedPosterior"]][["description"]]$implicitn <- switch(likelihood,
                                                                       "poisson" = result[["expectedPosterior"]][["description"]]$beta, 
                                                                       "binomial" = result[["expectedPosterior"]][["description"]]$beta - 1 + result[["expectedPosterior"]][["description"]]$implicitk, 
                                                                       "hypergeometric" = result[["expectedPosterior"]][["description"]]$beta - 1 + result[["expectedPosterior"]][["description"]]$implicitk)    
    
    # Create the statistics section
    result[["expectedPosterior"]][["statistics"]]           <- list()
    result[["expectedPosterior"]][["statistics"]]$mode      <- switch(likelihood, 
                                                                      "poisson" = (result[["expectedPosterior"]][["description"]]$alpha - 1) / result[["expectedPosterior"]][["description"]]$beta,
                                                                      "binomial" = (result[["expectedPosterior"]][["description"]]$alpha - 1) / (result[["expectedPosterior"]][["description"]]$alpha + result[["expectedPosterior"]][["description"]]$beta - 2),
                                                                      "hypergeometric" = .modeBetaBinom(N = result[["N"]] - result[["sampleSize"]], shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta))
    result[["expectedPosterior"]][["statistics"]]$mean      <- switch(likelihood, 
                                                                      "poisson" = result[["expectedPosterior"]][["description"]]$alpha / result[["expectedPosterior"]][["description"]]$beta,
                                                                      "binomial" = result[["expectedPosterior"]][["description"]]$alpha / (result[["expectedPosterior"]][["description"]]$alpha + result[["expectedPosterior"]][["description"]]$beta),
                                                                      "hypergeometric" = result[["expectedPosterior"]][["description"]]$alpha / (result[["expectedPosterior"]][["description"]]$alpha + result[["expectedPosterior"]][["description"]]$beta) * result[["N"]])
    result[["expectedPosterior"]][["statistics"]]$median    <- switch(likelihood, 
                                                                      "poisson" = stats::qgamma(0.5, shape = result[["expectedPosterior"]][["description"]]$alpha, rate = result[["expectedPosterior"]][["description"]]$beta),
                                                                      "binomial" = stats::qbeta(0.5, shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta),
                                                                      "hypergeometric" = .qBetaBinom(0.5, N = result[["N"]] - result[["sampleSize"]], shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta))
    result[["expectedPosterior"]][["statistics"]]$ub        <- switch(likelihood, 
                                                                      "poisson" = stats::qgamma(confidence, shape = result[["expectedPosterior"]][["description"]]$alpha, rate = result[["expectedPosterior"]][["description"]]$beta),
                                                                      "binomial" = stats::qbeta(confidence, shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta),
                                                                      "hypergeometric" = .qBetaBinom(confidence, N = result[["N"]] - result[["sampleSize"]], shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta))									
    result[["expectedPosterior"]][["statistics"]]$precision <- result[["expectedPosterior"]][["statistics"]]$ub - result[["expectedPosterior"]][["statistics"]]$mode
    
    # Create the hypotheses section
    if (result[["materiality"]] != 1) {
      result[["expectedPosterior"]][["hypotheses"]]             <- list()
      result[["expectedPosterior"]][["hypotheses"]]$hypotheses  <- c(paste0("H-: \u0398 < ", materiality), paste0("H+: \u0398 > ", materiality))
      result[["expectedPosterior"]][["hypotheses"]]$pHmin       <- .restrictprob(switch(likelihood, 
                                                                                        "poisson" = stats::pgamma(materiality, shape = result[["expectedPosterior"]][["description"]]$alpha, rate = result[["expectedPosterior"]][["description"]]$beta),
                                                                                        "binomial" = stats::pbeta(materiality, shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta),
                                                                                        "hypergeometric" = .pBetaBinom(ceiling(materiality * result[["N"]]), N = result[["N"]] - result[["sampleSize"]], shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta)))
      result[["expectedPosterior"]][["hypotheses"]]$pHplus      <- .restrictprob(switch(likelihood, 
                                                                                        "poisson" = stats::pgamma(materiality, shape = result[["expectedPosterior"]][["description"]]$alpha, rate = result[["expectedPosterior"]][["description"]]$beta, lower.tail = FALSE),
                                                                                        "binomial" = stats::pbeta(materiality, shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta, lower.tail = FALSE),
                                                                                        "hypergeometric" = .pBetaBinom(ceiling(materiality * result[["N"]]), N = result[["N"]] - result[["sampleSize"]], shape1 = result[["expectedPosterior"]][["description"]]$alpha, shape2 = result[["expectedPosterior"]][["description"]]$beta, lower.tail = FALSE)))
      result[["expectedPosterior"]][["hypotheses"]]$oddsHmin    <- result[["expectedPosterior"]][["hypotheses"]]$pHmin / result[["expectedPosterior"]][["hypotheses"]]$pHplus
      result[["expectedPosterior"]][["hypotheses"]]$oddsHplus   <- 1 / result[["expectedPosterior"]][["hypotheses"]]$oddsHmin
      result[["expectedPosterior"]][["hypotheses"]]$expectedBf  <- result[["expectedPosterior"]][["hypotheses"]]$oddsHmin / result[["prior"]][["hypotheses"]]$oddsHmin
    }
    
    result[["expectedPosterior"]][["N"]] <- result[["N"]]
    
    # Add class 'jfaPosterior' to the expected posterior distribution object
    class(result[["expectedPosterior"]]) <- "jfaPosterior"
  }
  
  # Add class 'jfaPlanning' to the result
  class(result) <- "jfaPlanning"
  return(result)
}