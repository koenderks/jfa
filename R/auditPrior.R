#' Create a prior distribution for audit sampling
#'
#' @description This function creates a prior distribution with audit information to be used in the \code{planning()} and \code{evaluation()} functions via their \code{prior} argument. The function returns an object of class \code{jfaPrior} which can be used with associated \code{print()} and \code{plot()} methods.
#'
#' For more details on how to use this function, see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage auditPrior(confidence, materiality = NULL, expectedError = 0, 
#'            method = 'none', likelihood = 'binomial', N = NULL, 
#'            ir = 1, cr = 1, ub = NULL, pHmin = NULL, pHplus = NULL, 
#'            sampleN = 0, sampleK = 0, factor = 1)
#' 
#' @param confidence      a numeric value between 0 and 1 specifying the confidence level to be used in the planning.
#' @param materiality     a numeric value between 0 and 1 specifying the performance materiality (i.e., the maximum upper limit) as a fraction of the total population size. Can be \code{NULL} for some methods.
#' @param expectedError   a numeric value between 0 and 1 specifying the expected errors in the sample relative to the total sample size, or a numeric value (>= 1) that represents the sum of expected errors in the sample. It is advised to set this value conservatively to minimize the probability of the observed errors exceeding the expected errors, which would imply that insufficient work has been done in the end.
#' @param method          a character specifying the method by which the prior distribution is constructed. Defaults to \code{none} which incorporates no existing information. Other options are \code{arm}, \code{bram}, \code{median}, \code{hypotheses}, \code{sample}, and \code{factor}. See the details section for more information about the available methods.
#' @param likelihood      a character specifying the likelihood assumed when updating the prior distribution. This can be either \code{binomial} for the binomial likelihood and beta prior distribution, \code{poisson} for the Poisson likelihood and gamma prior distribution, or \code{hypergeometric} for the hypergeometric likelihood and beta-binomial prior distribution. See the details section for more information about the available likelihoods.
#' @param N               an numeric value larger than 0 specifying the total population size. Optional unless \code{likelihood = 'hypergeometric'}.
#' @param ir              if \code{method = 'arm'}, a numeric value between 0 and 1 specifying the inherent risk in the audit risk model. Defaults to 1 for 100\% risk.
#' @param cr              if \code{method = 'arm'}, a numeric value between 0 and 1 specifying the internal control risk in the audit risk model. Defaults to 1 for 100\% risk.
#' @param ub              if \code{method = 'bram'}, a numeric value between 0 and 1 specifying the upper bound for the prior distribution as a fraction of the population size.
#' @param pHmin           if \code{method = 'hypotheses'}, a numeric value between 0 and 1 specifying the prior probability of the hypothesis \eqn{\theta <} materiality.
#' @param pHplus          if \code{method = 'hypotheses'}, a numeric value between 0 and 1 specifying the prior probability of the hypothesis \eqn{\theta >} materiality.
#' @param sampleN         if \code{method = 'sample'} or \code{method = 'factor'}, an integer larger than, or equal to, 0 specifying the sample size of the sample equivalent to the prior information.
#' @param sampleK         if \code{sample} or \code{factor}, a numeric value larger than, or equal to, 0 specifying the sum of errors in the sample equivalent to the prior information.
#' @param factor          if \code{method = 'factor'}, a numeric value between 0 and 1 specifying the weighting factor for the results of the sample equivalent to the prior information.
#' 
#' @details This section elaborates on the available options for the \code{method} argument.
#'
#' \itemize{
#'  \item{\code{none}:              This method constructs a prior distribution that incorporates negligible information about the possible values of the misstatement.}
#'  \item{\code{arm}:               This method constructs a prior distribution by translating the risks of material misstatement (inherent risk and internal control risk) from the audit risk model to an implicit sample. The method requires specification of the \code{ir} (inherent risk) and \code{cr} (internal control risk) arguments.}
#'  \item{\code{bram}:              This method constructs a prior distribution using the Bayesian audit risk assessment model (BRAM) in which the expected most likely error and expected upper bound of the misstatement must be specified. The method requires specification of the \code{ub} argument.}
#'  \item{\code{median}:            This method constructs a prior distribution so that the prior probability of tolerable misstatement (\eqn{\theta <} materiality) is equal to the prior probability of intolerable misstatement (\eqn{\theta >} materiality).}
#'  \item{\code{hypotheses}:        This method constructs a prior distribution with custom prior probabilities for the hypotheses of tolerable misstatement (\eqn{\theta <} materiality) and intolerable misstatement (\eqn{\theta >} materiality). This method requires specification of the \code{pHmin} and \code{pHplus} arguments.}
#'  \item{\code{sample}:            This method constructs a prior distribution on the basis of an earlier sample. This method requires specification of the \code{sampleN} and \code{sampleK} arguments.}
#'  \item{\code{factor}:            This method constructs a prior distribution on the basis of an earlier sample in combination with a weighting factor. This method requires specification of the \code{sampleN}, \code{sampleK}, and \code{factor} arguments.}
#' }
#'
#' @details This section elaborates on the available likelihoods and corresponding prior distributions for the \code{likelihood} argument.
#' 
#' \itemize{
#'  \item{\code{binomial}:         The binomial likelihood is often used as a likelihood for attributes sampling \emph{with} replacement. The likelihood function is defined as: \deqn{p(x) = {n \choose k} p^k (1 - p)^{n - k}} The conjugate \emph{beta(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{1}{B(\alpha, \beta)} x^{\alpha - 1} (1 - x)^{\beta - 1}}}
#'  \item{\code{poisson}:          The Poisson likelihood is often used as a likelihood for monetary unit sampling (MUS). The likelihood function is defined as: \deqn{p(x) = \frac{\lambda^x e^{-\lambda}}{x!}} The conjugate \emph{gamma(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{\beta^\alpha x^{\alpha - 1} e^{-\beta x}}{\Gamma(\alpha)}}}
#'  \item{\code{hypergeometric}:   The hypergeometric likelihood is used as a likelihood for sampling \emph{without} replacement. The likelihood function is defined as: \deqn{p(x = k) = \frac{{K \choose k} {N - K \choose n - k}}{{N \choose n}}} The conjugate \emph{beta-binomial(\eqn{\alpha, \beta})} prior (Dyer and Pierce, 1993) has probability density function: \deqn{f(k | n, \alpha, \beta) = {n \choose k} \frac{B(k + \alpha, n - k + \beta)}{B(\alpha, \beta)}} }
#' }
#'
#' @return An object of class \code{jfaPrior} containing:
#' 
#' \item{confidence}{a numeric value between 0 and 1 indicating the confidence level used.}
#' \item{materiality}{if \code{materiality} is specified, a numeric value between 0 and 1 indicating the materiality used to construct the prior distribution.}
#' \item{expectedError}{a numeric value larger than, or equal to, 0 indicating the input for the number of expected errors.}
#' \item{method}{a character indicating the method by which the prior distribution is constructed.}
#' \item{likelihood}{a character indicating the assumed likelihood.}
#' \item{N}{if \code{N} is specified, an integer larger than 0 indicating the population size.}
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
#' # Translate inherent risk (ir) and control risk (cr) to a prior distribution
#' auditPrior(confidence = 0.95, materiality = 0.05, expectedError = 0.025,
#'            method = 'arm', likelihood = 'binomial', ir = 1, cr = 0.6)
#' @export

auditPrior <- function(confidence, materiality = NULL, expectedError = 0, 
                       method = 'none', likelihood = 'binomial', N = NULL, 
                       ir = 1, cr = 1, ub = NULL, pHmin = NULL, pHplus = NULL, 
                       sampleN = 0, sampleK = 0, factor = 1) {
  
  if (confidence >= 1 || confidence <= 0 || is.null(confidence)) # Check if the confidence has a valid input
    stop("The value for the confidence must be between 0 and 1.")
  
  if (!(likelihood %in% c("poisson", "binomial", "hypergeometric"))) # Check if the likelihood has a valid input
    stop("Specify a valid likelihood. Possible options are 'binomial', 'poisson', and 'hypergeometric'.")
  
  if (!(method %in% c("none", "median", "hypotheses", "arm", "bram", "sample", "factor"))) # Check if the method has a valid input
    stop("Currently only method = 'none', 'median', 'hypotheses', 'arm', 'bram', 'sample', and 'factor' are supported")
  
  if (is.null(materiality) && method %in% c("median", "hypotheses", "arm")) # Materiality is required for these methods
    stop("The methods 'arm', 'median', and 'hypotheses' require that you specify a value for the materiality.")
  
  if (likelihood == "hypergeometric" && (is.null(N) || N <= 0)) # Check if N is specified if hypergeometric is specified
    stop("The hypergeometric likelihood requires that you specify a positive value for the populatin size N.")
  
  if (expectedError < 0) # Check if the expected errors has a valid input
    stop("The expected errors must be zero or larger than zero.")
  
  if (!is.null(materiality) && expectedError >= materiality && expectedError < 1) # Check if the expected errors do not exceed the materiality
    stop("The expected errors must be lower than the materiality.")
  
  if (expectedError >= 1 && method != 'none') # Check if the expected errors are consistent with the method
    stop("The expected errors must be entered as a proportion for this method.")
  
  if (method == "none") { # Method 1: Negligible prior information
    
    nPrior <- 0 # No earlier observations
    kPrior <- 0 # No earlier errors
    
  } else if (method == "arm") { # Method 2: Translate the risks from the audit risk model
    
    if (is.null(ir) || is.null(cr) || is.null(materiality)) # Check if ir and cr have valid inputs
      stop("Method = 'arm' requires non-null 'materiality', 'ir', and 'cr' arguments.")
    
    alpha  <- (1 - confidence) / (ir * cr) # Calculate the required detection risk from the audit risk model
    usedPopSize <- if (likelihood == "hypergeometric") N else NULL
    nPlus  <- planning(confidence = confidence, likelihood = likelihood, expectedError = expectedError, N = usedPopSize, materiality = materiality, prior = TRUE)$sampleSize # Calculate the sample size for the full detection risk  
    nMin   <- planning(confidence = 1 - alpha, likelihood = likelihood, expectedError = expectedError, N = usedPopSize, materiality = materiality, prior = TRUE)$sampleSize # Calculated the sample size for the adjusted detection risk
    nPrior <- nPlus - nMin # Calculate the sample size equivalent to the increase in detection risk
    kPrior <- (nPlus * expectedError) - (nMin * expectedError) # Calculate errors equivalent to the increase in detection risk
    
  } else if (method == 'bram') { # Method 3: Bayesian risk assessment model
    
    if (is.null(ub)) # Check if the value for the upper bound is present
      stop("You must specify a value for 'ub'.")
    
    if (ub <= 0 || ub >= 1 || ub <= expectedError) # Check if the value for the upper bound is valid
      stop("The value of 'ub' must be between 0 and 1 and higher than 'expectedErrors'.")
    
    if (likelihood == 'poisson' && expectedError > 0) { # Perform approximation described in Stewart (2013) on p. 45.
      
      r      <- expectedError / ub
      q      <- stats::qnorm(confidence)
      kPrior <- ((( (q * r) + sqrt(3 + ((r / 3) * (4 * q^2 - 10)) - ((r^2 / 3) * (q^2 - 1) ))) / (2 * (1 - r) ))^2 + (1 / 4)) - 1
      nPrior <- kPrior / expectedError
      
    } else { # Approximation through iteration over one of the parameters
      
      bound  <- Inf
      kPrior <- 0
      nPrior <- 0
      
      while (bound > ub) {
        
        if (expectedError == 0) { # In this case, iterate over nPrior because kPrior is zero
          nPrior <- nPrior + 0.001 # Increase of 0.001 (time intensive?)
        } else { # In this case, iterate over kPrior to save computation time
          kPrior <- kPrior + 0.0001 # Increase of 0.0001 (time intensive?)
          nPrior <- kPrior / expectedError
        }
        
        bound <- switch(likelihood, 
                        "binomial" = stats::qbeta(p = confidence, shape1 = 1 + kPrior, shape2 = 1 + nPrior - kPrior),
                        "poisson" = stats::qgamma(p = confidence, shape = 1 + kPrior, rate = nPrior),
                        "hypergeometric" = .qBetaBinom(p = confidence, N = N, shape1 = 1 + kPrior, shape2 = 1 + nPrior - kPrior) / N)
      }
      
    }
  } else if (method == "median" || method == "hypotheses") {
    
    if(method == "median") { # Method 4: Equal prior probabilities
      
      probH0 <- probH1 <- 0.5
      
    } else if (method == "hypotheses") { # Method 5: Custom prior probabilities
      
      if (is.null(pHmin) && is.null(pHplus)) # Must have the prior probabilities and materiality
        stop("Method = 'hypotheses' requires non-null 'pHmin' or 'pHplus' arguments.")
      
      if ((!is.null(pHmin) && !is.null(pHplus)) && pHmin + pHplus != 1) # Check for valid prior probabilities
        stop("The values for 'pHmin' and 'pHplus' should sum to one.")  
      
      if (!is.null(pHmin) && is.null(pHplus)) { # Adjust p(H1) for user input
        probH1 <- 1 - pHmin
      } else {
        probH1 <- pHplus
      }
      
      probH0 <- 1 - probH1 # Calculate p(H0)
    }
    
    if (expectedError == 0) { # Formulas for zero expected errors
      
      nPrior <- switch(likelihood,
                       "poisson" = -(log(probH1) / materiality),
                       "binomial" = log(probH1) / log(1 - materiality) - 1,
                       "hypergeometric" = log(probH1) / log(1 - materiality) - 1)
      kPrior <- 0
      
    } else { # Approximation through iteration over alpha parameter = more accurate than approximation through formulas
      
      median <- Inf
      kPrior <- 0
      
      while (median > materiality) {
        kPrior <- kPrior + 0.0001 # Increase of 0.0001 (time intensive?)
        nPrior <- kPrior / expectedError # Express beta in terms of alpha
        median <- switch(likelihood, # Calculate the median for the current parameters
                         "binomial" = stats::qbeta(p = probH0, shape1 = 1 + kPrior, shape2 = 1 + nPrior - kPrior),
                         "poisson" = stats::qgamma(p = probH0, shape = 1 + kPrior, rate = nPrior),
                         "hypergeometric" = .qBetaBinom(p = probH0, N = N, shape1 = 1 + kPrior, shape2 = 1 + nPrior - kPrior) / N)
      }
    }
    
  } else if (method == "sample") { # Method 6: Earlier sample
    
    if (is.null(sampleN) || is.null(sampleK)) # Check for valid arguments
      stop("Method = 'sample' requires non-null 'sampleN', and 'sampleK' arguments.")
    nPrior <- sampleN # Earlier sample size
    kPrior <- sampleK # Earlier errors
    
  } else if (method == "factor") { # Method 7: Weighted earlier sample
    
    if (is.null(sampleN) || is.null(sampleK) || is.null(factor)) # Check for valid arguments
      stop("Method = 'factor' requires non-null 'factor', 'sampleN', and 'sampleN=K' arguments.")  
    nPrior <- sampleN * factor # Earlier sample size
    kPrior <- sampleK * factor # Earlier errors
    
  }
  
  # Create the main result object	
  result <- list()
  result[["confidence"]]            <- as.numeric(confidence)
  if (!is.null(materiality))
    result[["materiality"]]         <- as.numeric(materiality)
  result[["expectedError"]]         <- as.numeric(expectedError)
  result[["method"]]                <- as.character(method)
  result[["likelihood"]]            <- as.character(likelihood)
  result[["N"]]                     <- as.numeric(N)
  
  # Create the description section
  result[["description"]]           <- list()
  result[["description"]]$density   <- switch(likelihood, "poisson" = "gamma", "binomial" = "beta", "hypergeometric" = "beta-binomial")
  result[["description"]]$implicitk <- as.numeric(kPrior)
  result[["description"]]$implicitn <- as.numeric(nPrior)
  result[["description"]]$alpha     <- as.numeric(1 + kPrior)
  result[["description"]]$beta      <- switch(likelihood, "poisson" = nPrior, "binomial" = 1 + nPrior - kPrior, "hypergeometric" = 1 + nPrior - kPrior)
  
  # Create the statistics section
  result[["statistics"]]            <- list()
  result[["statistics"]]$mode       <- switch(likelihood, 
                                              "poisson" = (result[["description"]]$alpha - 1) / result[["description"]]$beta,
                                              "binomial" = (result[["description"]]$alpha - 1) / (result[["description"]]$alpha + result[["description"]]$beta - 2),
                                              "hypergeometric" = .modeBetaBinom(N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta))
  result[["statistics"]]$mean       <- switch(likelihood, 
                                              "poisson" = result[["description"]]$alpha / result[["description"]]$beta,
                                              "binomial" = result[["description"]]$alpha / (result[["description"]]$alpha + result[["description"]]$beta),
                                              "hypergeometric" = result[["description"]]$alpha / (result[["description"]]$alpha + result[["description"]]$beta) * result[["N"]])
  result[["statistics"]]$median     <- switch(likelihood, 
                                              "poisson" = stats::qgamma(0.5, shape = result[["description"]]$alpha, rate = result[["description"]]$beta),
                                              "binomial" = stats::qbeta(0.5, shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta),
                                              "hypergeometric" = .qBetaBinom(0.5, N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta))
  result[["statistics"]]$ub         <- switch(likelihood, 
                                              "poisson" = stats::qgamma(confidence, shape = result[["description"]]$alpha, rate = result[["description"]]$beta),
                                              "binomial" = stats::qbeta(confidence, shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta),
                                              "hypergeometric" = .qBetaBinom(confidence, N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta))									
  result[["statistics"]]$precision  <- result[["statistics"]]$ub - result[["statistics"]]$mode
  
  # Create the specifics section
  if (method != "none")
    result[["specifics"]]           <- list()
  if (method == "median" || method == "hypotheses") {
    result[["specifics"]]$pHmin     <- as.numeric(probH0)
    result[["specifics"]]$pHplus    <- as.numeric(probH1)
  } else if (method == "sample" || method == "factor") {
    result[["specifics"]]$sampleN   <- as.numeric(sampleN)
    result[["specifics"]]$sampleK   <- as.numeric(sampleK)
    result[["specifics"]]$factor    <- as.numeric(factor)    
  } else if (method == "arm") {
    result[["specifics"]]$ir        <- as.numeric(ir)
    result[["specifics"]]$cr        <- as.numeric(cr)  
  } else if (method == "bram") {
    result[["specifics"]]$mode      <- result[["expectedError"]]
    result[["specifics"]]$ub        <- as.numeric(ub)
  }
  
  # Create the hypotheses section
  if (!is.null(result[["materiality"]])) {
    result[["hypotheses"]]              <- list()
    result[["hypotheses"]]$hypotheses   <- c(paste0("H-: \u0398 < ", materiality), paste0("H+: \u0398 > ", materiality))
    result[["hypotheses"]]$pHmin        <- .restrictprob(switch(likelihood, 
                                                                "poisson" = stats::pgamma(materiality, shape = result[["description"]]$alpha, rate = result[["description"]]$beta),
                                                                "binomial" = stats::pbeta(materiality, shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta),
                                                                "hypergeometric" = .pBetaBinom(ceiling(materiality * result[["N"]]), N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta)))
    result[["hypotheses"]]$pHplus       <- .restrictprob(switch(likelihood, 
                                                                "poisson" = stats::pgamma(materiality, shape = result[["description"]]$alpha, rate = result[["description"]]$beta, lower.tail = FALSE),
                                                                "binomial" = stats::pbeta(materiality, shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta, lower.tail = FALSE),
                                                                "hypergeometric" = .pBetaBinom(ceiling(materiality * result[["N"]]), N = result[["N"]], shape1 = result[["description"]]$alpha, shape2 = result[["description"]]$beta, lower.tail = FALSE)))
    result[["hypotheses"]]$oddsHmin     <- result[["hypotheses"]]$pHmin / result[["hypotheses"]]$pHplus
    result[["hypotheses"]]$oddsHplus    <- 1 / result[["hypotheses"]]$oddsHmin
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