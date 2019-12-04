#' Evaluation of Audit Samples using Confidence / Credible Bounds
#'
#' @description This function takes a sample data frame or summary statistics about an evaluated audit sample and calculates a confidence bound accordint to a specified method. The returned object is of class \code{jfaEvaluation} and can be used with associated \code{print()} method.
#'
#' @usage evaluation(sample = NULL, bookValues = NULL, auditValues = NULL, 
#'                   confidence = 0.95, nSumstats = NULL, kSumstats = NULL,
#'                   method = "binomial", materiality = NULL, N = NULL, 
#'                   prior = FALSE, nPrior = 0, kPrior = 0, 
#'                   rohrbachDelta = 2.7, momentPoptype = "accounts")
#'
#' @param sample        a data frame containing at least a column of book values and a column of audit (true) values.
#' @param bookValues    the column name for the book values in the sample.
#' @param auditValues   the column name for the audit (true) values in the sample.
#' @param confidence    the required confidence level for the bound.
#' @param nSumstats     the number of observations in the sample. If specified, overrides the \code{sample} and \code{bookValues} arguments and assumes that the data comes from summary statistics specified by \code{nSumstats} and \code{kSumstats}.
#' @param kSumstats     the sum of the errors found in the sample. If specified, overrides the \code{sample} and \code{bookValues} arguments and assumes that the data comes from summary statistics specified by \code{kSumstats} and \code{nSumstats}.
#' @param method        can be either one of \code{poisson}, \code{binomial}, \code{hypergeometric}, \code{stringer}, \code{stringer-meikle}, \code{stringer-lta}, \code{stringer-pvz}, \code{rohrbach}, \code{moment}. 
#' @param materiality   if specified, the function also returns the conclusion of the analysis with respect to the materiality. This value must be specified as a fraction of the total value of the population (a value between 0 and 1).
#' @param N             the total population size.
#' @param prior         whether to use a prior distribution when evaluating. Defaults to \code{FALSE} for frequentist evaluation. If \code{TRUE}, the prior distribution is updated by the specified likelihood. Chooses a conjugate gamma distribution for the Poisson likelihood, a conjugate beta distribution for the binomial likelihood, and a conjugate beta-binomial distribution for the hypergeometric likelihood.
#' @param nPrior        the prior parameter \eqn{\alpha} (number of errors in the assumed prior sample).
#' @param kPrior        the prior parameter \eqn{\beta} (total number of observations in the assumed prior sample).
#' @param rohrbachDelta the value of \eqn{\Delta} in Rohrbach's augmented variance bound.
#' @param momentPoptype can be either one of \code{accounts} or \code{inventory}. Options result in different methods for calculating the central moments, for more information see Dworin and Grimlund (1986)
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
#' }
#' 
#' @references Dworin, L., & Grimlund, R. A. (1986). Dollar-unit sampling: A comparison of the quasi-Bayesian and moment bounds. \emph{Accounting Review}, 36-57.
#' @references Leslie, D. A., Teitlebaum, A. D., & Anderson, R. J. (1979). \emph{Dollar-unit sampling: a practical guide for auditors}. Copp Clark Pitman; Belmont, Calif.: distributed by Fearon-Pitman.
#' @references Meikle, G. R. (1972). \emph{Statistical Sampling in an Audit Context: An Audit Technique}. Canadian Institute of Chartered Accountants.
#' @references Pap, G., & van Zuijlen, M. C. (1996). On the asymptotic behaviour of the Stringer bound 1. \emph{Statistica Neerlandica}, 50(3), 367-389.
#' @references Rohrbach, K. J. (1993). Variance augmentation to achieve nominal coverage probability in sampling from audit populations. \emph{Auditing}, 12(2), 79.
#' @references Stringer, K. W. (1963). Practical aspects of statistical sampling in auditing. \emph{In Proceedings of the Business and Economic Statistics Section} (pp. 405-411). American Statistical Association.
#'
#' @return An object of class \code{jfaEvaluation} containing:
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{planning}} \code{\link{sampling}}
#'
#' @examples
#' 
#' library(jfa)
#' 
#' # Generate some audit data (N = 1000)
#' set.seed(1)
#' data <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), 
#'                   bookValue = runif(n = 1000, min = 1000, max = 10000))
#'
#' # Using the binomial likelihood, calculates the upper 95% confidence bound for a 
#' # materiality of 5% when 1% full errors are found in a sample (n = 93).
#' jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.01, 
#'                    likelihood = "binomial")
#'
#' # Using monetary unit sampling, draw a random sample from the population.
#' samp <- sampling(population = data, sampleSize = jfaRes, units = "mus", 
#'                  bookValues = "bookValue", algorithm = "random")
#'
#' samp$sample$trueValue <- samp$sample$bookValue
#' samp$sample$trueValue[2] <- 1561.871 - 500 # One overstatement is found
#'
#' # Evaluate the sample using the stringer bound.
#' evaluation(sample = samp$sample, bookValues = "bookValue", auditValues = "trueValue", 
#'            method = "stringer", materiality = 0.05)
#'
#' # Evaluate the sample using summary statistics (n = 93, k = 1).
#' evaluation(nSumstats = 93, kSumstats = 1, method = "binomial", materiality = 0.05)
#' 
#' @keywords evaluation confidence bound
#'
#' @export 

evaluation <- function(sample = NULL, bookValues = NULL, auditValues = NULL, 
                       confidence = 0.95, nSumstats = NULL, kSumstats = NULL,
                       method = "binomial", materiality = NULL, N = NULL, 
                       prior = FALSE, nPrior = 0, kPrior = 0, 
                       rohrbachDelta = 2.7, momentPoptype = "accounts"){
  if(!(method %in% c("poisson", "binomial", "hypergeometric", "stringer", "stringer-meikle", "stringer-lta", "stringer-pvz",
                     "rohrbach", "moment")) || length(method) != 1)
    stop("Specify a valid method for the confidence bound")
  if(!is.null(nSumstats) || !is.null(kSumstats)){
     if(is.null(nSumstats) || is.null(kSumstats))
       stop("When using summary statistics, both nSumstats and kSumstats must be defined")
      if(nSumstats <= 0 || kSumstats < 0)
        stop("When using summary statistics, both nSumstats and kSumstats must be positive")
    if(length(nSumstats) != 1 || length(kSumstats) != 1)
      stop("Specify one value for nSumstat and kSumstat")
    if(kSumstats > nSumstats)
      stop("The sum of the errors is higher than the sample size")
    if(method %in% c("stringer", "stringer-meikle", "stringer-lta", "stringer-pvz",
                     "rohrbach", "moment"))
      stop("The selected method requires raw observations, and does not accomodate summary statistics")
    
    n <- nSumstats
    k <- kSumstats
    t <- kSumstats
    
  } else if(!is.null(sample)){
    if(is.null(bookValues) || is.null(auditValues) || length(bookValues) != 1 || length(auditValues) != 1)
      stop("Specify a valid book value column and audit value column when using a sample")
    
    sample <- stats::na.omit(sample)
    n <- nrow(sample)
    taints <- (sample[, bookValues] - sample[, auditValues]) / sample[, bookValues]
    k <- length(which(taints != 0))
    t <- sum(taints)
  }
  
  if(!is.null(materiality)){
    mat <- materiality
  } else {
    mat <- 0
  }
  
  if(method == "poisson"){
    if(prior){
      bound <- stats::qgamma(p = confidence, shape = 1 + kPrior + t, rate = 1 + nPrior + n)
    } else {
      bound <- stats::poisson.test(x = k, T = n, r = mat, alternative = "less", conf.level = confidence)$conf.int[2]
    }
  } else if(method == "binomial"){
    if(prior){
      bound <- stats::qbeta(p = confidence, shape1 = 1 + kPrior + t, shape2 = 1 + nPrior - kPrior + n - t)
    } else {
      bound <- stats::binom.test(x = k, n = n, p = mat, alternative = "less", conf.level = confidence)$conf.int[2]
    }
  } else if(method == "hypergeometric"){
    if(prior){
      if(is.null(N))
        stop("The beta-binomial distribution requires that you specify the population size N")
      bound <- .qBetaBinom(p = confidence, N = N - n, shape1 = 1 + kPrior + k, shape2 = 1 + nPrior - kPrior + n - k) / N
    } else {
      if(mat == 0)
        stop("The hypergeometric distribution requires that you specify the materiality")
      populationK <- materiality * N
      bound <- stats::phyper(q = k, m = populationK, n = N - populationK, k = n)
    }
  } else if(method == "stringer"){
    bound <- .stringerBound(taints, confidence, n)
  } else if(method == "stringer-meikle"){
    bound <- .stringerBound(taints, confidence, n, correction = "meikle")
  } else if(method == "stringer-lta"){
    bound <- .stringerBound(taints, confidence, n, correction = "lta")
  } else if(method == "stringer-pvz"){
    bound <- .stringerBound(taints, confidence, n, correction = "pvz")
  } else if(method == "rohrbach"){
    bound <- .rohrbachBound(taints, confidence, n, N, rohrbachDelta = rohrbachDelta)
  } else if(method == "moment"){
    bound <- .momentBound(taints, confidence, n, momentPoptype = momentPoptype)
  }
  
  results <- list()
  results[["n"]] <- n
  results[["k"]] <- k
  results[["confidence"]] <- confidence
  results[["confBound"]] <- bound
  results[["method"]] <- method
  if(!is.null(materiality)){
    results[["materiality"]] <- materiality
    results[["conclusion"]] <- ifelse(bound < materiality, yes = "Approve population", no = "Do not approve population")
  }
  class(results) <- "jfaEvaluation"
  return(results)
}
