#' Evaluation of Audit Samples using Confidence / Credible Bounds
#'
#' @description This function takes a sample data frame or summary statistics about an evaluated audit sample and calculates a confidence bound according to a specified method. The returned object is of class \code{jfaEvaluation} and can be used with associated \code{print()} and \code{plot()} methods.
#'
#' @usage evaluation(sample = NULL, bookValues = NULL, auditValues = NULL, 
#'            confidence = 0.95, nSumstats = NULL, kSumstats = NULL,
#'            method = "binomial", materiality = NULL, N = NULL, 
#'            prior = FALSE, nPrior = 0, kPrior = 0, 
#'            rohrbachDelta = 2.7, momentPoptype = "accounts",
#'            populationBookValue = NULL, 
#'            csA = 1, csB = 3, csMu = 0.5) 
#'
#' @param sample        a data frame containing at least a column of book values and a column of audit (true) values.
#' @param bookValues    the column name for the book values in the sample.
#' @param auditValues   the column name for the audit (true) values in the sample.
#' @param confidence    the required confidence level for the bound.
#' @param nSumstats     the number of observations in the sample. If specified, overrides the \code{sample}, \code{bookValues} and \code{auditValues} arguments and assumes that the data comes from summary statistics specified by \code{nSumstats} and \code{kSumstats}.
#' @param kSumstats     the sum of the errors found in the sample. If specified, overrides the \code{sample}, \code{bookValues} and \code{auditValues} arguments and assumes that the data comes from summary statistics specified by \code{kSumstats} and \code{nSumstats}.
#' @param method        can be either one of \code{poisson}, \code{binomial}, \code{hypergeometric}, \code{stringer}, \code{stringer-meikle}, \code{stringer-lta}, \code{stringer-pvz}, \code{rohrbach}, \code{moment}, \code{direct}, \code{difference}, \code{quotient}, or \code{regression}. 
#' @param materiality   if specified, the function also returns the conclusion of the analysis with respect to the materiality. This value must be specified as a fraction of the total value of the population (a value between 0 and 1). The value is discarded when \code{direct}, \code{difference}, \code{quotient}, or \code{regression} method is chosen.
#' @param N             the total population size.
#' @param prior         whether to use a prior distribution when evaluating. Defaults to \code{FALSE} for frequentist evaluation. If \code{TRUE}, the prior distribution is updated by the specified likelihood. Chooses a conjugate gamma distribution for the Poisson likelihood, a conjugate beta distribution for the binomial likelihood, and a conjugate beta-binomial distribution for the hypergeometric likelihood.
#' @param nPrior        the prior parameter \eqn{\alpha} (number of errors in the assumed prior sample).
#' @param kPrior        the prior parameter \eqn{\beta} (total number of observations in the assumed prior sample).
#' @param rohrbachDelta the value of \eqn{\Delta} in Rohrbach's augmented variance bound.
#' @param momentPoptype can be either one of \code{accounts} or \code{inventory}. Options result in different methods for calculating the central moments, for more information see Dworin and Grimlund (1986).
#' @param populationBookValue the total value of the audit population. Required when \code{method} is one of \code{direct}, \code{difference}, \code{quotient}, or \code{regression}.
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
#' \item{n}{the sample size.}
#' \item{k}{an integer specifying the number of observed errors.}
#' \item{t}{a number specifying the sum of observed taints.}
#' \item{confidence}{the confidence level of the result.}
#' \item{popBookvalue}{if specified as input, the total book value of the population.}
#' \item{pointEstimate}{if method is one of \code{direct}, \code{difference}, \code{quotient}, or \code{regression}, the value of the point estimate.}
#' \item{lowerBound}{if method is one of \code{direct}, \code{difference}, \code{quotient}, or \code{regression}, the value of the lower bound of the interval.}
#' \item{upperBound}{if method is one of \code{direct}, \code{difference}, \code{quotient}, or \code{regression}, the value of the upper bound of the interval.}
#' \item{confBound}{the upper confidence bound on the error percentage. }
#' \item{method}{the evaluation method that was used.}
#' \item{materiality}{the materiality.}
#' \item{conclusion}{if \code{materiality} is specified, the conclusion about whether to approve or not approve the population.}
#' \item{N}{if specified as input, the population size.}
#' \item{populationK}{the assumed total errors in the population. Used for inferences with \code{hypergeometric} method.}
#' \item{prior}{a logical, indicating whether a prior was used in the analysis.}
#' \item{nPrior}{if a prior is specified, the prior assumed sample size.}
#' \item{kPrior}{if a prior is specified, the prior assumed sample errors.}
#' \item{multiplicationFactor}{if \code{method = "coxsnell"}, the multiplication factor for the \emph{F}-distribution.}
#' \item{df1}{if \code{method = "coxsnell"}, the df1 for the \emph{F}-distribution.}
#' \item{df2}{if \code{method = "coxsnell"}, the df2 for the \emph{F}-distribution.}
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
#' # jfa evaluation results for binomial method:
#' #   
#' # Materiality:           5% 
#' # Confidence:            95% 
#' # Upper bound:           4.656% 
#' # Sample size:           100 
#' # Sample errors:         1 
#' # Sum of taints:         1  
#' # Conclusion:            Approve population
#' 
#' # Evaluate the raw sample using the stringer bound:
#' 
#' e2 <- evaluation(sample = s1_sample, bookValues = "bookValue", auditValues = "trueValue", 
#'                  method = "stringer", materiality = 0.05)
#' print(e2)
#' 
#' # jfa evaluation results for stringer method:
#' #   
#' # Materiality:           5% 
#' # Confidence:            95% 
#' # Upper bound:           3.952% 
#' # Sample size:           100 
#' # Sample errors:         1 
#' # Sum of taints:         0.587  
#' # Conclusion:            Approve population
#' 
#' @keywords evaluation confidence bound audit
#'
#' @export 

evaluation <- function(sample = NULL, bookValues = NULL, auditValues = NULL, 
                       confidence = 0.95, nSumstats = NULL, kSumstats = NULL,
                       method = "binomial", materiality = NULL, N = NULL, 
                       prior = FALSE, nPrior = 0, kPrior = 0, 
                       rohrbachDelta = 2.7, momentPoptype = "accounts",
                       populationBookValue = NULL, 
                       csA = 1, csB = 3, csMu = 0.5){
  
  tmp_method <- method
  
  if(class(prior) == "jfaPrior"){
    nPrior <- prior$nPrior
    kPrior <- prior$kPrior
    method <- prior$likelihood
  }
  
  if(tmp_method == "coxsnell")
    method <- "coxsnell"
  
  if(!(method %in% c("poisson", "binomial", "hypergeometric", "stringer", "stringer-meikle", "stringer-lta", "stringer-pvz",
                     "rohrbach", "moment", "coxsnell", "direct", "difference", "quotient", "regression")) || length(method) != 1)
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
    if(method %in% c("stringer", "stringer-meikle", "stringer-lta", "stringer-pvz", "coxsnell",
                     "rohrbach", "moment", "direct", "difference", "quotient", "regression"))
      stop("The selected method requires raw observations, and does not accomodate summary statistics")
    
    n <- nSumstats
    k <- kSumstats
    t <- kSumstats
    
  } else if(!is.null(sample)){
    if(is.null(bookValues) || is.null(auditValues) || length(bookValues) != 1 || length(auditValues) != 1)
      stop("Specify a valid book value column and audit value column when using a sample")
    
    sample <- stats::na.omit(sample)
    n <- nrow(sample)
    bv <- sample[, bookValues]
    av <- sample[, auditValues]
    taints <- (bv - av) / bv
    k <- length(which(taints != 0))
    t <- sum(taints)
  }
  
  if(!is.null(materiality)){
    mat <- materiality
  } else {
    mat <- 0
  }
  
  if((class(prior) == "logical" && prior == TRUE) && kPrior < 0 || nPrior < 0)
    stop("When you specify a prior, both kPrior and nPrior should be higher than zero")
  
  if(method == "poisson"){
    if((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior"){
      bound <- stats::qgamma(p = confidence, shape = 1 + kPrior + t, rate = 1 + nPrior + n)
    } else {
      bound <- stats::poisson.test(x = k, T = n, r = mat, alternative = "less", conf.level = confidence)$conf.int[2]
    }
  } else if(method == "binomial"){
    if((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior"){
      bound <- stats::qbeta(p = confidence, shape1 = 1 + kPrior + t, shape2 = 1 + nPrior - kPrior + n - t)
    } else {
      bound <- stats::binom.test(x = k, n = n, p = mat, alternative = "less", conf.level = confidence)$conf.int[2]
    }
  } else if(method == "hypergeometric"){
    if((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior"){
      if(is.null(N))
        stop("Evaluation with beta-binomial distribution requires that you specify the population size N")
      bound <- .qBetaBinom(p = confidence, N = N, shape1 = 1 + kPrior + k, shape2 = 1 + nPrior - kPrior + n - k) / N
    } else {
      if(mat == 0)
        stop("Evaluation with the hypergeometric distribution requires that you specify the materiality")
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
  } else if(method == "coxsnell"){
    bound <- .coxAndSnellBound(taints, confidence, n, csA, csB, csMu, aPrior = 1 + kPrior, bPrior = 1 + nPrior - kPrior)
  } else if(method == "direct"){
    bound <- .directMethod(bv, av, confidence, N, n, populationBookValue)
  } else if(method == "difference"){
    bound <- .differenceMethod(bv, av, confidence, N, n, populationBookValue)
  } else if(method == "quotient"){
    bound <- .quotientMethod(bv, av, confidence, N, n, populationBookValue)
  } else if(method == "regression"){
    bound <- .regressionMethod(bv, av, confidence, N, n, populationBookValue)
  }
  
  results <- list()
  results[["n"]]              <- as.numeric(n)
  results[["k"]]              <- as.numeric(k)
  results[["t"]]              <- as.numeric(t)
  results[["confidence"]]     <- as.numeric(confidence)
  results[["method"]]         <- as.character(method)
  
  if(method %in% c("direct", "difference", "quotient", "regression")){
    results[["popBookvalue"]]   <- as.numeric(populationBookValue)
    results[["pointEstimate"]]  <- as.numeric(bound$pointEstimate)
    results[["lowerBound"]]     <- as.numeric(bound$lowerBound)
    results[["upperBound"]]     <- as.numeric(bound$upperBound)
  } else {
    if(method == "coxsnell"){
      results[["confBound"]]            <- as.numeric(bound$bound)
      results[["multiplicationFactor"]] <- as.numeric(bound$multiplicationFactor)
      results[["df1"]]                  <- as.numeric(bound$df1)
      results[["df2"]]                  <- as.numeric(bound$df2)
    } else {
      results[["confBound"]]            <- as.numeric(bound) 
    }
  }
  if(!is.null(materiality)){
    results[["materiality"]]      <- as.numeric(materiality)
    if(method %in% c("direct", "difference", "quotient", "regression")){
      results[["conclusion"]]     <- ifelse(populationBookValue <= results[["upperBound"]] && populationBookValue >= results[["lowerBound"]] , yes = "Approve population", no = "Do not approve population")
    } else {
      results[["conclusion"]]     <- ifelse(results[["confBound"]] < materiality, yes = "Approve population", no = "Do not approve population")
    }
  }
  if(method == "hypergeometric"){
    results[["N"]]                <- N
    if(!prior)
      results[["populationK"]]    <- populationK
  }
  if((class(prior) == "logical" && prior == TRUE) || class(prior) == "jfaPrior" || method == "coxsnell"){
    results[["prior"]]            <- as.logical(TRUE)
    results[["nPrior"]]           <- nPrior
    results[["kPrior"]]           <- kPrior
  } else {
    results[["prior"]]            <- as.logical(FALSE) 
  }
  class(results)                  <- "jfaEvaluation"
  return(results)
}
