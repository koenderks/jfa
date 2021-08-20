#' Evaluate a statistical audit sample
#'
#' @description This function takes a data frame (using \code{sample}, \code{bookValues}, and \code{auditValues}) or summary statistics (using \code{nSumstats} and \code{kSumstats}) and performs inference on the misstatement in the sample. The function returns an object of class \code{jfaEvaluation} which can be used with associated \code{summary()} and \code{plot()} methods.
#'
#' For more details on how to use this function, see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage evaluation(materiality = NULL, minPrecision = NULL, method = 'binomial',
#'            confidence = 0.95, sample = NULL, bookValues = NULL, auditValues = NULL, 
#'            counts = NULL, nSumstats = NULL, kSumstats = NULL,
#'            N = NULL, populationBookValue = NULL,
#'            prior = FALSE, nPrior = 0, kPrior = 0, 
#'            rohrbachDelta = 2.7, momentPoptype = 'accounts',
#'            csA = 1, csB = 3, csMu = 0.5) 
#'
#' @param materiality   a numeric value between 0 and 1 specifying the performance materiality (maximum tolerable error) as a fraction of the total size of the population. If specified, the function also returns the conclusion of the analysis with respect to the performance materiality. The value is discarded when \code{direct}, \code{difference}, \code{quotient}, or \code{regression} method is chosen.
#' @param minPrecision  a numeric value between 0 and 1 specifying the required minimum precision (upper bound minus most likely error) as a fraction of the total size of the population. If specified, the function also returns the conclusion of the analysis with respect to the required minimum precision.
#' @param method        a character specifying the method to be used in the evaluation. Possible options are \code{poisson}, \code{binomial} (default), \code{hypergeometric}, \code{mpu}, \code{stringer}, \code{stringer-meikle}, \code{stringer-lta}, \code{stringer-pvz}, \code{rohrbach}, \code{moment}, \code{direct}, \code{difference}, \code{quotient}, or \code{regression}. See the details section for more information.
#' @param confidence   	a numeric value between 0 and 1 specifying the confidence level used in the evaluation. Defaults to 0.95 for 95\% confidence.
#' @param sample        a data frame containing the sample to be evaluated. The sample must at least contain a column of book values and a column of audit (true) values.
#' @param bookValues    a character specifying the column name for the book values in the \code{sample}.
#' @param auditValues   a character specifying the column name for the audit values in the \code{sample}.
#' @param counts        a integer vector specifying the number of times each item in the sample should be counted in the evaluation (due to it being selected multiple times for the sample).
#' @param nSumstats     an integer larger than 0 specifying the number of items in the sample. If specified, overrides the \code{sample}, \code{bookValues} and \code{auditValues} arguments and assumes that the data come from summary statistics specified by both \code{nSumstats} and \code{kSumstats}.
#' @param kSumstats     a numeric value larger than 0 specifying the sum of errors found in the sample. If specified, overrides the \code{sample}, \code{bookValues} and \code{auditValues} arguments and assumes that the data come from summary statistics specified by both \code{kSumstats} and \code{nSumstats}.
#' @param N             an integer larger than 0 specifying the total number of items in the population.
#' @param populationBookValue if \code{method} is one of \code{direct}, \code{difference}, \code{quotient}, or \code{regression}, a numeric value specifying the total value of the items in the population. This argument is optional otherwise.
#' @param prior         a logical specifying if a prior distribution must be used, or an object of class \code{jfaPrior} or \code{jfaPosterior} containing the prior distribution. Defaults to \code{FALSE} for frequentist planning. If \code{TRUE}, a negligible prior distribution is chosen by default, but can be adjusted using the `kPrior` and `nPrior` arguments. Chooses a conjugate gamma distribution for the Poisson likelihood, a conjugate beta distribution for the binomial likelihood, and a conjugate beta-binomial distribution for the hypergeometric likelihood.
#' @param nPrior        if \code{prior = TRUE}, a numeric value larger than, or equal to, 0 specifying the sample size of the sample equivalent to the prior information.
#' @param kPrior        if \code{prior = TRUE}, a numeric value larger than, or equal to, 0 specifying the sum of errors in the sample equivalent to the prior information.
#' @param rohrbachDelta if \code{method = 'rohrbach'}, a numeric value specifying \eqn{\Delta} in Rohrbach's augmented variance bound (Rohrbach, 1993).
#' @param momentPoptype if \code{method = 'moment'}, a character specifying the type of population (Dworin and Grimlund, 1984). Possible options are \code{accounts} and \code{inventory}. This argument affects the calculation of the central moments in the bound.
#' @param csA           if \code{method = "coxsnell"}, a numeric value specifying the \eqn{\alpha} parameter of the prior distribution on the mean taint. Defaults to 1 as recommended by Cox and Snell (1979).
#' @param csB           if \code{method = "coxsnell"}, a numeric value specifying the \eqn{\beta} parameter of the prior distribution on the mean taint. Defaults to 3 as recommended by Cox and Snell (1979).
#' @param csMu          if \code{method = "coxsnell"}, a numeric value between 0 and 1 specifying the mean of the prior distribution on the mean taint. Defaults to 0.5 as recommended by Cox and Snell (1979).
#'
#' @details This section lists the available options for the \code{methods} argument.
#' 
#' \itemize{
#'  \item{\code{poisson}:          Evaluates the sample with the Poisson distribution. If combined with \code{prior = TRUE}, performs Bayesian evaluation using a \emph{gamma} prior and posterior.}
#'  \item{\code{binomial}:         Evaluates the sample with the binomial distribution. If combined with \code{prior = TRUE}, performs Bayesian evaluation using a \emph{beta} prior and posterior.}
#'  \item{\code{hypergeometric}:   Evaluates the sample with the hypergeometric distribution. If combined with \code{prior = TRUE}, performs Bayesian evaluation using a \emph{beta-binomial} prior and posterior.}
#'	\item{\code{mpu}:			   Evaluates the sample with the mean-per-unit estimator.}
#'  \item{\code{stringer}:         Evaluates the sample with the Stringer bound (Stringer, 1963).}
#'  \item{\code{stringer-meikle}:  Evaluates the sample with the Stringer bound with Meikle's correction for understatements (Meikle, 1972).}
#'  \item{\code{stringer-lta}:     Evaluates the sample with the Stringer bound with LTA correction for understatements (Leslie, Teitlebaum, and Anderson, 1979).}
#'  \item{\code{stringer-pvz}:     Evaluates the sample with the Stringer bound with Pap and van Zuijlen's correction for understatements (Pap and van Zuijlen, 1996).}
#'  \item{\code{rohrbach}:         Evaluates the sample with Rohrbach's augmented variance bound (Rohrbach, 1993).}
#'  \item{\code{moment}:           Evaluates the sample with the modified moment bound (Dworin and Grimlund, 1984).}
#'  \item{\code{coxsnell}:         Evaluates the sample with the Cox and Snell bound (Cox and Snell, 1979).}
#'  \item{\code{direct}:           Evaluates the sample with the direct estimator (Touw and Hoogduin, 2011).}
#'  \item{\code{difference}:       Evaluates the sample with the difference estimator (Touw and Hoogduin, 2011).}
#'  \item{\code{quotient}:         Evaluates the sample with the quotient estimator (Touw and Hoogduin, 2011).}
#'  \item{\code{regression}:       Evaluates the sample with the regression estimator (Touw and Hoogduin, 2011).}
#' }
#' 
#' @references Cox, D. and Snell, E. (1979). On sampling and the estimation of rare errors. \emph{Biometrika}, 66(1), 125-132. 
#' @references Derks, K., de Swart, J., van Batenburg, P., Wagenmakers, E.-J., & Wetzels, R. (2021). Priors in a Bayesian audit: How integration of existing information into the prior distribution can improve audit transparency and efficiency. \emph{International Journal of Auditing}, 1-16.
#' @references Dworin, L. D. and Grimlund, R. A. (1984). Dollar-unit sampling for accounts receivable and inventory. \emph{The Accounting Review}, 59(2), 218–241
#' @references Leslie, D. A., Teitlebaum, A. D., & Anderson, R. J. (1979). \emph{Dollar-unit Sampling: A Practical Guide for Auditors}. Copp Clark Pitman; Belmont, Calif.: distributed by Fearon-Pitman.
#' @references Meikle, G. R. (1972). \emph{Statistical Sampling in an Audit Context: An Audit Technique}. Canadian Institute of Chartered Accountants.
#' @references Pap, G., and van Zuijlen, M. C. (1996). On the asymptotic behavior of the Stringer bound. \emph{Statistica Neerlandica}, 50(3), 367-389.
#' @references Rohrbach, K. J. (1993). Variance augmentation to achieve nominal coverage probability in sampling from audit populations. \emph{Auditing}, 12(2), 79.
#' @references Stringer, K. W. (1963). Practical aspects of statistical sampling in auditing. \emph{In Proceedings of the Business and Economic Statistics Section} (pp. 405-411). American Statistical Association.
#' @references Touw, P., and Hoogduin, L. (2011). \emph{Statistiek voor Audit en Controlling}. Boom uitgevers Amsterdam.
#'
#' @return An object of class \code{jfaEvaluation} containing:
#' 
#' \item{confidence}{a numeric value between 0 and 1 indicating the confidence level.}
#' \item{materiality}{if \code{materiality} is specified, a numeric value between 0 and 1 indicating the performance materiality as a fraction of the total population size.}
#' \item{minPrecision}{if \code{minPrecision} is specified, a numeric value between 0 and 1 indicating the minimum required precision as a fraction of the total population size.}
#' \item{method}{a character indicating the evaluation method.}
#' \item{N}{if \code{N} is specified, in integer larger than 0 indicating the population size.}
#' \item{n}{an integer larger than 0 indicating the sample size.}
#' \item{k}{an integer larger than, or equal to, 0 indicating the number of items in the sample that contained an error.}
#' \item{t}{a value larger than, or equal to, 0, indicating the sum of observed taints.}
#' \item{mle}{a numeric value between 0 and 1 indicating the most likely error in the population as a fraction of its total size.}
#' \item{precision}{a numeric value between 0 and 1 indicating the difference between the most likely error and the upper bound in the population as a fraction of the total population size.}
#' \item{popBookvalue}{if \code{populationBookValue} is specified, a numeric value larger than 0 indicating the total value of the population.}
#' \item{pointEstimate}{if \code{method} is one of \code{direct}, \code{difference}, \code{quotient}, or \code{regression}, a numeric value indicating the point estimate of the population misstatement as a fraction the total population size.}
#' \item{lowerBound}{if method is one of \code{direct}, \code{difference}, \code{quotient}, or \code{regression}, a numeric value indicating the lower bound of the interval around the population misstatement as a fraction the total population size.}
#' \item{upperBound}{if method is one of \code{direct}, \code{difference}, \code{quotient}, or \code{regression}, a numeric value indicating the upper bound of the interval around the population misstatement as a fraction the total population size.}
#' \item{confBound}{a numeric value indicating the upper bound on the population misstatement as a fraction the total population size.}
#' \item{conclusion}{if \code{materiality} is specified, a character indicating the conclusion about whether to approve or not approve the population with respect to the performance materiality.}
#' \item{populationK}{if \code{method = 'hypergeometric'}, an integer indicating the assumed total errors in the population.}
#' \item{prior}{an object of class 'jfaPrior' that contains the prior distribution.}
#' \item{posterior}{an object of class 'jfaPosterior' that contains the posterior distribution.}
#' \item{data}{a data frame containing the relevant columns from the \code{sample}.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}} \code{\link{planning}} \code{\link{selection}} \code{\link{report}} \code{\link{auditBF}}
#'
#' @keywords evaluation confidence bound audit
#'
#' @examples
#' data('BuildIt')
#'
#' # Draw a sample of 100 monetary units from the population using
#' # fixed interval monetary unit sampling
#' sample <- selection(population = BuildIt, sampleSize = 100, 
#'           algorithm = 'interval', units = 'mus', bookValues = 'bookValue')$sample
#' 
#' # Evaluate using the Stringer bound
#' evaluation(materiality = 0.05, method = 'stringer', confidence = 0.95,
#'            sample = sample, bookValues = 'bookValue', auditValues = 'auditValue')
#'
#' @export 

evaluation <- function(materiality = NULL, minPrecision = NULL, method = 'binomial', 
                       confidence = 0.95, sample = NULL, bookValues = NULL, auditValues = NULL,
                       counts = NULL, nSumstats = NULL, kSumstats = NULL,
                       N = NULL, populationBookValue = NULL,
                       prior = FALSE, nPrior = 0, kPrior = 0, 
                       rohrbachDelta = 2.7, momentPoptype = 'accounts',
                       csA = 1, csB = 3, csMu = 0.5) {
  
  # Import existing prior distribution with class 'jfaPrior' or 'jfaPosterior'.
  if (class(prior) %in% c("jfaPrior", "jfaPosterior")) {
    
    if (kPrior != 0 || nPrior != 0)
      warning("When the prior is of class 'jfaPrior' or 'jfaPosterior', the arguments 'nPrior' and 'kPrior' will not be used.")
    
    nPrior      <- prior[["description"]]$implicitn
    kPrior      <- prior[["description"]]$implicitk
    method      <- prior[["likelihood"]]
    
  }
  
  # Perform error handling with respect to incompatible input options
  if (confidence >= 1 || confidence <= 0 || is.null(confidence))
    stop("Specify a valid value for the 'confidence' argument. Possible values lie within the range of 0 to 1.")
  
  if (is.null(materiality) && is.null(minPrecision))
    stop("You must specify your sampling objective(s) using the 'materiality' or 'minPrecision' argument(s).")
  
  if (!is.null(minPrecision) && (minPrecision <= 0 || minPrecision >= 1))
    stop("The minimum required precision must be a positive value < 1.")
  
  if (!(method %in% c("poisson", "binomial", "hypergeometric", "stringer", "stringer-meikle", "stringer-lta", "stringer-pvz", "rohrbach", "moment", "coxsnell", "direct", "difference", "quotient", "regression", "mpu")) || length(method) != 1)
    stop("Specify a valid method for the evaluation.")
  
  if (!is.null(counts) && any(counts < 1))
    stop("When specified, your 'counts' must all be equal to, or larger than, 1.")          
  
  if (((class(prior) == "logical" && prior == TRUE) || class(prior) %in% c("jfaPrior", "jfaPosterior")) && method %in% c("stringer", "stringer-meikle", "stringer-lta", "stringer-pvz", "rohrbach", "moment", "direct", "difference", "quotient", "regression", "mpu"))
    stop("To use a prior distribution, you must use either the 'poisson', the 'binomial', or the 'hypergeometric' method.")  
  
  if ((class(prior) == "logical" && prior == TRUE) && kPrior < 0 || nPrior < 0)
    stop("When you specify a 'prior', both 'kPrior' and 'nPrior' should be higher than zero.")
  
  if (!is.null(nSumstats) || !is.null(kSumstats)) {
    
    if (is.null(nSumstats) || is.null(kSumstats))
      stop("When using summary statistics, both 'nSumstats' and 'kSumstats' must be specified.")
    
    if (nSumstats <= 0 || nSumstats%%1 != 0)
      stop("'nSumstats' must be a positive integer.")
    
    if (kSumstats < 0)
      stop("'kSumstats' must be equal to, or larger than, zero.")
    
    if (length(nSumstats) != 1 || length(kSumstats) != 1)
      stop("Specify one value for 'nSumstats' and 'kSumstats'.")
    
    if (kSumstats > nSumstats)
      stop("The sum of the errors provided in 'kSumstats' is higher than the sample size provided in 'nSumstats'.")
    
    if (method %in% c("stringer", "stringer-meikle", "stringer-lta", "stringer-pvz", "coxsnell", "rohrbach", "moment", "direct", "difference", "quotient", "regression", "mpu"))
      stop("The selected method requires raw observations and does not accomodate summary statistics")
    
    if (kSumstats%%1 != 0 && method == "hypergeometric" && !((class(prior) == "logical" && prior == TRUE) || class(prior) %in% c("jfaPrior", "jfaPosterior")))
      stop("When 'kSumstats' is specified and the likelihood is 'hypergeometric', its value must be an integer.")
    
    n <- nSumstats
    k <- kSumstats
    t <- kSumstats
    
  } else if (!is.null(sample)) {
    
    if (is.null(bookValues) || is.null(auditValues) || length(bookValues) != 1 || length(auditValues) != 1)
      stop("Specify a valid book value column name and a valid audit value column name when using a sample.")
    
    missingValues <- unique(c(which(is.na(sample[, bookValues])), which(is.na(sample[, auditValues]))))
    
    if (length(missingValues) == nrow(sample))
      stop("Your sample contains no items after removing missing values from the book value and audit value columns.")
    
    sample <- stats::na.omit(sample)
    n <- nrow(sample)
    
    if (!is.null(counts))
      n <- sum(counts)
    
    bv <- sample[, bookValues]
    av <- sample[, auditValues]
    taints <- (bv - av) / bv
    k <- length(which(taints != 0))
    
    if (!is.null(counts))
      taints <- taints * counts
    
    t <- sum(taints)
    
  } else {
    
    stop("You must specify an annotated sample using 'sample', 'bookValues', 'auditValues', and 'counts', or provide summary statistics using 'nSumstats' and 'kSumstats'.")
    
  }
  
  # Set the materiality and the minimium precision to 1 if they are NULL
  if (is.null(materiality))
    materiality <- 1
  if (is.null(minPrecision))
    minPrecision <- 1
  
  # Define placeholders for the most likely error and the precision  
  mle       <- NULL
  precision <- NULL
  
  # Calculate the results depending on the specified method
  if (method == 'poisson') {
    
    if ((class(prior) == "logical" && prior == TRUE) || class(prior) %in% c("jfaPrior", "jfaPosterior")) {
      # Bayesian evaluation using the gamma distribution
      bound     <- stats::qgamma(p = confidence, shape = 1 + kPrior + t, rate = nPrior + n)
      mle       <- ((1 + kPrior + t) - 1) / (nPrior + n)
      precision <- bound - mle
    } else {
      # Classical evaluation using the Poisson distribution
      bound     <- stats::qgamma(p = confidence, shape = 1 + t, rate = n)
      mle       <- t / n
      precision <- bound - mle
    }
    
  } else if (method == 'binomial') { 
    
    if ((class(prior) == "logical" && prior == TRUE) || class(prior) %in% c("jfaPrior", "jfaPosterior")) {
      # Bayesian evaluation using the beta distribution
      bound     <- stats::qbeta(p = confidence, shape1 = 1 + kPrior + t, shape2 = 1 + nPrior - kPrior + n - t)
      mle       <- (1 + kPrior + t - 1) / ((1 + kPrior + t) + (1 + nPrior - kPrior + n - t) - 2)
      precision <- bound - mle
    } else {
      # Classical evaluation using the binomial distribution
      bound     <- stats::qbeta(p = confidence, shape1 = 1 + t, shape2 = n - t)
      mle       <- t / n
      precision <- bound - mle
    }
    
  } else if (method == 'hypergeometric') {
    
    if (is.null(N))
      stop("Evaluation with 'hypergeometric' likelihood requires that you specify the population size 'N'.")
    
    if ((class(prior) == "logical" && prior == TRUE) || class(prior) %in% c("jfaPrior", "jfaPosterior")) {
      # Bayesian evaluation using the beta-binomial distribution
      bound     <- .qBetaBinom(p = confidence, N = N - n, shape1 = 1 + kPrior + t, shape2 = 1 + nPrior - kPrior + n - t) / N
      mle       <- .modeBetaBinom(N = N - n, shape1 = 1 + kPrior + t, shape2 = 1 + nPrior - kPrior + n - t) / N
      precision <- bound - mle
    } else {
      # Classical evaluation using the hypergeometric distribution
      populationK <- ceiling(materiality * N)
      bound     <- .qHyper(p = confidence, N = N, n = n, k = k) / N
      mle       <- k / n
      precision <- bound - mle
    }
    
  } else if (method == 'stringer') {
    
    # Classical evaluation using the Stringer bound
    out         <- .stringerBound(taints, confidence, n)
    bound       <- out[["confBound"]]
    mle         <- out[["mle"]]
    precision   <- out[["precision"]]
    
  } else if (method == 'stringer-meikle') {
    
    # Classical evaluation using the Stringer bound with Meikle's adjustment
    out         <- .stringerBound(taints, confidence, n, correction = 'meikle')
    bound       <- out[["confBound"]]
    mle         <- out[["mle"]]
    precision   <- out[["precision"]]
    
  } else if (method == 'stringer-lta') {
    
    # Classical evaluation using the Stringer bound with the LTA adjustment
    out         <- .stringerBound(taints, confidence, n, correction = 'lta')
    bound       <- out[["confBound"]]
    mle         <- out[["mle"]]
    precision   <- out[["precision"]]
    
  } else if (method == 'stringer-pvz') {
    
    # Classical evaluation using the Stringer bound with PvZ adjustment
    out         <- .stringerBound(taints, confidence, n, correction = 'pvz')
    bound       <- out[["confBound"]]
    mle         <- out[["mle"]]
    precision   <- out[["precision"]]
    
  } else if (method == 'rohrbach') {
    
    # Classical evaluation using Rohrbachs augmented variance bound
    out         <- .rohrbachBound(taints, confidence, n, N, rohrbachDelta = rohrbachDelta)
    bound       <- out[["confBound"]]
    mle         <- out[["mle"]]
    precision   <- out[["precision"]]
    
  } else if (method == 'moment') {
    
    # Classical evaluation using the Modified Moment bound
    out         <- .momentBound(taints, confidence, n, momentPoptype = momentPoptype)
    bound       <- out[["confBound"]]
    mle         <- out[["mle"]]
    precision   <- out[["precision"]]
    
  } else if (method == 'coxsnell') {
    
    # Bayesian evaluation using the Cox and Snell bound 
    out         <- .coxAndSnellBound(taints, confidence, n, csA, csB, csMu, aPrior = 1 + kPrior, bPrior = 1 + nPrior - kPrior)
    bound       <- out[["confBound"]]
    mle         <- out[["mle"]]
    precision   <- out[["precision"]]
    
  } else if (method == 'mpu') {
    
    # Classical evaluation using the Mean-per-unit estimator
    out         <- .mpuMethod(taints, confidence, n)
    bound       <- out[["confBound"]]
    mle         <- out[["mle"]]
    precision   <- out[["precision"]]
    
  } else if (method == 'direct') {
    
    # Classical evaluation using the Direct estimator
    out         <- .directMethod(bv, av, confidence, N, n, populationBookValue)
    mle         <- out[["pointEstimate"]]
    precision   <- out[["precision"]]
    
  } else if (method == 'difference') {
    
    # Classical evaluation using the Difference estimator
    out         <- .differenceMethod(bv, av, confidence, N, n, populationBookValue)
    mle         <- out[["pointEstimate"]]
    precision   <- out[["precision"]]
    
  } else if (method == 'quotient') {
    
    # Classical evaluation using the Quotient estimator
    out         <- .quotientMethod(bv, av, confidence, N, n, populationBookValue)
    mle         <- out[["pointEstimate"]]
    precision   <- out[["precision"]]
    
  } else if (method == 'regression') {
    
    # Classical evaluation using the Regression estimator
    out         <- .regressionMethod(bv, av, confidence, N, n, populationBookValue)
    mle         <- out[["pointEstimate"]]
    precision   <- out[["precision"]]
    
  } else if (method == 'newmethod') {
    
    # Evaluation using a new (to be added) method
    #
    # out       <- .functionFromMethodsFile()
    # bound     <- out[["confBound"]]
    # mle       <- out[["mle"]]
    # precision <- out[["precision"]]
    
  }
  
  # Create the main results object
  result                     <- list()
  result[["confidence"]]     <- as.numeric(confidence)
  result[["materiality"]]    <- as.numeric(materiality)
  result[["minPrecision"]]   <- as.numeric(minPrecision)
  result[["method"]]         <- as.character(method)
  result[["N"]]              <- as.numeric(N)
  result[["n"]]              <- as.numeric(n)
  result[["k"]]              <- as.numeric(k)
  result[["t"]]              <- as.numeric(t)
  
  if (!is.null(mle))
    result[["mle"]]          <- as.numeric(mle)
  
  if (!is.null(precision))
    result[["precision"]]    <- as.numeric(precision)
  
  if (!is.null(populationBookValue))
    result[["popBookvalue"]] <- as.numeric(populationBookValue)
  
  if (method %in% c("direct", "difference", "quotient", "regression")) {
    # These methods yield an interval instead of a bound
    result[["lowerBound"]]   <- as.numeric(out[["lowerBound"]])
    result[["upperBound"]]   <- as.numeric(out[["upperBound"]])
  } else {
    # These methods yield an upper bound
    result[["confBound"]]    <- as.numeric(bound) 
    if (method == "coxsnell") {
      # This method yields extra statistics
      result[["multiplicationFactor"]] <- as.numeric(out[["multiplicationFactor"]])
      result[["df1"]]                  <- as.numeric(out[["df1"]])
      result[["df2"]]                  <- as.numeric(out[["df2"]])
    }
  }
  
  if (method == 'hypergeometric' && is.logical(prior) && prior == FALSE)
    result[["populationK"]] <- as.numeric(populationK)
  
  # Has the minimum precision objective (if applicable) been achieved?
  approvePrecision <- TRUE
  if (minPrecision != 1) {
    if (method %in% c("direct", "difference", "quotient", "regression")) {
      approvePrecision <- (result[["precision"]] / populationBookValue) < minPrecision
    } else {
      approvePrecision <- result[["precision"]] < minPrecision
    }
  }
  
  # Has the materiality objective (if applicable) been achieved?
  approveMateriality <- TRUE
  if (materiality != 1) {
    if (method %in% c("direct", "difference", "quotient", "regression")) {
      approveMateriality <- (result[["upperBound"]] / populationBookValue) < materiality
    } else {
      approveMateriality <- result[["confBound"]] < materiality
    }
  }
  
  # Provide a conclusion with respect to the objectives
  result[["conclusion"]] <- ifelse(approveMateriality && approvePrecision, 
                                   yes = "Approve population",
                                   no = "Do not approve population")
  
  # Create the prior distribution object	
  if (((class(prior) == "logical" && prior == TRUE) || class(prior) %in% c("jfaPrior", "jfaPosterior"))) {
    if (class(prior) == "jfaPrior" && !is.null(prior[["hypotheses"]])) {
      result[["prior"]]           <- prior
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
  
  if (!is.null(result[["prior"]])) {
    # Create the posterior distribution object
    result[["posterior"]] <- list()
    
    # Functional form of the posterior distribution
    result[["posterior"]]$posterior <- switch(method, 
                                              "poisson" = paste0("gamma(\u03B1 = ", round(result[["prior"]][["description"]]$alpha + result[["t"]], 3), ", \u03B2 = ", round(result[["prior"]][["description"]]$beta + result[["n"]], 3), ")"),
                                              "binomial" = paste0("beta(\u03B1 = ", round(result[["prior"]][["description"]]$alpha + result[["t"]], 3), ", \u03B2 = ", round(result[["prior"]][["description"]]$beta + result[["n"]] - result[["t"]], 3), ")"),
                                              "hypergeometric" = paste0("beta-binomial(N = ", result[["N"]] - result[["n"]], ", \u03B1 = ", round(result[["prior"]][["description"]]$alpha + result[["t"]], 3), ", \u03B2 = ", round(result[["prior"]][["description"]]$beta + result[["n"]] - result[["t"]], 3), ")"))
    result[["posterior"]]$likelihood <- method
    
    # Create the description section
    result[["posterior"]][["description"]]           <- list()
    result[["posterior"]][["description"]]$density   <- switch(method, "poisson" = "gamma", "binomial" = "beta", "hypergeometric" = "beta-binomial")
    result[["posterior"]][["description"]]$n         <- result[["n"]]
    result[["posterior"]][["description"]]$k         <- result[["t"]]
    result[["posterior"]][["description"]]$alpha     <- switch(method,
                                                               "poisson" = result[["prior"]][["description"]]$alpha + result[["t"]],
                                                               "binomial" = result[["prior"]][["description"]]$alpha + result[["t"]],
                                                               "hypergeometric" = result[["prior"]][["description"]]$alpha + result[["t"]])
    result[["posterior"]][["description"]]$beta      <- switch(method,
                                                               "poisson" = result[["prior"]][["description"]]$beta + result[["n"]], 
                                                               "binomial" = result[["prior"]][["description"]]$beta + result[["n"]] - result[["t"]], 
                                                               "hypergeometric" = result[["prior"]][["description"]]$beta + result[["n"]] - result[["t"]])
    result[["posterior"]][["description"]]$implicitk <- result[["posterior"]][["description"]]$alpha - 1
    result[["posterior"]][["description"]]$implicitn <- switch(method,
                                                               "poisson" = result[["posterior"]][["description"]]$beta, 
                                                               "binomial" = result[["posterior"]][["description"]]$beta - 1 + result[["t"]], 
                                                               "hypergeometric" = result[["posterior"]][["description"]]$beta - 1 + result[["t"]])
    
    
    # Create the statistics section
    result[["posterior"]][["statistics"]]           <- list()
    result[["posterior"]][["statistics"]]$mode      <- switch(method,
                                                              "poisson" = (result[["posterior"]][["description"]]$alpha - 1) / result[["posterior"]][["description"]]$beta,
                                                              "binomial" = (result[["posterior"]][["description"]]$alpha - 1) / (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta - 2),
                                                              "hypergeometric" = .modeBetaBinom(N = result[["N"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta))
    result[["posterior"]][["statistics"]]$mean      <- switch(method, 
                                                              "poisson" = result[["posterior"]][["description"]]$alpha / result[["posterior"]][["description"]]$beta,
                                                              "binomial" = result[["posterior"]][["description"]]$alpha / (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta),
                                                              "hypergeometric" = result[["posterior"]][["description"]]$alpha / (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta) * result[["N"]])
    result[["posterior"]][["statistics"]]$median    <- switch(method, 
                                                              "poisson" = stats::qgamma(0.5, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
                                                              "binomial" = stats::qbeta(0.5, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
                                                              "hypergeometric" = .qBetaBinom(0.5, N = result[["N"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta))
    result[["posterior"]][["statistics"]]$ub        <- switch(method, 
                                                              "poisson" = stats::qgamma(confidence, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
                                                              "binomial" = stats::qbeta(confidence, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
                                                              "hypergeometric" = .qBetaBinom(confidence, N = result[["N"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta))									
    result[["posterior"]][["statistics"]]$precision <- result[["posterior"]][["statistics"]]$ub - result[["posterior"]][["statistics"]]$mode
    
    # Create the hypotheses section
    if (result[["materiality"]] != 1) {
      result[["posterior"]][["hypotheses"]]             <- list()
      result[["posterior"]][["hypotheses"]]$hypotheses  <- c(paste0("H-: \u0398 < ", materiality), paste0("H+: \u0398 > ", materiality))
      result[["posterior"]][["hypotheses"]]$pHmin       <- .restrictprob(switch(method, 
                                                                                "poisson" = stats::pgamma(materiality, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
                                                                                "binomial" = stats::pbeta(materiality, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
                                                                                "hypergeometric" = .pBetaBinom(ceiling(materiality * result[["N"]]), N = result[["N"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta)))
      result[["posterior"]][["hypotheses"]]$pHplus      <- .restrictprob(switch(method, 
                                                                                "poisson" = stats::pgamma(materiality, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta, lower.tail = FALSE),
                                                                                "binomial" = stats::pbeta(materiality, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta, lower.tail = FALSE),
                                                                                "hypergeometric" = .pBetaBinom(ceiling(materiality * result[["N"]]), N = result[["N"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta, lower.tail = FALSE)))
      result[["posterior"]][["hypotheses"]]$oddsHmin    <- result[["posterior"]][["hypotheses"]]$pHmin / result[["posterior"]][["hypotheses"]]$pHplus
      result[["posterior"]][["hypotheses"]]$oddsHplus   <- 1 / result[["posterior"]][["hypotheses"]]$oddsHmin
      result[["posterior"]][["hypotheses"]]$bf          <- result[["posterior"]][["hypotheses"]]$oddsHmin / result[["prior"]][["hypotheses"]]$oddsHmin
    }
    
    result[["posterior"]][["N"]] <- result[["N"]]
    
    # Add class 'jfaPosterior' to the posterior distribution object.
    class(result[["posterior"]]) <- "jfaPosterior"
  }
  
  # Add the data and taints to the output
  if (!is.null(sample)) {
    indexa                <- which(colnames(sample) == auditValues)
    indexb                <- which(colnames(sample) == bookValues)
    frame                 <- as.data.frame(sample[, c(indexb, indexa)])
    frame                 <- cbind(as.numeric(rownames(frame)), frame)
    frame[["difference"]] <- frame[, 2] - frame[, 3]
    frame[["taint"]]      <- frame[, 4] / frame[, 2]
    colnames(frame)       <- c("Row", bookValues, auditValues, "Difference", "Taint")
    result[["data"]]      <- frame
  }
  
  # Add class 'jfaEvaluation' to the result.
  class(result) <- "jfaEvaluation"
  return(result)
}
