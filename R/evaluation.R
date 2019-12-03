#' Evaluation of Audit Samples using Confidence / Credible Bounds
#'
#' @description This function takes a sample data frame or summary statistics about an evaluated audit sample and calculates a confidence bound accordint to a specified method. The returned object is of class \code{jfaEvaluation} and can be used with associated \code{print()} method.
#'
#' @usage evaluation(sample = NULL, bookValues = NULL, auditValues = NULL, 
#'                   confidence = 0.95, dataType = "sample", sampleSize = NULL, 
#'                   sumErrors = NULL, method = "binomial", materiality = NULL, 
#'                   N = NULL, rohrbachDelta = 2.7)
#'
#' @param sample a data frame containing at least a column of book values and a column of audit (true) values.
#' @param bookValues the column name for the book values in the sample.
#' @param auditValues the column name for the audit (true) values in the sample.
#' @param confidence the required confidence level for the bound.
#' @param dataType can be either \code{sample} for data input, or \code{sumstats} for input in the form of summary statistics.
#' @param sampleSize the number of observations in the sample. Only used when \code{dataType = "sumstats"}.
#' @param sumErrors the sum of the errors found in the sample. Only used when \code{dataType = "sumstats"}.
#' @param method can be either one of \code{poisson}, \code{binomial}, \code{hypergeometric}, \code{stringer}, \code{stringer-meikle}, \code{stringer-lta}, \code{stringer-pvz}, \code{rohrbach}, \code{moment}}. 
#' @param materiality if specified, the function also returns the conclusion of the analysis with respect to the materiality. This value must be specified as a fraction of the total value of the population.
#'
#' @details This section lists the available options for the \code{methods} argument.
#' 
#' \itemize{
#'  \item{\code{poisson}:          }
#'  \item{\code{binomial}:         Clopper-Pearson confidence interval for a binomial rate parameter.}
#'  \item{\code{hypergeometric}:   }
#'  \item{\code{stringer}:         The Stringer bound (Stringer, 1963).}
#'  \item{\code{stringer-meikle}:  Stringer bound with Meikle's correction (Meikle, 1972).}
#'  \item{\code{stringer-lta}:     Stringer bound with LTA correction (Leslie, Teitlebaum, and Anderson, 1979).}
#'  \item{\code{stringer-pvz}:     Stringer bound with Pap and van Zuijlen's correction (Pap and van Zuijlen, 1996).}
#'  \item{\code{rohrbach}:         Rohrbach's augmented variance bound (Rohrbach, 1993).}
#'  \item{\code{moment}:           Modified moment bound (Dworin and Grimlund, 1986).}
#' }
#' 
#' @references Bickel, P. J. (1992). Inference and auditing: the Stringer bound. International Statistical Review, 197-209.
#' @references Clopper, C. J., & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. Biometrika, 26(4), 404-413.
#' @references Dworin, L., & Grimlund, R. A. (1986). Dollar-unit sampling: A comparison of the quasi-Bayesian and moment bounds. Accounting Review, 36-57.
#' @references Leslie, D. A., Teitlebaum, A. D., & Anderson, R. J. (1979). Dollar-unit sampling: a practical guide for auditors. Copp Clark Pitman; Belmont, Calif.: distributed by Fearon-Pitman.
#' @references Meikle, G. R. (1972). Statistical Sampling in an Audit Context: An Audit Technique. Canadian Institute of Chartered Accountants.
#' @references Pap, G., & van Zuijlen, M. C. (1996). On the asymptotic behaviour of the Stringer bound 1. Statistica Neerlandica, 50(3), 367-389.
#' @references Rohrbach, K. J. (1993). Variance augmentation to achieve nominal coverage probability in sampling from audit populations. Auditing, 12(2), 79.
#' @references Stringer, K. W. (1963). Practical aspects of statistical sampling in auditing. In Proceedings of the Business and Economic Statistics Section (pp. 405-411). American Statistical Association.
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
#' # Evaluate the sample using summary statistics.
#' evaluation(sampleSize = nrow(samp$sample), sumErrors = 1, dataType = "sumstats",
#'            method = "binomial", materiality = 0.05)
#' 
#' @keywords evaluation confidence bound
#'
#' @export 

evaluation <- function(sample = NULL, bookValues = NULL, auditValues = NULL, confidence = 0.95,
                       dataType = "sample", sampleSize = NULL, sumErrors = NULL,
                       method = "binomial", materiality = NULL, N = NULL, 
                       rohrbachDelta = 2.7, momentPoptype = "accounts"){
  
  if(!(dataType %in% c("sample", "sumstats")) || length(dataType) != 1)
      stop("Specify a valid data type")
  if(!(method %in% c("binomial", "stringer", "stringer-meikle", "stringer-lta", "stringer-pvz",
                     "rohrbach", "moment")) || length(method) != 1)
    stop("Specify a valid method for the confidence bound")
  if(method %in% c("stringer", "stringer-meikle", "stringer-lta", "stringer-pvz",
                   "rohrbach", "moment") && dataType != "sample")
    stop("The selected method requires raw observations, and does not accomodate dataType = sumstats")
  
  if(!is.null(materiality)){
    mat <- materiality
  } else {
    mat <- 0
  }
  
  if(dataType == "sample"){
    
    if(is.null(bookValues) || is.null(auditValues) || length(bookValues) != 1 || length(auditValues) != 1)
      stop("Specify a valid book value column and audit value column when using dataType = sample")
    
    sample <- na.omit(sample)
    n <- nrow(sample)
    taints <- (sample[, bookValues] - sample[, auditValues]) / sample[, bookValues]
    k <- length(which(taints != 0))
    
  } else if(dataType == "sumstats"){
    
    if(is.null(sampleSize) || is.null(sumErrors) || length(sampleSize) != 1 || length(sumErrors) != 1)
      stop("Specify valid values for the sampleSize and sumErrors arguments with dataType = sumstats")
    if(sumErrors > sampleSize)
      stop("The sum of the errors is higher than the sample size")
    
    n <- sampleSize
    k <- sumErrors
    
  }
  
  if(method == "binomial"){
    bound <- stats::binom.test(x = k, n = n, p = mat, alternative = "less", conf.level = confidence)$conf.int[2]
  } else if(method == "stringer"){
    bound <- jfa:::.stringerBound(taints, confidence, n)
  } else if(method == "stringer-meikle"){
    bound <- jfa:::.stringerBound(taints, confidence, n, correction = "meikle")
  } else if(method == "stringer-lta"){
    bound <- jfa:::.stringerBound(taints, confidence, n, correction = "lta")
  } else if(method == "stringer-pvz"){
    bound <- jfa:::.stringerBound(taints, confidence, n, correction = "pvz")
  } else if(method == "rohrbach"){
    if(is.null(N))
      stop("Rohrbach's bound requires that you specify the population size N")
    w <- 1 - taints
    mu <- mean(taints)
    vars <- sum(w^2)/n - (2-(rohrbachDelta/n)) * ((1/2) * ((sum(w^2)/n) - var(w)))
    bound <- mu + qnorm(p = confidence) * sqrt((1-(n/N)) * (vars/n))
  } else if(method == "moment"){
    if(!(momentPoptype %in% c("inventory", "accounts")))
      stop("Specify a valid population type. Either inventory or accounts.")
    tall <- subset(taints, taints != 0)
    if(momentPoptype == "inventory" & length(tall) > 0){
      tstar <- 0.81 * (1 - 0.667 * tanh(10 * abs(mean(tall))))
    } else if(momentPoptype == "inventory" & length(tall) == 0){
      tstar <- 0.81 * (1 - 0.667 * tanh(10 * 0))
    }
    if(momentPoptype == "accounts" & length(tall) > 0){
      tstar <- 0.81 * (1 - 0.667 * tanh(10 * mean(tall))) * (1 + 0.667 * tanh(length(tall) / 10))
    } else if(momentPoptype == "accounts" & length(tall) == 0){
      tstar <- 0.81 * (1 - 0.667 * tanh(10 * 0)) * (1 + 0.667 * tanh(0 / 10))
    }
    ncm1_z <- (tstar^1 + sum(tall^1)) / (length(tall) + 1)
    ncm2_z <- (tstar^2 + sum(tall^2)) / (length(tall) + 1)
    ncm3_z <- (tstar^3 + sum(tall^3)) / (length(tall) + 1)
    ncm1_e <- (length(tall) + 1) / (n + 2)
    ncm2_e <- ((length(tall) + 2) / (n + 3)) * ncm1_e
    ncm3_e <- ((length(tall) + 3) / (n + 4)) * ncm2_e
    ncm1_t <- ncm1_e * ncm1_z
    ncm2_t <- (ncm1_e * ncm2_z + ((n - 1) * ncm2_e * ncm1_z^2)) / n
    ncm3_t <- ((ncm1_e * ncm3_z + (3 * (n - 1) * ncm2_e * ncm1_z * ncm2_z)) / 
                 n^2) + (((n - 1) * (n - 2) * ncm3_e * ncm1_z^3)/(n^2))
    cm2_t  <- ncm2_t - ncm1_t^2
    cm3_t  <- ncm3_t - (3 * ncm1_t * ncm2_t) + (2 * ncm1_t^3)
    A      <- (4 * cm2_t^3)/(cm3_t^2)
    B      <- cm3_t / (2 * cm2_t)
    G      <- ncm1_t - ((2 * cm2_t^2)/cm3_t)
    bound  <- G + (A * B * (1 + (qnorm(confidence, mean = 0, sd = 1)/ sqrt(9 * A)) - (1 / (9 * A)))^3)
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
