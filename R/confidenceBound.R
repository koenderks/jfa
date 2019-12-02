#' Audit confidence / credible bounds
#'
#' This function takes a sample data frame or summary statistics about an evaluated audit sample and calculates
#' a confidence bound accordint to a specified method.
#'
#' @usage confidenceBound(sample = NULL, bookValues = NULL, auditValues = NULL, confidence = 0.95,
#'                        dataType = "sample", sampleSize = NULL, sumErrors = NULL,
#'                        method = "binomial", materiality = NULL)
#'
#' @param sample a data frame containing at least a column of book values and a column of audit (true) values.
#' @param bookValues the column name for the book values in the sample.
#' @param auditValues the column name for the audit (true) values in the sample.
#' @param confidence the required confidence level for the bound.
#' @param dataType can be either \emph{sample} for data input, or \emph{sumstats} for input in the form of summary statistics.
#' @param sampleSize the number of observations in the sample. Only used when \emph{dataType = "sumstats"}.
#' @param sumErrors the sum of the errors found in the sample. Only used when \emph{dataType = "sumstats"}.
#' @param method can be either one of \emph{binomial, hypergeometric, poisson, stringer}. 
#' @param materiality if specified, the function also returns the conclusion of the analysis with respect to the materiality. This value must be specified as a fraction of the total value of the population.
#'
#' @details This section lists the available options for the \emph{methods} argument.
#' 
#' \itemize{
#'  \item{\emph{binomial}         Clopper-Pearson confidence interval for a binomial rate parameter.}
#'  \item{\emph{stringer}         The Stringer bound (Stringer, 1963).}
#'  \item{\emph{stringer-meikle}  Stringer bound with Meikle's correction (Meikle, 1972).}
#'  \item{\emph{stringer-lta}     Stringer bound with LTA correction (Leslie, Teitlebaum, and Anderson, 1979).}
#'  \item{\emph{stringer-pvz}     Stringer bound with Pap and van Zuijlen's correction.}
#'  \item{\emph{rohrbach}         Rohrbach's augmented variance bound.}
#'  \item{\emph{moment}           Modified moment bound.}
#' }
#' 
#' @references Bickel, P. J. (1992). Inference and auditing: the Stringer bound.
#' International Statistical Review, 197-209.
#' @references Clopper, C. J., & Pearson, E. S. (1934). The use of confidence or
#' fiducial limits illustrated in the case of the binomial. Biometrika, 26(4),
#' 404-413.
#' @references Leslie, D. A., Teitlebaum, A. D., & Anderson, R. J. (1979).
#' Dollar-unit sampling: a practical guide for auditors. Copp Clark Pitman;
#' Belmont, Calif.: distributed by Fearon-Pitman.
#' @references Meikle, G. R. (1972). Statistical Sampling in an Audit Context:
#' An Audit Technique. Canadian Institute of Chartered Accountants.
#' @references Pap, G., & van Zuijlen, M. C. (1996). On the asymptotic behaviour
#' of the Stringer bound 1. Statistica Neerlandica, 50(3), 367-389.
#' @references Stringer, K. W. (1963). Practical aspects of statistical sampling
#' in auditing. In Proceedings of the Business and Economic Statistics Section
#' (pp. 405-411). American Statistical Association.
#'
#' @return A list containing the confidence bound for the audit.
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso
#'
#' @references
#' 
#' @keywords confidence bound
#'
#' @examples
#' 
#' # Using the binomial distribution, calculates the upper confidence bound for a materiality of 5% 
#' # when 3 mistakes are found in a sample of 234.
#'
#' # Frequentist planning (n = 234)
#' jfaRes <- jfa::sampleSize(materiality = 0.05, confidence = 0.95,
#'                           expectedError = 0.025, likelihood = "binomial")
#' 
#' # Six errors are allowed in the sample. Three are found.
#' 
#' confidenceBound(sampleSize = jfaRes$sampleSize, sumErrors = 3, method = "binomial")
#' 
#' @keywords confidence bound
#'
#' @export 

confidenceBound <- function(sample = NULL, bookValues = NULL, auditValues = NULL, confidence = 0.95,
                            dataType = "sample", sampleSize = NULL, sumErrors = NULL,
                            method = "binomial", materiality = NULL){
  
  if(!(dataType %in% c("sample", "sumstats")) || length(dataType) != 1)
      stop("Specify a valid data type")
  if(!(method %in% c("binomial", "stringer", "stringer-meikle", "stringer-lta", "stringer-pvz")) || length(method) != 1)
    stop("Specify a valid method for the confidence bound")
  
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
    bound <- jfa::.stringerBound(taints, confidence, n)
  } else if(method == "stringer-meikle"){
    bound <- jfa::.stringerBound(taints, confidence, n, correction = "meikle")
  } else if(method == "stringer-lta"){
    bound <- jfa::.stringerBound(taints, confidence, n, correction = "lta")
  } else if(method == "stringer-pvz"){
    bound <- jfa::.stringerBound(taints, confidence, n, correction = "pvz")
  } else if(method == "rohrbach"){
    
  } else if(method == "moment"){
    
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
  results[["jfaType"]] <- "evaluation"
  class(results) <- "jfa"
  
  return(results)
}
