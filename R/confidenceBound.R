#' Audit confidence / credible bounds
#'
#' This function takes summary statistics about an evaluated audit sample and calculates
#' a confidence bound accordint to a specified method.
#'
#' @usage confidenceBound(sample = NULL, bookValues = NULL, auditValues = NULL, confidence = 0.95,
#'                        dataType = "sample", sampleSize = NULL, sumErrors = NULL,
#'                        method = "binomial", materiality = NULL)
#'
#' @param sample a data frame containing a column of book values and a column of audit values.
#' @param bookValues the column name for the book values in the sample.
#' @param auditValues the column name for the audit (true) values in the sample.
#' @param confidence the required confidence for the bound.
#' @param dataType can be either "sample" for data input, or "sumstats" for input in the form of summary statistics.
#' @param sampleSize the number of observations in the sample. Only used when dataType = "sumstats".
#' @param sumErrors the sum of the errors found in the sample. Only used when dataType = "sumstats".
#' @param method can be either one of "binomial", "hypergeometric", "poisson", "stringer".
#' @param materiality if specified, the function also returns the conclusion of the analysis with respect to the materiality.
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
  if(!(method %in% c("binomial", "stringer")) || length(method) != 1)
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
    taints <- ifelse(taints < 0, yes = 0, no = taints)
    taints <- ifelse(taints > 1, yes = 1, no = taints)
    taints <- sort(subset(taints, taints > 0), decreasing = TRUE)
    bound <- 1 - (1 - confidence)^(1 / n)
    if(length(taints) > 0){
      propSum <- 0
      for(i in 1:length(taints)){
        propSum <- propSum + (qbeta(p = confidence, shape1 = i + 1, shape2 = n - i) - qbeta(p = confidence, shape1 = (i - 1) + 1, shape2 = n - (i - 1)))  * taints[i]
      }
      bound <- bound + propSum
    }
  }
  
  results <- list()
  results[["n"]] <- n
  results[["k"]] <- k
  results[["confidence"]] <- confidence
  results[["confBound"]] <- bound
  results[["method"]] <- method
  if(!is.null(materiality)){
    results[["materiality"]] <- materiality
    results[["conclusion"]] <- ifelse(bound < materiality, yes = "approve", no = "not approve")
  }
  results[["jfaType"]] <- "evaluation"
  class(results) <- "jfa"
  
  return(results)
}
