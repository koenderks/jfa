#' Sampling from Audit Populations
#'
#' @description This function takes a data frame and performs sampling according to one of three popular algorithms: random sampling, cell sampling, or fixed interval sampling. Sampling is done in combination with one of two sampling units: records or monetary units The returned object is of class \code{jfaSampling} and can be used with associated \code{print()} and \code{plot()} methods. 
#'
#' @usage sampling(population, sampleSize, bookValues = NULL, units = "records", 
#'          algorithm = "random", intervalStartingPoint = 1, ordered = TRUE, 
#'          ascending = TRUE, withReplacement = FALSE, seed = 1)
#'
#' @param population            a data frame containing the population the auditor wishes to sample from.
#' @param sampleSize            the number of observations that need to be selected from the population. Can also be an object of class \code{jfaPlanning}.
#' @param bookValues            a character specifying the name of the column containing the book values (as in the population data).
#' @param algorithm             can be either one of \code{random} (default) for random sampling, \code{cell} for cell sampling, or \code{interval} for fixed interval sampling. 
#' @param units                 can be either \code{records} (default) for record sampling, or \code{mus} for monetary unit sampling.
#' @param intervalStartingPoint the starting point in the interval (used only in fixed interval sampling)
#' @param ordered               if \code{TRUE} (default), the population is first ordered according to the value of their book values.
#' @param ascending             if \code{TRUE} (default), order the population in ascending order. 
#' @param withReplacement       whether sampling should be performed with replacement. Defaults to \code{FALSE}.
#' @param seed                  seed to reproduce results. Default is 1.
#' 
#' @details This first part of this section elaborates on the possible options for the \code{units} argument:
#' 
#' \itemize{
#'  \item{\code{records}:     In record sampling, each observation in the population is seen as a sampling unit. An observation of $5000 is therefore equally likely to be selected as an observation of $500.}
#'  \item{\code{mus}:         In monetary unit sampling, each monetary unit in the population is seen as a sampling unit. An observation of $5000 is therefore ten times more likely to be selected as an observation of $500.}
#' }
#' 
#' This second part of this section elaborates on the possible options for the \code{algorithm} argument:
#' 
#' \itemize{
#'  \item{\code{random}:      In random sampling each sampling unit in the population is drawn with equal probability.}
#'  \item{\code{cell}:        In cell sampling the sampling units in the population are divided into a number (equal to the sample size) of intervals. From each interval one sampling unit is selected with equal probability.}
#'  \item{\code{interval}:    In fixed interval sampling the sampling units in the population are divided into a number (equal to the sample size) of intervals. From each interval one sampling unit is selected according to a fixed starting point (\code{intervalStartingPoint}).}
#' }
#'
#' @return An object of class \code{jfaSampling} containing:
#' 
#' \item{population}{a data frame containing the input population.}
#' \item{sample}{a data frame containing the selected observations.}
#' \item{bookValues}{if specified, the name of the specified book value column.}
#' \item{algorithm}{the algorithm that was used for sampling.}
#' \item{units}{the sampling units that were used for sampling.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}} \code{\link{planning}} \code{\link{evaluation}}
#'
#' @references Wampler, B., & McEacharn, M. (2005). Monetary-unit sampling using Microsoft Excel. \emph{The CPA journal}, 75(5), 36.
#'
#' @examples
#' library(jfa)
#' 
#' # Generate some audit data (N = 1000).
#' population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), 
#'                          bookValue = runif(n = 1000, min = 700, max = 1000))
#' 
#' # Calculate the sample size according to the binomial distribution with zero errors
#' jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, 
#'                    likelihood = "binomial")
#'
#' # Draw sample using random record sampling
#' sampling(population = population, sampleSize = jfaRes, algorithm = "random", 
#'          units = "records", seed = 1)
#' 
#' # Draw sample using random monetary unit sampling
#' sampling(population = population, sampleSize = jfaRes, algorithm = "random", 
#'          units = "mus", bookValues = "bookValue", seed = 1)
#'
#' @keywords sampling sample audit
#'
#' @export

sampling <- function(population, sampleSize, bookValues = NULL, units = "records", 
                     algorithm = "random", intervalStartingPoint = 1, ordered = TRUE, 
                     ascending = TRUE, withReplacement = FALSE, seed = 1){
  if(class(sampleSize) == "jfaPlanning"){
    sampleSize <- sampleSize$sampleSize 
  }
  if(sampleSize > nrow(population))
    stop("Cannot take a sample larger than the population")
  if(!(algorithm %in% c("random", "cell", "interval")) || length(algorithm) != 1)
    stop("Specify a valid algorithm")
  if(!(units %in% c("records", "mus")) || length(units) != 1)
    stop("Specify a valid sampling unit")
  if(units == "mus" && is.null(bookValues))
    stop("Book values must be specified if MUS is used")
  if(!is.null(bookValues) && length(bookValues) != 1)
    stop("Specify one column for the book values")
  if(!is.null(bookValues) && !(bookValues %in% colnames(population)))
    stop("The book value column cannot be found in the population data")
  rownames(population) <- 1:nrow(population)
  bv <- NULL
  if(!is.null(bookValues))
    bv <- population[, bookValues]
  if(ordered && !is.null(bv))
    population <- population[order(bv, decreasing = !ascending), ]
  if(!is.null(bv) && any(bv < 0))
    stop("The book values contain negative values")
  set.seed(seed)
  if(algorithm == "random" && units == "records"){
    index <- sample(rownames(population), size = sampleSize, replace = withReplacement)
  } else if(algorithm == "random" && units == "mus"){
    index <- sample(rownames(population), size = sampleSize, replace = withReplacement, prob = bv)
  } else if(algorithm == "cell" && units == "records"){
    interval <- ceiling(nrow(population) / sampleSize)
    suppressWarnings({
      mat <- matrix(as.numeric(rownames(population)), ncol = interval, byrow = TRUE, nrow = sampleSize)
    })
    colIndex <- sample(1:ncol(mat), size = sampleSize, replace = TRUE)
    index <- NULL
    for(i in 1:length(colIndex)){
      index <- c(index, mat[i, colIndex[i]])
    }
  } else if(algorithm == "cell" && units == "mus"){
    interval <- ceiling(sum(bv) / sampleSize)
    suppressWarnings({
      mat <- matrix(rep(as.numeric(rownames(population)), times = bv), ncol = interval, byrow = TRUE, nrow = sampleSize)
    })
    colIndex <- sample(1:ncol(mat), size = sampleSize, replace = TRUE)
    index <- NULL
    for(i in 1:length(colIndex)){
      index <- c(index, mat[i, colIndex[i]])
    }
  } else if(algorithm == "interval" && units == "records"){
    interval <- ceiling(nrow(population) / sampleSize)
    suppressWarnings({
      mat <- matrix(as.numeric(rownames(population)), ncol = interval, byrow = TRUE, nrow = sampleSize)
    })
    index <- mat[, intervalStartingPoint]
  } else if(algorithm == "interval" && units == "mus"){
    interval <- ceiling(sum(bv) / sampleSize)
    suppressWarnings({
      mat <- matrix(rep(as.numeric(rownames(population)), times = bv), ncol = interval, byrow = TRUE, nrow = sampleSize)
    })
    index <- mat[, intervalStartingPoint]
  }
  count <- as.numeric(table(index))
  index <- unique(index)
  if(length(index) < sampleSize)
    warning("The sample contains fewer observations than the specified sample size")
  rowNumber <- index
  sample <- cbind(rowNumber, count, population[rowNumber, ])
  rownames(sample) <- 1:nrow(sample)
  results <- list()
  results[["population"]] <- population
  results[["sample"]] <- sample
  results[["bookValues"]] <- bookValues
  results[["algorithm"]] <- algorithm
  results[["units"]] <- units
  class(results) <- "jfaSampling"
  return(results)
}