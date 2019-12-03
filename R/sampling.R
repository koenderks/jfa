#' Sampling from audit populations
#'
#' @description This function takes a data frame and performs sampling according to one of three algorithms: random sampling, cell sampling, or fixed interval sampling, in combination with either record sampling or monetary unit sampling. The returned object is of class \code{jfaSampling} and can be used with associated \code{print()} and \code{plot()} methods. 
#'
#' @usage sampling(population, sampleSize, bookValues = NULL, 
#'                 algorithm = "random", units = "record", intervalStartingPoint = 1,
#'                 ordered = TRUE, ascending = TRUE, withReplacement = FALSE, seed = 1)
#'
#' @param population            a data frame containing the population the auditor wishes to sample from.
#' @param sampleSize            the number of observations that need to be selected from the population. Can also be an object of class \code{jfaPlanning}.
#' @param bookValues            the name of the column containing the book values (as in the population data).
#' @param algorithm             can be either one of \code{random} (default) for random sampling, \code{cell} for cell sampling, or \code{interval} for fixed interval sampling. 
#' @param units                 can be either \code{records} for record (default) sampling, or \code{mus} for monetary unit sampling.
#' @param intervalStartingPoint the starting point in the interval (used only in fixed interval sampling)
#' @param ordered               if \code{TRUE} (default), the population is first ordered according to the value of their book values.
#' @param ascending             if \code{TRUE} (default), order the population in ascending order. 
#' @param withReplacement       whether sampling should be performed with replacement. Defaults to \code{FALSE}.
#' @param seed                  seed to reproduce results. Default is 1.
#'
#' @return An object of class \code{jfaSampling} containing:
#' \item{population}{a data frame containing the input population.}
#' \item{sample}{a data frame containing the selected observations.}
#' \item{bookValues}{if specified, the name of the specified book value column.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{planning}} \code{\link{evaluation}}
#'
#' @references
#'
#' @examples
#' 
#' library(jfa)
#' 
#' # Generate some audit data (N = 1000).
#' population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), 
#'                          bookValue = runif(n = 1000, min = 100, max = 500))
#' 
#' # Calculate the sample size according to the binomial distribution with zero errors
#' n <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, 
#'              likelihood = "binomial")$sampleSize
#'
#' # Draw sample using random record sampling
#' sampling(population = population, sampleSize = n, algorithm = "random", 
#'          units = "records", seed = 1)
#' 
#' # Draw sample using random monetary unit sampling
#' sampling(population = population, sampleSize = n, algorithm = "random", 
#'          units = "mus", bookValues = "bookValue", seed = 1)
#'
#' @keywords sampling sample
#'
#' @export

sampling <- function(population, sampleSize, bookValues = NULL, 
                     algorithm = "random", units = "record", intervalStartingPoint = 1,
                     ordered = TRUE, ascending = TRUE, withReplacement = FALSE, seed = 1){
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
  class(results) <- "jfaSampling"
  return(results)
}