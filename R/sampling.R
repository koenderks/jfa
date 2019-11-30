#' Audit sampling
#'
#' This function takes a data frame and performs sampling according to one of three algorithms:
#' random sampling, cell sampling, or fixed interval sampling in combination with either
#' record sampling or monetary unit sampling. 
#'
#' @usage sampling(population, sampleSize, bookValues = NULL, 
#'                 algorithm = "random", units = "record", intervalStartingPoint = 1,
#'                 ordered = TRUE, ascending = TRUE, withReplacement = FALSE, seed = 1)
#'
#' @param population a data frame containing the population the auditor wishes to sample from.
#' @param sampleSize the number of observations that need to be selected from the population.
#' @param bookValues the name of the column containing the book values (as in the population data).
#' @param algorithm can be either one of "random" (default) for random sampling, "cell" for cell sampling, or "interval" for fixed interval sampling. 
#' @param units can be either "records" for record (default) sampling, or "mus" for monetary unit sampling.
#' @param intervalStartingPoint The starting point in the interval (used only in fixed interval sampling)
#' @param ordered if TRUE (default), the population is first ordered according to the value of their book values.
#' @param ascending if TRUE (default), order the population in ascending order. 
#' @param withReplacement whether sampling should be performed with replacement. Defaults to FALSE.
#' @param seed seed to reproduce results. Default is 1.
#'
#' @return A data frame containing the required sample for the audit.
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso
#'
#' @references
#'
#' @examples
#' 
#' population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), 
#'                          bookValue = runif(n = 1000, min = 100, max = 500))
#' 
#' # Calculate sample size
#' ss <- sampleSize(materiality = 0.05, confidence = 0.95, expectedError = 0, 
#'                  likelihood = "binomial")$sampleSize
#'
#' # Random record sampling
#' sampling(population = population, sampleSize = ss, algorithm = "random", 
#'          units = "records", seed = 1)
#' 
#' # Random monetary unit sampling
#' sampling(population = population, sampleSize = ss, algorithm = "random", 
#'          units = "mus", bookValues = "bookValue", seed = 1)
#'
#' @keywords sampling
#'
#' @export

sampling <- function(population, sampleSize, bookValues = NULL, 
                     algorithm = "random", units = "record", intervalStartingPoint = 1,
                     ordered = TRUE, ascending = TRUE, withReplacement = FALSE, seed = 1){
  if(sampleSize > nrow(population))
    stop("Cannot take a sample larger than the population")
  if(!(algorithm %in% c("random", "cell", "interval")))
    stop("Specify a valid algorithm")
  if(!(units %in% c("records", "mus")))
    stop("Specify a valid sampling unit")
  if(units == "mus" && is.null(bookValues))
    stop("Book values must be specified if MUS is used")
  if(!is.null(bookValues) && !(bookValues %in% colnames(population)))
    stop("The book value column cannot be found in the population data")
  rownames(population) <- 1:nrow(population)
  if(ordered)
    population <- population[order(bv, decreasing = !ascending), ]
  bv <- NULL
  if(!is.null(bookValues))
      bv <- population[, bookValues]
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
    warning("The sample contains fewer observations than required")
  rowNumber <- index
  sample <- cbind(rowNumber, count, population[rowNumber, ])
  rownames(sample) <- 1:nrow(sample)
  return(sample)
}