#' Selecting a Sample from an Audit Population
#'
#' @description This function takes a data frame and performs statistical sampling according to one of three algorithms: random sampling, cell sampling, and fixed interval sampling. Sampling is done on the level of two possible sampling units: records or monetary units. The returned object is of class \code{jfaSelection} and has a \code{print()} and \code{plot()} method. 
#'
#' For more details on how to use this function see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage selection(population, sampleSize, units = 'records', algorithm = 'random',
#'           bookValues = NULL, intervalStartingPoint = 1, ordered = TRUE, 
#'           ascending = TRUE, withReplacement = FALSE, seed = 1)
#'
#' @param population            a data frame containing the population of items the auditor wishes to sample from.
#' @param sampleSize            an integer specifying the number of sampling units that need to be selected from the population. Can also be an object of class \code{jfaPlanning}.
#' @param algorithm             a character specifying the sampling algorithm used. Possible options are \code{random} (default) for random sampling, \code{cell} for cell sampling, or \code{interval} for fixed interval sampling. 
#' @param units                 a character specifying the sampling units used. Possible options are \code{records} (default) for selection on the level of items or \code{mus} for selection on the level of monetary units.
#' @param bookValues            a character specifying the name of the column in the \code{population} that contains the book values of the items.
#' @param intervalStartingPoint if \code{algorithm = 'interval'}, an integer specifying the starting point of the algorithm.
#' @param ordered               a logical specifying whether to first order the items in the \code{population} according to the value of their \code{bookValues}. Defaults to \code{TRUE}.
#' @param ascending             if \code{ordered = TRUE}, a logical specifying whether to order the population \code{bookValues} from smallest to largest. Defaults to \code{TRUE}.
#' @param withReplacement       if \code{algorithm = 'random'}, a logical specifying whether sampling should be performed with replacement. Defaults to \code{FALSE}.
#' @param seed                  an integer specifying a seed to reproduce results. Defaults to 1.
#' 
#' @details The first part of this section elaborates on the possible options for the \code{units} argument:
#' 
#' \itemize{
#'  \item{\code{records}:     In record sampling each item in the population is seen as a sampling unit. An item of $5000 is therefore equally likely to be selected as an item of $500.}
#'  \item{\code{mus}:         In monetary unit sampling each monetary unit in the population is seen as a sampling unit. An item of $5000 is therefore ten times more likely to be selected as an item of $500.}
#' }
#' 
#' The second part of this section elaborates on the possible options for the \code{algorithm} argument:
#' 
#' \itemize{
#'  \item{\code{random}:      In random sampling each sampling unit in the population is drawn with equal probability.}
#'  \item{\code{cell}:        In cell sampling the sampling units in the population are divided into a number (equal to the sample size) of intervals. From each interval one sampling unit is selected with equal probability.}
#'  \item{\code{interval}:    In fixed interval sampling the sampling units in the population are divided into a number (equal to the sample size) of intervals. From each interval one sampling unit is selected according to a fixed starting point (specified by \code{intervalStartingPoint}).}
#' }
#'
#' @return An object of class \code{jfaSelection} containing:
#' 
#' \item{population}{a data frame containing the input population.}
#' \item{sample}{a data frame containing the selection of items.}
#' \item{units}{a character indicating the sampling units that were used to create the selection.}
#' \item{algorithm}{a character indicating the the algorithm that was used to create the selection.}
#' \item{bookValues}{if \code{bookValues} is specified, a character indicating the name of the book value column.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}} \code{\link{planning}} \code{\link{evaluation}} \code{\link{report}}
#'
#' @references Wampler, B., & McEacharn, M. (2005). Monetary-unit sampling using Microsoft Excel. \emph{The CPA journal}, 75(5), 36.
#'
#' @examples
#' library(jfa)
#' set.seed(1)
#' 
#' # Generate some audit data (N = 1000).
#' population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), 
#'                          bookValue = runif(n = 1000, min = 700, max = 1000))
#' 
#' # Draw a custom sample of 100 from the population (via random record sampling):
#' 
#' s1 <- selection(population = population, sampleSize = 100, algorithm = "random", 
#'                 units = "records", seed = 1)
#' print(s1)
#' 
#' # ------------------------------------------------------------
#' #                  jfa Selection Summary
#' # ------------------------------------------------------------
#' # Input:
#' #       
#' # Population size:         1000 
#' # Requested sample size:   100 
#' # Sampling units:          Records 
#' # Algorithm:               Random sampling   
#' # ------------------------------------------------------------ 
#' # Output:
#' # 
#' # Obtained sample size:    100 
#' # ------------------------------------------------------------
#' # Statistics:
#' #
#' # Proportion n/N:          0.1 
#' # ------------------------------------------------------------  
#' 
#' # Use the result from the planning stage in the sampling stage:
#' 
#' p1 <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, 
#'                likelihood = "binomial")
#' 
#' # Draw a sample via random monetary unit sampling:
#' s2 <- selection(population = population, sampleSize = p1, algorithm = "random", 
#'                 units = "mus", seed = 1, bookValues = "bookValue")
#' print(s2)
#' 
#' # ------------------------------------------------------------
#' #                  jfa Selection Summary
#' # ------------------------------------------------------------
#' # Input:
#' #      
#' # Population size:         1000 
#' # Requested sample size:   234 
#' # Sampling units:          Monetary units 
#' # Algorithm:               Random sampling   
#' # ------------------------------------------------------------ 
#' # Output:
#' #
#' # Obtained sample size:    234 
#' # ------------------------------------------------------------
#' # Statistics:
#' #
#' # Proportion n/N:          0.23 
#' # Percentage of value:     23.06% 
#' # ------------------------------------------------------------ 
#'
#' @keywords selection sample audit
#'
#' @export

selection <- function(population, sampleSize, units = 'records', algorithm = 'random', 
                      bookValues = NULL, intervalStartingPoint = 1, ordered = TRUE, 
                      ascending = TRUE, withReplacement = FALSE, seed = 1) {
  # If the input for 'sampleSize' is of class 'jfaPlanning', extract the planned sample size
  if (class(sampleSize) == "jfaPlanning")
    sampleSize <- sampleSize$sampleSize 
  # Perform error handling with respect to incompatible input options
  if (units == "records" && sampleSize > nrow(population))
    stop("Cannot take a sample larger than the population size")
  if (!(algorithm %in% c("random", "cell", "interval")) || length(algorithm) != 1)
    stop("algorithm must be one of 'random', 'cell', or 'interval'.")
  if (!(units %in% c("records", "mus")) || length(units) != 1)
    stop("units must be one of 'records' or 'mus'.")
  if (units == "mus" && is.null(bookValues))
    stop("Book values must be specified if 'units = mus' is used.")
  if (!is.null(bookValues) && length(bookValues) != 1)
    stop("Specify one column for the book values")
  if (!is.null(bookValues) && !(bookValues %in% colnames(population)))
    stop("The book value column cannot be located in the population data.")
  interval <- NULL
  # Convert the population to a data frame
  population <- as.data.frame(population)
  rownames(population) <- 1:nrow(population)
  bv <- NULL
  if (!is.null(bookValues))
    bv <- population[, bookValues]
  if (units == "mus" && sampleSize > sum(bv))
    stop("Cannot take a sample larger than the population value")
  if (ordered && !is.null(bv)) {
    population <- population[order(bv, decreasing = !ascending), ]
    bv <- population[, bookValues]
  }
  if (!is.null(bv) && any(bv < 0)) {
    warning("The book values contain negative values, these are removed from the data")
    negativeValues <- which(bv < 0)
    population <- population[-negativeValues, ]
    bv <- population[, bookValues]
  }
  # Set a seed for reproducibility
  set.seed(seed)
  # Sampling algorithms:
  if (algorithm == "random" && units == "records") {
    # 1. Random record sampling
    index <- sample(rownames(population), size = sampleSize, replace = withReplacement)
  } else if (algorithm == "random" && units == "mus") {
    # 2. Random monetary unit sampling
    if (sampleSize > nrow(population))
      withReplacement <- TRUE
    index <- sample(rownames(population), size = sampleSize, replace = withReplacement, prob = bv)
  } else if (algorithm == "cell" && units == "records") {
    # 3. Cell record sampling
    interval <- nrow(population) / sampleSize
    intervals <- 0:sampleSize * interval
    index <- NULL
    for (i in 1:sampleSize) {
      intervalSelection <- stats::runif(min = intervals[i], max = intervals[i + 1], n = 1)
      index <- c(index, as.numeric(rownames(population))[intervalSelection])
    }
  } else if (algorithm == "cell" && units == "mus") {
    # 4. Cell monetary unit sampling
    interval <- sum(bv) / sampleSize
    intervals <- 0:sampleSize * interval
    index <- NULL
    for (i in 1:sampleSize) {
      intervalSelection <- stats::runif(min = intervals[i], max = intervals[i + 1], n = 1)
      index <- c(index, which(intervalSelection < cumsum(bv))[1])
    }
  } else if (algorithm == "interval" && units == "records") {
    # 5. Fixed interval record sampling
    interval <- nrow(population) / sampleSize
    intervalSelection <- intervalStartingPoint + 0:(sampleSize - 1) * interval
    mat <- as.numeric(rownames(population))
    index <- mat[intervalSelection]
  } else if (algorithm == "interval" && units == "mus") {
    # 6. Fixed interval monetary unit sampling
    interval <- sum(bv) / sampleSize
    intervalSelection <- intervalStartingPoint + 0:(sampleSize - 1) * interval
    index <- NULL
    for (i in 1:sampleSize) {
      index <- c(index, which(intervalSelection[i] < cumsum(bv))[1])
    }
  }
  # Gather output
  count <- as.numeric(table(index))
  rowNumber <- as.numeric(unique(index))
  if (length(rowNumber) < sampleSize)
    warning("The sample contains fewer items than the specified sample size")
  sample <- cbind(rowNumber, count, population[rowNumber, ])
  rownames(sample) <- 1:nrow(sample)
  colnames(sample) <- c("rowNumber", "count", colnames(population))
  # Create the main results object.
  result <- list()
  result[["population"]] 			<- as.data.frame(population)
  result[["sample"]] 				<- as.data.frame(sample)
  result[["populationSize"]] 		<- as.numeric(nrow(population))
  result[["requestedSampleSize"]] 	<- as.numeric(sampleSize)
  result[["obtainedSampleSize"]] 	<- as.numeric(nrow(sample))
  result[["units"]] 				<- as.character(units)
  result[["algorithm"]] 			<- as.character(algorithm)
  result[["bookValues"]] 			<- as.character(bookValues)
  result[["intervalStartingPoint"]] <- as.numeric(intervalStartingPoint)
  if (!is.null(interval))
    result[["interval"]] 			<- as.numeric(interval)
  # Add class 'jfaSelection' to the result.
  class(result) <- "jfaSelection"
  return(result)
}
