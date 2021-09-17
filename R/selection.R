#' Select a Statistical Audit Sample
#'
#' @description This function takes a data frame and performs statistical sampling according to one of three algorithms: random sampling, cell sampling, and fixed interval sampling. Sampling is done on the level of two possible sampling units: items (records) or monetary units. The function returns an object of class \code{jfaSelection} which can be used with associated \code{summary()} and a \code{plot()} methods.
#'
#' For more details on how to use this function, see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage selection(data, size, units = c('items', 'values'), 
#'           method = c('interval', 'cell', 'random'), values = NULL,
#'           start = 1, order = FALSE, decreasing = FALSE, replace = FALSE)
#'
#' @param data           a data frame containing the population of items the auditor wishes to sample from.
#' @param size           an integer larger than 0 specifying the number of sampling units that need to be selected from the population. Can also be an object of class \code{jfaPlanning}.
#' @param units          a character specifying the sampling units used. Possible options are \code{items} (default) for selection on the level of items (rows) or \code{values} for selection on the level of monetary units.
#' @param method         a character specifying the sampling algorithm used. Possible options are \code{interval} (default) for fixed interval sampling, \code{cell} for cell sampling, or \code{random} for random. 
#' @param values         a character specifying name of a column in \code{data} containing the book values of the items.
#' @param start          if \code{method = 'interval'}, an integer larger than 0 specifying the starting point of the algorithm.
#' @param order          a logical specifying whether to first order the items in the \code{data} according to the value of their \code{values}. Defaults to \code{FALSE}.
#' @param decreasing     if \code{order = TRUE}, a logical specifying whether to order the population \code{values} from smallest to largest. Defaults to \code{FALSE}.
#' @param replace        if \code{method = 'random'}, a logical specifying whether sampling should be performed with replacement. Defaults to \code{FALSE}.
#' 
#' @details The first part of this section elaborates on the two possible options for the \code{units} argument:
#' 
#' \itemize{
#'  \item{\code{items}:     In record sampling each item in the population is seen as a sampling unit. An item of $5000 is therefore equally likely to be selected as an item of $500.}
#'  \item{\code{values}:    In monetary unit sampling each monetary unit in the population is seen as a sampling unit. An item of $5000 is therefore ten times more likely to be selected as an item of $500.}
#' }
#' 
#' The second part of this section elaborates on the three possible options for the \code{method} argument:
#' 
#' \itemize{
#'  \item{\code{interval}:    In fixed interval sampling the sampling units in the population are divided into a number (equal to the sample size) of intervals. From each interval one sampling unit is selected according to a fixed starting point (specified by \code{start}).}
#'  \item{\code{cell}:        In cell sampling the sampling units in the population are divided into a number (equal to the sample size) of intervals. From each interval one sampling unit is selected with equal probability.}
#'  \item{\code{random}:      In random sampling each sampling unit in the population is drawn with equal probability.}
#' }
#'
#' @return An object of class \code{jfaSelection} containing:
#' 
#' \item{data}{a data frame containing the input data.}
#' \item{sample}{a data frame containing the selected sample of items.}
#' \item{n.req}{an integer indicating the requested sample size.}
#' \item{n.units}{an integer indicating the total number of obtained sampling units.}
#' \item{n.items}{an integer indicating the total number of obtained sample items.}
#' \item{N.units}{an integer indicating the total number of sampling units in the population.}
#' \item{N.items}{an integer indicating the total number of items in the population.}
#' \item{interval}{if \code{method = 'interval'}, a numeric value indicating the size of the selection interval.}
#' \item{units}{a character indicating the sampling units that were used to create the selection.}
#' \item{method}{a character indicating the the algorithm that was used to create the selection.}
#' \item{values}{if \code{values} is specified, a character indicating the name of the book value column.}
#' \item{start}{if \code{method = 'interval'}, an integer indicating the starting point in the interval.}
#' \item{data.name}{a character string giving the name(s) of the data.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}} \code{\link{planning}} \code{\link{evaluation}} \code{\link{report}}
#'
#' @references Leslie, D. A., Teitlebaum, A. D., & Anderson, R. J. (1979). \emph{Dollar-unit Sampling: A Practical Guide for Auditors}. Copp Clark Pitman; Belmont, Calif.: distributed by Fearon-Pitman.
#' @references Wampler, B., & McEacharn, M. (2005). Monetary-unit sampling using Microsoft Excel. \emph{The CPA journal}, 75(5), 36.
#'
#' @keywords selection sample audit
#'
#' @examples
#' data('BuildIt')
#' 
#' # Draw a sample of 100 monetary units from the population using
#' # fixed interval monetary unit sampling
#' selection(data = BuildIt, size = 100, units = 'values',
#'           method = 'interval', values = 'bookValue')
#'
#' @export

selection <- function(data, size, units = c('items', 'values'), 
                      method = c('interval', 'cell', 'random'), values = NULL, 
                      start = 1, order = FALSE, decreasing = FALSE, replace = FALSE) {
  method <- match.arg(method)
  units <- match.arg(units)
  if (class(size) == "jfaPlanning") # If the input for 'sampleSize' is of class 'jfaPlanning', extract the planned sample size
    size <- size[["n"]]
  if (units == "items" && size > nrow(data) && !replace) # Check if the sample size is valid (< N)
    stop("cannot take a sample larger than the population when 'replace = FALSE'")
  if (units == "values" && is.null(values)) # Check if the book values have a valid input
    stop("'values' is missing for selection")
  if (!is.null(values) && length(values) != 1) # Check if the book values have a valid input
    stop("'values' must be a single character")
  if (!is.null(values) && !(values %in% colnames(data))) # Check if the book values column can be found in the population
    stop(paste0("'", values, "' is not a column in 'data'"))
  if (method == 'interval' && start < 1)
    stop("'start' must be an integer > 1")
  interval       <- NULL # Placeholder for interval
  bookvalues     <- NULL # Placeholder for book values
  dname          <- deparse(substitute(data))
  data           <- as.data.frame(data) # Convert the population to a data frame
  rownames(data) <- 1:nrow(data)
  if (!is.null(values)) # Take the book values from the population
    bookvalues <- data[, values]
  if (units == "values" && size > sum(bookvalues)) # Check if the sample size is valid
    stop("cannot take a sample larger than the population value")
  if (order && !is.null(bookvalues)) { # Order the population
    data <- data[order(bookvalues, decreasing = decreasing), ]
    bookvalues <- data[, values]
  }
  if (!is.null(bookvalues) && any(bookvalues < 0)) { # Remove the negative book values from the population
    warning("'values' contains negative values which are removed before selection")
    negvals <- which(bookvalues < 0)
    data <- data[-negvals, ]
    bookvalues <- data[, values]
  }
  # Sampling algorithms:
  if (method == "random" && units == "items") {
    # 1. Random record sampling
    index <- sample(rownames(data), size = size, replace = replace)
  } else if (method == "random" && units == "values") {
    # 2. Random monetary unit sampling
    if (size > nrow(data))
      replace <- TRUE
    index <- sample(rownames(data), size = size, replace = replace, prob = bookvalues)
  } else if (method == "cell" && units == "items") {
    # 3. Cell record sampling
    interval  <- nrow(data) / size
    intervals <- 0:size * interval
    index     <- NULL
    for (i in 1:size) {
      int.selection <- stats::runif(min = intervals[i], max = intervals[i + 1], n = 1)
      index         <- c(index, as.numeric(rownames(data))[int.selection])
    }
  } else if (method == "cell" && units == "values") {
    # 4. Cell monetary unit sampling
    interval  <- sum(bookvalues) / size
    intervals <- 0:size * interval
    index     <- NULL
    for (i in 1:size) {
      int.selection <- stats::runif(min = intervals[i], max = intervals[i + 1], n = 1)
      index         <- c(index, which(int.selection < cumsum(bookvalues))[1])
    }
  } else if (method == "interval" && units == "items") {
    # 5. Fixed interval record sampling
    interval      <- nrow(data) / size
    int.selection <- start + 0:(size - 1) * interval
    mat           <- as.numeric(rownames(data))
    index         <- mat[int.selection]
  } else if (method == "interval" && units == "values") {
    # 6. Fixed interval monetary unit sampling
    interval      <- sum(bookvalues) / size
    int.selection <- start + 0:(size - 1) * interval
    index         <- NULL
    for (i in 1:size) {
      index <- c(index, which(int.selection[i] < cumsum(bookvalues))[1])
    }
  }
  # Gather output
  count            <- as.numeric(table(index))
  rowNumber        <- as.numeric(unique(index))
  sample           <- cbind(rowNumber, count, data[rowNumber, ])
  rownames(sample) <- 1:nrow(sample)
  colnames(sample) <- c("row", "times", colnames(data))
  # Create the main results object
  result <- list()
  result[["data"]]       <- as.data.frame(data)
  result[["sample"]]     <- as.data.frame(sample)
  result[["n.req"]]      <- size
  result[["n.units"]]    <- sum(count)
  result[["n.items"]]    <- nrow(sample)
  result[["N.units"]]    <- if (units == 'items') nrow(data) else sum(bookvalues)
  result[["N.items"]]    <- nrow(data)
  if (!is.null(interval))
    result[["interval"]] <- interval
  result[["units"]]      <- units
  result[["method"]]     <- method
  result[["values"]]     <- values
  if (method == 'interval')
    result[["start"]]    <- start
  result[["data.name"]] <- dname
  # Add class 'jfaSelection' to the result
  class(result) <- "jfaSelection"
  return(result)
}
