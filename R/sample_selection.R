# Copyright (C) 2020-2022 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Audit Sampling: Selection
#'
#' @description \code{sample_selection()} is used to perform statistical selection of audit samples. It offers flexible implementations of the most common audit sampling algorithms for attributes sampling and monetary unit sampling. The function returns an object of class \code{jfaSelection} which can be used with associated \code{summary()} and a \code{plot()} methods.
#'
#' For more details on how to use this function, see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage sample_selection(data, size, units = c('items', 'values'),
#'           method = c('interval', 'cell', 'random', 'sieve'), values = NULL,
#'           order = NULL, decreasing = FALSE, randomize = FALSE,
#'           replace = FALSE, start = 1)
#'
#' @param data       a data frame containing the population data.
#' @param size       an integer larger than 0 specifying the number of units to select. Can also be an object of class \code{jfaPlanning}.
#' @param units      a character specifying the type of sampling units. Possible options are \code{items} (default) for selection on the level of items (rows) or \code{values} for selection on the level of monetary units.
#' @param method     a character specifying the sampling algorithm. Possible options are \code{interval} (default) for fixed interval sampling, \code{cell} for cell sampling, \code{random} for random sampling, or \code{sieve} for modified sieve sampling.
#' @param values     a character specifying the name of a column in \code{data} containing the book values of the items.
#' @param order      a character specifying the name of a column in \code{data} containing the ranks of the items. The items in the \code{data} are ordered according to these values in the order indicated by \code{decreasing}.
#' @param decreasing a logical specifying whether to order the items from smallest to largest. Only used if \code{order} is specified.
#' @param randomize  a logical specifying if items should be randomly shuffled prior to selection. Note that \code{randomize = TRUE} overrules \code{order}.
#' @param replace    a logical specifying if sampling units should be selected with replacement. Only used for method \code{random}.
#' @param start      an integer larger than 0 specifying index of the unit that should be selected. Only used for method \code{interval}.
#'
#' @details This section elaborates on the possible options for the \code{units} argument:
#'
#' \itemize{
#'  \item{\code{items}:  In attributes sampling each item in the population is a sampling unit. An item with a book value of $5000 is therefore equally likely to be selected as an item with a book value of $500.}
#'  \item{\code{values}: In monetary unit sampling each monetary unit in the population is a sampling unit. An item with a book value of $5000 is therefore ten times more likely to be selected as an item with a book value of $500.}
#' }
#'
#' @details This section elaborates on the possible options for the \code{method} argument:
#'
#' \itemize{
#'  \item{\code{interval}: In fixed interval sampling the sampling units are divided into a number of equally large intervals. In each interval, a single sampling unit is selected according to a fixed starting point (specified by \code{start}).}
#'  \item{\code{cell}:     In cell sampling the sampling units in the population are divided into a number (equal to the sample size) of equally large intervals. In each interval, a single sampling unit is selected randomly.}
#'  \item{\code{random}:   In random sampling all sampling units are drawn with equal probability.}
#'  \item{\code{sieve}:    In modified sieve sampling items are selected with the largest sieve ratio (Hoogduin, Hall, & Tsay, 2010).}
#' }
#'
#' @return An object of class \code{jfaSelection} containing:
#'
#' \item{data}{a data frame containing the population data.}
#' \item{sample}{a data frame containing the selected data sample.}
#' \item{n.req}{an integer giving the requested sample size.}
#' \item{n.units}{an integer giving the number of obtained sampling units.}
#' \item{n.items}{an integer giving the number of obtained sample items.}
#' \item{N.units}{an integer giving the number of sampling units in the population data.}
#' \item{N.items}{an integer giving the number of items in the population data.}
#' \item{interval}{if \code{method = 'interval'}, a numeric value giving the size of the selection interval.}
#' \item{units}{a character indicating the type of sampling units.}
#' \item{method}{a character indicating the sampling algorithm.}
#' \item{values}{if \code{values} is specified, a character indicating the book value column.}
#' \item{start}{if \code{method = 'interval'}, an integer giving the index of the selected unit in each interval.}
#' \item{data.name}{a character indicating the name of the population data.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}} \code{\link{planning}} \code{\link{evaluation}} \code{\link{report}}
#'
#' @references Hoogduin, L. A., Hall, T. W., & Tsay, J. J. (2010). Modified sieve sampling: A method for single-and multi-stage probability-proportional-to-size sampling. \emph{Auditing: A Journal of Practice & Theory}, 29(1), 125-148.
#' @references Leslie, D. A., Teitlebaum, A. D., & Anderson, R. J. (1979). \emph{Dollar-unit Sampling: A Practical Guide for Auditors}. Copp Clark Pitman; Belmont, Calif.: distributed by Fearon-Pitman.
#' @references Wampler, B., & McEacharn, M. (2005). Monetary-unit sampling using Microsoft Excel. \emph{The CPA journal}, 75(5), 36.
#'
#' @keywords audit items mus selection
#'
#' @examples
#' data("BuildIt")
#'
#' # Select 100 items using random sampling
#' sample_selection(data = BuildIt, size = 100, method = "random")
#'
#' # Select 150 monetary units using fixed interval sampling
#' sample_selection(
#'   data = BuildIt, size = 150, units = "values",
#'   method = "interval", values = "bookValue"
#' )
#' @export

sample_selection <- function(data, size, units = c("items", "values"),
                             method = c("interval", "cell", "random", "sieve"), values = NULL,
                             order = NULL, decreasing = FALSE, randomize = FALSE,
                             replace = FALSE, start = 1) {
  units <- match.arg(units)
  method <- match.arg(method)
  if (inherits(size, "jfaPlanning")) { # If the input for 'sampleSize' is of class 'jfaPlanning', extract the planned sample size
    size <- size[["n"]]
  } else {
    stopifnot("'size' must be a single integer > 0" = size > 0)
  }
  switch(units,
    "items" = stopifnot("cannot take a sample larger than the population when 'replace = FALSE'" = !(size > nrow(data) && !replace)),
    "values" = stopifnot("missing value for 'values'" = !is.null(values))
  )
  if (!is.null(values)) {
    stopifnot("'values' must be a single character" = is.character(values) && length(values) == 1)
    if (!(values %in% colnames(data))) { # Check if the book values column can be found in the data
      stop(paste0("'", values, "' is not a column in 'data'"))
    }
  }
  if (!is.null(order) && !(order %in% colnames(data))) { # Check if the order column can be found in the data
    stop(paste0("'", order, "' is not a column in 'data'"))
  }
  if (method == "interval") {
    stopifnot("'start' must be an integer >= 1" = start >= 1)
  }
  interval <- NULL # Placeholder for interval
  bookvalues <- NULL # Placeholder for book values
  dname <- deparse(substitute(data))
  data <- as.data.frame(data, row.names = 1:nrow(data)) # Convert the population to a data frame
  if (!randomize && !is.null(order)) { # Order the population
    data <- data[order(data[, order], decreasing = decreasing), , drop = FALSE]
  } else if (randomize) { # Randomize the population
    if (!is.null(order)) {
      message("'order' overruled by 'randomize = TRUE'")
    }
    data <- data[sample(1:nrow(data)), , drop = FALSE]
  }
  if (!is.null(values)) { # Take the book values from the population
    bookvalues <- data[, values]
  }
  if (units == "values") { # Check if the sample size is valid
    stopifnot("cannot take a sample larger than the population value" = size <= sum(bookvalues))
  }
  if (!is.null(bookvalues) && any(bookvalues < 0)) { # Remove the negative book values from the population
    message("'values' contains negative values which are removed before sample selection")
    negvals <- which(bookvalues < 0)
    data <- data[-negvals, ]
    bookvalues <- data[, values]
  }
  rowNumbers <- as.numeric(rownames(data))
  # Sampling algorithms:
  if (method == "random" && units == "items") {
    # 1. Random record sampling
    index <- sample(rowNumbers, size = size, replace = replace)
  } else if (method == "random" && units == "values") {
    # 2. Random monetary unit sampling
    if (size > nrow(data)) {
      replace <- TRUE
    }
    index <- sample(rowNumbers, size = size, replace = replace, prob = bookvalues)
  } else if (method == "cell" && units == "items") {
    # 3. Cell record sampling
    interval <- nrow(data) / size
    intervals <- 0:size * interval
    index <- NULL
    for (i in 1:size) {
      int.selection <- stats::runif(min = intervals[i], max = intervals[i + 1], n = 1)
      index <- c(index, rowNumbers[int.selection])
    }
  } else if (method == "cell" && units == "values") {
    # 4. Cell monetary unit sampling
    interval <- sum(bookvalues) / size
    intervals <- 0:size * interval
    index <- NULL
    for (i in 1:size) {
      int.selection <- stats::runif(min = intervals[i], max = intervals[i + 1], n = 1)
      index <- c(index, rowNumbers[which(int.selection <= cumsum(bookvalues))[1]])
    }
  } else if (method == "interval" && units == "items") {
    # 5. Fixed interval record sampling
    interval <- nrow(data) / size
    if (start > interval) {
      stop(paste0("'start' must be an integer <= the selection interval (", interval, ")"))
    }
    int.selection <- start + 0:(size - 1) * interval
    index <- rowNumbers[int.selection]
  } else if (method == "interval" && units == "values") {
    # 6. Fixed interval monetary unit sampling
    interval <- sum(bookvalues) / size
    if (start > interval) {
      stop(paste0("'start' must be an integer <= the selection interval (", interval, ")"))
    }
    int.selection <- start + 0:(size - 1) * interval
    index <- NULL
    for (i in 1:size) {
      index <- c(index, rowNumbers[which(int.selection[i] <= cumsum(bookvalues))[1]])
    }
  } else if (method == "sieve") {
    stopifnot("'method = sieve' does not accomodate 'units = items'" = units == "values")
    # 7. Modified sieve sampling (Hoogduin, Hall, & Tsay, 2010)
    ri <- bookvalues / stats::runif(length(bookvalues), min = 0, max = 1)
    index <- rowNumbers[order(-ri)]
    index <- index[1:size]
  }
  # Gather output
  count <- as.numeric(table(index)[match(unique(index), names(table(index)))])
  sample <- cbind(unique(index), count, data[match(unique(index), rowNumbers), ])
  colnames(sample) <- c("row", "times", colnames(data))
  # Create the main results object
  result <- list()
  result[["data"]] <- as.data.frame(data)
  result[["sample"]] <- as.data.frame(sample, row.names = 1:nrow(sample))
  result[["n.req"]] <- size
  result[["n.units"]] <- sum(count)
  result[["n.items"]] <- nrow(sample)
  result[["N.units"]] <- switch(units,
    "items" = nrow(data),
    "values" = sum(bookvalues)
  )
  result[["N.items"]] <- nrow(data)
  if (!is.null(interval)) {
    result[["interval"]] <- interval
  }
  result[["units"]] <- units
  result[["method"]] <- method
  result[["values"]] <- values
  if (method == "interval") {
    result[["start"]] <- start
  }
  result[["data.name"]] <- dname
  result[["values.name"]] <- values
  # Add class 'jfaSelection' to the result
  class(result) <- "jfaSelection"
  return(result)
}
