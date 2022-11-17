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
# along with this program. If not, see <http://www.gnu.org/licenses/>.

#' Audit Sampling: Selection
#'
#' @description \code{selection()} is used to perform statistical selection of
#' audit samples. It offers flexible implementations of the most common audit
#' sampling algorithms for attributes sampling and monetary unit sampling. The
#' function returns an object of class \code{jfaSelection} that can be used with
#' associated \code{summary()} and a \code{plot()} methods.
#'
#' @usage selection(data, size, units = c('items', 'values'),
#'           method = c('interval', 'cell', 'random', 'sieve'), values = NULL,
#'           order = NULL, decreasing = FALSE, randomize = FALSE,
#'           replace = FALSE, start = 1)
#'
#' @param data       a data frame containing the population data.
#' @param size       an integer larger than 0 specifying the number of units to
#'   select. Can also be an object of class \code{jfaPlanning}.
#' @param units      a character specifying the type of sampling units. Possible
#'   options are \code{items} (default) for selection on the level of items
#'   (rows) or \code{values} for selection on the level of monetary units.
#' @param method     a character specifying the sampling algorithm. Possible
#'   options are \code{interval} (default) for fixed interval sampling,
#'   \code{cell} for cell sampling, \code{random} for random sampling, or
#'   \code{sieve} for modified sieve sampling.
#' @param values     a character specifying the name of a column in \code{data}
#'   containing the book values of the items.
#' @param order      a character specifying the name of a column in \code{data}
#'   containing the ranks of the items. The items in the \code{data} are ordered
#'   according to these values in the order indicated by \code{decreasing}.
#' @param decreasing a logical specifying whether to order the items from
#'   smallest to largest. Only used if \code{order} is specified.
#' @param randomize  a logical specifying if items should be randomly shuffled
#'   prior to selection. Note that \code{randomize = TRUE} overrules
#'   \code{order}.
#' @param replace    a logical specifying if sampling units should be selected
#'   with replacement. Only used for method \code{random}.
#' @param start      an integer larger than 0 specifying index of the unit that
#'   should be selected. Only used for method \code{interval}.
#'
#' @details This section elaborates on the possible options for the \code{units}
#'   argument:
#'
#' \itemize{
#'  \item{\code{items}: In attributes sampling each item in the population is a
#'    sampling unit. An item with a book value of $5000 is therefore equally
#'    likely to be selected as an item with a book value of $500.}
#'  \item{\code{values}: In monetary unit sampling each monetary unit in the
#'    population is a sampling unit. An item with a book value of $5000 is
#'    therefore ten times more likely to be selected as an item with a book
#'    value of $500.}
#' }
#'
#' @details This section elaborates on the possible options for the
#'   \code{method} argument:
#'
#' \itemize{
#'  \item{\code{interval}: In fixed interval sampling the sampling units are
#'    divided into a number of equally large intervals. In each interval, a
#'    single sampling unit is selected according to a fixed starting point
#'    (specified by \code{start}).}
#'  \item{\code{cell}:     In cell sampling the sampling units in the population
#'    are divided into a number (equal to the sample size) of equally large
#'    intervals. In each interval, a single sampling unit is selected randomly.}
#'  \item{\code{random}:   In random sampling all sampling units are drawn with
#'    equal probability.}
#'  \item{\code{sieve}:    In modified sieve sampling items are selected with
#'    the largest sieve ratio (Hoogduin, Hall, & Tsay, 2010).}
#' }
#'
#' @return An object of class \code{jfaSelection} containing:
#'
#' \item{data}{a data frame containing the population data.}
#' \item{sample}{a data frame containing the selected data sample.}
#' \item{n.req}{an integer giving the requested sample size.}
#' \item{n.units}{an integer giving the number of obtained sampling units.}
#' \item{n.items}{an integer giving the number of obtained sample items.}
#' \item{N.units}{an integer giving the number of sampling units in the
#'   population data.}
#' \item{N.items}{an integer giving the number of items in the population data.}
#' \item{interval}{if \code{method = 'interval'}, a numeric value giving the
#'   size of the selection interval.}
#' \item{units}{a character indicating the type of sampling units.}
#' \item{method}{a character indicating the sampling algorithm.}
#' \item{values}{if \code{values} is specified, a character indicating the book
#'   value column.}
#' \item{start}{if \code{method = 'interval'}, an integer giving the index of
#'   the selected unit in each interval.}
#' \item{data.name}{a character indicating the name of the population data.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}}
#'          \code{\link{planning}}
#'          \code{\link{evaluation}}
#'          \code{\link{report}}
#'
#' @references Derks, K., de Swart, J., Wagenmakers, E.-J., Wille, J., &
#'   Wetzels, R. (2021). JASP for audit: Bayesian tools for the auditing
#'   practice. \emph{Journal of Open Source Software}, \emph{6}(68), 2733.
#'   \doi{10.21105/joss.02733}
#' @references Hoogduin, L. A., Hall, T. W., & Tsay, J. J. (2010). Modified
#'   sieve sampling: A method for single-and multi-stage
#'   probability-proportional-to-size sampling. \emph{Auditing: A Journal of
#'   Practice & Theory}, 29(1), 125-148.
#'   \doi{10.2308/aud.2010.29.1.125}
#' @references Leslie, D. A., Teitlebaum, A. D., & Anderson, R. J. (1979).
#'   \emph{Dollar-unit Sampling: A Practical Guide for Auditors}. Copp Clark
#'   Pitman; Belmont, CA. ISBN: 9780773042780.
#'
#' @keywords audit items mus selection
#'
#' @examples
#' data("BuildIt")
#'
#' # Select 100 items using random sampling
#' set.seed(1)
#' selection(data = BuildIt, size = 100, method = "random")
#'
#' # Select 150 monetary units using fixed interval sampling
#' selection(
#'   data = BuildIt, size = 150, units = "values",
#'   method = "interval", values = "bookValue"
#' )
#' @export

selection <- function(data,
                      size,
                      units = c("items", "values"),
                      method = c("interval", "cell", "random", "sieve"),
                      values = NULL,
                      order = NULL,
                      decreasing = FALSE,
                      randomize = FALSE,
                      replace = FALSE,
                      start = 1) {
  units <- match.arg(units)
  method <- match.arg(method)
  use_mus <- units == "values"
  if (inherits(size, "jfaPlanning")) {
    size <- size[["n"]]
  }
  stopifnot("'size' must be a single integer > 0" = (size > 0) && (size %% 1 == 0))
  switch(units,
    "items" = stopifnot("cannot take a sample larger than the population when 'replace = FALSE'" = !(size > nrow(data) && !replace)),
    "values" = stopifnot("missing value for 'values'" = !is.null(values))
  )
  if (!is.null(values)) {
    valid_values <- is.character(values) && (length(values) == 1)
    stopifnot("'values' must be a single character" = valid_values)
    if (!(values %in% colnames(data))) {
      stop(paste0("'", values, "' is not a column in 'data'"))
    }
  }
  if (!is.null(order) && !(order %in% colnames(data))) {
    stop(paste0("'", order, "' is not a column in 'data'"))
  }
  if (method == "interval") {
    valid_start <- (start >= 1) && (start %% 1 == 0)
    stopifnot("'start' must be an integer >= 1" = valid_start)
  }
  interval <- NULL
  book_values <- NULL
  dname <- deparse(substitute(data))
  data <- as.data.frame(data, row.names = seq_len(nrow(data)))
  if (!randomize && !is.null(order)) {
    data <- data[order(data[, order], decreasing = decreasing), , drop = FALSE]
  } else if (randomize) {
    if (!is.null(order)) {
      message("'order' overruled by 'randomize = TRUE'")
    }
    new_order <- sample.int(nrow(data))
    data <- data[new_order, , drop = FALSE]
  }
  if (!is.null(values)) {
    book_values <- data[, values]
  }
  if (use_mus) {
    valid_size <- size <= sum(book_values)
    stopifnot("cannot take a sample larger than the population value" = valid_size)
  }
  if (!is.null(book_values) && any(book_values < 0)) {
    message("negative values in 'values' are removed before selection")
    negative_values <- which(book_values < 0)
    data <- data[-negative_values, ]
    book_values <- data[, values]
  }
  row_numbers <- as.numeric(rownames(data))
  if (method == "random" && !use_mus) {
    # Random record sampling
    sample_items <- sample(row_numbers, size, replace)
  } else if (method == "random" && use_mus) {
    # Random monetary unit sampling
    if (size > nrow(data)) {
      replace <- TRUE
    }
    sample_items <- sample(row_numbers, size, replace, book_values)
  } else if (method == "cell" && !use_mus) {
    # Cell record sampling
    interval <- nrow(data) / size
    intervals <- 0:size * interval
    sample_items <- NULL
    for (i in seq_len(size)) {
      selected_item <- stats::runif(1, intervals[i], intervals[i + 1])
      sample_items <- c(sample_items, row_numbers[selected_item])
    }
  } else if (method == "cell" && use_mus) {
    # Cell monetary unit sampling
    interval <- sum(book_values) / size
    intervals <- 0:size * interval
    sample_items <- NULL
    for (i in 1:size) {
      selected_unit <- stats::runif(1, intervals[i], intervals[i + 1])
      selected_item <- which(selected_unit <= cumsum(book_values))[1]
      sample_items <- c(sample_items, row_numbers[selected_item])
    }
  } else if (method == "interval" && !use_mus) {
    # Fixed interval record sampling
    interval <- nrow(data) / size
    if (start > interval) {
      stop(paste0("'start' must be an integer <= the selection interval (", interval, ")"))
    }
    selected_items <- start + 0:(size - 1) * interval
    sample_items <- row_numbers[selected_items]
  } else if (method == "interval" && use_mus) {
    # Fixed interval monetary unit sampling
    interval <- sum(book_values) / size
    if (start > interval) {
      stop(paste0("'start' must be an integer <= the selection interval (", interval, ")"))
    }
    selected_units <- start + 0:(size - 1) * interval
    sample_items <- NULL
    for (i in 1:size) {
      selected_item <- which(selected_units[i] <= cumsum(book_values))[1]
      sample_items <- c(sample_items, row_numbers[selected_item])
    }
  } else if (method == "sieve") {
    # Modified sieve sampling
    stopifnot("'method = sieve' does not accomodate 'units = items'" = use_mus)
    ri <- book_values / stats::runif(length(book_values), 0, 1)
    sample_items <- row_numbers[order(-ri)]
    sample_items <- sample_items[1:size]
  }
  names_match <- match(unique(sample_items), names(table(sample_items)))
  row_match <- match(unique(sample_items), row_numbers)
  count <- as.numeric(table(sample_items)[names_match])
  sample <- cbind(unique(sample_items), count, data[row_match, ])
  colnames(sample) <- c("row", "times", colnames(data))
  result <- list()
  result[["data"]] <- as.data.frame(data)
  result[["sample"]] <- as.data.frame(sample, row.names = seq_len(nrow(sample)))
  result[["n.req"]] <- size
  result[["n.units"]] <- sum(count)
  result[["n.items"]] <- nrow(sample)
  result[["N.units"]] <- switch(units,
    "items" = nrow(data),
    "values" = sum(book_values)
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
  class(result) <- c(class(result), "jfaSelection")
  return(result)
}
