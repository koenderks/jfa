#' Simple Random Sampling for Audit Populations
#'
#' This function takes as input a data frame containing an audit population,
#' which needs at least a column indicating the book values of the line items.
#'
#' @usage random.sampling(data, bookCol, sample.size)
#'
#' @param data A data frame containing at least the book values of the audit
#' population.
#' @param bookCol A character indicating the name of the column used for the book
#' values.
#' @param sample.size An integer representing the number of line items to sample.
#'
#' @return A data frame containing the sample values.
#'
#' @section Details: EMPTY FOR NOW
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso
#'
#' @references
#'
#' @examples
#' # Create an imaginary data set
#' bookValues   <- rgamma(n = 2400, shape = 1, rate = 0.001)
#' error.rate   <- 0.1
#' error        <- sample(0:1, 2400, TRUE, c(1-error.rate, error.rate))
#' taint        <- rchisq(n = 2400, df = 1) / 10
#' auditValues  <- bookValues - (error * taint * bookValues)
#' frame        <- data.frame( bookValues = round(bookValues,2),
#'                             auditValues = round(auditValues,2))
#' # Sample from the population
#' random.sampling(data = frame,
#'                 bookCol = "bookValues",
#'                 sample.size = 100)
#'
#' @keywords sampling
#'
#' @export

random.sampling <- function(data,
                            bookCol,
                            sample.size){

  if(!bookCol%in%colnames(data))
    stop("Data frame does not contain the specified book column name")

  if(sample.size > nrow(data))
    stop("Cannot take a sample larger than the population")

  bookValues    <- data[, bookCol]
  probs         <- bookValues / sum(bookValues)
  sample.no     <- sample(1:nrow(data),
                          size = sample.size,
                          replace = FALSE,
                          prob = probs)
  sample        <- data[sample.no, ]
  lineNo        <- rownames(sample)
  sample        <- cbind(lineNo, sample)
  rownames(sample) <- 1:nrow(sample)
  return(sample)
}
