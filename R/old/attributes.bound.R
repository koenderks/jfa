#' Binomial Bound for Attributes Testing
#'
#' Calculates a confidence bound for the maximum error in an audit
#' population according to the Binomial distribution used in attributes testing.
#'
#' @usage attributes.bound(bookValues, auditValues, confidence = 0.95)
#'
#' @param bookValues A vector of book values from sample.
#' @param auditValues A vector of corresponding audit values from the sample.
#' @param confidence The amount of confidence desired from the bound
#' (on a scale from 0 to 1), defaults to 95\% confidence.
#'
#' @return An estimate of the mean taint per dollar unit in the population.
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
#' # Draw a sample
#' samp.probs   <- frame$bookValues/sum(frame$bookValues)
#' sample.no    <- sample(1:nrow(frame), 100, FALSE, samp.probs)
#' sample       <- frame[sample.no, ]
#' # Calculate bound
#' attributes.bound(bookValues = sample$bookValues,
#'                  auditValues = sample$auditValues,
#'                  confidence = 0.95)
#'
#' @keywords bound
#'
#' @export

attributes.bound <- function(bookValues,
                             auditValues,
                             confidence = 0.95){

  if(!(length(bookValues) == length(auditValues)))
    stop("bookValues must be the same length as auditValues")

  n                       <- length(auditValues)
  t                       <- bookValues - auditValues
  z                       <- t / bookValues
  m                       <- length(subset(z, z > 0))
  bound                   <- qbeta(shape1 = m + 1, shape2 = n - m, p = confidence)
  return(bound)
}
