#' Stringer bound with Bickel's adjustment
#'
#' Calculates the Stringer confidence bound for the maximum error in an audit
#' population with bickels's adjustment for less consevativeness.
#'
#' @usage stringer.bickel(bookValues, auditValues, confidence = 0.95)
#'
#' @param bookValues A vector of book values from sample.
#' @param auditValues A vector of corresponding audit values from the sample.
#' @param confidence The amount of confidence desired from the bound
#' (on a scale from 0 to 1), defaults to 95\% confidence.
#'
#' @return An estimate of the mean taint per dollar unit in the population
#'
#' @section Details: EMPTY FOR NOW
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{stringer.bound}} \code{\link{stringer.lta}}
#' \code{\link{stringer.meikle}} \code{\link{stringer.modified}}
#'
#' @references Bickel, P. J. (1992). Inference and auditing: the Stringer bound.
#' International Statistical Review, 197-209.
#' @references Stringer, K. W. (1963). Practical aspects of statistical sampling
#'  in auditing. In Proceedings of the Business and Economic Statistics Section
#'  (pp. 405-411). American Statistical Association.
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
#' stringer.bickel(bookValues = sample$bookValues,
#'                 auditValues = sample$auditValues,
#'                 confidence = 0.95)
#'
#' @keywords bound
#'
#' @export

stringer.bickel <- function(bookValues,
                            auditValues,
                            confidence = 0.95){

  if(!(length(bookValues) == length(auditValues)))
    stop("bookValues must be the same length as auditValues")

  n               <- length(auditValues)
  alpha           <- 1 - confidence
  t               <- bookValues - auditValues
  z               <- t / bookValues
  z               <- sort(subset(z, z > 0), decreasing = TRUE)
  M               <- length(z)
  if(M == 0){
    bound         <- 1 - alpha^(1/n)
  } else if (M == 1){
    bound         <- z[1] *
                      ((1/n) + qbeta(1 + 1, n - 1, p = confidence) -
                      (qbinom(p = confidence,size = n,
                              prob = qbeta(1 + 1, n - 1, p = confidence),
                              lower.tail = TRUE)/n))
  } else if (M >= 2){
    bound.part.1        <- ((M/n) * mean(z))
    bound.part.2        <- (qnorm(p = confidence,lower.tail = TRUE)/sqrt(n))
    bound.part.3        <- (qbeta(M + 1, n - M, p = confidence) *
                              (var(z) + (1 - qbeta(M + 1, n - M, p = confidence)
                                         * mean(z)^2)))^(1/2)
    bound               <- bound.part.1 + bound.part.2 * bound.part.3
  }
  return(bound)
}
