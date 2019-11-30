#' Modified Stringer bound
#'
#' Calculates the Modified Stringer confidence bound for the maximum error in an
#' audit population with the Pap and van Zuijlen (1992) adjustment.
#'
#' @usage stringer.modified(bookValues, auditValues, confidence = 0.95)
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
#' @seealso \code{\link{stringer.bound}} \code{\link{stringer.bickel}}
#' \code{\link{stringer.meikle}} \code{\link{stringer.lta}}
#'
#' @references Pap, G., & van Zuijlen, M. C. (1996). On the asymptotic behaviour
#' of the Stringer bound 1. Statistica Neerlandica, 50(3), 367-389.
#' @references Stringer, K. W. (1963). Practical aspects of statistical sampling
#' in auditing. In Proceedings of the Business and Economic Statistics Section
#' (pp. 405-411). American Statistical Association.
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
#' stringer.modified(bookValues = sample$bookValues,
#'                   auditValues = sample$auditValues,
#'                   confidence = 0.95)
#'
#' @keywords bound
#'
#' @export

stringer.modified <- function(bookValues,
                              auditValues,
                              confidence = 0.95){

  if(!(length(bookValues) == length(auditValues)))
    stop("bookValues must be the same length as auditValues")

  bound         <- auditR::stringer.bound(bookValues, auditValues, confidence)
  n             <- length(auditValues)
  t             <- bookValues - auditValues
  z             <- t / bookValues
  z             <- ifelse(z < 0, 0, z)
  z             <- sort(subset(z, z > 0))
  if(length(z) > 0){
    c           <- 0
    for(i in 1:length(z)){
      c         <- c + (((n - 2*i + 1)/(2 * sqrt(i*(n - i + 1)))) * rev(z)[i])
    }
    c           <- (1/n) * c
    sigma       <- (1/n) * sum((z - mean(z))^2)
    correction  <- (c-sqrt(sigma)) / sqrt(n) * qnorm(p = confidence,
                                                     lower.tail = TRUE)
    bound       <- bound - correction
  }
  return(bound)
}
