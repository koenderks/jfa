#' Stringer bound with Meikle's adjustment
#'
#' Calculates the Stringer confidence bound for the maximum error in an audit
#' population with Meikle's adjustment to incorporate understatements.
#'
#' @usage stringer.meikle(bookValues, auditValues, confidence = 0.95)
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
#' \code{\link{stringer.bickel}} \code{\link{stringer.modified}}
#'
#' @references Meikle, G. R. (1972). Statistical Sampling in an Audit Context:
#' An Audit Technique. Canadian Institute of Chartered Accountants.
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
#' stringer.meikle(bookValues = sample$bookValues,
#'                 auditValues = sample$auditValues,
#'                 confidence = 0.95)
#'
#' @keywords bound
#'
#' @export

stringer.meikle <- function(bookValues,
                            auditValues,
                            confidence = 0.95){

  if(!(length(bookValues) == length(auditValues)))
    stop("bookValues must be the same length as auditValues")

  bound           <- auditR::stringer.bound(bookValues, auditValues, confidence)
  n               <- length(auditValues)
  t               <- bookValues - auditValues
  z               <- t / bookValues
  zmin            <- sort(subset(z, z < 0), decreasing = FALSE)
  Mmin            <- length(zmin)
  if(Mmin > 0){
    prop.sum.min  <- qbeta(1 + 1, n - 1, p = confidence) * abs(zmin[1])
    if(Mmin > 2){
      prop.sum.min.2  <- 0
      for(i in 2:Mmin){
        prop.sum.min.2 <- prop.sum.min.2 +
                          (qbeta(i + 1, n - 1, p = confidence) -
                           qbeta((i-1) + 1, n - 1, p = confidence)) *
                          abs(zmin[i])
      }
      prop.sum.min    <- prop.sum.min + prop.sum.min.2
    }
    bound             <- bound - prop.sum.min
  }
  return(bound)
}
