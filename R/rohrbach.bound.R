#' Rohrbach's Augmented Variance Estimator Bound
#'
#' Calculates Rohrbach's (1993) augmented variance estimator confidence bound for
#' the maximum error in an audit population.
#'
#' @usage rohrbach.bound(bookValues, auditValues, population.size, delta = 2.7, confidence = 0.95)
#'
#' @param bookValues A vector of book values from sample.
#' @param auditValues A vector of corresponding audit values from the sample.
#' @param population.size An integer representing the size of the population.
#' @param delta A value representing the adjustment factor for the variance.
#' Rohrbach (1993) argued that the smallest value of delta that yields nominal
#' coverage equals 2.7.
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
#' @references Rohrbach, K. J. (1993). Variance augmentation to achieve nominal
#' coverage probability in sampling from audit populations. Auditing, 12(2), 79.
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
#' rohrbach.bound(bookValues = sample$bookValues,
#'                auditValues = sample$auditValues,
#'                population.size = 2400,
#'                confidence = 0.95)
#'
#' @keywords bound
#'
#' @export

rohrbach.bound <- function(bookValues,
                           auditValues,
                           population.size,
                           delta = 2.7,
                           confidence = 0.95){

  if(!(length(bookValues) == length(auditValues)))
    stop("bookValues must be the same length as auditValues")

  n           <- length(auditValues)
  N           <- population.size
  t           <- bookValues - auditValues
  z           <- t / bookValues
  w           <- 1 - z
  mu          <- mean(z)
  variance    <- sum(w^2)/n - (2-(delta/n)) * ((1/2) * ((sum(w^2)/n) - var(w)))
  bound       <- mu + qnorm(p = confidence) * sqrt((1-(n/N)) * (variance/n))
  return(bound)
}
