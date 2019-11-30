#' Cox and Snell bound
#'
#' Calculates the Cox and Snell confidence bound for the maximum error in an
#' audit population according to the methodology described by Cox & Snell (1979).
#'
#' @usage cox.snell(bookValues, auditValues, priorPi = 0.10, priorMu = 0.40, priorA = 1, priorB = 6,
#'            confidence = 0.95)
#'
#' @param bookValues A vector of book values from sample.
#' @param auditValues A vector of corresponding audit values from the sample.
#' @param priorPi A value between 0 and 1 defining the prior expected error rate.
#' @param priorMu A value between 0 and 1 defining the prior expected mean taint.
#' @param priorA A value between 0 and 1 defining the prior expected successes.
#' @param priorB A value between 0 and 1 defining the prior expected failures.
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
#' @references Dworin, L., & Grimlund, R. A. (1986). Dollar-unit sampling: A
#' comparison of the quasi-Bayesian and moment bounds. Accounting Review, 36-57.
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
#' cox.snell(bookValues,
#'           auditValues,
#'           confidence = 0.95)
#'
#' @keywords bound
#'
#' @export

cox.snell <- function(bookValues,
                      auditValues,
                      priorPi = 0.10,
                      priorMu = 0.40,
                      priorA = 1,
                      priorB = 6,
                      confidence = 0.95){

  if(!(length(bookValues) == length(auditValues)))
    stop("bookValues must be the same length as auditValues")

  n                 <- length(auditValues)
  t                 <- bookValues - auditValues
  z                 <- t / bookValues
  z                 <- ifelse(z < 0, 0, z)
  z                 <- ifelse(z > 1, 1, z)
  z                 <- subset(z, z > 0)
  M                 <- length(z)
  z_bar             <- mean(z)
  if(M == 0){ z_bar <- 0 }
  posterior.part.1  <- (M + priorA) / (M + priorB)
  posterior.part.2  <- ((priorMu * (priorB - 1)) + (M * z_bar)) /
                       (n + (priorA / priorPi))
  posterior         <- posterior.part.1 * posterior.part.2 *
                        rf(n = 10000, df1 = (2*(M+priorA)), df2 = (2*(M+priorB)))
  bound             <- as.numeric(quantile(posterior, probs = confidence, na.rm = TRUE))
  return(bound)
}
