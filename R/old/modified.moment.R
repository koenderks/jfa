#' Modified Moment bound
#'
#' Calculates the Modified Moment confidence bound for the maximum error in an
#' audit population according to the methodology described by Dworin &
#' Grimlund (1986).
#'
#' @usage modified.moment(bookValues, auditValues, pop.type = "inventory", confidence = 0.95)
#'
#' @param bookValues A vector of book values from sample.
#' @param auditValues A vector of corresponding audit values from the sample.
#' @param pop.type A character defining the type of population audited.
#' \emph{inventory} for inventory populations. \emph{accounts} for populations
#' of accounts receivable.
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
#' modified.moment(bookValues = sample$bookValues,
#'                 auditValues = sample$auditValues,
#'                 pop.type = "inventory",
#'                 confidence = 0.95)
#'
#' @keywords bound
#'
#' @export

modified.moment <- function(bookValues,
                            auditValues,
                            pop.type = "inventory",
                            confidence = 0.95){

  if(!(length(bookValues) == length(auditValues)))
    stop("bookValues must be the same length as auditValues")

  n               <- length(auditValues)
  t               <- bookValues - auditValues
  z               <- t / bookValues
  z               <- subset(z, z != 0)
  M               <- length(z)
  if(pop.type == "inventory" & length(z) > 0){
    zstar         <- 0.81 * (1-0.667 * tanh(10*abs(mean(z))))
  } else if(pop.type == "inventory" & length(z) == 0){
    zstar         <- 0.81 * (1-0.667 * tanh(10*0))
  }
  if(pop.type == "accounts" & length(z) > 0){
    zstar         <- 0.81 * (1-0.667 * tanh(10 * mean(z))) * (1+0.667 * tanh(M/10))
  } else if(pop.type == "accounts" & length(z) == 0){
    zstar         <- 0.81 * (1-0.667 * tanh(10 * 0)) * (1+0.667 * tanh(0/10))
  }
  ncm1_z          <- (zstar^1 + sum(z^1)) / (M + 1)
  ncm2_z          <- (zstar^2 + sum(z^2)) / (M + 1)
  ncm3_z          <- (zstar^3 + sum(z^3)) / (M + 1)
  ncm1_e          <- (M+1)/(n+2)
  ncm2_e          <- ((M+2)/(n+3)) * ncm1_e
  ncm3_e          <- ((M+3)/(n+4)) * ncm2_e
  ncm1_t          <- ncm1_e * ncm1_z
  ncm2_t          <- (ncm1_e * ncm2_z + ((n - 1) * ncm2_e * ncm1_z^2)) / n
  ncm3_t          <- ((ncm1_e * ncm3_z + (3 * (n - 1) * ncm2_e * ncm1_z *
                      ncm2_z)) / n^2) + (((n - 1) * (n - 2) * ncm3_e *
                      ncm1_z^3)/(n^2))
  cm2_t           <- ncm2_t - ncm1_t^2
  cm3_t           <- ncm3_t - (3 * ncm1_t * ncm2_t) + (2 * ncm1_t^3)
  A               <- (4 * cm2_t^3)/(cm3_t^2)
  B               <- cm3_t / (2 * cm2_t)
  G               <- ncm1_t - ((2 * cm2_t^2)/cm3_t)
  bound           <- G + (A * B * (1 + (qnorm(confidence, mean = 0, sd = 1)/
                     sqrt(9*A)) - (1/(9*A)))^3)
  return(bound)
}
