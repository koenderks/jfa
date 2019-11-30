#' Stringer bound
#'
#' Calculates the Stringer confidence bound for the maximum error in an audit
#' population.
#'
#' @usage stringer.bound(bookValues, auditValues, confidence = 0.95)
#'
#' @param bookValues A vector of book values from sample.
#' @param auditValues A vector of corresponding audit values from the sample.
#' @param confidence The amount of confidence desired from the bound
#' (on a scale from 0 to 1), defaults to 95\% confidence.
#'
#' @return An estimate of the mean taint per dollar unit in the population
#'
#' @section Details:
#' The Stringer bound is the most well-known and used bound in the accounting
#' practice. Proposed by Stringer (1963), the bound estimates the mean taint per
#'  dollar unit.
#' It's formula is defined as:
#' \deqn{p(0; 1 - \alpha) + \sum_{j=1}^{m_+} \left[ p(j; 1 - \alpha) -
#' p(j-1; 1 - \alpha) \right] \cdot z_+_j}{p(0; 1 - \alpha) +
#' \sum [ p(j; 1 - \alpha) - p(j-1; 1 - \alpha) ] x Z}
#' where \eqn{p(j; 1 - \alpha)} equals the unique solution to:
#' \deqn{\sum^n_{k = j + 1} {n \choose k} p^k (1-p)^{n-k} = 1- \alpha}{\sum
#' (n choose k) p^k (1-p)^{n-k} = 1- \alpha}
#' and is therefore the Clopper-Pearson confidence interval for a binomial
#' parameter (Clopper-Pearson, 1934). The values \emph{Z} are the proportional
#' taints defined as \eqn{\frac{bookValues - auditValues}{bookValues}}
#' {(bookValues - auditValues) / bookValues}. Since the upper bound is only
#' defined for integer values of \emph{k}, when partial taints are observed
#' Stringer performs linear interpolation between the upper bounds that are
#' properly defined. The Stringer bound is often used for its capacity to yield
#' sensible results, even when zero errors are found. This comes with a
#' downside, as the bounds that are given by the Stringer approach are highly
#' conservative bounds due to the attribute sampling method. Additionally, the
#' Stringer bound does not accommodate understatements. However, it can be
#' corrected by using either one of 4 adjustments; Meikle's adjustment (1972),
#' the LTA adjustment (1979), Bickel's adjustment (1992) or the adjustment
#' suggested by Pap and van Zuijlen (1996).
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @references Bickel, P. J. (1992). Inference and auditing: the Stringer bound.
#' International Statistical Review, 197-209.
#' @references Clopper, C. J., & Pearson, E. S. (1934). The use of confidence or
#' fiducial limits illustrated in the case of the binomial. Biometrika, 26(4),
#' 404-413.
#' @references Leslie, D. A., Teitlebaum, A. D., & Anderson, R. J. (1979).
#' Dollar-unit sampling: a practical guide for auditors. Copp Clark Pitman;
#' Belmont, Calif.: distributed by Fearon-Pitman.
#' @references Meikle, G. R. (1972). Statistical Sampling in an Audit Context:
#' An Audit Technique. Canadian Institute of Chartered Accountants.
#' @references Pap, G., & van Zuijlen, M. C. (1996). On the asymptotic behaviour
#' of the Stringer bound 1. Statistica Neerlandica, 50(3), 367-389.
#' @references Stringer, K. W. (1963). Practical aspects of statistical sampling
#' in auditing. In Proceedings of the Business and Economic Statistics Section
#' (pp. 405-411). American Statistical Association.
#'
#' @seealso \code{\link{stringer.meikle}} \code{\link{stringer.lta}}
#' \code{\link{stringer.bickel}} \code{\link{stringer.modified}}
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
#' # Calculate Stringer bound
#' stringer.bound(bookValues = sample$bookValues,
#'                auditValues = sample$auditValues,
#'                confidence = 0.95)
#'
#' @keywords bound
#'
#' @export

stringer.bound <- function(bookValues,
                           auditValues,
                           confidence = 0.95){

  if(!(length(bookValues) == length(auditValues)))
    stop("bookValues must be the same length as auditValues")

  n                       <- length(auditValues)
  t                       <- bookValues - auditValues
  z                       <- t / bookValues
  z                       <- ifelse(z < 0, yes = 0, no = z)
  z                       <- ifelse(z > 1, yes = 1, no = z)
  z                       <- sort(subset(z, z > 0), decreasing = TRUE)
  M                       <- length(z)
  bound                   <- 1 - (1-confidence)^(1/n)
  if(M > 0){
    prop.sum              <- 0
    for(i in 1:M){
      prop.sum            <- prop.sum + (qbeta(p = confidence,
                                               shape1 = i + 1,
                                               shape2 = n - i) -
                                           qbeta(p = confidence,
                                                 shape1 = (i - 1) + 1,
                                                 shape2 = n - (i - 1)))  * z[i]
    }
    bound                 <- bound + prop.sum
  }
  return(bound)
}
