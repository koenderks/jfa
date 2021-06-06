#' Function to compute Bayes factors for audit sampling
#'
#' @description This function computes Bayes factors for audit sampling from summary statistics of an audit sample. By default, the Bayes factor is computed using an impartial prior distribution on the misstatement (Derks et al. 2021b). However, the arguments \code{nPrior} and \code{kPrior} can be used to specify an alternative prior distribution (Derks et al. 2021a).
#'
#' For more details on how to use this function, see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage auditBF(materiality, n, k, expectedError = 0, likelihood = 'binomial', 
#'         nPrior = NULL, kPrior = NULL, N = NULL, log = FALSE)
#' 
#' @param materiality   a numeric value between 0 and 1 specifying the performance materiality (i.e., the maximum upper limit) as a fraction of the total population size. Can be \code{NULL} for some methods.
#' @param n             an integer specifying the number of seen items.
#' @param k             a number larger than zero specifying the observed proportional error (i.e., sum of taints) in the sample.
#' @param expectedError a numeric value between 0 and 1 specifying the expected errors in the sample relative to the total sample size, or a numeric value (>= 1) that represents the sum of expected errors in the sample. It is advised to set this value conservatively to minimize the probability of the observed errors exceeding the expected errors, which would imply that insufficient work has been done in the end.
#' @param likelihood    a character specifying the likelihood assumed when updating the prior distribution. This can be either \code{binomial} for the binomial likelihood and beta prior distribution, \code{poisson} for the Poisson likelihood and gamma prior distribution, or \code{hypergeometric} for the hypergeometric likelihood and beta-binomial prior distribution. See the details section for more information about the available likelihoods.
#' @param nPrior        numeric value larger than, or equal to, 0 specifying the sample size of the sample equivalent to the prior information.
#' @param kPrior        a numeric value larger than, or equal to, 0 specifying the sum of errors in the sample equivalent to the prior information.
#' @param N             an integer larger than 0 specifying the total population size. Only required when \code{likelihood = 'hypergeometric'}.
#' @param log           whether to compute the logarithm of the Bayes factor.
#'
#' @details This section elaborates on the available likelihoods and corresponding prior distributions for the \code{likelihood} argument.
#' 
#' \itemize{
#'  \item{\code{binomial}:         The binomial likelihood is often used as a likelihood for attributes sampling \emph{with} replacement. The likelihood function is defined as: \deqn{p(x) = {n \choose k} p^k (1 - p)^{n - k}} The conjugate \emph{beta(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{1}{B(\alpha, \beta)} x^{\alpha - 1} (1 - x)^{\beta - 1}}}
#'  \item{\code{poisson}:          The Poisson likelihood is often used as a likelihood for monetary unit sampling (MUS). The likelihood function is defined as: \deqn{p(x) = \frac{\lambda^x e^{-\lambda}}{x!}} The conjugate \emph{gamma(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{\beta^\alpha x^{\alpha - 1} e^{-\beta x}}{\Gamma(\alpha)}}}
#'  \item{\code{hypergeometric}:   The hypergeometric likelihood is used as a likelihood for sampling \emph{without} replacement. The likelihood function is defined as: \deqn{p(x = k) = \frac{{K \choose k} {N - K \choose n - k}}{{N \choose n}}} The conjugate \emph{beta-binomial(\eqn{\alpha, \beta})} prior (Dyer and Pierce, 1993) has probability density function: \deqn{f(k | n, \alpha, \beta) = {n \choose k} \frac{B(k + \alpha, n - k + \beta)}{B(\alpha, \beta)}} }
#' }
#'
#' @return A value for the Bayes factor in favor of the hypothesis of tolerable misstatement against the hypothesis of intolerable misstatement.
#' 
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#' 
#' @seealso \code{\link{auditPrior}} \code{\link{planning}} \code{\link{selection}} \code{\link{evaluation}} \code{\link{report}}
#' 
#' @references Derks, K., de Swart, J., van Batenburg, P. Wagenmakers, E.-J., & Wetzels, R. (2021a). Priors in a Bayesian audit: How integrating information into the prior distribution can improve audit transparency and efficiency. In Press.
#' @references Derks, K., de Swart, J., Wagenmakers, E.-J., & Wetzels, R. (2021b). A default Bayesian hypothesis test for audit sampling.
#'
#' @keywords prior distribution audit Bayes factor
#'
#' @examples  
#' # Compute a default Bayes factor from an impartial prior
#' auditBF(materiality = 0.05, n = 50, k = 1)
#' 
#' # Compute a Bayes factor from a negligible prior
#' auditBF(materiality = 0.05, n = 50, k = 1, nPrior = 0, kPrior = 0)
#' 
#' @export

auditBF <- function(materiality, n, k, expectedError = 0, likelihood = 'binomial', 
                    nPrior = NULL, kPrior = NULL, N = NULL, log = FALSE) {
  
  if(materiality <= 0 || materiality >= 1)
    stop('The input for the performance materiality must be a value between 0 and 1.')
  
  if(n <= 0 || !(n%%1 == 0))
    stop('n must be an integer larger than zero.')
  
  if(k < 0)
    stop('k must be a number equal to, or larger than, zero.')
  
  if(k > n)
    stop('k cannot be larger than n.')
  
  if((is.null(nPrior) && !is.null(kPrior)) || (!is.null(nPrior) && is.null(kPrior)))
    warning("Falling back to an impartial prior distribution, since both 'nPrior' and 'kPrior' must be specified to use a custom prior distribution.")
  
  if(!is.null(nPrior) && !is.null(kPrior)) {
    # Create a prior distribution on the basis of an earlier sample (Derks et al. 2021a)
    p <- jfa::auditPrior(confidence = 0.95, materiality, expectedError, likelihood, method = 'sample', sampleN = nPrior, sampleK = kPrior, N = N)
  } else {
    # Create the impartial prior distribution (Derks et al. 2021b)
    p <- jfa::auditPrior(confidence = 0.95, materiality, expectedError, likelihood, method = 'median', N = N)
  }

  # Compute the Bayes factor in favor of tolerable misstatement
  bfminplus <- jfa::evaluation(confidence = 0.95, materiality, nSumstats = n, kSumstats = k, prior = p, N = N)$posterior$hypotheses$bf
  
  # Transform to logarithm if the user wants to
  if(log)
    bfminplus <- log(bfminplus)
  
  return(bfminplus)
}
