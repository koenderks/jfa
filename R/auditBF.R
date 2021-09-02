#' Function to compute Bayes factors for audit sampling
#'
#' @description This function computes Bayes factors for audit sampling from summary statistics of an audit sample. By default, the Bayes factor is computed using an impartial prior distribution on the misstatement (Derks et al., 2021). However, the arguments \code{nPrior} and \code{kPrior} can be used to specify an alternative prior distribution (Derks et al., 2021).
#'
#' For more details on how to use this function, see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage auditBF(materiality, n, k, expectedError = 0, likelihood = 'binomial', 
#'         nPrior = NULL, kPrior = NULL, N = NULL, log = FALSE)
#' 
#' @param materiality   a numeric value between 0 and 1 specifying the performance materiality (i.e., the maximum upper limit) as a fraction of the total population size. Can be \code{NULL} for some methods.
#' @param n             an integer larger than 0 specifying the number of items in the sample.
#' @param k             a number larger than zero specifying the observed proportional error (i.e., sum of taints) in the sample.
#' @param expectedError a numeric value between 0 and 1 specifying the expected errors in the sample relative to the total sample size, or a numeric value (>= 1) that represents the sum of expected errors in the sample. It is advised to set this value conservatively to minimize the probability of the observed errors exceeding the expected errors, which would imply that insufficient work has been done in the end.
#' @param likelihood    a character specifying the likelihood assumed when updating the prior distribution. This can be either \code{binomial} for the binomial likelihood and beta prior distribution, \code{poisson} for the Poisson likelihood and gamma prior distribution, or \code{hypergeometric} for the hypergeometric likelihood and beta-binomial prior distribution. See the details section for more information about the available likelihoods.
#' @param nPrior        numeric value larger than, or equal to, 0 specifying the sample size of the sample equivalent to the prior information.
#' @param kPrior        a numeric value larger than, or equal to, 0 specifying the sum of errors in the sample equivalent to the prior information.
#' @param N             an integer larger than 0 specifying the total population size. Only required when \code{likelihood = 'hypergeometric'}.
#' @param log           logical; if TRUE, the Bayes factor is given as log(bf).
#'
#' @details The Bayes Factor \eqn{BF_{-+}} quantifies how much more likely the data are to be observed under \eqn{H_{-}: \theta < \theta_{max}} than under \eqn{H_{+}: \theta > \theta_{max}}. Therefore, \eqn{BF_{-+}} can be interpreted as the relative support in the observed data for \eqn{H_{-}} versus \eqn{H_{+}}. If \eqn{BF_{-+}} is 1, there is no preference for either \eqn{H_{-}} or \eqn{H_{+}}. If \eqn{BF_{-+}} is larger than 1, \eqn{H_{-}} is preferred. If \eqn{BF_{-+}} is between 0 and 1, \eqn{H_{+}} is preferred.
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
#' @references Derks, K., de Swart, J., van Batenburg, P., Wagenmakers, E.-J., & Wetzels, R. (2021). Priors in a Bayesian audit: How integration of existing information into the prior distribution can improve audit transparency and efficiency. \emph{International Journal of Auditing}, 1-16.
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
    stop("'materiality' must be a single number between 0 and 1")
  
  if(n <= 0 || !(n%%1 == 0))
    stop("'n' must be a single number larger than 0")
  
  if(k < 0)
    stop("'k' must be a single number equal to or larger than 0")
  
  if(k > n)
    stop("'k' must be a single number equal to or smaller than 'n'")
  
  if(is.null(nPrior) && !is.null(kPrior))
    stop("'kPrior' will not be used")

  if(!is.null(nPrior) && is.null(kPrior))
    stop("'nPrior' will not be used")
    
  if(!is.null(nPrior) && !is.null(kPrior)) {
    # Create a prior distribution on the basis of an earlier sample (Derks et al., 2021)
    prior <- jfa::auditPrior(materiality = materiality, expectedError = expectedError, method = 'sample', likelihood = likelihood, sampleN = nPrior, sampleK = kPrior, N = N)
  } else {
    # Create the impartial prior distribution (Derks et al., 2021)
    prior <- jfa::auditPrior(materiality = materiality, expectedError = expectedError, method = 'median', likelihood = likelihood, N = N)
  }
  
  # Compute the Bayes factor in favor of tolerable misstatement
  bfminplus <- jfa::evaluation(materiality = materiality, nSumstats = n, kSumstats = k, prior = prior, N = N)$posterior$hypotheses$bf
  
  # Transform to logarithm if the user wants to
  if(log)
    bfminplus <- log(bfminplus)
  
  return(bfminplus)
}
