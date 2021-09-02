#' Bayes Factors for Audit Sampling
#'
#' @description This function computes Bayes factors for audit sampling from summary statistics of an audit sample. By default, the Bayes factor is computed using an impartial prior distribution on the misstatement (Derks et al., 2021). However, the arguments \code{nPrior} and \code{kPrior} can be used to specify an alternative prior distribution (Derks et al., 2021).
#'
#' For more details on how to use this function, see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage auditBF(x, n, materiality, likelihood = 'binomial', BF10 = TRUE,
#'         log = FALSE, N.units = NULL, alpha = NULL, beta = NULL)
#' 
#' @param x             a number larger than zero specifying the observed proportional error (i.e., sum of taints) in the sample.
#' @param n             an integer larger than 0 specifying the number of items in the sample (i.e., the sample size).
#' @param materiality   a numeric value between 0 and 1 specifying the performance materiality (i.e., the maximum upper limit) as a fraction of the total population size. Can be \code{NULL} for some methods.
#' @param likelihood    a character specifying the likelihood assumed when updating the prior distribution. This can be either \code{binomial} for the binomial likelihood and beta prior distribution, \code{poisson} for the Poisson likelihood and gamma prior distribution, or \code{hypergeometric} for the hypergeometric likelihood and beta-binomial prior distribution. See the details section for more information about the available likelihoods.
#' @param BF10          logical; if \code{TRUE}, the Bayes factor computed is in favor or tolerable misstatement. If \code{FALSE}, the Bayes factor is in favor of intolerable misstatement.
#' @param log           logical; if \code{TRUE}, the Bayes factor is given as log(bf).
#' @param N.units       an integer larger than 0 specifying the total number of sampling units in the population. Only required when \code{likelihood = 'hypergeometric'}.
#' @param alpha         a numeric value specifying the \eqn{\alpha} parameter of the prior distribution. If specified, overrides the default parameters from the impartial prior distribution.
#' @param beta          a numeric value specifying the \eqn{\beta} parameter of the prior distribution. If specified, overrides the default parameters from the impartial prior distribution.
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
#' @return A numeric value for the Bayes factor.
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
#' auditBF(x = 1, n = 50, materiality = 0.05)
#' 
#' # Compute a Bayes factor from a negligible prior
#' auditBF(x = 1, n = 50, materiality = 0.05, alpha = 1, beta = 1)
#' 
#' @export

auditBF <- function(x, n, materiality, likelihood = 'binomial', BF10 = TRUE,
                    log = FALSE, N.units = NULL, alpha = NULL, beta = NULL) {
  if(materiality <= 0 || materiality >= 1)
    stop("'materiality' must be a single number between 0 and 1")
  if(n <= 0 || !(n%%1 == 0))
    stop("'n' must be a single number larger than 0")
  if(x < 0)
    stop("'k' must be a single number equal to or larger than 0")
  if(x > n)
    stop("'k' must be a single number equal to or smaller than 'n'")
  if(is.null(alpha) && !is.null(beta))
    stop("'alpha' is missing for evaluation")
  if(!is.null(alpha) && is.null(beta))
    stop("'beta' is missing for evaluation")
  if(!is.null(alpha) && !is.null(beta)) {
    # Create a prior distribution on the basis of the raw parameters
    prior <- auditPrior(materiality = materiality, method = 'custom', likelihood = likelihood, alpha = alpha, beta = beta, N.units = N.units)
  } else {
    # Create the impartial prior distribution (Derks et al., 2021)
    prior <- auditPrior(materiality = materiality, method = 'median', likelihood = likelihood, N.units = N.units)
  }
  # Compute the Bayes factor in favor of tolerable misstatement
  bf <- evaluation(materiality = materiality, x = x, n = n, prior = prior, N.units = N.units)$posterior$hypotheses$bf
  if (!BF10)
    bf <- 1 / bf
  # Transform to logarithm if the user wants to
  if(log)
    bf <- log(bf)
  return(bf)
}
