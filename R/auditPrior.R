#' Prior Distributions for Audit Sampling
#'
#' @description This function creates a prior distribution for the misstatement parameter \eqn{\theta} in an audit sampling model. The prior can be used in the \code{planning()} and \code{evaluation()} functions via their \code{prior} argument. The function returns an object of class \code{jfaPrior} which can be used with associated \code{summary()} and \code{plot()} methods.
#'
#' For more details on how to use this function, see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage auditPrior(method = 'default', likelihood = c('poisson', 'binomial', 'hypergeometric'), 
#'            N.units = NULL, alpha = NULL, beta = NULL, materiality = NULL, expected = 0, 
#'            ir = NULL, cr = NULL, ub = NULL, p.hmin = NULL, x = NULL, 
#'            n = NULL, factor = NULL, conf.level = 0.95)
#' 
#' @param method          a character specifying the method by which the prior distribution is constructed. Defaults to \code{default} which incorporates no existing information. Other options are \code{strict}, \code{arm}, \code{bram}, \code{impartial}, \code{hyp}, \code{sample}, and \code{factor}. See the details section for more information about the available methods.
#' @param likelihood      a character specifying the likelihood assumed when updating the prior distribution. This can be either \code{poisson} (default) for the Poisson likelihood and gamma prior distribution, \code{biomial} for the binomial likelihood and beta prior distribution, or \code{hypergeometric} for the hypergeometric likelihood and beta-binomial prior distribution. See the details section for more information about the available likelihoods.
#' @param expected        a numeric value between 0 and 1 specifying the expected errors in the sample relative to the total sample size, or a numeric value (>= 1) that represents the sum of expected errors in the sample. It is advised to set this value conservatively to minimize the probability of the observed errors exceeding the expected errors, which would imply that insufficient work has been done in the end.
#' @param conf.level      a numeric value between 0 and 1 specifying the confidence level to be used in the planning. Defaults to 0.95 for 95\% confidence. Used to calculate the upper bound of the prior distribution.
#' @param materiality     a numeric value between 0 and 1 specifying the performance materiality (i.e., the maximum upper limit) as a fraction of the total population size. Can be \code{NULL} for some methods.
#' @param N.units         an numeric value larger than 0 specifying the total number of units in the population. Optional unless \code{likelihood = 'hypergeometric'}.
#' @param ir              if \code{method = 'arm'}, a numeric value between 0 and 1 specifying the inherent risk in the audit risk model. Defaults to 1 for 100\% risk.
#' @param cr              if \code{method = 'arm'}, a numeric value between 0 and 1 specifying the internal control risk in the audit risk model. Defaults to 1 for 100\% risk.
#' @param ub              if \code{method = 'bram'}, a numeric value between 0 and 1 specifying the upper bound for the prior distribution as a fraction of the population size.
#' @param p.hmin          if \code{method = 'hyp'}, a numeric value between 0 and 1 specifying the prior probability of the hypothesis of tolerable misstatement (H-: \eqn{\theta <} materiality).
#' @param x               if \code{method = 'sample'} or \code{method = 'factor'}, a numeric value larger than, or equal to, 0 specifying the sum of errors in the sample equivalent to the prior information.
#' @param n               if \code{method = 'sample'} or \code{method = 'factor'}, an integer larger than, or equal to, 0 specifying the sample size of the sample equivalent to the prior information.
#' @param factor          if \code{method = 'factor'}, a numeric value between 0 and 1 specifying the weighting factor for the results of the sample equivalent to the prior information.
#' @param alpha           if \code{method = 'param'}, a numeric value specifying the \eqn{\alpha} parameter of the prior distribution.
#' @param beta            if \code{method = 'param'}, a numeric value specifying the \eqn{\beta} parameter of the prior distribution.
#' 
#' @details \code{auditPrior} is used to define prior distributions for parameters in \code{jfa} models. To perform Bayesian audit sampling, you must assign a prior distribution to the misstatement parameter \eqn{\theta}. 
#'          The prior is a probability distribution that reflects the existing information about the parameter before seeing a sample. To keep the priors proper, the \code{default} priors used by \code{jfa} are very diffuse,
#'          meaning they contain minimal prior information. However, it is strongly recommended to use an informed prior distribution when possible.
#'
#' @details This section elaborates on the available options for the \code{method} argument.
#'
#' \itemize{
#'  \item{\code{default}:    This method produces \emph{gamma(1, 1)}, \emph{beta(1, 1)}, and \emph{beta-binomial(N, 1, 1)} prior distributions which incorporate minimal information about the possible values of the misstatement.}
#'  \item{\code{strict}:     This method produces \emph{gamma(1, 0)}, \emph{beta(1, 0)}, and \emph{beta-binomial(N, 1, 0)} prior distributions. Note that these prior distributions are improper and yield the same sample sizes and upper limits as classical techniques.}
#'  \item{\code{param}:      This method constructs a prior distribution on the basis of manually specified \eqn{\alpha} and \eqn{\beta} parameters.}
#'  \item{\code{impartial}:  This method constructs a prior distribution under which the prior probability of tolerable misstatement (\eqn{\theta <} materiality) is equal to the prior probability of intolerable misstatement (\eqn{\theta >} materiality).}
#'  \item{\code{hyp}:        This method constructs a prior distribution with manual prior probabilities for the hypotheses of tolerable misstatement (\eqn{\theta <} materiality) and intolerable misstatement (\eqn{\theta >} materiality). This method requires specification of the \code{p.hmin} argument.}
#'  \item{\code{arm}:        This method constructs a prior distribution by translating the risks of material misstatement (inherent risk and internal control risk) from the audit risk model to an implicit sample. The method requires specification of the \code{ir} (inherent risk) and \code{cr} (internal control risk) arguments.}
#'  \item{\code{bram}:       This method constructs a prior distribution using the Bayesian audit risk assessment model (BRAM) in which the expected most likely error and expected upper bound of the misstatement must be specified. The method requires specification of the \code{ub} argument.}
#'  \item{\code{sample}:     This method constructs a prior distribution on the basis of an earlier observed sample. This method requires specification of the \code{n} and \code{x} arguments.}
#'  \item{\code{factor}:     This method constructs a prior distribution on the basis of an earlier sample in combination with a weighting factor. This method requires specification of the \code{n}, \code{x}, and \code{factor} arguments.}
#' }
#'
#' @details This section elaborates on the available likelihoods and corresponding prior distributions for the \code{likelihood} argument.
#' 
#' \itemize{
#'  \item{\code{poisson}:          The Poisson likelihood is often used as a likelihood for monetary unit sampling (MUS). The likelihood function is defined as: \deqn{p(x) = \frac{\lambda^x e^{-\lambda}}{x!}} The conjugate \emph{gamma(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{\beta^\alpha x^{\alpha - 1} e^{-\beta x}}{\Gamma(\alpha)}}}
#'  \item{\code{binomial}:         The binomial likelihood is often used as a likelihood for attributes sampling \emph{with} replacement. The likelihood function is defined as: \deqn{p(x) = {n \choose k} p^k (1 - p)^{n - k}} The conjugate \emph{beta(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{1}{B(\alpha, \beta)} x^{\alpha - 1} (1 - x)^{\beta - 1}}}
#'  \item{\code{hypergeometric}:   The hypergeometric likelihood is used as a likelihood for sampling \emph{without} replacement. The likelihood function is defined as: \deqn{p(x = k) = \frac{{K \choose k} {N - K \choose n - k}}{{N \choose n}}} The conjugate \emph{beta-binomial(\eqn{\alpha, \beta})} prior (Dyer and Pierce, 1993) has probability density function: \deqn{f(k | n, \alpha, \beta) = {n \choose k} \frac{B(k + \alpha, n - k + \beta)}{B(\alpha, \beta)}} }
#' }
#'
#' @return An object of class \code{jfaPrior} containing:
#' 
#' \item{prior}{a string describing the functional form of the prior distribution.}
#' \item{description}{a list containing a description of the prior distribution, including the parameters of the prior distribution and the implicit sample on which the prior distribution is based.}
#' \item{statistics}{a list containing statistics of the prior distribution, including the mean, mode, median, and upper bound of the prior distribution.}
#' \item{specifics}{a list containing specifics of the prior distribution that vary depending on the \code{method}.}
#' \item{hypotheses}{if \code{materiality} is specified, a list containing information about the hypotheses, including prior probabilities and odds for the hypothesis of tolerable misstatement (H-) and the hypothesis of intolerable misstatement (H+).}
#' \item{method}{a character indicating the method by which the prior distribution is constructed.}
#' \item{likelihood}{a character indicating the assumed likelihood.}
#' \item{materiality}{if \code{materiality} is specified, a numeric value between 0 and 1 indicating the materiality used to construct the prior distribution.}
#' \item{expected}{a numeric value larger than, or equal to, 0 indicating the input for the number of expected errors.}
#' \item{conf.level}{a numeric value between 0 and 1 indicating the confidence level used.}
#' \item{N.units}{if \code{N} is specified, an integer larger than 0 indicating the population size.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#' 
#' @seealso \code{\link{planning}} \code{\link{selection}} \code{\link{evaluation}} \code{\link{report}}
#' 
#' @references Derks, K., de Swart, J., Wagenmakers, E.-J., Wille, J., & Wetzels, R. (2019). JASP for audit: Bayesian tools for the auditing practice.
#' @references Derks, K., de Swart, J., van Batenburg, P., Wagenmakers, E.-J., & Wetzels, R. (2021). Priors in a Bayesian audit: How integration of existing information into the prior distribution can improve audit transparency and efficiency. \emph{International Journal of Auditing}, 1-16.
#'
#' @keywords prior distribution audit
#'
#' @examples  
#' # Translate inherent risk (ir) and control risk (cr) to a prior distribution
#' auditPrior(method = 'arm', likelihood = 'poisson', expected = 0.025,
#'            materiality = 0.05, ir = 1, cr = 0.6)
#'
#' # Equal prior probabilities
#' auditPrior(method = 'impartial', likelihood = 'poisson', materiality = 0.05)
#'
#' # Custom prior distribution
#' auditPrior(method = 'param', likelihood = 'poisson', alpha = 1, beta = 10)
#'
#' @export

auditPrior <- function(method = 'default', likelihood = c('poisson', 'binomial', 'hypergeometric'), 
                       N.units = NULL, alpha = NULL, beta = NULL, materiality = NULL, expected = 0, 
                       ir = NULL, cr = NULL, ub = NULL, p.hmin = NULL, x = NULL, 
                       n = NULL, factor = NULL, conf.level = 0.95) {
  likelihood <- match.arg(likelihood)
  if (!(method %in% c("default", "strict", 'param', "impartial", "hyp", "arm", "bram", "sample", "factor")) || length(method) != 1)
    stop("'method' should be one of 'default', 'strict', 'param', 'impartial', 'hyp', 'arm', 'bram', 'sample', 'factor'")
  if (is.null(conf.level))
    stop("'conf.level' is missing for prior construction")
  if (conf.level >= 1 || conf.level <= 0 || length(conf.level) != 1)
    stop("'conf.level' must be a single number between 0 and 1")
  if (expected < 0)
    stop("'expected' must be a single number >= 0")
  if (is.null(materiality) && method %in% c("impartial", "hyp", "arm"))
    stop("'materiality' is missing for prior construction")
  if (!is.null(materiality) && expected >= materiality && expected < 1)
    stop("'expected' must be a single number < 'materiality'")
  if (expected >= 1 && !(method %in% c('default', 'sample', 'factor', 'param', 'strict')))
    stop(paste0("'expected' must be a single number between 0 and 1 for 'method = ", method, "'"))
  if (likelihood == 'hypergeometric') {
    if (is.null(N.units))
      stop("'N.units' is missing for prior construction")
    if (N.units <= 0)
      stop("'N.units' must be a nonnegative")
    if (N.units%%1 != 0)
      N.units <- ceiling(N.units)
  }
  if (method == "default") { # Method 1: Minimum prior observations to make the prior proper
    prior.n <- 1 # Single earlier observation
    prior.x <- 0 # No earlier errors
  } else if (method == 'strict') { # Method 2: Improper prior distribution (with classical properties)
    prior.n <- 0 # Single earlier observation
    prior.x <- 0 # Single earlier error
  } else if (method == "arm") { # Method 3: Translate risks from the audit risk model
    if (is.null(cr))
      stop("'cr' is missing for prior construction")
    if (cr < 0 || cr > 1)
      stop("'cr' must be a single value between 0 and 1")
    if (is.null(ir))
      stop("'ir' is missing for prior construction")
    if (ir < 0 || ir > 1)
      stop("'ir' must be a single value between 0 and 1")
    dr      <- (1 - conf.level) / (ir * cr) # Calculate the required detection risk from the audit risk model
    n.plus  <- planning(conf.level = conf.level, likelihood = likelihood, expected = expected, N.units = N.units, materiality = materiality, prior = TRUE)$n # Calculate the sample size for the full detection risk  
    n.min   <- planning(conf.level = 1 - dr, likelihood = likelihood, expected = expected, N.units = N.units, materiality = materiality, prior = TRUE)$n # Calculated the sample size for the adjusted detection risk
    prior.n <- n.plus - n.min # Calculate the sample size equivalent to the increase in detection risk
    prior.x <- (n.plus * expected) - (n.min * expected) # Calculate errors equivalent to the increase in detection risk
  } else if (method == 'bram') { # Method 4: Bayesian risk assessment model
    if (is.null(ub)) # Check if the value for the upper bound is present
      stop("'ub' is missing for prior construction")
    if (ub <= 0 || ub >= 1 || ub <= expected) # Check if the value for the upper bound is valid
      stop("'ub' must be a single number between 0 and 1 and >= 'expected'")
    if (likelihood == 'poisson' && expected > 0) { # Perform approximation described in Stewart (2013) on p. 45.
      r <- expected / ub
      q <- stats::qnorm(conf.level)
      prior.x <- ((( (q * r) + sqrt(3 + ((r / 3) * (4 * q^2 - 10)) - ((r^2 / 3) * (q^2 - 1) ))) / (2 * (1 - r) ))^2 + (1 / 4)) - 1
      prior.n <- prior.x / expected
    } else { # Approximation through iteration over one of the parameters
      bound  <- Inf
      prior.x <- 0
      prior.n <- 1
      while (bound > ub) {
        if (expected == 0) { # In this case, iterate over nPrior because kPrior is zero
          prior.n <- prior.n + 0.001 # Increase of 0.001 (time intensive?)
        } else { # In this case, iterate over kPrior to save computation time
          prior.x <- prior.x + 0.0001 # Increase of 0.0001 (time intensive?)
          prior.n <- if (likelihood == "poisson") prior.x / expected else 1 + prior.x / expected # Express beta in terms of alpha
        }
        bound <- switch(likelihood, 
                        "binomial" = stats::qbeta(p = conf.level, shape1 = 1 + prior.x, shape2 = prior.n - prior.x),
                        "poisson" = stats::qgamma(p = conf.level, shape = 1 + prior.x, rate = prior.n),
                        "hypergeometric" = .qbbinom(p = conf.level, N = N.units, shape1 = 1 + prior.x, shape2 = prior.n - prior.x) / N.units)
      }
    }
  } else if (method == "impartial" || method == "hyp") {
    if(method == "impartial") { # Method 5: Equal prior probabilities
      p.h0 <- p.h1 <- 0.5
    } else if (method == "hyp") { # Method 6: Custom prior probabilities
      if (is.null(p.hmin)) # Must have the prior probabilities and materiality
        stop("'p.hmin' is missing for prior construction")
      p.h1 <- p.hmin
      p.h0 <- 1 - p.h1 # Calculate p(H+)
    }
    if (expected == 0) { # Formulas for zero expected errors
      prior.n <- switch(likelihood,
                        "poisson" = -(log(p.h0) / materiality),
                        "binomial" = log(p.h0) / log(1 - materiality),
                        "hypergeometric" = log(p.h0) / log(1 - materiality))
      prior.x <- 0
    } else { # Approximation through iteration over alpha parameter = more accurate than approximation through formulas
      median <- Inf
      prior.x <- 0
      while (median > materiality) {
        prior.x <- prior.x + 0.0001 # Increase of 0.0001 (time intensive?)
        prior.n <- if (likelihood == "poisson") prior.x / expected else 1 + prior.x / expected # Express beta in terms of alpha
        median <- switch(likelihood, # Calculate the median for the current parameters
                         "binomial" = stats::qbeta(p = p.h1, shape1 = 1 + prior.x, shape2 = prior.n - prior.x),
                         "poisson" = stats::qgamma(p = p.h1, shape = 1 + prior.x, rate = prior.n),
                         "hypergeometric" = .qbbinom(p = p.h1, N = N.units, shape1 = 1 + prior.x, shape2 = prior.n - prior.x) / N.units)
      }
    }
  } else if (method == 'sample' || method == 'factor') { # Method 7: Earlier sample & Method 8: Weighted earlier sample
    if (is.null(n))
      stop("'n' is missing for prior construction")
    if (is.null(x))
      stop("'x' is missing for prior construction")
    if (method == 'factor') {
      if (is.null(factor))
        stop("'factor' is missing for prior construction")
    } else {
      factor <- 1
    }
    prior.n <- n * factor # Earlier sample size
    prior.x <- x * factor # Earlier errors
  } else if (method == 'param') { # Method 9: User specified prior distribution
    if (is.null(alpha))
      stop("'alpha' is missing for prior construction")
    if (alpha <= 0)
      stop("'alpha' must be a single value > 0")
    if (is.null(beta))
      stop("'beta' is missing for prior construction")
    if (beta < 0)
      stop("'beta' must be a single value >= 0")
    prior.x <- alpha - 1
    prior.n <- switch(likelihood, 'binomial' = beta + prior.x, 'poisson' = beta, 'hypergeometric' = beta + prior.x) 
  }
  # Create the description output
  description                 <- list()
  description[["density"]]    <- switch(likelihood, "poisson" = "gamma", "binomial" = "beta", "hypergeometric" = "beta-binomial")
  description[["alpha"]]      <- 1 + prior.x
  description[["beta"]]       <- switch(likelihood, "poisson" = prior.n, "binomial" = prior.n - prior.x, "hypergeometric" = prior.n - prior.x)
  description[["implicit.x"]] <- prior.x
  description[["implicit.n"]] <- prior.n
  description[["proper"]]     <- description[["alpha"]] != 0 && description[["beta"]] != 0
  # Create the prior string
  prior.string <- switch(likelihood, 
                         "poisson" = paste0("gamma(\u03B1 = ", round(description[["alpha"]], 3), ", \u03B2 = ", round(description[["beta"]], 3), ")"),
                         "binomial" = paste0("beta(\u03B1 = ", round(description[["alpha"]], 3), ", \u03B2 = ", round(description[["beta"]], 3), ")"),
                         "hypergeometric" = paste0("beta-binomial(N = ", N.units, ", \u03B1 = ", round(description[["alpha"]], 3), ", \u03B2 = ", round(description[["beta"]], 3), ")"))
  # Create the statistics section
  statistics           <- list()
  statistics[["mode"]] <- switch(likelihood, 
                                 "poisson" = (description[["alpha"]] - 1) / description[["beta"]],
                                 "binomial" = (description[["alpha"]] - 1) / (description[["alpha"]] + description[["beta"]] - 2),
                                 "hypergeometric" = .modebbinom(N = N.units, shape1 = description[["alpha"]], shape2 = description[["beta"]]))
  statistics[["mean"]] <- switch(likelihood, 
                                 "poisson" = description[["alpha"]] / description[["beta"]],
                                 "binomial" = description[["alpha"]] / (description[["alpha"]] + description[["beta"]]),
                                 "hypergeometric" = description[["alpha"]] / (description[["alpha"]] + description[["beta"]]) * N.units)
  statistics[["median"]] <- switch(likelihood, 
                                   "poisson" = stats::qgamma(0.5, shape = description[["alpha"]], rate = description[["beta"]]),
                                   "binomial" = stats::qbeta(0.5, shape1 = description[["alpha"]], shape2 = description[["beta"]]),
                                   "hypergeometric" = .qbbinom(0.5, N = N.units, shape1 = description[["alpha"]], shape2 = description[["beta"]]))
  statistics[["ub"]] <- switch(likelihood, 
                               "poisson" = stats::qgamma(conf.level, shape = description[["alpha"]], rate = description[["beta"]]),
                               "binomial" = stats::qbeta(conf.level, shape1 = description[["alpha"]], shape2 = description[["beta"]]),
                               "hypergeometric" = .qbbinom(conf.level, N = N.units, shape1 = description[["alpha"]], shape2 = description[["beta"]]))									
  statistics[["precision"]] <- statistics[["ub"]] - statistics[["mode"]]
  # Create the specifics section
  if (method != "default" && method != "strict")
    specifics             <- list()
  if (method == "impartial" || method == "hyp") {
    specifics[["p.h1"]] <- p.h1
    specifics[["p.h0"]] <- p.h0
  } else if (method == "sample" || method == "factor") {
    specifics[["x"]]      <- x
    specifics[["n"]]      <- n
    specifics[["factor"]] <- factor   
  } else if (method == "arm") {
    specifics[["ir"]]     <- ir
    specifics[["cr"]]     <- cr
  } else if (method == "bram") {
    specifics[["mode"]]   <- expected
    specifics[["ub"]]     <- ub
  } else if (method == 'param') {
    specifics[["alpha"]]  <- alpha
    specifics[["beta"]]   <- beta
  }
  # Create the hypotheses section
  if (!is.null(materiality)) {
    hypotheses                 <- list()
    hypotheses[["hypotheses"]] <- c(paste0("H-: \u0398 < ", materiality), paste0("H+: \u0398 > ", materiality))
    hypotheses[["p.h1"]]      <- switch(likelihood, 
                                        "poisson" = stats::pgamma(materiality, shape = description[["alpha"]], rate = description[["beta"]]),
                                        "binomial" = stats::pbeta(materiality, shape1 = description[["alpha"]], shape2 = description[["beta"]]),
                                        "hypergeometric" = extraDistr::pbbinom(ceiling(materiality * N.units), size = N.units, alpha = description[["alpha"]], beta = description[["beta"]]))
    hypotheses[["p.h0"]]     <- switch(likelihood, 
                                       "poisson" = stats::pgamma(materiality, shape = description[["alpha"]], rate = description[["beta"]], lower.tail = FALSE),
                                       "binomial" = stats::pbeta(materiality, shape1 = description[["alpha"]], shape2 = description[["beta"]], lower.tail = FALSE),
                                       "hypergeometric" = extraDistr::pbbinom(ceiling(materiality * N.units), size = N.units, alpha = description[["alpha"]], beta = description[["beta"]], lower.tail = FALSE))
    hypotheses[["odds.h1"]]   <- hypotheses[["p.h1"]] / hypotheses[["p.h0"]]
    hypotheses[["odds.h0"]]  <- 1 / hypotheses[["odds.h1"]]
    hypotheses[["density"]]     <- switch(likelihood,
                                          "poisson" = stats::dgamma(materiality, shape = description[["alpha"]], rate = description[["beta"]]),
                                          "binomial" = stats::dbeta(materiality, shape1 = description[["alpha"]], shape2 = description[["beta"]]),
                                          "hypergeometric" = extraDistr::dbbinom(ceiling(materiality * N.units), size = N.units, alpha = description[["alpha"]], beta = description[["beta"]]))
  }	
  # Create the main result object	
  result <- list()
  # Functional form of the prior distribution
  result[["prior"]]         <- prior.string
  result[["description"]]   <- description
  result[["statistics"]]    <- statistics
  if (method != "default" && method != "strict")
    result[["specifics"]]   <- specifics
  if (!is.null(materiality))
    result[["hypotheses"]]  <- hypotheses
  result[["method"]]        <- method
  result[["likelihood"]]    <- likelihood
  if (!is.null(materiality))
    result[["materiality"]] <- materiality
  result[["expected"]]      <- expected
  result[["conf.level"]]    <- conf.level
  result[["N.units"]]       <- N.units
  # Add class 'jfaPrior' to the result
  class(result) <- "jfaPrior"
  return(result)
}