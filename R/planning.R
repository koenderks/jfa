#' Plan a Statistical Audit Sample
#'
#' @description This function calculates the minimum sample size for a statistical audit sample based on the Poisson, binomial, or hypergeometric likelihood. The function returns an object of class \code{jfaPlanning} which can be used with associated \code{summary()} and \code{plot()} methods.
#'
#' For more details on how to use this function, see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage planning(materiality = NULL, min.precision = NULL, expected = 0,
#'          likelihood = 'poisson', conf.level = 0.95, N.units = NULL,
#'          by = 1, max = 5000, prior = FALSE)
#'
#' @param materiality   a numeric value between 0 and 1 specifying the performance materiality (i.e., maximum upper limit) as a fraction of the total population size. Can be \code{NULL}, but \code{min.precision} should be specified in that case.
#' @param min.precision a numeric value between 0 and 1 specifying the minimum precision (i.e., upper bound minus most likely error) as a fraction of the total population size. Can be \code{NULL}, but \code{materiality} should be specified in that case.
#' @param expected      a numeric value between 0 and 1 specifying the expected errors in the sample relative to the total sample size, or a number (>= 1) that represents the number of expected errors in the sample. It is advised to set this value conservatively to minimize the probability of the observed errors exceeding the expected errors, which would imply that insufficient work has been done in the end.
#' @param likelihood    a character specifying the likelihood assumed in the calculation. This can be either \code{poisson} (default) for the Poisson likelihood, \code{binomial} for the binomial likelihood, or \code{hypergeometric} for the hypergeometric likelihood. See the details section for more information about the available likelihoods.
#' @param conf.level    a numeric value between 0 and 1 specifying the confidence level used in the planning. Defaults to 0.95 for 95\% confidence.
#' @param N.units       an integer larger than 0 specifying the total number of units or items in the population (i.e., the population size). Only required when \code{likelihood = 'hypergeometric'}.
#' @param prior         a logical specifying whether to use a prior distribution when planning, or an object of class \code{jfaPrior} or \code{jfaPosterior} containing the prior distribution. Defaults to \code{FALSE} for frequentist planning. If \code{TRUE}, a noninformative prior distribution is used. Chooses a conjugate gamma distribution for the Poisson likelihood, a conjugate beta distribution for the binomial likelihood, and a conjugate beta-binomial distribution for the hypergeometric likelihood.
#' @param by            an integer larger than 0 specifying the desired increment for the sample size calculation.
#' @param max           an integer larger than 0 specifying the maximum sample size that is considered in the calculation. Defaults to 5000 for efficiency. Increase this value if the sample size cannot be found due to it being too large (e.g., for a low materiality).
#' 
#' @details This section elaborates on the available likelihoods and corresponding prior distributions for the \code{likelihood} argument.
#' 
#' \itemize{
#'  \item{\code{poisson}:          The Poisson likelihood is often used as a likelihood for monetary unit sampling (MUS). The likelihood function is defined as: \deqn{p(x) = \frac{\lambda^x e^{-\lambda}}{x!}} The conjugate \emph{gamma(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{\beta^\alpha x^{\alpha - 1} e^{-\beta x}}{\Gamma(\alpha)}}}
#'  \item{\code{binomial}:         The binomial likelihood is often used as a likelihood for attributes sampling \emph{with} replacement. The likelihood function is defined as: \deqn{p(x) = {n \choose k} p^k (1 - p)^{n - k}} The conjugate \emph{beta(\eqn{\alpha, \beta})} prior has probability density function: \deqn{f(x; \alpha, \beta) = \frac{1}{B(\alpha, \beta)} x^{\alpha - 1} (1 - x)^{\beta - 1}}}
#'  \item{\code{hypergeometric}:   The hypergeometric likelihood is used as a likelihood for sampling \emph{without} replacement. The likelihood function is defined as: \deqn{p(x = k) = \frac{{K \choose k} {N - K \choose n - k}}{{N \choose n}}} The conjugate \emph{beta-binomial(\eqn{\alpha, \beta})} prior (Dyer and Pierce, 1993) has probability density function: \deqn{f(k | n, \alpha, \beta) = {n \choose k} \frac{B(k + \alpha, n - k + \beta)}{B(\alpha, \beta)}} }
#' }
#'
#' @return An object of class \code{jfaPlanning} containing:
#' 
#' \item{conf.level}{a numeric value between 0 and 1 indicating the confidence level used.}
#' \item{x}{a numeric value larger than, or equal to, 0 indicating the number of tolerable errors in the sample.}
#' \item{n}{an integer larger than 0 indicating the required sample size.}
#' \item{ub}{a numeric value between 0 and 1 indicating the expected upper bound if the sample goes according to plan.}
#' \item{precision}{a numeric value between 0 and 1 indicating the expected precision if the sample goes according to plan.}
#' \item{p.value}{a numeric value indicating the one-sided p-value.}
#' \item{K}{if \code{likelihood = 'hypergeometric'}, an integer larger than 0 indicating the assumed population errors.}
#' \item{N.units}{an integer larger than 0 indicating the population size (only returned if \code{N} is specified).}
#' \item{materiality}{a numeric value between 0 and 1 indicating the specified materiality.}
#' \item{min.precision}{a numeric value between 0 and 1 indicating the minimum precision to be obtained.}
#' \item{expected}{a numeric value larger than, or equal to, 0 indicating the expected errors input.}
#' \item{likelihood}{a character indicating the specified likelihood.}
#' \item{errorType}{a character indicating whether the expected errors where specified as a percentage or as an integer.}
#' \item{iterations}{a numeric value indicating the number of iterations used.}
#' \item{prior}{if a prior distribution is specified, an object of class \code{jfaPrior} that contains information about the prior distribution.}
#' \item{posterior}{if a prior distribution is specified, an object of class \code{jfaPosterior} that contains information about the expected posterior distribution.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}} \code{\link{selection}} \code{\link{evaluation}} \code{\link{report}}
#'
#' @references Derks, K., de Swart, J., van Batenburg, P., Wagenmakers, E.-J., & Wetzels, R. (2021). Priors in a Bayesian audit: How integration of existing information into the prior distribution can improve audit transparency and efficiency. \emph{International Journal of Auditing}, 1-16.
#' @references Dyer, D. and Pierce, R.L. (1993). On the choice of the prior distribution in hypergeometric sampling. \emph{Communications in Statistics - Theory and Methods}, 22(8), 2125 - 2146.
#'
#' @keywords planning sample size audit
#'
#' @examples
#' # Classical planning using a Poisson likelihood
#' planning(materiality = 0.05, expected = 0.025, likelihood = 'poisson')
#' 
#' # Bayesian planning using a noninformative gamma prior distribution
#' planning(materiality = 0.05, expected = 0.025, likelihood = 'poisson', 
#'          prior = TRUE)
#'
#' # Bayesian planning using an informed gamma prior distribution
#' planning(materiality = 0.05, expected = 0.025, likelihood = 'poisson', 
#'          prior = auditPrior(method = 'impartial', materiality = 0.05))
#'
#' @export

planning <- function(materiality = NULL, min.precision = NULL, expected = 0,
                     likelihood = 'poisson', conf.level = 0.95, N.units = NULL,
                     by = 1, max = 5000, prior = FALSE) {
  proper <- TRUE
  bayesian <- (class(prior) == "logical" && prior == TRUE) || class(prior) %in% c("jfaPrior", "jfaPosterior")
  # Import existing prior distribution with class 'jfaPrior' or 'jfaPosterior'
  if (class(prior) %in% c("jfaPrior", "jfaPosterior")) {
    prior.n      <- prior[["description"]]$implicit.n
    prior.x      <- prior[["description"]]$implicit.x
    likelihood   <- prior[["likelihood"]]
    if (!is.null(prior[['N.units']]))
      N.units <- prior[['N.units']]
    proper <- prior[["description"]]$alpha != 0 && prior[["description"]]$beta != 0
  } else if (prior) {
	prior.n <- 1
    prior.x <- 0
  }
  if (is.null(materiality) && is.null(min.precision))
    stop("'materiality' or `min.precision` is missing for planning")
  if (conf.level >= 1 || conf.level <= 0 || is.null(conf.level) || length(conf.level) != 1)
    stop("'conf.level' must be a single number between 0 and 1")
  if (!(likelihood %in% c("poisson", "binomial", "hypergeometric")) || length(likelihood) != 1)
    stop("'likelihood' should be one of 'poisson', 'binomial', 'hypergeometric'")
  if (!is.null(min.precision) && (min.precision <= 0 || min.precision >= 1))
    stop("'min.precision' must be a single number between 0 and 1")
  # Define a placeholder for the sample size 
  n <- NULL
  # Find out the type of expected errors (percentage vs. number)
  if (expected >= 0 && expected < 1) {
    errorType <- "percentage"
    if (!is.null(materiality) && expected >= materiality)
      stop("'expected' must be a single number < 'materiality'")
  } else if (expected >= 1) {
    errorType <- "integer"
    if (expected%%1 != 0 && likelihood %in% c("binomial", "hypergeometric") && !bayesian)
      stop("'expected' must be a nonnegative and integer")
  }
  # Set the materiality and the minimium precision to 1 if they are not specified
  if (is.null(materiality))
    materiality <- 1
  if (is.null(min.precision))
    min.precision <- 1
  # Requirements for the hypergeometric distribution
  if (likelihood == 'hypergeometric') {
    if (is.null(N.units))
      stop("'N.units' is missing for planning")
    if (N.units <= 0 || N.units%%1 != 0)
      stop("'N.units' must be nonegative and integer")
    if (!is.null(materiality) && expected >= 1 && expected >= ceiling(materiality * N.units))
      stop("'expected' / 'N.units' must be < 'materiality'")
    if (!is.null(materiality) && expected < 1 && ceiling(expected * N.units) >= ceiling(materiality * N.units))
      stop("'expected' * 'N.units' must be < 'materiality' * 'N.units'")
  }
  # Define the sampling frame (the possible sample sizes)
  sframe <- seq(from = 0, to = max, by = by)
  sframe[1] <- 1
  # Set up counters
  iter <- 1
  sufficient <- FALSE
  # Start iterating over the sampling frame
  while (!sufficient && iter < max) {
    i <- sframe[iter]
    # Find the expected errors in the sample
    x <- switch(errorType, "percentage" = expected * i, "integer" = expected)
    if (likelihood == "hypergeometric") {
      x <- ceiling(x) # Convert to an integer in the case of a hypergeometric likelihood
      population.x <- ceiling(materiality * N.units)
    }
    while (i <= x) { # Remove redundant numbers from the sampling frame for more efficient sampling
      sframe <- sframe[-iter]
      i <- sframe[iter]
    }
    if (bayesian) { # Bayesian planning
      alpha <- 1 + prior.x + x
      beta <- switch(likelihood, "poisson" = prior.n + i, "binomial" = prior.n - prior.x + i - x, "hypergeometric" = prior.n - prior.x + i - x)
      bound <- switch(likelihood, 
                      "poisson" = stats::qgamma(conf.level, shape = alpha, rate = beta),
                      "binomial" = stats::qbeta(conf.level, shape1 = alpha, shape2 = beta),
                      "hypergeometric" = .qbbinom(p = conf.level, N = N.units - i, shape1 = alpha, shape2 = beta) / N.units)
      mle <- switch(likelihood,
                    "poisson" = (alpha - 1) / beta,
                    "binomial" = (alpha - 1) / (alpha + beta - 2),
                    "hypergeometric" = .modebbinom(N = N.units - i, shape1 = alpha, shape2 = beta) / N.units)
    } else { # Classical planning
      if (likelihood == "binomial") 
        x <- ceiling(x) # Convert to an integer in the case of a binomial likelihood
      bound <- switch(likelihood, 
                      "poisson" = stats::qgamma(p = conf.level, shape = 1 + x, rate = i),
                      "binomial" = stats::qbeta(p = conf.level, shape1 = 1 + x, shape2 = i - x),
                      "hypergeometric" = .qhyper(p = conf.level, N = N.units, n = i, k = x) / N.units)
      mle <- x / i
      if (materiality < 1)
        p.val <- switch(likelihood,
                        'poisson' = stats::pgamma(q = materiality, shape = 1 + x, rate = i, lower.tail = FALSE),
                        'binomial' = stats::pbeta(q = materiality, shape1 = 1 + x, shape2 = i - x, lower.tail = FALSE),
                        'hypergeometric' = stats::phyper(q = x, m = population.x, n = N.units - population.x, k = i))
    }
    sufficient <- bound < materiality && (bound - mle) < min.precision # Sufficient work done?
    if (sufficient)
      n <- i
    iter <- iter + 1
  }
  # No sample size could be calculated, throw an error
  if (is.null(n))
    stop("the sample size is lower than 'max'")
  if (!is.null(N.units) && n > N.units) # The sample size is too large
    warning("the sample size is larger than 'N.units'")
  # Create the main results object
  result <- list()
  result[["conf.level"]]    <- conf.level
  result[["x"]]             <- x
  result[["n"]]             <- n
  result[["ub"]]            <- bound
  result[["precision"]]     <- bound - mle
  if (!bayesian && materiality < 1)
    result[["p.value"]]     <- p.val
  result[["N.units"]]       <- N.units
  if (likelihood == "hypergeometric")
    result[["K"]]           <- population.x
  result[["materiality"]]   <- materiality
  result[["min.precision"]] <- min.precision
  result[["expected"]]      <- expected
  result[["likelihood"]]    <- likelihood
  result[["errorType"]]     <- errorType
  result[["iterations"]]    <- iter
  # Create the prior distribution object	
  if (bayesian) {
    if (class(prior) == "jfaPrior" && !is.null(prior[["hypotheses"]])) {
      result[["prior"]] <- prior
    } else {
      result[["prior"]] <- auditPrior(conf.level = conf.level, materiality = result[["materiality"]], method = "sample", 
                                      likelihood = likelihood, N.units = result[["N.units"]], n = prior.n, x = prior.x)
    }
  }
  if (!is.null(result[["prior"]])) {
    # Create the expected posterior distribution object  
    result[["posterior"]] <- list()
    # Functional form of the expected posterior
    result[["posterior"]]$posterior <- switch(likelihood, 
                                              "poisson" = paste0("gamma(\u03B1 = ", round(result[["prior"]]$description$alpha + result[["x"]], 3), ", \u03B2 = ", round(result[["prior"]]$description$beta + result[["n"]], 3), ")"),
                                              "binomial" = paste0("beta(\u03B1 = ", round(result[["prior"]]$description$alpha + result[["x"]], 3), ", \u03B2 = ", round(result[["prior"]]$description$beta + result[["n"]] - result[["x"]], 3), ")"),
                                              "hypergeometric" = paste0("beta-binomial(N = ", result[["N.units"]] - result[["n"]], ", \u03B1 = ", round(result[["prior"]]$description$alpha + result[["x"]], 3), ", \u03B2 = ", round(result[["prior"]]$description$beta + result[["n"]] - result[["x"]], 3), ")"))
    result[["posterior"]]$likelihood <- likelihood
    # Create the description section
    result[["posterior"]][["description"]]          <- list()
    result[["posterior"]][["description"]]$density  <- switch(likelihood, "poisson" = "gamma", "binomial" = "beta", "hypergeometric" = "beta-binomial")
    result[["posterior"]][["description"]]$n        <- result[["n"]]
    result[["posterior"]][["description"]]$x        <- result[["x"]]
    result[["posterior"]][["description"]]$alpha    <- result[["prior"]]$description$alpha + result[["x"]]
    result[["posterior"]][["description"]]$beta     <- switch(likelihood, 
                                                              "poisson" = result[["prior"]]$description$beta + result[["n"]], 
                                                              "binomial" = result[["prior"]]$description$beta + result[["n"]] - result[["x"]], 
                                                              "hypergeometric" = result[["prior"]]$description$beta + result[["n"]] - result[["x"]])
    result[["posterior"]][["description"]]$implicit.x <- result[["expectedPosterior"]][["description"]]$alpha - 1
    result[["posterior"]][["description"]]$implicit.n <- switch(likelihood,
                                                                "poisson" = result[["posterior"]][["description"]]$beta, 
                                                                "binomial" = result[["posterior"]][["description"]]$beta - result[["posterior"]][["description"]]$implicit.x,
                                                                "hypergeometric" = result[["posterior"]][["description"]]$beta - result[["posterior"]][["description"]]$implicit.x)
    # Create the statistics section
    result[["posterior"]][["statistics"]]           <- list()
    result[["posterior"]][["statistics"]]$mode      <- switch(likelihood, 
                                                              "poisson" = (result[["posterior"]][["description"]]$alpha - 1) / result[["posterior"]][["description"]]$beta,
                                                              "binomial" = (result[["posterior"]][["description"]]$alpha - 1) / (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta - 2),
                                                              "hypergeometric" = .modebbinom(N = result[["N.units"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta))
    result[["posterior"]][["statistics"]]$mean      <- switch(likelihood, 
                                                              "poisson" = result[["posterior"]][["description"]]$alpha / result[["posterior"]][["description"]]$beta,
                                                              "binomial" = result[["posterior"]][["description"]]$alpha / (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta),
                                                              "hypergeometric" = result[["posterior"]][["description"]]$alpha / (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta) * result[["N.units"]])
    result[["posterior"]][["statistics"]]$median    <- switch(likelihood, 
                                                              "poisson" = stats::qgamma(0.5, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
                                                              "binomial" = stats::qbeta(0.5, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
                                                              "hypergeometric" = .qbbinom(0.5, N = result[["N.units"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta))
    result[["posterior"]][["statistics"]]$ub        <- switch(likelihood, 
                                                              "poisson" = stats::qgamma(conf.level, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
                                                              "binomial" = stats::qbeta(conf.level, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
                                                              "hypergeometric" = .qbbinom(conf.level, N = result[["N.units"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta))									
    result[["posterior"]][["statistics"]]$precision <- result[["posterior"]][["statistics"]]$ub - result[["posterior"]][["statistics"]]$mode
    # Create the hypotheses section
    if (result[["materiality"]] != 1) {
      result[["posterior"]][["hypotheses"]]             <- list()
      result[["posterior"]][["hypotheses"]]$hypotheses  <- c(paste0("H-: \u0398 < ", materiality), paste0("H+: \u0398 > ", materiality))
      result[["posterior"]][["hypotheses"]]$p.hmin      <- switch(likelihood, 
                                                                  "poisson" = stats::pgamma(materiality, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
                                                                  "binomial" = stats::pbeta(materiality, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
                                                                  "hypergeometric" = extraDistr::pbbinom(ceiling(materiality * result[["N.units"]]), size = result[["N.units"]] - result[["n"]], alpha = result[["posterior"]][["description"]]$alpha, beta = result[["posterior"]][["description"]]$beta))
      result[["posterior"]][["hypotheses"]]$p.hplus     <- switch(likelihood, 
                                                                  "poisson" = stats::pgamma(materiality, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta, lower.tail = FALSE),
                                                                  "binomial" = stats::pbeta(materiality, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta, lower.tail = FALSE),
                                                                  "hypergeometric" = extraDistr::pbbinom(ceiling(materiality * result[["N.units"]]), size = result[["N.units"]] - result[["n"]], alpha = result[["posterior"]][["description"]]$alpha, beta = result[["posterior"]][["description"]]$beta, lower.tail = FALSE))
      result[["posterior"]][["hypotheses"]]$odds.hmin   <- result[["posterior"]][["hypotheses"]]$p.hmin / result[["posterior"]][["hypotheses"]]$p.hplus
      result[["posterior"]][["hypotheses"]]$odds.hplus  <- 1 / result[["posterior"]][["hypotheses"]]$odds.hmin
      # For improper priors we take the posterior odds as Bayes factor
      result[["posterior"]][["hypotheses"]]$bf.hmin     <- result[["posterior"]][["hypotheses"]]$odds.hmin
      if (proper) # The prior is proper, so we divide by the prior odds
        result[["posterior"]][["hypotheses"]]$bf.hmin   <- result[["posterior"]][["hypotheses"]]$bf.hmin / result[["prior"]][["hypotheses"]]$odds.hmin
      result[["posterior"]][["hypotheses"]]$bf.hplus    <- 1 / result[["posterior"]][["hypotheses"]]$bf.hmin
    }
    result[["posterior"]][["N.units"]] <- result[["N.units"]]
    # Add class 'jfaPosterior' to the posterior distribution object
    class(result[["posterior"]]) <- "jfaPosterior"
  }
  # Add class 'jfaPlanning' to the result
  class(result) <- "jfaPlanning"
  return(result)
}