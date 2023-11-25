# Copyright (C) 2020-2023 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

#' Audit Sampling: Planning
#'
#' @description \code{planning()} is used to calculate a minimum sample size for
#' audit samples. It allows specification of statistical requirements for the
#' sample with respect to the performance materiality or the precision. The
#' function returns an object of class \code{jfaPlanning} that can be used with
#' associated \code{summary()} and \code{plot()} methods.
#'
#' @usage planning(
#'   materiality = NULL,
#'   min.precision = NULL,
#'   expected = 0,
#'   likelihood = c("poisson", "binomial", "hypergeometric"),
#'   conf.level = 0.95,
#'   N.units = NULL,
#'   by = 1,
#'   max = 5000,
#'   prior = FALSE
#' )
#'
#' @param materiality   a numeric value between 0 and 1 specifying the
#'   performance materiality (i.e., the maximum tolerable misstatement in the
#'   population) as a fraction. Can be \code{NULL}, but \code{min.precision}
#'   should be specified in that case.
#' @param min.precision a numeric value between 0 and 1 specifying the minimum
#'   precision (i.e., the estimated upper bound minus the estimated most likely
#'   error) as a fraction. Can be \code{NULL}, but \code{materiality} should be
#'   specified in that case.
#' @param expected      a numeric value between 0 and 1 specifying the expected
#'   (tolerable) misstatements in the sample relative to the total sample size,
#'   or a number (>= 1) specifying the expected (tolerable) number of
#'   misstatements in the sample. It is advised to set this value conservatively
#'   to minimize the probability of the observed misstatements in the sample
#'   exceeding the expected misstatements, which would imply that insufficient
#'   work has been done in the end and that additional samples are required.
#' @param likelihood    a character specifying the likelihood of the data.
#'   Possible options are \code{poisson} (default) for the Poisson likelihood,
#'   \code{binomial} for the binomial likelihood, or \code{hypergeometric} for
#'   the hypergeometric likelihood. See the details section for more information
#'   about the possible likelihoods.
#' @param conf.level    a numeric value between 0 and 1 specifying the
#'   confidence level (i.e., 1 - audit risk / detection risk).
#' @param N.units       a numeric value larger than 0 specifying the total
#'   number of units in the population. Required for the \code{hypergeometric}
#'   likelihood.
#' @param by            an integer larger than 0 specifying the increment
#'   between acceptable sample sizes (e.g., \code{increment = 5} considers only
#'   sample sizes of 5, 10, 15, ...).
#' @param max           an integer larger than 0 specifying the sample size at
#'   which the algorithm terminates (e.g., \code{max = 100} will terminate the
#'   algorithm at \emph{n} = 100).
#' @param prior         a logical specifying whether to use a prior distribution,
#'   or an object of class \code{jfaPrior} or \code{jfaPosterior}. If this
#'   argument is specified as \code{FALSE} (default), the function performs
#'   classical planning. If this argument is specified as \code{TRUE} or as a
#'   prior from \code{auditPrior}, this function performs Bayesian planning
#'   using a prior that is conjugate to the specified \code{likelihood}.
#'
#' @details This section elaborates on the available input options for the
#' \code{likelihood} argument and the corresponding conjugate prior
#' distributions used by \code{jfa}.
#'
#' \itemize{
#'  \item{\code{poisson}:        The Poisson distribution is an approximation of
#'    the binomial distribution. The Poisson distribution is defined as:
#'    \deqn{f(\theta, n) = \frac{\lambda^\theta e^{-\lambda}}{\theta!}}. The
#'    conjugate \emph{gamma(\eqn{\alpha, \beta})} prior has probability density
#'    function:
#'    \deqn{p(\theta; \alpha, \beta) = \frac{\beta^\alpha \theta^{\alpha - 1}
#'    e^{-\beta \theta}}{\Gamma(\alpha)}}.}
#'  \item{\code{binomial}:       The binomial distribution is an approximation
#'  of the hypergeometric distribution. The binomial distribution is defined as:
#'  \deqn{f(\theta, n, x) = {n \choose x} \theta^x (1 - \theta)^{n - x}}. The
#'  conjugate \emph{beta(\eqn{\alpha, \beta})} prior has probability density
#'  function: \deqn{p(\theta; \alpha, \beta) = \frac{1}{B(\alpha, \beta)}
#'  \theta^{\alpha - 1} (1 - \theta)^{\beta - 1}}.}
#'  \item{\code{hypergeometric}: The hypergeometric distribution is defined as:
#'  \deqn{f(x, n, K, N) = \frac{{K \choose x} {N - K \choose n - x}}
#'  {{N \choose n}}}. The conjugate \emph{beta-binomial(\eqn{\alpha, \beta})}
#'  prior (Dyer and Pierce, 1993) has probability mass function:
#'  \deqn{f(x, n, \alpha, \beta) = {n \choose x}
#'  \frac{B(x + \alpha, n - x + \beta)}{B(\alpha, \beta)}}.}
#' }
#'
#' @return An object of class \code{jfaPlanning} containing:
#'
#' \item{conf.level}{a numeric value between 0 and 1 giving the confidence
#'   level.}
#' \item{x}{a numeric value larger than, or equal to, 0 giving (the proportional
#'   sum of) the tolerable errors in the sample.}
#' \item{n}{an integer larger than 0 giving the minimal sample size.}
#' \item{ub}{a numeric value between 0 and 1 giving the expected upper bound.}
#' \item{precision}{a numeric value between 0 and 1 giving the expected
#'   precision.}
#' \item{p.value}{a numeric value giving the expected one-sided p-value.}
#' \item{K}{if \code{likelihood = 'hypergeometric'}, an integer larger than 0
#'   giving the assumed population errors.}
#' \item{N.units}{an integer larger than 0 giving the number of units in the
#'   population (only returned if \code{N.units} is specified).}
#' \item{materiality}{a numeric value between 0 and 1 giving the performance
#'   materiality if specified.}
#' \item{min.precision}{a numeric value between 0 and 1 giving the minimum
#'   precision if specified.}
#' \item{expected}{a numeric value larger than, or equal to, 0 giving the
#'   expected misstatement input.}
#' \item{likelihood}{a character indicating the likelihood.}
#' \item{errorType}{a character indicating the expected misstatements input.}
#' \item{iterations}{an integer giving the number of iterations of the
#'   algorithm.}
#' \item{prior}{if a prior distribution is specified, an object of class
#'   \code{jfaPrior} that contains information about the prior distribution.}
#' \item{posterior}{if a prior distribution is specified, an object of class
#'   \code{jfaPosterior} that contains information about the expected posterior
#'   distribution.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}}
#'          \code{\link{selection}}
#'          \code{\link{evaluation}}
#'
#' @references Derks, K., de Swart, J., van Batenburg, P., Wagenmakers, E.-J.,
#'   & Wetzels, R. (2021). Priors in a Bayesian audit: How integration of
#'   existing information into the prior distribution can improve audit
#'   transparency and efficiency. \emph{International Journal of Auditing},
#'   25(3), 621-636. \doi{10.1111/ijau.12240}
#' @references Derks, K., de Swart, J., Wagenmakers, E.-J., Wille, J., &
#'   Wetzels, R. (2021). JASP for audit: Bayesian tools for the auditing
#'   practice. \emph{Journal of Open Source Software}, \emph{6}(68), 2733.
#'   \doi{10.21105/joss.02733}
#' @references Dyer, D. and Pierce, R.L. (1993). On the choice of the prior
#' distribution in hypergeometric sampling. \emph{Communications in Statistics -
#' Theory and Methods}, 22(8), 2125-2146. \doi{10.1080/03610929308831139}
#'
#' @keywords audit evaluation planning prior
#'
#' @examples
#' # Classical planning
#' planning(materiality = 0.03)
#'
#' # Bayesian planning using a default prior
#' planning(materiality = 0.03, prior = TRUE)
#'
#' # Bayesian planning using a custom prior
#' prior <- auditPrior(method = "impartial", materiality = 0.05)
#' planning(materiality = 0.05, prior = prior)
#' @export

planning <- function(materiality = NULL,
                     min.precision = NULL,
                     expected = 0,
                     likelihood = c("poisson", "binomial", "hypergeometric"),
                     conf.level = 0.95,
                     N.units = NULL,
                     by = 1,
                     max = 5000,
                     prior = FALSE) {
  # Input checking
  likelihood <- match.arg(likelihood)
  is_jfa_prior <- inherits(prior, "jfaPrior") || inherits(prior, "jfaPosterior")
  is_bayesian <- isTRUE(prior) || is_jfa_prior
  mcmc_prior <- is_jfa_prior && prior[["likelihood"]] == "mcmc"
  analytical <- TRUE
  if (is_jfa_prior && !mcmc_prior) {
    conjugate_prior <- likelihood == prior[["likelihood"]]
    possible_match <- likelihood %in% c("poisson", "binomial", "hypergeometric") && prior[["likelihood"]] %in% c("poisson", "binomial", "hypergeometric")
    if (!conjugate_prior && possible_match) {
      likelihood <- prior[["likelihood"]]
      conjugate_prior <- TRUE
    }
    if (!is.null(prior[["N.units"]]) && is.null(N.units)) {
      message(paste0("Using 'N.units = ", prior[["N.units"]], "' from 'prior'"))
      N.units <- prior[["N.units"]]
    }
    if (!is.null(materiality) && is.null(prior[["hypotheses"]])) {
      hypotheses <- list()
      hypotheses[["hypotheses"]] <- .hyp_string(materiality, "less")
      hypotheses[["materiality"]] <- materiality
      hypotheses[["alternative"]] <- "less"
      hypotheses[["p.h1"]] <- .hyp_prob(TRUE, materiality, prior[["likelihood"]], prior[["description"]]$alpha, prior[["description"]]$beta, 0, N.units, N.units)
      hypotheses[["p.h0"]] <- .hyp_prob(FALSE, materiality, prior[["likelihood"]], prior[["description"]]$alpha, prior[["description"]]$beta, 0, N.units, N.units)
      hypotheses[["odds.h1"]] <- hypotheses[["p.h1"]] / hypotheses[["p.h0"]]
      hypotheses[["odds.h0"]] <- 1 / hypotheses[["odds.h1"]]
      hypotheses[["density"]] <- .hyp_dens(materiality, prior[["likelihood"]], prior[["description"]]$alpha, prior[["description"]]$beta, N.units, N.units)
      prior[["hypotheses"]] <- hypotheses
    }
  } else if (mcmc_prior) {
    conjugate_prior <- FALSE
    analytical <- FALSE
    prior_samples <- .rsample(prior[["fitted.density"]], getOption("jfa.iterations", 1e5))
    if (!is.null(materiality) && is.null(prior[["hypotheses"]])) {
      hypotheses <- list()
      hypotheses[["hypotheses"]] <- .hyp_string(materiality, "less")
      hypotheses[["materiality"]] <- materiality
      hypotheses[["alternative"]] <- "less"
      hypotheses[["p.h1"]] <- .hyp_prob(TRUE, materiality, analytical = FALSE, samples = prior_samples)
      hypotheses[["p.h0"]] <- .hyp_prob(FALSE, materiality, analytical = FALSE, samples = prior_samples)
      hypotheses[["odds.h1"]] <- hypotheses[["p.h1"]] / hypotheses[["p.h0"]]
      hypotheses[["odds.h0"]] <- 1 / hypotheses[["odds.h1"]]
      prior[["hypotheses"]] <- hypotheses
    }
  } else if (isTRUE(prior)) {
    prior <- auditPrior("default", likelihood, N.units, materiality = materiality, conf.level = conf.level)
    conjugate_prior <- TRUE
  } else if (!isFALSE(prior)) {
    stop("object in 'prior' is incompatible")
  }
  stopifnot("missing value for 'conf.level'" = !is.null(conf.level))
  valid_confidence <- is.numeric(conf.level) && length(conf.level) == 1 && conf.level > 0 && conf.level < 1
  stopifnot("'conf.level' must be a single value between 0 and 1" = valid_confidence)
  stopifnot("missing value for 'materiality' or `min.precision`" = !(is.null(materiality) && is.null(min.precision)))
  if (!is.null(materiality)) {
    valid_materiality <- is.numeric(materiality) && materiality > 0 && materiality < 1
    stopifnot("'materiality' must be a single value between 0 and 1" = valid_materiality)
  }
  if (!is.null(min.precision)) {
    valid_precision <- is.numeric(min.precision) && min.precision > 0 && min.precision < 1
    stopifnot("'min.precision' must be a single value between 0 and 1" = valid_precision)
  }
  n <- NULL
  if (expected >= 0 && expected < 1) {
    error_type <- "percentage"
    if (!is.null(materiality)) {
      valid_expected <- is.numeric(expected) && expected < materiality
      stopifnot("'expected' must be a single value < 'materiality'" = valid_expected)
    }
  } else if (expected >= 1) {
    error_type <- "integer"
    if (expected %% 1 != 0 && likelihood %in% c("binomial", "hypergeometric") && !is_bayesian) {
      expected <- ceiling(expected)
      message(paste0("Using 'expected = ", expected, "' since 'expected' must be a single integer >= 0"))
    }
  }
  stopifnot("'expected' must be an integer < 'max'" = expected < max)
  if (is.null(materiality)) {
    materiality <- 1
  }
  if (is.null(min.precision)) {
    min.precision <- 1
  }
  if (likelihood == "hypergeometric") {
    stopifnot("missing value for 'N.units'" = !is.null(N.units))
    valid_units <- is.numeric(N.units) && length(N.units) == 1 && N.units > 0
    stopifnot("'N.units' must be a single value > 0" = valid_units)
    valid_units_expected <- N.units > expected
    stopifnot("'N.units' must be > expected" = valid_units_expected)
    N.units <- ceiling(N.units)
    if (!is.null(materiality)) {
      if (expected >= 1) {
        valid_expected <- (expected < ceiling(materiality * N.units))
        stopifnot("'expected' / 'N.units' must be < 'materiality'" = valid_expected)
      } else {
        valid_expected <- (ceiling(expected * N.units) < ceiling(materiality * N.units))
        stopifnot("'expected' * 'N.units' must be < 'materiality' * 'N.units'" = valid_expected)
      }
    }
  }
  sframe <- seq(from = 0, to = max, by = by)
  sframe[1] <- 1
  iter <- 1
  sufficient <- FALSE
  K <- NULL
  if (!is.null(N.units)) {
    K <- 0:N.units
  }
  # Compute results
  while (!sufficient && iter <= length(sframe)) {
    i <- sframe[iter]
    x <- switch(error_type,
      "percentage" = expected * i,
      "integer" = expected
    )
    if (likelihood == "hypergeometric") {
      x <- ceiling(x)
      population.x <- ceiling(materiality * N.units)
    }
    while (i <= x) {
      sframe <- sframe[-iter]
      i <- sframe[iter]
    }
    if (is_bayesian) {
      if (conjugate_prior) {
        alpha <- prior[["description"]]$alpha + x
        if (likelihood == "poisson") {
          beta <- prior[["description"]]$beta + i
        } else {
          beta <- prior[["description"]]$beta + i - x
        }
        bound <- .comp_ub_bayes("less", conf.level, likelihood, alpha, beta, K - x, N.units - i, analytical, prior_samples)
        mle <- .comp_mode_bayes(likelihood, alpha, beta, K, N.units - i, analytical, prior_samples)
        if (likelihood == "hypergeometric") {
          bound <- bound / N.units
          mle <- mle / N.units
        }
      } else {
        stopifnot("likelihood = 'hypergeometric' does not support non-conjugate priors" = likelihood != "hypergeometric")
        samples <- .mcmc_cp(likelihood, x, i, prior)
        mle <- .comp_mode_bayes(analytical = FALSE, samples = samples[, 1])
        bound <- .comp_ub_bayes("less", conf.level, analytical = FALSE, samples = samples[, 1])
      }
    } else {
      if (likelihood == "binomial") {
        x <- ceiling(x)
      }
      bound <- .comp_ub_freq("less", conf.level, likelihood, i, x, x, N.units)
      if (likelihood == "hypergeometric") {
        bound <- bound / N.units
      }
      mle <- x / i
      if (materiality < 1) {
        p.val <- .comp_pval("less", materiality, likelihood, i, x, x, N.units, population.x)
      }
    }
    sufficient <- bound < materiality && (bound - mle) < min.precision
    if (sufficient) {
      n <- i
    } else {
      iter <- iter + 1
    }
  }
  stopifnot("the sample size is larger than 'max'" = !is.null(n))
  if (!is.null(N.units) && n > N.units) {
    message("The sample size is larger than 'N.units'")
  }
  # Initialize main results
  result <- list()
  result[["conf.level"]] <- conf.level
  result[["x"]] <- x
  result[["n"]] <- n
  result[["ub"]] <- bound
  result[["precision"]] <- bound - mle
  if (!is_bayesian && materiality < 1) {
    result[["p.value"]] <- p.val
  }
  result[["N.units"]] <- N.units
  if (likelihood == "hypergeometric") {
    result[["K"]] <- population.x
  }
  result[["materiality"]] <- materiality
  result[["min.precision"]] <- min.precision
  result[["expected"]] <- expected
  result[["likelihood"]] <- likelihood
  result[["errorType"]] <- error_type
  result[["iterations"]] <- iter
  # Prior and posterior distribution
  if (is_bayesian) {
    result[["prior"]] <- prior
    if (conjugate_prior) {
      # Parameters
      post_alpha <- result[["prior"]]$description$alpha + result[["x"]]
      if (likelihood == "poisson") {
        post_beta <- result[["prior"]]$description$beta + result[["n"]]
      } else {
        post_beta <- result[["prior"]]$description$beta + result[["n"]] - result[["x"]]
      }
      prior_samples <- NULL
      post_samples <- NULL
    } else {
      prior_samples <- samples[, 2]
      post_samples <- samples[, 1]
    }
    post_N <- result[["N.units"]] - result[["n"]]
    post_K <- K - result[["x"]]
    # Initialize posterior distribution
    posterior <- list()
    posterior[["posterior"]] <- .functional_form(likelihood, post_alpha, post_beta, post_N, conjugate_prior)
    posterior[["likelihood"]] <- if (conjugate_prior) likelihood else "mcmc"
    posterior[["method"]] <- if (conjugate_prior) "sample" else "mcmc"
    result[["posterior"]] <- posterior
    # Description
    description <- list()
    description[["density"]] <- .functional_density(likelihood, conjugate_prior)
    description[["n"]] <- result[["n"]]
    description[["x"]] <- result[["x"]]
    if (conjugate_prior) {
      description[["alpha"]] <- post_alpha
      description[["beta"]] <- post_beta
      description[["implicit.x"]] <- post_alpha - 1
      if (likelihood == "poisson") {
        description[["implicit.n"]] <- post_beta
      } else {
        description[["implicit.n"]] <- post_beta - description[["implicit.x"]]
      }
    } else {
      result[["posterior"]][["fitted.density"]] <- .bounded_density(post_samples)
    }
    result[["posterior"]][["description"]] <- description
    # Statistics
    statistics <- list()
    statistics[["mode"]] <- .comp_mode_bayes(likelihood, post_alpha, post_beta, post_K, post_N, conjugate_prior, post_samples)
    statistics[["mean"]] <- .comp_mean_bayes(likelihood, post_alpha, post_beta, post_N, conjugate_prior, post_samples)
    statistics[["median"]] <- .comp_median_bayes(likelihood, post_alpha, post_beta, post_K, post_N, conjugate_prior, post_samples)
    statistics[["var"]] <- .comp_var_bayes(likelihood, post_alpha, post_beta, post_N, conjugate_prior, post_samples)
    statistics[["skewness"]] <- .comp_skew_bayes(likelihood, post_alpha, post_beta, post_N, conjugate_prior, post_samples)
    statistics[["entropy"]] <- .comp_entropy_bayes(likelihood, post_alpha, post_beta, conjugate_prior, post_samples)
    statistics[["ub"]] <- .comp_ub_bayes("less", conf.level, likelihood, post_alpha, post_beta, post_K, post_N, conjugate_prior, post_samples)
    statistics[["precision"]] <- statistics[["ub"]] - statistics[["mode"]]
    result[["posterior"]][["statistics"]] <- statistics
    # Hypotheses
    if (materiality < 1) {
      hypotheses <- list()
      hypotheses[["hypotheses"]] <- .hyp_string(materiality, "less")
      hypotheses[["materiality"]] <- materiality
      hypotheses[["alternative"]] <- "less"
      hypotheses[["p.h1"]] <- .hyp_prob(TRUE, materiality, likelihood, post_alpha, post_beta, result[["x"]], N.units, post_N, conjugate_prior, post_samples)
      hypotheses[["p.h0"]] <- .hyp_prob(FALSE, materiality, likelihood, post_alpha, post_beta, result[["x"]], N.units, post_N, conjugate_prior, post_samples)
      hypotheses[["odds.h1"]] <- hypotheses[["p.h1"]] / hypotheses[["p.h0"]]
      hypotheses[["odds.h0"]] <- 1 / hypotheses[["odds.h1"]]
      hypotheses[["bf.h1"]] <- hypotheses[["odds.h1"]] / result[["prior"]][["hypotheses"]]$odds.h1
      hypotheses[["bf.h0"]] <- 1 / hypotheses[["bf.h1"]]
      result[["posterior"]][["hypotheses"]] <- hypotheses
    }
    # Additional info
    result[["posterior"]]$N.units <- N.units
    result[["posterior"]]$conf.level <- conf.level
    class(result[["posterior"]]) <- c("jfaPosterior", "list")
  }
  class(result) <- c("jfaPlanning", "list")
  return(result)
}
