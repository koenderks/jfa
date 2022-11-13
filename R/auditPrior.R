# Copyright (C) 2020-2022 Koen Derks

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

#' Audit Sampling: Prior Distributions
#'
#' @description \code{auditPrior()} is used to create a prior distribution for Bayesian audit sampling. The interface allows a complete customization of the prior distribution as well as a formal translation of pre-existing audit information into a prior distribution. The function returns an object of class \code{jfaPrior} that can be used in the \code{planning()} and \code{evaluation()} functions via their \code{prior} argument. Objects with class \code{jfaPrior} can be further inspected via associated \code{summary()} and \code{plot()} methods.
#'
#' @usage auditPrior(method = c(
#'              "default", "strict", "param", "impartial", "hyp",
#'              "arm", "bram", "sample", "factor"
#'            ), likelihood = c('poisson', 'binomial', 'hypergeometric'),
#'            N.units = NULL, alpha = NULL, beta = NULL,
#'            materiality = NULL, expected = 0, ir = NULL, cr = NULL,
#'            ub = NULL, p.hmin = NULL, x = NULL, n = NULL,
#'            factor = NULL, conf.level = 0.95)
#'
#' @param method      a character specifying the method by which the prior distribution is constructed. Possible options are \code{default}, \code{strict}, \code{impartial}, \code{param}, \code{arm}, \code{bram}, \code{hyp}, \code{sample}, and \code{factor}. See the details section for more information.
#' @param likelihood  a character specifying the likelihood for updating the prior distribution. Possible options are \code{poisson} (default) for a conjugate gamma prior distribution, \code{binomial} for a conjugate beta prior distribution, or \code{hypergeometric} for a conjugate beta-binomial prior distribution. See the details section for more information.
#' @param N.units     a numeric value larger than 0 specifying the total number of units in the population. Only used for the \code{hypergeometric} likelihood.
#' @param alpha       a numeric value specifying the \eqn{\alpha} parameter of the prior distribution. Only used for method \code{param}.
#' @param beta        a numeric value specifying the \eqn{\beta} parameter of the prior distribution. Only used for method \code{param}.
#' @param materiality a numeric value between 0 and 1 specifying the performance materiality (i.e., the maximum tolerable misstatement) as a fraction of the total number of units in the population. Only used for methods \code{impartial}, \code{arm}, and \code{hyp}.
#' @param expected    a numeric value between 0 and 1 specifying the expected errors in the sample relative to the total sample size. Only used for methods \code{impartial}, \code{arm}, \code{bram}, and \code{hyp}.
#' @param ir          a numeric value between 0 and 1 specifying the inherent risk in the audit risk model. Only used for method \code{arm}.
#' @param cr          a numeric value between 0 and 1 specifying the internal control risk in the audit risk model. Only used for method \code{arm}.
#' @param ub          a numeric value between 0 and 1 specifying the \code{conf.level}-\% upper bound for the prior distribution as a fraction of the total number of units in the population. Only used for method \code{bram}.
#' @param p.hmin      a numeric value between 0 and 1 specifying the prior probability of the hypothesis of tolerable misstatement (H1: \eqn{\theta <} materiality). Only used for method \code{hyp}.
#' @param x           a numeric value larger than, or equal to, 0 specifying the sum of proportional errors (taints) in a prior sample. Only used for methods \code{sample} and \code{factor}.
#' @param n           a numeric value larger than 0 specifying the sample size of a prior sample. Only used for methods \code{sample} and \code{factor}.
#' @param factor      a numeric value between 0 and 1 specifying the weight of the prior sample. Only used for method \code{factor}.
#' @param conf.level  a numeric value between 0 and 1 specifying the confidence level.
#'
#' @details To perform Bayesian audit sampling you must assign a prior probability distribution to the parameter in the model, i.e., the population misstatement \eqn{\theta}.
#'          The prior distribution can incorporate pre-existing audit information about \eqn{\theta} before seeing a sample, which consequently allows for a more efficient or more accurate estimate of \eqn{\theta}.
#'          However, the default priors used in \code{jfa} are purposely indifferent towards the individual values of \eqn{\theta} in order to 'let the data speak for themselves'.
#'          Note that these default priors are a conservative choice of prior since they assume all possible misstatement to be (roughly) equally likely before seeing a data sample.
#'          It is therefore strongly recommended to construct an informed prior distribution based on pre-existing audit information if possible.
#'
#' @details This section elaborates on the available options for the \code{method} argument.
#'
#' \itemize{
#'  \item{\code{default}:   This method produces a \emph{gamma(1, 1)}, \emph{beta(1, 1)}, or \emph{beta-binomial(N, 1, 1)} prior distribution. These priors are indifferent towards the possible values of the misstatement.}
#'  \item{\code{strict}:    This method produces an improper \emph{gamma(1, 0)}, \emph{beta(1, 0)}, or \emph{beta-binomial(N, 1, 0)} prior distribution. These prior distributions exactly match sample sizes and upper limits from classical methods.}
#'  \item{\code{impartial}: This method produces an impartial prior distribution. These prior distributions assume that tolerable misstatement (\eqn{\theta <} materiality) and intolerable misstatement (\eqn{\theta >} materiality) are equally likely.}
#'  \item{\code{param}:     This method produces a \code{gamma(alpha, beta)}, \code{beta(alpha, beta)}, or \code{beta-binomial(N, alpha, beta)} prior distribution.}
#'  \item{\code{hyp}:       This method translates an assessment of the prior probability for tolerable misstatement (\eqn{\theta <} materiality) to a prior distribution.}
#'  \item{\code{arm}:       This method translates an assessment of inherent risk and internal control risk (Audit Risk Model, Derks et al., 2021) to a prior distribution.}
#'  \item{\code{bram}:      This method translates an assessment of the expected most likely error and x-\% upper bound to a prior distribution.}
#'  \item{\code{sample}:    This method translates sampling results from an earlier sample to a prior distribution.}
#'  \item{\code{factor}:    This method translates and weighs sampling results from an earlier sample to a prior distribution.}
#' }
#'
#' @details This section elaborates on the available likelihoods and corresponding prior distributions for the \code{likelihood} argument.
#'
#' \itemize{
#'  \item{\code{poisson}:        The Poisson distribution is an approximation of the binomial distribution. The Poisson distribution is defined as: \deqn{f(\theta, n) = \frac{\lambda^\theta e^{-\lambda}}{\theta!}} The conjugate \emph{gamma(\eqn{\alpha, \beta})} prior has probability density function: \deqn{p(\theta; \alpha, \beta) = \frac{\beta^\alpha \theta^{\alpha - 1} e^{-\beta \theta}}{\Gamma(\alpha)}}}
#'  \item{\code{binomial}:       The binomial distribution is an approximation of the hypergeometric distribution. The binomial distribution is defined as: \deqn{f(\theta, n, x) = {n \choose x} \theta^x (1 - \theta)^{n - x}} The conjugate \emph{beta(\eqn{\alpha, \beta})} prior has probability density function: \deqn{p(\theta; \alpha, \beta) = \frac{1}{B(\alpha, \beta)} \theta^{\alpha - 1} (1 - \theta)^{\beta - 1}}}
#'  \item{\code{hypergeometric}: The hypergeometric distribution is defined as: \deqn{f(x, n, K, N) = \frac{{K \choose x} {N - K \choose n - x}}{{N \choose n}}} The conjugate \emph{beta-binomial(\eqn{\alpha, \beta})} prior (Dyer and Pierce, 1993) has probability mass function: \deqn{f(x, n, \alpha, \beta) = {n \choose x} \frac{B(x + \alpha, n - x + \beta)}{B(\alpha, \beta)}} }
#' }
#'
#' @return An object of class \code{jfaPrior} containing:
#'
#' \item{prior}{a string describing the functional form of the prior distribution.}
#' \item{description}{a list containing a description of the prior distribution, including the parameters of the prior distribution and the implicit sample on which the prior distribution is based.}
#' \item{statistics}{a list containing statistics of the prior distribution, including the mean, mode, median, and upper bound of the prior distribution.}
#' \item{specifics}{a list containing specifics of the prior distribution that vary depending on the \code{method}.}
#' \item{hypotheses}{if \code{materiality} is specified, a list containing information about the hypotheses, including prior probabilities and odds for the hypothesis of tolerable misstatement (H1) and the hypothesis of intolerable misstatement (H0).}
#' \item{method}{a character indicating the method by which the prior distribution is constructed.}
#' \item{likelihood}{a character indicating the likelihood of the data.}
#' \item{materiality}{if \code{materiality} is specified, a numeric value between 0 and 1 giving the materiality used to construct the prior distribution.}
#' \item{expected}{a numeric value larger than, or equal to, 0 giving the input for the number of expected errors.}
#' \item{conf.level}{a numeric value between 0 and 1 giving the confidence level.}
#' \item{N.units}{if \code{N.units} is specified, the number of units in the population.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{planning}} \code{\link{selection}} \code{\link{evaluation}} \code{\link{report}}
#'
#' @references Derks, K., de Swart, J., Wagenmakers, E.-J., Wille, J., & Wetzels, R. (2021). JASP for audit: Bayesian tools for the auditing practice. \emph{Journal of Open Source Software}, \emph{6}(68), 2733.
#' @references Derks, K., de Swart, J., van Batenburg, P., Wagenmakers, E.-J., & Wetzels, R. (2021). Priors in a Bayesian audit: How integration of existing information into the prior distribution can improve audit transparency and efficiency. \emph{International Journal of Auditing}, \emph{25}(3), 621-636.
#'
#' @keywords audit evaluation planning prior
#'
#' @examples
#' # Default uniform beta(1, 1) prior distribution
#' auditPrior(method = "default", likelihood = "binomial")
#'
#' # Translate inherent risk (ir) and control risk (cr) to a gamma prior distribution
#' auditPrior(method = "arm", expected = 0.025, materiality = 0.05, ir = 1, cr = 0.6)
#'
#' # Impartial beta prior distribution (equal prior probabilities)
#' auditPrior(method = "impartial", likelihood = "binomial", materiality = 0.05)
#' @export

auditPrior <- function(method = c(
                         "default", "strict", "param", "impartial", "hyp",
                         "arm", "bram", "sample", "factor"
                       ), likelihood = c("poisson", "binomial", "hypergeometric"),
                       N.units = NULL, alpha = NULL, beta = NULL,
                       materiality = NULL, expected = 0, ir = NULL, cr = NULL,
                       ub = NULL, p.hmin = NULL, x = NULL, n = NULL,
                       factor = NULL, conf.level = 0.95) {
  method <- match.arg(method)
  likelihood <- match.arg(likelihood)
  stopifnot(
    "missing value for 'conf.level'" = !is.null(conf.level),
    "'conf.level' must be a single value between 0 and 1" = is.numeric(conf.level) && length(conf.level) == 1 && conf.level > 0 && conf.level < 1,
    "'expected' must be a single value >= 0" = expected >= 0
  )
  if (method %in% c("impartial", "hyp", "arm")) {
    stopifnot(
      "missing value for 'materiality'" = !is.null(materiality),
      "'materiality' must be a single value between 0 and 1" = is.numeric(materiality) && materiality > 0 && materiality < 1
    )
  }
  if (!is.null(materiality) && expected < 1) {
    stopifnot("'expected' must be a single value < 'materiality'" = expected < materiality)
  }
  if (expected >= 1 && !(method %in% c("default", "sample", "factor", "param", "strict"))) {
    stop(paste0("'expected' must be a single value between 0 and 1 for 'method = ", method, "'"))
  }
  if (likelihood == "hypergeometric") {
    stopifnot(
      "missing value for 'N.units'" = !is.null(N.units),
      "'N.units' must be a single value > 0" = is.numeric(N.units) && length(N.units) == 1 && N.units > 0
    )
    N.units <- ceiling(N.units)
  }
  if (method == "default") { # Method 1: Minimum prior observations to make the prior proper
    prior.n <- 1 # Single earlier observation
    prior.x <- 0 # No earlier errors
  } else if (method == "strict") { # Method 2: Improper prior distribution (with frequentist properties)
    prior.n <- 0 # No earlier observations
    prior.x <- 0 # No earlier errors
  } else if (method == "arm") { # Method 3: Translate risks from the audit risk model
    stopifnot(
      "missing value for 'ir' (inherent risk)" = !is.null(ir),
      "'ir' (inherent risk) must be a single value between 0 and 1" = ir > 0 && ir <= 1,
      "missing value for 'cr' (control risk)" = !is.null(cr),
      "'cr' (control risk) must be a single value between 0 and 1" = cr > 0 && cr <= 1,
      "ir * cr must be > 1 - conf.level" = ir * cr > 1 - conf.level
    )
    dr <- (1 - conf.level) / (ir * cr) # Calculate the required detection risk from the audit risk model
    n.plus <- planning(conf.level = conf.level, likelihood = likelihood, expected = expected, N.units = N.units, materiality = materiality, prior = TRUE)$n # Calculate the sample size for the full detection risk
    n.min <- planning(conf.level = 1 - dr, likelihood = likelihood, expected = expected, N.units = N.units, materiality = materiality, prior = TRUE)$n # Calculated the sample size for the adjusted detection risk
    prior.n <- n.plus - n.min # Calculate the sample size equivalent to the increase in detection risk
    prior.x <- (n.plus * expected) - (n.min * expected) # Calculate errors equivalent to the increase in detection risk
  } else if (method == "bram") { # Method 4: Bayesian risk assessment model
    stopifnot(
      "missing value for 'ub'" = !is.null(ub),
      "'ub' must be a single value between 0 and 1 and > 'expected'" = length(ub) == 1 && is.numeric(ub) && ub > 0 && ub < 1 && ub > expected
    )
    if (likelihood == "poisson" && expected > 0) { # Perform approximation described in Stewart (2013) on p. 45.
      r <- expected / ub
      q <- stats::qnorm(conf.level)
      prior.x <- ((((q * r) + sqrt(3 + ((r / 3) * (4 * q^2 - 10)) - ((r^2 / 3) * (q^2 - 1)))) / (2 * (1 - r)))^2 + (1 / 4)) - 1
      prior.n <- prior.x / expected
    } else { # Approximation through iteration over one of the parameters
      bound <- Inf
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
          "hypergeometric" = .qbbinom(p = conf.level, N = N.units, shape1 = 1 + prior.x, shape2 = prior.n - prior.x) / N.units
        )
      }
    }
  } else if (method == "impartial" || method == "hyp") {
    if (method == "impartial") { # Method 5: Equal prior probabilities
      p.h0 <- p.h1 <- 0.5
    } else if (method == "hyp") { # Method 6: Custom prior probabilities
      stopifnot("missing value for 'p.hmin'" = !is.null(p.hmin))
      p.h1 <- p.hmin
      p.h0 <- 1 - p.h1 # Calculate p(H+)
    }
    if (expected == 0) { # Formulas for zero expected errors
      prior.n <- switch(likelihood,
        "poisson" = -(log(p.h0) / materiality),
        "binomial" = log(p.h0) / log(1 - materiality),
        "hypergeometric" = log(2) / (log(N.units / (N.units - ceiling(materiality * N.units))))
      )
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
          "hypergeometric" = .qbbinom(p = p.h1, N = N.units, shape1 = 1 + prior.x, shape2 = prior.n - prior.x) / N.units
        )
      }
    }
  } else if (method == "sample" || method == "factor") { # Method 7: Earlier sample & Method 8: Weighted earlier sample
    stopifnot(
      "missing value for 'n'" = !is.null(n),
      "'n' must be a single value >= 0" = is.numeric(n) && length(n) == 1 && n >= 0,
      "missing value for 'x'" = !is.null(x),
      "'x' must be a single value >= 0" = is.numeric(x) && length(x) == 1 && x >= 0
    )
    if (method == "factor") {
      stopifnot("missing value for 'factor'" = !is.null(factor))
    } else {
      factor <- 1
    }
    prior.n <- n * factor # Earlier sample size
    prior.x <- x * factor # Earlier errors
  } else if (method == "param") { # Method 9: User specified prior distribution
    stopifnot(
      "missing value for 'alpha'" = !is.null(alpha),
      "'alpha' must be a single value > 0" = is.numeric(alpha) && length(alpha) == 1 && alpha > 0,
      "missing value for 'beta'" = !is.null(beta),
      "'beta' must be a single value >= 0" = is.numeric(beta) && length(beta) == 1 && beta >= 0
    )
    prior.x <- alpha - 1
    prior.n <- switch(likelihood,
      "binomial" = beta + prior.x,
      "poisson" = beta,
      "hypergeometric" = beta + prior.x
    )
  }
  # Create the description output
  description <- list()
  description[["density"]] <- switch(likelihood,
    "poisson" = "gamma",
    "binomial" = "beta",
    "hypergeometric" = "beta-binomial"
  )
  description[["alpha"]] <- 1 + prior.x
  description[["beta"]] <- switch(likelihood,
    "poisson" = prior.n,
    "binomial" = prior.n - prior.x,
    "hypergeometric" = prior.n - prior.x
  )
  description[["implicit.x"]] <- prior.x
  description[["implicit.n"]] <- prior.n
  # Create the prior string
  prior.string <- switch(likelihood,
    "poisson" = paste0("gamma(\u03B1 = ", round(description[["alpha"]], 3), ", \u03B2 = ", round(description[["beta"]], 3), ")"),
    "binomial" = paste0("beta(\u03B1 = ", round(description[["alpha"]], 3), ", \u03B2 = ", round(description[["beta"]], 3), ")"),
    "hypergeometric" = paste0("beta-binomial(N = ", N.units, ", \u03B1 = ", round(description[["alpha"]], 3), ", \u03B2 = ", round(description[["beta"]], 3), ")")
  )
  # Create the statistics section
  statistics <- list()
  statistics[["mode"]] <- switch(likelihood,
    "poisson" = (description[["alpha"]] - 1) / description[["beta"]],
    "binomial" = (description[["alpha"]] - 1) / (description[["alpha"]] + description[["beta"]] - 2),
    "hypergeometric" = .modebbinom(N = N.units, shape1 = description[["alpha"]], shape2 = description[["beta"]])
  )
  statistics[["mean"]] <- switch(likelihood,
    "poisson" = description[["alpha"]] / description[["beta"]],
    "binomial" = description[["alpha"]] / (description[["alpha"]] + description[["beta"]]),
    "hypergeometric" = description[["alpha"]] / (description[["alpha"]] + description[["beta"]]) * N.units
  )
  statistics[["median"]] <- switch(likelihood,
    "poisson" = stats::qgamma(0.5, shape = description[["alpha"]], rate = description[["beta"]]),
    "binomial" = stats::qbeta(0.5, shape1 = description[["alpha"]], shape2 = description[["beta"]]),
    "hypergeometric" = .qbbinom(0.5, N = N.units, shape1 = description[["alpha"]], shape2 = description[["beta"]])
  )
  statistics[["var"]] <- switch(likelihood,
    "poisson" = description[["alpha"]] / description[["beta"]]^2,
    "binomial" = (description[["alpha"]] * description[["beta"]]) / ((description[["alpha"]] + description[["beta"]])^2 * (description[["alpha"]] + description[["beta"]] + 1)),
    "hypergeometric" = ((N.units * description[["alpha"]] * description[["beta"]]) * (description[["alpha"]] + description[["beta"]] + N.units)) / ((description[["alpha"]] + description[["beta"]])^2 * (description[["alpha"]] + description[["beta"]] + 1))
  )
  statistics[["skewness"]] <- switch(likelihood,
    "poisson" = 2 / sqrt(description[["alpha"]]),
    "binomial" = ((2 * (description[["beta"]] - description[["alpha"]])) * sqrt(description[["alpha"]] + description[["beta"]] + 1)) / ((description[["alpha"]] + description[["beta"]] + 2) * sqrt(description[["alpha"]] * description[["beta"]])),
    "hypergeometric" = (((description[["alpha"]] + description[["beta"]] + 2 * N.units) * (description[["beta"]] - description[["alpha"]])) / (description[["alpha"]] + description[["beta"]] + 2)) * sqrt((1 + description[["alpha"]] + description[["beta"]]) / (N.units * description[["alpha"]] * description[["beta"]] * (N.units + description[["alpha"]] + description[["beta"]])))
  )
  statistics[["ub"]] <- switch(likelihood,
    "poisson" = stats::qgamma(conf.level, shape = description[["alpha"]], rate = description[["beta"]]),
    "binomial" = stats::qbeta(conf.level, shape1 = description[["alpha"]], shape2 = description[["beta"]]),
    "hypergeometric" = .qbbinom(conf.level, N = N.units, shape1 = description[["alpha"]], shape2 = description[["beta"]])
  )
  statistics[["precision"]] <- statistics[["ub"]] - statistics[["mode"]]
  # Create the specifics section
  if (method != "default" && method != "strict") {
    specifics <- list()
  }
  if (method == "impartial" || method == "hyp") {
    specifics[["p.h1"]] <- p.h1
    specifics[["p.h0"]] <- p.h0
  } else if (method == "sample" || method == "factor") {
    specifics[["x"]] <- x
    specifics[["n"]] <- n
    specifics[["factor"]] <- factor
  } else if (method == "arm") {
    specifics[["ir"]] <- ir
    specifics[["cr"]] <- cr
  } else if (method == "bram") {
    specifics[["mode"]] <- expected
    specifics[["ub"]] <- ub
  } else if (method == "param") {
    specifics[["alpha"]] <- alpha
    specifics[["beta"]] <- beta
  }
  # Create the hypotheses section
  if (!is.null(materiality)) {
    hypotheses <- list()
    hypotheses[["hypotheses"]] <- c(paste0("H\u2081: \u0398 < ", materiality), paste0("H\u2080: \u0398 > ", materiality))
    hypotheses[["p.h1"]] <- switch(likelihood,
      "poisson" = stats::pgamma(materiality, shape = description[["alpha"]], rate = description[["beta"]]),
      "binomial" = stats::pbeta(materiality, shape1 = description[["alpha"]], shape2 = description[["beta"]]),
      "hypergeometric" = extraDistr::pbbinom(ceiling(materiality * N.units) - 1, size = N.units, alpha = description[["alpha"]], beta = description[["beta"]])
    )
    hypotheses[["p.h0"]] <- switch(likelihood,
      "poisson" = stats::pgamma(materiality, shape = description[["alpha"]], rate = description[["beta"]], lower.tail = FALSE),
      "binomial" = stats::pbeta(materiality, shape1 = description[["alpha"]], shape2 = description[["beta"]], lower.tail = FALSE),
      "hypergeometric" = extraDistr::pbbinom(ceiling(materiality * N.units) - 1, size = N.units, alpha = description[["alpha"]], beta = description[["beta"]], lower.tail = FALSE)
    )
    hypotheses[["odds.h1"]] <- hypotheses[["p.h1"]] / hypotheses[["p.h0"]]
    hypotheses[["odds.h0"]] <- 1 / hypotheses[["odds.h1"]]
    hypotheses[["density"]] <- switch(likelihood,
      "poisson" = stats::dgamma(materiality, shape = description[["alpha"]], rate = description[["beta"]]),
      "binomial" = stats::dbeta(materiality, shape1 = description[["alpha"]], shape2 = description[["beta"]]),
      "hypergeometric" = extraDistr::dbbinom(ceiling(materiality * N.units), size = N.units, alpha = description[["alpha"]], beta = description[["beta"]])
    )
  }
  # Create the main result object
  result <- list()
  # Functional form of the prior distribution
  result[["prior"]] <- prior.string
  result[["description"]] <- description
  result[["statistics"]] <- statistics
  if (method != "default" && method != "strict") {
    result[["specifics"]] <- specifics
  }
  if (!is.null(materiality)) {
    result[["hypotheses"]] <- hypotheses
  }
  result[["method"]] <- method
  result[["likelihood"]] <- likelihood
  if (!is.null(materiality)) {
    result[["materiality"]] <- materiality
  }
  result[["expected"]] <- expected
  result[["conf.level"]] <- conf.level
  result[["N.units"]] <- N.units
  # Add class 'jfaPrior' to the result
  class(result) <- "jfaPrior"
  return(result)
}
