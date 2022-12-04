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
#' @description \code{auditPrior()} is used to create a prior distribution for
#' Bayesian audit sampling. The interface allows a complete customization of the
#' prior distribution as well as a formal translation of pre-existing audit
#' information into a prior distribution. The function returns an object of
#' class \code{jfaPrior} that can be used in the \code{planning()} and
#' \code{evaluation()} functions via their \code{prior} argument. Objects with
#' class \code{jfaPrior} can be further inspected via associated
#' \code{summary()} and \code{plot()} methods.
#'
#' @usage auditPrior(method = c(
#'              "default", "strict", "param", "impartial", "hyp",
#'              "arm", "bram", "sample", "factor"
#'            ),
#'            likelihood = c("poisson", "binomial", "hypergeometric"),
#'            N.units = NULL,
#'            alpha = NULL,
#'            beta = NULL,
#'            materiality = NULL,
#'            expected = 0,
#'            ir = NULL,
#'            cr = NULL,
#'            ub = NULL,
#'            p.hmin = NULL,
#'            x = NULL, n = NULL,
#'            factor = NULL,
#'            conf.level = 0.95)
#'
#' @param method      a character specifying the method by which the prior
#'   distribution is constructed. Possible options are \code{default},
#'   \code{strict}, \code{impartial}, \code{param}, \code{arm}, \code{bram},
#'   \code{hyp}, \code{sample}, and \code{factor}. See the details section for
#'   more information.
#' @param likelihood  a character specifying the likelihood for updating the
#'   prior distribution. Possible options are \code{poisson} (default) for a
#'   conjugate gamma prior distribution, \code{binomial} for a conjugate beta
#'   prior distribution, or \code{hypergeometric} for a conjugate beta-binomial
#'   prior distribution. See the details section for more information.
#' @param N.units     a numeric value larger than 0 specifying the total number
#'   of units in the population. Required for the \code{hypergeometric}
#'   likelihood.
#' @param alpha       a numeric value specifying the \eqn{\alpha} parameter of
#'   the prior distribution. Required for method \code{param}.
#' @param beta        a numeric value specifying the \eqn{\beta} parameter of
#'   the prior distribution. Required for method \code{param}.
#' @param materiality   a numeric value between 0 and 1 specifying the
#'   performance materiality (i.e., the maximum tolerable misstatement in the
#'   population) as a fraction. Required for methods \code{impartial},
#'   \code{arm}, and \code{hyp}.
#' @param expected      a numeric value between 0 and 1 specifying the expected
#'   (tolerable) misstatements in the sample relative to the total sample size.
#'   Required for methods \code{impartial}, \code{arm}, \code{bram}, and
#'   \code{hyp}.
#' @param ir          a numeric value between 0 and 1 specifying the inherent
#'   risk (i.e., the probability of material misstatement occurring due to
#'   inherent factors) in the audit risk model. Required for method \code{arm}.
#' @param cr          a numeric value between 0 and 1 specifying the internal
#'   control risk (i.e., the probability of material misstatement occurring due
#'   to internal control systems) in the audit risk model. Required for method
#'   \code{arm}.
#' @param ub          a numeric value between 0 and 1 specifying the
#'   \code{conf.level}-\% upper bound for the prior distribution as a
#'   fraction. Required for method \code{bram}.
#' @param p.hmin      a numeric value between 0 and 1 specifying the prior
#'   probability of the hypothesis of tolerable misstatement (H1: \eqn{\theta <}
#'   materiality). Required for method \code{hyp}.
#' @param x           a numeric value larger than, or equal to, 0 specifying the
#'   sum of proportional misstatements (taints) in a prior sample. Required for
#'   methods \code{sample} and \code{factor}.
#' @param n           a numeric value larger than 0 specifying the number of
#'   units in a prior sample. Required for methods \code{sample} and
#'   \code{factor}.
#' @param factor      a numeric value between 0 and 1 specifying the weight of
#'   a prior sample specified via \code{x} and \code{n}. Required for method
#'   \code{factor}.
#' @param conf.level  a numeric value between 0 and 1 specifying the confidence
#'   level (1 - audit risk).
#'
#' @details To perform Bayesian audit sampling you must assign a prior
#'   distribution to the parameter in the model, i.e., the population
#'   misstatement \eqn{\theta}. The prior distribution can incorporate
#'   pre-existing audit information about \eqn{\theta} into the analysis, which
#'   consequently allows for a more efficient or more accurate estimates. The
#'   default priors used by \code{jfa} are indifferent towards the possible
#'   values of \eqn{\theta}, while still being proper. Note that the default
#'   prior distributions are a conservative choice of prior since they, in most
#'   cases, assume all possible misstatement to be equally likely before seeing
#'   a data sample. It is recommended to construct an informed prior
#'   distribution based on pre-existing audit information when possible.
#'
#' @details This section elaborates on the available input options for the
#'   \code{method} argument.
#'
#' \itemize{
#'  \item{\code{default}:   This method produces a \emph{gamma(1, 1)},
#'    \emph{beta(1, 1)}, or \emph{beta-binomial(N, 1, 1)} prior distribution.
#'    These prior distributions are indifferent towards the possible values of
#'    the misstatement.}
#'  \item{\code{strict}:    This method produces an improper \emph{gamma(1, 0)},
#'    \emph{beta(1, 0)}, or \emph{beta-binomial(N, 1, 0)} prior distribution.
#'    These prior distributions match sample sizes and upper limits from
#'    classical methods and can be used to emulate classical results.}
#'  \item{\code{impartial}: This method produces an impartial prior
#'    distribution. These prior distributions assume that tolerable misstatement
#'    (\eqn{\theta <} materiality) and intolerable misstatement (\eqn{\theta >}
#'    materiality) are equally likely.}
#'  \item{\code{param}:     This method produces a custom
#'    \code{gamma(alpha, beta)}, \code{beta(alpha, beta)}, or
#'    \code{beta-binomial(N, alpha, beta)} prior distribution. The alpha and
#'    beta parameters must be set using \code{alpha} and \code{beta}.}
#'  \item{\code{hyp}:       This method translates an assessment of the prior
#'    probability for tolerable misstatement (\eqn{\theta <} materiality) to a
#'    prior distribution.}
#'  \item{\code{arm}:       This method translates an assessment of inherent
#'    risk and internal control risk to a prior distribution.}
#'  \item{\code{bram}:      This method translates an assessment of the
#'    expected most likely error and \emph{x}-\% upper bound to a prior
#'    distribution.}
#'  \item{\code{sample}:    This method translates the outcome of an earlier
#'    sample to a prior distribution.}
#'  \item{\code{factor}:    This method translates and weighs the outcome of an
#'    earlier sample to a prior distribution.}
#' }
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
#' @return An object of class \code{jfaPrior} containing:
#'
#' \item{prior}{a string describing the functional form of the prior
#'   distribution.}
#' \item{description}{a list containing a description of the prior distribution,
#'   including the parameters of the prior distribution and the implicit sample
#'   on which the prior distribution is based.}
#' \item{statistics}{a list containing statistics of the prior distribution,
#'   including the mean, mode, median, and upper bound of the prior
#'   distribution.}
#' \item{specifics}{a list containing specifics of the prior distribution that
#'   vary depending on the \code{method}.}
#' \item{hypotheses}{if \code{materiality} is specified, a list containing
#'   information about the hypotheses, including prior probabilities and odds
#'   for the hypothesis of tolerable misstatement (H1) and the hypothesis of
#'   intolerable misstatement (H0).}
#' \item{method}{a character indicating the method by which the prior
#'   distribution is constructed.}
#' \item{likelihood}{a character indicating the likelihood of the data.}
#' \item{materiality}{if \code{materiality} is specified, a numeric value
#'   between 0 and 1 giving the materiality used to construct the prior
#'   distribution.}
#' \item{expected}{a numeric value larger than, or equal to, 0 giving the input
#'   for the number of expected misstatements.}
#' \item{conf.level}{a numeric value between 0 and 1 giving the confidence
#'   level.}
#' \item{N.units}{if \code{N.units} is specified, the number of units in the
#'   population.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{planning}}
#'          \code{\link{selection}}
#'          \code{\link{evaluation}}
#'          \code{\link{report}}
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
#' @references Derks, K., de Swart, J., Wagenmakers, E.-J., & Wetzels, R.
#'   (2022). An impartial Bayesian hypothesis test for audit sampling.
#'   \emph{PsyArXiv}. \doi{10.31234/osf.io/8nf3e}
#'
#' @keywords audit evaluation planning prior
#'
#' @examples
#' # Default beta prior
#' auditPrior(likelihood = "binomial")
#'
#' # Impartial prior
#' auditPrior(method = "impartial", materiality = 0.05)
#'
#' # Translate inherent risk (ir) and control risk (cr) to a prior
#' auditPrior(method = "arm", expected = 0.025, materiality = 0.05, ir = 1, cr = 0.6)
#' @export

auditPrior <- function(method = c(
                         "default", "strict", "param", "impartial", "hyp",
                         "arm", "bram", "sample", "factor"
                       ),
                       likelihood = c("poisson", "binomial", "hypergeometric"),
                       N.units = NULL,
                       alpha = NULL,
                       beta = NULL,
                       materiality = NULL,
                       expected = 0,
                       ir = NULL,
                       cr = NULL,
                       ub = NULL,
                       p.hmin = NULL,
                       x = NULL,
                       n = NULL,
                       factor = NULL,
                       conf.level = 0.95) {
  # Input checking
  method <- match.arg(method)
  likelihood <- match.arg(likelihood)
  stopifnot("missing value for 'conf.level'" = !is.null(conf.level))
  valid_confidence <- is.numeric(conf.level) && length(conf.level) == 1 && conf.level > 0 && conf.level < 1
  stopifnot("'conf.level' must be a single value between 0 and 1" = valid_confidence)
  stopifnot("'expected' must be a single value >= 0" = expected >= 0)
  if (method %in% c("impartial", "hyp", "arm")) {
    stopifnot("missing value for 'materiality'" = !is.null(materiality))
    valid_materiality <- is.numeric(materiality) && materiality > 0 && materiality < 1
    stopifnot("'materiality' must be a single value between 0 and 1" = valid_materiality)
  }
  if (!is.null(materiality) && expected < 1) {
    stopifnot("'expected' must be a single value < 'materiality'" = expected < materiality)
  }
  requires_expected_percentage <- method %in% c("default", "sample", "factor", "param", "strict")
  if (expected >= 1 && !requires_expected_percentage) {
    stop(paste0("'expected' must be a single value between 0 and 1 for 'method = ", method, "'"))
  }
  if (likelihood == "hypergeometric") {
    stopifnot("missing value for 'N.units'" = !is.null(N.units))
    valid_units <- is.numeric(N.units) && length(N.units) == 1 && N.units > 0
    stopifnot("'N.units' must be a single value > 0" = valid_units)
    N.units <- ceiling(N.units)
  }
  # Compute prior per method
  if (method == "default") {
    prior.n <- 1
    prior.x <- 0
  } else if (method == "strict") {
    prior.n <- 0
    prior.x <- 0
  } else if (method == "arm") {
    stopifnot("missing value for 'ir' (inherent risk)" = !is.null(ir))
    valid_ir <- ir > 0 && ir <= 1
    stopifnot("'ir' (inherent risk) must be a single value between 0 and 1" = valid_ir)
    stopifnot("missing value for 'cr' (control risk)" = !is.null(cr))
    valid_cr <- cr > 0 && cr <= 1
    stopifnot("'cr' (control risk) must be a single value between 0 and 1" = valid_cr)
    stopifnot("ir * cr must be > 1 - conf.level" = ir * cr > 1 - conf.level)
    detection_risk <- (1 - conf.level) / (ir * cr)
    n.plus <- planning(materiality, min.precision = NULL, expected, likelihood, conf.level, N.units, prior = TRUE)$n
    n.min <- planning(materiality, min.precision = NULL, expected, likelihood, 1 - detection_risk, N.units, prior = TRUE)$n
    prior.n <- n.plus - n.min
    prior.x <- (n.plus * expected) - (n.min * expected)
  } else if (method == "bram") {
    stopifnot("missing value for 'ub'" = !is.null(ub))
    valid_ub <- length(ub) == 1 && is.numeric(ub) && ub > 0 && ub < 1 && ub > expected
    stopifnot("'ub' must be a single value between 0 and 1 and > 'expected'" = valid_ub)
    if (likelihood == "poisson" && expected > 0) { # Stewart (2013, p. 45)
      r <- expected / ub
      q <- stats::qnorm(conf.level)
      prior.x <- ((((q * r) + sqrt(3 + ((r / 3) * (4 * q^2 - 10)) - ((r^2 / 3) * (q^2 - 1)))) / (2 * (1 - r)))^2 + (1 / 4)) - 1
      prior.n <- prior.x / expected
    } else {
      bound <- Inf
      prior.x <- 0
      prior.n <- 1
      while (bound > ub) {
        if (expected == 0) {
          prior.n <- prior.n + 0.001
        } else {
          prior.x <- prior.x + 0.0001
          a <- 1 + prior.x
          if (likelihood == "poisson") {
            prior.n <- prior.x / expected
            b <- prior.n
          } else {
            prior.n <- 1 + prior.x / expected
            b <- prior.n - prior.x
          }
        }
        bound <- .comp_ub_bayes("less", conf.level, likelihood, a, b, N.units)
      }
    }
  } else if (method == "impartial" || method == "hyp") {
    if (method == "impartial") {
      p.h0 <- p.h1 <- 0.5
    } else if (method == "hyp") {
      stopifnot("missing value for 'p.hmin'" = !is.null(p.hmin))
      p.h1 <- p.hmin
      p.h0 <- 1 - p.h1
    }
    if (expected == 0) {
      prior.n <- switch(likelihood,
        "poisson" = -(log(p.h0) / materiality),
        "binomial" = log(p.h0) / log(1 - materiality),
        "hypergeometric" = log(2) / (log(N.units / (N.units - ceiling(materiality * N.units))))
      )
      prior.x <- 0
    } else {
      median <- Inf
      prior.x <- 0
      while (median > materiality) {
        prior.x <- prior.x + 0.0001
        if (likelihood == "poisson") {
          prior.n <- prior.x / expected
        } else {
          prior.n <- 1 + prior.x / expected
        }
        a <- 1 + prior.x
        if (likelihood == "poisson") {
          b <- prior.n
        } else {
          b <- prior.n - prior.x
        }
        median <- .comp_ub_bayes("less", p.h1, likelihood, a, b, N.units)
      }
    }
  } else if (method == "sample" || method == "factor") {
    stopifnot("missing value for 'n'" = !is.null(n))
    valid_n <- is.numeric(n) && length(n) == 1 && n >= 0
    stopifnot("'n' must be a single value >= 0" = valid_n)
    stopifnot("missing value for 'x'" = !is.null(x))
    valid_x <- is.numeric(x) && length(x) == 1 && x >= 0
    stopifnot("'x' must be a single value >= 0" = valid_x)
    if (method == "factor") {
      stopifnot("missing value for 'factor'" = !is.null(factor))
    } else {
      factor <- 1
    }
    prior.n <- n * factor
    prior.x <- x * factor
  } else if (method == "param") {
    stopifnot("missing value for 'alpha'" = !is.null(alpha))
    valid_alpha <- is.numeric(alpha) && length(alpha) == 1 && alpha > 0
    stopifnot("'alpha' must be a single value > 0" = valid_alpha)
    stopifnot("missing value for 'beta'" = !is.null(beta))
    valid_beta <- is.numeric(beta) && length(beta) == 1 && beta >= 0
    stopifnot("'beta' must be a single value >= 0" = valid_beta)
    prior.x <- alpha - 1
    if (likelihood == "poisson") {
      prior.n <- beta
    } else {
      prior.n <- beta + prior.x
    }
  }
  # Parameters
  prior_alpha <- 1 + prior.x
  if (likelihood == "poisson") {
    prior_beta <- prior.n
  } else {
    prior_beta <- prior.n - prior.x
  }
  # Initialize main results
  result <- list()
  result[["prior"]] <- .functional_form(likelihood, prior_alpha, prior_beta, N.units)
  # Description
  description <- list()
  description[["density"]] <- .functional_density(likelihood)
  description[["alpha"]] <- prior_alpha
  description[["beta"]] <- prior_beta
  description[["implicit.x"]] <- prior.x
  description[["implicit.n"]] <- prior.n
  result[["description"]] <- description
  # Statistics
  statistics <- list()
  statistics[["mode"]] <- .comp_mode_bayes(likelihood, prior_alpha, prior_beta, N.units)
  statistics[["mean"]] <- .comp_mean_bayes(likelihood, prior_alpha, prior_beta, N.units)
  statistics[["median"]] <- .comp_median_bayes(likelihood, prior_alpha, prior_beta, N.units)
  statistics[["var"]] <- .comp_var_bayes(likelihood, prior_alpha, prior_beta, N.units)
  statistics[["skewness"]] <- .comp_skew_bayes(likelihood, prior_alpha, prior_beta, N.units)
  statistics[["ub"]] <- .comp_ub_bayes("less", conf.level, likelihood, prior_alpha, prior_beta, N.units)
  statistics[["precision"]] <- .comp_precision("less", statistics[["mode"]], NULL, statistics[["ub"]])
  result[["statistics"]] <- statistics
  # Specifics
  if (method != "default" && method != "strict") {
    specifics <- list()
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
    result[["specifics"]] <- specifics
  }
  # Hypotheses
  if (!is.null(materiality)) {
    hypotheses <- list()
    hypotheses[["hypotheses"]] <- .hyp_string(materiality, "less")
    hypotheses[["p.h1"]] <- .hyp_prob(TRUE, materiality, likelihood, prior_alpha, prior_beta, N.units, N.units)
    hypotheses[["p.h0"]] <- .hyp_prob(FALSE, materiality, likelihood, prior_alpha, prior_beta, N.units, N.units)
    hypotheses[["odds.h1"]] <- hypotheses[["p.h1"]] / hypotheses[["p.h0"]]
    hypotheses[["odds.h0"]] <- 1 / hypotheses[["odds.h1"]]
    hypotheses[["density"]] <- .hyp_dens(materiality, likelihood, prior_alpha, prior_beta, N.units, N.units)
    result[["hypotheses"]] <- hypotheses
  }
  # Additional info
  result[["method"]] <- method
  result[["likelihood"]] <- likelihood
  if (!is.null(materiality)) {
    result[["materiality"]] <- materiality
  }
  result[["expected"]] <- expected
  result[["conf.level"]] <- conf.level
  result[["N.units"]] <- N.units
  class(result) <- c(class(result), "jfaPrior")
  return(result)
}
