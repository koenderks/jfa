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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Evaluate a statistical audit sample
#'
#' @description This function takes a data frame (using \code{data}, \code{values}, and \code{values.audit}) or summary statistics (using \code{x} and \code{n}) and performs inference on the misstatement in the sample. The function returns an object of class \code{jfaEvaluation} which can be used with associated \code{summary()} and \code{plot()} methods.
#'
#' For more details on how to use this function, see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage evaluation(materiality = NULL, min.precision = NULL, method = 'poisson',
#'            alternative = c('less', 'two.sided', 'greater'), conf.level = 0.95,
#'            data = NULL, values = NULL, values.audit = NULL, times = NULL,
#'            x = NULL, n = NULL, N.units = NULL, N.items = NULL,
#'            r.delta = 2.7, m.type = 'accounts', cs.a = 1, cs.b = 3, cs.mu = 0.5,
#'            prior = FALSE)
#'
#' @param materiality   a numeric value between 0 and 1 specifying the performance materiality (maximum tolerable error) as a fraction of the total size of the population. If specified, the function also returns the conclusion of the analysis with respect to the performance materiality. The value is discarded when \code{direct}, \code{difference}, \code{quotient}, or \code{regression} method is chosen.
#' @param min.precision a numeric value between 0 and 1 specifying the required minimum precision (upper bound minus most likely error) as a fraction of the total size of the population. If specified, the function also returns the conclusion of the analysis with respect to the required minimum precision.
#' @param method        a character specifying the method to be used in the evaluation. Possible options are \code{poisson}, \code{binomial} (default), \code{hypergeometric}, \code{mpu}, \code{stringer}, \code{stringer.meikle}, \code{stringer.lta}, \code{stringer.pvz}, \code{rohrbach}, \code{moment}, \code{direct}, \code{difference}, \code{quotient}, or \code{regression}. See the details section for more information.
#' @param alternative   a character indicating the alternative hypothesis to be tested (and the type of interval to be produced). This must be one of \code{less} (default), \code{two.sided}, or \code{greater}. You can specify just the initial letter.
#' @param conf.level    a numeric value between 0 and 1 specifying the confidence level used in the evaluation. Defaults to 0.95 for 95\% confidence.
#' @param data          a data frame containing the sample to be evaluated. The sample must at least contain a column of book values and a column of audit (true) values.
#' @param values        a character specifying name of a column in \code{data} containing the book values of the items.
#' @param values.audit  a character specifying name of a column in \code{data} containing the audit (true) values of the items.
#' @param times         a character specifying name of a column in \code{data} containing the number of times each item in the \code{data} should be counted in the evaluation (due to it being selected multiple times for the sample).
#' @param x             a numeric value larger than 0 specifying the sum of errors found in the sample. If specified, overrides the \code{data}, \code{values} and \code{values.audit} arguments and assumes that the data come from summary statistics specified by both \code{x} and \code{n}.
#' @param n             an integer larger than 0 specifying the number of items in the sample. If specified, overrides the \code{data}, \code{values} and \code{values.audit} arguments and assumes that the data come from summary statistics specified by both \code{x} and \code{n}.
#' @param N.units       an integer larger than 0 specifying the total number of sampling units in the population (i.e., the population size / value). Only required if \code{method} is one of \code{'hypergeometric'}, \code{direct}, \code{difference}, \code{quotient}, or \code{regression}.
#' @param N.items       an integer larger than 0 specifying the total number of items in the population. Only required if \code{method} is one of \code{direct}, \code{difference}, \code{quotient}, or \code{regression}.
#' @param r.delta       if \code{method = 'rohrbach'}, a numeric value specifying \eqn{\Delta} in Rohrbach's augmented variance bound (Rohrbach, 1993).
#' @param m.type        if \code{method = 'moment'}, a character specifying the type of population (Dworin and Grimlund, 1984). Possible options are \code{accounts} and \code{inventory}. This argument affects the calculation of the central moments in the bound.
#' @param cs.a          if \code{method = "coxsnell"}, a numeric value specifying the \eqn{\alpha} parameter of the prior distribution on the mean taint. Defaults to 1 as recommended by Cox and Snell (1979).
#' @param cs.b          if \code{method = "coxsnell"}, a numeric value specifying the \eqn{\beta} parameter of the prior distribution on the mean taint. Defaults to 3 as recommended by Cox and Snell (1979).
#' @param cs.mu         if \code{method = "coxsnell"}, a numeric value between 0 and 1 specifying the mean of the prior distribution on the mean taint. Defaults to 0.5 as recommended by Cox and Snell (1979).
#' @param prior         a logical specifying if a prior distribution must be used, or an object of class \code{jfaPrior} or \code{jfaPosterior} containing the prior distribution. Defaults to \code{FALSE} for frequentist planning. If \code{TRUE}, a minimal information prior is chosen by default. Chooses a conjugate gamma distribution for the Poisson likelihood, a conjugate beta distribution for the binomial likelihood, and a conjugate beta-binomial distribution for the hypergeometric likelihood.
#'
#' @details This section lists the available options for the \code{methods} argument.
#'
#' \itemize{
#'  \item{\code{poisson}:          Evaluates the sample with the Poisson distribution. If combined with \code{prior = TRUE}, performs Bayesian evaluation using a \emph{gamma} prior and posterior.}
#'  \item{\code{binomial}:         Evaluates the sample with the binomial distribution. If combined with \code{prior = TRUE}, performs Bayesian evaluation using a \emph{beta} prior and posterior.}
#'  \item{\code{hypergeometric}:   Evaluates the sample with the hypergeometric distribution. If combined with \code{prior = TRUE}, performs Bayesian evaluation using a \emph{beta-binomial} prior and posterior.}
#' 	\item{\code{mpu}:              Evaluates the sample with the mean-per-unit estimator.}
#'  \item{\code{stringer}:         Evaluates the sample with the Stringer bound (Stringer, 1963).}
#'  \item{\code{stringer.meikle}:  Evaluates the sample with the Stringer bound with Meikle's correction for understatements (Meikle, 1972).}
#'  \item{\code{stringer.lta}:     Evaluates the sample with the Stringer bound with LTA correction for understatements (Leslie, Teitlebaum, and Anderson, 1979).}
#'  \item{\code{stringer.pvz}:     Evaluates the sample with the Stringer bound with Pap and van Zuijlen's correction for understatements (Pap and van Zuijlen, 1996).}
#'  \item{\code{rohrbach}:         Evaluates the sample with Rohrbach's augmented variance bound (Rohrbach, 1993).}
#'  \item{\code{moment}:           Evaluates the sample with the modified moment bound (Dworin and Grimlund, 1984).}
#'  \item{\code{coxsnell}:         Evaluates the sample with the Cox and Snell bound (Cox and Snell, 1979).}
#'  \item{\code{direct}:           Evaluates the sample with the direct estimator (Touw and Hoogduin, 2011).}
#'  \item{\code{difference}:       Evaluates the sample with the difference estimator (Touw and Hoogduin, 2011).}
#'  \item{\code{quotient}:         Evaluates the sample with the quotient estimator (Touw and Hoogduin, 2011).}
#'  \item{\code{regression}:       Evaluates the sample with the regression estimator (Touw and Hoogduin, 2011).}
#' }
#'
#' @references Cox, D. and Snell, E. (1979). On sampling and the estimation of rare errors. \emph{Biometrika}, 66(1), 125-132.
#' @references Derks, K., de Swart, J., van Batenburg, P., Wagenmakers, E.-J., & Wetzels, R. (2021). Priors in a Bayesian audit: How integration of existing information into the prior distribution can improve audit transparency and efficiency. \emph{International Journal of Auditing}, 25(3), 621-636.
#' @references Dworin, L. D. and Grimlund, R. A. (1984). Dollar-unit sampling for accounts receivable and inventory. \emph{The Accounting Review}, 59(2), 218–241
#' @references Leslie, D. A., Teitlebaum, A. D., & Anderson, R. J. (1979). \emph{Dollar-unit Sampling: A Practical Guide for Auditors}. Copp Clark Pitman; Belmont, Calif.: distributed by Fearon-Pitman.
#' @references Meikle, G. R. (1972). \emph{Statistical Sampling in an Audit Context: An Audit Technique}. Canadian Institute of Chartered Accountants.
#' @references Pap, G., and van Zuijlen, M. C. (1996). On the asymptotic behavior of the Stringer bound. \emph{Statistica Neerlandica}, 50(3), 367-389.
#' @references Rohrbach, K. J. (1993). Variance augmentation to achieve nominal coverage probability in sampling from audit populations. \emph{Auditing}, 12(2), 79.
#' @references Stringer, K. W. (1963). Practical aspects of statistical sampling in auditing. \emph{In Proceedings of the Business and Economic Statistics Section} (pp. 405-411). American Statistical Association.
#' @references Touw, P., and Hoogduin, L. (2011). \emph{Statistiek voor Audit en Controlling}. Boom uitgevers Amsterdam.
#'
#' @return An object of class \code{jfaEvaluation} containing:
#'
#' \item{conf.level}{a numeric value between 0 and 1 indicating the confidence level used.}
#' \item{mle}{a numeric value between 0 and 1 indicating the most likely error in the population as a fraction of its total size.}
#' \item{ub}{a numeric value indicating the upper bound on the (probability of) misstatement.}
#' \item{lb}{a numeric value indicating the lower bound of the interval around the (probability of) misstatement.}
#' \item{precision}{a numeric value between 0 and 1 indicating the difference between the most likely error and the upper bound in the population as a fraction of the total population size.}
#' \item{p.value}{a numeric value indicating the one-sided p-value.}
#' \item{x}{an integer larger than, or equal to, 0 indicating the number of items in the sample that contained an error.}
#' \item{t}{a value larger than, or equal to, 0, indicating the sum of observed taints.}
#' \item{n}{an integer larger than 0 indicating the sample size.}
#' \item{materiality}{if \code{materiality} is specified, a numeric value between 0 and 1 indicating the performance materiality as a fraction of the total population size.}
#' \item{min.precision}{if \code{min.precision} is specified, a numeric value between 0 and 1 indicating the minimum required precision as a fraction of the total population size.}
#' \item{alternative}{a character indicating the alternative hypothesis.}
#' \item{method}{a character indicating the evaluation method.}
#' \item{N.units}{if \code{N.units} is specified, in integer larger than 0 indicating the total number of units in the population.}
#' \item{N.items}{if \code{N.items} is specified, in integer larger than 0 indicating the total number of items in the population.}
#' \item{K}{if \code{method = 'hypergeometric'}, an integer indicating the assumed total errors in the population.}
#' \item{prior}{an object of class 'jfaPrior' that contains the prior distribution.}
#' \item{posterior}{an object of class 'jfaPosterior' that contains the posterior distribution.}
#' \item{data}{a data frame containing the relevant columns from the \code{data}.}
#' \item{data.name}{a character string giving the name of the data.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}} \code{\link{planning}} \code{\link{selection}} \code{\link{report}}
#'
#' @keywords evaluation confidence bound audit
#'
#' @examples
#' data("BuildIt")
#'
#' # Draw a sample of 100 monetary units from the population using
#' # fixed interval monetary unit sampling
#' sample <- selection(
#'   data = BuildIt, size = 100, units = "values",
#'   method = "interval", values = "bookValue"
#' )$sample
#'
#' # Classical evaluation using the Stringer bound
#' evaluation(
#'   materiality = 0.05, method = "stringer", conf.level = 0.95,
#'   data = sample, values = "bookValue", values.audit = "auditValue"
#' )
#'
#' # Classical evaluation using the Poisson likelihood
#' evaluation(
#'   materiality = 0.05, method = "poisson", conf.level = 0.95,
#'   data = sample, values = "bookValue", values.audit = "auditValue"
#' )
#'
#' # Bayesian evaluation using a noninformative gamma prior distribution
#' evaluation(
#'   materiality = 0.05, method = "poisson", conf.level = 0.95,
#'   data = sample, values = "bookValue", values.audit = "auditValue",
#'   prior = TRUE
#' )
#'
#' # Bayesian evaluation using an informed prior distribution
#' evaluation(
#'   materiality = 0.05, method = "poisson", conf.level = 0.95,
#'   data = sample, values = "bookValue", values.audit = "auditValue",
#'   prior = auditPrior(method = "param", alpha = 1, beta = 10)
#' )
#' @export

evaluation <- function(materiality = NULL, min.precision = NULL, method = "poisson",
                       alternative = c("less", "two.sided", "greater"), conf.level = 0.95,
                       data = NULL, values = NULL, values.audit = NULL, times = NULL,
                       x = NULL, n = NULL, N.units = NULL, N.items = NULL,
                       r.delta = 2.7, m.type = "accounts", cs.a = 1, cs.b = 3, cs.mu = 0.5,
                       prior = FALSE) {
  alternative <- match.arg(alternative)
  bayesian <- (class(prior) == "logical" && prior == TRUE) || class(prior) %in% c("jfaPrior", "jfaPosterior")
  # Import existing prior distribution with class 'jfaPrior' or 'jfaPosterior'.
  if (class(prior) %in% c("jfaPrior", "jfaPosterior")) {
    if (method != prior[["likelihood"]]) {
      warning(paste0("using 'method = ", prior[["likelihood"]], "' from 'prior'"))
    }
    prior.x <- prior[["description"]]$implicit.x
    prior.n <- prior[["description"]]$implicit.n
    method <- prior[["likelihood"]]
    if (!is.null(prior[["N.units"]])) {
      warning(paste0("using 'N.units = ", prior[["N.units"]], "' from 'prior'"))
      N.units <- prior[["N.units"]]
    }
  } else {
    prior.n <- 1
    prior.x <- 0
  }
  if (conf.level >= 1 || conf.level <= 0 || is.null(conf.level) || length(conf.level) != 1) {
    stop("'conf.level' must be a single value between 0 and 1")
  }
  if (is.null(materiality) && is.null(min.precision)) {
    stop("missing value for 'materiality' or `min.precision`")
  }
  if (!is.null(materiality) && (materiality <= 0 || materiality > 1)) {
    stop("'materiality' must be a single value between 0 and 1")
  }
  if (!is.null(min.precision) && (min.precision <= 0 || min.precision >= 1)) {
    stop("'min.precision' must be a single value between 0 and 1")
  }
  if (!(method %in% c("poisson", "binomial", "hypergeometric", "stringer", "stringer.meikle", "stringer.lta", "stringer.pvz", "rohrbach", "moment", "coxsnell", "direct", "difference", "quotient", "regression", "mpu")) || length(method) != 1) {
    stop("'method' should be one of 'poisson', 'binomial', 'hypergeometric', 'stringer', 'stringer.meikle', 'stringer.lta', 'stringer.pvz', 'rohrbach', 'moment', 'coxsnell', 'direct', 'difference', 'quotient', 'regression', 'mpu'")
  }
  if (bayesian && method %in% c("stringer", "stringer.meikle", "stringer.lta", "stringer.pvz", "rohrbach", "moment", "direct", "difference", "quotient", "regression", "mpu")) {
    stop("'method' should be one of 'poisson', 'binomial', 'hypergeometric'")
  }
  if (alternative %in% c("two.sided", "greater") && method %in% c("stringer", "stringer.meikle", "stringer.lta", "stringer.pvz")) {
    stop(paste0("'method = ", method, "' does not accomodate 'alternative = ", alternative, "'"))
  }
  if (!is.null(x) || !is.null(n)) { # Use summary statistics
    if (method %in% c("stringer", "stringer.meikle", "stringer.lta", "stringer.pvz", "coxsnell", "rohrbach", "moment", "direct", "difference", "quotient", "regression", "mpu")) {
      stop(paste0("missing value for 'data' with 'method = ", method, "'"))
    }
    if (is.null(n)) {
      stop("missing value for 'n'")
    }
    if (n <= 0 || n %% 1 != 0 || length(n) != 1) {
      stop("'n' must be a single integer > 0")
    }
    if (is.null(x)) {
      stop("missing value for 'x'")
    }
    if (x < 0 || length(x) != 1) {
      stop("'x' must be a single value >= 0")
    }
    if (x > n) {
      stop("'x' must be a positive value <= 'n'")
    }
    if (x %% 1 != 0 && method == "hypergeometric" && !bayesian) {
      x <- ceiling(x)
      warning(paste0("using 'x = ", x, "' since 'x' must be a single integer >= 0"))
    }
    if (!is.null(data)) {
      warning("'x' and 'n' are used while 'data' is specified")
    }
    n.obs <- n
    x.obs <- x
    t.obs <- x
  } else if (!is.null(data)) { # Use data sample
    dname <- deparse(substitute(data))
    if (is.null(values)) {
      stop("missing value for 'values'")
    }
    if (length(values) != 1) {
      stop("'values' must be a single character")
    }
    if (!(values %in% colnames(data))) {
      stop(paste0("'", values, "' is not a column in 'data'"))
    }
    if (is.null(values.audit)) {
      stop("missing value for 'values.audit'")
    }
    if (length(values.audit) != 1) {
      stop("'values.audit' must be a single character")
    }
    if (!(values.audit %in% colnames(data))) {
      stop(paste0("'", values.audit, "' is not a column in 'data'"))
    }
    if (!is.null(x) || !is.null(n)) {
      warning("'data' is used while 'x' or 'n' is specified")
    }
    missing <- unique(c(which(is.na(data[, values])), which(is.na(data[, values.audit]))))
    if (length(missing) == nrow(data)) {
      stop("not enough rows in 'data'")
    }
    data <- stats::na.omit(data)
    if (!is.null(times)) {
      if (!(times %in% colnames(data))) {
        stop(paste0("'", times, "' is not a column in 'data'"))
      }
      times <- data[, times]
      if (!is.null(times) && any(times < 1)) {
        stop("column 'times' in 'data' must be a vector of integers >= 1")
      }
      if (!is.null(times) && any(times %% 1 != 0)) {
        stop("column 'times' in 'data' must be a vector of nonnegative integers")
      }
      n.obs <- sum(times)
    } else {
      n.obs <- nrow(data)
    }
    bookvalues <- data[, values]
    auditvalues <- data[, values.audit]
    t <- (bookvalues - auditvalues) / bookvalues
    x.obs <- length(which(t != 0))
    if (!is.null(times)) {
      t <- t * times
    }
    t.obs <- sum(t)
  } else {
    stop("missing value(s) for 'data' or a combination of 'x' and 'n'")
  }
  # Set the materiality and the minimium precision to 1 if they are NULL
  if (is.null(materiality)) {
    materiality <- 1
  }
  if (is.null(min.precision)) {
    min.precision <- 1
  }
  # Define placeholders for the most likely error and the precision
  mle <- NULL
  precision <- NULL
  # Calculate the results depending on the specified 'method'
  if (method == "poisson") {
    if (bayesian) {
      # Bayesian evaluation using the gamma distribution
      mle <- ((1 + prior.x + t.obs) - 1) / (prior.n + n.obs)
      ub <- switch(alternative,
        "two.sided" = stats::qgamma(p = conf.level + (1 - conf.level) / 2, shape = 1 + prior.x + t.obs, rate = prior.n + n.obs),
        "less" = stats::qgamma(p = conf.level, shape = 1 + prior.x + t.obs, rate = prior.n + n.obs),
        "greater" = 1
      )
      lb <- switch(alternative,
        "two.sided" = stats::qgamma(p = (1 - conf.level) / 2, shape = 1 + prior.x + t.obs, rate = prior.n + n.obs),
        "less" = 0,
        "greater" = stats::qgamma(p = 1 - conf.level, shape = 1 + prior.x + t.obs, rate = prior.n + n.obs)
      )
    } else {
      # Classical evaluation using the Poisson distribution
      mle <- t.obs / n.obs
      ub <- switch(alternative,
        "two.sided" = stats::qgamma(p = conf.level + (1 - conf.level) / 2, shape = 1 + t.obs, rate = n.obs),
        "less" = stats::qgamma(p = conf.level, shape = 1 + t.obs, rate = n.obs),
        "greater" = Inf
      )
      lb <- switch(alternative,
        "two.sided" = stats::qgamma(p = (1 - conf.level) / 2, shape = t.obs, rate = n.obs),
        "less" = 0,
        "greater" = stats::qgamma(p = 1 - conf.level, shape = t.obs, rate = n.obs)
      )
      if (materiality < 1) {
        p.val <- switch(alternative,
          "two.sided" = stats::poisson.test(x = ceiling(t.obs), T = n.obs, r = materiality, alternative = "two.sided")$p.value,
          "less" = stats::pgamma(q = materiality, shape = 1 + t.obs, rate = n.obs, lower.tail = FALSE),
          "greater" = stats::pgamma(q = materiality, shape = t.obs, rate = n.obs)
        )
      }
    }
  } else if (method == "binomial") {
    if (bayesian) {
      # Bayesian evaluation using the beta distribution
      mle <- (1 + prior.x + t.obs - 1) / ((1 + prior.x + t.obs) + (prior.n - prior.x + n.obs - t.obs) - 2)
      ub <- switch(alternative,
        "two.sided" = stats::qbeta(p = conf.level + (1 - conf.level) / 2, shape1 = 1 + prior.x + t.obs, shape2 = prior.n - prior.x + n.obs - t.obs),
        "less" = stats::qbeta(p = conf.level, shape1 = 1 + prior.x + t.obs, shape2 = prior.n - prior.x + n.obs - t.obs),
        "greater" = 1
      )
      lb <- switch(alternative,
        "two.sided" = stats::qbeta(p = (1 - conf.level) / 2, shape1 = 1 + prior.x + t.obs, shape2 = prior.n - prior.x + n.obs - t.obs),
        "less" = 0,
        "greater" = stats::qbeta(p = 1 - conf.level, shape1 = 1 + prior.x + t.obs, shape2 = prior.n - prior.x + n.obs - t.obs)
      )
    } else {
      # Classical evaluation using the binomial distribution
      mle <- t.obs / n.obs
      ub <- switch(alternative,
        "two.sided" = stats::qbeta(p = conf.level + (1 - conf.level) / 2, shape1 = 1 + t.obs, shape2 = n.obs - t.obs),
        "less" = stats::qbeta(p = conf.level, shape1 = 1 + t.obs, shape2 = n.obs - t.obs),
        "greater" = 1
      )
      lb <- switch(alternative,
        "two.sided" = stats::qbeta(p = (1 - conf.level) / 2, shape1 = t.obs, shape2 = 1 + n.obs - t.obs),
        "less" = 0,
        "greater" = stats::qbeta(p = 1 - conf.level, shape1 = t.obs, shape2 = 1 + n.obs - t.obs)
      )
      if (materiality < 1) {
        p.val <- switch(alternative,
          "two.sided" = stats::binom.test(x = ceiling(t.obs), n = n.obs, p = materiality, alternative = "two.sided")$p.value,
          "less" = stats::pbeta(q = materiality, shape1 = 1 + t.obs, shape2 = n.obs - t.obs, lower.tail = FALSE),
          "greater" = stats::pbeta(q = materiality, shape1 = t.obs, shape2 = 1 + n.obs - t.obs)
        )
      }
    }
  } else if (method == "hypergeometric") {
    if (is.null(N.units)) {
      stop("missing value for 'N.units'")
    }
    if (N.units <= 0 || length(N.units) != 1) {
      stop("'N.units' must be a single integer > 0")
    }
    N.units <- ceiling(N.units)
    if (bayesian) {
      # Bayesian evaluation using the beta-binomial distribution
      mle <- .modebbinom(N = N.units - n.obs, shape1 = 1 + prior.x + t.obs, shape2 = prior.n - prior.x + n.obs - t.obs) / N.units
      ub <- switch(alternative,
        "two.sided" = .qbbinom(p = conf.level + (1 - conf.level) / 2, N = N.units - n.obs, shape1 = 1 + prior.x + t.obs, shape2 = prior.n - prior.x + n.obs - t.obs) / N.units,
        "less" = .qbbinom(p = conf.level, N = N.units - n.obs, shape1 = 1 + prior.x + t.obs, shape2 = prior.n - prior.x + n.obs - t.obs) / N.units,
        "greater" = 1
      )
      lb <- switch(alternative,
        "two.sided" = .qbbinom(p = (1 - conf.level) / 2, N = N.units - n.obs, shape1 = 1 + prior.x + t.obs, shape2 = prior.n - prior.x + n.obs - t.obs) / N.units,
        "less" = 0,
        "greater" = .qbbinom(p = 1 - conf.level, N = N.units - n.obs, shape1 = 1 + prior.x + t.obs, shape2 = prior.n - prior.x + n.obs - t.obs) / N.units
      )
    } else {
      # Classical evaluation using the hypergeometric distribution
      K <- ceiling(materiality * N.units)
      mle <- x.obs / n.obs
      ub <- switch(alternative,
        "two.sided" = .qhyper(p = conf.level + (1 - conf.level) / 2, N = N.units, n = n.obs, k = x.obs) / N.units,
        "less" = .qhyper(p = conf.level, N = N.units, n = n.obs, k = x.obs) / N.units,
        "greater" = 1
      )
      lb <- switch(alternative,
        "two.sided" = .qhyper(p = (1 - conf.level) / 2, N = N.units, n = n.obs, k = x.obs) / N.units,
        "less" = 0,
        "greater" = .qhyper(p = 1 - conf.level, N = N.units, n = n.obs, k = x.obs) / N.units
      )
      if (materiality < 1) {
        p.val <- switch(alternative,
          "two.sided" = stats::fisher.test(matrix(c(x.obs, n.obs - x.obs, K - x.obs, N.units - n.obs - K + x.obs), nrow = 2), alternative = "two.sided")$p.value,
          "less" = stats::phyper(q = x.obs, m = K, n = N.units - K, k = n.obs),
          "greater" = stats::phyper(q = x.obs - 1, m = K, n = N.units - K, k = n.obs, lower.tail = FALSE)
        )
      }
    }
  } else {
    out <- switch(method,
      "stringer" = .stringer(t, conf.level, n.obs), # Classical evaluation using the Stringer bound
      "stringer.meikle" = .stringer(t, conf.level, n.obs, correction = "meikle"), # Classical evaluation using the Stringer bound with Meikle's adjustment
      "stringer.lta" = .stringer(t, conf.level, n.obs, correction = "lta"), # Classical evaluation using the Stringer bound with the LTA adjustment
      "stringer.pvz" = .stringer(t, conf.level, n.obs, correction = "pvz"), # Classical evaluation using the Stringer bound with PvZ adjustment
      "rohrbach" = .rohrbach(t, conf.level, n.obs, alternative, N.units, r.delta), # Classical evaluation using Rohrbachs augmented variance bound
      "moment" = .moment(t, conf.level, n.obs, alternative, m.type), # Classical evaluation using the Modified Moment bound
      "coxsnell" = .coxsnell(t, conf.level, n.obs, alternative, cs.a, cs.b, cs.mu, 1 + prior.x, 1 + prior.n - prior.x), # Bayesian evaluation using the Cox and Snell bound
      "mpu" = .mpu(t, conf.level, alternative, n.obs), # Classical evaluation using the Mean-per-unit estimator
      "direct" = .direct(bookvalues, auditvalues, conf.level, alternative, N.items, n.obs, N.units), # Classical evaluation using the Direct estimator
      "difference" = .difference(bookvalues, auditvalues, conf.level, alternative, N.items, n.obs), # Classical evaluation using the Difference estimator
      "quotient" = .quotient(bookvalues, auditvalues, conf.level, alternative, N.items, n.obs), # Classical evaluation using the Quotient estimator
      "regression" = .regression(bookvalues, auditvalues, conf.level, alternative, N.items, n.obs, N.units), # Classical evaluation using the Regression estimator
      "newmethod" = NULL
    ) # Add new method here
    mle <- out[["mle"]]
    ub <- out[["ub"]]
    lb <- out[["lb"]]
  }
  # Create the main results object
  result <- list()
  result[["conf.level"]] <- conf.level
  result[["mle"]] <- mle
  result[["ub"]] <- ub
  result[["lb"]] <- lb
  result[["precision"]] <- if (alternative == "greater") result[["mle"]] - result[["lb"]] else result[["ub"]] - result[["mle"]]
  if (!bayesian && materiality < 1 && method %in% c("binomial", "poisson", "hypergeometric")) {
    result[["p.value"]] <- p.val
  }
  result[["x"]] <- x.obs
  result[["t"]] <- t.obs
  result[["n"]] <- n.obs
  result[["materiality"]] <- materiality
  result[["min.precision"]] <- min.precision
  result[["alternative"]] <- alternative
  result[["method"]] <- method
  if (!is.null(N.units)) {
    result[["N.units"]] <- N.units
  }
  if (!is.null(N.items)) {
    result[["N.items"]] <- N.items
  }
  if (method == "hypergeometric" && is.logical(prior) && prior == FALSE) {
    result[["K"]] <- K
  }
  # Create the prior distribution object
  if (((class(prior) == "logical" && prior == TRUE) || class(prior) %in% c("jfaPrior", "jfaPosterior"))) {
    if (class(prior) == "jfaPrior" && !is.null(prior[["hypotheses"]])) {
      result[["prior"]] <- prior
      if (alternative == "greater") { # The prior uses alternative = 'less' for p.h1 and p.h0
        result[["prior"]][["hypotheses"]]$odds.h1 <- 1 / result[["prior"]][["hypotheses"]]$odds.h1
      }
    } else {
      result[["prior"]] <- auditPrior(
        method = "sample", likelihood = method, N.units = result[["N.units"]],
        materiality = result[["materiality"]], x = prior.x, n = prior.n
      )
    }
  }
  if (!is.null(result[["prior"]])) {
    # Create the posterior distribution object
    result[["posterior"]] <- list()
    # Functional form of the posterior distribution
    result[["posterior"]]$posterior <- switch(method,
      "poisson" = paste0("gamma(\u03B1 = ", round(result[["prior"]][["description"]]$alpha + result[["t"]], 3), ", \u03B2 = ", round(result[["prior"]][["description"]]$beta + result[["n"]], 3), ")"),
      "binomial" = paste0("beta(\u03B1 = ", round(result[["prior"]][["description"]]$alpha + result[["t"]], 3), ", \u03B2 = ", round(result[["prior"]][["description"]]$beta + result[["n"]] - result[["t"]], 3), ")"),
      "hypergeometric" = paste0("beta-binomial(N = ", result[["N.units"]] - result[["n"]], ", \u03B1 = ", round(result[["prior"]][["description"]]$alpha + result[["t"]], 3), ", \u03B2 = ", round(result[["prior"]][["description"]]$beta + result[["n"]] - result[["t"]], 3), ")")
    )
    result[["posterior"]]$likelihood <- method
    # Create the description section
    result[["posterior"]][["description"]] <- list()
    result[["posterior"]][["description"]]$density <- switch(method,
      "poisson" = "gamma",
      "binomial" = "beta",
      "hypergeometric" = "beta-binomial"
    )
    result[["posterior"]][["description"]]$n <- result[["n"]]
    result[["posterior"]][["description"]]$x <- result[["t"]]
    result[["posterior"]][["description"]]$alpha <- switch(method,
      "poisson" = result[["prior"]][["description"]]$alpha + result[["t"]],
      "binomial" = result[["prior"]][["description"]]$alpha + result[["t"]],
      "hypergeometric" = result[["prior"]][["description"]]$alpha + result[["t"]]
    )
    result[["posterior"]][["description"]]$beta <- switch(method,
      "poisson" = result[["prior"]][["description"]]$beta + result[["n"]],
      "binomial" = result[["prior"]][["description"]]$beta + result[["n"]] - result[["t"]],
      "hypergeometric" = result[["prior"]][["description"]]$beta + result[["n"]] - result[["t"]]
    )
    result[["posterior"]][["description"]]$implicit.x <- result[["posterior"]][["description"]]$alpha - 1
    result[["posterior"]][["description"]]$implicit.n <- switch(method,
      "poisson" = result[["posterior"]][["description"]]$beta,
      "binomial" = result[["posterior"]][["description"]]$beta - result[["t"]],
      "hypergeometric" = result[["posterior"]][["description"]]$beta - result[["t"]]
    )
    # Create the statistics section
    result[["posterior"]][["statistics"]] <- list()
    result[["posterior"]][["statistics"]]$mode <- switch(method,
      "poisson" = (result[["posterior"]][["description"]]$alpha - 1) / result[["posterior"]][["description"]]$beta,
      "binomial" = (result[["posterior"]][["description"]]$alpha - 1) / (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta - 2),
      "hypergeometric" = .modebbinom(N = result[["N.units"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta)
    )
    result[["posterior"]][["statistics"]]$mean <- switch(method,
      "poisson" = result[["posterior"]][["description"]]$alpha / result[["posterior"]][["description"]]$beta,
      "binomial" = result[["posterior"]][["description"]]$alpha / (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta),
      "hypergeometric" = result[["posterior"]][["description"]]$alpha / (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta) * (result[["N.units"]] - result[["n"]])
    )
    result[["posterior"]][["statistics"]]$median <- switch(method,
      "poisson" = stats::qgamma(0.5, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
      "binomial" = stats::qbeta(0.5, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
      "hypergeometric" = .qbbinom(0.5, N = result[["N.units"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta)
    )
    result[["posterior"]][["statistics"]]$var <- switch(method,
      "poisson" = result[["posterior"]][["description"]]$alpha / result[["posterior"]][["description"]]$beta^2,
      "binomial" = (result[["posterior"]][["description"]]$alpha * result[["posterior"]][["description"]]$beta) / ((result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta)^2 * (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta + 1)),
      "hypergeometric" = (((result[["N.units"]] - result[["n"]]) * result[["posterior"]][["description"]]$alpha * result[["posterior"]][["description"]]$beta) * (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta + (result[["N.units"]] - result[["n"]]))) / ((result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta)^2 * (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta + 1))
    )
    result[["posterior"]][["statistics"]]$skewness <- switch(method,
      "poisson" = 2 / sqrt(result[["posterior"]][["description"]]$alpha),
      "binomial" = ((2 * (result[["posterior"]][["description"]]$beta - result[["posterior"]][["description"]]$alpha)) * sqrt(result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta + 1)) / ((result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta + 2) * sqrt(result[["posterior"]][["description"]]$alpha * result[["posterior"]][["description"]]$beta)),
      "hypergeometric" = (((result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta + 2 * (result[["N.units"]] - result[["n"]])) * (result[["posterior"]][["description"]]$beta - result[["posterior"]][["description"]]$alpha)) / (result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta + 2)) * sqrt((1 + result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta) / ((result[["N.units"]] - result[["n"]]) * result[["posterior"]][["description"]]$alpha * result[["posterior"]][["description"]]$beta * ((result[["N.units"]] - result[["n"]]) + result[["posterior"]][["description"]]$alpha + result[["posterior"]][["description"]]$beta)))
    )
    if (alternative == "less") {
      result[["posterior"]][["statistics"]]$ub <- switch(method,
        "poisson" = stats::qgamma(conf.level, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
        "binomial" = stats::qbeta(conf.level, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
        "hypergeometric" = .qbbinom(conf.level, N = result[["N.units"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta)
      )
      result[["posterior"]][["statistics"]]$lb <- 0
    } else if (alternative == "two.sided") {
      result[["posterior"]][["statistics"]]$ub <- switch(method,
        "poisson" = stats::qgamma(conf.level + (1 - conf.level) / 2, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
        "binomial" = stats::qbeta(conf.level + (1 - conf.level) / 2, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
        "hypergeometric" = .qbbinom(conf.level + (1 - conf.level) / 2, N = result[["N.units"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta)
      )
      result[["posterior"]][["statistics"]]$lb <- switch(method,
        "poisson" = stats::qgamma((1 - conf.level) / 2, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
        "binomial" = stats::qbeta((1 - conf.level) / 2, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
        "hypergeometric" = .qbbinom((1 - conf.level) / 2, N = result[["N.units"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta)
      )
    } else if (alternative == "greater") {
      result[["posterior"]][["statistics"]]$ub <- if (method == "poisson") Inf else 1
      result[["posterior"]][["statistics"]]$lb <- switch(method,
        "poisson" = stats::qgamma(1 - conf.level, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
        "binomial" = stats::qbeta(1 - conf.level, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
        "hypergeometric" = .qbbinom(1 - conf.level, N = result[["N.units"]] - result[["n"]], shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta)
      )
    }
    result[["posterior"]][["statistics"]]$precision <- if (alternative == "greater") result[["posterior"]][["statistics"]]$mode - result[["posterior"]][["statistics"]]$lb else result[["posterior"]][["statistics"]]$ub - result[["posterior"]][["statistics"]]$mode
    # Create the hypotheses section
    if (result[["materiality"]] != 1) {
      result[["posterior"]][["hypotheses"]] <- list()
      if (alternative == "two.sided") {
        result[["posterior"]][["hypotheses"]]$hypotheses <- c(paste0("H\u2080: \u0398 = ", materiality), paste0("H\u2081: \u0398 \u2260 ", materiality))
        result[["posterior"]][["hypotheses"]]$density <- switch(method,
          "poisson" = stats::dgamma(materiality, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
          "binomial" = stats::dbeta(materiality, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
          "hypergeometric" = extraDistr::dbbinom(ceiling(materiality * N.units), size = N.units, alpha = result[["posterior"]][["description"]]$alpha, beta = result[["posterior"]][["description"]]$beta)
        )
        result[["posterior"]][["hypotheses"]]$bf.h0 <- result[["posterior"]][["hypotheses"]]$density / result[["prior"]][["hypotheses"]]$density
        result[["posterior"]][["hypotheses"]]$bf.h1 <- 1 / result[["posterior"]][["hypotheses"]]$bf.h0
      } else {
        if (alternative == "less") {
          result[["posterior"]][["hypotheses"]]$hypotheses <- c(paste0("H\u2081: \u0398 < ", materiality), paste0("H\u2080: \u0398 > ", materiality))
          result[["posterior"]][["hypotheses"]]$p.h1 <- switch(method,
            "poisson" = stats::pgamma(materiality, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
            "binomial" = stats::pbeta(materiality, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
            "hypergeometric" = extraDistr::pbbinom(ceiling(materiality * result[["N.units"]]) - 1, size = result[["N.units"]] - result[["n"]], alpha = result[["posterior"]][["description"]]$alpha, beta = result[["posterior"]][["description"]]$beta)
          )
          result[["posterior"]][["hypotheses"]]$p.h0 <- switch(method,
            "poisson" = stats::pgamma(materiality, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta, lower.tail = FALSE),
            "binomial" = stats::pbeta(materiality, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta, lower.tail = FALSE),
            "hypergeometric" = extraDistr::pbbinom(ceiling(materiality * result[["N.units"]]) - 1, size = result[["N.units"]] - result[["n"]], alpha = result[["posterior"]][["description"]]$alpha, beta = result[["posterior"]][["description"]]$beta, lower.tail = FALSE)
          )
        } else {
          result[["posterior"]][["hypotheses"]]$hypotheses <- c(paste0("H\u2081: \u0398 > ", materiality), paste0("H\u2080: \u0398 < ", materiality))
          result[["posterior"]][["hypotheses"]]$p.h0 <- switch(method,
            "poisson" = stats::pgamma(materiality, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
            "binomial" = stats::pbeta(materiality, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
            "hypergeometric" = extraDistr::pbbinom(ceiling(materiality * result[["N.units"]]) - 1, size = result[["N.units"]] - result[["n"]], alpha = result[["posterior"]][["description"]]$alpha, beta = result[["posterior"]][["description"]]$beta)
          )
          result[["posterior"]][["hypotheses"]]$p.h1 <- switch(method,
            "poisson" = stats::pgamma(materiality, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta, lower.tail = FALSE),
            "binomial" = stats::pbeta(materiality, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta, lower.tail = FALSE),
            "hypergeometric" = extraDistr::pbbinom(ceiling(materiality * result[["N.units"]]) - 1, size = result[["N.units"]] - result[["n"]], alpha = result[["posterior"]][["description"]]$alpha, beta = result[["posterior"]][["description"]]$beta, lower.tail = FALSE)
          )
        }
        result[["posterior"]][["hypotheses"]]$odds.h1 <- result[["posterior"]][["hypotheses"]]$p.h1 / result[["posterior"]][["hypotheses"]]$p.h0
        result[["posterior"]][["hypotheses"]]$odds.h0 <- 1 / result[["posterior"]][["hypotheses"]]$odds.h1
        result[["posterior"]][["hypotheses"]]$bf.h1 <- result[["posterior"]][["hypotheses"]]$odds.h1 / result[["prior"]][["hypotheses"]]$odds.h1
        result[["posterior"]][["hypotheses"]]$bf.h0 <- 1 / result[["posterior"]][["hypotheses"]]$bf.h1
      }
    }
    result[["posterior"]]$N.units <- result[["N.units"]]
    # Add class 'jfaPosterior' to the posterior distribution object.
    class(result[["posterior"]]) <- "jfaPosterior"
  }
  # Add the data and taints to the output
  if (!is.null(data) && !is.null(values) && !is.null(values.audit)) {
    indexa <- which(colnames(data) == values.audit)
    indexb <- which(colnames(data) == values)
    frame <- as.data.frame(data[, c(indexb, indexa)])
    frame <- cbind(as.numeric(rownames(frame)), frame)
    frame[["difference"]] <- frame[, 2] - frame[, 3]
    frame[["taint"]] <- frame[, 4] / frame[, 2]
    colnames(frame) <- c("row", values, values.audit, "difference", "taint")
    result[["data"]] <- frame
    result[["data.name"]] <- dname
  }
  # Add class 'jfaEvaluation' to the result.
  class(result) <- "jfaEvaluation"
  return(result)
}
