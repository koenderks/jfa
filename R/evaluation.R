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

#' Audit Sampling: Evaluation
#'
#' @description \code{evaluation()} is used to perform statistical inference about the misstatement in an audit population. It allows specification of statistical requirements for the sample with respect to the performance materiality or the precision. The function returns an object of class \code{jfaEvaluation} that can be used with associated \code{summary()} and \code{plot()} methods.
#'
#' @usage evaluation(materiality = NULL, min.precision = NULL, method = c(
#'              "poisson", "binomial", "hypergeometric",
#'              "stringer", "stringer.meikle", "stringer.lta", "stringer.pvz",
#'              "rohrbach", "moment", "coxsnell",
#'              "direct", "difference", "quotient", "regression", "mpu"
#'            ), alternative = c('less', 'two.sided', 'greater'), conf.level = 0.95,
#'            data = NULL, values = NULL, values.audit = NULL, strata = NULL, times = NULL,
#'            x = NULL, n = NULL, N.units = NULL, N.items = NULL,
#'            pooling = c("none", "complete", "partial"),
#'            prior = FALSE)
#'
#' @param materiality   a numeric value between 0 and 1 specifying the performance materiality (i.e., the maximum tolerable misstatement) as a fraction of the total number of units in the population. Can be \code{NULL}, but \code{min.precision} should be specified in that case. Not used for methods \code{direct}, \code{difference}, \code{quotient}, and \code{regression}.
#' @param min.precision a numeric value between 0 and 1 specifying the minimum precision (i.e., upper bound minus most likely error) as a fraction of the total population size. Can be \code{NULL}, but \code{materiality} should be specified in that case.
#' @param method        a character specifying the inference method. Possible options are \code{poisson} (default), \code{binomial}, \code{hypergeometric}, \code{mpu}, \code{stringer}, \code{stringer.meikle}, \code{stringer.lta}, \code{stringer.pvz}, \code{rohrbach}, \code{moment}, \code{direct}, \code{difference}, \code{quotient}, or \code{regression}. See the details section for more information.
#' @param alternative   a character indicating the alternative hypothesis and the type of confidence / credible interval. Possible options are \code{less} (default), \code{two.sided}, or \code{greater}.
#' @param conf.level    a numeric value between 0 and 1 specifying the confidence level.
#' @param data          a data frame containing a data sample.
#' @param values        a character specifying name of a numeric column in \code{data} containing the book values of the items.
#' @param values.audit  a character specifying name of a numeric column in \code{data} containing the audit (true) values of the items.
#' @param strata        a character specifying name of a factor column in \code{data} indicating to which stratum each item belongs.
#' @param times         a character specifying name of a numeric column in \code{data} containing the number of times each item in \code{data} should be counted (e.g., due to being selected multiple times for the sample).
#' @param x             a numeric value larger than 0 specifying the sum of (proportional) misstatements in the sample. If specified, overrides the \code{data}, \code{values} and \code{values.audit} arguments and assumes that the data come from summary statistics specified by both \code{x} and \code{n}.
#' @param n             an integer larger than 0 specifying the number of items in the sample. If specified, overrides the \code{data}, \code{values} and \code{values.audit} arguments and assumes that the data come from summary statistics specified by both \code{x} and \code{n}.
#' @param N.units       an integer larger than 0 specifying the number of units in the population. Only used for methods \code{hypergeometric}, \code{direct}, \code{difference}, \code{quotient}, and \code{regression}.
#' @param N.items       an integer larger than 0 specifying the number of items in the population. Only used for methods \code{direct}, \code{difference}, \code{quotient}, and \code{regression}.
#' @param pooling       a character specifying the type of model to use when analyzing stratified samples. Possible options are \code{none} (default) for no pooling (i.e., no information is shared between strata), \code{complete} for complete pooling (i.e., all information is shared between strata) or \code{partial} for partial pooling (i.e., information can be shared between strata). The latter option fits a pre-compiled stan model to the data using a MCMC sampling procedure whose options can be set globally using \code{options("mcmc.iterations")} (otherwise: 2000), \code{options("mcmc.warmup")} (otherwise: 1000), \code{options("mcmc.chains")} (otherwise: 4) and \code{options("mcmc.cores")} (otherwise: 1).
#' @param prior         a logical specifying whether to use a prior distribution, or an object of class \code{jfaPrior} or \code{jfaPosterior} containing the prior distribution. If \code{FALSE} (default), performs classical planning. If \code{TRUE}, performs Bayesian planning using a default conjugate prior.
#'
#' @details This section lists the available options for the \code{methods} argument.
#'
#' \itemize{
#'  \item{\code{poisson}:         Evaluates the sample with the Poisson distribution. If combined with \code{prior = TRUE}, performs Bayesian evaluation using a \emph{gamma} prior and posterior.}
#'  \item{\code{binomial}:        Evaluates the sample with the binomial distribution. If combined with \code{prior = TRUE}, performs Bayesian evaluation using a \emph{beta} prior and posterior.}
#'  \item{\code{hypergeometric}:  Evaluates the sample with the hypergeometric distribution. If combined with \code{prior = TRUE}, performs Bayesian evaluation using a \emph{beta-binomial} prior and posterior.}
#' 	\item{\code{mpu}:             Evaluates the sample with the mean-per-unit estimator.}
#'  \item{\code{stringer}:        Evaluates the sample with the Stringer bound (Stringer, 1963).}
#'  \item{\code{stringer.meikle}: Evaluates the sample with the Stringer bound with Meikle's correction for understatements (Meikle, 1972).}
#'  \item{\code{stringer.lta}:    Evaluates the sample with the Stringer bound with LTA correction for understatements (Leslie, Teitlebaum, and Anderson, 1979).}
#'  \item{\code{stringer.pvz}:    Evaluates the sample with the Stringer bound with Pap and van Zuijlen's correction for understatements (Pap and van Zuijlen, 1996).}
#'  \item{\code{rohrbach}:        Evaluates the sample with Rohrbach's augmented variance bound (Rohrbach, 1993).}
#'  \item{\code{moment}:          Evaluates the sample with the modified moment bound (Dworin and Grimlund, 1984).}
#'  \item{\code{coxsnell}:        Evaluates the sample with the Cox and Snell bound (Cox and Snell, 1979).}
#'  \item{\code{direct}:          Evaluates the sample with the direct estimator (Touw and Hoogduin, 2011).}
#'  \item{\code{difference}:      Evaluates the sample with the difference estimator (Touw and Hoogduin, 2011).}
#'  \item{\code{quotient}:        Evaluates the sample with the quotient estimator (Touw and Hoogduin, 2011).}
#'  \item{\code{regression}:      Evaluates the sample with the regression estimator (Touw and Hoogduin, 2011).}
#' }
#'
#' @references Cox, D. and Snell, E. (1979). On sampling and the estimation of rare errors. \emph{Biometrika}, 66(1), 125-132.
#' @references Derks, K., de Swart, J., van Batenburg, P., Wagenmakers, E.-J., & Wetzels, R. (2021). Priors in a Bayesian audit: How integration of existing information into the prior distribution can improve audit transparency and efficiency. \emph{International Journal of Auditing}, 25(3), 621-636.
#' @references Dworin, L. D. and Grimlund, R. A. (1984). Dollar-unit sampling for accounts receivable and inventory. \emph{The Accounting Review}, 59(2), 218-241
#' @references Leslie, D. A., Teitlebaum, A. D., & Anderson, R. J. (1979). \emph{Dollar-unit Sampling: A Practical Guide for Auditors}. Copp Clark Pitman; Belmont, Calif.: distributed by Fearon-Pitman.
#' @references Meikle, G. R. (1972). \emph{Statistical Sampling in an Audit Context: An Audit Technique}. Canadian Institute of Chartered Accountants.
#' @references Pap, G., and van Zuijlen, M. C. (1996). On the asymptotic behavior of the Stringer bound. \emph{Statistica Neerlandica}, 50(3), 367-389.
#' @references Rohrbach, K. J. (1993). Variance augmentation to achieve nominal coverage probability in sampling from audit populations. \emph{Auditing}, 12(2), 79.
#' @references Stringer, K. W. (1963). Practical aspects of statistical sampling in auditing. \emph{In Proceedings of the Business and Economic Statistics Section} (pp. 405-411). American Statistical Association.
#' @references Touw, P., and Hoogduin, L. (2011). \emph{Statistiek voor Audit en Controlling}. Boom uitgevers Amsterdam.
#'
#' @return An object of class \code{jfaEvaluation} containing:
#'
#' \item{conf.level}{a numeric value between 0 and 1 giving the confidence level.}
#' \item{mle}{a numeric value between 0 and 1 giving the most likely error in the population.}
#' \item{ub}{a numeric value between 0 and 1 giving the upper bound for the population misstatement.}
#' \item{lb}{{a numeric value between 0 and 1 giving the lower bound for the population misstatement.}}
#' \item{precision}{a numeric value between 0 and 1 giving the difference between the most likely error and the upper bound.}
#' \item{p.value}{for classical tests, a numeric value giving the one-sided p-value.}
#' \item{x}{an integer larger than, or equal to, 0 giving the number of sample errors.}
#' \item{t}{a value larger than, or equal to, 0, giving the sum of proportional sample errors.}
#' \item{n}{an integer larger than 0 giving the sample size.}
#' \item{materiality}{if \code{materiality} is specified, a numeric value between 0 and 1 giving the performance materiality as a fraction of the number of units in the population.}
#' \item{min.precision}{if \code{min.precision} is specified, a numeric value between 0 and 1 giving the minimum precision as a fraction of the number of units in the population.}
#' \item{alternative}{a character indicating the alternative hypothesis.}
#' \item{method}{a character indicating the inference method.}
#' \item{N.units}{if \code{N.units} is specified, in integer larger than 0 indicating the number of units in the population.}
#' \item{N.items}{if \code{N.items} is specified, in integer larger than 0 indicating the number of items in the population.}
#' \item{K}{if \code{method = 'hypergeometric'}, an integer indicating the assumed total errors in the population.}
#' \item{prior}{an object of class 'jfaPrior' that contains the prior distribution.}
#' \item{posterior}{an object of class 'jfaPosterior' that contains the posterior distribution.}
#' \item{data}{a data frame containing the relevant columns from the \code{data}.}
#' \item{strata}{a data frame containing the relevant statistical results for the strata.}
#' \item{data.name}{a character giving the name of the data.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}} \code{\link{planning}} \code{\link{selection}} \code{\link{report}}
#'
#' @keywords audit evaluation prior
#'
#' @examples
#' # Using summary statistics
#' evaluation(materiality = 0.05, x = 0, n = 100)
#' evaluation(materiality = 0.05, x = c(2, 1, 0), n = c(50, 70, 40))
#'
#' # Using data
#' data("BuildIt")
#' BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
#' levs <- c("low", "medium", "high")
#' BuildIt$stratum <- factor(c(levs[3], levs[2], rep(levs, times = 1166)))
#' sample <- subset(BuildIt, BuildIt$inSample == 1)
#'
#' evaluation(
#'   materiality = 0.05, data = sample,
#'   values = "bookValue", values.audit = "auditValue"
#' )
#' evaluation(
#'   materiality = 0.05, data = sample, values = "bookValue",
#'   values.audit = "auditValue", strata = "stratum"
#' )
#' @export

evaluation <- function(materiality = NULL, min.precision = NULL, method = c(
                         "poisson", "binomial", "hypergeometric",
                         "stringer", "stringer.meikle", "stringer.lta", "stringer.pvz",
                         "rohrbach", "moment", "coxsnell",
                         "direct", "difference", "quotient", "regression", "mpu"
                       ), alternative = c("less", "two.sided", "greater"), conf.level = 0.95,
                       data = NULL, values = NULL, values.audit = NULL, strata = NULL, times = NULL,
                       x = NULL, n = NULL, N.units = NULL, N.items = NULL,
                       pooling = c("none", "complete", "partial"),
                       prior = FALSE) {
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  pooling <- match.arg(pooling)
  bayesian <- (inherits(prior, "logical") && prior) || inherits(prior, "jfaPrior") || inherits(prior, "jfaPosterior")
  # Import existing prior distribution with class 'jfaPrior' or 'jfaPosterior'.
  if (inherits(prior, "jfaPrior") || inherits(prior, "jfaPosterior")) {
    if (method != prior[["likelihood"]]) {
      message(paste0("Using 'method = ", prior[["likelihood"]], "' from 'prior'"))
    }
    prior.x <- prior[["description"]]$implicit.x
    prior.n <- prior[["description"]]$implicit.n
    method <- prior[["likelihood"]]
    if (!is.null(prior[["N.units"]])) {
      message(paste0("Using 'N.units = ", prior[["N.units"]], "' from 'prior'"))
      N.units <- prior[["N.units"]]
    }
  } else {
    prior.n <- 1
    prior.x <- 0
  }
  stopifnot(
    "missing value for 'conf.level'" = !is.null(conf.level),
    "'conf.level' must be a single value between 0 and 1" = is.numeric(conf.level) && length(conf.level) == 1 && conf.level > 0 && conf.level < 1,
    "missing value for 'materiality' or `min.precision`" = !(is.null(materiality) && is.null(min.precision))
  )
  if (!is.null(materiality)) {
    stopifnot("'materiality' must be a single value between 0 and 1" = is.numeric(materiality) && materiality > 0 && materiality < 1)
  }
  if (!is.null(min.precision)) {
    stopifnot("'min.precision' must be a single value between 0 and 1" = is.numeric(min.precision) && min.precision > 0 && min.precision < 1)
  }
  if (bayesian) {
    stopifnot("'method' should be one of 'poisson', 'binomial', or 'hypergeometric'" = method %in% c("poisson", "binomial", "hypergeometric"))
  }
  if (alternative %in% c("two.sided", "greater") && method %in% c("stringer", "stringer.meikle", "stringer.lta", "stringer.pvz")) {
    stop(paste0("'method = ", method, "' does not accomodate 'alternative = ", alternative, "'"))
  }
  stopifnot("missing value(s) for 'data' or a combination of 'x' and 'n'" = !is.null(x) || !is.null(n) || !is.null(data))
  if (!is.null(x) || !is.null(n)) { # Use summary statistics
    if (method %in% c("stringer", "stringer.meikle", "stringer.lta", "stringer.pvz", "coxsnell", "rohrbach", "moment", "direct", "difference", "quotient", "regression", "mpu")) {
      stop(paste0("missing value for 'data' with 'method = ", method, "'"))
    }
    stopifnot(
      "missing value for 'n'" = !is.null(n),
      "all values in 'n' must be integers >= 0" = is.numeric(n) && all(n %% 1 == 0) && length(n) > 0 && all(n > 0),
      "missing value for 'x'" = !is.null(x),
      "all values in 'x' must be >= 0" = is.numeric(x) && length(x) > 0 && all(x >= 0),
      "all values in 'x' must be <= 'n'" = all(x <= n),
      "length(x) must be = length(n)" = length(x) == length(n)
    )
    if (any(x %% 1 != 0) && method == "hypergeometric" && !bayesian) {
      x <- ceiling(x)
      message(paste0("Using 'x = ", x, "' since 'x' must contain integers >= 0"))
    }
    if (!is.null(data)) {
      message("'x' and 'n' are used while 'data' is specified")
    }
    n.obs <- n
    x.obs <- x
    t.obs <- x
  } else if (!is.null(data)) { # Use data sample
    dname <- deparse(substitute(data))
    stopifnot(
      "missing value for 'values'" = !is.null(values),
      "'values' must be a single character" = is.character(values) && length(values) == 1
    )
    if (!(values %in% colnames(data))) {
      stop(paste0("'", values, "' is not a column in 'data'"))
    }
    stopifnot(
      "missing value for 'values.audit'" = !is.null(values.audit),
      "'values.audit' must be a single character" = is.character(values.audit) && length(values.audit) == 1
    )
    if (!(values.audit %in% colnames(data))) {
      stop(paste0("'", values.audit, "' is not a column in 'data'"))
    }
    if (!is.null(x) || !is.null(n)) {
      message("'data' is used while 'x' or 'n' is specified")
    }
    if (!is.null(times)) {
      if (!(times %in% colnames(data))) {
        stop(paste0("'", times, "' is not a column in 'data'"))
      }
      times <- data[, times]
      stopifnot("'times' contains missing values" = sum(!is.na(times)) == nrow(data),
                "column 'times' in 'data' must be a vector of integers" = all(times %% 1 == 0))
      data <- data[times > 0, ]
      times <- times[times > 0]
      n.obs <- sum(times)
    } else {
      n.obs <- nrow(data)
    }
    stopifnot("'data' contains missing values" = sum(stats::complete.cases(data)) == nrow(data))
    bookvalues <- auditvalues <- t <- list()
    if (is.null(strata)) {
      bookvalues[[1]] <- data[, values]
      auditvalues[[1]] <- data[, values.audit]
      t[[1]] <- (bookvalues[[1]] - auditvalues[[1]]) / bookvalues[[1]]
      x.obs <- length(which(t[[1]] != 0))
      if (!is.null(times)) { # Partial pooling uses item level taints
        t[[1]] <- t[[1]] * times
      }
      t.obs <- sum(t[[1]])
    } else {
      stopifnot(
        "'strata' must be a single character" = is.character(strata) && length(strata) == 1
      )
      if (!(strata %in% colnames(data))) {
        stop(paste0("'", strata, "' is not a column in 'data'"))
      }
      stratum <- data[, strata]
      stopifnot(
        "column 'strata' in 'data' must be a factor variable" = is.factor(stratum)
      )
      x.obs <- t.obs <- n.obs <- numeric(nlevels(stratum))
      for (i in 1:nlevels(stratum)) {
        index <- which(stratum == levels(stratum)[i])
        subdata <- data[index, ]
        n.obs[i] <- nrow(subdata)
        bookvalues[[i]] <- subdata[, values]
        auditvalues[[i]] <- subdata[, values.audit]
        t[[i]] <- (bookvalues[[i]] - auditvalues[[i]]) / bookvalues[[i]]
        x.obs[i] <- length(which(t[[i]] != 0))
        if (!is.null(times) && pooling != "partial") {
          t[[i]] <- t[[i]] * times[index]
          n.obs[i] <- sum(times[index])
        }
        t.obs[i] <- sum(t[[i]])
      }
      if (length(n.obs) > 1) {
        bookvalues <- c(data[, values], bookvalues)
        auditvalues <- c(data[, values.audit], bookvalues)
        t <- c(list(unlist(t)), t)
      }
    }
  }
  # Set the materiality and the minimium precision to 1 if they are NULL
  if (is.null(materiality)) {
    materiality <- 1
  }
  if (is.null(min.precision)) {
    min.precision <- 1
  }
  # When performing stratification, add the full population as a first stratum
  if (length(n.obs) > 1) {
    n.obs <- c(sum(n.obs), n.obs)
    x.obs <- c(sum(ceiling(x.obs)), ceiling(x.obs))
    t.obs <- c(sum(t.obs), t.obs)
    if (!is.null(N.units)) {
      N.units <- c(sum(N.units), N.units)
    }
    if (!is.null(N.items)) {
      N.items <- c(sum(N.items), N.items)
    }
  }
  nstrata <- length(t.obs)
  use_stratification <- nstrata > 1
  # Define placeholders for the most likely error and the precision
  mle <- ub <- lb <- precision <- p.val <- K <- numeric(nstrata)
  if (pooling != "partial" || length(t.obs) == 1) {
    for (i in 1:nstrata) {
      # Calculate the results per stratum depending on the specified 'method'
      if (method == "poisson") {
        if (bayesian) {
          # Bayesian evaluation using the gamma distribution
          mle[i] <- ((1 + prior.x + t.obs[i]) - 1) / (prior.n + n.obs[i])
          ub[i] <- switch(alternative,
            "two.sided" = stats::qgamma(p = conf.level + (1 - conf.level) / 2, shape = 1 + prior.x + t.obs[i], rate = prior.n + n.obs[i]),
            "less" = stats::qgamma(p = conf.level, shape = 1 + prior.x + t.obs[i], rate = prior.n + n.obs[i]),
            "greater" = 1
          )
          lb[i] <- switch(alternative,
            "two.sided" = stats::qgamma(p = (1 - conf.level) / 2, shape = 1 + prior.x + t.obs[i], rate = prior.n + n.obs[i]),
            "less" = 0,
            "greater" = stats::qgamma(p = 1 - conf.level, shape = 1 + prior.x + t.obs[i], rate = prior.n + n.obs[i])
          )
        } else {
          # Classical evaluation using the Poisson distribution
          mle[i] <- t.obs[i] / n.obs[i]
          ub[i] <- switch(alternative,
            "two.sided" = stats::qgamma(p = conf.level + (1 - conf.level) / 2, shape = 1 + t.obs[i], rate = n.obs[i]),
            "less" = stats::qgamma(p = conf.level, shape = 1 + t.obs[i], rate = n.obs[i]),
            "greater" = Inf
          )
          lb[i] <- switch(alternative,
            "two.sided" = stats::qgamma(p = (1 - conf.level) / 2, shape = t.obs[i], rate = n.obs[i]),
            "less" = 0,
            "greater" = stats::qgamma(p = 1 - conf.level, shape = t.obs[i], rate = n.obs[i])
          )
          if (materiality < 1) {
            p.val[i] <- switch(alternative,
              "two.sided" = stats::poisson.test(x = ceiling(t.obs[i]), T = n.obs[i], r = materiality, alternative = "two.sided")$p.value,
              "less" = stats::pgamma(q = materiality, shape = 1 + t.obs[i], rate = n.obs[i], lower.tail = FALSE),
              "greater" = stats::pgamma(q = materiality, shape = t.obs[i], rate = n.obs[i])
            )
          }
        }
      } else if (method == "binomial") {
        if (bayesian) {
          # Bayesian evaluation using the beta distribution
          mle[i] <- (1 + prior.x + t.obs[i] - 1) / ((1 + prior.x + t.obs[i]) + (prior.n - prior.x + n.obs[i] - t.obs[i]) - 2)
          ub[i] <- switch(alternative,
            "two.sided" = stats::qbeta(p = conf.level + (1 - conf.level) / 2, shape1 = 1 + prior.x + t.obs[i], shape2 = prior.n - prior.x + n.obs[i] - t.obs[i]),
            "less" = stats::qbeta(p = conf.level, shape1 = 1 + prior.x + t.obs[i], shape2 = prior.n - prior.x + n.obs[i] - t.obs[i]),
            "greater" = 1
          )
          lb[i] <- switch(alternative,
            "two.sided" = stats::qbeta(p = (1 - conf.level) / 2, shape1 = 1 + prior.x + t.obs[i], shape2 = prior.n - prior.x + n.obs[i] - t.obs[i]),
            "less" = 0,
            "greater" = stats::qbeta(p = 1 - conf.level, shape1 = 1 + prior.x + t.obs[i], shape2 = prior.n - prior.x + n.obs[i] - t.obs[i])
          )
        } else {
          # Classical evaluation using the binomial distribution
          mle[i] <- t.obs[i] / n.obs[i]
          ub[i] <- switch(alternative,
            "two.sided" = stats::qbeta(p = conf.level + (1 - conf.level) / 2, shape1 = 1 + t.obs[i], shape2 = n.obs[i] - t.obs[i]),
            "less" = stats::qbeta(p = conf.level, shape1 = 1 + t.obs[i], shape2 = n.obs[i] - t.obs[i]),
            "greater" = 1
          )
          lb[i] <- switch(alternative,
            "two.sided" = stats::qbeta(p = (1 - conf.level) / 2, shape1 = t.obs[i], shape2 = 1 + n.obs[i] - t.obs[i]),
            "less" = 0,
            "greater" = stats::qbeta(p = 1 - conf.level, shape1 = t.obs[i], shape2 = 1 + n.obs[i] - t.obs[i])
          )
          if (materiality < 1) {
            p.val[i] <- switch(alternative,
              "two.sided" = stats::binom.test(x = ceiling(t.obs[i]), n = n.obs[i], p = materiality, alternative = "two.sided")$p.value,
              "less" = stats::pbeta(q = materiality, shape1 = 1 + t.obs[i], shape2 = n.obs[i] - t.obs[i], lower.tail = FALSE),
              "greater" = stats::pbeta(q = materiality, shape1 = t.obs[i], shape2 = 1 + n.obs[i] - t.obs[i])
            )
          }
        }
      } else if (method == "hypergeometric") {
        stopifnot(
          "missing value for 'N.units'" = !is.null(N.units),
          "all values in 'N.units' must be > 0" = is.numeric(N.units) && length(N.units) > 0 && all(N.units > 0)
        )
        N.units <- ceiling(N.units)
        if (bayesian) {
          # Bayesian evaluation using the beta-binomial distribution
          mle[i] <- .modebbinom(N = N.units[i] - n.obs[i], shape1 = 1 + prior.x + t.obs[i], shape2 = prior.n - prior.x + n.obs[i] - t.obs[i]) / N.units[i]
          ub[i] <- switch(alternative,
            "two.sided" = .qbbinom(p = conf.level + (1 - conf.level) / 2, N = N.units[i] - n.obs[i], shape1 = 1 + prior.x + t.obs[i], shape2 = prior.n - prior.x + n.obs[i] - t.obs[i]) / N.units[i],
            "less" = .qbbinom(p = conf.level, N = N.units[i] - n.obs[i], shape1 = 1 + prior.x + t.obs[i], shape2 = prior.n - prior.x + n.obs[i] - t.obs[i]) / N.units[i],
            "greater" = 1
          )
          lb[i] <- switch(alternative,
            "two.sided" = .qbbinom(p = (1 - conf.level) / 2, N = N.units[i] - n.obs[i], shape1 = 1 + prior.x + t.obs[i], shape2 = prior.n - prior.x + n.obs[i] - t.obs[i]) / N.units[i],
            "less" = 0,
            "greater" = .qbbinom(p = 1 - conf.level, N = N.units[i] - n.obs[i], shape1 = 1 + prior.x + t.obs[i], shape2 = prior.n - prior.x + n.obs[i] - t.obs[i]) / N.units[i]
          )
        } else {
          # Classical evaluation using the hypergeometric distribution
          K[i] <- ceiling(materiality * N.units[i])
          mle[i] <- x.obs[i] / n.obs[i]
          ub[i] <- switch(alternative,
            "two.sided" = .qhyper(p = conf.level + (1 - conf.level) / 2, N = N.units[i], n = n.obs[i], k = x.obs[i]) / N.units[i],
            "less" = .qhyper(p = conf.level, N = N.units[i], n = n.obs[i], k = x.obs[i]) / N.units[i],
            "greater" = 1
          )
          lb[i] <- switch(alternative,
            "two.sided" = .qhyper(p = (1 - conf.level) / 2, N = N.units[i], n = n.obs[i], k = x.obs[i]) / N.units[i],
            "less" = 0,
            "greater" = .qhyper(p = 1 - conf.level, N = N.units[i], n = n.obs[i], k = x.obs[i]) / N.units[i]
          )
          if (materiality < 1) {
            p.val[i] <- switch(alternative,
              "two.sided" = stats::fisher.test(matrix(c(x.obs[i], n.obs[i] - x.obs[i], K[i] - x.obs[i], N.units[i] - n.obs[i] - K[i] + x.obs[i]), nrow = 2), alternative = "two.sided")$p.value,
              "less" = stats::phyper(q = x.obs[i], m = K[i], n = N.units[i] - K[i], k = n.obs[i]),
              "greater" = stats::phyper(q = x.obs[i] - 1, m = K[i], n = N.units[i] - K[i], k = n.obs[i], lower.tail = FALSE)
            )
          }
        }
      } else {
        out <- switch(method,
          "stringer" = .stringer(t[[i]], conf.level, n.obs[i]), # Classical evaluation using the Stringer bound
          "stringer.meikle" = .stringer(t[[i]], conf.level, n.obs[i], correction = "meikle"), # Classical evaluation using the Stringer bound with Meikle's adjustment
          "stringer.lta" = .stringer(t[[i]], conf.level, n.obs[i], correction = "lta"), # Classical evaluation using the Stringer bound with the LTA adjustment
          "stringer.pvz" = .stringer(t[[i]], conf.level, n.obs[i], correction = "pvz"), # Classical evaluation using the Stringer bound with PvZ adjustment
          "rohrbach" = .rohrbach(t[[i]], conf.level, n.obs[i], alternative, N.units[i], r.delta = 2.7), # Classical evaluation using Rohrbachs augmented variance bound
          "moment" = .moment(t[[i]], conf.level, n.obs[i], alternative, m.type = "accounts"), # Classical evaluation using the Modified Moment bound
          "coxsnell" = .coxsnell(t[[i]], conf.level, n.obs[i], alternative, cs.a = 1, cs.b = 3, cs.mu = 0.5, 1 + prior.x, prior.n - prior.x), # Bayesian evaluation using the Cox and Snell bound
          "mpu" = .mpu(t[[i]], conf.level, alternative, n.obs[i]), # Classical evaluation using the Mean-per-unit estimator
          "direct" = .direct(bookvalues[[i]], auditvalues[[i]], conf.level, alternative, N.items[i], n.obs[i], N.units[i]), # Classical evaluation using the Direct estimator
          "difference" = .difference(bookvalues[[i]], auditvalues[[i]], conf.level, alternative, N.items[i], n.obs[i]), # Classical evaluation using the Difference estimator
          "quotient" = .quotient(bookvalues[[i]], auditvalues[[i]], conf.level, alternative, N.items[i], n.obs[i]), # Classical evaluation using the Quotient estimator
          "regression" = .regression(bookvalues[[i]], auditvalues[[i]], conf.level, alternative, N.items[i], n.obs[i], N.units[i]), # Classical evaluation using the Regression estimator
          "newmethod" = NULL
        ) # Add new method here
        mle[i] <- out[["mle"]]
        ub[i] <- out[["ub"]]
        lb[i] <- out[["lb"]]
      }
      precision[i] <- if (alternative == "greater") mle[i] - lb[i] else ub[i] - mle[i]
    }
  } else {
    stopifnot("pooling = 'partial' only possible when 'prior != FALSE'" = bayesian)
    # Fit multilevel model, see jfa-internal.R
    if (any(t.obs %% 1 != 0) && !is.null(data)) { # If there are broken taints, use the beta likelihood
      samples <- .partial_pooling(method, prior.x, prior.n, n.obs, t.obs, t, nstrata, stratum, likelihood = "beta")
    } else {
      if (any(t.obs %% 1 != 0)) {
        warning("sum of taints in each stratum is rounded upwards")
        t.obs <- ceiling(t.obs)
      }
      samples <- .partial_pooling(method, prior.x, prior.n, n.obs, t.obs, t = NULL, nstrata, stratum, likelihood = "binomial")
    }
    for (i in 2:nstrata) {
      mle[i] <- .getmode(samples[, i - 1])
      lb[i] <- switch(alternative,
        "two.sided" = stats::quantile(samples[, i - 1], probs = (1 - conf.level) / 2),
        "less" = 0,
        "greater" = stats::quantile(samples[, i - 1], probs = 1 - conf.level)
      )
      ub[i] <- switch(alternative,
        "two.sided" = stats::quantile(samples[, i - 1], probs = conf.level + (1 - conf.level) / 2),
        "less" = stats::quantile(samples[, i - 1], probs = conf.level),
        "greater" = 1
      )
      precision[i] <- if (alternative == "greater") mle[i] - lb[i] else ub[i] - mle[i]
    }
  }
  # Create the main results object
  result <- list()
  result[["conf.level"]] <- conf.level
  if (!use_stratification || pooling == "complete" || !(method %in% c("poisson", "binomial", "hypergeometric"))) { # Only results for population
    if (!(method %in% c("poisson", "binomial", "hypergeometric"))) {
      warning("population results are displayed for aggregated sample")
    }
    result[["mle"]] <- mle[1]
    result[["ub"]] <- ub[1]
    result[["lb"]] <- lb[1]
    result[["precision"]] <- precision[1]
  } else { # Poststratification to get to population results
    if (pooling != "partial") {
      samples <- .fake_mcmc(method, nstrata, bayesian, prior.x, t.obs, prior.n, n.obs, N.units, iterations = 100000)
    }
    prior_samples <- .poststratify_samples(samples[, nstrata:ncol(samples)], N.units, nstrata)
    posterior_samples <- .poststratify_samples(samples[, 1:(nstrata - 1)], N.units, nstrata)
    result[["mle"]] <- .getmode(posterior_samples)
    result[["lb"]] <- switch(alternative,
      "two.sided" = as.numeric(stats::quantile(posterior_samples, probs = (1 - conf.level) / 2)),
      "less" = 0,
      "greater" = as.numeric(stats::quantile(posterior_samples, probs = 1 - conf.level))
    )
    result[["ub"]] <- switch(alternative,
      "two.sided" = as.numeric(stats::quantile(posterior_samples, probs = conf.level + (1 - conf.level) / 2)),
      "less" = as.numeric(stats::quantile(posterior_samples, probs = conf.level)),
      "greater" = 1
    )
  }
  result[["precision"]] <- if (alternative == "greater") result[["mle"]] - result[["lb"]] else result[["ub"]] - result[["mle"]]
  if (!bayesian && materiality < 1 && method %in% c("binomial", "poisson", "hypergeometric")) {
    if (!use_stratification || pooling == "complete") {
      result[["p.value"]] <- p.val[1]
    } else {
      result[["p.value"]] <- NA
    }
  }
  # Add the stratum results
  if (use_stratification) {
    if (pooling == "complete") {
      mle <- rep(mle[1], nstrata - 1)
      lb <- rep(lb[1], nstrata - 1)
      ub <- rep(ub[1], nstrata - 1)
      precision <- rep(precision[1], nstrata - 1)
    } else {
      mle <- mle[-1]
      lb <- lb[-1]
      ub <- ub[-1]
      precision <- precision[-1]
    }
    result[["strata"]] <- data.frame(n = n.obs[-1], x = x.obs[-1], t = t.obs[-1], mle = mle, lb = lb, ub = ub, precision = precision)
    if (pooling == "complete") {
      if (!bayesian && materiality < 1 && method %in% c("binomial", "poisson", "hypergeometric")) {
        result[["strata"]][["p.value"]] <- p.val[1]
      }
      if (bayesian && materiality != 1 && method %in% c("binomial", "poisson", "hypergeometric")) {
        if (alternative == "less") {
          result[["strata"]][["bf10"]] <- switch(method,
            "poisson" = (stats::pgamma(materiality, 1 + prior.x + t.obs[1], prior.n + n.obs[1]) / stats::pgamma(materiality, 1 + prior.x + t.obs[1], prior.n + n.obs[1], lower.tail = FALSE)) / (stats::pgamma(materiality, 1 + prior.x, prior.n) / stats::pgamma(materiality, 1 + prior.x, prior.n, lower.tail = FALSE)),
            "binomial" = (stats::pbeta(materiality, 1 + prior.x + t.obs[1], prior.n - prior.x + n.obs[1] - t.obs[1]) / stats::pbeta(materiality, 1 + prior.x + t.obs[1], prior.n - prior.x + n.obs[1] - t.obs[1], lower.tail = FALSE)) / (stats::pbeta(materiality, 1 + prior.x, prior.n - prior.x) / stats::pbeta(materiality, 1 + prior.x, prior.n - prior.x, lower.tail = FALSE)),
            "hypergeometric" = (extraDistr::pbbinom(ceiling(materiality * N.units[1]) - 1, size = N.units[1] - n.obs[1], alpha = 1 + prior.x + t.obs[1], beta = prior.n - prior.x + n.obs[1] - t.obs[1]) / extraDistr::pbbinom(ceiling(materiality * N.units[1]) - 1, size = N.units[1] - n.obs[1], alpha = 1 + prior.x + t.obs[1], beta = prior.n - prior.x + n.obs[1] - t.obs[1], lower.tail = FALSE)) / (extraDistr::pbbinom(ceiling(materiality * N.units[1]) - 1, size = N.units[1], alpha = 1 + prior.x, beta = prior.n - prior.x) / extraDistr::pbbinom(ceiling(materiality * N.units[1]) - 1, size = N.units[1], alpha = 1 + prior.x, beta = prior.n - prior.x, lower.tail = FALSE))
          )
        } else if (alternative == "greater") {
          result[["strata"]][["bf10"]] <- switch(method,
            "poisson" = (stats::pgamma(materiality, 1 + prior.x + t.obs[1], prior.n + n.obs[1], lower.tail = FALSE) / stats::pgamma(materiality, 1 + prior.x + t.obs[1], prior.n + n.obs[1])) / (stats::pgamma(materiality, 1 + prior.x, prior.n, lower.tail = FALSE) / stats::pgamma(materiality, 1 + prior.x, prior.n)),
            "binomial" = (stats::pbeta(materiality, 1 + prior.x + t.obs[1], prior.n - prior.x + n.obs[1] - t.obs[-1], lower.tail = FALSE) / stats::pbeta(materiality, 1 + prior.x + t.obs[1], prior.n - prior.x + n.obs[1] - t.obs[1])) / (stats::pbeta(materiality, 1 + prior.x, prior.n - prior.x, lower.tail = FALSE) / stats::pbeta(materiality, 1 + prior.x, prior.n - prior.x)),
            "hypergeometric" = (extraDistr::pbbinom(ceiling(materiality * N.units[1]) - 1, size = N.units[1] - n.obs[1], alpha = 1 + prior.x + t.obs[1], beta = prior.n - prior.x + n.obs[1] - t.obs[1], lower.tail = FALSE) / extraDistr::pbbinom(ceiling(materiality * N.units[1]) - 1, size = N.units[1] - n.obs[1], alpha = 1 + prior.x + t.obs[1], beta = prior.n - prior.x + n.obs[1] - t.obs[1])) / (extraDistr::pbbinom(ceiling(materiality * N.units[1]) - 1, size = N.units[1], alpha = 1 + prior.x, beta = prior.n - prior.x, lower.tail = FALSE) / extraDistr::pbbinom(ceiling(materiality * N.units[1]) - 1, size = N.units[1], alpha = 1 + prior.x, beta = prior.n - prior.x))
          )
        } else if (alternative == "two.sided") {
          result[["strata"]][["bf10"]] <- switch(method,
            "poisson" = 1 / (stats::dgamma(materiality, 1 + prior.x + t.obs[1], prior.n + n.obs[1]) / stats::dgamma(materiality, 1 + prior.x, prior.n)),
            "binomial" = 1 / (stats::dbeta(materiality, 1 + prior.x + t.obs[1], prior.n - prior.x + n.obs[1] - t.obs[1]) / stats::dbeta(materiality, 1 + prior.x, prior.n - prior.x)),
            "hypergeometric" = 1 / (extraDistr::dbbinom(ceiling(materiality * N.units[1]), size = N.units[1] - n.obs[1], alpha = 1 + prior.x + t.obs[1], beta = prior.n - prior.x + n.obs[1] - t.obs[1]) / extraDistr::dbbinom(ceiling(materiality * N.units[1]), size = N.units[1], alpha = 1 + prior.x, beta = prior.n - prior.x))
          )
        }
      }
    } else if (pooling == "none") {
      if (!bayesian && materiality < 1 && method %in% c("binomial", "poisson", "hypergeometric")) {
        result[["strata"]][["p.value"]] <- p.val[-1]
      }
      if (bayesian && materiality != 1 && method %in% c("binomial", "poisson", "hypergeometric")) {
        if (alternative == "less") {
          result[["strata"]][["bf10"]] <- switch(method,
            "poisson" = (stats::pgamma(materiality, 1 + prior.x + t.obs[-1], prior.n + n.obs[-1]) / stats::pgamma(materiality, 1 + prior.x + t.obs[-1], prior.n + n.obs[-1], lower.tail = FALSE)) / (stats::pgamma(materiality, 1 + prior.x, prior.n) / stats::pgamma(materiality, 1 + prior.x, prior.n, lower.tail = FALSE)),
            "binomial" = (stats::pbeta(materiality, 1 + prior.x + t.obs[-1], prior.n - prior.x + n.obs[-1] - t.obs[-1]) / stats::pbeta(materiality, 1 + prior.x + t.obs[-1], prior.n - prior.x + n.obs[-1] - t.obs[-1], lower.tail = FALSE)) / (stats::pbeta(materiality, 1 + prior.x, prior.n - prior.x) / stats::pbeta(materiality, 1 + prior.x, prior.n - prior.x, lower.tail = FALSE)),
            "hypergeometric" = (extraDistr::pbbinom(ceiling(materiality * N.units[-1]) - 1, size = N.units[-1] - n.obs[-1], alpha = 1 + prior.x + t.obs[-1], beta = prior.n - prior.x + n.obs[-1] - t.obs[-1]) / extraDistr::pbbinom(ceiling(materiality * N.units[-1]) - 1, size = N.units[-1] - n.obs[-1], alpha = 1 + prior.x + t.obs[-1], beta = prior.n - prior.x + n.obs[-1] - t.obs[-1], lower.tail = FALSE)) / (extraDistr::pbbinom(ceiling(materiality * N.units[-1]) - 1, size = N.units[-1], alpha = 1 + prior.x, beta = prior.n - prior.x) / extraDistr::pbbinom(ceiling(materiality * N.units[-1]) - 1, size = N.units[-1], alpha = 1 + prior.x, beta = prior.n - prior.x, lower.tail = FALSE))
          )
        } else if (alternative == "greater") {
          result[["strata"]][["bf10"]] <- switch(method,
            "poisson" = (stats::pgamma(materiality, 1 + prior.x + t.obs[-1], prior.n + n.obs[-1], lower.tail = FALSE) / stats::pgamma(materiality, 1 + prior.x + t.obs[-1], prior.n + n.obs[-1])) / (stats::pgamma(materiality, 1 + prior.x, prior.n, lower.tail = FALSE) / stats::pgamma(materiality, 1 + prior.x, prior.n)),
            "binomial" = (stats::pbeta(materiality, 1 + prior.x + t.obs[-1], prior.n - prior.x + n.obs[-1] - t.obs[-1], lower.tail = FALSE) / stats::pbeta(materiality, 1 + prior.x + t.obs[-1], prior.n - prior.x + n.obs[-1] - t.obs[-1])) / (stats::pbeta(materiality, 1 + prior.x, prior.n - prior.x, lower.tail = FALSE) / stats::pbeta(materiality, 1 + prior.x, prior.n - prior.x)),
            "hypergeometric" = (extraDistr::pbbinom(ceiling(materiality * N.units[-1]) - 1, size = N.units[-1] - n.obs[-1], alpha = 1 + prior.x + t.obs[-1], beta = prior.n - prior.x + n.obs[-1] - t.obs[-1], lower.tail = FALSE) / extraDistr::pbbinom(ceiling(materiality * N.units[-1]) - 1, size = N.units[-1] - n.obs[-1], alpha = 1 + prior.x + t.obs[-1], beta = prior.n - prior.x + n.obs[-1] - t.obs[-1])) / (extraDistr::pbbinom(ceiling(materiality * N.units[-1]) - 1, size = N.units[-1], alpha = 1 + prior.x, beta = prior.n - prior.x, lower.tail = FALSE) / extraDistr::pbbinom(ceiling(materiality * N.units[-1]) - 1, size = N.units[-1], alpha = 1 + prior.x, beta = prior.n - prior.x))
          )
        } else if (alternative == "two.sided") {
          result[["strata"]][["bf10"]] <- switch(method,
            "poisson" = 1 / (stats::dgamma(materiality, 1 + prior.x + t.obs[-1], prior.n + n.obs[-1]) / stats::dgamma(materiality, 1 + prior.x, prior.n)),
            "binomial" = 1 / (stats::dbeta(materiality, 1 + prior.x + t.obs[-1], prior.n - prior.x + n.obs[-1] - t.obs[-1]) / stats::dbeta(materiality, 1 + prior.x, prior.n - prior.x)),
            "hypergeometric" = 1 / (extraDistr::dbbinom(ceiling(materiality * N.units[-1]), size = N.units[-1] - n.obs[-1], alpha = 1 + prior.x + t.obs[-1], beta = prior.n - prior.x + n.obs[-1] - t.obs[-1]) / extraDistr::dbbinom(ceiling(materiality * N.units[-1]), size = N.units[-1], alpha = 1 + prior.x, beta = prior.n - prior.x))
          )
        }
      }
    } else if (pooling == "partial") {
      if (materiality != 1) {
        if (alternative == "less") {
          result[["strata"]][["bf10"]] <- (apply(samples[, 1:(nstrata - 1)], 2, function(x) length(which(x < materiality)) / length(x)) / apply(samples[, 1:(nstrata - 1)], 2, function(x) length(which(x > materiality)) / length(x))) / (apply(samples[, nstrata:ncol(samples)], 2, function(x) length(which(x < materiality)) / length(x)) / apply(samples[, nstrata:ncol(samples)], 2, function(x) length(which(x > materiality)) / length(x)))
        } else if (alternative == "greater") {
          result[["strata"]][["bf10"]] <- (apply(samples[, 1:(nstrata - 1)], 2, function(x) length(which(x > materiality)) / length(x)) / apply(samples[, 1:(nstrata - 1)], 2, function(x) length(which(x < materiality)) / length(x))) / (apply(samples[, nstrata:ncol(samples)], 2, function(x) length(which(x > materiality)) / length(x)) / apply(samples[, nstrata:ncol(samples)], 2, function(x) length(which(x < materiality)) / length(x)))
        } else if (alternative == "two.sided") {
          for (i in 1:(nstrata - 1)) {
            result[["strata"]][["bf10"]][i] <- 1 / (stats::approx(stats::density(samples[, i], from = 0, to = 1)$x, stats::density(samples[, i], from = 0, to = 1)$y, materiality)$y / stats::approx(stats::density(samples[, (nstrata - 1) + i], from = 0, to = 1)$x, stats::density(samples[, (nstrata - 1) + i], from = 0, to = 1)$y, materiality)$y)
          }
        }
      }
    }
    rownames(result[["strata"]]) <- if (is.null(strata)) 1:nrow(result[["strata"]]) else levels(stratum)
  }
  result[["x"]] <- x.obs[1]
  result[["t"]] <- t.obs[1]
  result[["n"]] <- n.obs[1]
  result[["materiality"]] <- materiality
  result[["min.precision"]] <- min.precision
  result[["alternative"]] <- alternative
  result[["method"]] <- method
  result[["pooling"]] <- pooling
  if (!is.null(N.units)) {
    result[["N.units"]] <- N.units[1]
  }
  if (!is.null(N.items)) {
    result[["N.items"]] <- N.items[1]
  }
  if (method == "hypergeometric" && is.logical(prior) && prior == FALSE) {
    result[["K"]] <- K[1]
  }
  # Create the prior distribution object
  if ((inherits(prior, "logical") && prior) || inherits(prior, "jfaPrior") || inherits(prior, "jfaPosterior")) {
    if (inherits(prior, "jfaPrior") && !is.null(prior[["hypotheses"]])) {
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
    analytical_posterior <- !use_stratification || pooling == "complete"
    # Create the posterior distribution object
    result[["posterior"]] <- list()
    # Functional form of the posterior distribution
    if (analytical_posterior) {
      result[["posterior"]]$posterior <- switch(method,
        "poisson" = paste0("gamma(\u03B1 = ", round(result[["prior"]][["description"]]$alpha + result[["t"]], 3), ", \u03B2 = ", round(result[["prior"]][["description"]]$beta + result[["n"]], 3), ")"),
        "binomial" = paste0("beta(\u03B1 = ", round(result[["prior"]][["description"]]$alpha + result[["t"]], 3), ", \u03B2 = ", round(result[["prior"]][["description"]]$beta + result[["n"]] - result[["t"]], 3), ")"),
        "hypergeometric" = paste0("beta-binomial(N = ", result[["N.units"]] - result[["n"]], ", \u03B1 = ", round(result[["prior"]][["description"]]$alpha + result[["t"]], 3), ", \u03B2 = ", round(result[["prior"]][["description"]]$beta + result[["n"]] - result[["t"]], 3), ")")
      )
    } else {
      result[["posterior"]]$posterior <- "Approximated via MCMC sampling"
    }
    result[["posterior"]]$likelihood <- method
    # Create the description section
    result[["posterior"]][["description"]] <- list()
    if (analytical_posterior) {
      result[["posterior"]][["description"]]$density <- switch(method,
        "poisson" = "gamma",
        "binomial" = "beta",
        "hypergeometric" = "beta-binomial"
      )
    }
    result[["posterior"]][["description"]]$n <- result[["n"]]
    result[["posterior"]][["description"]]$x <- result[["t"]]
    if (analytical_posterior) {
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
    }
    # Create the statistics section
    result[["posterior"]][["statistics"]] <- list()
    if (analytical_posterior) {
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
    } else {
      result[["posterior"]][["statistics"]]$mode <- result[["mle"]]
      result[["posterior"]][["statistics"]]$mean <- mean(posterior_samples)
      result[["posterior"]][["statistics"]]$median <- stats::median(posterior_samples)
      result[["posterior"]][["statistics"]]$var <- stats::var(posterior_samples)
      result[["posterior"]][["statistics"]]$skewness <- moments::skewness(posterior_samples)
      result[["posterior"]][["statistics"]]$ub <- result[["ub"]]
      result[["posterior"]][["statistics"]]$lb <- result[["lb"]]
      result[["posterior"]][["statistics"]]$precision <- result[["precision"]]
    }
    # Create the hypotheses section
    if (result[["materiality"]] != 1) {
      result[["posterior"]][["hypotheses"]] <- list()
      if (alternative == "two.sided") {
        result[["posterior"]][["hypotheses"]]$hypotheses <- c(paste0("H\u2080: \u0398 = ", materiality), paste0("H\u2081: \u0398 \u2260 ", materiality))
        if (analytical_posterior) {
          result[["posterior"]][["hypotheses"]]$density <- switch(method,
            "poisson" = stats::dgamma(materiality, shape = result[["posterior"]][["description"]]$alpha, rate = result[["posterior"]][["description"]]$beta),
            "binomial" = stats::dbeta(materiality, shape1 = result[["posterior"]][["description"]]$alpha, shape2 = result[["posterior"]][["description"]]$beta),
            "hypergeometric" = extraDistr::dbbinom(ceiling(materiality * N.units), size = N.units, alpha = result[["posterior"]][["description"]]$alpha, beta = result[["posterior"]][["description"]]$beta)
          )
          result[["posterior"]][["hypotheses"]]$bf.h0 <- result[["posterior"]][["hypotheses"]]$density / result[["prior"]][["hypotheses"]]$density
        } else {
          result[["posterior"]][["hypotheses"]]$density <- stats::approx(stats::density(posterior_samples, from = 0, to = 1)$x, stats::density(posterior_samples, from = 0, to = 1)$y, materiality)$y
          result[["posterior"]][["hypotheses"]]$bf.h0 <- result[["posterior"]][["hypotheses"]]$density / stats::approx(stats::density(prior_samples, from = 0, to = 1)$x, stats::density(prior_samples, from = 0, to = 1)$y, materiality)$y
        }
        result[["posterior"]][["hypotheses"]]$bf.h1 <- 1 / result[["posterior"]][["hypotheses"]]$bf.h0
      } else {
        if (analytical_posterior) {
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
        } else {
          if (alternative == "less") {
            result[["posterior"]][["hypotheses"]]$hypotheses <- c(paste0("H\u2081: \u0398 < ", materiality), paste0("H\u2080: \u0398 > ", materiality))
            result[["posterior"]][["hypotheses"]]$p.h0 <- length(which(posterior_samples > materiality)) / length(posterior_samples)
            result[["posterior"]][["hypotheses"]]$p.h1 <- length(which(posterior_samples < materiality)) / length(posterior_samples)
            result[["posterior"]][["hypotheses"]]$odds.h1 <- result[["posterior"]][["hypotheses"]]$p.h1 / result[["posterior"]][["hypotheses"]]$p.h0
            result[["posterior"]][["hypotheses"]]$odds.h0 <- 1 / result[["posterior"]][["hypotheses"]]$odds.h1
            result[["posterior"]][["hypotheses"]]$bf.h1 <- result[["posterior"]][["hypotheses"]]$odds.h1 / ((length(which(prior_samples < materiality)) / length(prior_samples)) / (length(which(prior_samples > materiality)) / length(prior_samples)))
          } else if (alternative == "greater") {
            result[["posterior"]][["hypotheses"]]$hypotheses <- c(paste0("H\u2081: \u0398 > ", materiality), paste0("H\u2080: \u0398 < ", materiality))
            result[["posterior"]][["hypotheses"]]$p.h0 <- length(which(posterior_samples < materiality)) / length(posterior_samples)
            result[["posterior"]][["hypotheses"]]$p.h1 <- length(which(posterior_samples > materiality)) / length(posterior_samples)
            result[["posterior"]][["hypotheses"]]$odds.h1 <- result[["posterior"]][["hypotheses"]]$p.h1 / result[["posterior"]][["hypotheses"]]$p.h0
            result[["posterior"]][["hypotheses"]]$odds.h0 <- 1 / result[["posterior"]][["hypotheses"]]$odds.h1
            result[["posterior"]][["hypotheses"]]$bf.h1 <- result[["posterior"]][["hypotheses"]]$odds.h1 / ((length(which(prior_samples > materiality)) / length(prior_samples)) / (length(which(prior_samples < materiality)) / length(prior_samples)))
          }
          result[["posterior"]][["hypotheses"]]$bf.h0 <- 1 / result[["posterior"]][["hypotheses"]]$bf.h1
        }
      }
    }
    result[["posterior"]]$N.units <- result[["N.units"]]
    # Add class 'jfaPosterior' to the posterior distribution object.
    class(result[["posterior"]]) <- "jfaPosterior"
  }
  # Add the data and taints to the output
  if (!is.null(data) && !is.null(values) & !is.null(values.audit)) {
    indexa <- which(colnames(data) == values.audit)
    indexb <- which(colnames(data) == values)
    frame <- as.data.frame(data[, c(indexb, indexa)])
    frame <- cbind(as.numeric(rownames(frame)), frame)
    frame[["difference"]] <- frame[, 2] - frame[, 3]
    frame[["taint"]] <- frame[, 4] / frame[, 2]
    if (!is.null(times)) {
      frame[["times"]] <- times
    } else {
      frame[["times"]] <- 1
    }
    colnames(frame) <- c("row", values, values.audit, "difference", "taint", "times")
    if (use_stratification) {
      frame <- cbind(frame, "strata" = stratum)
    }
    result[["data"]] <- frame
    result[["data.name"]] <- dname
  }
  # Add class 'jfaEvaluation' to the result.
  class(result) <- "jfaEvaluation"
  return(result)
}
