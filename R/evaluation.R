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
#' @description \code{evaluation()} is used to perform statistical inference
#' about the misstatement in a population after auditing a statistical sample.
#' It allows specification of statistical requirements for the sample with
#' respect to the performance materiality or the precision. The function returns
#' an object of class \code{jfaEvaluation} that can be used with associated
#' \code{summary()} and \code{plot()} methods.
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
#' @param materiality   a numeric value between 0 and 1 specifying the
#'   performance materiality (i.e., the maximum tolerable misstatement in the
#'   population) as a fraction of the total number of units in the population.
#'   Can be \code{NULL}, but \code{min.precision} should be specified in that
#'   case. Not used for methods \code{direct}, \code{difference},
#'   \code{quotient}, and \code{regression}.
#' @param min.precision a numeric value between 0 and 1 specifying the minimum
#'   precision (i.e., the estimated upper bound minus the estimated most likely
#'   error) as a fraction of the total population size. Can be \code{NULL}, but
#'   \code{materiality} should be specified in that case.
#' @param method        a character specifying the inference method. Possible
#'   options are \code{poisson} (default), \code{binomial},
#'   \code{hypergeometric}, \code{mpu}, \code{stringer}, \code{stringer.meikle},
#'   \code{stringer.lta}, \code{stringer.pvz}, \code{rohrbach}, \code{moment},
#'   \code{direct}, \code{difference}, \code{quotient}, or \code{regression}.
#'   See the details section for more information.
#' @param alternative   a character indicating the alternative hypothesis and
#'   the type of confidence / credible interval returned by the function.
#'   Possible options are  \code{less} (default), \code{two.sided}, or
#'   \code{greater}.
#' @param conf.level    a numeric value between 0 and 1 specifying the
#'   confidence level (i.e., 1 - audit risk / detection risk).
#' @param data          a data frame containing a data sample.
#' @param values        a character specifying name of a numeric column in
#'   \code{data} containing the book values of the items.
#' @param values.audit  a character specifying name of a numeric column in
#'   \code{data} containing the audit (true) values of the items.
#' @param strata        a character specifying name of a factor column in
#'   \code{data} indicating to which stratum the item belongs.
#' @param times         a character specifying name of an integer column in
#'   \code{data} containing the number of times an item should be counted due
#'   to (not) being selected (multiple times) for the sample. Items for which
#'   this value is 0 will not be included in the evaluation.
#' @param x             a numeric value or vector of values equal to or larger
#'   than 0 specifying the sum of (proportional) misstatements in the sample or,
#'   if this is a vector, the sum of taints in each stratum. If this argument is
#'   specified, the input for the \code{data}, \code{values} and
#'   \code{values.audit} arguments is discarded and it is assumed that the data
#'   come from summary statistics specified by \code{x} and \code{n}.
#' @param n             an integer or vector of integers larger than 0
#'   specifying the sum of (proportional) misstatements in the sample or, if
#'   this is a vector, the sum of taints in each stratum. If this argument is
#'   specified, the input for the \code{data}, \code{values} and
#'   \code{values.audit} arguments is discarded and it is assumed that the data
#'   come from summary statistics specified by \code{x} and \code{n}.
#' @param N.units       a numeric value or vector of values than 0 specifying
#'   the total number of units in the population or, if this is a vector, the
#'   total number of units in each stratum of the population.
#'   This argument is strictly required for the \code{hypergeometric},
#'   \code{direct}, \code{difference}, \code{quotient}, and \code{regression}
#'   methods, but is also used in stratification to weigh the estimates of each
#'   individual stratum to arrive at the population estimate. If \code{NULL},
#'   each stratum is assumed to be equally represented in the population.
#' @param N.items       an integer larger than 0 specifying the number of items
#'   in the population. Only used for methods \code{direct}, \code{difference},
#'   \code{quotient}, and \code{regression}.
#' @param pooling       a character specifying the type of model to use when
#'   analyzing stratified samples. Possible options are \code{none} (default)
#'   for no pooling (i.e., no information is shared between strata),
#'   \code{complete} for complete pooling (i.e., all information is shared
#'   between strata) or \code{partial} for partial pooling (i.e., some
#'   information is shared between strata). The latter option fits a
#'   pre-compiled Stan model to the data using a MCMC sampling procedure whose
#'   options can be set globally using \code{options("mcmc.iterations")}
#'   (otherwise: 2000), \code{options("mcmc.warmup")} (otherwise: 1000),
#'   \code{options("mcmc.chains")} (otherwise: 4) and
#'   \code{options("mcmc.cores")} (otherwise: 1).
#' @param prior         a logical specifying whether to use a prior
#'   distribution, or an object of class \code{jfaPrior} or \code{jfaPosterior}.
#'   If this argument is specified as \code{FALSE} (default), the function
#'   performs classical evaluation If this argument is specified as \code{TRUE}
#'   or as a prior from \code{auditPrior}, this function performs Bayesian
#'   evaluation using a prior that is conjugate to the specified \code{method}.
#'
#' @details This section lists the available options for the \code{method}
#'   argument.
#'
#' \itemize{
#'  \item{\code{poisson}:         Evaluates the sample with the Poisson
#'    distribution. If combined with \code{prior = TRUE}, performs Bayesian
#'    evaluation using a \emph{gamma} prior.}
#'  \item{\code{binomial}:        Evaluates the sample with the binomial
#'    distribution. If combined with \code{prior = TRUE}, performs Bayesian
#'    evaluation using a \emph{beta} prior.}
#'  \item{\code{hypergeometric}:  Evaluates the sample with the hypergeometric
#'    distribution. If combined with \code{prior = TRUE}, performs Bayesian
#'    evaluation using a \emph{beta-binomial} prior.}
#'  \item{\code{mpu}:             Evaluates the sample using the mean-per-unit
#'    estimator.}
#'  \item{\code{stringer}:        Evaluates the sample using the Stringer
#'    bound (Stringer, 1963).}
#'  \item{\code{stringer.meikle}: Evaluates the sample using the Stringer bound
#'    with Meikle's correction for understatements (Meikle, 1972).}
#'  \item{\code{stringer.lta}:    Evaluates the sample using the Stringer bound
#'    with LTA correction for understatements (Leslie, Teitlebaum, and Anderson,
#'    1979).}
#'  \item{\code{stringer.pvz}:    Evaluates the sample using the Stringer bound
#'    with Pap and van Zuijlen's correction for understatements (Pap and van
#'    Zuijlen, 1996).}
#'  \item{\code{rohrbach}:        Evaluates the sample using Rohrbach's
#'    augmented variance bound (Rohrbach, 1993).}
#'  \item{\code{moment}:          Evaluates the sample using the modified moment
#'    bound (Dworin and Grimlund, 1984).}
#'  \item{\code{coxsnell}:        Evaluates the sample using the Cox and Snell
#'    bound (Cox and Snell, 1979).}
#'  \item{\code{direct}:          Evaluates the sample using the direct
#'    estimator (Touw and Hoogduin, 2011).}
#'  \item{\code{difference}:      Evaluates the sample using the difference
#'    estimator (Touw and Hoogduin, 2011).}
#'  \item{\code{quotient}:        Evaluates the sample using the quotient
#'    estimator (Touw and Hoogduin, 2011).}
#'  \item{\code{regression}:      Evaluates the sample using the regression
#'    estimator (Touw and Hoogduin, 2011).}
#' }
#'
#' @references Cox, D. and Snell, E. (1979). On sampling and the estimation of
#'   rare errors. \emph{Biometrika}, 66(1), 125-132.
#'   \doi{10.1093/biomet/66.1.125}.
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
#'   (2021). The Bayesian approach to audit evidence: Quantifying statistical
#'   evidence using the Bayes factor. \emph{PsyArXiv}.
#'   \doi{10.31234/osf.io/kzqp5}
#' @references Derks, K., de Swart, J., Wagenmakers, E.-J., & Wetzels, R.
#'   (2022). An impartial Bayesian hypothesis test for audit sampling.
#'   \emph{PsyArXiv}. \doi{10.31234/osf.io/8nf3e}
#' @references Derks, K., de Swart, J., Wagenmakers, E.-J., & Wetzels, R.
#'   (2022). Bayesian generalized linear modeling for audit sampling: How to
#'   incorporate audit information into the statistical model. \emph{PsyArXiv}.
#'   \doi{10.31234/osf.io/byj2a}
#' @references Dworin, L. D. and Grimlund, R. A. (1984). Dollar-unit sampling
#'   for accounts receivable and inventory. \emph{The Accounting Review}, 59(2),
#'   218-241. \url{https://www.jstor.org/stable/247296}
#' @references Leslie, D. A., Teitlebaum, A. D., & Anderson, R. J. (1979).
#'   \emph{Dollar-unit Sampling: A Practical Guide for Auditors}. Copp Clark
#'   Pitman; Belmont, CA. ISBN: 9780773042780.
#' @references Meikle, G. R. (1972). \emph{Statistical Sampling in an Audit
#'   Context}. Canadian Institute of Chartered Accountants.
#' @references Pap, G., and van Zuijlen, M. C. (1996). On the asymptotic
#'   behavior of the Stringer bound. \emph{Statistica Neerlandica}, 50(3),
#'   367-389. \doi{10.1111/j.1467-9574.1996.tb01503.x}.
#' @references Rohrbach, K. J. (1993). Variance augmentation to achieve nominal
#'   coverage probability in sampling from audit populations. \emph{Auditing},
#'   12(2), 79.
#' @references Stringer, K. W. (1963). Practical aspects of statistical sampling
#'   in auditing. \emph{In Proceedings of the Business and Economic Statistics
#'   Section} (pp. 405-411). American Statistical Association.
#' @references Touw, P., and Hoogduin, L. (2011). \emph{Statistiek voor Audit en
#'   Controlling}. Boom uitgevers Amsterdam.
#'
#' @return An object of class \code{jfaEvaluation} containing:
#'
#' \item{conf.level}{a numeric value between 0 and 1 giving the confidence
#'   level.}
#' \item{mle}{a numeric value between 0 and 1 giving the most likely
#'   misstatement in the population as a fraction.}
#' \item{ub}{a numeric value between 0 and 1 giving the upper bound for the
#'   misstatement in the population.}
#' \item{lb}{a numeric value between 0 and 1 giving the lower bound for the
#'   misstatement in the population.}
#' \item{precision}{a numeric value between 0 and 1 giving the difference
#'   between the most likely misstatement and the bound relative to
#'   \code{alternative}.}
#' \item{p.value}{for classical tests, a numeric value giving the p-value.}
#' \item{x}{an integer larger than, or equal to, 0 giving the number of
#'   misstatements in the sample.}
#' \item{t}{a value larger than, or equal to, 0, giving the sum of proportional
#'   misstatements in the sample.}
#' \item{n}{an integer larger than 0 giving the sample size.}
#' \item{materiality}{if \code{materiality} is specified, a numeric value
#'   between 0 and 1 giving the performance materiality as a fraction.}
#' \item{min.precision}{if \code{min.precision} is specified, a numeric value
#'   between 0 and 1 giving the minimum precision as a fraction.}
#' \item{alternative}{a character indicating the alternative hypothesis.}
#' \item{method}{a character the method used.}
#' \item{N.units}{if \code{N.units} is specified, in integer larger than 0
#'   indicating the number of units in the population}
#' \item{N.items}{if \code{N.items} is specified, in integer larger than 0
#'   indicating the number of items in the population.}
#' \item{K}{if \code{method = 'hypergeometric'}, an integer indicating the
#'   assumed total errors in the population.}
#' \item{prior}{an object of class \code{jfaPrior} that contains the prior
#'   distribution.}
#' \item{posterior}{an object of class \code{jfaPosterior} that contains the
#'   posterior distribution.}
#' \item{data}{a data frame containing the relevant columns from the
#'   \code{data}.}
#' \item{strata}{a data frame containing the relevant statistical results for
#'   the strata.}
#' \item{data.name}{a character giving the name of the data.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}}
#'          \code{\link{planning}}
#'          \code{\link{selection}}
#'          \code{\link{report}}
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

evaluation <- function(materiality = NULL,
                       min.precision = NULL,
                       method = c(
                         "poisson", "binomial", "hypergeometric",
                         "stringer", "stringer.meikle", "stringer.lta", "stringer.pvz",
                         "rohrbach", "moment", "coxsnell",
                         "direct", "difference", "quotient", "regression", "mpu"
                       ),
                       alternative = c("less", "two.sided", "greater"),
                       conf.level = 0.95,
                       data = NULL,
                       values = NULL,
                       values.audit = NULL,
                       strata = NULL,
                       times = NULL,
                       x = NULL,
                       n = NULL,
                       N.units = NULL,
                       N.items = NULL,
                       pooling = c("none", "complete", "partial"),
                       prior = FALSE) {
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  pooling <- match.arg(pooling)
  is_jfa_prior <- inherits(prior, "jfaPrior") || inherits(prior, "jfaPosterior")
  is_bayesian <- (inherits(prior, "logical") && prior) || is_jfa_prior
  if (is_jfa_prior) {
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
  valid_bayesian_method <- method %in% c("poisson", "binomial", "hypergeometric")
  if (is_bayesian) {
    stopifnot("'method' should be one of 'poisson', 'binomial', or 'hypergeometric'" = valid_bayesian_method)
  }
  is_stringer_method <- method %in% c("stringer", "stringer.meikle", "stringer.lta", "stringer.pvz")
  if (alternative %in% c("two.sided", "greater") && is_stringer_method) {
    stop(paste0("'method = ", method, "' does not accomodate 'alternative = ", alternative, "'"))
  }
  stopifnot("missing value(s) for 'data' or a combination of 'x' and 'n'" = !is.null(x) || !is.null(n) || !is.null(data))
  has_summary_statistics <- !is.null(x) || !is.null(n)
  has_data <- !is.null(data)
  if (has_summary_statistics) {
    valid_method <- !(method %in% c("stringer", "stringer.meikle", "stringer.lta", "stringer.pvz", "coxsnell", "rohrbach", "moment", "direct", "difference", "quotient", "regression", "mpu"))
    stopifnot("missing value for 'data'" = valid_method)
    stopifnot("missing value for 'n'" = !is.null(n))
    valid_n <- is.numeric(n) && all(n %% 1 == 0) && length(n) > 0 && all(n > 0)
    stopifnot("all values in 'n' must be integers >= 0" = valid_n)
    stopifnot("missing value for 'x'" = !is.null(x))
    valid_x <- is.numeric(x) && length(x) > 0 && all(x >= 0)
    stopifnot("all values in 'x' must be >= 0" = valid_x)
    stopifnot("all values in 'x' must be <= 'n'" = all(x <= n))
    stopifnot("length(x) must be = length(n)" = length(x) == length(n))
    broken_taints <- any(x %% 1 != 0)
    if (broken_taints && method == "hypergeometric" && !is_bayesian) {
      x <- ceiling(x)
      message(paste0("Using 'x = ", x, "' since 'x' must contain integers >= 0"))
    }
    if (has_data) {
      message("'x' and 'n' are used while 'data' is also specified")
    }
    n.obs <- n
    x.obs <- x
    t.obs <- x
  } else if (has_data) {
    dname <- deparse(substitute(data))
    stopifnot("missing value for 'values'" = !is.null(values))
    valid_values <- is.character(values) && length(values) == 1
    stopifnot("'values' must be a single character" = valid_values)
    stopifnot("column 'values' not found in 'data'" = values %in% colnames(data))
    stopifnot("missing value for 'values.audit'" = !is.null(values.audit))
    valid_values_audit <- is.character(values.audit) && length(values.audit) == 1
    stopifnot("'values.audit' must be a single character" = valid_values_audit)
    stopifnot("column 'values.audit' not found in 'data'" = values.audit %in% colnames(data))
    if (has_summary_statistics) {
      message("'data' is used while 'x' or 'n' are also specified")
    }
    if (!is.null(times)) {
      stopifnot("column 'times' not found in 'data'" = times %in% colnames(data))
      times <- data[, times]
      stopifnot("'times' contains missing values" = sum(!is.na(times)) == nrow(data))
      stopifnot("column 'times' in 'data' must be a vector of integers" = all(times %% 1 == 0))
      data <- data[times > 0, ]
      times <- times[times > 0]
      n.obs <- sum(times)
    } else {
      n.obs <- nrow(data)
    }
    stopifnot("'data' contains missing values" = sum(stats::complete.cases(data)) == nrow(data))
    book_values <- audit_values <- taints <- list()
    if (is.null(strata)) {
      book_values[[1]] <- data[, values]
      audit_values[[1]] <- data[, values.audit]
      taints[[1]] <- (book_values[[1]] - audit_values[[1]]) / book_values[[1]]
      x.obs <- length(which(taints[[1]] != 0))
      if (!is.null(times)) {
        taints[[1]] <- taints[[1]] * times
      }
      t.obs <- sum(taints[[1]])
    } else {
      valid_strata <- is.character(strata) && length(strata) == 1
      stopifnot("'strata' must be a single character" = valid_strata)
      strata_message <- paste0("'", strata, "' is not a column in 'data'")
      stopifnot(strata_message = strata %in% colnames(data))
      stratum <- data[, strata]
      stopifnot("column 'strata' in 'data' must be a factor variable" = is.factor(stratum))
      x.obs <- t.obs <- n.obs <- numeric(nlevels(stratum))
      for (i in 1:nlevels(stratum)) {
        stratum_indices <- which(stratum == levels(stratum)[i])
        stratum_data <- data[stratum_indices, ]
        n.obs[i] <- nrow(stratum_data)
        book_values[[i]] <- stratum_data[, values]
        audit_values[[i]] <- stratum_data[, values.audit]
        taints[[i]] <- (book_values[[i]] - audit_values[[i]]) / book_values[[i]]
        x.obs[i] <- length(which(taints[[i]] != 0))
        if (!is.null(times) && pooling != "partial") {
          taints[[i]] <- taints[[i]] * times[stratum_indices]
          n.obs[i] <- sum(times[stratum_indices])
        }
        t.obs[i] <- sum(taints[[i]])
      }
      if (length(n.obs) > 1) {
        book_values <- c(data[, values], book_values)
        audit_values <- c(data[, values.audit], book_values)
        all_taints <- list(rep(NA, length(stratum)))
        for (j in 1:nlevels(stratum)) {
          stratum_indices <- which(stratum == levels(stratum)[j])
          all_taints[[1]][stratum_indices] <- taints[[j]]
        }
        taints <- c(all_taints, taints)
      }
    }
    broken_taints <- any(t.obs %% 1 != 0)
  }
  if (is.null(materiality)) {
    materiality <- 1
  }
  if (is.null(min.precision)) {
    min.precision <- 1
  }
  if (length(n.obs) > 1) {
    n.obs <- c(sum(n.obs), n.obs)
    x.obs <- c(sum(ceiling(x.obs)), ceiling(x.obs))
    t.obs <- c(sum(t.obs), t.obs)
    if (!is.null(N.units)) {
      N.units <- c(sum(N.units), N.units)
      valid_units <- length(N.units) == length(t.obs)
      stopifnot("'N.units' must be equal to number of strata" = valid_units)
    }
    if (!is.null(N.items)) {
      N.items <- c(sum(N.items), N.items)
      valid_items <- length(N.items) == length(t.obs)
      stopifnot("'N.items' must be equal to number of strata" = valid_items)
    }
  } else {
    if (!is.null(N.units)) {
      valid_units <- length(N.units) == 1
      stopifnot("'N.units' must be a single integer" = valid_units)
    }
    if (!is.null(N.items)) {
      valid_items <- length(N.items) == 1
      stopifnot("'N.items' must be a single integer" = valid_items)
    }
  }
  nstrata <- length(t.obs) # Population is first 'stratum'
  use_stratification <- nstrata > 1
  mle <- ub <- lb <- precision <- p.val <- K <- numeric(nstrata)
  if (pooling != "partial" || !use_stratification) {
    for (i in 1:nstrata) {
      if (method == "poisson") {
        if (is_bayesian) {
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
        if (is_bayesian) {
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
        if (is_bayesian) {
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
          "stringer" = .stringer(taints[[i]], conf.level, n.obs[i]),
          "stringer.meikle" = .stringer(taints[[i]], conf.level, n.obs[i], correction = "meikle"),
          "stringer.lta" = .stringer(taints[[i]], conf.level, n.obs[i], correction = "lta"),
          "stringer.pvz" = .stringer(taints[[i]], conf.level, n.obs[i], correction = "pvz"),
          "rohrbach" = .rohrbach(taints[[i]], conf.level, n.obs[i], alternative, N.units[i], r.delta = 2.7),
          "moment" = .moment(taints[[i]], conf.level, n.obs[i], alternative, m.type = "accounts"),
          "coxsnell" = .coxsnell(taints[[i]], conf.level, n.obs[i], alternative, cs.a = 1, cs.b = 3, cs.mu = 0.5, 1 + prior.x, prior.n - prior.x),
          "mpu" = .mpu(taints[[i]], conf.level, alternative, n.obs[i]),
          "direct" = .direct(book_values[[i]], audit_values[[i]], conf.level, alternative, N.items[i], n.obs[i], N.units[i]),
          "difference" = .difference(book_values[[i]], audit_values[[i]], conf.level, alternative, N.items[i], n.obs[i]),
          "quotient" = .quotient(book_values[[i]], audit_values[[i]], conf.level, alternative, N.items[i], n.obs[i]),
          "regression" = .regression(book_values[[i]], audit_values[[i]], conf.level, alternative, N.items[i], n.obs[i], N.units[i]),
          "newmethod" = NULL
        )
        mle[i] <- out[["mle"]]
        ub[i] <- out[["ub"]]
        lb[i] <- out[["lb"]]
      }
      precision[i] <- if (alternative == "greater") mle[i] - lb[i] else ub[i] - mle[i]
    }
  } else {
    stopifnot("pooling = 'partial' only possible when 'prior != FALSE'" = is_bayesian)
    if (broken_taints && has_data) {
      stratum_samples <- .partial_pooling(method, prior.x, prior.n, n.obs, t.obs, taints, nstrata, stratum, likelihood = "beta")
    } else {
      if (broken_taints) {
        message("sum of taints in each stratum is rounded upwards")
        t.obs <- ceiling(t.obs)
      }
      stratum_samples <- .partial_pooling(method, prior.x, prior.n, n.obs, t.obs, t = NULL, nstrata, stratum, likelihood = "binomial")
    }
    for (i in 2:nstrata) {
      mle[i] <- .dist_mode(analytical = FALSE, samples = stratum_samples[, i - 1])
      lb[i] <- switch(alternative,
        "two.sided" = stats::quantile(stratum_samples[, i - 1], probs = (1 - conf.level) / 2),
        "less" = 0,
        "greater" = stats::quantile(stratum_samples[, i - 1], probs = 1 - conf.level)
      )
      ub[i] <- switch(alternative,
        "two.sided" = stats::quantile(stratum_samples[, i - 1], probs = conf.level + (1 - conf.level) / 2),
        "less" = stats::quantile(stratum_samples[, i - 1], probs = conf.level),
        "greater" = 1
      )
      precision[i] <- if (alternative == "greater") mle[i] - lb[i] else ub[i] - mle[i]
    }
  }
  # Main results object
  result <- list()
  result[["conf.level"]] <- conf.level
  if (!use_stratification || pooling == "complete" || !valid_bayesian_method) {
    if (use_stratification && !valid_bayesian_method) {
      message("population results are displayed for aggregated sample")
    }
    result[["mle"]] <- mle[1]
    result[["ub"]] <- ub[1]
    result[["lb"]] <- lb[1]
    result[["precision"]] <- precision[1]
  } else {
    if (pooling != "partial") {
      stratum_samples <- .sample_analytical(method, nstrata, is_bayesian, prior.x, t.obs, prior.n, n.obs, N.units, iterations = 100000)
    }
    prior_samples <- .poststratify_samples(stratum_samples[, nstrata:ncol(stratum_samples)], N.units)
    post_samples <- .poststratify_samples(stratum_samples[, 1:(nstrata - 1)], N.units)
    result[["mle"]] <- .compute_mode(post_samples)
    result[["lb"]] <- switch(alternative,
      "two.sided" = as.numeric(stats::quantile(post_samples, probs = (1 - conf.level) / 2)),
      "less" = 0,
      "greater" = as.numeric(stats::quantile(post_samples, probs = 1 - conf.level))
    )
    result[["ub"]] <- switch(alternative,
      "two.sided" = as.numeric(stats::quantile(post_samples, probs = conf.level + (1 - conf.level) / 2)),
      "less" = as.numeric(stats::quantile(post_samples, probs = conf.level)),
      "greater" = 1
    )
  }
  result[["precision"]] <- if (alternative == "greater") result[["mle"]] - result[["lb"]] else result[["ub"]] - result[["mle"]]
  if (!is_bayesian && materiality < 1 && method %in% c("binomial", "poisson", "hypergeometric")) {
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
      if (!is_bayesian && materiality < 1 && method %in% c("binomial", "poisson", "hypergeometric")) {
        result[["strata"]][["p.value"]] <- p.val[1]
      }
      if (is_bayesian && materiality != 1 && method %in% c("binomial", "poisson", "hypergeometric")) {
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
      if (!is_bayesian && materiality < 1 && method %in% c("binomial", "poisson", "hypergeometric")) {
        result[["strata"]][["p.value"]] <- p.val[-1]
      }
      if (is_bayesian && materiality != 1 && method %in% c("binomial", "poisson", "hypergeometric")) {
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
          result[["strata"]][["bf10"]] <- (apply(stratum_samples[, 1:(nstrata - 1)], 2, function(x) length(which(x < materiality)) / length(x)) / apply(stratum_samples[, 1:(nstrata - 1)], 2, function(x) length(which(x > materiality)) / length(x))) / (apply(stratum_samples[, nstrata:ncol(stratum_samples)], 2, function(x) length(which(x < materiality)) / length(x)) / apply(stratum_samples[, nstrata:ncol(stratum_samples)], 2, function(x) length(which(x > materiality)) / length(x)))
        } else if (alternative == "greater") {
          result[["strata"]][["bf10"]] <- (apply(stratum_samples[, 1:(nstrata - 1)], 2, function(x) length(which(x > materiality)) / length(x)) / apply(stratum_samples[, 1:(nstrata - 1)], 2, function(x) length(which(x < materiality)) / length(x))) / (apply(stratum_samples[, nstrata:ncol(stratum_samples)], 2, function(x) length(which(x > materiality)) / length(x)) / apply(stratum_samples[, nstrata:ncol(stratum_samples)], 2, function(x) length(which(x < materiality)) / length(x)))
        } else if (alternative == "two.sided") {
          for (i in 1:(nstrata - 1)) {
            result[["strata"]][["bf10"]][i] <- 1 / (stats::approx(stats::density(stratum_samples[, i], from = 0, to = 1)$x, stats::density(stratum_samples[, i], from = 0, to = 1)$y, materiality)$y / stats::approx(stats::density(stratum_samples[, (nstrata - 1) + i], from = 0, to = 1)$x, stats::density(stratum_samples[, (nstrata - 1) + i], from = 0, to = 1)$y, materiality)$y)
          }
        }
      }
    }
    rownames(result[["strata"]]) <- if (is.null(strata)) seq_len(nrow(result[["strata"]])) else levels(stratum)
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
  if (method == "hypergeometric" && !is_bayesian) {
    result[["K"]] <- K[1]
  }
  # Prior distribution object
  if (is_bayesian) {
    if (is_jfa_prior && !is.null(prior[["hypotheses"]])) {
      if (alternative == "greater") {
        prior[["hypotheses"]]$odds.h1 <- 1 / prior[["hypotheses"]]$odds.h1
      }
    } else {
      prior <- auditPrior("sample", method, result[["N.units"]],
        materiality = result[["materiality"]], x = prior.x, n = prior.n
      )
    }
    result[["prior"]] <- prior
  }
  # Posterior distribution object
  if (!is.null(result[["prior"]])) {
    analytical <- !use_stratification || pooling == "complete"
    # Posterior parameters
    post_alpha <- result[["prior"]][["description"]]$alpha + result[["t"]]
    if (method == "poisson") {
      post_beta <- result[["prior"]][["description"]]$beta + result[["n"]]
    } else {
      post_beta <- result[["prior"]][["description"]]$beta + result[["n"]] - result[["t"]]
    }
    post_N <- result[["N.units"]] - result[["n"]]
    # Main object
    result[["posterior"]] <- list()
    result[["posterior"]]$posterior <- .dist_string(method, post_alpha, post_beta, post_N, analytical)
    result[["posterior"]]$likelihood <- method
    # Description section
    description <- list()
    description[["density"]] <- .dist_form(method, analytical)
    description[["n"]] <- result[["n"]]
    description[["x"]] <- result[["t"]]
    if (analytical) {
      description[["alpha"]] <- post_alpha
      description[["beta"]] <- post_beta
      description[["implicit.x"]] <- description[["alpha"]] - 1
      if (method == "poisson") {
        description[["implicit.n"]] <- description[["beta"]]
      } else {
        description[["implicit.n"]] <- description[["beta"]] - result[["t"]]
      }
      prior_samples <- NULL
      post_samples <- NULL
    }
    result[["posterior"]][["description"]] <- description
    # Statistics section
    statistics <- list()
    statistics[["mode"]] <- .dist_mode(method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["mean"]] <- .dist_mean(method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["median"]] <- .dist_median(method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["var"]] <- .dist_var(method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["skewness"]] <- .dist_skew(method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["ub"]] <- .dist_ub(alternative, conf.level, method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["lb"]] <- .dist_lb(alternative, conf.level, method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["precision"]] <- .dist_precision(alternative, statistics[["mode"]], statistics[["lb"]], statistics[["ub"]])
    result[["posterior"]][["statistics"]] <- statistics
    # Hypotheses section
    if (result[["materiality"]] != 1) {
      hypotheses <- list()
      hypotheses[["hypotheses"]] <- .hyp_string(materiality, alternative)
      if (alternative == "two.sided") {
        hypotheses[["density"]] <- .hyp_dens(materiality, method, post_alpha, post_beta, N.units, post_N, analytical, post_samples)
        hypotheses[["bf.h0"]] <- .bf01_twosided(materiality, hypotheses[["density"]], result[["prior"]], analytical, prior_samples)
        hypotheses[["bf.h1"]] <- 1 / hypotheses[["bf.h0"]]
      } else {
        lower_tail <- alternative == "less"
        hypotheses[["p.h1"]] <- .hyp_prob(lower_tail, materiality, method, post_alpha, post_beta, result[["N.units"]], post_N, analytical, post_samples)
        hypotheses[["p.h0"]] <- .hyp_prob(!lower_tail, materiality, method, post_alpha, post_beta, result[["N.units"]], post_N, analytical, post_samples)
        hypotheses[["odds.h1"]] <- hypotheses[["p.h1"]] / hypotheses[["p.h0"]]
        hypotheses[["odds.h0"]] <- 1 / hypotheses[["odds.h1"]]
        hypotheses[["bf.h1"]] <- .bf10_onesided(materiality, alternative, hypotheses[["odds.h1"]], result[["prior"]], analytical, prior_samples)
        hypotheses[["bf.h0"]] <- 1 / hypotheses[["bf.h1"]]
      }
      result[["posterior"]][["hypotheses"]] <- hypotheses
    }
    result[["posterior"]]$N.units <- result[["N.units"]]
    class(result[["posterior"]]) <- c(class(result[["posterior"]]), "jfaPosterior")
  }
  if (has_data && !is.null(values) && !is.null(values.audit)) {
    values_column <- which(colnames(data) == values)
    values_audit_column <- which(colnames(data) == values.audit)
    sample_data <- as.data.frame(data[, c(values_column, values_audit_column)])
    sample_data <- cbind(as.numeric(rownames(sample_data)), sample_data)
    sample_data[["difference"]] <- sample_data[, 2] - sample_data[, 3]
    sample_data[["taint"]] <- sample_data[, 4] / sample_data[, 2]
    if (!is.null(times)) {
      sample_data[["times"]] <- times
    } else {
      sample_data[["times"]] <- 1
    }
    colnames(sample_data) <- c("row", values, values.audit, "difference", "taint", "times")
    if (use_stratification) {
      sample_data <- cbind(sample_data, "strata" = stratum)
    }
    result[["data"]] <- sample_data
    result[["data.name"]] <- dname
  }
  class(result) <- c(class(result), "jfaEvaluation")
  return(result)
}
