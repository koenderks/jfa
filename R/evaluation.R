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
#' @usage evaluation(materiality = NULL,
#'            method = c(
#'              "poisson", "binomial", "hypergeometric", "stringer",
#'              "stringer.meikle", "stringer.lta", "stringer.pvz", "rohrbach",
#'              "moment", "coxsnell", "direct", "difference", "quotient",
#'              "regression", "mpu"
#'            ),
#'            alternative = c("less", "two.sided", "greater"),
#'            conf.level = 0.95,
#'            data = NULL,
#'            values = NULL,
#'            values.audit = NULL,
#'            strata = NULL,
#'            times = NULL,
#'            x = NULL,
#'            n = NULL,
#'            N.units = NULL,
#'            N.items = NULL,
#'            pooling = c("none", "complete", "partial"),
#'            prior = FALSE)
#'
#' @param materiality   a numeric value between 0 and 1 specifying the
#'   performance materiality (i.e., the maximum tolerable misstatement in the
#'   population) as a fraction of the total number of units in the population.
#'   Can be \code{NULL}. Not used for methods \code{direct}, \code{difference},
#'   \code{quotient}, and \code{regression}.
#' @param method        a character specifying the statistical method. Possible
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
#'   information is shared between strata). The latter two options fit a
#'   Bayesian model to the data using a MCMC sampling procedure whose options
#'   can be set globally using \code{options("mc.iterations")} (otherwise:
#'   2000), \code{options("mc.warmup")} (otherwise: 1000),
#'   \code{options("mc.chains")} (otherwise: 4) and \code{options("mc.cores")}
#'   (otherwise: 1).
#' @param prior         a logical specifying whether to use a prior
#'   distribution, or an object of class \code{jfaPrior} or \code{jfaPosterior}.
#'   If this argument is specified as \code{FALSE} (default), the function
#'   performs classical evaluation. If this argument is specified as \code{TRUE}
#'   or as a prior from \code{auditPrior}, this function performs Bayesian
#'   evaluation using the specified prior.
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
                       method = c(
                         "poisson", "binomial", "hypergeometric", "stringer",
                         "stringer.meikle", "stringer.lta", "stringer.pvz",
                         "rohrbach", "moment", "coxsnell", "direct", "difference",
                         "quotient", "regression", "mpu"
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
  # Input checking
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  pooling <- match.arg(pooling)
  is_jfa_prior <- inherits(prior, "jfaPrior") || inherits(prior, "jfaPosterior")
  is_bayesian <- (inherits(prior, "logical") && prior) || is_jfa_prior
  if (is_jfa_prior) {
    stopifnot("method = 'mcmc' not supported" = prior[["likelihood"]] != "mcmc")
    conjugate_prior <- method == prior[["likelihood"]]
    possible_match <- method %in% c("poisson", "binomial", "hypergeometric") && prior[["likelihood"]] %in% c("poisson", "binomial", "hypergeometric")
    if (!conjugate_prior && possible_match) {
      method <- prior[["likelihood"]]
      conjugate_prior <- TRUE
    }
    if (!is.null(prior[["N.units"]])) {
      message(paste0("Using 'N.units = ", prior[["N.units"]], "' from 'prior'"))
      N.units <- prior[["N.units"]]
    }
    if (!is.null(materiality) && is.null(prior[["hypotheses"]])) {
      hypotheses <- list()
      hypotheses[["hypotheses"]] <- .hyp_string(materiality, "less")
      hypotheses[["materiality"]] <- materiality
      hypotheses[["alternative"]] <- "less"
      hypotheses[["p.h1"]] <- .hyp_prob(TRUE, materiality, prior[["likelihood"]], prior[["description"]]$alpha, prior[["description"]]$beta, N.units, N.units)
      hypotheses[["p.h0"]] <- .hyp_prob(FALSE, materiality, prior[["likelihood"]], prior[["description"]]$alpha, prior[["description"]]$beta, N.units, N.units)
      hypotheses[["odds.h1"]] <- hypotheses[["p.h1"]] / hypotheses[["p.h0"]]
      hypotheses[["odds.h0"]] <- 1 / hypotheses[["odds.h1"]]
      hypotheses[["density"]] <- .hyp_dens(materiality, prior[["likelihood"]], prior[["description"]]$alpha, prior[["description"]]$beta, N.units, N.units)
      prior[["hypotheses"]] <- hypotheses
    }
  } else if (prior) {
    accommodates_simple_prior <- method %in% c("poisson", "binomial", "hypergeometric")
    stopifnot("'method' should be one of 'poisson', 'binomial', or 'hypergeometric'" = accommodates_simple_prior)
    prior <- auditPrior("default", method, N.units, materiality = materiality, conf.level = conf.level)
    conjugate_prior <- TRUE
  }
  stopifnot("missing value for 'conf.level'" = !is.null(conf.level))
  valid_confidence <- is.numeric(conf.level) && length(conf.level) == 1 && conf.level > 0 && conf.level < 1
  stopifnot("'conf.level' must be a single value between 0 and 1" = valid_confidence)
  if (!is.null(materiality)) {
    valid_materiality <- is.numeric(materiality) && materiality > 0 && materiality < 1
    stopifnot("'materiality' must be a single value between 0 and 1" = valid_materiality)
  }
  valid_test_method <- method %in% c("poisson", "binomial", "hypergeometric", "normal", "uniform", "cauchy", "t", "chisq")
  if (is_bayesian) {
    stopifnot("'method' should be one of 'poisson', 'binomial', or 'hypergeometric'" = valid_test_method)
  }
  is_stringer_method <- method %in% c("stringer", "stringer.meikle", "stringer.lta", "stringer.pvz")
  if (alternative %in% c("two.sided", "greater") && is_stringer_method) {
    stop(paste0("'method = ", method, "' does not accomodate 'alternative = ", alternative, "'"))
  }
  stopifnot("missing value(s) for 'data' or 'x' and 'n'" = !is.null(x) || !is.null(n) || !is.null(data))
  has_summary_statistics <- !is.null(x) || !is.null(n)
  has_data <- !is.null(data)
  if (has_summary_statistics) {
    valid_method <- !(method %in% c(
      "stringer", "stringer.meikle", "stringer.lta", "stringer.pvz",
      "coxsnell", "rohrbach", "moment", "mpu",
      "direct", "difference", "quotient", "regression"
    ))
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
      stopifnot("column 'strata' not found in 'data'" = strata %in% colnames(data))
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
  if (length(n.obs) > 1) {
    n.obs <- c(sum(n.obs), n.obs)
    x.obs <- c(sum(ceiling(x.obs)), ceiling(x.obs))
    t.obs <- c(sum(t.obs), t.obs)
    if (!is.null(N.units)) {
      N.units <- c(sum(N.units), N.units)
      valid_units <- length(N.units) == length(t.obs)
      stopifnot("length of 'N.units' must be equal to number of strata" = valid_units)
    }
    if (!is.null(N.items)) {
      N.items <- c(sum(N.items), N.items)
      valid_items <- length(N.items) == length(t.obs)
      stopifnot("length of 'N.items' must be equal to number of strata" = valid_items)
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
  valid_units_n <- all(N.units >= n.obs)
  stopifnot("all 'N.units' must be >= the number of samples" = valid_units_n)
  if (method == "hypergeometric") {
    stopifnot("missing value for 'N.units'" = !is.null(N.units))
    valid_units <- is.numeric(N.units) && length(N.units) > 0 && all(N.units > 0)
    stopifnot("all values in 'N.units' must be > 0" = valid_units)
    N.units <- ceiling(N.units)
  }
  use_stratification <- length(t.obs) > 1
  if (pooling == "complete") {
    nstrata <- 1
  } else {
    nstrata <- length(t.obs) # Population is first 'stratum'
  }
  no_rows <- length(t.obs) - 1
  if (is_bayesian) {
    mcmc_prior <- nstrata > 1
    mcmc_posterior <- nstrata > 1 || !conjugate_prior
    if (conjugate_prior) {
      stratum_samples <- NULL
    } else {
      stratum_samples <- matrix(NA, nrow = (getOption("mc.iterations", 2000) - getOption("mc.warmup", 1000)) * getOption("mc.chains", 4), ncol = no_rows * 2)
    }
  }
  mle <- lb <- ub <- precision <- p.val <- K <- numeric(nstrata)
  # Compute results
  if (!use_stratification || pooling != "partial") {
    for (i in 1:nstrata) {
      if (valid_test_method) {
        if (is_bayesian) {
          if (conjugate_prior) {
            stratum_alpha <- prior[["description"]]$alpha + t.obs[i]
            if (method == "poisson") {
              stratum_beta <- prior[["description"]]$beta + n.obs[i]
            } else {
              stratum_beta <- prior[["description"]]$beta + n.obs[i] - t.obs[i]
            }
            stratum_N <- N.units[i] - n.obs[i]
            mle[i] <- .comp_mode_bayes(method, stratum_alpha, stratum_beta, stratum_N)
            lb[i] <- .comp_lb_bayes(alternative, conf.level, method, stratum_alpha, stratum_beta, stratum_N)
            ub[i] <- .comp_ub_bayes(alternative, conf.level, method, stratum_alpha, stratum_beta, stratum_N)
          } else {
            stopifnot("likelihood = 'hypergeometric' does not support non-conjugate priors" = method != "hypergeometric")
            samples <- .mcmc_cp(method, t.obs[i], n.obs[i], prior)
            prior_samples <- samples[, 2]
            post_samples <- samples[, 1]
            mle[i] <- .comp_mode_bayes(analytical = FALSE, samples = post_samples)
            lb[i] <- .comp_lb_bayes(alternative, conf.level, analytical = FALSE, samples = post_samples)
            ub[i] <- .comp_ub_bayes(alternative, conf.level, analytical = FALSE, samples = post_samples)
            if (i != 1) {
              stratum_samples[, i - 1] <- post_samples
              stratum_samples[, no_rows + i - 1] <- prior_samples
            }
          }
        } else {
          if (method == "hypergeometric") {
            K[i] <- ceiling(materiality * N.units[i])
          }
          mle[i] <- .comp_mle_freq(method, n.obs[i], x.obs[i], t.obs[i], N.units[i])
          lb[i] <- .comp_lb_freq(alternative, conf.level, method, n.obs[i], x.obs[i], t.obs[i], N.units[i])
          ub[i] <- .comp_ub_freq(alternative, conf.level, method, n.obs[i], x.obs[i], t.obs[i], N.units[i])
          if (materiality < 1) {
            p.val[i] <- .comp_pval(alternative, materiality, method, n.obs[i], x.obs[i], t.obs[i], N.units[i], K[i])
          }
        }
        if (method == "hypergeometric") {
          if (is_bayesian) {
            mle[i] <- mle[i] / N.units[i]
          }
          lb[i] <- lb[i] / N.units[i]
          ub[i] <- ub[i] / N.units[i]
        }
      } else {
        out <- switch(method,
          "stringer" = .stringer(taints[[i]], conf.level, n.obs[i]),
          "stringer.meikle" = .stringer(taints[[i]], conf.level, n.obs[i], "meikle"),
          "stringer.lta" = .stringer(taints[[i]], conf.level, n.obs[i], "lta"),
          "stringer.pvz" = .stringer(taints[[i]], conf.level, n.obs[i], "pvz"),
          "rohrbach" = .rohrbach(taints[[i]], conf.level, n.obs[i], alternative, N.units[i], 2.7),
          "moment" = .moment(taints[[i]], conf.level, n.obs[i], alternative, "accounts"),
          "coxsnell" = .coxsnell(taints[[i]], conf.level, n.obs[i], alternative, 1, 3, 0.5, 1, 1),
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
      precision[i] <- .comp_precision(alternative, mle[i], lb[i], ub[i])
    }
    if (use_stratification && pooling == "none") {
      if (is_bayesian && conjugate_prior) {
        stratum_samples <- .mcmc_analytical(no_rows, t.obs[-1], n.obs[-1], N.units[-1], prior)
      } else if (!is_bayesian) {
        stratum_samples <- .mcmc_emulate(method, alternative, no_rows, t.obs[-1], n.obs[-1], N.units[-1])
      }
    }
  } else {
    stopifnot("pooling = 'partial' only possible when 'prior != FALSE'" = is_bayesian)
    if (broken_taints && has_data) {
      stratum_samples <- .mcmc_pp("beta", n.obs, t.obs, taints, nstrata, stratum, prior)
    } else {
      if (broken_taints) {
        message("sum of taints in each stratum is rounded upwards")
        t.obs <- ceiling(t.obs)
      }
      stratum_samples <- .mcmc_pp(method, n.obs, t.obs, t = NULL, nstrata, stratum, prior)
    }
    for (i in 2:nstrata) {
      if (is_bayesian) {
        mle[i] <- .comp_mode_bayes(analytical = FALSE, samples = stratum_samples[, i - 1])
      } else {
        mle[i] <- .comp_mle_freq(method, n.obs[i], x.obs[i], t.obs[i], N.units[i])
      }
      if (method == "hypergeometric") {
        mle[i] <- mle[i] / N.units[i]
      }
      lb[i] <- .comp_lb_bayes(alternative, conf.level, analytical = FALSE, samples = stratum_samples[, i - 1])
      ub[i] <- .comp_ub_bayes(alternative, conf.level, analytical = FALSE, samples = stratum_samples[, i - 1])
      precision[i] <- .comp_precision(alternative, mle[i], lb[i], ub[i])
    }
  }
  use_poststratification <- use_stratification && pooling != "complete" && valid_test_method
  if (use_poststratification) {
    prior_samples <- .poststratification(stratum_samples[, (no_rows + 1):ncol(stratum_samples)], N.units[-1])
    post_samples <- .poststratification(stratum_samples[, 1:no_rows], N.units[-1])
    if (is_bayesian) {
      mle[1] <- .comp_mode_bayes(analytical = FALSE, samples = post_samples)
      if (method == "hypergeometric") {
        mle[1] <- mle[1] / N.units[1]
      }
      lb[1] <- .comp_lb_bayes(alternative, conf.level, analytical = FALSE, samples = post_samples)
      ub[1] <- .comp_ub_bayes(alternative, conf.level, analytical = FALSE, samples = post_samples)
    } else {
      mle[1] <- .comp_mle_freq(method, n.obs[-1], x.obs[-1], t.obs[-1], N.units[-1])
      lb[1] <- .comp_lb_freq(alternative, conf.level, analytical = FALSE, samples = post_samples)
      ub[1] <- .comp_ub_freq(alternative, conf.level, analytical = FALSE, samples = post_samples)
    }
    precision[1] <- .comp_precision(alternative, mle[1], lb[1], ub[1])
  } else if (use_stratification && !valid_test_method) {
    message("population results are displayed for aggregated sample")
  }
  # Initialize main results
  result <- list()
  result[["conf.level"]] <- conf.level
  result[["mle"]] <- mle[1]
  result[["ub"]] <- ub[1]
  result[["lb"]] <- lb[1]
  result[["precision"]] <- precision[1]
  if (!is_bayesian && materiality < 1 && valid_test_method) {
    if (!use_stratification || pooling == "complete") {
      result[["p.value"]] <- p.val[1]
    } else {
      result[["p.value"]] <- NA
    }
  }
  result[["x"]] <- x.obs[1]
  result[["t"]] <- t.obs[1]
  result[["n"]] <- n.obs[1]
  result[["materiality"]] <- materiality
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
  # Prior and posterior distribution
  if (is_bayesian) {
    analytical <- !mcmc_prior && !mcmc_posterior
    # Prior distribution
    result[["prior"]] <- prior
    if (mcmc_prior) {
      result[["prior"]]$prior <- "Determined via MCMC sampling"
      result[["prior"]]$plotsamples <- stats::density(ifelse(is.infinite(prior_samples), 1, prior_samples), from = 0, to = 1, n = 1000)
      result[["prior"]]$method <- "mcmc"
      # Description
      description <- list()
      description[["density"]] <- "MCMC"
      result[["prior"]][["description"]] <- description
      # Statistics
      statistics <- list()
      statistics[["mode"]] <- .comp_mode_bayes(analytical = FALSE, samples = prior_samples)
      statistics[["mean"]] <- .comp_mean_bayes(analytical = FALSE, samples = prior_samples)
      statistics[["median"]] <- .comp_median_bayes(analytical = FALSE, samples = prior_samples)
      statistics[["var"]] <- .comp_var_bayes(analytical = FALSE, samples = prior_samples)
      statistics[["skewness"]] <- .comp_skew_bayes(analytical = FALSE, samples = prior_samples)
      statistics[["entropy"]] <- .comp_entropy_bayes(analytical = FALSE, samples = prior_samples)
      statistics[["ub"]] <- .comp_ub_bayes("less", conf.level, analytical = FALSE, samples = prior_samples)
      statistics[["precision"]] <- .comp_precision("less", statistics[["mode"]], NULL, statistics[["ub"]])
      result[["prior"]][["statistics"]] <- statistics
      if (materiality < 1) {
        hypotheses <- list()
        hypotheses[["hypotheses"]] <- .hyp_string(materiality, "less")
        hypotheses[["alternative"]] <- alternative
        if (alternative == "two.sided") {
          hypotheses[["density"]] <- .hyp_dens(materiality, analytical = FALSE, samples = prior_samples)
        } else {
          lower_tail <- alternative == "less"
          hypotheses[["p.h1"]] <- .hyp_prob(lower_tail, materiality, analytical = FALSE, samples = prior_samples)
          hypotheses[["p.h0"]] <- .hyp_prob(!lower_tail, materiality, analytical = FALSE, samples = prior_samples)
          hypotheses[["odds.h1"]] <- hypotheses[["p.h1"]] / hypotheses[["p.h0"]]
          hypotheses[["odds.h0"]] <- 1 / hypotheses[["odds.h1"]]
        }
        result[["prior"]][["hypotheses"]] <- hypotheses
      }
      result[["prior"]]$conf.level <- conf.level
    }
    # Posterior distribution{
    post_alpha <- result[["prior"]][["description"]]$alpha + result[["t"]]
    if (method == "poisson") {
      post_beta <- result[["prior"]][["description"]]$beta + result[["n"]]
    } else {
      post_beta <- result[["prior"]][["description"]]$beta + result[["n"]] - result[["t"]]
    }
    post_N <- result[["N.units"]] - result[["n"]]
    # Initialize posterior distribution
    result[["posterior"]] <- list()
    result[["posterior"]]$posterior <- .functional_form(method, post_alpha, post_beta, post_N, analytical)
    result[["posterior"]]$likelihood <- method
    if (mcmc_posterior) {
      result[["posterior"]]$plotsamples <- stats::density(ifelse(is.infinite(post_samples), 1, post_samples), from = 0, to = 1, n = 1000)
      result[["posterior"]]$method <- "mcmc"
    } else {
      result[["posterior"]]$method <- "sample"
    }
    # Description
    description <- list()
    description[["density"]] <- .functional_density(method, analytical)
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
    }
    result[["posterior"]][["description"]] <- description
    # Statistics
    statistics <- list()
    statistics[["mode"]] <- .comp_mode_bayes(method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["mean"]] <- .comp_mean_bayes(method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["median"]] <- .comp_median_bayes(method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["var"]] <- .comp_var_bayes(method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["skewness"]] <- .comp_skew_bayes(method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["entropy"]] <- .comp_entropy_bayes(method, post_alpha, post_beta, analytical, post_samples)
    statistics[["kl"]] <- .comp_kl_bayes(method, result[["prior"]][["description"]]$alpha, result[["prior"]][["description"]]$beta, post_alpha, post_beta, analytical, prior_samples, post_samples)
    statistics[["ub"]] <- .comp_ub_bayes(alternative, conf.level, method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["lb"]] <- .comp_lb_bayes(alternative, conf.level, method, post_alpha, post_beta, post_N, analytical, post_samples)
    statistics[["precision"]] <- .comp_precision(alternative, statistics[["mode"]], statistics[["lb"]], statistics[["ub"]])
    result[["posterior"]][["statistics"]] <- statistics
    # Hypotheses
    if (materiality < 1) {
      hypotheses <- list()
      hypotheses[["hypotheses"]] <- .hyp_string(materiality, alternative)
      hypotheses[["materiality"]] <- materiality
      hypotheses[["alternative"]] <- alternative
      if (alternative == "two.sided") {
        hypotheses[["density"]] <- .hyp_dens(materiality, method, post_alpha, post_beta, result[["N.units"]], post_N, analytical, post_samples)
        hypotheses[["bf.h0"]] <- hypotheses[["density"]] / result[["prior"]][["hypotheses"]]$density
        hypotheses[["bf.h1"]] <- 1 / hypotheses[["bf.h0"]]
      } else {
        lower_tail <- alternative == "less"
        hypotheses[["p.h1"]] <- .hyp_prob(lower_tail, materiality, method, post_alpha, post_beta, result[["N.units"]], post_N, analytical, post_samples)
        hypotheses[["p.h0"]] <- .hyp_prob(!lower_tail, materiality, method, post_alpha, post_beta, result[["N.units"]], post_N, analytical, post_samples)
        hypotheses[["odds.h1"]] <- hypotheses[["p.h1"]] / hypotheses[["p.h0"]]
        hypotheses[["odds.h0"]] <- 1 / hypotheses[["odds.h1"]]
        hypotheses[["bf.h1"]] <- hypotheses[["odds.h1"]] / result[["prior"]][["hypotheses"]]$odds.h1
        hypotheses[["bf.h0"]] <- 1 / hypotheses[["bf.h1"]]
      }
      result[["posterior"]][["hypotheses"]] <- hypotheses
    }
    result[["posterior"]]$N.units <- result[["N.units"]]
    result[["posterior"]]$conf.level <- conf.level
    class(result[["posterior"]]) <- c("jfaPosterior", "list")
  }
  # Stratum results
  if (use_stratification) {
    if (pooling == "complete") {
      mle <- rep(result[["mle"]], no_rows)
      lb <- rep(result[["lb"]], no_rows)
      ub <- rep(result[["ub"]], no_rows)
      precision <- rep(result[["precision"]], no_rows)
    } else {
      mle <- mle[-1]
      lb <- lb[-1]
      ub <- ub[-1]
      precision <- precision[-1]
    }
    stratum_table <- data.frame(
      n = n.obs[-1], x = x.obs[-1], t = t.obs[-1],
      mle = mle, lb = lb, ub = ub, precision = precision
    )
    if (!is.null(N.units)) {
      stratum_table <- cbind(N = N.units[-1], stratum_table)
    }
    if (materiality < 1 && valid_test_method) {
      if (!is_bayesian) {
        stratum_table[["p.value"]] <- switch(pooling,
          "complete" = result[["p.value"]],
          "none" = p.val[-1]
        )
      } else {
        if (pooling == "complete") {
          stratum_table[["bf10"]] <- result[["posterior"]][["hypotheses"]]$bf.h1
        } else {
          if (alternative == "two.sided") {
            if (conjugate_prior) {
              stratum_table[["bf10"]] - 1 / .bf01_twosided_sumstats(materiality, method, prior[["description"]]$alpha, prior[["description"]]$beta, n.obs[-1], t.obs[-1], N.units[-1])
            } else {
              stratum_table[["bf10"]] <- 1 / .bf01_twosided_samples(materiality, nstrata, stratum_samples)
            }
          } else {
            if (conjugate_prior) {
              stratum_table[["bf10"]] <- .bf10_onesided_sumstats(materiality, alternative, method, prior[["description"]]$alpha, prior[["description"]]$beta, n.obs[-1], t.obs[-1], N.units[-1])
            } else {
              stratum_table[["bf10"]] <- .bf10_onesided_samples(materiality, alternative, nstrata, stratum_samples)
            }
          }
        }
      }
    }
    rownames(stratum_table) <- if (is.null(strata)) seq_len(nrow(stratum_table)) else levels(stratum)
    result[["strata"]] <- stratum_table
  }
  # Data
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
  class(result) <- c("jfaEvaluation", "list")
  return(result)
}
