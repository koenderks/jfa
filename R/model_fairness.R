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

#' Algorithm Auditing: Fairness Metrics and Parity
#'
#' @description This function aims to assess fairness in algorithmic
#' decision-making systems by computing and testing the equality of one of
#' several model-agnostic fairness metrics between protected classes. The
#' metrics are computed based on a set of true labels and the predictions of an
#' algorithm. The ratio of these metrics between any unprivileged protected
#' class and the privileged protected class is called parity. This measure can
#' quantify potential fairness or discrimination in the algorithms predictions.
#' Available parity metrics include predictive rate parity, proportional parity,
#' accuracy parity, false negative rate parity, false positive rate parity, true
#' positive rate parity, negative predicted value parity, specificity parity,
#' and demographic parity. The function returns an object of class
#' \code{jfaFairness} that can be used with associated \code{summary()} and
#' \code{plot()} methods.
#'
#' @usage model_fairness(
#'   data,
#'   protected,
#'   target,
#'   predictions,
#'   privileged = NULL,
#'   positive = NULL,
#'   metric = c(
#'     "prp", "pp", "ap", "fnrp", "fprp",
#'     "tprp", "npvp", "sp", "dp"
#'   ),
#'   alternative = c("two.sided", "less", "greater"),
#'   conf.level = 0.95,
#'   prior = FALSE
#' )
#'
#' @param data         a data frame containing the input data.
#' @param protected    a character specifying the column name in \code{data}
#'   containing the protected classes (i.e., the sensitive attribute).
#' @param target       a character specifying the column name in \code{data}
#'   containing the true labels of the target (i.e., to be predicted) variable.
#' @param predictions  a character specifying the column name in \code{data}
#'   containing the predicted labels of the target variable.
#' @param privileged  a character specifying the factor level of the column
#'   \code{protected} to be used as the privileged group. If \code{NULL} (the
#'   default), the first factor level of the \code{protected} column is used.
#' @param positive     a character specifying the factor level positive class of
#'   the column \code{target} to be used as the positive class. If \code{NULL}
#'   (the default), the first factor level of the \code{target} column is used.
#' @param metric      a character indicating the fairness metrics to compute.
#'   See the Details section below for more information.
#' @param alternative   a character indicating the alternative hypothesis and
#'   the type of confidence / credible interval used in the individual
#'   comparisons to the privileged group. Possible options are  \code{two.sided}
#'   (default), \code{less}, or \code{greater}. The alternative hypothesis
#'   relating to the overall equality is always two sided.
#' @param conf.level   a numeric value between 0 and 1 specifying the
#'   confidence level (i.e., 1 - audit risk / detection risk).
#' @param prior        a logical specifying whether to use a prior distribution,
#'   or a numeric value equal to or larger than 1 specifying the prior
#'   concentration parameter. If this argument is specified as \code{FALSE}
#'   (default), classical estimation is performed and if it is \code{TRUE},
#'   Bayesian estimation using a default prior with concentration parameter 1 is
#'   performed.
#'
#' @details The following model-agnostic fairness metrics are computed based on
#'   the confusion matrix for each protected class, using the true positives
#'   (TP), false positives (FP), true negative (TN) and false negatives (FN).
#'   See Pessach & Shmueli (2022) for a more detailed explanation of the
#'   individual metrics. The equality of metrics across groups is done according
#'   to the methodology described in Fisher (1970) and Jamil et al. (2017).
#'
#'   \itemize{
#'     \item{Predictive rate parity (\code{prp}): calculated as TP / (TP +
#'       FP), its ratio quantifies whether the predictive rate is equal across
#'       protected classes.}
#'     \item{Proportional parity (\code{pp}): calculated as (TP + FP) / (TP +
#'       FP + TN + FN), its ratio quantifies whether the positive prediction
#'       rate is equal across protected classes.}
#'     \item{Accuracy parity (\code{ap}): calculated as (TP + TN) / (TP + FP +
#'       TN + FN), quantifies whether the accuracy is the same across groups.}
#'     \item{False negative rate parity (\code{fnrp}): calculated as FN / (FP
#'       + FN), quantifies whether the false negative rate is the same across
#'       groups.}
#'     \item{False positive rate parity (\code{fprp}): calculated as FP / (TN
#'       + FP), quantifies whether the false positive rate is the same across
#'       groups.}
#'     \item{True positive rate parity (\code{tprp}): calculated as TP / (TP +
#'       FN), quantifies whether the true positive rate is the same across
#'       groups.}
#'     \item{Negative predicted value parity (\code{npvp}): calculated as TN /
#'       (TN + FN), quantifies whether the negative predicted value is equal
#'       across groups.}
#'     \item{Specificity parity (\code{sp}): calculated as TN / (TN + FP),
#'       quantifies whether the true positive rate is the same across groups.}
#'     \item{Demographic parity (\code{dp}): calculated as TP + FP, quantifies
#'       whether the positive predictions are equal across groups.}
#'   }
#'
#'   Note that, in an audit context, not all fairness measures are equally
#'   appropriate in all situations. The fairness tree below aids in choosing
#'   which fairness measure is appropriate for the situation at hand (Büyük,
#'   2023).
#'
#'   \if{html}{\figure{fairness-tree.png}{options: width="100\%" alt="fairness-tree"}}
#'   \if{latex}{\figure{fairness-tree.pdf}{options: width=5in}}
#'
#' @return An object of class \code{jfaFairness} containing:
#'
#' \item{data}{the specified data.}
#' \item{conf.level}{a numeric value between 0 and 1 giving the confidence
#'   level.}
#' \item{privileged}{The privileged group for computing the fairness metrics.}
#' \item{unprivileged}{The unprivileged group(s).}
#' \item{target}{The target variable used in computing the fairness metrics.}
#' \item{predictions}{The predictions used to compute the fairness metrics.}
#' \item{protected}{The variable indicating the protected classes.}
#' \item{positive}{The positive class used in computing the fairness metrics.}
#' \item{negative}{The negative class used in computing the fairness metrics.}
#' \item{alternative}{The type of confidence interval.}
#' \item{confusion.matrix}{A list of confusion matrices for each group.}
#' \item{performance}{A data frame containing performance metrics for each
#'   group, including accuracy, precision, recall, and F1 score.}
#' \item{metric}{A data frame containing, for each group, the estimates of the
#'   fairness metric along with the associated confidence / credible interval.}
#' \item{parity}{A data frame containing, for each unprivileged group, the
#'   parity and associated confidence / credible interval when compared to the
#'   privileged group.}
#' \item{odds.ratio}{A data frame containing, for each unprivileged group, the
#'   odds ratio of the fairness metric and its associated confidence/credible
#'   interval, along with inferential measures such as uncorrected p-values or
#'   Bayes factors.}
#' \item{measure}{The abbreviation of the selected fairness metric.}
#' \item{prior}{a logical indicating whether a prior distribution was used.}
#' \item{data.name}{The name of the input data object.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @references Büyük, S. (2023). \emph{Automatic Fairness Criteria and Fair
#'   Model Selection for Critical ML Tasks}, Master Thesis, Utrecht University.
#' @references Calders, T., & Verwer, S. (2010). Three naive Bayes approaches
#'   for discrimination-free classification. In \emph{Data Mining and Knowledge
#'   Discovery}. Springer Science and Business Media LLC.
#'   \doi{10.1007/s10618-010-0190-x}
#' @references Chouldechova, A. (2017). Fair prediction with disparate impact:
#'   A study of bias in recidivism prediction instruments. In \emph{Big Data}.
#'   Mary Ann Liebert Inc. \doi{10.1089/big.2016.0047}
#' @references Feldman, M., Friedler, S. A., Moeller, J., Scheidegger, C., &
#'   Venkatasubramanian, S. (2015). Certifying and removing disparate impact. In
#'   \emph{Proceedings of the 21th ACM SIGKDD International Conference on
#'   Knowledge Discovery and Data Mining}. \doi{10.1145/2783258.2783311}
#' @references Friedler, S. A., Scheidegger, C., Venkatasubramanian, S.,
#'   Choudhary, S., Hamilton, E. P., & Roth, D. (2019). A comparative study of
#'   fairness-enhancing interventions in machine learning. In \emph{Proceedings
#'   of the Conference on Fairness, Accountability, and Transparency}.
#'   \doi{10.1145/3287560.3287589}
#' @references Fisher, R. A. (1970). \emph{Statistical Methods for Research
#'   Workers}. Oliver & Boyd.
#' @references Jamil, T., Ly, A., Morey, R. D., Love, J., Marsman, M., &
#'   Wagenmakers, E. J. (2017). Default "Gunel and Dickey" Bayes factors for
#'   contingency tables. \emph{Behavior Research Methods}, 49, 638-652.
#'   \doi{10.3758/s13428-016-0739-8}
#' @references Pessach, D. & Shmueli, E. (2022). A review on fairness in machine
#'   learning. \emph{ACM Computing Surveys}, 55(3), 1-44. \doi{10.1145/3494672}
#' @references Zafar, M. B., Valera, I., Gomez Rodriguez, M., & Gummadi, K. P.
#'   (2017). Fairness beyond disparate treatment & disparate impact. In
#'   \emph{Proceedings of the 26th International Conference on World Wide Web}.
#'   \doi{10.1145/3038912.3052660}
#'
#' @keywords algorithm audit bias fairness model performance
#'
#' @examples
#' # Frequentist test of specificy parity
#' model_fairness(
#'   data = compas,
#'   protected = "Gender",
#'   target = "TwoYrRecidivism",
#'   predictions = "Predicted",
#'   privileged = "Male",
#'   positive = "yes",
#'   metric = "sp"
#' )
#' @export

model_fairness <- function(data,
                           protected,
                           target,
                           predictions,
                           privileged = NULL,
                           positive = NULL,
                           metric = c(
                             "prp", "pp", "ap", "fnrp", "fprp",
                             "tprp", "npvp", "sp", "dp"
                           ),
                           alternative = c("two.sided", "less", "greater"),
                           conf.level = 0.95,
                           prior = FALSE) {
  if (inherits(metric, "jfaFairnessSelection")) {
    metric <- metric[["measure"]]
  } else {
    metric <- match.arg(metric)
  }
  alternative <- match.arg(alternative)
  dname <- deparse(substitute(data))
  data <- as.data.frame(data, row.names = seq_len(nrow(data)))
  valid_prior <- (is.logical(prior)) || (is.numeric(prior) && prior >= 1)
  is_bayesian <- (is.logical(prior) && isTRUE(prior)) || (is.numeric(prior) && prior >= 1)
  stopifnot("'prior' must be TRUE or FALSE, or a numeric value >= 1 representing the prior concentration parameter" = valid_prior)
  stopifnot("'protected' does not exist in 'data'" = protected %in% colnames(data))
  stopifnot("'protected' must be a factor column" = is.factor(data[, protected]))
  stopifnot("'target' does not exist in 'data'" = target %in% colnames(data))
  stopifnot("'target' must be a factor column" = is.factor(data[, target]))
  stopifnot("'predictions' does not exist in 'data'" = predictions %in% colnames(data))
  stopifnot("'predictions' must be a factor column" = is.factor(data[, predictions]))
  groups <- levels(data[, protected])
  targetLevels <- levels(data[, target])
  stopifnot("'target' must contain at least 2 factor levels" = length(targetLevels) > 1)
  stopifnot("'predictions' must contain at least 1 factor level" = nlevels(data[, predictions]) > 0)
  stopifnot("'predictions' contain a factor level not in 'target'" = all(levels(data[, predictions]) %in% targetLevels))
  if (is.null(privileged)) {
    privileged <- groups[1]
  }
  stopifnot("'privileged' is not a class in 'protected'" = privileged %in% groups)
  if (is.null(positive)) {
    positive <- targetLevels[1]
  }
  stopifnot("'positive' is not a class in 'target'" = positive %in% targetLevels)
  confmat <- list()
  samples_list <- list()
  unprivileged <- groups[-which(groups == privileged)]
  negative <- targetLevels[-which(targetLevels == positive)]
  performance <- list(all = as.data.frame(matrix(NA, nrow = length(groups), ncol = 5), row.names = groups))
  colnames(performance[["all"]]) <- c("support", "accuracy", "precision", "recall", "f1.score")
  metrics <- list(all = as.data.frame(matrix(NA, nrow = length(groups), ncol = if (metric == "dp") 1 else 3), row.names = groups))
  parity <- list(all = as.data.frame(matrix(NA, nrow = length(groups), ncol = if (metric == "dp") 1 else 3), row.names = groups))
  colnames(metrics[["all"]]) <- colnames(parity[["all"]]) <- if (metric != "dp") c("estimate", "lb", "ub") else "estimate"
  odds.ratio <- list(all = as.data.frame(matrix(NA, nrow = length(unprivileged), ncol = 4), row.names = unprivileged))
  colnames(odds.ratio[["all"]]) <- if (is_bayesian) c("estimate", "lb", "ub", "bf10") else c("estimate", "lb", "ub", "p.value")
  for (i in seq_len(nlevels(data[, protected]))) {
    group <- levels(data[, protected])[i]
    groupDat <- data[data[, protected] == group, ]
    # Confusion matrices for each group
    confmat[[group]][["matrix"]] <- table("Actual" = groupDat[, target], "Predicted" = groupDat[, predictions])
    confmat[[group]][["tp"]] <- tp <- confmat[[group]][["matrix"]][positive, positive]
    confmat[[group]][["fp"]] <- fp <- sum(confmat[[group]][["matrix"]][negative, positive])
    confmat[[group]][["tn"]] <- tn <- sum(confmat[[group]][["matrix"]][negative, negative])
    confmat[[group]][["fn"]] <- fn <- sum(confmat[[group]][["matrix"]][positive, negative])
    confmat[[group]][["n"]] <- sum(confmat[[group]][["matrix"]])
    # Performance measures for each group
    performance[[group]][["support"]] <- performance[["all"]][i, 1] <- sum(confmat[[group]][["matrix"]])
    performance[[group]][["accuracy"]] <- performance[["all"]][i, 2] <- (confmat[[group]][["tp"]] + confmat[[group]][["tn"]]) / (confmat[[group]][["tp"]] + confmat[[group]][["tn"]] + confmat[[group]][["fp"]] + confmat[[group]][["fn"]])
    performance[[group]][["precision"]] <- performance[["all"]][i, 3] <- confmat[[group]][["tp"]] / (confmat[[group]][["tp"]] + confmat[[group]][["fp"]])
    performance[[group]][["recall"]] <- performance[["all"]][i, 4] <- confmat[[group]][["tp"]] / (confmat[[group]][["tp"]] + confmat[[group]][["fn"]])
    performance[[group]][["f1.score"]] <- performance[["all"]][i, 5] <- 2 * ((performance[[group]][["precision"]] * performance[[group]][["recall"]]) / (performance[[group]][["precision"]] + performance[[group]][["recall"]]))
    if (metric == "dp") {
      metrics[[group]][["estimate"]] <- metrics[["all"]][i, 1] <- tp + fp
    } else {
      metrics[[group]][["numerator"]] <- switch(metric,
        "pp" = tp + fp,
        "prp" = tp,
        "ap" = tp + tn,
        "fnrp" = fn,
        "fprp" = fp,
        "tprp" = tp,
        "npvp" = tn,
        "sp" = tn
      )
      metrics[[group]][["denominator"]] <- switch(metric,
        "pp" = tp + fp + tn + fn,
        "prp" = tp + fp,
        "ap" = tp + fp + tn + fn,
        "fnrp" = tp + fn,
        "fprp" = tn + fp,
        "tprp" = tp + fn,
        "npvp" = tn + fn,
        "sp" = tn + fp
      )
    }
  }
  names(confmat) <- groups
  # Sample estimates for each group
  if (metric != "dp") {
    for (i in seq_len(nlevels(data[, protected]))) {
      group <- levels(data[, protected])[i]
      if (!is_bayesian) {
        binom_test <- stats::binom.test(x = metrics[[group]][["numerator"]], n = metrics[[group]][["denominator"]], conf.level = conf.level, alternative = alternative)
        metrics[[group]][["estimate"]] <- metrics[["all"]][i, 1] <- as.numeric(binom_test$estimate)
        metrics[[group]][["lb"]] <- metrics[["all"]][i, 2] <- as.numeric(binom_test$conf.int[1])
        metrics[[group]][["ub"]] <- metrics[["all"]][i, 3] <- as.numeric(binom_test$conf.int[2])
      } else {
        contingencyTable <- matrix(c(
          metrics[[group]][["numerator"]],
          metrics[[group]][["denominator"]] - metrics[[group]][["numerator"]],
          metrics[[privileged]][["numerator"]],
          metrics[[privileged]][["denominator"]] - metrics[[privileged]][["numerator"]]
        ), ncol = 2)
        samples_list[[group]] <- .mcmc_or(counts = c(contingencyTable), prior_a = prior)
        metrics[[group]][["estimate"]] <- metrics[["all"]][i, 1] <- .comp_mode_bayes(analytical = FALSE, samples = samples_list[[group]]$prob)
        metrics[[group]][["lb"]] <- metrics[["all"]][i, 2] <- .comp_lb_bayes(alternative, conf.level, analytical = FALSE, samples = samples_list[[group]]$prob)
        metrics[[group]][["ub"]] <- metrics[["all"]][i, 3] <- .comp_ub_bayes(alternative, conf.level, analytical = FALSE, samples = samples_list[[group]]$prob)
      }
    }
  }
  # Parity for each group
  rowIndex <- 1
  for (group in groups) {
    if (group == privileged) {
      parity[[group]][["estimate"]] <- parity[["all"]][rowIndex, 1] <- 1
      if (metric != "dp") {
        parity[[group]][["lb"]] <- parity[[group]][["ub"]] <- parity[["all"]][rowIndex, 2] <- parity[["all"]][rowIndex, 3] <- 1
      }
    } else {
      parity[[group]][["estimate"]] <- parity[["all"]][rowIndex, 1] <- metrics[[group]][["estimate"]] / metrics[[privileged]][["estimate"]]
      if (metric != "dp") {
        parity[[group]][["lb"]] <- parity[["all"]][rowIndex, 2] <- metrics[[group]][["lb"]] / metrics[[privileged]][["estimate"]]
        parity[[group]][["ub"]] <- parity[["all"]][rowIndex, 3] <- metrics[[group]][["ub"]] / metrics[[privileged]][["estimate"]]
      }
    }
    rowIndex <- rowIndex + 1
  }
  # Odds ratio for each protected class
  if (metric != "dp") {
    rowIndex <- 1
    for (group in unprivileged) {
      contingencyTable <- matrix(c(
        metrics[[group]][["numerator"]],
        metrics[[group]][["denominator"]] - metrics[[group]][["numerator"]],
        metrics[[privileged]][["numerator"]],
        metrics[[privileged]][["denominator"]] - metrics[[privileged]][["numerator"]]
      ), ncol = 2)
      if (!is_bayesian) {
        fisher_test <- stats::fisher.test(contingencyTable, alternative = alternative, conf.level = conf.level)
        odds.ratio[[group]][["estimate"]] <- odds.ratio[["all"]][rowIndex, 1] <- as.numeric(fisher_test$estimate)
        odds.ratio[[group]][["lb"]] <- odds.ratio[["all"]][rowIndex, 2] <- as.numeric(fisher_test$conf.int[1])
        odds.ratio[[group]][["ub"]] <- odds.ratio[["all"]][rowIndex, 3] <- as.numeric(fisher_test$conf.int[2])
        odds.ratio[[group]][["p.value"]] <- odds.ratio[["all"]][rowIndex, 4] <- as.numeric(fisher_test$p.value)
      } else {
        odds.ratio[[group]][["estimate"]] <- odds.ratio[["all"]][rowIndex, 1] <- .comp_mode_bayes(analytical = FALSE, samples = samples_list[[group]]$OR)
        odds.ratio[[group]][["lb"]] <- odds.ratio[["all"]][rowIndex, 2] <- .comp_lb_bayes(alternative, conf.level, analytical = FALSE, samples = samples_list[[group]]$OR)
        odds.ratio[[group]][["ub"]] <- odds.ratio[["all"]][rowIndex, 3] <- .comp_ub_bayes(alternative, conf.level, analytical = FALSE, samples = samples_list[[group]]$OR)
        odds.ratio[[group]][["bf10"]] <- odds.ratio[["all"]][rowIndex, 4] <- switch(alternative,
          "two.sided" = .contingencyTableBf(contingencyTable, prior, "none"),
          "less" = (length(which(samples_list[[group]]$OR < 1)) / length(samples_list[[group]]$OR)) / (length(which(samples_list[[group]]$OR > 1)) / length(samples_list[[group]]$OR)),
          "greater" = (length(which(samples_list[[group]]$OR > 1)) / length(samples_list[[group]]$OR)) / (length(which(samples_list[[group]]$OR < 1)) / length(samples_list[[group]]$OR))
        )
        density_post <- stats::density(log(samples_list[[group]]$OR), n = 1000)
        density_post_alt <- stats::density(log(samples_list[[group]]$OR), n = 10000, from = -10, to = 10)
        density_prior_alt <- stats::density(log(samples_list[[group]]$prior), n = 10000, from = -10, to = 10)
        odds.ratio[[group]][["density"]] <- list(x = density_post_alt$x, y = density_post_alt$y, xmin = min(c(density_post$x, -2)), xmax = max(c(density_post$x, 2)), prior_x = density_prior_alt$x, prior_y = density_prior_alt$y)
      }
      rowIndex <- rowIndex + 1
    }
  }
  # Test for overall effect
  n <- nrow(data)
  names(n) <- "n"
  if (metric != "dp") {
    nums <- unlist(lapply(metrics, function(group) group[["numerator"]]))
    denoms <- unlist(lapply(metrics, function(group) group[["denominator"]]))
    crossTab <- matrix(c(nums, denoms - nums), nrow = 2, byrow = TRUE) # Contingency table used for bf robustness check
    colnames(crossTab) <- groups
    if (!is_bayesian) {
      suppressWarnings({ # Temporary until better solution
        test <- stats::chisq.test(crossTab)
      })
    } else {
      bf <- .contingencyTableBf(crossTab, prior, "none")
      names(bf) <- "BF10"
    }
  }
  result <- list()
  result[["data"]] <- data
  result[["conf.level"]] <- conf.level
  result[["privileged"]] <- privileged
  result[["unprivileged"]] <- unprivileged
  result[["target"]] <- target
  result[["predictions"]] <- predictions
  result[["protected"]] <- protected
  result[["positive"]] <- positive
  result[["negative"]] <- negative
  result[["alternative"]] <- alternative
  result[["measure"]] <- metric
  result[["n"]] <- n
  if (metric != "dp") {
    result[["crossTab"]] <- crossTab
    if (!is_bayesian) {
      result[["statistic"]] <- test[["statistic"]]
      result[["parameter"]] <- test[["parameter"]]
      result[["p.value"]] <- test[["p.value"]]
    } else {
      result[["bf"]] <- bf
    }
  }
  result[["confusion.matrix"]] <- confmat
  result[["performance"]] <- performance
  result[["metric"]] <- metrics
  result[["parity"]] <- parity
  if (result[["measure"]] != "dp") {
    result[["odds.ratio"]] <- odds.ratio
  }
  result[["prior"]] <- prior
  result[["data.name"]] <- dname
  class(result) <- c("jfaFairness", "list")
  return(result)
}
