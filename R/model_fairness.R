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

#' Algorithm Auditing: Fairness Metrics and Bias Detection
#'
#' @description This function detects bias in algorithmic decision-making
#' systems by computing various model-agnostic fairness metrics. These measures
#' quantify fairness across different groups based on the observed and predicted
#' outcomes of the model. The calculated metrics are commonly used and include
#' demographic parity, proportional parity, predictive rate parity, accuracy
#' parity, false negative rate parity, false positive rate parity, true positive
#' rate parity, negative predicted value parity, and specificity parity. In
#' addition, the user can specify a materiality threshold to decide whether
#' the algorithm is fair to a certain degree and within a certain range.
#' Currently, this function only supports binary classification.
#'
#' @usage model_fairness(
#'   data,
#'   sensitive,
#'   target,
#'   predictions,
#'   reference = NULL,
#'   positive = NULL,
#'   materiality = 0.8,
#'   alternative = c("two.sided", "greater", "less"),
#'   conf.level = 0.95
#' )
#'
#' @param data         a data frame containing the input data.
#' @param sensitive    a character specifying the column name in \code{data}
#'   indicating the sensitive variable.
#' @param target       A character specifying the column name in \code{data}
#'   indicating the actual values of the target (to be predicted) variable.
#' @param predictions  a character specifying the column name in \code{data}
#'   indicating the predicted values of the target variable.
#' @param reference    a character specifying the class in the column
#'   \code{sensitive} used as the reference class for computing the fairness
#'   metrics. If \code{NULL} (the default), the first factor level of the
#'   \code{sensitive} column is used as the reference group.
#' @param positive     a character specifying the positive class in the column
#'   \code{target} used for computing the fairness metrics. If \code{NULL} (the
#'   default), the first factor level of the \code{target} column is used as the
#'   positive class.
#' @param materiality  a numeric value between 0 and 1 specifying the
#'   materiality used to decide whether the statistics are out of bound. The
#'   tolerance range is defined on the parity statistics as
#'   \code{materiality} and \code{1 / materiality}.
#' @param alternative  the type of confidence interval to produce. Possible
#'   options are \code{two.sided} (the default), \code{greater} and \code{less}.
#' @param conf.level   a numeric value between 0 and 1 specifying the
#'   confidence level (i.e., 1 - audit risk / detection risk).
#'
#' @details The following model-agnostic fairness metrics are computed based on
#'   the confusion matrix for each sensitive group, using the  true positives
#'   (TP), false positives (FP), true negative (TN) and false negatives (FN).
#'
#'   \itemize{
#'     \item{Demographic parity: }{measures whether the observed variable is
#'     distributed equally across different groups, calculated as TP + FP.}
#'     \item{Proportional parity: }{measures whether the positive rate is
#'       distributed equally across different groups, calculated as (TP + FP) /
#'       (TP + FP + TN + FN).}
#'     \item{Predictive rate parity: }{measures whether the positive prediction
#'       rate is the same across different groups, calculated as TP / (TP + FP).}
#'     \item{Accuracy parity: }{measures whether the overall accuracy is the same
#'       across different groups, calculated as (TP + TN) / (TP + FP + TN + FN).}
#'     \item{False negative rate parity: }{measures whether the false negative
#'       rate is the same across different groups, calculated as FN / (FP + FN).}
#'     \item{False positive rate parity: }{measures whether the false positive
#'       rate is the same across different groups, calculated as FP / (TN + FP).}
#'     \item{True positive rate parity: }{measures whether the true positive rate
#'       is the same across different groups, calculated as TP / (TP + FN).}
#'     \item{Negative predicted value parity: }{measures whether the negative
#'       predicted value is the same across different groups, calculated as TN /
#'       (TN + FN).}
#'     \item{Specificity parity: }{measures whether the true positive rate
#'       is the same across different groups, calculated as TN / (TN + FP).}
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
#' @return An object of class \code{jfaModelBias} containing:
#'
#' \item{reference}{The reference group for computing the fairness metrics.}
#' \item{positive}{The positive class used in computing the fairness metrics.}
#' \item{alternative}{The type of confidence interval.}
#' \item{confusion.matrix}{A list of confusion matrices for each group.}
#' \item{performance}{A data frame containing performance metrics for each
#'   group, including accuracy, precision, recall, and F1 score.}
#' \item{metrics}{A data frame containing fairness metrics for each group,
#'   including demographic parity, proportional parity, predictive rate parity,
#'   accuracy parity, false negative rate parity, false positive rate parity,
#'   true positive rate parity, negative predicted value parity, and statistical
#'   parity.}
#' \item{parity}{A data frame containing fairness parity for each metric,
#'   comparing each group to the reference group.}
#' \item{odds.ratio}{A data frame containing odds ratio of the fairness parity
#'   for each metric, comparing each group to the reference group.}
#' \item{materiality}{The materiality value used to determine the out of bounds
#'   metrics.}
#' \item{data.name}{The name of the input data object.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @references Büyük, S. (2023). \emph{Automatic Fairness Criteria and Fair
#'   Model Selection for Critical ML Tasks}, Master Thesis, Utrecht University.
#' @references Pessach, D. & Shmueli, E. (2022). A review on fairness in machine
#'   learning. \emph{ACM Computing Surveys}, 55(3), 1-44. \doi{10.1145/3494672}
#'
#' @keywords algorithm audit bias fairness model performance
#'
#' @examples
#' model_fairness(compas, "Ethnicity", "TwoYrRecidivism", "Predicted",
#'   reference = "Caucasian", positive = "yes"
#' )
#' @export

model_fairness <- function(data,
                           sensitive,
                           target,
                           predictions,
                           reference = NULL,
                           positive = NULL,
                           materiality = 0.8,
                           alternative = c("two.sided", "greater", "less"),
                           conf.level = 0.95) {
  alternative <- match.arg(alternative)
  dname <- deparse(substitute(data))
  data <- as.data.frame(data, row.names = seq_len(nrow(data)))
  stopifnot("'sensitive' does not exist in 'data'" = sensitive %in% colnames(data))
  stopifnot("'sensitive' must be a factor column" = is.factor(data[, sensitive]))
  stopifnot("'target' does not exist in 'data'" = target %in% colnames(data))
  stopifnot("'target' must be a factor column" = is.factor(data[, target]))
  stopifnot("'predictions' does not exist in 'data'" = predictions %in% colnames(data))
  stopifnot("'predictions' must be a factor column" = is.factor(data[, predictions]))
  stopifnot("'materiality' must be a single value between 0 and 1" = materiality > 0 && materiality < 1)
  groupLevels <- levels(data[, sensitive])
  targetLevels <- levels(data[, target])
  stopifnot("number of factor levels in 'target' must be 2" = length(targetLevels) == 2)
  stopifnot("number of factor levels in 'predictions' must be 2" = nlevels(data[, predictions]) == 2)
  measures <- c("dp", "pp", "prp", "ap", "fnrp", "fprp", "tprp", "npvp", "sp")
  if (is.null(reference)) {
    reference <- groupLevels[1]
  }
  stopifnot("'reference' is not a class in 'sensitive'" = reference %in% groupLevels)
  if (is.null(positive)) {
    positive <- targetLevels[1]
  }
  stopifnot("'positive' is not a class in 'target'" = positive %in% targetLevels)
  negative <- targetLevels[-which(targetLevels == positive)]
  metrics <- list(all = as.data.frame(matrix(NA, nrow = length(groupLevels), ncol = length(measures))))
  parity <- list(all = as.data.frame(matrix(NA, nrow = length(groupLevels), ncol = length(measures))))
  odds.ratio <- list(all = as.data.frame(matrix(NA, nrow = length(groupLevels), ncol = length(measures) - 1)))
  performance <- list(all = as.data.frame(matrix(NA, nrow = length(groupLevels), ncol = 5)))
  confmat <- list()
  for (i in seq_len(nlevels(data[, sensitive]))) {
    group <- levels(data[, sensitive])[i]
    groupDat <- data[data[, sensitive] == group, ]
    # Confusion matrices
    confmat[[group]][["matrix"]] <- table("Actual" = groupDat[, target], "Predicted" = groupDat[, predictions])
    confmat[[group]][["tp"]] <- tp <- confmat[[group]][["matrix"]][positive, positive]
    confmat[[group]][["fp"]] <- fp <- confmat[[group]][["matrix"]][negative, positive]
    confmat[[group]][["tn"]] <- tn <- confmat[[group]][["matrix"]][negative, negative]
    confmat[[group]][["fn"]] <- fn <- confmat[[group]][["matrix"]][positive, negative]
    # Performance measures
    performance[[group]][["support"]] <- performance[["all"]][i, 1] <- sum(confmat[[group]][["matrix"]])
    performance[[group]][["accuracy"]] <- performance[["all"]][i, 2] <- (confmat[[group]][["tp"]] + confmat[[group]][["tn"]]) / (confmat[[group]][["tp"]] + confmat[[group]][["tn"]] + confmat[[group]][["fp"]] + confmat[[group]][["fn"]])
    performance[[group]][["precision"]] <- performance[["all"]][i, 3] <- confmat[[group]][["tp"]] / (confmat[[group]][["tp"]] + confmat[[group]][["fp"]])
    performance[[group]][["recall"]] <- performance[["all"]][i, 4] <- confmat[[group]][["tp"]] / (confmat[[group]][["tp"]] + confmat[[group]][["fn"]])
    performance[[group]][["f1.score"]] <- performance[["all"]][i, 5] <- 2 * ((performance[[group]][["precision"]] * performance[[group]][["recall"]]) / (performance[[group]][["precision"]] + performance[[group]][["recall"]]))
    # Fairness metrics
    for (j in seq_len(length(measures))) {
      metric <- measures[j]
      if (metric == "dp") {
        metrics[[metric]][[group]][["mle"]] <- tp + fp
      } else {
        metrics[[metric]][[group]][["numerator"]] <- num <- switch(metric,
          "pp" = tp + fp,
          "prp" = tp,
          "ap" = tp + tn,
          "fnrp" = fn,
          "fprp" = fp,
          "tprp" = tp,
          "npvp" = tn,
          "sp" = tn
        )
        metrics[[metric]][[group]][["denominator"]] <- denom <- switch(metric,
          "pp" = tp + fp + tn + fn,
          "prp" = tp + fp,
          "ap" = tp + fp + tn + fn,
          "fnrp" = tp + fn,
          "fprp" = tn + fp,
          "tprp" = tp + fn,
          "npvp" = tn + fn,
          "sp" = tn + fp
        )
        metrics[[metric]][[group]][["mle"]] <- num / denom
        test <- stats::binom.test(x = num, n = denom, conf.level = conf.level, alternative = alternative)
        metrics[[metric]][[group]][["lb"]] <- test$conf.int[1]
        metrics[[metric]][[group]][["ub"]] <- test$conf.int[2]
      }
      metrics[["all"]][i, j] <- metrics[[metric]][[group]][["mle"]]
    }
  }
  rownames(metrics[["all"]]) <- rownames(performance[["all"]]) <- groupLevels
  colnames(metrics[["all"]]) <- measures
  colnames(performance[["all"]]) <- c("support", "accuracy", "precision", "recall", "f1.score")
  # Parity ratios
  for (i in seq_len(nlevels(data[, sensitive]))) {
    group <- levels(data[, sensitive])[i]
    for (j in seq_len(length(measures))) {
      metric <- measures[j]
      parity[[metric]][[group]][["mle"]] <- metrics[[metric]][[group]][["mle"]] / metrics[[metric]][[reference]][["mle"]]
      if (metric != "dp") {
        parity[[metric]][[group]][["lb"]] <- metrics[[metric]][[group]][["lb"]] / metrics[[metric]][[reference]][["mle"]]
        parity[[metric]][[group]][["ub"]] <- metrics[[metric]][[group]][["ub"]] / metrics[[metric]][[reference]][["mle"]]
      }
      parity[[metric]][[group]][["deviation"]] <- ((parity[[metric]][[group]][["mle"]] < materiality) || (parity[[metric]][[group]][["mle"]] > 1 / materiality))
      parity[["all"]][i, j] <- parity[[metric]][[group]][["mle"]]
    }
  }
  rownames(parity[["all"]]) <- groupLevels
  colnames(parity[["all"]]) <- measures
  names(confmat) <- groupLevels
  # Odds ratios
  for (i in seq_len(nlevels(data[, sensitive]))) {
    group <- levels(data[, sensitive])[i]
    for (j in 2:length(measures)) {
      metric <- measures[j]
      odds.ratio[[metric]][[group]][["mle"]] <- (metrics[[metric]][[group]][["mle"]] / (1 + metrics[[metric]][[group]][["mle"]])) / (metrics[[metric]][[reference]][["mle"]] / (1 + metrics[[metric]][[reference]][["mle"]]))
      test <- stats::fisher.test(
        matrix(c(
          metrics[[metric]][[group]][["numerator"]],
          metrics[[metric]][[group]][["denominator"]] - metrics[[metric]][[group]][["numerator"]],
          metrics[[metric]][[reference]][["numerator"]],
          metrics[[metric]][[reference]][["denominator"]] - metrics[[metric]][[reference]][["numerator"]]
        ), ncol = 2),
        alternative = alternative,
        conf.level = conf.level
      )
      odds.ratio[[metric]][[group]][["lb"]] <- test$conf.int[1]
      odds.ratio[[metric]][[group]][["ub"]] <- test$conf.int[2]
      odds.ratio[[metric]][[group]][["p.value"]] <- test$p.value
      odds.ratio[["all"]][i, j - 1] <- odds.ratio[[metric]][[group]][["mle"]]
    }
  }
  rownames(odds.ratio[["all"]]) <- groupLevels
  colnames(odds.ratio[["all"]]) <- measures[-1]
  names(confmat) <- groupLevels
  result <- list()
  result[["reference"]] <- reference
  result[["positive"]] <- positive
  result[["alternative"]] <- alternative
  result[["confusion.matrix"]] <- confmat
  result[["performance"]] <- performance
  result[["metrics"]] <- metrics
  result[["parity"]] <- parity
  result[["odds.ratio"]] <- odds.ratio
  result[["materiality"]] <- materiality
  result[["data.name"]] <- dname
  class(result) <- c("jfaModelBias", "list")
  return(result)
}
