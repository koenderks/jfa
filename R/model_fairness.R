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
#'   materiality = 0.2,
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
#'   \code{1 - materiality} and \code{1 + materiality}.
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
#' \item{ratio}{A data frame containing fairness parity for each metric,
#'   comparing each group to the reference group.}
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
                           materiality = 0.2,
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
  if (is.null(reference)) {
    reference <- groupLevels[1]
  }
  stopifnot("'reference' is not a class in 'sensitive'" = reference %in% groupLevels)
  if (is.null(positive)) {
    positive <- targetLevels[1]
  }
  stopifnot("'positive' is not a class in 'target'" = positive %in% targetLevels)
  negative <- targetLevels[-which(targetLevels == positive)]
  refIndex <- which(groupLevels == reference)
  mle <- data.frame()
  performance <- data.frame()
  lb <- data.frame()
  ub <- data.frame()
  confmat <- list()
  for (i in seq_len(nlevels(data[, sensitive]))) {
    group <- levels(data[, sensitive])[i]
    groupDat <- data[data[, sensitive] == group, ]
    confmat[[i]] <- table("Actual" = groupDat[, target], "Predicted" = groupDat[, predictions])
    counts <- data.frame(
      tp = confmat[[i]][positive, positive],
      fp = confmat[[i]][negative, positive],
      tn = confmat[[i]][negative, negative],
      fn = confmat[[i]][positive, negative]
    )
    dp <- counts[["tp"]] + counts[["fp"]] # Demographic parity
    pp <- (counts[["tp"]] + counts[["fp"]]) / (counts[["tp"]] + counts[["fp"]] + counts[["tn"]] + counts[["fn"]]) # Proportional parity
    prp <- counts[["tp"]] / (counts[["tp"]] + counts[["fp"]]) # Predictive rate parity
    ap <- (counts[["tp"]] + counts[["tn"]]) / (counts[["tp"]] + counts[["fp"]] + counts[["tn"]] + counts[["fn"]]) # Accuracy parity
    fnrp <- counts[["fn"]] / (counts[["tp"]] + counts[["fn"]]) # False negative rate parity
    fprp <- counts[["fp"]] / (counts[["tn"]] + counts[["fp"]]) # False positive rate parity
    tprp <- counts[["tp"]] / (counts[["tp"]] + counts[["fn"]]) # True positive rate parity
    npvp <- counts[["tn"]] / (counts[["tn"]] + counts[["fn"]]) # Negative predicted value parity
    sp <- counts[["tn"]] / (counts[["tn"]] + counts[["fp"]]) # Specificity parity
    test_pp <- stats::binom.test(x = counts[["tp"]] + counts[["fp"]], n = counts[["tp"]] + counts[["fp"]] + counts[["tn"]] + counts[["fn"]], conf.level = conf.level, alternative = alternative)
    test_prp <- stats::binom.test(x = counts[["tp"]], n = counts[["tp"]] + counts[["fp"]], conf.level = conf.level, alternative = alternative)
    test_ap <- stats::binom.test(x = counts[["tp"]] + counts[["tn"]], n = counts[["tp"]] + counts[["tn"]] + counts[["fp"]] + counts[["fn"]], conf.level = conf.level, alternative = alternative)
    test_fnrp <- stats::binom.test(x = counts[["fn"]], n = counts[["fn"]] + counts[["tp"]], conf.level = conf.level, alternative = alternative)
    test_fprp <- stats::binom.test(x = counts[["fp"]], n = counts[["tn"]] + counts[["fp"]], conf.level = conf.level, alternative = alternative)
    test_tprp <- stats::binom.test(x = counts[["tp"]], n = counts[["tp"]] + counts[["fn"]], conf.level = conf.level, alternative = alternative)
    test_npvp <- stats::binom.test(x = counts[["tn"]], n = counts[["tn"]] + counts[["fn"]], conf.level = conf.level, alternative = alternative)
    test_sp <- stats::binom.test(x = counts[["tn"]], n = counts[["tn"]] + counts[["fp"]], conf.level = conf.level, alternative = alternative)
    pp_lb <- test_pp$conf.int[1]
    prp_lb <- test_prp$conf.int[1]
    ap_lb <- test_ap$conf.int[1]
    fnrp_lb <- test_fnrp$conf.int[1]
    fprp_lb <- test_fprp$conf.int[1]
    tprp_lb <- test_tprp$conf.int[1]
    npvp_lb <- test_npvp$conf.int[1]
    sp_lb <- test_sp$conf.int[1]
    pp_ub <- test_pp$conf.int[2]
    prp_ub <- test_prp$conf.int[2]
    ap_ub <- test_ap$conf.int[2]
    fnrp_ub <- test_fnrp$conf.int[2]
    fprp_ub <- test_fprp$conf.int[2]
    tprp_ub <- test_tprp$conf.int[2]
    npvp_ub <- test_npvp$conf.int[2]
    sp_ub <- test_sp$conf.int[2]
    mle <- rbind(mle, data.frame(group, dp, pp, prp, ap, fnrp, fprp, tprp, npvp, sp))
    ub <- rbind(ub, data.frame(group, pp = pp_ub, prp = prp_ub, ap = ap_ub, fnrp = fnrp_ub, fprp = fprp_ub, tprp = tprp_ub, npvp = npvp_ub, sp = sp_ub))
    lb <- rbind(lb, data.frame(group, pp = pp_lb, prp = prp_lb, ap = ap_lb, fnrp = fnrp_lb, fprp = fprp_lb, tprp = tprp_lb, npvp = npvp_lb, sp = sp_lb))
    support <- sum(counts[1, ])
    accuracy <- (counts[["tp"]] + counts[["tn"]]) / (counts[["tp"]] + counts[["fn"]] + counts[["fp"]] + counts[["tn"]])
    precision <- counts[["tp"]] / (counts[["tp"]] + counts[["fp"]])
    recall <- counts[["tp"]] / (counts[["tp"]] + counts[["fn"]])
    f1.score <- 2 * ((precision * recall) / (precision + recall))
    performance <- rbind(performance, data.frame(group, support, accuracy, precision, recall, f1.score))
  }
  mle_ratio <- as.data.frame(apply(mle[, -1], 2, function(x, ref) as.numeric(x) / as.numeric(x[ref]), ref = refIndex))
  mle_ratio <- cbind(mle_ratio, deviation = apply(mle_ratio, 1, function(x) {
    x <- x[!is.na(x)]
    return(sum(x < 1 - materiality | x > 1 + materiality))
  }))
  mle_ratio <- cbind(group = mle[, 1], mle_ratio)
  ub_ratio <- ub
  lb_ratio <- lb
  for (i in seq_len(nrow(ub))) {
    for (j in 2:ncol(ub)) {
      ub_ratio[i, j] <- ub[i, j] / mle[refIndex, j + 1]
      lb_ratio[i, j] <- lb[i, j] / mle[refIndex, j + 1]
    }
  }
  names(confmat) <- groupLevels
  result <- list()
  result[["reference"]] <- reference
  result[["positive"]] <- positive
  result[["alternative"]] <- alternative
  result[["confusion.matrix"]] <- confmat
  result[["performance"]] <- performance
  result[["metrics"]] <- list()
  result[["metrics"]][["mle"]] <- mle
  result[["metrics"]][["ub"]] <- ub
  result[["metrics"]][["lb"]] <- lb
  result[["ratio"]][["mle"]] <- mle_ratio
  result[["ratio"]][["ub"]] <- ub_ratio
  result[["ratio"]][["lb"]] <- lb_ratio
  result[["materiality"]] <- materiality
  result[["data.name"]] <- dname
  class(result) <- c("jfaModelBias", "list")
  return(result)
}
