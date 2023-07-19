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
#' systems by computing various fairness metrics. It provides an assessment of
#' fairness across different groups based on the observed and predicted
#' outcomes. The function allows for evaluating fairness using metrics such as
#' demographic parity, predictive parity, predictive rate parity, accuracy
#' parity, false negative rate parity, false positive rate parity, true positive
#' rate parity, negative predicted value parity, and statistical parity and
#' decide whether groups are fair to a certain degree and within a certain
#' materiality threshold. Currently, it only supports binary classification.
#'
#' @usage model_fairness(
#'   data,
#'   sensitive,
#'   target,
#'   predictions,
#'   reference = NULL,
#'   positive = NULL,
#'   materiality = 0.2
#' )
#'
#' @param data         The input data.
#' @param sensitive    The column name indicating the sensitive variable.
#' @param target       The column name indicating the target class labels.
#' @param predictions  The column name indicating the predictions class labels.
#' @param reference    The reference class for computing the fairness metrics.
#'   If \code{NULL} (the default), the first level of the \code{sensitive}
#'   column is used.
#' @param positive     The positive class for computing the fairness metrics.
#'   If \code{NULL} (the default), the first level of the \code{target}
#'   column is used.
#' @param materiality  The materiality value for determining fairness.
#'
#' @details The following fairness metrics are computed:
#'
#' \itemize{
#'   \item{Demographic parity}{measures whether the observed variable is
#'     distributed equally across different groups.}
#'   \item{Predictive parity}{measures whether the predicted variable is
#'     distributed equally across different groups.}
#'   \item{Predictive rate parity}{measures whether the positive prediction rate
#'     is the same across different groups.}
#'   \item{Accuracy parity}{measures whether the overall accuracy is the same
#'     across different groups.}
#'   \item{False negative rate parity}{measures whether the false negative rate
#'     is the same across different groups.}
#'   \item{False positive rate parity}{measures whether the false positive rate
#'     is the same across different groups.}
#'   \item{True positive rate parity}{measures whether the true positive rate is
#'     the same across different groups.}
#'   \item{Negative predicted value parity}{measures whether the negative
#'     predicted value is the same across different groups.}
#'   \item{Statistical parity}{measures whether the predicted variable is
#'     distributed equally across different groups.}
#' }
#'
#' @return An object of class \code{jfaModelBias} containing:
#'
#' \item{reference}{The reference group for computing the fairness metrics.}
#' \item{positive}{The positive class used in computing the fairness metrics.}
#' \item{confusion.matrix}{A list of confusion matrices for each group.}
#' \item{performance}{A data frame containing performance metrics for each
#'   group, including accuracy, precision, recall, and F1 score.}
#' \item{fairness}{A data frame containing fairness metrics for each group,
#'   including demographic parity, predictive parity, predictive rate parity,
#'   accuracy parity, false negative rate parity, false positive rate parity,
#'   true positive rate parity, negative predicted value parity, and statistical
#'   parity.}
#' \item{ratio}{A data frame containing fairness ratios for each metric,
#'   comparing each group to the reference group.}
#' \item{materiality}{The materiality value used to determine the out of bounds
#'   metrics.}
#' \item{data.name}{The name of the input data object.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
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
    # Most likely values
    dp <- counts[["tp"]] + counts[["fp"]]
    pp <- (counts[["tp"]] + counts[["fp"]]) / (counts[["tp"]] + counts[["fp"]] + counts[["tn"]] + counts[["fn"]])
    prp <- counts[["tp"]] / (counts[["tp"]] + counts[["fp"]])
    ap <- (counts[["tp"]] + counts[["tn"]]) / (counts[["tp"]] + counts[["fp"]] + counts[["tn"]] + counts[["fn"]])
    fnrp <- counts[["fn"]] / (counts[["tp"]] + counts[["fn"]])
    fprp <- counts[["fp"]] / (counts[["tn"]] + counts[["fp"]])
    tprp <- counts[["tp"]] / (counts[["tp"]] + counts[["fn"]])
    npvp <- counts[["tn"]] / (counts[["tn"]] + counts[["fn"]])
    sp <- counts[["tn"]] / (counts[["tn"]] + counts[["fp"]])
    # Upper bounds
    pp_ub <- stats::qbeta(conf.level + (1 - conf.level) / 2, 1 + counts[["tp"]] + counts[["fp"]], counts[["tn"]] + counts[["fn"]])
    prp_ub <- stats::qbeta(conf.level + (1 - conf.level) / 2, 1 + counts[["tp"]], counts[["fp"]])
    ap_ub <- stats::qbeta(conf.level + (1 - conf.level) / 2, 1 + counts[["tp"]] + counts[["tn"]], counts[["fp"]] + counts[["fn"]])
    fnrp_ub <- stats::qbeta(conf.level + (1 - conf.level) / 2, 1 + counts[["fn"]], counts[["tp"]])
    fprp_ub <- stats::qbeta(conf.level + (1 - conf.level) / 2, 1 + counts[["fp"]], counts[["fp"]])
    tprp_ub <- stats::qbeta(conf.level + (1 - conf.level) / 2, 1 + counts[["tp"]], counts[["fn"]])
    npvp_ub <- stats::qbeta(conf.level + (1 - conf.level) / 2, 1 + counts[["tn"]], counts[["fn"]])
    sp_ub <- stats::qbeta(conf.level + (1 - conf.level) / 2, 1 + counts[["tn"]], counts[["fp"]])
    # Lower bounds
    pp_lb <- stats::qbeta((1 - conf.level) / 2, counts[["tp"]] + counts[["fp"]], 1 + counts[["tn"]] + counts[["fn"]])
    prp_lb <- stats::qbeta((1 - conf.level) / 2, counts[["tp"]], 1 + counts[["fp"]])
    ap_lb <- stats::qbeta((1 - conf.level) / 2, counts[["tp"]] + counts[["tn"]], 1 + counts[["fp"]] + counts[["fn"]])
    fnrp_lb <- stats::qbeta((1 - conf.level) / 2, counts[["fn"]], 1 + counts[["tp"]])
    fprp_lb <- stats::qbeta((1 - conf.level) / 2, counts[["fp"]], 1 + counts[["tn"]])
    tprp_lb <- stats::qbeta((1 - conf.level) / 2, counts[["tp"]], 1 + counts[["fn"]])
    npvp_lb <- stats::qbeta((1 - conf.level) / 2, counts[["tn"]], 1 + counts[["fn"]])
    sp_lb <- stats::qbeta((1 - conf.level) / 2, counts[["tn"]], 1 + counts[["fp"]])
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
  result[["confusion.matrix"]] <- confmat
  result[["performance"]] <- performance
  result[["mle"]] <- mle
  result[["ub"]] <- ub
  result[["lb"]] <- lb
  result[["mle_ratio"]] <- mle_ratio
  result[["ub_ratio"]] <- ub_ratio
  result[["lb_ratio"]] <- lb_ratio
  result[["materiality"]] <- materiality
  result[["data.name"]] <- dname
  class(result) <- c("jfaModelBias", "list")
  return(result)
}
