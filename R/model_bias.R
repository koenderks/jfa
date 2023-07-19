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
#' tolerance.
#'
#' @usage model_bias(
#'   data,
#'   grouping,
#'   observed,
#'   predicted,
#'   reference,
#'   tolerance = 0.8
#' )
#'
#' @param data       The input data.
#' @param grouping   The column name indicating the grouping variable.
#' @param observed   The column name indicating the observed class labels.
#' @param predicted  The column name indicating the predicted class labels.
#' @param reference  The reference class for computing fairness metrics.
#' @param tolerance  The tolerance value for determining fairness.
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
#' \item{reference}{The reference group for computing fairness metrics.}
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
#' \item{tolerance}{The tolerance value used to determine fairness.}
#' \item{data.name}{The name of the input data object.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @keywords algorithm audit bias fairness model performance
#'
#' @examples
#' library(randomForest)
#' data(iris)
#' set.seed(1)
#'
#' iris$Priveledged <- factor(sample(c("Yes", "No", "Unknown"), nrow(iris), TRUE))
#' train_index <- sample.int(nrow(iris), 100)
#' train <- iris[train_index, ]
#' test <- iris[-train_index, ]
#' model <- randomForest(Species ~ ., data = train)
#' iris$Predicted <- factor(predict(model, newdata = test))
#'
#' model_bias(iris, "Priveledged", "Species", "Predicted",
#'   reference = "No"
#' )
#' @export

model_bias <- function(data,
                       grouping,
                       observed,
                       predicted,
                       reference,
                       tolerance = 0.8) {
  dname <- deparse(substitute(data))
  data <- as.data.frame(data, row.names = seq_len(nrow(data)))
  stopifnot("'grouping' does not exist in 'data'" = grouping %in% colnames(data))
  stopifnot("'grouping' must be a factor column" = is.factor(data[, grouping]))
  stopifnot("'observed' does not exist in 'data'" = observed %in% colnames(data))
  stopifnot("'observed' must be a factor column" = is.factor(data[, observed]))
  stopifnot("'predicted' does not exist in 'data'" = predicted %in% colnames(data))
  stopifnot("'predicted' must be a factor column" = is.factor(data[, predicted]))
  stopifnot("'tolerance' must be a single value between 0 and 1" = tolerance > 0 && tolerance < 1)
  groupLevels <- levels(data[, grouping])
  stopifnot("error" = reference %in% groupLevels)
  refIndex <- which(groupLevels == reference)
  targetLevels <- levels(data[, observed])
  fairness <- data.frame()
  performance <- data.frame()
  confmat <- list()
  for (i in seq_len(nlevels(data[, grouping]))) {
    group <- levels(data[, grouping])[i]
    groupDat <- data[data[, grouping] == group, ]
    confmat[[i]] <- table("Observed" = groupDat[, observed], "Predicted" = groupDat[, predicted])
    counts <- data.frame(tp = confmat[[i]][2, 2], fp = confmat[[i]][2, 1], tn = confmat[[i]][1, 1], fn = confmat[[i]][1, 2])
    dp <- counts$tp + counts$fp
    pp <- (counts$tp + counts$fp) / (counts$tp + counts$fp + counts$tn + counts$fn)
    prp <- counts$tp / (counts$tp + counts$fp)
    ap <- (counts$tp + counts$tn) / (counts$tp + counts$fp + counts$tn + counts$fn)
    fnrp <- counts$fn / (counts$tp + counts$fn)
    fprp <- counts$fp / (counts$tn + counts$fp)
    tprp <- (counts$tp / (counts$tp + counts$fn))
    npvp <- counts$tn / (counts$tn + counts$fn)
    sp <- counts$tn / (counts$tn + counts$fp)
    fairness <- rbind(fairness, data.frame(group, dp, pp, prp, ap, fnrp, fprp, tprp, npvp, sp))
    accuracy <- (counts$tp + counts$tn) / (counts$tp + counts$fn + counts$fp + counts$tn)
    precision <- counts$tp / (counts$tp + counts$fp)
    recall <- counts$tp / (counts$tp + counts$fn)
    f1.score <- 2 * ((precision * recall) / (precision + recall))
    performance <- rbind(performance, data.frame(group, accuracy, precision, recall, f1.score))
  }
  ratio <- as.data.frame(apply(fairness[, -1], 2, function(x, ref) as.numeric(x) / as.numeric(x[ref]), ref = refIndex))
  ratio <- cbind(ratio, deviation = apply(ratio, 1, function(x) {
    x <- x[!is.na(x)]
    return(sum(x < tolerance | x > 1 + (1 - tolerance)))
  }))
  ratio <- cbind(group = fairness[, 1], ratio)
  names(confmat) <- groupLevels
  result <- list()
  result[["reference"]] <- reference
  result[["confusion.matrix"]] <- confmat
  result[["performance"]] <- performance
  result[["fairness"]] <- fairness
  result[["ratio"]] <- ratio
  result[["tolerance"]] <- tolerance
  result[["data.name"]] <- dname
  class(result) <- c("jfaModelBias", "list")
  return(result)
}
