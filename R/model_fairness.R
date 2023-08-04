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
#' @description This function aims to assess fairness and bias in algorithmic
#' decision-making systems by computing and testing one of several
#' model-agnostic fairness metrics. These metrics quantify fairness across
#' groups in the data based on the predictions of the algorithm in several ways.
#' The calculated metrics are commonly used and include predictive rate parity,
#' proportional parity, true positive rate parity, accuracy parity, false
#' negative rate parity, false positive rate parity, true positive
#' rate parity, negative predicted value parity, specificity parity, and
#' demographic parity. Currently, this function only supports binary
#' classification.
#'
#' @usage model_fairness(
#'   data,
#'   sensitive,
#'   target,
#'   predictions,
#'   reference = NULL,
#'   positive = NULL,
#'   metric = c(
#'     "prp", "pp", "ap", "fnrp", "fprp",
#'     "tprp", "npvp", "sp", "dp"
#'   ),
#'   alternative = c("two.sided", "greater", "less"),
#'   conf.level = 0.95,
#'   prior = FALSE
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
#' @param metric      a character (vector) indicating the fairness metrics(s)
#'   to compute. See the Details section for more information.
#' @param alternative  the type of confidence interval to produce. Possible
#'   options are \code{two.sided} (the default), \code{greater} and \code{less}.
#' @param conf.level   a numeric value between 0 and 1 specifying the
#'   confidence level (i.e., 1 - audit risk / detection risk).
#' @param prior        a logical specifying whether to use a prior distribution,
#'   or a numeric value larger than 1 specifying the prior concentration
#'   parameter. If this argument is specified as \code{FALSE} (default), the
#'   function performs classical evaluation. If this argument is \code{TRUE},
#'   the function performs Bayesian evaluation using a default prior with a
#'   concentration parameter of 1.
#'
#' @details The following model-agnostic fairness metrics are computed based on
#'   the confusion matrix for each sensitive group, using the  true positives
#'   (TP), false positives (FP), true negative (TN) and false negatives (FN).
#'
#'   \itemize{
#'     \item{Predictive rate parity (\code{prp}): }{measures whether the positive prediction
#'       rate is the same across different groups, calculated as TP / (TP + FP).}
#'     \item{Proportional parity (\code{pp}): }{measures whether the positive rate is
#'       distributed equally across different groups, calculated as (TP + FP) /
#'       (TP + FP + TN + FN).}
#'     \item{Accuracy parity (\code{ap}): }{measures whether the overall accuracy is the same
#'       across different groups, calculated as (TP + TN) / (TP + FP + TN + FN).}
#'     \item{False negative rate parity (\code{fnrp}): }{measures whether the false negative
#'       rate is the same across different groups, calculated as FN / (FP + FN).}
#'     \item{False positive rate parity (\code{fprp}): }{measures whether the false positive
#'       rate is the same across different groups, calculated as FP / (TN + FP).}
#'     \item{True positive rate parity (\code{tprp}): }{measures whether the true positive rate
#'       is the same across different groups, calculated as TP / (TP + FN).}
#'     \item{Negative predicted value parity (\code{npvp}): }{measures whether the negative
#'       predicted value is the same across different groups, calculated as TN /
#'       (TN + FN).}
#'     \item{Specificity parity (\code{sp}): }{measures whether the true positive rate
#'       is the same across different groups, calculated as TN / (TN + FP).}
#'     \item{Demographic parity (\code{dp}): }{measures whether the observed variable is
#'       distributed equally across different groups, calculated as TP + FP.}
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
#' @return An object of class \code{jfaModelFairness} containing:
#'
#' \item{reference}{The reference group for computing the fairness metrics.}
#' \item{positive}{The positive class used in computing the fairness metrics.}
#' \item{alternative}{The type of confidence interval.}
#' \item{confusion.matrix}{A list of confusion matrices for each group.}
#' \item{performance}{A data frame containing performance metrics for each
#'   group, including accuracy, precision, recall, and F1 score.}
#' \item{metric}{A data frame containing, for each group, the estimates of the
#'   fairness metric along with the associated confidence / credible interval.}
#' \item{parity}{A data frame containing, for each sensitive group, the parity
#'   ratio and associated confidence / credible interval when compared to the
#'   reference group.}
#' \item{odds.ratio}{A data frame containing, for each sensitive group, the odds
#'   ratio of the fairness meatrics and associated confidence/credible interval,
#'   along with any inferential measures, for the comparison to the reference
#'   group.}
#' \item{measure}{The abbreviation of the selected fairness metric.}
#' \item{data.name}{The name of the input data object.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @references Büyük, S. (2023). \emph{Automatic Fairness Criteria and Fair
#'   Model Selection for Critical ML Tasks}, Master Thesis, Utrecht University.
#' @references Fisher, R. A. (1970). Statistical Methods for Research Workers.
#'   Oliver & Boyd.
#' @references Jamil, T., Ly, A., Morey, R. D., Love, J., Marsman, M., &
#'   Wagenmakers, E. J. (2017). Default "Gunel and Dickey" Bayes factors for
#'   contingency tables. \emph{Behavior Research Methods}, 49, 638-652.
#'   \doi{10.3758/s13428-016-0739-8}
#' @references Pessach, D. & Shmueli, E. (2022). A review on fairness in machine
#'   learning. \emph{ACM Computing Surveys}, 55(3), 1-44. \doi{10.1145/3494672}
#'
#' @keywords algorithm audit bias fairness model performance
#'
#' @examples
#' # Frequentist test of specificy parity
#' model_fairness(
#'   data = compas,
#'   sensitive = "Gender",
#'   target = "TwoYrRecidivism",
#'   predictions = "Predicted",
#'   reference = "Male",
#'   positive = "yes",
#'   metric = "sp"
#' )
#'
#' # Bayesian test of predictive rate parity
#' model_fairness(
#'   data = compas,
#'   sensitive = "Ethnicity",
#'   target = "TwoYrRecidivism",
#'   predictions = "Predicted",
#'   reference = "Caucasian",
#'   positive = "yes",
#'   metric = "prp"
#' )
#' @export

model_fairness <- function(data,
                           sensitive,
                           target,
                           predictions,
                           reference = NULL,
                           positive = NULL,
                           metric = c(
                             "prp", "pp", "ap", "fnrp", "fprp",
                             "tprp", "npvp", "sp", "dp"
                           ),
                           alternative = c("two.sided", "greater", "less"),
                           conf.level = 0.95,
                           prior = FALSE) {
  metric <- match.arg(metric)
  alternative <- match.arg(alternative)
  dname <- deparse(substitute(data))
  data <- as.data.frame(data, row.names = seq_len(nrow(data)))
  valid_prior <- (is.logical(prior)) || (is.numeric(prior) && prior >= 1)
  is_bayesian <- (is.logical(prior) && isTRUE(prior)) || (is.numeric(prior) && prior >= 1)
  stopifnot("'prior' must be TRUE or FALSE, or a numeric value >= 1 representing the prior concentration parameter" = valid_prior)
  stopifnot("'sensitive' does not exist in 'data'" = sensitive %in% colnames(data))
  stopifnot("'sensitive' must be a factor column" = is.factor(data[, sensitive]))
  stopifnot("'target' does not exist in 'data'" = target %in% colnames(data))
  stopifnot("'target' must be a factor column" = is.factor(data[, target]))
  stopifnot("'predictions' does not exist in 'data'" = predictions %in% colnames(data))
  stopifnot("'predictions' must be a factor column" = is.factor(data[, predictions]))
  groupLevels <- levels(data[, sensitive])
  targetLevels <- levels(data[, target])
  stopifnot("'target' must contain exactly 2 factor levels" = length(targetLevels) == 2) # Binary classification only
  stopifnot("'predictions' must contain exactly 2 factor levels" = nlevels(data[, predictions]) == 2) # Binary classification only
  if (is.null(reference)) {
    reference <- groupLevels[1]
  }
  stopifnot("'reference' is not a class in 'sensitive'" = reference %in% groupLevels)
  if (is.null(positive)) {
    positive <- targetLevels[1]
  }
  stopifnot("'positive' is not a class in 'target'" = positive %in% targetLevels)
  confmat <- list()
  samples_list <- list()
  inferenceLevels <- groupLevels[-which(groupLevels == reference)]
  negative <- targetLevels[-which(targetLevels == positive)]
  performance <- list(all = as.data.frame(matrix(NA, nrow = length(groupLevels), ncol = 5), row.names = groupLevels))
  colnames(performance[["all"]]) <- c("support", "accuracy", "precision", "recall", "f1.score")
  metrics <- list(all = as.data.frame(matrix(NA, nrow = length(groupLevels), ncol = if (metric == "dp") 1 else 3), row.names = groupLevels))
  parity <- list(all = as.data.frame(matrix(NA, nrow = length(groupLevels), ncol = if (metric == "dp") 1 else 3), row.names = groupLevels))
  colnames(metrics[["all"]]) <- colnames(parity[["all"]]) <- if (metric != "dp") c("estimate", "lb", "ub") else "estimate"
  odds.ratio <- list(all = as.data.frame(matrix(NA, nrow = length(inferenceLevels), ncol = 4), row.names = inferenceLevels))
  colnames(odds.ratio[["all"]]) <- if (is_bayesian) c("estimate", "lb", "ub", "bf10") else c("estimate", "lb", "ub", "p.value")
  for (i in seq_len(nlevels(data[, sensitive]))) {
    group <- levels(data[, sensitive])[i]
    groupDat <- data[data[, sensitive] == group, ]
    # Confusion matrices for each group
    confmat[[group]][["matrix"]] <- table("Actual" = groupDat[, target], "Predicted" = groupDat[, predictions])
    confmat[[group]][["tp"]] <- tp <- confmat[[group]][["matrix"]][positive, positive]
    confmat[[group]][["fp"]] <- fp <- confmat[[group]][["matrix"]][negative, positive]
    confmat[[group]][["tn"]] <- tn <- confmat[[group]][["matrix"]][negative, negative]
    confmat[[group]][["fn"]] <- fn <- confmat[[group]][["matrix"]][positive, negative]
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
  names(confmat) <- groupLevels
  # Sample estimates for each group
  if (metric != "dp") {
    for (i in seq_len(nlevels(data[, sensitive]))) {
      group <- levels(data[, sensitive])[i]
      if (!is_bayesian) {
        binom_test <- stats::binom.test(x = metrics[[group]][["numerator"]], n = metrics[[group]][["denominator"]], conf.level = conf.level, alternative = alternative)
        metrics[[group]][["estimate"]] <- metrics[["all"]][i, 1] <- as.numeric(binom_test$estimate)
        metrics[[group]][["lb"]] <- metrics[["all"]][i, 2] <- as.numeric(binom_test$conf.int[1])
        metrics[[group]][["ub"]] <- metrics[["all"]][i, 3] <- as.numeric(binom_test$conf.int[2])
      } else {
        contingencyTable <- matrix(c(
          metrics[[group]][["numerator"]],
          metrics[[group]][["denominator"]] - metrics[[group]][["numerator"]],
          metrics[[reference]][["numerator"]],
          metrics[[reference]][["denominator"]] - metrics[[reference]][["numerator"]]
        ), ncol = 2)
        samples_list[[group]] <- .mcmc_or(counts = c(contingencyTable), prior_a = prior)
        metrics[[group]][["estimate"]] <- metrics[["all"]][i, 1] <- .comp_mode_bayes(analytical = FALSE, samples = samples_list[[group]]$prob)
        metrics[[group]][["lb"]] <- metrics[["all"]][i, 2] <- .comp_lb_bayes(alternative, conf.level, analytical = FALSE, samples = samples_list[[group]]$prob)
        metrics[[group]][["ub"]] <- metrics[["all"]][i, 3] <- .comp_ub_bayes(alternative, conf.level, analytical = FALSE, samples = samples_list[[group]]$prob)
      }
    }
  }
  # Parity ratio for each group
  rowIndex <- 1
  for (group in groupLevels) {
    if (group == reference) {
      parity[[group]][["estimate"]] <- parity[["all"]][rowIndex, 1] <- 1
      if (metric != "dp") {
        parity[[group]][["lb"]] <- parity[[group]][["ub"]] <- parity[["all"]][rowIndex, 2] <- parity[["all"]][rowIndex, 3] <- 1
      }
    } else {
      parity[[group]][["estimate"]] <- parity[["all"]][rowIndex, 1] <- metrics[[group]][["estimate"]] / metrics[[reference]][["estimate"]]
      if (metric != "dp") {
        parity[[group]][["lb"]] <- parity[["all"]][rowIndex, 2] <- metrics[[group]][["lb"]] / metrics[[reference]][["estimate"]]
        parity[[group]][["ub"]] <- parity[["all"]][rowIndex, 3] <- metrics[[group]][["ub"]] / metrics[[reference]][["estimate"]]
      }
    }
    rowIndex <- rowIndex + 1
  }
  # Odds ratio for each sensitive group
  if (metric != "dp") {
    rowIndex <- 1
    for (group in inferenceLevels) {
      contingencyTable <- matrix(c(
        metrics[[group]][["numerator"]],
        metrics[[group]][["denominator"]] - metrics[[group]][["numerator"]],
        metrics[[reference]][["numerator"]],
        metrics[[reference]][["denominator"]] - metrics[[reference]][["numerator"]]
      ), ncol = 2)
      if (!is_bayesian) {
        fisher_test <- stats::fisher.test(contingencyTable, alternative = alternative, conf.level = conf.level)
        odds.ratio[[group]][["estimate"]] <- odds.ratio[["all"]][rowIndex, 1] <- as.numeric(fisher_test$estimate)
        odds.ratio[[group]][["lb"]] <- odds.ratio[["all"]][rowIndex, 2] <- as.numeric(fisher_test$conf.int[1])
        odds.ratio[[group]][["ub"]] <- odds.ratio[["all"]][rowIndex, 3] <- as.numeric(fisher_test$conf.int[2])
        odds.ratio[[group]][["p.value"]] <- odds.ratio[["all"]][rowIndex, 4] <- as.numeric(fisher_test$p.value)
      } else {
        odds.ratio[[group]][["estimate"]] <- odds.ratio[["all"]][i, 1] <- .comp_mode_bayes(analytical = FALSE, samples = samples_list[[group]]$OR)
        odds.ratio[[group]][["lb"]] <- odds.ratio[["all"]][i, 2] <- .comp_lb_bayes(alternative, conf.level, analytical = FALSE, samples = samples_list[[group]]$OR)
        odds.ratio[[group]][["ub"]] <- odds.ratio[["all"]][i, 3] <- .comp_ub_bayes(alternative, conf.level, analytical = FALSE, samples = samples_list[[group]]$OR)
        odds.ratio[[group]][["bf10"]] <- odds.ratio[["all"]][i, 4] <- .contingencyTableBf(contingencyTable, prior_a = prior)
        density_post <- density(log(samples_list[[group]]$OR), n = 1000)
        density_post_alt <- density(log(samples_list[[group]]$OR), n = 10000, from = -10, to = 10)
        odds.ratio[[group]][["density"]] <- list(x = density_post_alt$x, y = density_post_alt$y, xmin = min(density_post$x), xmax = max(density_post$x))
      }
      rowIndex <- rowIndex + 1
    }
  }
  result <- list()
  result[["reference"]] <- reference
  result[["positive"]] <- positive
  result[["alternative"]] <- alternative
  result[["measure"]] <- metric
  result[["confusion.matrix"]] <- confmat
  result[["performance"]] <- performance
  result[["metric"]] <- metrics
  result[["parity"]] <- parity
  if (result[["measure"]] != "dp") {
    result[["odds.ratio"]] <- odds.ratio
  }
  result[["prior"]] <- prior
  result[["data.name"]] <- dname
  class(result) <- c("jfaModelFairness", "list")
  return(result)
}
