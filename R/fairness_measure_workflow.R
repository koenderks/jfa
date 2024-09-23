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
#'   (2017). Fairness beyond disparate Ttreatment & disparate impact. In
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

DecisionMakingWorkflow <- function(q1 = NULL, q2 = NULL, q3 = NULL, q4 = NULL) {
  if (is.null(q1)) {
    q1 <- utils::menu(choices = c("Yes", "No"), title = "Do you have the information on the true values of the classification?")
  } else {
    if (!(q1 %in% c(1, 2))) {
      stop("Invalid input: The value of the first argument must be 1 (to indicate 'Yes') or 2 (to indicate 'No')")
    }
  }

  if (is.null(q2)) {
    q2 <- utils::menu(choices = c("Correct Classification", "Incorrect Classification", "Correct and Incorrect Classification"), title = "In what type of classification are you interested?")
  } else {
    if (!(q2 %in% c(1, 2, 3))) {
      stop("Invalid input: The value of the second argument must be 1 (to indicate 'Correct Classification'), 2 (to indicate Well-defined') or 3 (to indicate 'Correct and Incorrect Classification )")
    }
  }

  if (is.null(q3)) {
    q3 <- utils::menu(choices = c("Everything not positive", "Well-defined"), title = "Can the negative class be considered as everything not positive or is it well-defined?")
  } else {
    if (!(q3 %in% c(1, 2))) {
      stop("Invalid input: The value of the third argument must be 1 (to indicate 'Everythig not positive') or 2 (to indicate 'Well-defined')")
    }
  }

  if (is.null(q4)) {
    q4 <- utils::menu(choices = c("False Positive", "False Negative", "No preference"), title = "What are the errors with the highest cost?")
  } else {
    if (!(q4 %in% c(1, 2, 3))) {
      stop("Invalid input: The value of the fourth argument must be 1 (to indicate 'False Positive'), 2 (to indicate 'False Negative'), or 3 (to indicate 'No preference').")
    }
  }

  if (q1 == 2) {
    name <- "Disparate Impact"
    metric <- "di"
  } else if (q1 == 1) {
    if (q2 == 3) {
      name <- "Equalized Odds"
      metric <- "eo"
    } else {
      if (q2 == 1) {
        if (q3 == 1) {
          if (q4 == 1) {
            name <- "Predictive Rate Parity"
            metric <- "prp"
          } else if (q4 == 2) {
            name <- "Equal Opportunity"
            metric <- "eo"
          } else if (q4 == 3) {
            name <- "No measure with these characteristics is available in the Decision-Making Workflow if the Negative Class is Everything not positive"
            metric <- "error"
          } else {
            stop("Invalid imput:  The value of the fourth argument must be 1 (to indicate 'False Positive'), 2 (to indicate 'False Negative'), or 3 (to indicate 'No preference').")
          }
        } else if (q3 == 2) {
          if (q4 == 1) {
            name <- "Specificity Parity"
            metric <- "sp"
          } else if (q4 == 2) {
            name <- "Negative Predictive Rate Parity"
            metric <- "nprp"
          } else if (q4 == 3) {
            name <- "Accuracy Parity"
            metric <- "ap"
          } else {
            stop("Invalid imput:  The value of the fourth argument must be 1 (to indicate 'False Positive'), 2 (to indicate 'False Negative'), or 3 (to indicate 'No preference').")
          }
        } else {
          stop("Invalid input: The value of the third argument must be 1 (to indicate 'Everythig not positive') or 2 (to indicate Well-defined')")
        }
      } else if (q2 == 2) {
        if (q4 == 1) {
          name <- "False Positive Rate Parity"
          metric <- "fprp"
        } else if (q4 == 2) {
          name <- "False Negative Rate Parity"
          metric <- "fnrp"
        } else if (q4 == 3) {
          name <- "No measure with these characteristics is available in the Decision-Making Workflow if the interest is in the incorrect classification"
          metric <- "error"
        } else {
          stop("Invalid imput:  The value of the fourth argument must be 1 (to indicate 'False Positive'), 2 (to indicate 'False Negative'), or 3 (to indicate 'No preference').")
        }
      } else {
        stop("Invalid input: The value of the second argument must be 1 (to indicate 'Correct Classification'), 2 (to indicate Well-defined') or 3 (to indicate 'Correct and Incorrect Classification )")
      }
    }
  } else {
    stop("Invalid input: The value of the first argument must be 1 (to indicate 'Yes') or 2 (to indicate 'No')")
  }

  output <- list()
  output[["metric"]] <- metric
  output[["name"]] <- name
  # output[["qq"]]<- qq
  class(output) <- c(class(output), "jfaFairnessWorkflow")
  return(output)
}
