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

#' Algorithm Auditing: Fairness Measures Selection
#'
#' @description This function aims to provide a fairness measure tailored
#' to a specific context and dataset by answering the questions in the developed
#' decision-making workflow. The questions within the decision-making workflow are based
#' on observable data characteristics, the properties of fairness measures and the
#' information required for their calculation. However, these questions are posed to the user
#' in an easily understandable manner, requiring no statistical background or in-depth knowledge of the fairness measures.
#' Obtainable fairness measures include Disparate Impact, Equalized Odds, Predictive Rate Parity,
#' Equal Opportunity, Specificity Parity, Negative Predictive Rate Parity, Accuracy Parity,
#' False Positive Rate Parity and False Negative Rate Parity. The function returns an object
#' of class \code{jfaFairnessSelection} that can be used with associated \code{print()} and \code{plot()} methods.
#'
#' @usage fairness_selection(
#'   q1 = NULL,
#'   q2 = NULL,
#'   q3 = NULL,
#'   q4 = NULL
#' )
#'
#' @param q1  a character indicating the answer to the first question of the decision-making workflow ('Do you
#'            have the information on the true values of the classification?').
#'    If \code{NULL} (the default) the user is presented with the first question of the decision-making
#'    workflow and can respond interactively by selecting the numerical value corresponding to their
#'    desired answer. Possible options are \code{NULL} (default), \code{1} (to indicate 'Yes'), or
#'    \code{2} (to indicate 'No').
#' @param q2   a character indicating the answer to the second question of the decision-making workflow ('In what type
#'              of classification are you interested?').
#'    If \code{NULL} (the default) the user is presented with the second question of the decision-making
#'    workflow and can respond interactively by selecting the numerical value corresponding to their
#'    desired answer. Possible options are \code{NULL} (default), \code{1} (to indicate 'Correct Classification'),
#'    \code{2} (to indicate 'Incorrect Classification'), or \code{3} (to indicate 'Correct and
#'    Incorrect Classification').
#' @param q3  a character indicating the answer to the third question of the decision-making workflow ('Can the negative class
#'            be considered as everything not positive or is it well defined?').
#'    If \code{NULL} (the default) the user is presented with the third question of the decision-making
#'    workflow and can respond interactively by selecting the numerical value corresponding to their
#'    desired answer. Possible options are \code{NULL} (default), \code{1} (to indicate 'Everythig not positive'),
#'    or \code{2} (to indicate 'Well-defined'). To understand the concept of a negative class defined as Everything Not Positive,
#'      consider the example of evaluating customer satisfaction for a service, where customers can either be satisfied or dissatisfied.
#'      Dissatisfaction, however, can stem from various reasons (such as 'not satisfied due to shipping delays', 'not satisfied because the
#'      product or its features are malfunctioning', or 'not satisfied due to unhelpful customer service'). Despite the different reasons,
#'     they all fall under the category of 'not satisfied' and can be grouped together without further distinction. On the other hand,
#'      to understand the concept of a negative class as Well Defined, consider the example of classifying bank transactions as either fraudulent or legitimate.
#'        In this case, the negative class simply refers to all fraudulent transactions, without needing to analyze why a transaction is considered non-legitimate (i.e. fraudulent).
#' @param q4  a character indicating the answer to the fourth question of the decision-making workflow ('What are the
#'          errors with the highest cost?').
#'    If \code{NULL} (the default) the user is presented with the fourth question of the decision-making
#'    workflow and can respond interactively by selecting the numerical value corresponding to their
#'    desired answer. Possible options are \code{NULL} (default), \code{1} (to indicate 'False Positive'),
#'    \code{2} (to indicate 'False Negative'), or \code{3} (to indicate 'No preference').
#'
#' @details Several fairness measures can be used to assess the fairness of AI-predicted classifications. These include:
#'
#'   \itemize{
#'     \item{Disparate Impact. See Friedler et al. (2019), Feldman et al. (2015),
#'          Castelnovo et al. (2022) and and Büyük, S. (2023) for a more detailed explanation of this measure.}
#'     \item{Equalized Odds. See Hardt et al. (2016), Verma et al. (2018) and Büyük, S. (2023)
#'            for a more detailed explanation of this measure.}
#'     \item{False Positive Rate Parity. See Castelnovo et al. (2022) (under the name Predictive Equality),
#'            Verma et al. (2018) and Büyük, S. (2023) for a more detailed explanation of this measure.}
#'     \item{False Negative Rate Parity. See Castelnovo et al. (2022) (under the name Equality of Opportunity),
#'            Verma et al. (2018) and Büyük, S. (2023) for a more detailed explanation of this measure.}
#'     \item{Predictive Rate Parity. See Castelnovo et al. (2022) (under the name Predictive Parity) and Büyük, S. (2023) for
#'            a more detailed explanation of this measure.}
#'     \item{Equal Opportunity. See Hardt et al. (2016), Friedler et al. (2019), Verma et al. (2018) and Büyük, S. (2023) for
#'            a more detailed explanation of this measure.}
#'     \item{Specificity Parity. See Friedler et al. (2019), Verma et al. (2018) and Büyük, S. (2023) for
#'            a more detailed explanation of this measure.}
#'     \item{Negative Predictive Rate Parity. See Verma et al. (2018) and Büyük, S. (2023) for a more detailed explanation of this measure.}
#'     \item{Accuracy Parity. See Friedler et al. (2019) and Büyük, S. (2023) for a more detailed explanation of this measure.}
#'   }
#'
#' The fairness decision-making workflow below aids in choosing
#'   which fairness measure is appropriate for the situation at hand. It is important to note that
#'   there are three specific input parameter combinations ((q1="1", q2="2", q3="1", q4="3"),
#'   (q1="1", q2="2", q3="2", q4="3") and (q1="1", q2="1", q3="1", q4="3")) for which a fairness measure is currently
#'   unavailable as outcome that meet the required input criteria.
#'
#'   \if{html}{\figure{fairness-tree.png}{options: width="100\%" alt="fairness-tree"}}
#'   \if{latex}{\figure{fairness-tree.pdf}{options: width=5in}}
#'
#' @return An object of class \code{jfaFairnessSelection} containing:
#'
#' \item{measure}{The abbreviation of the selected fairness measure.}
#' \item{name}{The name of the selected fairness measure.}
#'
#' @author Federica Picogna, \email{f.picogna@nyenrode.nl}
#'
#' @references Büyük, S. (2023). \emph{Automatic Fairness Criteria and Fair
#'   Model Selection for Critical ML Tasks}, Master Thesis, Utrecht University.
#' @references Castelnovo, A., Crupi, R., Greco, G. et al. (2022). A clarification
#' of the nuances in the fairness metrics landscape. In \emph{Sci Rep 12, 4209}.
#' \doi{10.1038/s41598-022-07939-1}
#' @references Feldman, M., Friedler, S. A., Moeller, J., Scheidegger, C., &
#'   Venkatasubramanian, S. (2015). Certifying and removing disparate impact. In
#'   \emph{Proceedings of the 21th ACM SIGKDD International Conference on
#'   Knowledge Discovery and Data Mining}. \doi{10.1145/2783258.2783311}
#' @references Friedler, S. A., Scheidegger, C., Venkatasubramanian, S.,
#'   Choudhary, S., Hamilton, E. P., & Roth, D. (2019). A comparative study of
#'   fairness-enhancing interventions in machine learning. In \emph{Proceedings
#'   of the Conference on Fairness, Accountability, and Transparency}.
#'   \doi{10.1145/3287560.3287589}
#' @references Hardt M. , Price E., Srebro N. (2016). Equality of opportunity in
#'    supervised learning. In \emph{Advances in neural information processing systems, 29}.
#'    \doi{10.48550/arXiv.1610.02413}
#' @references Verma S., Rubin J. (2018). Fairness definitions explained. In \emph{Proceedings
#'     of the international workshop on software fairness, 1--7}. \doi{10.1145/3194770.3194776}
#'
#' @keywords algorithm audit bias fairness workflow
#'
#' @examples
#' # Combination to obtain Accuracy Parity
#' fairness_selection(
#'   q1 = "1",
#'   q2 = "1",
#'   q3 = "2",
#'   q4 = "3"
#' )
#' @export

fairness_selection <- function(q1 = NULL, q2 = NULL, q3 = NULL, q4 = NULL) {
  if (is.null(q1)) {
    q1 <- utils::menu(choices = c("Yes", "No"), title = "Do you have information on the true values of the classification?")
  } else if (!(q1 %in% c(1, 2))) {
    stop("Invalid input: The value of the first argument must be 1 (to indicate 'Yes') or 2 (to indicate 'No')")
  }

  if (q1 == 1) {
    if (is.null(q2)) {
      q2 <- utils::menu(choices = c("Correct Classification", "Incorrect Classification", "Correct and Incorrect Classification"), title = "In what type of classification are you interested?")
    } else if (!(q2 %in% c(1, 2, 3))) {
      stop("Invalid input: The value of the second argument must be 1 (to indicate 'Correct Classification'), 2 (to indicate 'Incorrect Classification') or 3 (to indicate 'Correct and Incorrect Classification')")
    }
    if (q2 == 1) {
      if (is.null(q3)) {
        q3 <- utils::menu(choices = c("Everything not positive", "Well-defined"), title = "Can the negative class be considered as everything not positive or is it well-defined?")
      } else if (!(q3 %in% c(1, 2))) {
        stop("Invalid input: The value of the third argument must be 1 (to indicate 'Everythig not positive') or 2 (to indicate 'Well-defined')")
      }
    }

    if (!(q1 == 3)) {
      if (is.null(q4)) {
        q4 <- utils::menu(choices = c("False Positive", "False Negative", "No preference"), title = "What are the errors with the highest cost?")
      } else if (!(q4 %in% c(1, 2, 3))) {
        stop("Invalid input: The value of the fourth argument must be 1 (to indicate 'False Positive'), 2 (to indicate 'False Negative'), or 3 (to indicate 'No preference').")
      }
    }
  }

  if (q1 == 2) {
    name <- "Disparate Impact"
    measure <- "pp"
  } else if (q1 == 1) {
    if (q2 == 3) {
      name <- "Equalized Odds"
      measure <- "dp"
    } else {
      if (q2 == 1) {
        if (q3 == 1) {
          if (q4 == 1) {
            name <- "Predictive Rate Parity"
            measure <- "prp"
          } else if (q4 == 2) {
            name <- "Equal Opportunity"
            measure <- "tprp"
          } else if (q4 == 3) {
            name <- "No measure with these characteristics is available in the Decision-Making Workflow"
            measure <- "error"
          } else {
            stop("Invalid imput:  The value of the fourth argument must be 1 (to indicate 'False Positive'), 2 (to indicate 'False Negative'), or 3 (to indicate 'No preference').")
          }
        } else if (q3 == 2) {
          if (q4 == 1) {
            name <- "Specificity Parity"
            measure <- "sp"
          } else if (q4 == 2) {
            name <- "Negative Predictive Rate Parity"
            measure <- "npvp"
          } else if (q4 == 3) {
            name <- "Accuracy Parity"
            measure <- "ap"
          } else {
            stop("Invalid imput:  The value of the fourth argument must be 1 (to indicate 'False Positive'), 2 (to indicate 'False Negative'), or 3 (to indicate 'No preference').")
          }
        } else {
          stop("Invalid input: The value of the third argument must be 1 (to indicate 'Everythig not positive') or 2 (to indicate Well-defined')")
        }
      } else if (q2 == 2) {
        if (q4 == 1) {
          name <- "False Positive Rate Parity"
          measure <- "fprp"
        } else if (q4 == 2) {
          name <- "False Negative Rate Parity"
          measure <- "fnrp"
        } else if (q4 == 3) {
          name <- "No measure with these characteristics is available in the Decision-Making Workflow"
          measure <- "error"
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

  q1_name <- q2_name <- q3_name <- q4_name <- NULL

  if (q1 == 1) {
    q1_name <- "Yes"
  } else {
    q1_name <- "No"
  }

  if (q2 == 1) {
    q2_name <- "Correct Classification"
  } else if (q2 == 2) {
    q2_name <- "Incorrect Classification"
  } else {
    q2_name <- "Correct and Incorrect Classification"
  }

  if (q3 == 1) {
    q3_name <- "Everything Not Positive"
  } else {
    q3_name <- "Well-Defined"
  }

  if (q4 == 1) {
    q4_name <- "False Positive"
  } else if (q4 == 2) {
    q4_name <- "False Negative"
  } else if (q4 == 3) {
    q4_name <- "No Preference"
  }

  output <- list()
  output[["measure"]] <- measure
  output[["name"]] <- name
  output[["q1"]] <- q1
  output[["q1_name"]] <- q1_name
  output[["q2"]] <- q2
  output[["q2_name"]] <- q2_name
  output[["q3"]] <- q3
  output[["q3_name"]] <- q3_name
  output[["q4"]] <- q4
  output[["q4_name"]] <- q4_name
  class(output) <- c(class(output), "jfaFairnessSelection")
  return(output)
}
