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

#' Algorithm Auditing: Fairness Measures Decision-Making Workflow
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
#' of class \code{jfaFairnessWorkflow} that can be used with associated \code{print()} and \code{plot()} methods.
#'
#' @usage DecisionMakingWorkflow(
#'   q1 = NULL,
#'   q2 = NULL,
#'   q3 = NULL,
#'   q4 = NULL
#' )
#'
#' @param q1  a character indicating the answer to the first question of the decision-making workflow.
#'    If \code{NULL} (the default) the user is presented with the first question of the decision-making
#'    workflow and can respond interactively by selecting the numerical value corresponding to their
#'    desired answer. Possible options are \code{NULL} (default), \code{1} (to indicate 'Yes'), or
#'    \code{2} (to indicate 'No').
#' @param q2   a character indicating the answer to the second question of the decision-making workflow.
#'    If \code{NULL} (the default) the user is presented with the second question of the decision-making
#'    workflow and can respond interactively by selecting the numerical value corresponding to their
#'    desired answer. Possible options are \code{NULL} (default), \code{1} (to indicate 'Correct Classification'),
#'    \code{2} (to indicate 'Incorrect Classification'), or \code{3} (to indicate 'Correct and
#'    Incorrect Classification').
#' @param q3  a character indicating the answer to the third question of the decision-making workflow.
#'    If \code{NULL} (the default) the user is presented with the third question of the decision-making
#'    workflow and can respond interactively by selecting the numerical value corresponding to their
#'    desired answer. Possible options are \code{NULL} (default), \code{1} (to indicate 'Everythig not positive'),
#'    or \code{2} (to indicate 'Well-defined').
#' @param q4  a character indicating the answer to the fourth question of the decision-making workflow.
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
#' @return An object of class \code{jfaFairness} containing:
#'
#' \item{measure}{The abbreviation of the selected fairness measure. Possible options are \code{di},
#'                \code{eo}, \code{eodds}, \code{prp}, \code{eo}, \code{sp}, \code{nprp}, \code{ap},
#'                \code{fprp}, \code{fnrp} and \code{error}. }
#' \item{name}{The name of the selected fairness measure. Possible options are \code{Disparate Impact},
#'              \code{Equalized Odds}, \code{Predictive Rate Parity}, \code{Equal Opportunity}, \code{Specificity Parity},
#'              \code{Negative Predictive Rate Parity}, \code{Accuracy Parity}, \code{False Positive Rate Parity},
#'              \code{False Negative Rate Parity}, \code{No measure with these characteristics is available in the Decision-Making Workflow}.}
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
#' DecisionMakingWorkflow(
#'   q1 = "1",
#'   q2 = "1",
#'   q3 = "2",
#'   q4 = "3"
#' )
#' @export

fairness_measure_workflow <- function(q1 = NULL, q2 = NULL, q3 = NULL, q4 = NULL) {
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
      stop("Invalid input: The value of the second argument must be 1 (to indicate 'Correct Classification'), 2 (to indicate 'Incorrect Classification') or 3 (to indicate 'Correct and Incorrect Classification')")
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
    measure <- "di"
  } else if (q1 == 1) {
    if (q2 == 3) {
      name <- "Equalized Odds"
      measure <- "eodds"
    } else {
      if (q2 == 1) {
        if (q3 == 1) {
          if (q4 == 1) {
            name <- "Predictive Rate Parity"
            measure <- "prp"
          } else if (q4 == 2) {
            name <- "Equal Opportunity"
            measure <- "eo"
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
            measure <- "nprp"
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

  output <- list()
  output[["measure"]] <- measure
  output[["name"]] <- name
  class(output) <- c(class(output), "jfaFairnessWorkflow")
  return(output)
}
