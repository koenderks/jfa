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
#' @description This function aims to provide a fairness measure tailored to a
#' specific context and dataset by answering the questions in the developed
#' decision-making workflow. The questions within the decision-making workflow
#' are based on observable data characteristics, the properties of fairness
#' measures and the information required for their calculation. However, these
#' questions are posed to the user in an easily understandable manner, requiring
#' no statistical background or in-depth knowledge of the fairness measures.
#' Obtainable fairness measures include Disparate Impact, Equalized Odds,
#' Predictive Rate Parity, Equal Opportunity, Specificity Parity, Negative
#' Predictive Rate Parity, Accuracy Parity, False Positive Rate Parity and False
#' Negative Rate Parity. The function returns an object of class
#' \code{jfaFairnessSelection} that can be used with associated \code{print()}
#' and \code{plot()} methods.
#'
#' @usage fairness_selection(
#'   q1 = NULL,
#'   q2 = NULL,
#'   q3 = NULL,
#'   q4 = NULL
#' )
#'
#' @param q1 a character indicating the answer to the first question of the
#'   decision-making workflow ('Is the information on the true values of the
#'   classification relevant in your context?'). If \code{NULL} (the default)
#'   the user is presented with the first question of the decision-making
#    workflow and can respond interactively by selecting the numerical value
#'   corresponding to their desired answer. Possible options are \code{NULL}
#'   (default), \code{1} (to indicate 'Yes'), or \code{2} (to indicate 'No').
#' @param q2 a character indicating the answer to the second question of the
#'   decision-making workflow ('In what type of classification are you
#'   interested?'). If \code{NULL} (the default) the user is presented with the
#'   second question of the decision-making workflow and can respond
#'   interactively by selecting the numerical value corresponding to their
#'   desired answer. Possible options are \code{NULL} (default), \code{1} (to
#'   indicate 'Correct classification'), \code{2} (to indicate 'Incorrect
#'   classification'), or \code{3} (to indicate 'Correct and incorrect
#'   classification').
#' @param q3 a character indicating the answer to the third question of the
#'   decision-making workflow ('What is more important: a correct classification
#'  of the positive class, a correct classification of the negative class, or both?').
#'  If \code{NULL} (the default) the user is presented with the third
#'   question of the decision-making workflow and can respond interactively by
#'   selecting the numerical value corresponding to their desired answer.
#'   Possible options are  \code{NULL} (default), \code{1} (to indicate
#'   'Correct classificvation of the positive class'), \code{2} (to indicate
#'   'Correct classificvation of the negative class') or \code{3} (to indicate
#'   'Both a correct classification of the positive and of the negative class').
#' @param q4 a character indicating the answer to the fourth question of the
#'   decision-making workflow ('What are the errors with the highest cost?').
#'   If \code{NULL} (the default) the user is presented with the fourth
#'   question of the decision-making workflow and can respond interactively by
#'   selecting the numerical value corresponding to their desired answer.
#'   Possible options are \code{NULL} (default), \code{1} (to indicate 'False
#'   Positive') or \code{2} (to indicate 'False Negative').
#'
#' @details Several fairness measures can be used to assess the fairness of
#'   AI-predicted classifications. These include:
#'
#'   \itemize{
#'    \item{Disparate Impact. See Friedler et al. (2019), Feldman et al.
#'     (2015), Castelnovo et al. (2022) for a more detailed explanation of this
#'     measure.}
#'    \item{Equalized Odds. See Hardt et al. (2016), Verma et al. (2018) and
#'     Picogna et al. (2025) for a more detailed explanation of this measure.}
#'    \item{False Positive Rate Parity. See Castelnovo et al. (2022) (under the
#'     name Predictive Equality), Verma et al. (2018) and Picogna et al. (2025)
#'     for a more detailed explanation of this measure.}
#'    \item{False Negative Rate Parity. See Castelnovo et al. (2022) (under the
#'     name Equality of Opportunity), Verma et al. (2018) and and Picogna et al.
#'     (2025) for a more detailed explanation of this measure.}
#'    \item{Predictive Rate Parity. See Castelnovo et al. (2022) (under the
#'     name Predictive Parity) and Picogna et al. (2025) for a more detailed
#'     explanation of this measure.}
#'    \item{Equal Opportunity. See Hardt et al. (2016), Friedler et al. (2019),
#'     Verma et al. (2018) and Picogna et al. (2025) for a more detailed
#'     explanation of this measure.}
#'    \item{Specificity Parity. See Friedler et al. (2019), Verma et al. (2018)
#'     and Picogna et al. (2025) for a more detailed explanation of this
#'     measure.}
#'    \item{Negative Predictive Value Parity. See Verma et al. (2018) and
#'     Picogna et al. (2025) for a more detailed explanation of this measure.}
#'    \item{Accuracy Parity. See Friedler et al. (2019) and Picogna et al.
#'     (2025) for a more detailed explanation of this measure.}
#'   }
#'
#'   The fairness decision-making workflow below aids in choosing which fairness
#'   measure is appropriate for the situation at hand (Picogna et al., 2025).
#'
#'   \if{html}{\figure{fairness-tree.png}{options: style="width: 100\%;" alt="fairness-tree"}}
#'   \if{latex}{\figure{fairness-tree.pdf}{options: width=5in}}
#'
#' @return An object of class \code{jfaFairnessSelection} containing:
#'
#' \item{measure}{The abbreviation for the selected fairness measure's name.}
#' \item{name}{The name of the selected fairness measure.}
#'
#' @author Federica Picogna, \email{f.picogna@nyenrode.nl}
#'
#' @references Castelnovo, A., Crupi, R., Greco, G. et al. (2022). A clarification
#'   of the nuances in the fairness metrics landscape. In \emph{Sci Rep 12,
#'   4209}. \doi{10.1038/s41598-022-07939-1}
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
#'   supervised learning. In \emph{Advances in neural information processing
#'   systems, 29}. \doi{10.48550/arXiv.1610.02413}
#' @references Picogna, F., de Swart, J., Kaya, H., & Wetzels, R. (2025). How to
#'   choose a fairness measure: A decision-making workflow for auditors.
#'   \doi{10.31219/osf.io/cpxmf_v1}
#' @references Verma S., Rubin J. (2018). Fairness definitions explained. In
#'   \emph{Proceedings of the international workshop on software fairness,
#'   1--7}. \doi{10.1145/3194770.3194776}
#'
#' @keywords algorithm audit bias fairness workflow
#'
#' @examples
#' # Workflow leading to predictive rate parity
#' fairness_selection(q1 = 1, q2 = 1, q3 = 1, q4 = 1)
#' @export

fairness_selection <- function(q1 = NULL,
                               q2 = NULL,
                               q3 = NULL,
                               q4 = NULL) {
  if (is.null(q1)) {
    stopifnot("Function must be run in an interactive environment" = interactive())
    q1 <- utils::menu(choices = c("Yes", "No"), title = "(q1) Is the information on the true values of the classification relevant in your context?")
  } else {
    stopifnot("Invalid input: The value of `q1` must be 1 (to indicate 'Yes') or 2 (to indicate 'No')" = q1 %in% c(1, 2))
  }
  q2_name <- q3_name <- q4_name <- NULL
  if (q1 == 1) {
    q1_name <- "Yes"
    if (is.null(q2)) {
      stopifnot("Function must be run in an interactive environment" = interactive())
      q2 <- utils::menu(choices = c("Correct classification", "Incorrect classification", "Correct and incorrect classification"), title = "(q2) In what type of classification are you interested?")
    } else {
      stopifnot("Invalid input: The value of `q2` must be 1 (to indicate 'Correct classification'), 2 (to indicate 'Incorrect classification') or 3 (to indicate 'Correct and incorrect classification')" = q2 %in% c(1, 2, 3))
    }
    if (q2 == 1) {
      q2_name <- "Correct classification"
      if (is.null(q3)) {
        stopifnot("Function must be run in an interactive environment" = interactive())
        q3 <- utils::menu(choices = c("Correct classification of the positive class", "Correct classification of the negative class", "Both a correct classification of the positive and of the negative class"), title = "(q3) What is more important: a correct classification of the positive class, a correct classification of the negative class, or both?")
      } else {
        stopifnot("Invalid input: The value of `q3` must be 1 (to indicate 'Correct classification of the positive class'), 2 (to indicate 'Correct classification of the negative class') or 3 (to indicate 'Both a correct classification of the positive and of the negative class)" = q3 %in% c(1, 2, 3))
      }
      if (q3 == 1) {
        q3_name <- "Correct classification of the positive class"
        if (is.null(q4)) {
          choices <- c("False Positive", "False Negative")
          stopifnot("Function must be run in an interactive environment" = interactive())
          q4 <- utils::menu(choices = choices, title = "(q4) What are the errors with the highest cost?")
        } else {
          stopifnot("Invalid input: The value of `q4` must be 1 (to indicate 'False Positive') or 2 (to indicate 'False Negative')" = q4 %in% c(1, 2))
        }
        if (q4 == 1) {
          name <- "Predictive Rate Parity"
          measure <- "prp"
          q4_name <- "False Positive"
        } else {
          name <- "Equal Opportunity"
          measure <- "tprp"
          q4_name <- "False Negative"
        }
      } else if (q3 == 2) {
        q3_name <- "Correct classification of the negative class"
        if (is.null(q4)) {
          choices <- c("False Positive", "False Negative")
          stopifnot("Function must be run in an interactive environment" = interactive())
          q4 <- utils::menu(choices = choices, title = "(q4) What are the errors with the highest cost?")
        } else {
          stopifnot("Invalid input: The value of `q4` must be 1 (to indicate 'False Positive') or 2 (to indicate 'False Negative')" = q4 %in% c(1, 2))
        }
        if (q4 == 1) {
          name <- "Specificity Parity"
          measure <- "sp"
          q4_name <- "False Positive"
        } else {
          name <- "Negative Predictive Value Parity"
          measure <- "npvp"
          q4_name <- "False Negative"
        }
      } else if (q3 == 3) {
        q3_name <- "Correct classification of the positive class and of the negative class"
        name <- "Accuracy Parity"
        measure <- "ap"
        if (!is.null(q4)) {
          warning("The value assigned to 'q4' is not relevant in this path of the decision-making workflow and will be ignored.")
        }
      }
    } else if (q2 == 2) {
      q2_name <- "Incorrect classification"
      if (is.null(q4)) {
        stopifnot("Function must be run in an interactive environment" = interactive())
        q4 <- utils::menu(choices = c("False Positive", "False Negative"), title = "(q4) What are the errors with the highest cost?")
      } else {
        stopifnot("Invalid input: The value of `q4` must be 1 (to indicate 'False Positive') or 2 (to indicate 'False Negative')" = q4 %in% c(1, 2))
      }
      if (!is.null(q3)) {
        warning("The value assigned to 'q3' is not relevant in this path of the decision-making workflow and will be ignored.")
      }
      if (q4 == 1) {
        name <- "False Positive Rate Parity"
        measure <- "fprp"
        q4_name <- "False Positive"
      } else if (q4 == 2) {
        name <- "False Negative Rate Parity"
        measure <- "fnrp"
        q4_name <- "False Negative"
      }
    } else if (q2 == 3) {
      name <- "Equalized Odds"
      measure <- "eo"
      q2_name <- "Correct and incorrect classification"
      if (!is.null(q3) || !is.null(q4)) {
        warning("The values assigned to 'q3 and 'q4' are not relevant in this path of the decision-making workflow and will be ignored.")
      }
    }
  } else if (q1 == 2) {
    name <- "Disparate Impact"
    measure <- "pp"
    q1_name <- "No"
    if (!is.null(q2) || !is.null(q3) || !is.null(q4)) {
      warning("The values assigned to 'q2, 'q3 and 'q4' are not relevant in this path of the decision-making workflow and will be ignored.")
    }
  }
  output <- list()
  output[["measure"]] <- measure
  output[["name"]] <- name
  output[["q1"]] <- list(value = q1, name = q1_name)
  if (!is.null(q2_name)) {
    output[["q2"]] <- list(value = q2, name = q2_name)
  }
  if (!is.null(q3_name)) {
    output[["q3"]] <- list(value = q3, name = q3_name)
  }
  if (!is.null(q4_name)) {
    output[["q4"]] <- list(value = q4, name = q4_name)
  }
  class(output) <- c("jfaFairnessSelection", "list")
  return(output)
}
