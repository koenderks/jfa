#' BuildIt Construction financial statements
#'
#' Fictional data from a construction company in the United States, containing 3500 observations identification numbers, book values, and audit values. The audit values are added for illustrative purposes, as these would need to be assessed by the auditor in the execution stage of the audit.
#'
#' @docType data
#'
#' @usage data(BuildIt)
#'
#' @format A data frame with 3500 rows and 3 variables.
#' \describe{
#'   \item{ID}{unique record identification number.}
#'   \item{bookValue}{book value in US dollars ($14.47--$2,224.40).}
#'   \item{auditValue}{true value in US dollars ($14.47--$2,224.40).}
#' }
#'
#' @keywords datasets
#'
#' @references Derks, K., de Swart, J., Wagenmakers, E.-J., Wille, J., & Wetzels, R. (2019). JASP for audit: Bayesian tools for the auditing practice.
#'
#' @examples
#' data(BuildIt)
"BuildIt"
