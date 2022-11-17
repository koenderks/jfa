# Copyright (C) 2020-2022 Koen Derks

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

#' BuildIt Construction Financial Statements
#'
#' Fictional data from a construction company in the United States, containing
#' 3500 observations identification numbers, book values, and audit values. The
#' audit values are added for illustrative purposes, as these would need to be
#' assessed by the auditor in the execution stage of the audit.
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
#' @references Derks, K., de Swart, J., Wagenmakers, E.-J., Wille, J., &
#' Wetzels, R. (2021). JASP for audit: Bayesian tools for the auditing practice.
#' \emph{Journal of Open Source Software}, \emph{6}(68), 2733.
#'
#' @examples
#' data(BuildIt)
"BuildIt"
