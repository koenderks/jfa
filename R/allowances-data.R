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

#' Legitimacy Audit
#'
#' Fictional population from a legitimacy audit, containing 4189 records with
#' identification numbers, stratum identifiers, book values, and audit values.
#' The audit values are added for illustrative purposes, as these would
#' need to be assessed by the auditor in the execution stage of the audit.
#'
#' @docType data
#'
#' @usage data(allowances)
#'
#' @format A data frame with 4189 rows and 5 variables.
#' \describe{
#'   \item{item}{a unique record identification number.}
#'   \item{branch}{the stratum identifier / branch number.}
#'   \item{bookValue}{the item book value in US dollars.}
#'   \item{auditValue}{the item audit (i.e., true) value in US dollars.}
#'   \item{times}{a sample selection indicator (\code{0} = not in sample).}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(allowances)
"allowances"
