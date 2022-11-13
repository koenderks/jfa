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

#' Retailer data
#'
#' Sample outcomes per stratum from a retail company consisting of 20 branches.
#'
#' @docType data
#'
#' @usage data(retailer)
#'
#' @format A data frame with 20 rows and 5 variables.
#' \describe{
#'   \item{stratum}{branch/stratum number.}
#'   \item{items}{total number of items in each branch.}
#'   \item{samples}{number of items in sample per branch.}
#'   \item{errors}{number of errors in sample per branch.}
#' }
#'
#' @keywords datasets
#'
#' @source Derks, K., de Swart, J., & Wetzels, R. (2022). Een Bayesiaanse blik op gestratificeerde steekproeven heeft voordelen voor de auditor. \emph{Maandblad voor Accountancy en Bedrijfseconomie}, 96(1/2), 37-46. https://doi.org/10.5117/mab.96.78836
#'
#' @examples
#' data(retailer)
"retailer"
