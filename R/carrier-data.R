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

#' Carrier Company Financial Statements
#'
#' Fictional data from a carrier company in Europe, containing 202 ledger items across 10 company entities.
#'
#' @docType data
#'
#' @usage data(carrier)
#'
#' @format A data frame with 202 rows and 12 variables.
#' \describe{
#'   \item{description}{description of the ledger item.}
#'   \item{entity1}{recorded values for entity 1, in US dollars.}
#'   \item{entity2}{recorded values for entity 2, in US dollars.}
#'   \item{entity3}{recorded values for entity 3, in US dollars.}
#'   \item{entity4}{recorded values for entity 4, in US dollars.}
#'   \item{entity5}{recorded values for entity 5, in US dollars.}
#'   \item{entity6}{recorded values for entity 6, in US dollars.}
#'   \item{entity7}{recorded values for entity 7, in US dollars.}
#'   \item{entity8}{recorded values for entity 8, in US dollars.}
#'   \item{entity9}{recorded values for entity 9, in US dollars.}
#'   \item{entity10}{recorded values for entity 10, in US dollars.}
#'   \item{total}{total value, in US dollars.}
#' }
#'
#' @keywords datasets
#'
#' @source \url{https://towardsdatascience.com/data-driven-audit-1-automated-sampling-using-python-52e83347add5}
#'
#' @examples
#' data(carrier)
"carrier"
