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

#' Benchmark Analysis of Sales Versus Cost of Sales
#'
#' Fictional data from a benchmark analysis comparing industry sales versus
#' the industry cost of sales.
#'
#' @docType data
#'
#' @usage data(benchmark)
#'
#' @format A data frame with 100 rows and 2 variables.
#' \describe{
#'   \item{sales}{book value in US dollars ($100,187,432--$398,280,933).}
#'   \item{costofsales}{true value in US dollars ($71,193,639--$309,475,784).}
#' }
#'
#' @keywords datasets
#'
#' @references Derks, K., de Swart, J., van Batenburg, P., Wagenmakers, E.-J.,
#'   & Wetzels, R. (2021). Priors in a Bayesian audit: How integration of
#'   existing information into the prior distribution can improve audit
#'   transparency and efficiency. \emph{International Journal of Auditing},
#'   25(3), 621-636. \doi{10.1111/ijau.12240}
#'
#' @examples
#' data(benchmark)
"benchmark"
