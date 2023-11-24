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

#' Accounts Receivable
#'
#' Audit sample obtained from a population of N = 87 accounts receivable,
#' totaling $612,824 in book value (Higgins and Nandram, 2009; Lohr, 2021).
#'
#' @docType data
#'
#' @usage data(accounts)
#'
#' @format A data frame with 20 rows and 3 variables.
#' \describe{
#'   \item{account}{account number (between 1 - 87)}
#'   \item{bookValue}{booked value of the account}
#'   \item{auditValue}{audited (true) value of the account}
#' }
#'
#' @keywords datasets
#'
#' @references Higgins, H. N., & Nandram, B. (2009). Monetary unit sampling:
#'   Improving estimation of the total audit error
#'   \emph{Advances in Accounting}, 25(2), 174-182.
#'   \doi{10.1016/j.adiac.2009.06.001}
#' @references Lohr, S. L. (2021). \emph{Sampling: Design and Analysis}. CRC
#'   press.
#'
#' @examples
#' data(accounts)
"accounts"
