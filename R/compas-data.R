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

#' COMPAS Recidivism Prediction
#'
#' This data was used to predict recidivism (whether a criminal will reoffend or
#' not) in the USA.
#'
#' @docType data
#'
#' @usage data(compas)
#'
#' @format A data frame with 100 rows and 2 variables.
#' \describe{
#'   \item{TwoYrRecidivism}{yes/no for recidivism or no recidivism.}
#'   \item{AgeAboveFoutryFive}{yes/no for age above 45 years or not}
#'    \item{AgeBelowTwentyFive}{yes/no for age below 25 years or not}
#'    \item{Gender}{female/male for gender}
#'    \item{Misdemeanor}{yes/no for having recorded misdemeanor(s) or not}
#'    \item{Ethnicity}{Caucasian, African American, Asian, Hispanic, Native American or Other}
#'    \item{Predicted}{yes/no, predicted values for recidivism}
#' }
#'
#' @keywords datasets
#'
#' @references \url{https://www.kaggle.com/danofer/compass}
#'             \url{https://cran.r-project.org/package=fairness}
#'
#' @examples
#' data(compas)
"compas"
