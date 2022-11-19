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

#' Audit Sampling: Reporting
#'
#' @description \code{report()} takes an object of class \code{jfaEvaluation} as
#' returned by the \code{evaluation()} function automatically generates a
#' \code{html} or \code{pdf} report containing the most relevant statistical
#' results and their interpretation.
#'
#' @usage report(object, file = 'report.html',
#'        format = c('html_document', 'pdf_document'))
#'
#' @param object an object of class \code{jfaEvaluation} as returned by the
#'   \code{evaluation()} function.
#' @param file   a character specifying the name of the report (e.g.
#'   \code{report.html}).
#' @param format a character specifying the output format of the report.
#'   Possible options are \code{html_document} (default) and
#'   \code{pdf_document}, but compiling to \code{pdf} format requires a local
#'   version of \code{MikTex}.
#'
#' @return A \code{html} or \code{pdf} file containing the report.
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}}
#'          \code{\link{planning}}
#'          \code{\link{selection}}
#'          \code{\link{evaluation}}
#'
#' @keywords audit evaluation report
#'
#' @examples
#' data("BuildIt")
#'
#' # Draw a sample of 100 monetary units from the population using
#' # fixed interval monetary unit sampling
#' sample <- selection(
#'   data = BuildIt, size = 100, method = "interval",
#'   units = "values", values = "bookValue"
#' )$sample
#'
#' # Evaluate using the Stringer bound
#' result <- evaluation(
#'   conf.level = 0.95, materiality = 0.05, method = "stringer",
#'   data = sample, values = "bookValue", values.audit = "auditValue"
#' )
#' \dontrun{
#' report(result)
#' }
#'
#' @export

report <- function(object,
                   file = "report.html",
                   format = c("html_document", "pdf_document")) {
  valid_object <- inherits(object, "jfaEvaluation")
  stopifnot("'object' must be of class 'jfaEvaluation'" = valid_object)
  format <- match.arg(format)
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop('package \"rmarkdown\" needed for this function to work, please install it', call. = FALSE)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop('package \"knitr\" needed for this function to work, please install it', call. = FALSE)
  }
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop('package \"kableExtra\" needed for this function to work, please install it', call. = FALSE)
  }
  template_location <- system.file("rmd/report.Rmd", package = "jfa")
  args <- list()
  args$input <- template_location
  args$output_dir <- getwd()
  args$output_format <- format
  args$output_file <- file
  output_file <- do.call(.markdown_call("rmarkdown::render"), args = args)
  invisible(output_file)
}
