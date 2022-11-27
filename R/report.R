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
#' @usage report(object,
#'        file = "report.html",
#'        format = c("html_document", "pdf_document"))
#'
#' @param object an object of class \code{jfaEvaluation} as returned by the
#'   \code{evaluation()} function.
#' @param file   a character specifying the name and format of the report (e.g.
#'   \code{report.html}).
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
#' result <- evaluation(x = 0, n = 100)
#' \dontrun{
#' report(result)
#' }
#' @export

report <- function(object,
                   file = "report.html") {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop('package \"rmarkdown\" needed for this function to work, please install it', call. = FALSE)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop('package \"knitr\" needed for this function to work, please install it', call. = FALSE)
  }
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop('package \"kableExtra\" needed for this function to work, please install it', call. = FALSE)
  }
  if (inherits(object, "jfaEvaluation")) {
    name <- "report_evaluation"
  } else {
    stop("'object' must be of class 'jfaEvaluation'")
  }
  args <- list()
  args$input <- system.file(paste0("rmd/", name, ".Rmd"), package = "jfa")
  args$output_dir <- getwd()
  if (is.null(file)) {
    stop("'file' should have a .html or .pdf extention")
  } else if (grepl(pattern = ".html", x = file, fixed = TRUE)) {
    args$output_format <- "html_document"
  } else if (grepl(pattern = ".pdf", x = file, fixed = TRUE)) {
    args$output_format <- "pdf_document"
  } else {
    stop("'file' should have a .html or .pdf extention")
  }
  args$output_file <- file
  output_file <- do.call(.markdown_call("rmarkdown::render"), args = args)
  invisible(output_file)
}
