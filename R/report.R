#' Generate an Audit Report
#'
#' @description This function takes an object of class \code{jfaEvaluation}, creates a report containing the results, and saves the report to a file in your working directory.
#'
#' For more details on how to use this function see the package vignette:
#' \code{vignette("jfa", package = "jfa")}
#'
#' @usage report(object = NULL, file = NULL, format = "html_document")
#'
#' @param object an object of class 'jfaEvaluation' as returned by the \code{evaluation()} function.
#' @param file a string that gives the desired name of the file (e.g. \code{"report.html"}). The report is created in your current working directory.          
#' @param format can be either one of \code{"html_document"} or \code{"pdf_document"} (required MikTex).
#'
#' @return A html or pdf report containing the results of the evaluation.
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{evaluation}}
#'
#' @examples
#' library(jfa)
#' set.seed(1)
#' 
#' # Generate some audit data (N = 1000):
#' data <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), 
#'                    bookValue = runif(n = 1000, min = 700, max = 1000))
#' 
#' # Using monetary unit sampling, draw a random sample from the population.
#' s1 <- selection(population = data, sampleSize = 100, units = "mus", 
#'                  bookValues = "bookValue", algorithm = "random")
#' s1_sample <- s1$sample
#' s1_sample$trueValue <- s1_sample$bookValue
#' s1_sample$trueValue[2] <- s1_sample$trueValue[2] - 500 # One overstatement is found
#'
#' e2 <- evaluation(sample = s1_sample, bookValues = "bookValue", auditValues = "trueValue", 
#'                  method = "stringer", materiality = 0.05, counts = s1_sample$counts)
#'
#' # Generate report
#' # report(e2, file = "myFile.html")
#'
#' @keywords evaluation report audit
#'
#' @export

report <- function(object = NULL, file = NULL, format = "html_document"){

  if(!class(object) == "jfaEvaluation")
    stop("Object must be of class 'jfaEvaluation'.")

  #Determine the template
  theFile <- system.file("rmd/report.Rmd", package = "jfa")

  #Process the Arguments
  args               <- list()
  args$input         <- theFile
  args$output_dir    <- getwd()
  args$output_format <- format
  args$output_file   <- file

  #Run the render
  outputFileName <- do.call(.getfun('rmarkdown::render'), args = args)
  invisible(outputFileName)
}