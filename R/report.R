#' Create a statistical audit sampling report
#'
#' @description This function takes an object of class \code{jfaEvaluation} as returned by the \code{evaluation()} function automatically generates a \code{html} or \code{pdf} report containing the analysis results and their interpretation.
#'
#' For more details on how to use this function, see the package vignette:
#' \code{vignette('jfa', package = 'jfa')}
#'
#' @usage report(object, file = 'report.html', format = 'html_document')
#'
#' @param object an object of class \code{jfaEvaluation} as returned by the \code{evaluation()} function.
#' @param file a character specifying the name of the report (e.g. \code{report.html}). By default, the report is created in your current working directory.          
#' @param format a character specifying the output format of the report. Possible options are \code{html_document} (default) and \code{pdf_document}, but compiling to \code{pdf} format requires a local version of MikTex.
#'
#' @return A \code{html} or \code{pdf} file containing a report of the evaluation.
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @seealso \code{\link{auditPrior}} \code{\link{planning}} \code{\link{selection}} \code{\link{evaluation}}
#'
#' @keywords evaluation report audit
#'
#' @examples
#' data('BuildIt')
#'
#' # Draw a sample of 100 monetary units from the population using
#' # fixed interval monetary unit sampling
#' sample <- selection(population = BuildIt, sampleSize = 100, 
#'           algorithm = 'interval', units = 'mus', bookValues = 'bookValue')$sample
#' 
#' # Evaluate using the Stringer bound
#' result <- evaluation(confidence = 0.95, materiality = 0.05, 
#'                      method = 'stringer', sample = sample, 
#'                      bookValues = 'bookValue', auditValues = 'auditValue')
#'
#' \dontrun{ 
#'  report(result) 
#' }
#'
#' @export

report <- function(object, file = 'report.html', format = 'html_document'){
  
  if (!class(object) == 'jfaEvaluation')
    stop("Object must be of class 'jfaEvaluation'.")
  
  if (!requireNamespace('rmarkdown', quietly = TRUE))
    stop('Package \"rmarkdown\" needed for this function to work. Please install it.', call. = FALSE)
  
  if (!requireNamespace('knitr', quietly = TRUE))
    stop('Package \"knitr\" needed for this function to work. Please install it.', call. = FALSE)
  
  if (!requireNamespace('kableExtra', quietly = TRUE))
    stop('Package \"kableExtra\" needed for this function to work. Please install it.', call. = FALSE)
  
  # Determine the template
  theFile <- system.file('rmd/report.Rmd', package = 'jfa')
  
  # Process the function arguments
  args               <- list()
  args$input         <- theFile
  args$output_dir    <- getwd()
  args$output_format <- format
  args$output_file   <- file
  
  # Start the renderer via rmarkdown
  outputFileName <- do.call(.getfun('rmarkdown::render'), args = args)
  invisible(outputFileName)
}