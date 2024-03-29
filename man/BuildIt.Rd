% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BuildIt-data.R
\docType{data}
\name{BuildIt}
\alias{BuildIt}
\title{BuildIt Construction Financial Statements}
\format{
A data frame with 3500 rows and 3 variables.
\describe{
  \item{ID}{unique record identification number.}
  \item{bookValue}{book value in US dollars ($14.47--$2,224.40).}
  \item{auditValue}{true value in US dollars ($14.47--$2,224.40).}
}
}
\usage{
data(BuildIt)
}
\description{
Fictional data from a construction company in the United States, containing
3500 observations identification numbers, book values, and audit values. The
audit values are added for illustrative purposes, as these would need to be
assessed by the auditor in the execution stage of the audit.
}
\examples{
data(BuildIt)
}
\references{
Derks, K., de Swart, J., Wagenmakers, E.-J., Wille, J., &
Wetzels, R. (2021). JASP for audit: Bayesian tools for the auditing practice.
\emph{Journal of Open Source Software}, \emph{6}(68), 2733.
}
\keyword{datasets}
