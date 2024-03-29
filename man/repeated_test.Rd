% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repeated_test.R
\name{repeated_test}
\alias{repeated_test}
\title{Data Auditing: Repeated Values Test}
\usage{
repeated_test(
  x,
  check = c("last", "lasttwo", "all"),
  method = c("af", "entropy"),
  samples = 2000
)
}
\arguments{
\item{x}{a numeric vector of values from which the digits should be
analyzed.}

\item{check}{which digits to shuffle during the procedure. Can be
\code{last} or \code{lasttwo}.}

\item{method}{which statistics is used. Defaults to \code{af} for average
frequency, but can also be \code{entropy} for entropy.}

\item{samples}{how many samples to use in the bootstraping procedure.}
}
\value{
An object of class \code{jfaRv} containing:

\item{x}{input data.}
\item{frequencies}{frequencies of observations in \code{x}.}
\item{samples}{vector of simulated samples.}
\item{integers}{counts for extracted integers.}
\item{decimals}{counts for extracted decimals.}
\item{n}{the number of observations in \code{x}.}
\item{statistic}{the value the average frequency or entropy statistic.}
\item{p.value}{the p-value for the test.}
\item{cor.test}{correlation test for the integer portions of the number
  versus the decimals portions of the number.}
\item{method}{method used.}
\item{check}{checked digits.}
\item{data.name}{a character string giving the name(s) of the data.}
}
\description{
This function analyzes the frequency with which values get
repeated within a set of numbers. Unlike Benford's law, and its
generalizations, this approach examines the entire number at once, not only
the first or last digit(s).
}
\details{
To determine whether the data show an excessive amount of bunching,
  the null hypothesis that \code{x} does not contain an unexpected amount of
  repeated values is tested against the alternative hypothesis that \code{x}
  has more repeated values than expected. The statistic can either be the
  average frequency (\eqn{AF = sum(f_i^2)/sum(f_i))} of the data or the
  entropy (\eqn{E = - sum(p_i * log(p_i))}, with \eqn{p_i=f_i/n}) of the
  data. Average frequency and entropy are highly correlated, but the average
  frequency is often more interpretable. For example, an average frequency of
  2.5 means that, on average, your observations contain a value that appears
  2.5 times in the data set.To quantify what is expected, this test requires
  the assumption that the integer portions of the numbers are not associated
  with their decimal portions.
}
\examples{
set.seed(1)
x <- rnorm(50)

# Repeated values analysis shuffling last digit
repeated_test(x, check = "last", method = "af", samples = 2000)
}
\references{
Simohnsohn, U. (2019, May 25). Number-Bunching: A New Tool for
  Forensic Data Analysis. Retrieved from \url{https://datacolada.org/77}.
}
\seealso{
\code{\link{digit_test}}
}
\author{
Koen Derks, \email{k.derks@nyenrode.nl}
}
\keyword{audit}
\keyword{repeated}
\keyword{values}
