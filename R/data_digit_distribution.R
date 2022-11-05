# Copyright (C) 2021-2022 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Auditing Data: Digit Distribution
#'
#' @description This function extracts and performs a test of the distribution of (leading) digits in a vector against a reference distribution. By default, the distribution of leading digits is checked against Benford's law.
#'
#' @usage data_digit_distribution(x, check = 'first',
#'                         reference = 'benford', prior = FALSE)
#'
#' @param x           a numeric vector.
#' @param check       location of the digits to analyze. Can be \code{first}, \code{firsttwo}, or \code{last}.
#' @param reference   which character string given the reference distribution for the digits, or a vector of probabilities for each digit. Can be \code{benford} for Benford's law, \code{uniform} for the uniform distribution. An error is given if any entry of \code{reference} is negative. Probabilities that do not sum to one are normalized.
#' @param prior       a logical specifying whether to use a prior distribution, or a numeric vector containing the prior parameters for the Dirichlet distribution on the digit categories.
#'
#' @details Benford's law is defined as \eqn{p(d) = log10(1/d)}. The uniform distribution is defined as \eqn{p(d) = 1/d}.
#'
#' @return An object of class \code{jfaDistr} containing:
#'
#' \item{observed}{the observed counts.}
#' \item{expected}{the expected counts under the null hypothesis.}
#' \item{n}{the number of observations in \code{x}.}
#' \item{statistic}{the value the chi-squared test statistic.}
#' \item{parameter}{the degrees of freedom of the approximate chi-squared distribution of the test statistic.}
#' \item{p.value}{the p-value for the test.}
#' \item{check}{checked digits.}
#' \item{digits}{vector of digits.}
#' \item{reference}{reference distribution}
#' \item{data.name}{a character string giving the name(s) of the data.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @references Benford, F. (1938). The law of anomalous numbers. \emph{In Proceedings of the American Philosophical Society}, 551-572.
#'
#' @seealso \code{\link{data_repeated_values}}
#'
#' @keywords benford distribution Bayes factor
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#'
#' # First digit analysis against Benford's law
#' data_digit_distribution(x, check = "first", reference = "benford")
#'
#' # Bayesian first digit analysis against Benford's law
#' data_digit_distribution(x, check = "first", reference = "benford", prior = TRUE)
#'
#' # Last digit analysis against the uniform distribution
#' data_digit_distribution(x, check = "last", reference = "uniform")
#'
#' # Bayesian last digit analysis against the uniform distribution
#' data_digit_distribution(x, check = "last", reference = "uniform", prior = TRUE)
#'
#' # First digit analysis against a custom distribution
#' data_digit_distribution(x, check = "last", reference = 1:9)
#'
#' # Bayesian first digit analysis against a custom distribution
#' data_digit_distribution(x, check = "last", reference = 1:9, prior = TRUE)
#'
#' @export

data_digit_distribution <- function(x, check = c("first", "last", "firsttwo"),
                                    reference = "benford", prior = FALSE) {
  bayesian <- inherits(prior, "jfaPrior") || prior
  dname <- deparse(substitute(x))
  x <- x[!is.na(x)]
  x <- x[!is.infinite(x)]
  d <- .extract_digits(x, check = check, include.zero = FALSE)
  d <- d[!is.na(d)]
  n <- length(d)
  d_tab <- table(d)
  dig <- if (check == "firsttwo") 10:99 else 1:9
  obs <- rep(0, length(dig))
  d_included <- as.numeric(names(d_tab))
  index <- if (check == "firsttwo") d_included - 9 else d_included
  obs[index] <- as.numeric(d_tab)
  p_obs <- obs / n
  if (is.numeric(reference)) {
    if (length(reference) != length(dig)) {
      stop("The number of elements in reference must be equal to the number of possible digits")
    }
    if (any(reference < 0)) {
      stop("All elements in reference must be positive")
    }
    p_exp <- reference / sum(reference)
  } else if (reference == "benford") {
    p_exp <- log10(1 + 1 / dig) # Benfords law: log_10(1 + 1 / d)
  } else if (reference == "uniform") {
    p_exp <- rep(1 / length(dig), length(dig))
  } else {
    stop("Specify a valid input for the reference argument")
  }
  exp <- n * p_exp
  if (!bayesian) {
    statistic <- sum((obs - exp)^2 / exp)
    parameter <- length(dig) - 1
    pval <- stats::pchisq(q = statistic, df = parameter, lower.tail = FALSE)
    names(statistic) <- "X-squared"
    names(parameter) <- "df"
  } else {
    if (is.logical(prior)) {
      alpha <- rep(1, length(dig))
    } else if (length(alpha) != length(dig)) {
      stop("The number of elements in alpha must be equal to the number of possible digits.")
    }
    # compute Bayes factor
    lbeta.xa <- sum(lgamma(alpha + obs)) - lgamma(sum(alpha + obs)) # Prior with alpha[i] counts for each digit
    lbeta.a <- sum(lgamma(alpha)) - lgamma(sum(alpha))
    # in this case, counts*log(thetas) should be zero, omit to avoid numerical issue with log(0)
    if (any(rowSums(cbind(p_exp, obs)) == 0)) {
      log_bf10 <- (lbeta.xa - lbeta.a)
    } else {
      log_bf10 <- (lbeta.xa - lbeta.a) + (0 - sum(obs * log(p_exp)))
    }
    bf <- exp(log_bf10)
    names(bf) <- "BF10"
  }
  names(n) <- "n"
  names(obs) <- dig
  names(exp) <- dig
  # Create results object
  result <- list()
  result[["observed"]] <- obs
  result[["expected"]] <- exp
  result[["n"]] <- n
  if (!prior) {
    result[["statistic"]] <- statistic
    result[["parameter"]] <- parameter
    result[["p.value"]] <- pval
  } else {
    result[["bf"]] <- bf
  }
  result[["check"]] <- check
  result[["digits"]] <- dig
  result[["reference"]] <- reference
  result[["data.name"]] <- dname
  class(result) <- c(class(result), "jfaDistr")
  return(result)
}
