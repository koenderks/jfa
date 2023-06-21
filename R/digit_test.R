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

#' Data Auditing: Digit Distribution Test
#'
#' @description This function extracts and performs a test of the distribution
#' of (leading) digits in a vector against a reference distribution. By default,
#' the distribution of leading digits is checked against Benford's law.
#'
#' @usage digit_test(x,
#'            check = c("first", "last", "firsttwo"),
#'            reference = "benford",
#'            conf.level = 0.95,
#'            prior = FALSE)
#'
#' @param x          a numeric vector.
#' @param check      location of the digits to analyze. Can be \code{first},
#'   \code{firsttwo}, or \code{last}.
#' @param reference  which character string given the reference distribution for
#'   the digits, or a vector of probabilities for each digit. Can be
#'   \code{benford} for Benford's law, \code{uniform} for the uniform
#'   distribution. An error is given if any entry of \code{reference} is
#'   negative. Probabilities that do not sum to one are normalized.
#' @param conf.level a numeric value between 0 and 1 specifying the
#'   confidence level (i.e., 1 - audit risk / detection risk).
#' @param prior      a logical specifying whether to use a prior distribution,
#'   or a numeric vector containing the prior parameters for the Dirichlet
#'   distribution on the digit categories.
#'
#' @details Benford's law is defined as \eqn{p(d) = log10(1/d)}. The uniform
#'   distribution is defined as \eqn{p(d) = 1/d}.
#'
#' @return An object of class \code{jfaDistr} containing:
#'
#' \item{observed}{the observed counts.}
#' \item{expected}{the expected counts under the null hypothesis.}
#' \item{n}{the number of observations in \code{x}.}
#' \item{statistic}{the value the chi-squared test statistic.}
#' \item{parameter}{the degrees of freedom of the approximate chi-squared
#'   distribution of the test statistic.}
#' \item{p.value}{the p-value for the test.}
#' \item{check}{checked digits.}
#' \item{digits}{vector of digits.}
#' \item{reference}{reference distribution}
#' \item{match}{a list containing the row numbers corresponding to the
#'   observations matching each digit.}
#' \item{data.name}{a character string giving the name(s) of the data.}
#'
#' @author Koen Derks, \email{k.derks@nyenrode.nl}
#'
#' @references Benford, F. (1938). The law of anomalous numbers.
#'   \emph{In Proceedings of the American Philosophical Society}, 551-572.
#'
#' @seealso \code{\link{repeated_test}}
#'
#' @keywords audit Bayesian Benford digits distribution
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#'
#' # First digit analysis against Benford's law
#' digit_test(x, check = "first", reference = "benford")
#'
#' # Bayesian first digit analysis against Benford's law
#' digit_test(x, check = "first", reference = "benford", prior = TRUE)
#'
#' # Last digit analysis against the uniform distribution
#' digit_test(x, check = "last", reference = "uniform")
#'
#' # Bayesian last digit analysis against the uniform distribution
#' digit_test(x, check = "last", reference = "uniform", prior = TRUE)
#'
#' # First digit analysis against a custom distribution
#' digit_test(x, check = "last", reference = 1:9)
#'
#' # Bayesian first digit analysis against a custom distribution
#' digit_test(x, check = "last", reference = 1:9, prior = TRUE)
#' @export

digit_test <- function(x,
                       check = c("first", "last", "firsttwo"),
                       reference = "benford",
                       conf.level = 0.95,
                       prior = FALSE) {
  stopifnot("'x' must be a vector" = (NCOL(x) == 1) && !is.data.frame(x))
  check <- match.arg(check)
  bayesian <- prior[1] != FALSE
  dname <- deparse(substitute(x))
  x <- x[!is.na(x)]
  x <- x[!is.infinite(x)]
  x <- x[x != 0]
  d <- .extract_digits(x, check = check, include.zero = FALSE)
  d <- d[!is.na(d)]
  n <- length(d)
  d_tab <- table(d)
  dig <- if (check == "firsttwo") 10:99 else 1:9
  obs <- rep(0, length(dig))
  d_included <- as.numeric(names(d_tab))
  index <- if (check == "firsttwo") d_included - 9 else d_included
  obs[index] <- as.numeric(d_tab)
  if (is.numeric(reference)) {
    stopifnot(
      "number of elements in 'reference' must be equal to the number of digits" = length(reference) == length(dig),
      "all elements in 'reference' must be >= 0" = all(reference >= 0)
    )
    p_exp <- reference / sum(reference)
  } else if (reference == "benford") {
    p_exp <- log10(1 + 1 / dig)
  } else if (reference == "uniform") {
    p_exp <- rep(1 / length(dig), length(dig))
  } else {
    stop("specify a valid input for the 'reference' argument")
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
    } else if (length(prior) != length(dig)) {
      stop("number of elements in 'prior' must be equal to number of digits")
    } else {
      alpha <- prior
    }
    lbeta_xa <- sum(lgamma(alpha + obs)) - lgamma(sum(alpha + obs))
    lbeta_a <- sum(lgamma(alpha)) - lgamma(sum(alpha))
    if (any(rowSums(cbind(p_exp, obs)) == 0)) {
      log_bf10 <- (lbeta_xa - lbeta_a)
    } else {
      log_bf10 <- (lbeta_xa - lbeta_a) + (0 - sum(obs * log(p_exp)))
    }
    bf <- exp(log_bf10)
    names(bf) <- "BF10"
  }
  mad <- mean(abs((obs / n) - (exp / n)))
  names(mad) <- "MAD"
  names(n) <- "n"
  names(obs) <- dig
  names(exp) <- dig
  result <- list()
  result[["conf.level"]] <- conf.level
  result[["observed"]] <- obs
  result[["expected"]] <- exp
  result[["n"]] <- n
  if (!bayesian) {
    result[["statistic"]] <- statistic
    result[["parameter"]] <- parameter
    result[["p.value"]] <- pval
  } else {
    result[["bf"]] <- bf
  }
  result[["mad"]] <- mad
  result[["check"]] <- check
  result[["digits"]] <- dig
  result[["reference"]] <- reference
  result[["match"]] <- split(x = data.frame(row = seq_along(d), value = x), f = d)
  result[["estimates"]] <- data.frame(d = dig, n = obs, p.exp = p_exp, p.obs = obs / n)
  if (!prior) {
    result[["estimates"]]$lb <- stats::qbeta((1 - conf.level) / 2, obs, 1 + n - obs)
    result[["estimates"]]$ub <- stats::qbeta(conf.level + (1 - conf.level) / 2, 1 + obs, n - obs)
    result[["estimates"]]$p.value <- apply(result[["estimates"]], 1, function(x, n) binom.test(x[2], n, x[3], alternative = "two.sided")$p.value, n = n)
  } else {
    result[["estimates"]]$lb <- stats::qbeta((1 - conf.level) / 2, 1 + obs, 1 + n - obs)
    result[["estimates"]]$ub <- stats::qbeta(conf.level + (1 - conf.level) / 2, 1 + obs, 1 + n - obs)
    result[["estimates"]]$bf10 <- 1 / (dbeta(p_exp, 1 + obs, 1 + n - obs) / dbeta(p_exp, 1, 1))
  }
  deviation <- result[["estimates"]]$p.exp <= result[["estimates"]]$lb | result[["estimates"]]$p.exp >= result[["estimates"]]$ub
  names(deviation) <- dig
  result[["deviation"]] <- deviation
  result[["data.name"]] <- dname
  class(result) <- c("jfaDistr", "list")
  return(result)
}
