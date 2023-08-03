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

#' Methods for jfa objects
#'
#' Methods defined for objects returned from the \code{\link{auditPrior}}, \code{\link{planning}}, \code{\link{selection}}, and \code{\link{evaluation}} functions.
#'
#' @param object,x    an object of class \code{jfaPrior}, \code{jfaPosterior}, \code{jfaPlanning}, \code{jfaSelection}, \code{jfaEvaluation}, \code{jfaDistr}, or \code{jfaRv}.
#' @param digits      an integer specifying the number of digits to which output should be rounded. Used in \code{summary}.
#' @param type        used in \code{plot}. Specifies the type of plot to produce.
#' @param n           used in \code{predict}. Specifies the sample size for which predictions should be made.
#' @param cumulative  used in \code{predict}. Specifies whether cumulative probabilities should be shown.
#' @param ...         further arguments, currently ignored.
#'
#' @return
#' The \code{summary} methods return a \code{data.frame} which contains the input and output.
#'
#' The \code{print} methods simply print and return nothing.
#'
#' @name jfa-methods
NULL

# Methods for class: jfaPrior #####################################################

#' @rdname jfa-methods
#' @method print jfaPrior
#' @export
print.jfaPrior <- function(x, ...) {
  cat("\n")
  cat(strwrap("Prior Distribution for Audit Sampling", prefix = "\t"), sep = "\n")
  cat("\n")
  cat("functional form:", x[["prior"]], "\nparameters obtained via method", paste0("'", x[["method"]], "'\n"))
}

#' @rdname jfa-methods
#' @method print summary.jfaPrior
#' @export
print.summary.jfaPrior <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap("Prior Distribution Summary", prefix = "\t"), sep = "\n")
  cat("\nOptions:\n")
  cat(paste("  Likelihood:                   ", x[["likelihood"]]), "\n")
  cat(paste("  Specifics:                    ", switch(x[["method"]],
    "default" = "default prior",
    "strict" = "improper prior",
    "impartial" = paste0("p(\u0398 < ", x[["materiality"]], ") = p(\u0398 > ", x[["materiality"]], ") = 0.5"),
    "hyp" = paste0("p(\u0398 < ", x[["materiality"]], ") = ", x[["p.h1"]], "; p(\u0398 > ", x[["materiality"]], ") = ", x[["p.h0"]]),
    "arm" = paste0("ir = ", x[["ir"]], "; cr = ", x[["icr"]], "; dr = ", x[["dr"]]),
    "bram" = paste0("mode = ", x[["mode.prior"]], "; upper bound = ", x[["ub.prior"]]),
    "sample" = paste0("earlier sample of ", x[["n.prior"]], " items with ", x[["x.prior"]], " errors"),
    "factor" = paste0("earlier sample of ", x[["n.prior"]], " items with ", x[["x.prior"]], " errors weighted by ", x[["factor"]]),
    "param" = paste0("\u03B1 = ", x[["alpha"]], "; \u03B2 = ", x[["beta"]]),
    "nonparam" = "nonparametric prior",
    "mcmc" = "nonparametric prior"
  )), "\n")
  cat("\nResults:\n")
  cat(paste("  Functional form:              ", x[["prior"]]), "\n")
  if (!is.null(x[["implicit.n"]]) && x[["method"]] %in% c("poisson", "binomial", "hypergeometric")) {
    cat(paste("  Equivalent sample size:       ", format(x[["implicit.n"]], digits = max(1L, digits - 2L))), "\n")
  }
  if (!is.null(x[["implicit.x"]]) && x[["method"]] %in% c("poisson", "binomial", "hypergeometric")) {
    cat(paste("  Equivalent errors:            ", format(x[["implicit.x"]], digits = max(1L, digits - 2L))), "\n")
  }
  cat(paste("  Mode:                         ", format(x[["mode"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Mean:                         ", format(x[["mean"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Median:                       ", format(x[["median"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Variance:                     ", format(x[["var"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Skewness:                     ", format(x[["skewness"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Information entropy (nat):    ", format(x[["entropy"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste(" ", format(x[["conf.level"]] * 100), "percent upper bound:       ", format(x[["ub"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Precision:                    ", format(x[["precision"]], digits = max(1L, digits - 2L))), "\n")
}

#' @rdname jfa-methods
#' @method summary jfaPrior
#' @export
summary.jfaPrior <- function(object, digits = getOption("digits"), ...) {
  out <- data.frame(
    "conf.level" = round(object[["conf.level"]], digits),
    "likelihood" = object[["likelihood"]],
    "method" = object[["method"]],
    "prior" = object[["prior"]],
    "ub" = round(object[["statistics"]]$ub, digits),
    "precision" = round(object[["statistics"]]$precision, digits),
    "mode" = round(object[["statistics"]]$mode, digits),
    "mean" = round(object[["statistics"]]$mean, digits),
    "median" = round(object[["statistics"]]$median, digits),
    "var" = round(object[["statistics"]]$var, digits),
    "skewness" = round(object[["statistics"]]$skewness, digits),
    "entropy" = round(object[["statistics"]]$entropy, digits),
    stringsAsFactors = FALSE
  )
  if (!is.null(object[["expected"]])) {
    out[["x"]] <- round(object[["expected"]], digits)
  }
  if (!is.null(object[["description"]]$implicit.n)) {
    out[["implicit.n"]] <- round(object[["description"]]$implicit.n, digits)
  }
  if (!is.null(object[["description"]]$implicit.x)) {
    out[["implicit.x"]] <- round(object[["description"]]$implicit.x, digits)
  }
  if (object[["method"]] == "impartial") {
    out[["materiality"]] <- round(object[["materiality"]], digits)
  } else if (object[["method"]] == "arm") {
    out[["ir"]] <- round(object[["specifics"]]$ir, digits)
    out[["icr"]] <- round(object[["specifics"]]$cr, digits)
    out[["dr"]] <- round((1 - object[["conf.level"]]) / (object[["specifics"]]$ir * object[["specifics"]]$cr), digits)
  } else if (object[["method"]] == "bram") {
    out[["mode.prior"]] <- round(object[["specifics"]]$mode, digits)
    out[["ub.prior"]] <- round(object[["specifics"]]$ub, digits)
  } else if (object[["method"]] == "hyp") {
    out[["materiality"]] <- round(object[["materiality"]], digits)
    out[["p.h1"]] <- round(object[["specifics"]]$p.h1, digits)
    out[["p.h0"]] <- round(object[["specifics"]]$p.h0, digits)
  } else if (object[["method"]] == "sample" || object[["method"]] == "factor") {
    out[["n.prior"]] <- object[["specifics"]]$n
    out[["x.prior"]] <- object[["specifics"]]$x
    if (object[["method"]] == "factor") {
      out[["factor"]] <- round(object[["specifics"]]$factor, digits)
    }
  } else if (object[["method"]] == "param") {
    out[["alpha"]] <- round(object[["specifics"]]$alpha, digits)
    out[["beta"]] <- round(object[["specifics"]]$beta, digits)
  }
  class(out) <- c("summary.jfaPrior", "data.frame")
  return(out)
}

#' @rdname jfa-methods
#' @method predict jfaPrior
#' @export
predict.jfaPrior <- function(object, n, cumulative = FALSE, ...) {
  nobs <- 5e5
  if (object[["description"]]$density == "gamma") {
    p <- stats::dnbinom(0:n, object[["description"]]$alpha, 1 / (1 + object[["description"]]$beta))
    names(p) <- 0:n
  } else if (object[["description"]]$density %in% c("beta", "beta-binomial")) {
    p <- extraDistr::dbbinom(0:n, n, object[["description"]]$alpha, object[["description"]]$beta)
    names(p) <- 0:n
  } else if (object[["description"]]$density == "normal") {
    p <- prop.table(table(stats::rbinom(nobs, n, truncdist::rtrunc(nobs, "norm", 0, 1, mean = object[["description"]]$alpha, sd = object[["description"]]$beta))))
  } else if (object[["description"]]$density == "uniform") {
    p <- prop.table(table(stats::rbinom(nobs, n, truncdist::rtrunc(nobs, "unif", 0, 1, min = object[["description"]]$alpha, max = object[["description"]]$beta))))
  } else if (object[["description"]]$density == "Cauchy") {
    p <- prop.table(table(stats::rbinom(nobs, n, truncdist::rtrunc(nobs, "cauchy", 0, 1, location = object[["description"]]$alpha, scale = object[["description"]]$beta))))
  } else if (object[["description"]]$density == "Student-t") {
    p <- prop.table(table(stats::rbinom(nobs, n, truncdist::rtrunc(nobs, "t", 0, 1, df = object[["description"]]$alpha))))
  } else if (object[["description"]]$density == "chi-squared") {
    p <- prop.table(table(stats::rbinom(nobs, n, truncdist::rtrunc(nobs, "chisq", 0, 1, df = object[["description"]]$alpha))))
  } else if (object[["description"]]$density == "exponential") {
    p <- prop.table(table(stats::rbinom(nobs, n, truncdist::rtrunc(nobs, "exp", 0, 1, rate = object[["description"]]$alpha))))
  } else if (object[["description"]]$density == "MCMC") {
    p <- prop.table(table(stats::rbinom(nobs, n, .rsample(object[["fitted.density"]], n = nobs))))
  }
  if (cumulative) {
    p <- cumsum(p)
    names(p) <- if (object[["description"]]$density == "gamma") paste0("n<=", names(p)) else paste0("x<=", names(p))
  } else {
    names(p) <- if (object[["description"]]$density == "gamma") paste0("n=", names(p)) else paste0("x=", names(p))
  }
  class(p) <- c("jfaPredict", "table")
  return(p)
}

#' @rdname jfa-methods
#' @method print jfaPredict
#' @export
print.jfaPredict <- function(x, ...) {
  print.table(x, ...)
}

#' @rdname jfa-methods
#' @method plot jfaPrior
#' @export
plot.jfaPrior <- function(x, ...) {
  y <- NULL
  xs <- seq(0, 1, length.out = 1001)
  if (x[["description"]]$density == "gamma") {
    y <- stats::dgamma(xs, x[["description"]]$alpha, x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta") {
    y <- stats::dbeta(xs, x[["description"]]$alpha, x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta-binomial") {
    xs <- seq(0, x[["N.units"]], by = 1)
    if (inherits(x, "jfaPrior")) {
      y <- extraDistr::dbbinom(xs, x[["N.units"]], x[["description"]]$alpha, x[["description"]]$beta)
    } else {
      y <- extraDistr::dbbinom(xs - x[["description"]]$x, x[["N.units"]] - x[["description"]]$n, x[["description"]]$alpha, x[["description"]]$beta)
    }
  } else if (x[["description"]]$density == "normal") {
    y <- truncdist::dtrunc(xs, spec = "norm", a = 0, b = 1, mean = x[["description"]]$alpha, sd = x[["description"]]$beta)
  } else if (x[["description"]]$density == "uniform") {
    y <- truncdist::dtrunc(xs, spec = "unif", a = 0, b = 1, min = x[["description"]]$alpha, max = x[["description"]]$beta)
  } else if (x[["description"]]$density == "Cauchy") {
    y <- truncdist::dtrunc(xs, spec = "cauchy", a = 0, b = 1, location = x[["description"]]$alpha, scale = x[["description"]]$beta)
  } else if (x[["description"]]$density == "Student-t") {
    y <- truncdist::dtrunc(xs, spec = "t", a = 0, b = 1, df = x[["description"]]$alpha)
  } else if (x[["description"]]$density == "chi-squared") {
    y <- truncdist::dtrunc(xs, spec = "chisq", a = 0, b = 1, df = x[["description"]]$alpha)
  } else if (x[["description"]]$density == "exponential") {
    y <- truncdist::dtrunc(xs, spec = "exp", a = 0, b = 1, rate = x[["description"]]$alpha)
  } else if (x[["description"]]$density == "MCMC") {
    y <- x[["fitted.density"]]$y
  }
  yMax <- if (is.infinite(max(y))) 10 else max(y)
  yBreaks <- pretty(c(0, yMax), min.n = 4)
  xBreaks <- pretty(xs, min.n = 4)
  df <- data.frame(x = xs, y = y)
  p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y))
  if (x[["description"]]$density != "beta-binomial") {
    if (inherits(x, "jfaPrior")) {
      p <- p + ggplot2::geom_line(linetype = "dashed")
    } else if (inherits(x, "jfaPosterior")) {
      p <- p + ggplot2::geom_line(linetype = "solid")
    }
    p <- p + ggplot2::scale_x_continuous(name = "Population misstatement", breaks = xBreaks, limits = range(xBreaks)) +
      ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, limits = range(yBreaks))
  } else {
    if (inherits(x, "jfaPrior")) {
      p <- p + ggplot2::geom_col(colour = "black", fill = "lightgray")
    } else if (inherits(x, "jfaPosterior")) {
      p <- p + ggplot2::geom_col(colour = "black", fill = "darkgray")
    }
    p <- p + ggplot2::scale_x_continuous(name = "Population misstatements", breaks = xBreaks, limits = c(xBreaks[1] - 1, max(xBreaks) + 1)) +
      ggplot2::scale_y_continuous(name = "Probability", breaks = yBreaks, limits = range(yBreaks))
  }
  p <- p + ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = max(yBreaks)) +
    ggplot2::geom_segment(x = min(xBreaks), xend = max(xBreaks), y = -Inf, yend = -Inf)
  p <- .theme_jfa(p)
  return(p)
}

#' @rdname jfa-methods
#' @method plot jfaPredict
#' @export
plot.jfaPredict <- function(x, ...) {
  y <- NULL
  df <- data.frame(x = seq_len(length(x)) - 1, y = as.numeric(x), lab = names(x))
  yBreaks <- pretty(c(0, df$y), min.n = 4)
  xBreaks <- pretty(df$x, min.n = 4)
  p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_col(position = "identity", colour = "black", fill = "lightgray") +
    ggplot2::scale_x_continuous(name = "Misstatements", breaks = xBreaks, limits = c(min(xBreaks) - 0.5, max(xBreaks) + 0.5)) +
    ggplot2::scale_y_continuous(name = "Probability", breaks = yBreaks, limits = c(0, max(yBreaks))) +
    ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = max(yBreaks)) +
    ggplot2::geom_segment(x = min(xBreaks), xend = max(xBreaks), y = -Inf, yend = -Inf)
  p <- .theme_jfa(p)
  return(p)
}

# Methods for class: jfaPosterior #####################################################

#' @rdname jfa-methods
#' @method print jfaPosterior
#' @export
print.jfaPosterior <- function(x, ...) {
  cat("\n")
  cat(strwrap("Posterior Distribution from Audit Sampling", prefix = "\t"), sep = "\n")
  cat("\n")
  cat("functional form:", x[["posterior"]], "\nparameters obtained via method", paste0("'", x[["method"]], "'\n"))
}

#' @rdname jfa-methods
#' @method print summary.jfaPosterior
#' @export
print.summary.jfaPosterior <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap("Posterior Distribution Summary", prefix = "\t"), sep = "\n")
  cat("\nResults:\n")
  cat(paste("  Functional form:              ", x[["posterior"]]), "\n")
  cat(paste("  Mode:                         ", format(x[["mode"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Mean:                         ", format(x[["mean"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Median:                       ", format(x[["median"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Variance:                     ", format(x[["var"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Skewness:                     ", format(x[["skewness"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Information entropy (nat):    ", format(x[["entropy"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste(" ", format(x[["conf.level"]] * 100), "percent upper bound:       ", format(x[["ub"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Precision:                    ", format(x[["precision"]], digits = max(1L, digits - 2L))), "\n")
}

#' @rdname jfa-methods
#' @method summary jfaPosterior
#' @export
summary.jfaPosterior <- function(object, digits = getOption("digits"), ...) {
  out <- data.frame(
    "posterior" = object[["posterior"]],
    "ub" = round(object[["statistics"]]$ub, digits),
    "precision" = round(object[["statistics"]]$precision, digits),
    "mode" = round(object[["statistics"]]$mode, digits),
    "mean" = round(object[["statistics"]]$mean, digits),
    "median" = round(object[["statistics"]]$median, digits),
    "var" = round(object[["statistics"]]$var, digits),
    "skewness" = round(object[["statistics"]]$skewness, digits),
    "entropy" = round(object[["statistics"]]$entropy, digits),
    "conf.level" = object[["conf.level"]],
    stringsAsFactors = FALSE
  )
  class(out) <- c("summary.jfaPosterior", "data.frame")
  return(out)
}

#' @rdname jfa-methods
#' @method predict jfaPosterior
#' @export
predict.jfaPosterior <- function(object, n, cumulative = FALSE, ...) {
  predict.jfaPrior(object, n, cumulative, ...)
}

#' @rdname jfa-methods
#' @method plot jfaPosterior
#' @export
plot.jfaPosterior <- function(x, ...) {
  plot.jfaPrior(x, ...)
}

# Methods for class: jfaPlanning #####################################################

#' @rdname jfa-methods
#' @method print jfaPlanning
#' @export
print.jfaPlanning <- function(x, ...) {
  cat("\n")
  if (is.null(x[["prior"]])) cat(strwrap("Classical Audit Sample Planning", prefix = "\t"), sep = "\n") else cat(strwrap("Bayesian Audit Sample Planning", prefix = "\t"), sep = "\n")
  cat("\n")
  cat("minimum sample size =", x[["n"]], "\nsample size obtained in", x[["iterations"]], "iterations via method", if (is.null(x[["prior"]])) paste0("'", x[["likelihood"]], "'\n") else paste0("'", x[["likelihood"]], "' + 'prior'\n"))
}

#' @rdname jfa-methods
#' @method print summary.jfaPlanning
#' @export
print.summary.jfaPlanning <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap(paste(x[["type"]], "Audit Sample Planning Summary"), prefix = "\t"), sep = "\n")
  cat("\nOptions:\n")
  cat(paste("  Confidence level:             ", format(x[["conf.level"]], digits = max(1L, digits - 2L))), "\n")
  if (!is.null(x[["N.units"]])) {
    cat(paste("  Population size:              ", format(x[["N.units"]], digits = max(1L, digits - 2L))), "\n")
  }
  if (x[["materiality"]] < 1) {
    cat(paste("  Materiality:                  ", format(x[["materiality"]], digits = max(1L, digits - 2L))), "\n")
    cat(paste("  Hypotheses:                   ", paste0("H\u2080: \u0398 ", if (x[["type"]] == "Bayesian") ">" else ">=", " ", format(x[["materiality"]], digits = max(1L, digits - 2L)), " vs. H\u2081: \u0398 < ", format(x[["materiality"]], digits = max(1L, digits - 2L)))), "\n")
  }
  if (x[["min.precision"]] < 1) {
    cat(paste("  Min. precision:               ", format(x[["min.precision"]], digits = max(1L, digits - 2L))), "\n")
  }
  cat(paste("  Expected:                     ", format(x[["expected"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Likelihood:                   ", x[["likelihood"]]), "\n")
  if (x[["type"]] == "Bayesian") {
    cat(paste("  Prior distribution:           ", x[["prior"]]), "\n")
  }
  cat("\nResults:\n")
  cat(paste("  Minimum sample size:          ", x[["n"]]), "\n")
  cat(paste("  Tolerable errors:             ", format(x[["x"]], digits = max(1L, digits - 2L))), "\n")
  if (x[["type"]] == "Bayesian") {
    cat(paste("  Posterior distribution:       ", x[["posterior"]]), "\n")
  }
  cat(paste("  Expected most likely error:   ", format(x[["mle"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Expected upper bound:         ", format(x[["ub"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Expected precision:           ", format(x[["precision"]], digits = max(1L, digits - 2L))), "\n")
  if (x[["materiality"]] < 1 && x[["type"]] == "Bayesian") {
    cat(paste("  Expected BF\u2081\u2080:                ", format(x[["bf.h1"]], digits = max(1L, digits - 2L))), "\n")
  }
  if (x[["materiality"]] < 1 && x[["type"]] == "Classical") {
    cat(paste("  Expected p-value:             ", format.pval(x[["p.value"]], digits = max(1L, digits - 2L), eps = 0.001)), "\n")
  }
}

#' @rdname jfa-methods
#' @method summary jfaPlanning
#' @export
summary.jfaPlanning <- function(object, digits = getOption("digits"), ...) {
  out <- data.frame(
    "conf.level" = round(object[["conf.level"]], digits),
    "expected" = round(object[["expected"]], digits),
    "materiality" = round(object[["materiality"]], digits),
    "min.precision" = round(object[["min.precision"]], digits),
    "likelihood" = object[["likelihood"]],
    "x" = round(object[["x"]], digits),
    "n" = object[["n"]],
    "ub" = round(object[["ub"]], digits),
    "precision" = round(object[["precision"]], digits),
    stringsAsFactors = FALSE
  )
  if (!is.null(object[["p.value"]])) {
    out[["p.value"]] <- object[["p.value"]]
  }
  if (!is.null(object[["N.units"]])) {
    out[["N.units"]] <- object[["N.units"]]
  }
  if (inherits(object[["prior"]], "jfaPrior")) {
    out[["prior"]] <- object[["prior"]]$prior
    out[["posterior"]] <- object[["posterior"]]$posterior
    out[["mle"]] <- round(object[["posterior"]][["statistics"]]$mode, digits)
    if (object[["materiality"]] != 1) {
      out[["bf.h1"]] <- round(object[["posterior"]][["hypotheses"]]$bf.h1, digits)
    }
  } else {
    out[["mle"]] <- round(object[["x"]] / object[["n"]], digits)
  }
  out[["type"]] <- if (inherits(object[["prior"]], "jfaPrior")) "Bayesian" else "Classical"
  class(out) <- c("summary.jfaPlanning", "data.frame")
  return(out)
}

#' @rdname jfa-methods
#' @method plot jfaPlanning
#' @export
plot.jfaPlanning <- function(x, ...) {
  if (is.null(x[["prior"]])) {
    if (inherits(x, "jfaPlanning")) {
      likelihood <- x[["likelihood"]]
    } else if (inherits(x, "jfaEvaluation")) {
      likelihood <- x[["method"]]
    }
    xs <- 0:max(10, x[["x"]])
    if (likelihood == "hypergeometric") {
      ys <- stats::dhyper(x = xs, m = x[["K"]], n = x[["N.units"]] - x[["K"]], k = x[["n"]])
    } else if (likelihood == "binomial") {
      ys <- stats::dbinom(x = xs, size = x[["n"]], prob = x[["materiality"]])
    } else if (likelihood == "poisson") {
      ys <- stats::dpois(x = xs, lambda = x[["n"]] * x[["materiality"]])
    }
    ncolored <- 1 + ceiling(x[["x"]])
    fill <- c(rep("firebrick", ncolored), rep("darkgray", length(xs) - ncolored))
    df <- data.frame(x = xs, y = ys)
    y_breaks <- pretty(c(0, df$y), min.n = 4)
    p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_col(position = "identity", fill = fill, colour = "black") +
      ggplot2::annotate(geom = "text", x = df$x, y = df$y, label = round(df$y, 3), vjust = -0.5, size = 3) +
      ggplot2::scale_x_continuous(name = "Misstatements", breaks = xs) +
      ggplot2::scale_y_continuous(name = "Probability", breaks = y_breaks, limits = c(0, max(y_breaks))) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = max(y_breaks)) +
      ggplot2::geom_segment(x = 0, xend = max(xs), y = -Inf, yend = -Inf)
    p <- .theme_jfa(p)
  } else {
    y <- type <- NULL
    x1 <- x2 <- seq(0, 1, length.out = 1001)
    if (x[["prior"]][["description"]]$density == "gamma") {
      y1 <- stats::dgamma(x1, x[["prior"]][["description"]]$alpha, x[["prior"]][["description"]]$beta)
    } else if (x[["prior"]][["description"]]$density == "beta") {
      y1 <- stats::dbeta(x1, x[["prior"]][["description"]]$alpha, x[["prior"]][["description"]]$beta)
    } else if (x[["prior"]][["description"]]$density == "beta-binomial") {
      x1 <- x2 <- seq(0, x[["N.units"]], by = 1)
      y1 <- extraDistr::dbbinom(x1, x[["prior"]][["N.units"]], x[["prior"]][["description"]]$alpha, x[["prior"]][["description"]]$beta)
    } else if (x[["prior"]][["description"]]$density == "normal") {
      y1 <- truncdist::dtrunc(x1, spec = "norm", a = 0, b = 1, mean = x[["prior"]][["description"]]$alpha, sd = x[["prior"]][["description"]]$beta)
    } else if (x[["prior"]][["description"]]$density == "uniform") {
      y1 <- truncdist::dtrunc(x1, spec = "unif", a = 0, b = 1, min = x[["prior"]][["description"]]$alpha, max = x[["prior"]][["description"]]$beta)
    } else if (x[["prior"]][["description"]]$density == "Cauchy") {
      y1 <- truncdist::dtrunc(x1, spec = "cauchy", a = 0, b = 1, location = x[["prior"]][["description"]]$alpha, scale = x[["prior"]][["description"]]$beta)
    } else if (x[["prior"]][["description"]]$density == "Student-t") {
      y1 <- truncdist::dtrunc(x1, spec = "t", a = 0, b = 1, df = x[["prior"]][["description"]]$alpha)
    } else if (x[["prior"]][["description"]]$density == "chi-squared") {
      y1 <- truncdist::dtrunc(x1, spec = "chisq", a = 0, b = 1, df = x[["prior"]][["description"]]$alpha)
    } else if (x[["prior"]][["description"]]$density == "exponential") {
      y1 <- truncdist::dtrunc(x1, spec = "exp", a = 0, b = 1, rate = x[["prior"]][["description"]]$alpha)
    } else if (x[["prior"]][["description"]]$density == "MCMC") {
      y1 <- x[["prior"]][["fitted.density"]]$y
    }
    if (x[["posterior"]][["description"]]$density == "gamma") {
      y2 <- stats::dgamma(x2, x[["posterior"]][["description"]]$alpha, x[["posterior"]][["description"]]$beta)
    } else if (x[["posterior"]][["description"]]$density == "beta") {
      y2 <- stats::dbeta(x2, x[["posterior"]][["description"]]$alpha, x[["posterior"]][["description"]]$beta)
    } else if (x[["posterior"]][["description"]]$density == "beta-binomial") {
      y2 <- extraDistr::dbbinom(x2 - x[["x"]], x[["posterior"]][["N.units"]] - x[["n"]], x[["posterior"]][["description"]]$alpha, x[["posterior"]][["description"]]$beta)
    } else if (x[["posterior"]][["description"]]$density == "MCMC") {
      y2 <- x[["posterior"]][["fitted.density"]]$y
    }
    df <- data.frame(x = c(x1, x2), y = c(y1, y2), type = c(rep("Prior", length(y1)), rep("Posterior", length(y2))))
    yMax <- if (is.infinite(max(df$y))) max(y2) else max(df$y)
    yBreaks <- pretty(c(0, yMax * 1.05), min.n = 4)
    xBreaks <- pretty(df$x, min.n = 4)
    p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y))
    if (x[["prior"]][["description"]]$density != "beta-binomial") {
      p <- p + ggplot2::geom_line(mapping = ggplot2::aes(linetype = type)) +
        ggplot2::scale_linetype_manual(name = NULL, values = c("solid", "dashed")) +
        ggplot2::scale_x_continuous(name = "Population misstatement", breaks = xBreaks, limits = c(0, 1)) +
        ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, limits = range(yBreaks))
    } else {
      p <- p + ggplot2::geom_col(mapping = ggplot2::aes(fill = type), alpha = 0.75, colour = "black", position = "identity") +
        ggplot2::scale_fill_manual(name = NULL, values = c("darkgray", "lightgray")) +
        ggplot2::scale_x_continuous(name = "Population misstatements", breaks = xBreaks, limits = c(xBreaks[1] - 1, max(xBreaks) + 1)) +
        ggplot2::scale_y_continuous(name = "Probability", breaks = yBreaks, limits = range(yBreaks))
    }
    if (inherits(x, "jfaEvaluation")) {
      if (x[["prior"]][["description"]]$density != "beta-binomial") {
        p <- p + ggplot2::geom_segment(x = x[["lb"]], xend = x[["ub"]], y = max(yBreaks), yend = max(yBreaks)) +
          ggplot2::geom_segment(x = x[["lb"]], xend = x[["lb"]], y = max(yBreaks) - ((yBreaks[2] - yBreaks[1]) / 10), yend = max(yBreaks) + ((yBreaks[2] - yBreaks[1]) / 10)) +
          ggplot2::geom_segment(x = x[["ub"]], xend = x[["ub"]], y = max(yBreaks) - ((yBreaks[2] - yBreaks[1]) / 10), yend = max(yBreaks) + ((yBreaks[2] - yBreaks[1]) / 10)) +
          ggplot2::geom_point(x = x[["mle"]], y = max(yBreaks), size = 2, fill = "darkgray", colour = "black", shape = 21)
      } else {
        p <- p + ggplot2::geom_segment(x = ceiling(x[["lb"]] * x[["N.units"]]), xend = ceiling(x[["ub"]] * x[["N.units"]]), y = max(yBreaks), yend = max(yBreaks)) +
          ggplot2::geom_segment(x = ceiling(x[["lb"]] * x[["N.units"]]), xend = ceiling(x[["lb"]] * x[["N.units"]]), y = max(yBreaks) - ((yBreaks[2] - yBreaks[1]) / 10), yend = max(yBreaks) + ((yBreaks[2] - yBreaks[1]) / 10)) +
          ggplot2::geom_segment(x = ceiling(x[["ub"]] * x[["N.units"]]), xend = ceiling(x[["ub"]] * x[["N.units"]]), y = max(yBreaks) - ((yBreaks[2] - yBreaks[1]) / 10), yend = max(yBreaks) + ((yBreaks[2] - yBreaks[1]) / 10)) +
          ggplot2::geom_point(x = ceiling(x[["mle"]] * x[["N.units"]]), y = max(yBreaks), size = 2, fill = "darkgray", colour = "black", shape = 21)
      }
    }
    p <- p + ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = max(yBreaks)) +
      ggplot2::geom_segment(x = min(xBreaks), xend = max(xBreaks), y = -Inf, yend = -Inf)
    p <- .theme_jfa(p, legend.position = c(0.8, 0.8))
  }
  return(p)
}

# Methods for class: jfaSelection #####################################################

#' @rdname jfa-methods
#' @method print jfaSelection
#' @export
print.jfaSelection <- function(x, ...) {
  cat("\n")
  cat(strwrap("Audit Sample Selection", prefix = "\t"), sep = "\n")
  cat("\n")
  cat(paste("data: ", x[["data.name"]]), sep = "\n")
  cat(paste0("number of sampling units = ", x[["n.units"]], ", number of items = ", x[["n.items"]], "\nsample selected via method ", paste0("'", x[["units"]], "' + '", x[["method"]], "'\n")))
}

#' @rdname jfa-methods
#' @method print summary.jfaSelection
#' @export
print.summary.jfaSelection <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap(paste(x[["type"]], "Audit Sample Selection Summary"), prefix = "\t"), sep = "\n")
  cat("\nOptions:\n")
  cat(paste("  Requested sample size:        ", x[["n.req"]]), "\n")
  cat(paste("  Sampling units:               ", switch(x[["units"]],
    "values" = "monetary units",
    "items" = "items"
  )), "\n")
  cat(paste("  Method:                       ", switch(x[["method"]],
    "random" = "random sampling",
    "interval" = "fixed interval sampling",
    "cell" = "cell sampling",
    "sieve" = "modified sieve sampling"
  )), "\n")
  if (x[["method"]] == "interval") {
    cat(paste("  Starting point:               ", x[["start"]]), "\n")
  }
  cat("\nData:\n")
  cat(paste("  Population size:              ", x[["N.items"]]), "\n")
  if (x[["units"]] == "values") {
    cat(paste("  Population value:             ", format(x[["N.units"]], digits = max(1L, digits - 2L))), "\n")
  }
  if (x[["method"]] == "interval" || x[["method"]] == "cell") {
    cat(paste("  Selection interval:           ", format(x[["interval"]], digits = max(1L, digits - 2L))), "\n")
  }
  cat("\nResults:\n")
  cat(paste("  Selected sampling units:      ", x[["n.units"]]), "\n")
  if (x[["units"]] == "values") {
    cat(paste("  Proportion of value:          ", format(x[["prop.val"]], digits = max(1L, digits - 2L))), "\n")
  }
  cat(paste("  Selected items:               ", x[["n.items"]]), "\n")
  cat(paste("  Proportion of size:           ", format(x[["prop.n"]], digits = max(1L, digits - 2L))), "\n")
}

#' @rdname jfa-methods
#' @method summary jfaSelection
#' @export
summary.jfaSelection <- function(object, digits = getOption("digits"), ...) {
  out <- data.frame(
    "N.items" = object[["N.items"]],
    "N.units" = round(object[["N.units"]], digits),
    "n.req" = object[["n.req"]],
    "units" = object[["units"]],
    "method" = object[["method"]],
    "n.units" = object[["n.units"]],
    "n.items" = object[["n.items"]],
    "prop.n" = round(object[["n.items"]] / object[["N.items"]], digits),
    stringsAsFactors = FALSE
  )
  if (object[["method"]] == "interval" || object[["method"]] == "cell") {
    out[["interval"]] <- round(object[["interval"]], digits)
    if (object[["method"]] == "interval") {
      out[["start"]] <- object[["start"]]
    }
  }
  if (object[["units"]] == "values") {
    out[["prop.val"]] <- round(sum(abs(object[["sample"]][, object[["values.name"]]])) / object[["N.units"]], digits)
  }
  class(out) <- c("summary.jfaSelection", "data.frame")
  return(out)
}

# Methods for class: jfaEvaluation #####################################################

#' @rdname jfa-methods
#' @method print jfaEvaluation
#' @export
print.jfaEvaluation <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  if (is.null(x[["prior"]])) cat(strwrap(paste0("Classical Audit Sample Evaluation"), prefix = "\t"), sep = "\n") else cat(strwrap(paste0("Bayesian Audit Sample Evaluation"), prefix = "\t"), sep = "\n")
  cat("\n")
  lab <- if (!is.null(x[["data"]])) x[["data.name"]] else paste0(x[["x"]], " and ", x[["n"]])
  cat(paste("data: ", lab), sep = "\n")
  out <- character()
  out <- c(out, paste("number of errors =", format(x[["x"]], digits = max(1L, digits - 2L))))
  out <- c(out, paste("number of samples =", format(x[["n"]], digits = max(1L, digits - 2L))))
  out <- c(out, paste("taint =", format(x[["t"]], digits = max(1L, digits - 2L))))
  if (is.null(x[["prior"]]) && x[["materiality"]] < 1 && x[["method"]] %in% c("binomial", "poisson", "hypergeometric")) {
    out <- c(out, paste("p-value =", format.pval(x[["p.value"]], digits = max(1L, digits - 2L))))
  }
  if (!is.null(x[["prior"]]) && x[["materiality"]] < 1) {
    out <- c(out, paste("BF\u2081\u2080 =", format(x[["posterior"]][["hypotheses"]]$bf.h1, digits = max(1L, digits - 2L))))
  }
  cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
  if (!is.null(x[["p.value"]]) || !is.null(x[["posterior"]]$hypotheses)) {
    if (x[["alternative"]] == "less") {
      cat(paste0("alternative hypothesis: true misstatement rate is less than ", x[["materiality"]]), sep = "\n")
    }
    if (x[["alternative"]] == "two.sided") {
      cat(paste0("alternative hypothesis: true misstatement rate is not equal to ", x[["materiality"]]), sep = "\n")
    }
    if (x[["alternative"]] == "greater") {
      cat(paste0("alternative hypothesis: true misstatement rate is greater than ", x[["materiality"]]), sep = "\n")
    }
  }
  if (!is.null(x[["ub"]])) {
    cat(format(100 * x$conf.level), " percent ", if (is.null(x[["prior"]])) "confidence" else "credible", " interval:\n", " ", paste(format(c(x[["lb"]], x[["ub"]]), digits = digits), collapse = " "), "\n", sep = "")
  }
  if (!is.null(x[["mle"]])) {
    cat("most likely estimate:\n", format(x[["mle"]], digits = max(1L, digits - 2L)), "\n")
  }
  method_string <- paste0("'", x[["method"]], "'")
  if (!is.null(x[["strata"]]) && x[["pooling"]] == "complete") {
    method_string <- paste0(method_string, " + 'complete-pooling'")
  } else if (!is.null(x[["strata"]]) && x[["pooling"]] == "none") {
    method_string <- paste0(method_string, " + 'no-pooling'")
  } else if (!is.null(x[["strata"]]) && x[["pooling"]] == "partial") {
    method_string <- paste0(method_string, " + 'partial-pooling'")
  }
  if (!is.null(x[["prior"]])) {
    method_string <- paste0(method_string, " + 'prior'")
  }
  cat("results obtained via method", method_string, "\n")
}

#' @rdname jfa-methods
#' @method print summary.jfaEvaluation
#' @export
print.summary.jfaEvaluation <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap(paste(x[["type"]], "Audit Sample Evaluation Summary"), prefix = "\t"), sep = "\n")
  cat("\nOptions:\n")
  cat(paste("  Confidence level:              ", format(x[["conf.level"]], digits = max(1L, digits - 2L))), "\n")
  if (!is.null(x[["N.units"]])) {
    cat(paste("  Population size:               ", format(x[["N.units"]], digits = max(1L, digits - 2L))), "\n")
  }
  if (x[["materiality"]] < 1) {
    cat(paste("  Materiality:                   ", format(x[["materiality"]], digits = max(1L, digits - 2L))), "\n")
    if (x[["method"]] %in% c("poisson", "binomial", "hypergeometric")) {
      cat(paste("  Hypotheses:                    ", switch(x[["alternative"]],
        "two.sided" = paste0("H\u2080: \u0398 = ", format(x[["materiality"]], digits = max(1L, digits - 2L)), " vs. H\u2081: \u0398 \u2260 ", format(x[["materiality"]], digits = max(1L, digits - 2L))),
        "less" = paste0("H\u2080: \u0398 ", if (x[["type"]] == "Bayesian") ">" else ">=", " ", format(x[["materiality"]], digits = max(1L, digits - 2L)), " vs. H\u2081: \u0398 < ", format(x[["materiality"]], digits = max(1L, digits - 2L))),
        "greater" = paste0("H\u2080: \u0398 ", if (x[["type"]] == "Bayesian") "<" else "<=", " ", format(x[["materiality"]], digits = max(1L, digits - 2L)), " vs. H\u2081: \u0398 > ", format(x[["materiality"]], digits = max(1L, digits - 2L)))
      )), "\n")
    }
  }
  cat(paste("  Method:                        ", x[["method"]]), "\n")
  if (x[["type"]] == "Bayesian") {
    cat(paste("  Prior distribution:            ", x[["prior"]]), "\n")
  }
  cat("\nData:\n")
  cat(paste("  Sample size:                   ", x[["n"]]), "\n")
  cat(paste("  Number of errors:              ", x[["x"]]), "\n")
  cat(paste("  Sum of taints:                 ", x[["t"]]), "\n")
  cat("\nResults:\n")
  if (x[["type"]] == "Bayesian") {
    cat("  Posterior distribution:        ", x[["posterior"]], "\n")
  }
  cat(paste("  Most likely error:             ", format(x[["mle"]], digits = max(1L, digits - 2L))), "\n")
  if (x[["type"]] == "Bayesian") {
    cat(paste(" ", paste0(format(x[["conf.level"]] * 100), " percent credible interval:   ", paste0("[", format(x[["lb"]], digits = max(1L, digits - 2L)), ", ", format(x[["ub"]], digits = max(1L, digits - 2L)), "]"))), "\n")
  } else if (x[["type"]] == "Classical") {
    cat(paste(" ", paste0(format(x[["conf.level"]] * 100), " percent confidence interval: ", paste0("[", format(x[["lb"]], digits = max(1L, digits - 2L)), ", ", format(x[["ub"]], digits = max(1L, digits - 2L)), "]"))), "\n")
  }
  cat(paste("  Precision:                     ", format(x[["precision"]], digits = max(1L, digits - 2L))), "\n")
  if (x[["materiality"]] < 1 && x[["type"]] == "Bayesian") {
    cat(paste("  BF\u2081\u2080:\t                         ", format(x[["bf.h1"]], digits = max(1L, digits - 2L))), "\n")
  }
  if (x[["materiality"]] < 1 && x[["type"]] == "Classical" && x[["method"]] %in% c("poisson", "binomial", "hypergeometric")) {
    cat(paste("  p-value:                       ", format.pval(x[["p.value"]], digits = max(1L, digits - 2L))), "\n")
  }
  if (!is.null(x[["strata"]])) {
    cat(paste0("\nStrata (", nrow(x[["strata"]]), "):\n"))
    print(round(x[["strata"]], digits = max(1L, digits - 2L)), quote = FALSE)
  }
}

#' @rdname jfa-methods
#' @method summary jfaEvaluation
#' @export
summary.jfaEvaluation <- function(object, digits = getOption("digits"), ...) {
  out <- list(
    "conf.level" = round(object[["conf.level"]], digits),
    "materiality" = round(object[["materiality"]], digits),
    "x" = object[["x"]],
    "n" = object[["n"]],
    "method" = object[["method"]],
    "mle" = round(object[["mle"]], digits),
    "precision" = round(object[["precision"]], digits),
    "alternative" = object[["alternative"]],
    "pooling" = object[["pooling"]],
    stringsAsFactors = FALSE
  )
  if (!is.null(object[["p.value"]])) {
    out[["p.value"]] <- object[["p.value"]]
  }
  if (!is.null(object[["N.units"]])) {
    out[["N.units"]] <- object[["N.units"]]
  }
  if (object[["method"]] %in% c("direct", "difference", "quotient", "regression")) {
    out[["type"]] <- "Classical"
    out[["lb"]] <- round(object[["lb"]], digits)
    out[["ub"]] <- round(object[["ub"]], digits)
  } else {
    out[["t"]] <- round(object[["t"]], digits)
    out[["ub"]] <- round(object[["ub"]], digits)
    out[["lb"]] <- round(object[["lb"]], digits)
    if (!is.null(object[["prior"]])) {
      out[["type"]] <- "Bayesian"
      out[["prior"]] <- object[["prior"]][["prior"]]
      out[["posterior"]] <- object[["posterior"]][["posterior"]]
      if (object[["materiality"]] != 1) {
        out[["bf.h1"]] <- round(object[["posterior"]][["hypotheses"]]$bf.h1, digits)
      }
    } else {
      out[["type"]] <- "Classical"
    }
  }
  if (!is.null(object[["strata"]])) {
    out[["strata"]] <- object[["strata"]]
  }
  class(out) <- c("summary.jfaEvaluation", "list")
  return(out)
}

#' @rdname jfa-methods
#' @method plot jfaEvaluation
#' @export
plot.jfaEvaluation <- function(x, type = c("posterior", "estimates"), ...) {
  y <- lb <- ub <- NULL
  type <- match.arg(type)
  if (type == "posterior") {
    p <- plot.jfaPlanning(x, ...)
  } else {
    xs <- 0
    labels <- "Population"
    mles <- x[["mle"]]
    lbs <- x[["lb"]]
    ubs <- x[["ub"]]
    if (!is.null(x[["strata"]])) {
      xs <- c(xs, seq_len(nrow(x[["strata"]])))
      labels <- c(labels, rownames(x[["strata"]]))
      mles <- c(mles, x[["strata"]]$mle)
      lbs <- c(lbs, x[["strata"]]$lb)
      ubs <- c(ubs, x[["strata"]]$ub)
    }
    df <- data.frame(x = xs, y = mles, lb = lbs, ub = ubs, lab = labels)
    xBreaks <- df$x
    xLimits <- c(min(df$x) - 0.25, max(df$x) + 0.25)
    p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y, ymin = lb, ymax = ub))
    if (x[["materiality"]] < 1) {
      p <- p + ggplot2::geom_segment(
        x = -Inf,
        xend = Inf,
        y = x[["materiality"]],
        yend = x[["materiality"]],
        colour = "darkred",
        linetype = "dashed"
      )
    }
    p <- p + ggplot2::geom_errorbar(size = 0.5, width = 0.25) +
      ggplot2::geom_point(size = 2.5, fill = "darkgray", colour = "black", shape = 21) +
      ggplot2::scale_x_continuous(name = NULL, breaks = xBreaks, limits = xLimits, labels = df$lab) +
      ggplot2::scale_y_continuous(name = "Misstatement", limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1) +
      ggplot2::geom_segment(x = 0, xend = max(xBreaks), y = -Inf, yend = -Inf)
    p <- .theme_jfa(p)
  }
  return(p)
}

# Methods for class: jfaDistr #####################################################

#' @rdname jfa-methods
#' @method print jfaDistr
#' @export
print.jfaDistr <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap("Digit Distribution Test", prefix = "\t"), sep = "\n")
  cat("\n")
  cat("data:  ", x$data.name, "\n", sep = "")
  out <- character()
  if (!is.null(x$n)) {
    out <- c(out, paste(names(x$n), "=", format(x$n, digits = max(1L, digits - 2L))))
  }
  if (!is.null(x$mad)) {
    out <- c(out, paste(names(x$mad), "=", format(x$mad, digits = max(1L, digits - 2L))))
  }
  if (!is.null(x$statistic)) {
    out <- c(out, paste(names(x$statistic), "=", format(x$statistic, digits = max(1L, digits - 2L))))
  }
  if (!is.null(x$parameter)) {
    out <- c(out, paste(names(x$parameter), "=", format(x$parameter, digits = max(1L, digits - 2L))))
  }
  if (!is.null(x$bf)) {
    out <- c(out, paste("BF10", "=", format(x$bf, digits = max(1L, digits - 2L))))
  }
  if (!is.null(x$p.value)) {
    fp <- format.pval(x$p.value, digits = max(1L, digits - 3L))
    out <- c(out, paste("p-value", if (startsWith(fp, "<")) fp else paste("=", fp)))
  }
  cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
  digitLabel <- switch(x$check,
    "first" = "leading",
    "last" = "last",
    "firsttwo" = "first two"
  )
  distLabel <- if (is.numeric(x$reference)) "reference" else x$reference
  cat(paste0("alternative hypothesis: ", digitLabel, " digit(s) are not distributed according to the ", distLabel, " distribution."))
  cat("\n")
  invisible(x)
}

#' @rdname jfa-methods
#' @method print summary.jfaDistr
#' @export
print.summary.jfaDistr <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap("Digit Distribution Test Summary", prefix = "\t"), sep = "\n")
  cat("\nOptions:\n")
  cat(paste("  Confidence level:              ", format(x[["conf.level"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Digits:                        ", switch(x[["check"]],
    "first" = "Leading",
    "firsttwo" = "First and second",
    "last" = "Last"
  )), "\n")
  cat(paste("  Reference:                     ", if (x[["reference"]] == "benford") {
    "Benford's law"
  } else if (x[["reference"]] == "uniform") {
    "Uniform distribution"
  } else {
    "Custom distribution"
  }), "\n")
  if (x[["type"]] == "Bayesian") {
    cat(paste("  Prior distribution:            ", "Dirichlet(1, ..., 1)"), "\n")
  }
  cat("\nData:\n")
  cat(paste("  Sample size:                   ", format(x[["n"]], digits = max(1L, digits - 2L))), "\n")
  cat("\nResults:\n")
  if (x[["type"]] == "Bayesian") {
    cat(paste("  BF\u2081\u2080:\t                         ", format(x[["bf"]], digits = max(1L, digits - 2L))), "\n")
  } else {
    cat(paste("  X-squared:\t                 ", format(x[["statistic"]], digits = max(1L, digits - 2L))), "\n")
    cat(paste("  Degrees of freedom:\t         ", format(x[["parameter"]], digits = max(1L, digits - 2L))), "\n")
    cat(paste("  p-value:                       ", format.pval(x[["p.value"]], digits = max(1L, digits - 2L))), "\n")
  }
  cat(paste("  Mean absolute difference (MAD):", format(x[["mad"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste0("\nDigits (", length(x[["digits"]]), "):\n"))
  print(round(x[["estimates"]], digits = max(1L, digits - 2L)), quote = FALSE)
}

#' @rdname jfa-methods
#' @method summary jfaDistr
#' @export
summary.jfaDistr <- function(object, digits = getOption("digits"), ...) {
  out <- list(
    "conf.level" = round(object[["conf.level"]], digits),
    "n" = object[["n"]],
    "check" = object[["check"]],
    "digits" = object[["digits"]],
    "reference" = object[["reference"]],
    "mad" = object[["mad"]],
    stringsAsFactors = FALSE
  )
  if (!is.null(object[["p.value"]])) {
    out[["statistic"]] <- object[["statistic"]]
    out[["parameter"]] <- object[["parameter"]]
    out[["p.value"]] <- object[["p.value"]]
    out[["type"]] <- "Classical"
  } else if (!is.null(object[["bf"]])) {
    out[["bf"]] <- object[["bf"]]
    out[["type"]] <- "Bayesian"
  }
  out[["estimates"]] <- object[["estimates"]]
  class(out) <- c("summary.jfaDistr", "list")
  return(out)
}

#' @rdname jfa-methods
#' @method plot jfaDistr
#' @export
plot.jfaDistr <- function(x, ...) {
  y <- type <- d <- lb <- ub <- NULL
  df <- data.frame(
    x = c(x[["digits"]], x[["digits"]]),
    y = c(x$observed / x$n, x$expected / x$n),
    type = c(rep("Observed", length(x[["digits"]])), rep("Expected", length(x[["digits"]])))
  )
  yBreaks <- pretty(c(0, df$y, x[["estimates"]]$ub), min.n = 4)
  if (x[["check"]] == "first" || x[["check"]] == "last") {
    xBreaks <- x[["digits"]]
    xLabels <- x[["digits"]]
    pointSize <- 5
    lineSize <- 1.5
  } else {
    xBreaks <- x[["digits"]]
    xLabels <- c(
      10, rep("", 9),
      20, rep("", 9),
      30, rep("", 9),
      40, rep("", 9),
      50, rep("", 9),
      60, rep("", 9),
      70, rep("", 9),
      80, rep("", 9),
      90, rep("", 8),
      99
    )
    pointSize <- 2
    lineSize <- 1.2
  }
  axisName <- switch(x[["check"]],
    "first" = "Leading digit",
    "firsttwo" = "Leading digits",
    "last" = "Last digit"
  )
  xs <- rep(xBreaks[1], 2)
  ys <- rep(yBreaks[1], 2)
  types <- c("Observed", "Expected")
  sizes <- c(7, 10)
  shapes <- c(21, 22)
  fills <- c("dodgerblue", "darkgray")
  if (any(x[["deviation"]])) {
    xs <- c(xs, xBreaks[1])
    ys <- c(ys, yBreaks[1])
    types <- c(types, "Deviation")
    sizes <- c(sizes, 10)
    shapes <- c(shapes, 22)
    fills <- c(fills, "firebrick")
  }
  plotData <- data.frame(x = xs, y = ys, type = types)
  plotData[["type"]] <- factor(plotData[["type"]], levels = types)
  p <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, y = y, fill = type)) +
    ggplot2::geom_point(alpha = 0) +
    ggplot2::geom_bar(data = subset(df, type == "Expected"), mapping = ggplot2::aes(x = x, y = y), fill = ifelse(x[["deviation"]], "firebrick", "darkgray"), stat = "identity", color = "black") +
    ggplot2::geom_line(data = subset(df, type == "Observed"), mapping = ggplot2::aes(x = x, y = y), color = "dodgerblue", linewidth = lineSize) +
    ggplot2::geom_errorbar(data = x[["estimates"]], mapping = ggplot2::aes(x = d, ymin = lb, ymax = ub), width = 0.5) +
    ggplot2::geom_point(data = subset(df, type == "Observed"), mapping = ggplot2::aes(x = x, y = y), fill = "dodgerblue", size = pointSize, shape = 21) +
    ggplot2::scale_x_continuous(name = axisName, breaks = xBreaks, labels = xLabels, limits = c(min(x[["digits"]]) - 0.5, max(x[["digits"]]) + 0.5), ) +
    ggplot2::scale_y_continuous(name = "Relative frequency", breaks = yBreaks, limits = c(0, max(yBreaks))) +
    ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = max(yBreaks)) +
    ggplot2::geom_segment(x = min(xBreaks), xend = max(xBreaks), y = -Inf, yend = -Inf) +
    ggplot2::labs(fill = "") +
    ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(l = -5, r = 50))) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(
      size = sizes, shape = shapes, fill = fills, color = "black", alpha = 1
    )))
  p <- .theme_jfa(p, legend.position = "top")
  return(p)
}

# Methods for class: jfaRv #####################################################

#' @rdname jfa-methods
#' @method print jfaRv
#' @export
print.jfaRv <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap("Repeated Values Test", prefix = "\t"), sep = "\n")
  cat("\n")
  cat("data:  ", x$data.name, "\n", sep = "")
  out <- character()
  if (!is.null(x$n)) {
    out <- c(out, paste(names(x$n), "=", format(x$n, digits = max(1L, digits - 2L))))
  }
  if (!is.null(x$statistic)) {
    out <- c(out, paste(names(x$statistic), "=", format(x$statistic, digits = max(1L, digits - 2L))))
  }
  if (!is.null(x$p.value)) {
    fp <- format.pval(x$p.value, digits = max(1L, digits - 3L))
    out <- c(out, paste("p-value", if (startsWith(fp, "<")) fp else paste("=", fp)))
  }
  cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
  cat(paste0("alternative hypothesis: ", switch(x$method,
    "af" = "average frequency",
    "entropy" = "entropy"
  ), " in data is ", switch(x$method,
    "af" = "greater",
    "entropy" = "lower"
  ), " than for random data."))
  cat("\n")
  invisible(x)
}

#' @rdname jfa-methods
#' @method plot jfaRv
#' @export
plot.jfaRv <- function(x, ...) {
  y <- NULL
  df <- data.frame(x = as.numeric(names(x$frequencies)), y = as.numeric(x$frequencies))
  xBreaks <- pretty(df$x, min.n = 4)
  yBreaks <- pretty(c(0, df$y), min.n = 4)
  p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_bar(fill = "darkgray", color = "black", linewidth = 0.2, stat = "identity") +
    ggplot2::scale_x_continuous(name = "Value", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = "Frequency", breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::geom_segment(x = -Inf, xend = -Inf, y = 0, yend = max(yBreaks)) +
    ggplot2::geom_segment(x = min(xBreaks), xend = max(xBreaks), y = -Inf, yend = -Inf)
  p <- .theme_jfa(p)
  return(p)
}

# Methods for class: jfaModelFairness ##############################################

#' @rdname jfa-methods
#' @method print jfaModelFairness
#' @export
print.jfaModelFairness <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap("Algorithmic Fairness Metrics", prefix = "\t"), sep = "\n")
  cat("\n")
  cat("data:  ", x[["data.name"]], "\n", sep = "")
  groupLevels <- names(x$confusion.matrix)
  cat(paste0("reference group: ", x[["reference"]], ", positive class: ", x[["positive"]], "\n"))
  cat("alternative hypothesis: true odds ratio is not equal to 1\n")
  if (length(colnames(x[["metrics"]]$all)) == 1 && colnames(x[["metrics"]]$all) != "dp") {
    measure <- switch(colnames(x[["metrics"]]$all),
      "pp" = "Proportional parity",
      "prp" = "Predictive rate parity",
      "ap" = "Accuracy parity",
      "fnrp" = "False negative rate parity",
      "fprp" = "False positive rate parity",
      "tprp" = "True positive rate parity",
      "npvp" = "Negative predictive value parity",
      "sp" = "Specificity parity"
    )
    cat(paste0("\n", measure, " (", length(groupLevels) - 1, " sensitive groups):"))
    for (i in seq_len(length(groupLevels))) {
      if (groupLevels[i] == x[["reference"]]) {
        next
      }
      if (!x[["prior"]]) {
        cat("\n", paste0(groupLevels[i], ": p-value = ", format.pval(x[["odds.ratio"]][[colnames(x[["metrics"]]$all)]][[groupLevels[i]]]$p.value, digits = max(1L, digits - 2L))))
      } else {
        cat("\n", paste0(groupLevels[i], ": BF\u2081\u2080 = ", format(x[["odds.ratio"]][[colnames(x[["metrics"]]$all)]][[groupLevels[i]]]$bf10, digits = max(1L, digits - 2L))))
      }
    }
  } else {
    cat(paste0("\n", length(colnames(x[["metrics"]]$all)), " fairness measures (", length(groupLevels) - 1, " sensitive groups):"))
    for (i in seq_len(length(groupLevels))) {
      if (groupLevels[i] == x[["reference"]]) {
        next
      }
      cat("\n", paste0(groupLevels[i], ": ", sum(sapply(colnames(x[["metrics"]]$all), function(measure) {
        x[["parity"]][[measure]][[groupLevels[i]]][["deviation"]]
      })), " measure(s) outside tolerance region"))
    }
  }
}

#' @rdname jfa-methods
#' @method print summary.jfaModelFairness
#' @export
print.summary.jfaModelFairness <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap("Algorithmic Fairness Metrics Summary", prefix = "\t"), sep = "\n")
  groupLevels <- names(x[["confusion.matrix"]])
  ind <- which(groupLevels == x[["reference"]])
  cat(paste0("\nReference group:  ", x[["reference"]], "\n"))
  cat("\nModel performance:\n")
  df <- data.frame(matrix(NA, nrow = 5, ncol = length(groupLevels)))
  for (i in seq_along(groupLevels)) {
    for (j in seq_len(nrow(df))) {
      df[j, i] <- format(x[["performance"]][["all"]][i, j], digits = max(1L, digits - 2L))
    }
  }
  rownames(df) <- c(
    "  Support",
    "  Accuracy",
    "  Precision",
    "  Recall",
    "  F1 score"
  )
  colnames(df) <- groupLevels
  print(df)
  cat("\nFairness metrics (parity ratio):\n")
  df <- data.frame(matrix(NA, nrow = length(colnames(x[["metrics"]]$all)) + 1, ncol = length(groupLevels)))
  rownames <- c(colnames(x[["metrics"]]$all), "out")
  for (i in seq_along(groupLevels)) {
    for (j in seq_along(rownames)) {
      if (j %in% seq_along(colnames(x[["metrics"]]$all))) {
        df[j, i] <- paste0(format(x[["metrics"]][["all"]][i, j], digits = max(1L, digits - 2L)), " (", format(x[["parity"]][["all"]][i, j], digits = max(1L, digits - 2L)), ")")
      } else {
        df[j, i] <- sum(sapply(colnames(x[["metrics"]]$all), function(measure) {
          x[["parity"]][[measure]][[groupLevels[i]]][["deviation"]]
        }))
      }
    }
  }
  rownames <- replace(rownames, rownames == "dp", "  Demographic parity")
  rownames <- replace(rownames, rownames == "pp", "  Proportional parity")
  rownames <- replace(rownames, rownames == "prp", "  Predictive rate parity")
  rownames <- replace(rownames, rownames == "ap", "  Accuracy parity")
  rownames <- replace(rownames, rownames == "fnrp", "  False negative rate parity")
  rownames <- replace(rownames, rownames == "fprp", "  False positive rate parity")
  rownames <- replace(rownames, rownames == "tprp", "  True positive rate parity")
  rownames <- replace(rownames, rownames == "npvp", "  Negative predicted value parity")
  rownames <- replace(rownames, rownames == "sp", "  Specificity parity")
  rownames <- replace(rownames, rownames == "out", "  Outside tolerance region")
  rownames(df) <- rownames
  df["  Outside tolerance region", ind] <- ""
  colnames(df) <- groupLevels
  if (length(colnames(x[["metrics"]]$all)) == 1) {
    df <- as.data.frame(t(df))
    df <- df[, -ncol(df), drop = FALSE]
  }
  print(df)
  if (!(length(colnames(x[["metrics"]]$all)) == 1 && colnames(x[["metrics"]]$all) == "dp")) {
    if (!x[["prior"]]) {
      cat("\nOdds ratio (p-value):\n")
    } else {
      cat("\nOdds ratio (BF\u2081\u2080):\n")
    }
    groupLevels <- groupLevels[-ind]
    df <- data.frame(matrix(NA, nrow = length(subset(colnames(x[["metrics"]]$all), colnames(x[["metrics"]]$all) != "dp")), ncol = length(groupLevels)))
    for (i in seq_along(groupLevels)) {
      for (j in seq_along(subset(colnames(x[["metrics"]]$all), colnames(x[["metrics"]]$all) != "dp"))) {
        if (!x[["prior"]]) {
          crit <- format.pval(x[["odds.ratio"]][[j + 1]][[groupLevels[i]]][["p.value"]], digits = max(1L, digits - 2L))
        } else {
          crit <- format(x[["odds.ratio"]][[j + 1]][[groupLevels[i]]][["bf10"]], digits = max(1L, digits - 2L))
        }
        df[j, i] <- paste0(format(x[["odds.ratio"]][["all"]][i, j], digits = max(1L, digits - 2L)), " (", crit, ")")
      }
    }
    rownames <- subset(colnames(x[["metrics"]]$all), colnames(x[["metrics"]]$all) != "dp")
    rownames <- replace(rownames, rownames == "pp", "  Proportional parity")
    rownames <- replace(rownames, rownames == "prp", "  Predictive rate parity")
    rownames <- replace(rownames, rownames == "ap", "  Accuracy parity")
    rownames <- replace(rownames, rownames == "fnrp", "  False negative rate parity")
    rownames <- replace(rownames, rownames == "fprp", "  False positive rate parity")
    rownames <- replace(rownames, rownames == "tprp", "  True positive rate parity")
    rownames <- replace(rownames, rownames == "npvp", "  Negative predicted value parity")
    rownames <- replace(rownames, rownames == "sp", "  Specificity parity")
    rownames(df) <- rownames
    colnames(df) <- groupLevels
    if (length(colnames(x[["metrics"]]$all)) == 1) {
      df <- as.data.frame(t(df))
    }
    print(df)
  }
}

#' @rdname jfa-methods
#' @method summary jfaModelFairness
#' @export
summary.jfaModelFairness <- function(object, digits = getOption("digits"), ...) {
  out <- list()
  out[["reference"]] <- object[["reference"]]
  out[["confusion.matrix"]] <- object[["confusion.matrix"]]
  out[["metrics"]] <- object[["metrics"]]
  out[["performance"]] <- object[["performance"]]
  out[["parity"]] <- object[["parity"]]
  out[["odds.ratio"]] <- object[["odds.ratio"]]
  out[["prior"]] <- object[["prior"]]
  class(out) <- c("summary.jfaModelFairness", "list")
  return(out)
}

#' @rdname jfa-methods
#' @method plot jfaModelFairness
#' @export
plot.jfaModelFairness <- function(x, type = c("estimates", "posterior"), ...) {
  type <- match.arg(type)
  value <- variable <- group <- NULL
  measures <- colnames(x[["metrics"]]$all)
  groupLevels <- names(x[["confusion.matrix"]])
  ind <- which(groupLevels == x[["reference"]])
  groupLevels <- groupLevels[-ind]
  if (type == "estimates") {
    mle_ratio <- x[["parity"]][["all"]][, measures, drop = FALSE]
    mle <- stats::reshape(
      mle_ratio[-ind, , drop = FALSE],
      direction = "long",
      varying = measures,
      v.names = "value",
      timevar = "variable"
    )
    if (length(measures[measures != "dp"]) > 0) {
      lb_ratio <- data.frame(row.names = groupLevels)
      for (measure in measures[measures != "dp"]) {
        lb_ratio[[measure]] <- sapply(groupLevels, function(group) {
          x$parity[[measure]][[group]]$lb
        })
      }
      lb <- stats::reshape(
        lb_ratio[, , drop = FALSE],
        direction = "long",
        varying = measures[measures != "dp"],
        v.names = "value",
        timevar = "variable"
      )
      ub_ratio <- data.frame(row.names = groupLevels)
      for (measure in measures[measures != "dp"]) {
        ub_ratio[[measure]] <- sapply(groupLevels, function(group) {
          x$parity[[measure]][[group]]$ub
        })
      }
      ub <- stats::reshape(
        ub_ratio[, , drop = FALSE],
        direction = "long",
        varying = measures[measures != "dp"],
        v.names = "value",
        timevar = "variable"
      )
      if ("dp" %in% measures) {
        lb$variable <- lb$variable + 1
        ub$variable <- ub$variable + 1
      }
      prefix <- if ("dp" %in% measures) mle_ratio$dp[-ind] else NULL
      ratio <- cbind(mle, lb = c(prefix, lb[, 2]), ub = c(prefix, ub[, 2]))
    } else {
      ratio <- mle
    }
    ratio[["group"]] <- rep(groupLevels, length(measures))
    ratio[["variable"]] <- factor(toupper(measures[ratio[["variable"]]]), levels = toupper(measures))
    yBreaks <- pretty(c(0, ratio[["value"]], 1 / x[["materiality"]], ratio[["ub"]]), min.n = 4)
    p <- ggplot2::ggplot(data = ratio, mapping = ggplot2::aes(x = group, y = value, fill = variable)) +
      ggplot2::geom_col(colour = "black", position = ggplot2::position_dodge()) +
      ggplot2::scale_x_discrete(name = "Sensitive Group") +
      ggplot2::scale_y_continuous(name = "Parity Ratio", breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = x[["materiality"]], ymax = 1 / x[["materiality"]], fill = "darkgray", alpha = 0.5) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = min(yBreaks), yend = max(yBreaks)) +
      ggplot2::geom_segment(x = -Inf, xend = Inf, y = 1, yend = 1, linetype = "dashed", color = "black", linewidth = 0.35) +
      ggplot2::scale_fill_brewer(name = "Measure", type = "div")
    if (length(measures[measures != "dp"]) > 0) {
      p <- p + ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = lb, ymax = ub), width = 0.5, position = ggplot2::position_dodge(width = 0.9))
    }
    p <- .theme_jfa(p)
  } else {
    y <- group <- NULL
    stopifnot("plot not supported for frequentist inference" = isTRUE(x[["prior"]]))
    stopifnot("plot not supported for multiple measures" = length(measures) == 1)
    stopifnot("plot not supported for demographic parity" = measures != "dp")
    plotdata <- data.frame(x = numeric(), y = numeric(), group = numeric())
    for (i in seq_along(groupLevels)) {
      tmp <- data.frame(x = x[["odds.ratio"]][[measures]][[groupLevels[i]]]$density$x, y = x[["odds.ratio"]][[measures]][[groupLevels[i]]]$density$y, group = groupLevels[i])
      plotdata <- rbind(plotdata, tmp)
    }
    xBreaks <- pretty(c(0, plotdata$x), min.n = 4)
    yBreaks <- pretty(plotdata$y, min.n = 4)
    p <- ggplot2::ggplot(data = plotdata, mapping = ggplot2::aes(x = x, y = y, group = factor(group), color = factor(group))) +
      ggplot2::geom_line() +
      ggplot2::scale_color_brewer(name = "Sensitive group", type = "div") +
      ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, limits = range(yBreaks)) +
      ggplot2::scale_x_continuous(name = "Odds ratio", breaks = xBreaks, limits = c(0, max(xBreaks))) +
      ggplot2::geom_segment(x = -Inf, xend = -Inf, y = min(yBreaks), yend = max(yBreaks)) +
      ggplot2::geom_segment(x = min(xBreaks), xend = max(xBreaks), y = -Inf, yend = -Inf)
    p <- .theme_jfa(p, legend.position = c(0.75, 0.75))
  }
  return(p)
}
