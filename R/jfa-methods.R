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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Methods for jfa objects
#'
#' Methods defined for objects returned from the \code{\link{auditPrior}}, \code{\link{planning}}, \code{\link{selection}}, and \code{\link{evaluation}} functions.
#'
#' @param object,x    an object of class \code{jfaPrior}, \code{jfaPosterior}, \code{jfaPlanning}, \code{jfaSelection}, \code{jfaEvaluation}, \code{jfaDistr}, or \code{jfaRv}.
#' @param digits      an integer specifying the number of digits to which output should be rounded. Used in \code{summary}.
#' @param xlim        used in \code{plot}. Specifies the x limits (x1, x2) of the plot.
#' @param n           used in \code{predict}. Specifies the sample size for which predictions should be made.
#' @param lim         used in \code{predict}. Limits the number of errors for which predictions should be made.
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
    "default" = "noninformative",
    "strict" = "classical properties",
    "impartial" = paste0("p(\u0398 < ", x[["materiality"]], ") = p(\u0398 > ", x[["materiality"]], ") = 0.5"),
    "hyp" = paste0("p(\u0398 < ", x[["materiality"]], ") = ", x[["p.h1"]], "; p(\u0398 > ", x[["materiality"]], ") = ", x[["p.h0"]]),
    "arm" = paste0("ir = ", x[["ir"]], "; cr = ", x[["icr"]], "; dr = ", x[["dr"]]),
    "bram" = paste0("mode = ", x[["mode.prior"]], "; upper bound = ", x[["ub.prior"]]),
    "sample" = paste0("earlier sample of ", x[["n.prior"]], " items with ", x[["x.prior"]], " errors"),
    "factor" = paste0("earlier sample of ", x[["n.prior"]], " items with ", x[["x.prior"]], " errors weighted by ", x[["factor"]]),
    "param" = paste0("\u03B1 = ", x[["alpha"]], "; \u03B2 = ", x[["beta"]])
  )), "\n")
  cat("\nResults:\n")
  cat(paste("  Functional form:              ", x[["prior"]]), "\n")
  cat(paste("  Equivalent sample size:       ", format(x[["implicit.n"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Equivalent errors:            ", format(x[["implicit.x"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Mode:                         ", format(x[["mode"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Mean:                         ", format(x[["mean"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Median:                       ", format(x[["median"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Variance:                     ", format(x[["var"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Skewness:                     ", format(x[["skewness"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste(" ", format(x[["conf.level"]] * 100), "percent upper bound:       ", format(x[["ub"]], digits = max(1L, digits - 2L))), "\n")
  cat(paste("  Precision:                    ", format(x[["precision"]], digits = max(1L, digits - 2L))), "\n")
}

#' @rdname jfa-methods
#' @method summary jfaPrior
#' @export
summary.jfaPrior <- function(object, digits = getOption("digits"), ...) {
  out <- data.frame(
    "conf.level" = round(object[["conf.level"]], digits),
    "x" = round(object[["expected"]], digits),
    "likelihood" = object[["likelihood"]],
    "method" = object[["method"]],
    "prior" = object[["prior"]],
    "implicit.n" = round(object[["description"]]$implicit.n, digits),
    "implicit.x" = round(object[["description"]]$implicit.x, digits),
    "ub" = round(object[["statistics"]]$ub, digits),
    "precision" = round(object[["statistics"]]$precision, digits),
    "mode" = round(object[["statistics"]]$mode, digits),
    "mean" = round(object[["statistics"]]$mean, digits),
    "median" = round(object[["statistics"]]$median, digits),
    "var" = round(object[["statistics"]]$var, digits),
    "skewness" = round(object[["statistics"]]$skewness, digits),
    stringsAsFactors = FALSE
  )
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
predict.jfaPrior <- function(object, n, lim = NULL, cumulative = FALSE, ...) {
  if (!is.null(lim)) {
    if (cumulative) {
      stopifnot("'lim' must be a single value >= 0" = length(lim) == 1 && lim > 0)
      if (object[["description"]]$density == "gamma") {
        p <- stats::dnbinom(0:lim, size = object[["description"]]$alpha, prob = 1 / (1 + object[["description"]]$beta))
      } else {
        p <- extraDistr::dbbinom(0:lim, size = n, alpha = object[["description"]]$alpha, beta = object[["description"]]$beta)
      }
      p <- cumsum(p)
      names(p) <- if (object[["description"]]$density == "gamma") paste0("n<=", 0:lim) else paste0("x<=", 0:lim)
    } else {
      if (object[["description"]]$density == "gamma") {
        p <- stats::dnbinom(lim, size = object[["description"]]$alpha, prob = 1 / (1 + object[["description"]]$beta))
      } else {
        p <- extraDistr::dbbinom(lim, size = n, alpha = object[["description"]]$alpha, beta = object[["description"]]$beta)
      }
      names(p) <- if (object[["description"]]$density == "gamma") paste0("n=", lim) else paste0("x=", lim)
    }
  } else {
    if (cumulative) {
      if (object[["description"]]$density == "gamma") {
        p <- stats::dnbinom(0:n, size = object[["description"]]$alpha, prob = 1 / (1 + object[["description"]]$beta))
      } else {
        p <- extraDistr::dbbinom(0:n, size = n, alpha = object[["description"]]$alpha, beta = object[["description"]]$beta)
      }
      p <- cumsum(p)
      names(p) <- if (object[["description"]]$density == "gamma") paste0("n<=", 0:n) else paste0("x<=", 0:n)
    } else {
      if (object[["description"]]$density == "gamma") {
        p <- stats::dnbinom(0:n, size = object[["description"]]$alpha, prob = 1 / (1 + object[["description"]]$beta))
      } else {
        p <- extraDistr::dbbinom(0:n, size = n, alpha = object[["description"]]$alpha, beta = object[["description"]]$beta)
      }
      names(p) <- if (object[["description"]]$density == "gamma") paste0("n=", 0:n) else paste0("x=", 0:n)
    }
  }
  return(p)
}

#' @rdname jfa-methods
#' @method plot jfaPrior
#' @export
plot.jfaPrior <- function(x, xlim = c(0, 1), ...) {
  if (x[["description"]]$density == "gamma") {
    xseq <- seq(xlim[1], xlim[2], length.out = 1000)
    d <- stats::dgamma(xseq, shape = x[["description"]]$alpha, rate = x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta") {
    xseq <- seq(xlim[1], xlim[2], length.out = 1000)
    d <- stats::dbeta(xseq, shape1 = x[["description"]]$alpha, shape2 = x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta-binomial") {
    xseq <- seq(xlim[1], xlim[2], by = 1)
    d <- extraDistr::dbbinom(x = xseq, size = x[["N.units"]], alpha = x[["description"]]$alpha, beta = x[["description"]]$beta)
  }
  yMax <- if (is.infinite(max(d))) 10 else max(d)
  if (x[["description"]]$density == "gamma" || x[["description"]]$density == "beta") {
    graphics::plot(x = xseq, y = d, type = "l", bty = "n", xlab = expression(theta), ylab = "Density", las = 1, ylim = c(0, yMax), main = x[["prior"]], axes = FALSE, lty = 2)
    graphics::axis(1, at = pretty(xseq, min.n = 4), labels = round(pretty(xseq, min.n = 4), 2))
    graphics::axis(2, at = c(0, yMax), labels = FALSE, las = 1, lwd.ticks = 0)
  } else {
    graphics::barplot(d, bty = "n", xlab = "K", ylab = "Probability", las = 1, ylim = c(0, yMax), width = 1, space = 0, main = x[["prior"]], axes = FALSE, col = "darkgray")
    graphics::axis(1, at = pretty(xseq, min.n = 4) + 0.5, labels = pretty(xseq, min.n = 4))
    graphics::axis(2, at = c(0, yMax), labels = FALSE, las = 1, lwd.ticks = 0)
  }
}

# Methods for class: jfaPosterior #####################################################

#' @rdname jfa-methods
#' @method print jfaPosterior
#' @export
print.jfaPosterior <- function(x, ...) {
  cat("\n")
  cat(strwrap("Posterior Distribution from Audit Sampling", prefix = "\t"), sep = "\n")
  cat("\n")
  cat("functional form:", x[["posterior"]], "\nparameters obtained via method 'sample'\n")
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
    stringsAsFactors = FALSE
  )
  class(out) <- c("summary.jfaPosterior", "data.frame")
  return(out)
}

#' @rdname jfa-methods
#' @method predict jfaPosterior
#' @export
predict.jfaPosterior <- function(object, n, lim = NULL, cumulative = FALSE, ...) {
  predict.jfaPrior(object, n, lim, cumulative, ...)
}

#' @rdname jfa-methods
#' @method plot jfaPosterior
#' @export
plot.jfaPosterior <- function(x, xlim = c(0, 1), ...) {
  if (x[["description"]]$density == "gamma") {
    xseq <- seq(xlim[1], xlim[2], length.out = 1000)
    d <- stats::dgamma(xseq, shape = x[["description"]]$alpha, rate = x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta") {
    xseq <- seq(xlim[1], xlim[2], length.out = 1000)
    d <- stats::dbeta(xseq, shape1 = x[["description"]]$alpha, shape2 = x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta-binomial") {
    xseq <- seq(xlim[1], xlim[2], by = 1)
    d <- c(rep(0, x[["description"]]$x), extraDistr::dbbinom(x = xseq, size = x[["N.units"]] - x[["description"]]$n, alpha = x[["description"]]$alpha, beta = x[["description"]]$beta))
  }
  yMax <- if (is.infinite(max(d))) 10 else max(d)
  if (x[["description"]]$density == "gamma" || x[["description"]]$density == "beta") {
    graphics::plot(x = xseq, y = d, type = "l", bty = "n", xlab = expression(theta), ylab = "Density", las = 1, ylim = c(0, yMax), main = x[["posterior"]], axes = FALSE, lty = 1)
    graphics::axis(1, at = pretty(xseq, min.n = 4), labels = round(pretty(xseq, min.n = 4), 2))
    graphics::axis(2, at = c(0, yMax), labels = FALSE, las = 1, lwd.ticks = 0)
  } else {
    mainLab <- paste0("beta-binomial posterior distribution (N = ", x[["N.units"]] - x[["description"]]$n, ", alpha = ", round(x[["description"]]$alpha, 3), ", beta = ", round(x[["description"]]$beta, 3), ")")
    graphics::barplot(d, bty = "n", xlab = "K", ylab = "Probability", las = 1, ylim = c(0, yMax), width = 1, space = 0, main = x[["posterior"]], axes = FALSE, col = "darkgray")
    graphics::axis(1, at = pretty(xseq, min.n = 4) + 0.5, labels = pretty(xseq, min.n = 4))
    graphics::axis(2, at = c(0, yMax), labels = FALSE, las = 1, lwd.ticks = 0)
  }
}

# Methods for class: jfaPlanning #####################################################

#' @rdname jfa-methods
#' @method print jfaPlanning
#' @export
print.jfaPlanning <- function(x, ...) {
  cat("\n")
  if (is.null(x[["prior"]])) cat(strwrap("Classical Audit Sample Planning", prefix = "\t"), sep = "\n") else cat(strwrap("Bayesian Audit Sample Planning", prefix = "\t"), sep = "\n")
  cat("\n")
  cat("minimum sample size =", x[["n"]], "\nsample size obtained in", x[["iterations"]], "iteration(s) via method", if (is.null(x[["prior"]])) paste0("'", x[["likelihood"]], "'\n") else paste0("'", x[["likelihood"]], "' + 'prior'\n"))
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
    cat(paste("  Expected BF\u2081\u2080:\t                ", format(x[["bf.h1"]], digits = max(1L, digits - 2L))), "\n")
  }
  if (x[["materiality"]] < 1 && x[["type"]] == "Classical") {
    cat(paste("  Expected p-value:             ", format.pval(x[["p.value"]], digits = max(1L, digits - 2L))), "\n")
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
    out[["p.value"]] <- format.pval(object[["p.value"]], digits, eps = 0.01)
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
plot.jfaPlanning <- function(x, xlim = c(0, 1), ...) {
  if (!is.null(x[["prior"]])) {
    if (x[["likelihood"]] == "poisson") {
      xseq <- seq(xlim[1], xlim[2], length.out = 1000)
      d <- stats::dgamma(xseq, shape = x[["prior"]][["description"]]$alpha, rate = x[["prior"]][["description"]]$beta)
      d1 <- stats::dgamma(xseq, shape = x[["posterior"]][["description"]]$alpha, rate = x[["posterior"]][["description"]]$beta)
    } else if (x[["likelihood"]] == "binomial") {
      xseq <- seq(xlim[1], xlim[2], length.out = 1000)
      d <- stats::dbeta(xseq, shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
      d1 <- stats::dbeta(xseq, shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
    } else if (x[["likelihood"]] == "hypergeometric") {
      xseq <- seq(xlim[1], xlim[2], by = 1)
      d <- extraDistr::dbbinom(x = xseq, size = x[["N.units"]], alpha = x[["prior"]][["description"]]$alpha, beta = x[["prior"]][["description"]]$beta)
      d1 <- extraDistr::dbbinom(x = xseq, size = x[["N.units"]] - x[["n"]], alpha = x[["posterior"]][["description"]]$alpha, beta = x[["posterior"]][["description"]]$beta)
    }
    if (x$likelihood == "poisson" || x$likelihood == "binomial") {
      graphics::plot(
        x = xseq, y = d1, type = "l", bty = "n", xlab = expression(theta), ylab = "Density", las = 1, ylim = c(0, max(d1)),
        main = paste0(x[["prior"]]$prior, "  \u2192  ", x[["posterior"]]$posterior), axes = FALSE
      )
      graphics::lines(x = xseq, y = d, lty = 2)
      graphics::axis(1, at = pretty(xseq, min.n = 4), labels = round(pretty(xseq, min.n = 4), 2))
      graphics::axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
      graphics::legend("topright", legend = c("Prior", "Posterior"), lty = c(2, 1), bty = "n", cex = 1.2, lwd = 2)
    } else {
      graphics::barplot(d1,
        bty = "n", xlab = "K", ylab = "Probability", las = 1, ylim = c(0, max(d1)), width = 1, space = 0,
        main = paste0(x[["prior"]]$prior, "  \u2192  ", x[["posterior"]]$posterior), axes = FALSE, col = "darkgray"
      )
      graphics::barplot(d, col = "lightgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
      graphics::axis(1, at = pretty(xseq, min.n = 4) + 0.5, labels = pretty(xseq, min.n = 4))
      graphics::axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
      graphics::legend("topright", legend = c("Prior", "Posterior"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2)
    }
  } else {
    xseq <- seq(xlim[1], xlim[2], by = 1)
    if (x[["likelihood"]] == "poisson") {
      mainLab <- paste0("Poisson distribution (lambda = ", round(x[["materiality"]] * x[["n"]], 2), ")")
      d <- stats::dpois(x = xseq, lambda = x[["materiality"]] * x[["n"]])
      d1 <- stats::dpois(x = 0:x[["x"]], lambda = x[["materiality"]] * x[["n"]])
    } else if (x[["likelihood"]] == "binomial") {
      mainLab <- paste0("Binomial distribution (n = ", x[["n"]], ", p = ", x[["materiality"]], ")")
      d <- stats::dbinom(x = xseq, size = x[["n"]], prob = x[["materiality"]])
      d1 <- stats::dbinom(x = 0:x[["x"]], size = x[["n"]], prob = x[["materiality"]])
    } else if (x$likelihood == "hypergeometric") {
      mainLab <- paste0("Hypergeometric distribution (N = ", x[["N.units"]], ", n = ", x[["n"]], ", K = ", x[["K"]], ")")
      d <- stats::dhyper(x = xseq, m = x[["K"]], n = x[["N.units"]] - x[["K"]], k = x[["n"]])
      d1 <- stats::dhyper(x = 0:x[["x"]], m = x[["K"]], n = x[["N.units"]] - x[["K"]], k = x[["n"]])
    }
    graphics::barplot(d, xlab = "x", col = "lightgray", ylab = "Probability", las = 1, main = mainLab, width = 1, space = 0)
    graphics::axis(1, at = pretty(xseq, min.n = 4) + 0.5, labels = pretty(xseq, min.n = 4))
    graphics::barplot(d1, col = "darkgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
    graphics::legend("topright", legend = c("Expected error free", "Expected errors"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2)
  }
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

#' @rdname jfa-methods
#' @method plot jfaSelection
#' @export
plot.jfaSelection <- function(x, ...) {
  stopifnot("No plotting method available for selection with units = 'items'" = x[["units"]] == "values")
  name <- x[["values"]]
  graphics::hist(x[["data"]][[name]], breaks = 30, main = "Histogram of population and sample book values", xlab = "Book values", las = 1, col = "lightgray")
  graphics::hist(x[["sample"]][[name]], breaks = 30, main = "Sample", xlab = "Book values", las = 1, add = TRUE, col = "darkgray")
  graphics::legend("topright", legend = c("Population", "Sample"), bty = "n", fill = c("lightgray", "darkgray"))
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
  if (is.null(x[["prior"]]) && x[["materiality"]] < 1 && is.null(x[["strata"]]) && x[["method"]] %in% c("binomial", "poisson", "hypergeometric")) {
    out <- c(out, paste("p-value =", format.pval(x[["p.value"]], digits = max(1L, digits - 2L))))
  }
  if (!is.null(x[["prior"]]) && x[["materiality"]] < 1 && is.null(x[["strata"]])) {
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
  cat(paste0("estimate:\n", " ", format(x[["mle"]], digits = digits)), "\n")
  cat(paste0("estimates obtained via method ", if (is.null(x[["prior"]])) paste0("'", x[["method"]], "'\n") else paste0("'", x[["method"]], "' + 'prior'\n")))
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
  if (x[["min.precision"]] < 1) {
    cat(paste("  Min. precision:                ", format(x[["min.precision"]], digits = max(1L, digits - 2L))), "\n")
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
    cat(paste("  Posterior distribution:        ", if (is.null(x[["strata"]]) || x[["pooling"]] == "complete") x[["posterior"]] else "Approximated via MCMC sampling"), "\n")
  }
  cat(paste("  Most likely error:             ", format(x[["mle"]], digits = max(1L, digits - 2L))), "\n")
  if (x[["type"]] == "Bayesian") {
    cat(paste(" ", paste0(format(x[["conf.level"]] * 100), " percent credible interval:   ", paste0("[", format(x[["lb"]], digits = max(1L, digits - 2L)), ", ", format(x[["ub"]], digits = max(1L, digits - 2L)), "]"))), "\n")
  } else if (x[["type"]] == "Classical") {
    cat(paste(" ", paste0(format(x[["conf.level"]] * 100), " percent confidence interval: ", paste0("[", format(x[["lb"]], digits = max(1L, digits - 2L)), ", ", format(x[["ub"]], digits = max(1L, digits - 2L)), "]"))), "\n")
  }
  cat(paste("  Precision:                     ", format(x[["precision"]], digits = max(1L, digits - 2L))), "\n")
  if (x[["materiality"]] < 1 && x[["type"]] == "Bayesian" && (is.null(x[["strata"]]) || x[["pooling"]] == "complete")) {
    cat(paste("  BF\u2081\u2080:\t                         ", format(x[["bf.h1"]], digits = max(1L, digits - 2L))), "\n")
  }
  if (x[["materiality"]] < 1 && x[["type"]] == "Classical" && is.null(x[["strata"]]) && x[["method"]] %in% c("poisson", "binomial", "hypergeometric")) {
    cat(paste("  p-value:                       ", format.pval(x[["p.value"]], digits = max(1L, digits - 2L))), "\n")
  }
  if (!is.null(x[["strata"]])) {
    cat(paste0("\nStrata (", nrow(x[["strata"]]), "):\n"))
    print(format(x[["strata"]], digits = max(1L, digits - 2L), scientific = FALSE), quote = FALSE)
  }
}

#' @rdname jfa-methods
#' @method summary jfaEvaluation
#' @export
summary.jfaEvaluation <- function(object, digits = getOption("digits"), ...) {
  out <- list(
    "conf.level" = round(object[["conf.level"]], digits),
    "materiality" = round(object[["materiality"]], digits),
    "min.precision" = round(object[["min.precision"]], digits),
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
plot.jfaEvaluation <- function(x, xlim = c(0, 1), ...) {
  if (x[["method"]] %in% c("stringer", "stringer-meikle", "stringer-lta", "stringer-pvz", "rohrbach", "moment", "coxsnell")) {
    stop(paste0("No plotting method available for sample evaluation using 'method = ", x[["method"]], "'"))
  }
  if (x[["method"]] %in% c("direct", "difference", "quotient", "regression")) {
    ymin <- x[["mle"]] - 2 * x[["precision"]]
    ymax <- x[["mle"]] + 2 * x[["precision"]]
    graphics::plot(0, type = "n", ylim = c(ymin, ymax), ylab = expression(E), xlim = c(0, 1), bty = "n", xaxt = "n", xlab = "", yaxt = "n", main = paste0(round(x[["conf.level"]] * 100, 2), "% Confidence interval"))
    yBreaks <- base::pretty(c(ymin, ymax), n = 6)
    graphics::axis(side = 2, at = yBreaks, labels = base::format(round(yBreaks), scientific = F, big.mark = ","), las = 1)
    graphics::segments(x0 = 0, x1 = 1, y0 = 0, y1 = 0, lty = 2, col = "gray")
    if (x[["materiality"]] != 1) {
      graphics::segments(x0 = 0, x1 = 1, y0 = (x[["N.units"]] * x[["materiality"]]), y1 = (x[["N.units"]] * x[["materiality"]]), lty = 2, col = "red")
    }
    graphics::points(x = 0.5, y = x[["mle"]], pch = 19)
    graphics::arrows(x0 = 0.5, x1 = 0.5, y0 = x[["lb"]], y1 = x[["ub"]], code = 3, lwd = 2, col = "black", angle = 90)
    graphics::text(x = 0.86, y = x[["mle"]], labels = paste0("Most likely error = ", format(round(x[["mle"]], 2), scientific = FALSE, big.mark = ",")), cex = 0.75, adj = c(1, 0.5))
    graphics::text(x = 0.87, y = x[["lb"]], labels = paste0("Lower bound = ", format(round(x[["lb"]], 2), scientific = FALSE, big.mark = ",")), cex = 0.75, adj = c(1, 0.5))
    graphics::text(x = 0.87, y = x[["ub"]], labels = paste0("Upper bound = ", format(round(x[["ub"]], 2), scientific = FALSE, big.mark = ",")), cex = 0.75, adj = c(1, 0.5))
    graphics::segments(x0 = 0.40, x1 = 0.40, y0 = x[["mle"]], y1 = x[["ub"]], col = "black")
    graphics::segments(x0 = 0.40, x1 = 0.42, y0 = x[["mle"]], y1 = x[["mle"]], col = "black")
    graphics::segments(x0 = 0.40, x1 = 0.42, y0 = x[["ub"]], y1 = x[["ub"]], col = "black")
    graphics::text(x = 0.15, y = (x[["ub"]] - x[["precision"]] / 2), labels = paste0("Precision = ", format(round(x[["precision"]], 2), scientific = FALSE, big.mark = ",")), cex = 0.75, adj = c(0, 0.5))
  } else {
    if (!is.null(x[["prior"]])) {
      if (x[["method"]] == "poisson") {
        xseq <- seq(xlim[1], xlim[2], length.out = 1000)
        d <- stats::dgamma(xseq, shape = x[["prior"]][["description"]]$alpha, rate = x[["prior"]][["description"]]$beta)
        d1 <- stats::dgamma(xseq, shape = x[["posterior"]][["description"]]$alpha, rate = x[["posterior"]][["description"]]$beta)
      } else if (x[["method"]] == "binomial") {
        xseq <- seq(xlim[1], xlim[2], length.out = 1000)
        d <- stats::dbeta(xseq, shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
        d1 <- stats::dbeta(xseq, shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
      } else if (x[["method"]] == "hypergeometric") {
        xseq <- seq(xlim[1], xlim[2], by = 1)
        d <- extraDistr::dbbinom(x = xseq, size = x[["N.units"]], alpha = x[["prior"]][["description"]]$alpha, beta = x[["prior"]][["description"]]$beta)
        d1 <- extraDistr::dbbinom(x = xseq, size = x[["N.units"]] - x[["n"]], alpha = x[["posterior"]][["description"]]$alpha, beta = x[["posterior"]][["description"]]$beta)
      }
      if (x[["method"]] == "poisson" || x[["method"]] == "binomial") {
        graphics::plot(
          x = xseq, y = d1, type = "l", bty = "n", xlab = expression(theta), ylab = "Density", las = 1, ylim = c(0, max(d1)),
          main = paste0(x[["prior"]]$prior, "  \u2192  ", x[["posterior"]]$posterior), axes = FALSE
        )
        graphics::lines(x = xseq, y = d, lty = 2)
        graphics::axis(1, at = pretty(xseq, min.n = 4), labels = round(pretty(xseq, min.n = 4), 2))
        graphics::axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
        graphics::legend("topright", legend = c("Prior", "Posterior"), lty = c(2, 1), bty = "n", cex = 1.2, lwd = 2)
      } else {
        graphics::barplot(d1,
          bty = "n", xlab = "K", ylab = "Probability", las = 1, ylim = c(0, max(d1)), width = 1, space = 0,
          main = paste0(x[["prior"]]$prior, "  \u2192  ", x[["posterior"]]$posterior), axes = FALSE, col = "darkgray"
        )
        graphics::barplot(d, col = "lightgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
        graphics::axis(1, at = pretty(xseq, min.n = 4) + 0.5, labels = pretty(xseq, min.n = 4))
        graphics::axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
        graphics::legend("topright", legend = c("Prior", "Posterior"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2)
      }
    } else {
      stopifnot(
        "missing value for 'materiality'" = x[["materiality"]] != 1,
        "'likelihood' should be one of 'poisson', 'binomial', 'hypergeometric'" = x[["method"]] %in% c("poisson", "binomial", "hypergeometric")
      )
      xseq <- seq(xlim[1], xlim[2], by = 1)
      if (x[["method"]] == "poisson") {
        mainLab <- paste0("Poisson distribution (lambda = ", round(x[["materiality"]] * x[["n"]], 2), ")")
        d <- stats::dpois(x = xseq, lambda = x[["materiality"]] * x[["n"]])
        d1 <- stats::dpois(x = 0:x[["x"]], lambda = x[["materiality"]] * x[["n"]])
      } else if (x[["method"]] == "binomial") {
        mainLab <- paste0("Binomial distribution (n = ", x[["n"]], ", p = ", round(x[["materiality"]], 2), ")")
        d <- stats::dbinom(x = xseq, size = x[["n"]], prob = x[["materiality"]])
        d1 <- stats::dbinom(x = 0:x[["x"]], size = x[["n"]], prob = x[["materiality"]])
      } else if (x[["method"]] == "hypergeometric") {
        mainLab <- paste0("Hypergeometric distribution (N = ", x[["N.units"]], ", n = ", x[["n"]], ", K = ", x[["K"]], ")")
        d <- stats::dhyper(x = xseq, m = x[["K"]], n = x[["N.units"]] - x[["K"]], k = x[["n"]])
        d1 <- stats::dhyper(x = 0:x[["n"]], m = x[["K"]], n = x[["N.units"]] - x[["K"]], k = x[["n"]])
      }
      graphics::barplot(d, xlab = "x", col = "lightgray", ylab = "Probability", las = 1, main = mainLab, width = 1, space = 0)
      graphics::axis(1, at = pretty(xseq, min.n = 4) + 0.5, labels = pretty(xseq, min.n = 4))
      graphics::barplot(d1, col = "darkgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
      graphics::legend("topright", legend = c("Error free", "Errors"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2)
    }
  }
}

# Methods for class: jfaDistr #####################################################

#' @rdname jfa-methods
#' @method print jfaDistr
#' @export
print.jfaDistr <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap("Digit distribution test", prefix = "\t"), sep = "\n")
  cat("\n")
  cat("data:  ", x$data.name, "\n", sep = "")
  out <- character()
  if (!is.null(x$n)) {
    out <- c(out, paste(names(x$n), "=", format(x$n, digits = max(1L, digits - 2L))))
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
#' @method plot jfaDistr
#' @export
plot.jfaDistr <- function(x, ...) {
  p_exp <- x$expected / x$n
  p_obs <- x$observed / x$n
  yTicks <- pretty(c(0, p_exp, p_obs), min.n = 4)
  plot <- graphics::barplot(p_exp,
    las = 1, main = "Observed vs. Expected Distribution", xlab = "Digit", ylab = "Relative frequency",
    names.arg = x$digits, ylim = c(0, max(yTicks)), col = "gray", axes = FALSE
  )
  graphics::legend("topright", legend = c("Observed", "Expected"), fill = c("blue", "gray"), bty = "n")
  xloc <- as.numeric(plot)
  graphics::lines(x = xloc, y = p_obs, cex = 2, col = "blue")
  graphics::points(x = xloc, y = p_obs, cex = if (x$check == "firsttwo") 1 else 1.5, col = "blue", pch = 19)
  graphics::axis(side = 1, at = xloc, labels = rep("", length(x$digits)), pos = -0.01)
  graphics::axis(side = 2, at = yTicks, las = 1)
}

# Methods for class: jfaRv #####################################################

#' @rdname jfa-methods
#' @method print jfaRv
#' @export
print.jfaRv <- function(x, digits = getOption("digits"), ...) {
  cat("\n")
  cat(strwrap("Repeated values test", prefix = "\t"), sep = "\n")
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
  plot <- graphics::barplot(as.numeric(x$frequencies), las = 1, main = "Histogram with Individual Bins", ylab = "Frequency", xlab = "Value", names.arg = "")
  xloc <- as.numeric(plot)
  ticks <- pretty(xloc, min.n = 4)
  graphics::axis(side = 1, at = ticks, labels = round(seq(min(x$x), max(x$x), length.out = length(ticks)), 2))
}
