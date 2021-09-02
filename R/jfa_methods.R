#' Methods for jfa objects
#'
#' Methods defined for objects returned from the \code{\link{auditPrior}}, \code{\link{planning}}, \code{\link{selection}}, and \code{\link{evaluation}} functions.
#'
#' @param object,x   an object of class \code{jfaPrior}, \code{jfaPosterior}, \code{jfaPlanning}, \code{jfaSelection}, or \code{jfaEvaluation}.
#' @param digits     an integer specifying the number of digits to which output should be rounded. Used in \code{summary}.
#' @param xmax       a number between 0 and 1 specifying the x-axis limits of the plot. Used in \code{plot}.
#' @param ...        further arguments, currently ignored.
#'
#' @return
#' The \code{summary} methods return a \code{data.frame} which contains the input and output.
#'
#' The \code{print} methods simply print and return nothing.
#'
#' @name jfa-methods
NULL

# Print methods

#' @rdname jfa-methods
#' @method print jfaPrior
#' @export
print.jfaPrior <- function(x, ...) {
  cat("Probability distribution for audit sampling:", x[["prior"]], "\nDistribution obtained via method", paste0("'", x[["method"]], "'\n"))
}

#' @rdname jfa-methods
#' @method print jfaPosterior
#' @export
print.jfaPosterior <- function(x, ...) {
  cat("Probability distribution for audit sampling:", x[["posterior"]], "\nDistribution obtained via method 'sample'\n")
}

#' @rdname jfa-methods
#' @method print jfaPlanning
#' @export
print.jfaPlanning <- function(x, ...) {
  cat("Minimum sample size:", x[["n"]], "\nSample size obtained in", x[["iterations"]], "iteration(s) via method", paste0("'", x[["likelihood"]], "'\n"))
}

#' @rdname jfa-methods
#' @method print jfaSelection
#' @export
print.jfaSelection <- function(x, ...) {
  cat("Obtained sampling units:", x[["n.units"]], "| Obtained sample items:", x[["n.items"]], "\nSample obtained via methods", paste0("'", x[["units"]], "' + '", x[["method"]], "'\n"))
}

#' @rdname jfa-methods
#' @method print jfaEvaluation
#' @export
print.jfaEvaluation <- function(x, ...) {
  if (x[["method"]] %in% c("direct", "difference", "quotient", "regression")) {
    cat("Most likely error:", round(x[["mle"]], 3), "|", paste0(round(x[["conf.level"]] * 100, 3), "%"), "Interval:", paste0("[", round(x[["lb"]], 3), "; ", round(x[["ub"]], 3), "]"), "| Precision:", round(x[["precision"]], 3), "\nEstimates obtained via method", paste0("'", x[["method"]], "'\n"))
  } else {
    if (!is.null(x[["prior"]]) && x[["materiality"]] != 1) {
      cat("Most likely error:", paste0(round(x[["mle"]] * 100, 3), "%"), "|", paste0(round(x[["conf.level"]] * 100, 3), "%"), "Upper bound:", paste0(round(x[["ub"]] * 100, 3), "%"), "| Precision:", paste0(round(x[["precision"]] * 100, 3), "%"), "| BF-+:", round(x[["posterior"]][["hypotheses"]]$bf, 3), "\nEstimates obtained via method", paste0("'", x[["method"]], "'\n"))
    } else {
      cat("Most likely error:", paste0(round(x[["mle"]] * 100, 3), "%"), "|", paste0(round(x[["conf.level"]] * 100, 3), "%"), "Upper bound:", paste0(round(x[["ub"]] * 100, 3), "%"), "| Precision:", paste0(round(x[["precision"]] * 100, 3), "%"), "\nEstimates obtained via method", paste0("'", x[["method"]], "'\n"))
    }
  }
}

#' @rdname jfa-methods
#' @method print summary.jfaPrior
#' @export
print.summary.jfaPrior <- function(x, ...) {
  cat("# ------------------------------------------------------------
#         jfa Prior Distribution Summary (Bayesian)
# ------------------------------------------------------------
# Input:
#
# Confidence level:             ", paste0(x[["conf.level"]] * 100, "%"),"
# Expected sample errors:       ", paste0(x[["x"]] * 100, "%"),"
# Likelihood:                   ", x[["likelihood"]],"
# Specifics:                    ", switch(x[["method"]],
                                          "none" = "None",
                                          "median" = paste0("p(\u0398 < ", x[["materiality"]], ") = p(\u0398 > ", x[["materiality"]], ") = 0.5"),
                                          "hypotheses" = paste0("p(\u0398 < ", x[["materiality"]], ") = ", x[["p.min"]],"; p(\u0398 > ", x[["materiality"]],") = ", x[["p.plus"]]),
                                          "arm" = paste0("IR = ", x[["ir"]], "; ICR = ", x[["icr"]], "; DR = ", x[["dr"]]),
                                          "bram" = paste0("Mode = ", x[["mode.prior"]], "; Upper bound = ", x[["ub.prior"]]),
                                          "sample" = paste0("Earlier sample of ", x[["n.prior"]], " items with ", x[["x.prior"]], " errors"),
                                          "factor" = paste0("Earlier sample of ", x[["n.prior"]], " items with ", x[["x.prior"]], " errors weighted by ", x[["factor"]]),
                                          "custom" = paste0("\u03B1 = ", x[["alpha"]], "; \u03B2 = ", x[["beta"]])), "
# ------------------------------------------------------------
# Output: 
#
# Prior distribution:           ", x[["prior"]],"
# Implicit sample size:         ", x[["implicit.n"]],"
# Implicit errors:              ", x[["implicit.x"]],"
# ------------------------------------------------------------
# Statistics: 
#
# Upper bound:                  ", x[["ub"]],"
# Precision:                    ", x[["precision"]],"
# Mode:                         ", x[["mode"]],"
# Mean:                         ", x[["mean"]],"
# Median:                       ", x[["median"]],"
# ------------------------------------------------------------"
  )
}

#' @rdname jfa-methods
#' @method print summary.jfaPosterior
#' @export
print.summary.jfaPosterior <- function(x, ...) {
  cat("# ------------------------------------------------------------
#             jfa Posterior Distribution (Bayesian)
# ------------------------------------------------------------
# Input: 
#
# Posterior distribution:       ", x[["posterior"]],"
# ------------------------------------------------------------
# Statistics:
#
# Upper bound:                  ", x[["ub"]],"
# Precision:                    ", x[["precision"]],"
# Mode:                         ", x[["mode"]],"
# Mean:                         ", x[["mean"]],"
# Median:                       ", x[["median"]],"
# ------------------------------------------------------------"
  )
}

#' @rdname jfa-methods
#' @method print summary.jfaPlanning
#' @export
print.summary.jfaPlanning <- function(x, ...) {
  cat("# ------------------------------------------------------------",
      ifelse(x[["type"]] == "Bayesian", yes = "
#              jfa Planning Summary (Bayesian)
# ------------------------------------------------------------", no = "
#              jfa Planning Summary (Classical)
# ------------------------------------------------------------"), "
# Input:
# 
# Confidence level:             ", paste0(x[["conf.level"]] * 100, "%"),"
# Materiality:                  ", ifelse(x[["materiality"]] == 1, yes = "Not specified", no = paste0(x[["materiality"]] * 100, "%")),"
# Minimum precision:            ", ifelse(x[["min.precision"]] == 1, yes = "Not specified", no = paste0(x[["min.precision"]] * 100, "%")),"
# Likelihood:                   ", x[["likelihood"]],
      ifelse(x[["Type"]] == "Bayesian", no = "", yes = paste0("
# Prior distribution:            ", x[["prior"]])),"
# Population size:              ", ifelse(is.null(x[["N.units"]]), yes = "Not specified", no = x[["N.units"]]),"
# Expected sample errors:       ", x[["x"]],"
# ------------------------------------------------------------
# Output:
#
# Sample size:                  ", x[["n"]],
      ifelse(x[["Type"]] == "Bayesian", no = "", yes = paste0("
# Posterior distribution:        ", x[["posterior"]])),"
# ------------------------------------------------------------
# Statistics:
#
# Expected most likely error:   ", paste0(x[["mle"]] * 100, "%"),"
# Expected upper bound:         ", paste0(x[["ub"]] * 100, "%"),"
# Expected precision:           ", paste0(x[["precision"]] * 100, "%"),
      ifelse(x[["type"]] == "Bayesian", no = "", yes = paste0("
# Expected Bayes factor-+:       ", ifelse(x[["materiality"]] == 1, yes = "Requires materiality", no = x[["bf"]]))),"
# ------------------------------------------------------------ ")
}

#' @rdname jfa-methods
#' @method print summary.jfaSelection
#' @export
print.summary.jfaSelection <- function(x, ...) {
  cat("# ------------------------------------------------------------
#                  jfa Selection Summary
# ------------------------------------------------------------
# Input:
#      
# Population size:              ", switch(x[["units"]], "mus" = x[["N.units"]], "items" = x[["N.items"]]),"
# Requested sample size:        ", x[["n.req"]],"
# Sampling units:               ", switch(x[["units"]], "mus" = "Monetary units", "items" = "Items"),"
# Algorithm:                    ", switch(x[["method"]], "random" = "Random sampling", "interval" = "Fixed interval sampling", "cell" = "Cell sampling"), 
      ifelse(x[["method"]] == "interval" || x[["method"]] == "cell", no = "", yes = paste0("
# Interval:                      ", x[["interval"]])),
      ifelse(x[["method"]] == "interval", no = "", yes = paste0("
# Starting point:                ", x[["start"]])),"
# ------------------------------------------------------------ 
# Output:
#
# Obtained sampling units:      ", x[["n.units"]],"
# Obtained sample items:        ", x[["n.items"]],"
# ------------------------------------------------------------
# Statistics:
#
# Proportion n/N:               ", x[["prop.n"]], 
      ifelse(x[["units"]] == "mus", no = "", yes = paste0("
# Percentage of value:           ", paste0(x[["prop.val"]] * 100, "%"))),"
# ------------------------------------------------------------ ") 
}

#' @rdname jfa-methods
#' @method print summary.jfaEvaluation
#' @export
print.summary.jfaEvaluation <- function(x, ...) {
  cat("# ------------------------------------------------------------",
      ifelse(x[["type"]] == "Bayesian", yes = "
#             jfa Evaluation Summary (Bayesian)
# ------------------------------------------------------------", no = "
#             jfa Evaluation Summary (Classical)
# ------------------------------------------------------------"), "
# Input:
#
# Confidence level:             ", paste0(x[["conf.level"]] * 100, "%"),"
# Materiality:                  ", ifelse(x[["materiality"]] == 1, yes = "Not specified", no = paste0(x[["materiality"]] * 100, "%")),"
# Minimum precision:            ", ifelse(x[["min.precision"]] == 1, yes = "Not specified", no = paste0(x[["min.precision"]] * 100, "%")),"
# Population size:              ", ifelse(is.null(x[["N.units"]]), yes = "Not specified", no = x[["N.units"]]),"
# Sample size:                  ", x[["n"]],"
# Sample errors:                ", x[["x"]],
      ifelse(!(x[["method"]] %in% c("direct", "difference", "quotient", "regression")), no = "", yes = paste0("
# Sum of taints:                 ", x[["t"]])), "
# Method:                       ", x[["method"]],
      ifelse(x[["method"]] %in% c("direct", "difference", "quotient", "regression"), no = "", yes = paste0("
# Population book value:         ", format(x[["N.units"]], scientific = FALSE))),
      ifelse(x[["type"]] == "Bayesian", no = "", yes = paste0("
# Prior distribution:            ", x[["prior"]])), "
# ------------------------------------------------------------
# Output:
#",
      ifelse(x[["type"]] == "Bayesian", no = "", yes = paste0("
# Posterior distribution:        ", x[["posterior"]])),
      ifelse(x[["method"]] %in% c("direct", "difference", "quotient", "regression"), no = paste0("
# Most likely error:             ", paste0(x[["mle"]] * 100, "%"), "
# Upper bound:                   ", paste0(x[["ub"]] * 100, "%"),"
# Precision:                     ", paste0(x[["precision"]] * 100, "%")), yes = paste0("
# Most likely error:             ", x[["mle"]], "
# Lower bound:                   ", x[["lb"]],"
# Upper bound:                   ", x[["ub"]],"
# Precision:                     ", x[["precision"]])),
      ifelse(x[["type"]] == "Bayesian", no = "", yes = paste0("
# Bayes factor-+:                ", ifelse(x[["materiality"]] == 1, yes = "Requires materiality", no = x[["bf"]]))),"
# Sufficient evidence:          ", x[["sufficient"]], "
# ------------------------------------------------------------ ")
}

# Summary methods

#' @rdname jfa-methods
#' @method summary jfaPrior
#' @export
summary.jfaPrior <- function(object, digits = 3, ...) {
  out <- data.frame("conf.level" = round(object[["conf.level"]], digits + 2),
                    "x" = round(object[["expected"]], digits + 2),
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
                    stringsAsFactors = FALSE)
  if (object[["method"]] == "median") {
    out[["materiality"]] <- round(object[["materiality"]], digits + 2)
  } else if (object[["method"]] == "arm") {
    out[["ir"]] <- round(object[["specifics"]]$ir, digits)
    out[["icr"]] <- round(object[["specifics"]]$cr, digits)
    out[["dr"]] <- round((1 - object[["conf.level"]]) / (object[["specifics"]]$ir * object[["specifics"]]$cr), digits)
  } else if (object[["method"]] == "bram") {
    out[["mode.prior"]] <- round(object[["specifics"]]$mode, digits)
    out[["ub.prior"]] <- round(object[["specifics"]]$ub, digits)
  } else if (object[["method"]] == "hypotheses") {
    out[["materiality"]] <- round(object[["materiality"]], digits + 2)
    out[["p.min"]] <- round(object[["specifics"]]$p.min, digits)
    out[["p.plus"]] <- round(object[["specifics"]]$p.plus, digits)
  } else if (object[["method"]] == "sample" || object[["method"]] == "factor") {
    out[["n.prior"]] <- object[["specifics"]]$n
    out[["x.prior"]] <- object[["specifics"]]$x
    if (object[["method"]] == "factor")
      out[["factor"]] <- round(object[["specifics"]]$factor, digits)
  } else if (object[["method"]] == 'custom') {
    out[["alpha"]] <- round(object[["specifics"]]$alpha, digits)
    out[["beta"]] <- round(object[["specifics"]]$beta, digits)
  }
  class(out) <- c("summary.jfaPrior", "data.frame")
  return(out)
}

#' @rdname jfa-methods
#' @method summary jfaPosterior
#' @export 
summary.jfaPosterior <- function(object, digits = 3, ...) {
  out <- data.frame("posterior" = object[["posterior"]],
                    "ub" = round(object[["statistics"]]$ub, digits),
                    "precision" = round(object[["statistics"]]$precision, digits),
                    "mode" = round(object[["statistics"]]$mode, digits),
                    "mean" = round(object[["statistics"]]$mean, digits),
                    "median" = round(object[["statistics"]]$median, digits),
                    stringsAsFactors = FALSE)
  class(out) <- c("summary.jfaPosterior", "data.frame")
  return(out)
}

#' @rdname jfa-methods
#' @method summary jfaPlanning
#' @export
summary.jfaPlanning <- function(object, digits = 3, ...) {
  out <- data.frame("conf.level" = round(object[["conf.level"]], digits + 2),
                    "materiality" = round(object[["materiality"]], digits + 2),
                    "min.precision" = round(object[["min.precision"]], digits + 2),
                    "likelihood" = object[["likelihood"]],
                    "x" = round(object[["x"]], digits),
                    "n" = object[["n"]],
                    "ub" = round(object[["ub"]], digits + 2),
                    "precision" = round(object[["precision"]], digits + 2),
                    stringsAsFactors = FALSE)
  if (!is.null(object[["N.units"]]))
    out[["N.units"]] <- object[["N.units"]]
  if (inherits(object[["prior"]], "jfaPrior")) {
    out[["prior"]] <- object[["prior"]]$prior
    out[["posterior"]] <- object[["posterior"]]$posterior
    out[["mle"]] <- round(object[["posterior"]][["statistics"]]$mode, digits + 2)
    if (object[["materiality"]] != 1)
      out[["bf"]] <- round(object[["posterior"]][["hypotheses"]]$bf, digits)
  } else {
    out[["mle"]] <- round(object[["x"]] / object[["n"]], digits + 2)
  }
  out[["type"]] <- if (inherits(object[["prior"]], "jfaPrior")) "Bayesian" else "Classical"
  class(out) <- c("summary.jfaPlanning", "data.frame")
  return(out)
}

#' @rdname jfa-methods
#' @method summary jfaSelection
#' @export
summary.jfaSelection <- function(object, digits = 3, ...) {
  out <- data.frame("N.items" = object[["N.items"]],
                    "N.units" = round(object[["N.units"]], digits),
                    "n.req" = object[["n.req"]],
                    "units" = object[["units"]],
                    "method" = object[["method"]],
                    "n.units" = object[["n.units"]],
                    "n.items" = object[["n.items"]],
                    "prop.n" = round(object[["n.items"]] / object[["N.items"]], digits),
                    stringsAsFactors = FALSE)
  if (object[["method"]] == "interval" || object[["method"]] == "cell") {
    out[["interval"]] <- round(object[["interval"]], digits)
    if (object[["method"]] == "interval")
      out[["start"]] <- object[["start"]]
  }
  if (object[["units"]] == "mus") 
    out[["prop.val"]] <- round(object[["n.units"]] / object[["N.units"]], digits + 2)
  class(out) <- c("summary.jfaSelection", "data.frame")
  return(out)
}

#' @rdname jfa-methods
#' @method summary jfaEvaluation
#' @export
summary.jfaEvaluation <- function(object, digits = 3, ...) {
  out <- data.frame("conf.level" = round(object[["conf.level"]], digits + 2),
                    "materiality" = round(object[["materiality"]], digits + 2),
                    "min.precision" = round(object[["min.precision"]], digits + 2),
                    "x" = object[["x"]],
                    "n" = object[["n"]],
                    "method" = object[["method"]],
                    "mle" = round(object[["mle"]], digits + 2),
                    "precision" = round(object[["precision"]], digits + 2),
                    "sufficient" = object[["sufficient"]],
                    stringsAsFactors = FALSE)
  if (!is.null(object[["N.units"]]))
    out[["N.units"]] <- object[["N.units"]]
  if (object[["method"]] %in% c("direct", "difference", "quotient", "regression")) {
    out[["type"]] <- "Classical"
    out[["lb"]] <- round(object[["lb"]], digits)
    out[["ub"]] <- round(object[["ub"]], digits)
  } else {
    out[["t"]] <- round(object[["t"]], digits)
    out[["ub"]] <- round(object[["ub"]], digits + 2)
    if (!is.null(object[["prior"]])) {
      out[["type"]] <- "Bayesian"
      out[["prior"]] <- object[["prior"]][["prior"]]
      out[["posterior"]] <- object[["posterior"]][["posterior"]]
      if (object[["materiality"]] != 1)
        out[["bf"]] <- round(object[["posterior"]][["hypotheses"]]$bf, digits)
    } else {
      out[["type"]] <- "Classical"
    }
  }
  class(out) <- c("summary.jfaEvaluation", "data.frame")
  return(out)
}

# Plot methods

#' @rdname jfa-methods
#' @method plot jfaPrior
#' @export
plot.jfaPrior <- function(x, xmax = 0.5, ...) {
  xseq   <- seq(0, xmax, length.out = 1000)
  if (x[["description"]]$density == "gamma") {
    d    <- stats::dgamma(xseq, shape = x[["description"]]$alpha, rate = x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta") {
    d    <- stats::dbeta(xseq, shape1 = x[["description"]]$alpha, shape2 = x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta-binomial") {
    xlim <- ceiling(xmax * x[["N.units"]])
    xseq <- seq(0, xlim, by = 1)
    d    <- .dBetaBinom(x = xseq, N = x[["N.units"]], shape1 = x[["description"]]$alpha, shape2 = x[["description"]]$beta)
  }
  if (x[["description"]]$density == "gamma" || x[["description"]]$density == "beta") {
    mainLab <- switch(x[["description"]]$density,
                      "gamma" = paste0("gamma prior distribution (shape = ", round(x[["description"]]$alpha, 3), ", rate = ", round(x[["description"]]$beta, 3), ")"),
                      "beta" = paste0("beta prior distribution (alpha = ", round(x[["description"]]$alpha, 3), ", beta = ", round(x[["description"]]$beta, 3), ")"))
    graphics::plot(x = xseq, y = d, type = "l", lwd = 2, bty = "n", xlab = expression(theta), ylab = "Density", las = 1, ylim = c(0, max(d)), main = mainLab, axes = FALSE, lty = 2)
    graphics::axis(1, at = pretty(xseq, min.n = 4), labels = paste0(round(pretty(xseq, min.n = 4) * 100, 2), "%"))
    graphics::axis(2, at = c(0, max(d)), labels = FALSE, las = 1, lwd.ticks = 0)
    graphics::legend("topright", legend = "Prior", lty = 2, bty = "n", cex = 1.2, lwd = 2)
  } else {
    mainLab <- paste0("beta-binomial prior distribution (N = ", x[["N.units"]], ", alpha = ", round(x[["description"]]$alpha, 3), ", beta = ", round(x[["description"]]$beta, 3), ")")
    graphics::barplot(d, bty = "n", xlab = "K", ylab = "Probability", las = 1, ylim = c(0, max(d)), width = 1, space = 0, main = mainLab, axes = FALSE, col = "darkgray")
    graphics::axis(1, at = pretty(xseq, min.n = 4) + 0.5, labels = pretty(xseq, min.n = 4))
    graphics::axis(2, at = c(0, max(d)), labels = FALSE, las = 1, lwd.ticks = 0)
    graphics::legend("topright", legend = "Prior", fill = "lightgray", bty = "n", cex = 1.2) 
  }
}

#' @rdname jfa-methods
#' @method plot jfaPosterior
#' @export
plot.jfaPosterior <- function(x, xmax = 0.5, ...) {
  xseq   <- seq(0, xmax, length.out = 1000)
  if (x[["description"]]$density == "gamma") {
    d    <- stats::dgamma(xseq, shape = x[["description"]]$alpha, rate = x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta") {
    d    <- stats::dbeta(xseq, shape1 = x[["description"]]$alpha, shape2 = x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta-binomial") {
    xlim <- ceiling(xmax * x[["N.units"]])
    xseq <- seq(0, xlim, by = 1)
    d    <- c(rep(0, x[["description"]]$x), .dBetaBinom(x = xseq, N = x[["N.units"]] - x[["description"]]$n, shape1 = x[["description"]]$alpha, shape2 = x[["description"]]$beta))
  }
  if (x[["description"]]$density == "gamma" || x[["description"]]$density == "beta") {
    mainLab <- switch(x[["description"]]$density,
                      "gamma" = paste0("gamma posterior distribution (shape = ", round(x[["description"]]$alpha, 3), ", rate = ", round(x[["description"]]$beta, 3), ")"),
                      "beta" = paste0("beta posterior distribution (alpha = ", round(x[["description"]]$alpha, 3), ", beta = ", round(x[["description"]]$beta, 3), ")"))
    graphics::plot(x = xseq, y = d, type = "l", lwd = 2, bty = "n", xlab = expression(theta), ylab = "Density", las = 1, ylim = c(0, max(d)), main = mainLab, axes = FALSE, lty = 2)
    graphics::axis(1, at = pretty(xseq, min.n = 4), labels = paste0(round(pretty(xseq, min.n = 4) * 100, 2), "%"))
    graphics::axis(2, at = c(0, max(d)), labels = FALSE, las = 1, lwd.ticks = 0)
  } else {
    mainLab <- paste0("beta-binomial posterior distribution (N = ", x[["N.units"]] - x[["description"]]$n, ", alpha = ", round(x[["description"]]$alpha, 3), ", beta = ", round(x[["description"]]$beta, 3), ")")
    graphics::barplot(d, bty = "n", xlab = "K", ylab = "Probability", las = 1, ylim = c(0, max(d)), width = 1, space = 0, main = mainLab, axes = FALSE, col = "darkgray")
    graphics::axis(1, at = pretty(xseq, min.n = 4) + 0.5, labels = pretty(xseq, min.n = 4))
    graphics::axis(2, at = c(0, max(d)), labels = FALSE, las = 1, lwd.ticks = 0) 
  }
}

#' @rdname jfa-methods
#' @method plot jfaPlanning
#' @export
plot.jfaPlanning <- function(x, xmax = 0.5, ...) {
  if (!is.null(x[["prior"]])) {
    xseq         <- seq(0, xmax, length.out = 1000)
    if (x[["likelihood"]] == "poisson") {
      d          <- stats::dgamma(xseq, shape = x[["prior"]][["description"]]$alpha, rate = x[["prior"]][["description"]]$beta)
      d1         <- stats::dgamma(xseq, shape = x[["posterior"]][["description"]]$alpha, rate = x[["posterior"]][["description"]]$beta)
      bound      <- stats::qgamma(x[["conf.level"]], shape = x[["posterior"]][["description"]]$alpha, rate = x[["posterior"]][["description"]]$beta)
    } else if (x[["likelihood"]] == "binomial") {
      d          <- stats::dbeta(xseq, shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
      d1         <- stats::dbeta(xseq, shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
      bound      <- stats::qbeta(x[["conf.level"]], shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
    } else if (x[["likelihood"]] == "hypergeometric") {
      xlim_prior <- ceiling(xmax * x[["N.units"]])
      xseq_prior <- seq(0, xlim_prior, by = 1)
      xlim_post  <- min(xlim_prior, x[["N.units"]] - x[["n"]])
      xseq_post  <- seq(0, xlim_post, by = 1)
      d          <- .dBetaBinom(x = xseq_prior, N = x[["N.units"]], shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
      d1         <- c(rep(0, x[["x"]]), .dBetaBinom(x = xseq_post, N = x[["N.units"]] - x[["n"]], shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta))
      bound      <- .qBetaBinom(p = x[["conf.level"]], N = x[["N.units"]] - x[["n"]], shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
    }
    if (x$likelihood == "poisson" || x$likelihood == "binomial") {
      graphics::plot(x = xseq, y = d1, type = "l", lwd = 2, bty = "n", xlab = expression(theta), ylab = "Density", las = 1, ylim = c(0, max(d1)),
                     main = paste0(x[["prior"]][["description"]]$density, " prior and expected posterior distribution"), axes = FALSE)
      graphics::polygon(x = c(0, xseq[xseq<=bound], xseq[xseq<=bound][length(xseq[xseq<=bound])]), y = c(0, d1[xseq<=bound], 0), col="lightgray", border = NA)
      graphics::lines(x = xseq, y = d, lwd = 2, lty = 2)
      graphics::axis(1, at = pretty(xseq, min.n = 4), labels = paste0(round(pretty(xseq, min.n = 4) * 100, 2), "%"))
      graphics::axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
      graphics::legend("topright", legend = c("Prior", "Expected posterior"), lty = c(2, 1), bty = "n", cex = 1.2, lwd = 2)
    } else {
      graphics::barplot(d1, bty = "n", xlab = "K", ylab = "Probability", las = 1, ylim = c(0, max(d1)), width = 1, space = 0,
                        main = paste0(x[["prior"]][["description"]]$density, " prior and expected posterior distribution"), axes = FALSE, col = "darkgray")
      graphics::barplot(d, col = "lightgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
      graphics::axis(1, at = pretty(xseq_prior, min.n = 4) + 0.5, labels = pretty(xseq_prior, min.n = 4))
      graphics::axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
      graphics::legend("topright", legend = c("Prior", "Expected posterior"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
    }
  } else {
    xseq <- seq(0, ceiling(xmax * x[["n"]]), by = 1)
    if (x[["likelihood"]] == "poisson") {
      mainLab <- paste0("Poisson distribution (lambda = ", round(x[["materiality"]] * x[["n"]], 2), ")")
      d       <- stats::dpois(x = xseq, lambda = x[["materiality"]] * x[["n"]])
      d1      <- stats::dpois(x = 0:x[["x"]], lambda = x[["materiality"]] * x[["n"]])
    } else if (x[["likelihood"]] == "binomial") {
      mainLab <- paste0("Binomial distribution (n = ", x[["n"]], ", p = ", x[["materiality"]],")")
      d       <- stats::dbinom(x = xseq, size = x[["n"]], prob = x[["materiality"]])
      d1      <- stats::dbinom(x = 0:x[["x"]], size = x[["n"]], prob = x[["materiality"]])
    } else if (x$likelihood == "hypergeometric") {
      mainLab <- paste0("Hypergeometric distribution (N = ", x[["N.units"]], ", n = ", x[["n"]], ", K = ", x[["K"]], ")")
      d       <- stats::dhyper(x = xseq, m = x[["K"]], n = x[["N.units"]] - x[["K"]], k = x[["n"]])
      d1      <- stats::dhyper(x = 0:x[["x"]], m = x[["K"]], n = x[["N.units"]] - x[["K"]], k = x[["n"]])
    }
    graphics::barplot(d, xlab = "x", col = "lightgray", ylab = "Probability", las = 1, main = mainLab, width = 1, space = 0)
    graphics::axis(1, at = pretty(xseq, min.n = 4) + 0.5, labels = pretty(xseq, min.n = 4))
    graphics::barplot(d1, col = "darkgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
    graphics::legend("topright", legend = c("Expected error free", "Expected errors"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
  }
}

#' @rdname jfa-methods
#' @method plot jfaSelection
#' @export
plot.jfaSelection <- function(x, ...) {
  if (x[["units"]] == "items")
    stop("No plotting method available for record sampling")
  name <- x[["values"]]
  graphics::hist(x[["data"]][[name]], breaks = 30, main = "Histogram of population and sample book values", xlab = "Book values", las = 1, col = "lightgray")
  graphics::hist(x[["sample"]][[name]], breaks = 30, main = "Sample", xlab = "Book values", las = 1, add = TRUE, col = "darkgray")
  graphics::legend("topright", legend = c("Population", "Sample"), bty = "n", fill = c("lightgray", "darkgray"))
}

#' @rdname jfa-methods
#' @method plot jfaEvaluation
#' @export
plot.jfaEvaluation <- function(x, xmax = 0.5, ...) {
  if (x[["method"]] %in% c("stringer", "stringer-meikle", "stringer-lta", "stringer-pvz", "rohrbach", "moment", "coxsnell"))
    stop(paste0("No plotting method available for a sample evaluation using 'method = ", x[["method"]], "'"))
  if (x[["method"]] %in% c("direct", "difference", "quotient", "regression")) {
    ymin <- x[["mle"]] - 2 * x[["precision"]]
    ymax <- x[["mle"]] + 2 * x[["precision"]]
    graphics::plot(0, type = "n", ylim = c(ymin, ymax), ylab = expression(E), xlim = c(0, 1), bty = "n", xaxt = "n", xlab = "", yaxt = "n", main = paste0(round(x[["conf.level"]] * 100, 2), "% Confidence interval"))
    yBreaks <- base::pretty(c(ymin, ymax), n = 6)
    graphics::axis(side = 2, at = yBreaks, labels = base::format(round(yBreaks), scientific = F, big.mark = ","), las = 1)
    graphics::segments(x0 = 0, x1 = 1, y0 = 0, y1 = 0, lty = 2, col = "gray")
    if (x[["materiality"]] != 1)
      graphics::segments(x0 = 0, x1 = 1, y0 = (x[["N.units"]] * x[["materiality"]]), y1 = (x[["N.units"]] * x[["materiality"]]), lty = 2, col = "red")
    graphics::points(x = 0.5, y = x[["mle"]], pch = 19)
    graphics::arrows(x0 = 0.5, x1 = 0.5, y0 = x[["lb"]], y1 = x[["ub"]], code = 3, lwd = 2, col = "black", angle = 90)
    graphics::text(x = 0.86, y = x[["mle"]], labels = paste0("Most likely error = ", format(round(x[["mle"]], 2), scientific = FALSE, big.mark = ",")), cex = 0.75, adj = c(1, 0.5))
    graphics::text(x = 0.87, y = x[["lb"]], labels = paste0("Lower bound = ", format(round(x[["lb"]], 2), scientific = FALSE, big.mark = ",")), cex = 0.75, adj = c(1, 0.5))
    graphics::text(x = 0.87, y = x[["ub"]], labels = paste0("Upper bound = ", format(round(x[["ub"]], 2), scientific = FALSE, big.mark = ",")), cex = 0.75, adj = c(1, 0.5))
    graphics::segments(x0 = 0.40, x1 = 0.40, y0 = x[["mle"]], y1 = x[["ub"]], col = "black")
    graphics::segments(x0 = 0.40, x1 = 0.42, y0 = x[["mle"]], y1 = x[["mle"]], col = "black")
    graphics::segments(x0 = 0.40, x1 = 0.42, y0 = x[["ub"]], y1 = x[["ub"]], col = "black")
    graphics::text(x = 0.15, y = (x[["ub"]] - x[["precision"]]/2), labels = paste0("Precision = ", format(round(x[["precision"]], 2), scientific = FALSE, big.mark = ",")), cex = 0.75, adj = c(0, 0.5))
  } else {
    if (!is.null(x[["prior"]])) {
      xseq         <- seq(0, xmax, length.out = 1000)
      if (x[["method"]] == "poisson") {
        d          <- stats::dgamma(xseq, shape = x[["prior"]][["description"]]$alpha, rate = x[["prior"]][["description"]]$beta)
        d1         <- stats::dgamma(xseq, shape = x[["posterior"]][["description"]]$alpha, rate = x[["posterior"]][["description"]]$beta)
        bound      <- stats::qgamma(x[["conf.level"]], shape = x[["posterior"]][["description"]]$alpha, rate = x[["posterior"]][["description"]]$beta)
      } else if (x[["method"]] == "binomial") {
        d          <- stats::dbeta(xseq, shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
        d1         <- stats::dbeta(xseq, shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
        bound      <- stats::qbeta(x[["conf.level"]], shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
      } else if (x[["method"]] == "hypergeometric") {
        xlim_prior <- ceiling(xmax * x[["N.units"]])
        xseq_prior <- seq(0, xlim_prior, by = 1)
        xlim_post  <- min(xlim_prior, x[["N.units"]] - x[["n"]])
        xseq_post  <- seq(0, xlim_post, by = 1)
        d          <- .dBetaBinom(x = xseq_prior, N = x[["N.units"]], shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
        d1         <- c(rep(0, x[["x"]]), .dBetaBinom(x = xseq_post, N = x[["N.units"]] - x[["n"]], shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta))
        bound      <- .qBetaBinom(p = x[["conf.level"]], N = x[["N.units"]] - x[["n"]], shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
      }
      if (x[["method"]] == "poisson" || x[["method"]] == "binomial") {
        graphics::plot(x = xseq, y = d1, type = "l", lwd = 2, bty = "n", xlab = expression(theta), ylab = "Density", las = 1, ylim = c(0, max(d1)),
                       main = paste0(x[["prior"]][["description"]]$density, " prior and posterior distribution"), axes = FALSE)
        graphics::polygon(x = c(0, xseq[xseq<=bound], xseq[xseq<=bound][length(xseq[xseq<=bound])]), y = c(0, d1[xseq<=bound], 0), col="lightgray", border = NA)
        graphics::lines(x = xseq, y = d, lwd = 2, lty = 2)
        graphics::axis(1, at = pretty(xseq, min.n = 4), labels = paste0(round(pretty(xseq, min.n = 4) * 100, 2), "%"))
        graphics::axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
        graphics::legend("topright", legend = c("Prior", "Posterior"), lty = c(2, 1), bty = "n", cex = 1.2, lwd = 2)
      } else {
        graphics::barplot(d1, bty = "n", xlab = "K", ylab = "Probability", las = 1, ylim = c(0, max(d1)), width = 1, space = 0,
                          main = paste0(x[["prior"]][["description"]]$density, " prior and posterior distribution"), axes = FALSE, col = "darkgray")
        graphics::barplot(d, col = "lightgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
        graphics::axis(1, at = pretty(xseq_prior, min.n = 4) + 0.5, labels = pretty(xseq_prior, min.n = 4))
        graphics::axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
        graphics::legend("topright", legend = c("Prior", "Posterior"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
      }
    } else {
      if (x[["materiality"]] == 1)
        stop("'materiality' is missing for plotting")
      if (!(x[["method"]] %in% c("poisson", "binomial", "hypergeometric")))
        stop("'likelihood' should be one of 'poisson', 'binomial', 'hypergeometric'")
      xseq      <- seq(0, ceiling(xmax * x[["n"]]), by = 1)
      if (x[["method"]] == "poisson") {
        mainLab <- paste0("Poisson distribution (lambda = ", round(x[["materiality"]] * x[["n"]], 2), ")")
        d       <- stats::dpois(x = xseq, lambda = x[["materiality"]] * x[["n"]])
        d1      <- stats::dpois(x = 0:x[["x"]], lambda = x[["materiality"]] * x[["n"]])
      } else if (x[["method"]] == "binomial") {
        mainLab <- paste0("Binomial distribution (n = ", x[["n"]], ", p = ", round(x[["materiality"]], 2),")")
        d       <- stats::dbinom(x = xseq, size = x[["n"]], prob = x[["materiality"]])
        d1      <- stats::dbinom(x = 0:x[["x"]], size = x[["n"]], prob = x[["materiality"]])
      } else if (x[["method"]] == "hypergeometric") {
        mainLab <- paste0("Hypergeometric distribution (N = ", x[["N.units"]], ", n = ", x[["n"]], ", K = ", x[["K"]], ")")
        d       <- stats::dhyper(x = xseq, m = x[["K"]], n = x[["N.units"]] - x[["K"]], k = x[["n"]])
        d1      <- stats::dhyper(x = 0:x[["n"]], m = x[["K"]], n = x[["N.units"]] - x[["K"]], k = x[["n"]])
      }
      graphics::barplot(d, xlab = "x", col = "lightgray", ylab = "Probability", las = 1, main = mainLab, width = 1, space = 0)
      graphics::axis(1, at = pretty(xseq, min.n = 4) + 0.5, labels = pretty(xseq, min.n = 4))
      graphics::barplot(d1, col = "darkgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
      graphics::legend("topright", legend = c("Error free", "Errors"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
    }
  }
}