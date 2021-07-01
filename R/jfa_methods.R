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
  cat("Probability distribution for audit sampling:", x[["prior"]], "\nDistribution obtained via method", paste0("'", x[["method"]], "'."))
}

#' @rdname jfa-methods
#' @method print jfaPosterior
#' @export
print.jfaPosterior <- function(x, ...) {
  cat("Probability distribution for audit sampling:", x[["posterior"]], "\nDistribution obtained via method 'sample'.")
}

#' @rdname jfa-methods
#' @method print jfaPlanning
#' @export
print.jfaPlanning <- function(x, ...) {
  cat("Minimum sample size:", x[["sampleSize"]], "\nSample size obtained in", x[["iterations"]], "iteration(s) via method", paste0("'", x[["likelihood"]], "'."))
}

#' @rdname jfa-methods
#' @method print jfaSelection
#' @export
print.jfaSelection <- function(x, ...) {
  cat("Obtained sampling units:", sum(x[["sample"]][["count"]]), "| Sample items:", x[["obtainedSampleSize"]], "\nSample obtained via methods", paste0("'", x[["units"]], "' + '", x[["algorithm"]], "'."))
}

#' @rdname jfa-methods
#' @method print jfaEvaluation
#' @export
print.jfaEvaluation <- function(x, ...) {
  if (x[["method"]] %in% c("direct", "difference", "quotient", "regression")) {
    cat("Most likely error:", round(x[["mle"]], 3), "|", paste0(round(x[["confidence"]] * 100, 3), "%"), "Interval:", paste0("[", round(x[["lowerBound"]], 3), "; ", round(x[["upperBound"]], 3), "]"), "| Precision:", round(x[["precision"]], 3), "\nEstimates obtained via method", paste0("'", x[["method"]], "'."))
  } else {
    if (!is.null(x[["prior"]]) && x[["materiality"]] != 1) {
      cat("Most likely error:", paste0(round(x[["mle"]] * 100, 3), "%"), "|", paste0(round(x[["confidence"]] * 100, 3), "%"), "Upper bound:", paste0(round(x[["confBound"]] * 100, 3), "%"), "| Precision:", paste0(round(x[["precision"]] * 100, 3), "%"), "| BF-+:", round(x[["posterior"]][["hypotheses"]]$bf, 3), "\nEstimates obtained via method", paste0("'", x[["method"]], "'."))
    } else {
      cat("Most likely error:", paste0(round(x[["mle"]] * 100, 3), "%"), "|", paste0(round(x[["confidence"]] * 100, 3), "%"), "Upper bound:", paste0(round(x[["confBound"]] * 100, 3), "%"), "| Precision:", paste0(round(x[["precision"]] * 100, 3), "%"), "\nEstimates obtained via method", paste0("'", x[["method"]], "'."))
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
# Confidence:                   ", paste0(x[["Confidence"]] * 100, "%"),"
# Expected sample errors:       ", paste0(x[["Expected_Sample_Errors"]] * 100, "%"),"
# Likelihood:                   ", x[["Likelihood"]],"
# Specifics:                    ", switch(x[["Method"]],
                                          "none" = "None",
                                          "median" = paste0("p(\u0398 < ", x[["Materiality"]], ") = p(\u0398 > ", x[["Materiality"]], ") = 0.5"),
                                          "hypotheses" = paste0("p(\u0398 < ", x[["Materiality"]], ") = ", x[["p_Hmin"]],"; p(\u0398 > ", x[["Materiality"]],") = ", x[["p_Hplus"]]),
                                          "arm" = paste0("IR = ", x[["IR"]], "; ICR = ", x[["ICR"]], "; DR = ", x[["DR"]]),
                                          "bram" = paste0("Mode = ", x[["Mode_Prior"]], "; Upper bound = ", x[["UB_Prior"]]),
                                          "sample" = paste0("Earlier sample of ", x[["N_Prior"]], " items with ", x[["K_Prior"]], " errors"),
                                          "factor" = paste0("Earlier sample of ", x[["N_Prior"]], " items with ", x[["K_Prior"]], " errors weighted by ", x[["Factor"]])), "
# ------------------------------------------------------------
# Output: 
#
# Prior distribution:           ", x[["Prior"]],"
# Implicit sample size:         ", x[["Implicit_Sample_Size"]],"
# Implicit errors:              ", x[["Implicit_Sample_Errors"]],"
# ------------------------------------------------------------
# Statistics: 
#
# Upper bound:                  ", x[["UB"]],"
# Precision:                    ", x[["Precision"]],"
# Mode:                         ", x[["Mode"]],"
# Mean:                         ", x[["Mean"]],"
# Median:                       ", x[["Median"]],"
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
# Posterior distribution:       ", x[["Posterior"]],"
# ------------------------------------------------------------
# Statistics:
#
# Upper bound:                  ", x[["UB"]],"
# Precision:                    ", x[["Precision"]],"
# Mode:                         ", x[["Mode"]],"
# Mean:                         ", x[["Mean"]],"
# Median:                       ", x[["Median"]],"
# ------------------------------------------------------------"
  )
}

#' @rdname jfa-methods
#' @method print summary.jfaPlanning
#' @export
print.summary.jfaPlanning <- function(x, ...) {
  cat("# ------------------------------------------------------------",
      ifelse(x[["Type"]] == "Bayesian", yes = "
#              jfa Planning Summary (Bayesian)
# ------------------------------------------------------------", no = "
#              jfa Planning Summary (Classical)
# ------------------------------------------------------------"), "
# Input:
# 
# Confidence:                   ", paste0(x[["Confidence"]] * 100, "%"),"
# Materiality:                  ", ifelse(x[["Materiality"]] == 1, yes = "Not specified", no = paste0(x[["Materiality"]] * 100, "%")),"
# Minimum precision:            ", ifelse(x[["Minimum_Precision"]] == 1, yes = "Not specified", no = paste0(x[["Minimum_Precision"]] * 100, "%")),"
# Likelihood:                   ", x[["Likelihood"]],
      ifelse(x[["Type"]] == "Bayesian", no = "", yes = paste0("
# Prior distribution:            ", x[["Prior"]])),"
# Expected sample errors:       ", x[["Expected_Sample_Errors"]],"
# ------------------------------------------------------------
# Output:
#
# Sample size:                  ", x[["Sample_Size"]],
      ifelse(x[["Type"]] == "Bayesian", no = "", yes = paste0("
# Posterior distribution:        ", x[["Posterior"]])),"
# ------------------------------------------------------------
# Statistics:
#
# Expected most likely error:   ", paste0(x[["Expected_MLE"]] * 100, "%"),"
# Expected upper bound:         ", paste0(x[["Expected_UB"]] * 100, "%"),"
# Expected precision:           ", paste0(x[["Expected_Precision"]] * 100, "%"),
      ifelse(x[["Type"]] == "Bayesian", no = "", yes = paste0("
# Expected Bayes factor-+:       ", ifelse(x[["Materiality"]] == 1, yes = "Requires materiality", no = x[["Expected_BF"]]))),"
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
# Population size:              ", x[["Population_Size"]],"
# Requested sample size:        ", x[["Requested_Sample_Size"]],"
# Sampling units:               ", switch(x[["Units"]], "mus" = "Monetary units", "records" = "Records"),"
# Algorithm:                    ", switch(x[["Algorithm"]], "random" = "Random sampling", "interval" = "Fixed interval sampling", "cell" = "Cell sampling"), 
      ifelse(x[["Algorithm"]] == "interval" || x[["Algorithm"]] == "cell", no = "", yes = paste0("
# Interval:                      ", x[["Interval"]])),
      ifelse(x[["Algorithm"]] == "interval", no = "", yes = paste0("
# Starting point:                ", x[["Starting_Point"]])),"
# ------------------------------------------------------------ 
# Output:
#
# Obtained sampling units:      ", x[["Obtained_Sampling_Units"]],"
# Obtained sample items:        ", x[["Obtained_Sample_Items"]],"
# ------------------------------------------------------------
# Statistics:
#
# Proportion n/N:               ", x[["Proportion_Total"]], 
      ifelse(x[["Units"]] == "mus", no = "", yes = paste0("
# Percentage of value:           ", paste0(x[["Proportion_Value"]] * 100, "%"))),"
# ------------------------------------------------------------ ") 
}

#' @rdname jfa-methods
#' @method print summary.jfaEvaluation
#' @export
print.summary.jfaEvaluation <- function(x, ...) {
  cat("# ------------------------------------------------------------",
      ifelse(x[["Type"]] == "Bayesian", yes = "
#             jfa Evaluation Summary (Bayesian)
# ------------------------------------------------------------", no = "
#             jfa Evaluation Summary (Classical)
# ------------------------------------------------------------"), "
# Input:
#
# Confidence:                   ", paste0(x[["Confidence"]] * 100, "%"),"
# Materiality:                  ", ifelse(x[["Materiality"]] == 1, yes = "Not specified", no = paste0(x[["Materiality"]] * 100, "%")),"
# Minimum precision:            ", ifelse(x[["Minimum_Precision"]] == 1, yes = "Not specified", no = paste0(x[["Minimum_Precision"]] * 100, "%")),"
# Sample size:                  ", x[["Sample_Size"]],"
# Sample errors:                ", x[["Sample_Errors"]],
      ifelse(!(x[["Method"]] %in% c("direct", "difference", "quotient", "regression")), no = "", yes = paste0("
# Sum of taints:                 ", x[["Sum_Taints"]])), "
# Method:                       ", x[["Method"]],
      ifelse(x[["Method"]] %in% c("direct", "difference", "quotient", "regression"), no = "", yes = paste0("
# Population book value:         ", format(x[["Population_Value"]], scientific = FALSE))),
      ifelse(x[["Type"]] == "Bayesian", no = "", yes = paste0("
# Prior distribution:            ", x[["Prior"]])), "
# ------------------------------------------------------------
# Output:
#",
      ifelse(x[["Type"]] == "Bayesian", no = "", yes = paste0("
# Posterior distribution:        ", x[["Posterior"]])),
      ifelse(x[["Method"]] %in% c("direct", "difference", "quotient", "regression"), no = paste0("
# Most likely error:             ", paste0(x[["MLE"]] * 100, "%"), "
# Upper bound:                   ", paste0(x[["UB"]] * 100, "%"),"
# Precision:                     ", paste0(x[["Precision"]] * 100, "%")), yes = paste0("
# Most likely error:             ", x[["MLE"]], "
# Lower bound:                   ", x[["LB"]],"
# Upper bound:                   ", x[["UB"]],"
# Precision:                     ", x[["Precision"]])),
      ifelse(x[["Type"]] == "Bayesian", no = "", yes = paste0("
# Bayes factor-+:                ", ifelse(x[["Materiality"]] == 1, yes = "Requires materiality", no = x[["BF"]]))),"
# Conclusion:                   ", x[["Conclusion"]], "
# ------------------------------------------------------------ ")
}

# Summary methods

#' @rdname jfa-methods
#' @method summary jfaPrior
#' @export
summary.jfaPrior <- function(object, digits = 3, ...) {
  out <- data.frame("Confidence" = round(object[["confidence"]], digits + 2),
                    "Expected_Sample_Errors" = round(object[["expectedError"]], digits + 2),
                    "Likelihood" = object[["likelihood"]],
                    "Method" = object[["method"]],
                    "Prior" = object[["prior"]],
                    "Implicit_Sample_Size" = round(object[["description"]]$implicitn, digits),
                    "Implicit_Sample_Errors" = round(object[["description"]]$implicitk, digits),
                    "UB" = round(object[["statistics"]]$ub, digits),
                    "Precision" = round(object[["statistics"]]$precision, digits),
                    "Mode" = round(object[["statistics"]]$mode, digits),
                    "Mean" = round(object[["statistics"]]$mean, digits),
                    "Median" = round(object[["statistics"]]$median, digits),
                    stringsAsFactors = FALSE)
  if (object[["method"]] == "median") {
    out[["Materiality"]] <- round(object[["materiality"]], digits + 2)
  } else if (object[["method"]] == "arm") {
    out[["IR"]] <- round(object[["specifics"]]$ir, digits)
    out[["ICR"]] <- round(object[["specifics"]]$cr, digits)
    out[["DR"]] <- round((1 - object[["confidence"]]) / (object[["specifics"]]$ir * object[["specifics"]]$cr), digits)
  } else if (object[["method"]] == "bram") {
    out[["Mode_Prior"]] <- round(object[["specifics"]]$mode, digits)
    out[["UB_Prior"]] <- round(object[["specifics"]]$ub, digits)
  } else if (object[["method"]] == "hypotheses") {
    out[["Materiality"]] <- round(object[["materiality"]], digits + 2)
    out[["p_Hmin"]] <- round(object[["specifics"]]$pHmin, digits)
    out[["p_Hplus"]] <- round(object[["specifics"]]$pHplus, digits)
  } else if (object[["method"]] == "sample" || object[["method"]] == "factor") {
    out[["N_Prior"]] <- object[["specifics"]]$sampleN
    out[["K_Prior"]] <- object[["specifics"]]$sampleK
    if (object[["method"]] == "factor")
      out[["Factor"]] <- round(object[["specifics"]]$factor, digits)
  }
  class(out) <- c("summary.jfaPrior", "data.frame")
  return(out)
}

#' @rdname jfa-methods
#' @method summary jfaPosterior
#' @export 
summary.jfaPosterior <- function(object, digits = 3, ...) {
  out <- data.frame("Posterior" = object[["posterior"]],
                    "UB" = round(object[["statistics"]]$ub, digits),
                    "Precision" = round(object[["statistics"]]$precision, digits),
                    "Mode" = round(object[["statistics"]]$mode, digits),
                    "Mean" = round(object[["statistics"]]$mean, digits),
                    "Median" = round(object[["statistics"]]$median, digits),
                    stringsAsFactors = FALSE)
  class(out) <- c("summary.jfaPosterior", "data.frame")
  return(out)
}

#' @rdname jfa-methods
#' @method summary jfaPlanning
#' @export
summary.jfaPlanning <- function(object, digits = 3, ...) {
  out <- data.frame("Confidence" = round(object[["confidence"]], digits + 2),
                    "Materiality" = round(object[["materiality"]], digits + 2),
                    "Minimum_Precision" = round(object[["minPrecision"]], digits + 2),
                    "Likelihood" = object[["likelihood"]],
                    "Expected_Sample_Errors" = round(object[["expectedSampleError"]], digits),
                    "Sample_Size" = object[["sampleSize"]],
                    "Expected_UB" = round(object[["expectedBound"]], digits + 2),
                    "Expected_Precision" = round(object[["expectedPrecision"]], digits + 2),
                    stringsAsFactors = FALSE)
  if (inherits(object[["prior"]], "jfaPrior")) {
    out[["Prior"]] <- object[["prior"]]$prior
    out[["Posterior"]] <- object[["expectedPosterior"]]$posterior
    out[["Expected_MLE"]] <- round(object[["expectedPosterior"]][["statistics"]]$mode, digits + 2)
    if (object[["materiality"]] != 1)
      out[["Expected_BF"]] <- round(object[["expectedPosterior"]][["hypotheses"]]$expectedBf, digits)
  } else {
    out[["Expected_MLE"]] <- round(object[["expectedSampleError"]] / object[["sampleSize"]], digits + 2)
  }
  out[["Type"]] <- if (inherits(object[["prior"]], "jfaPrior")) "Bayesian" else "Classical"
  class(out) <- c("summary.jfaPlanning", "data.frame")
  return(out)
}

#' @rdname jfa-methods
#' @method summary jfaSelection
#' @export
summary.jfaSelection <- function(object, digits = 3, ...) {
  out <- data.frame("Population_Size" = round(object[["populationSize"]], digits),
                    "Requested_Sample_Size" = object[["requestedSampleSize"]],
                    "Units" = object[["units"]],
                    "Algorithm" = object[["algorithm"]],
                    "Obtained_Sampling_Units" = sum(object[["sample"]][["count"]]),
                    "Obtained_Sample_Items" = object[["obtainedSampleSize"]],
                    "Proportion_Total" = round(object[["obtainedSampleSize"]] / object[["populationSize"]], digits),
                    stringsAsFactors = FALSE)
  if (object[["algorithm"]] == "interval" || object[["algorithm"]] == "cell") {
    out[["Interval"]] <- round(object[["interval"]], digits)
    if (object[["algorithm"]] == "interval")
      out[["Starting_Point"]] <- object[["intervalStartingPoint"]]
  }
  if (object[["units"]] == "mus") 
    out[["Proportion_Value"]] <- round(sum(object[["sample"]][, object[["bookValues"]]]) / sum(object[["population"]][, object[["bookValues"]]]), digits + 2)
  class(out) <- c("summary.jfaSelection", "data.frame")
  return(out)
}

#' @rdname jfa-methods
#' @method summary jfaEvaluation
#' @export
summary.jfaEvaluation <- function(object, digits = 3, ...) {
  out <- data.frame("Confidence" = round(object[["confidence"]], digits + 2),
                    "Materiality" = round(object[["materiality"]], digits + 2),
                    "Minimum_Precision" = round(object[["minPrecision"]], digits + 2),
                    "Sample_Size" = object[["n"]],
                    "Sample_Errors" = object[["k"]],
                    "Method" = object[["method"]],
                    "MLE" = round(object[["mle"]], digits + 2),
                    "Precision" = round(object[["precision"]], digits + 2),
                    "Conclusion" = object[["conclusion"]],
                    stringsAsFactors = FALSE)
  if (object[["method"]] %in% c("direct", "difference", "quotient", "regression")) {
    out[["Type"]] <- "Classical"
    out[["Population_Value"]] <- round(object[["popBookvalue"]], digits)
    out[["LB"]] <- round(object[["lowerBound"]], digits)
    out[["UB"]] <- round(object[["upperBound"]], digits)
  } else {
    out[["Sum_Taints"]] <- round(object[["t"]], digits)
    out[["UB"]] <- round(object[["confBound"]], digits + 2)
    if (!is.null(object[["prior"]])) {
      out[["Type"]] <- "Bayesian"
      out[["Prior"]] <- object[["prior"]][["prior"]]
      out[["Posterior"]] <- object[["posterior"]][["posterior"]]
      if (object[["materiality"]] != 1)
        out[["BF"]] <- round(object[["posterior"]][["hypotheses"]]$bf, digits)
    } else {
      out[["Type"]] <- "Classical"
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
    xlim <- ceiling(xmax * x[["N"]])
    xseq <- seq(0, xlim, by = 1)
    d    <- .dBetaBinom(x = xseq, N = x[["N"]], shape1 = x[["description"]]$alpha, shape2 = x[["description"]]$beta)
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
    mainLab <- paste0("beta-binomial prior distribution (N = ", x[["N"]], ", alpha = ", round(x[["description"]]$alpha, 3), ", beta = ", round(x[["description"]]$beta, 3), ")")
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
    xlim <- ceiling(xmax * x[["N"]])
    xseq <- seq(0, xlim, by = 1)
    d    <- c(rep(0, x[["description"]]$k), .dBetaBinom(x = xseq, N = x[["N"]] - x[["description"]]$n, shape1 = x[["description"]]$alpha, shape2 = x[["description"]]$beta))
  }
  if (x[["description"]]$density == "gamma" || x[["description"]]$density == "beta") {
    mainLab <- switch(x[["description"]]$density,
                      "gamma" = paste0("gamma posterior distribution (shape = ", round(x[["description"]]$alpha, 3), ", rate = ", round(x[["description"]]$beta, 3), ")"),
                      "beta" = paste0("beta posterior distribution (alpha = ", round(x[["description"]]$alpha, 3), ", beta = ", round(x[["description"]]$beta, 3), ")"))
    graphics::plot(x = xseq, y = d, type = "l", lwd = 2, bty = "n", xlab = expression(theta), ylab = "Density", las = 1, ylim = c(0, max(d)), main = mainLab, axes = FALSE, lty = 2)
    graphics::axis(1, at = pretty(xseq, min.n = 4), labels = paste0(round(pretty(xseq, min.n = 4) * 100, 2), "%"))
    graphics::axis(2, at = c(0, max(d)), labels = FALSE, las = 1, lwd.ticks = 0)
  } else {
    mainLab <- paste0("beta-binomial posterior distribution (N = ", x[["N"]] - x[["description"]]$n, ", alpha = ", round(x[["description"]]$alpha, 3), ", beta = ", round(x[["description"]]$beta, 3), ")")
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
      d1         <- stats::dgamma(xseq, shape = x[["expectedPosterior"]][["description"]]$alpha, rate = x[["expectedPosterior"]][["description"]]$beta)
      bound      <- stats::qgamma(x[["confidence"]], shape = x[["expectedPosterior"]][["description"]]$alpha, rate = x[["expectedPosterior"]][["description"]]$beta)
    } else if (x[["likelihood"]] == "binomial") {
      d          <- stats::dbeta(xseq, shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
      d1         <- stats::dbeta(xseq, shape1 = x[["expectedPosterior"]][["description"]]$alpha, shape2 = x[["expectedPosterior"]][["description"]]$beta)
      bound      <- stats::qbeta(x[["confidence"]], shape1 = x[["expectedPosterior"]][["description"]]$alpha, shape2 = x[["expectedPosterior"]][["description"]]$beta)
    } else if (x[["likelihood"]] == "hypergeometric") {
      xlim_prior <- ceiling(xmax * x[["N"]])
      xseq_prior <- seq(0, xlim_prior, by = 1)
      xlim_post  <- min(xlim_prior, x[["N"]] - x[["sampleSize"]])
      xseq_post  <- seq(0, xlim_post, by = 1)
      d          <- .dBetaBinom(x = xseq_prior, N = x[["N"]], shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
      d1         <- c(rep(0, x[["expectedSampleError"]]), .dBetaBinom(x = xseq_post, N = x[["N"]] - x[["sampleSize"]], shape1 = x[["expectedPosterior"]][["description"]]$alpha, shape2 = x[["expectedPosterior"]][["description"]]$beta))
      bound      <- .qBetaBinom(p = x[["confidence"]], N = x[["N"]] - x[["sampleSize"]], shape1 = x[["expectedPosterior"]][["description"]]$alpha, shape2 = x[["expectedPosterior"]][["description"]]$beta)
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
    xseq <- seq(0, ceiling(xmax * x[["sampleSize"]]), by = 1)
    if (x[["likelihood"]] == "poisson") {
      mainLab <- paste0("Poisson distribution (lambda = ", round(x[["materiality"]] * x[["sampleSize"]], 2), ")")
      d       <- stats::dpois(x = xseq, lambda = x[["materiality"]] * x[["sampleSize"]])
      d1      <- stats::dpois(x = 0:x[["expectedSampleError"]], lambda = x[["materiality"]] * x[["sampleSize"]])
    } else if (x[["likelihood"]] == "binomial") {
      mainLab <- paste0("Binomial distribution (n = ", x[["sampleSize"]], ", p = ", x[["materiality"]],")")
      d       <- stats::dbinom(x = xseq, size = x[["sampleSize"]], prob = x[["materiality"]])
      d1      <- stats::dbinom(x = 0:x[["expectedSampleError"]], size = x[["sampleSize"]], prob = x[["materiality"]])
    } else if (x$likelihood == "hypergeometric") {
      mainLab <- paste0("Hypergeometric distribution (N = ", x[["N"]], ", n = ", x[["sampleSize"]], ", K = ", x[["populationK"]], ")")
      d       <- stats::dhyper(x = xseq, m = x[["populationK"]], n = x[["N"]] - x[["populationK"]], k = x[["sampleSize"]])
      d1      <- stats::dhyper(x = 0:x[["expectedSampleError"]], m = x[["populationK"]], n = x[["N"]] - x[["populationK"]], k = x[["sampleSize"]])
    }
    graphics::barplot(d, xlab = "k", col = "lightgray", ylab = "Probability", las = 1, main = mainLab, width = 1, space = 0)
    graphics::axis(1, at = pretty(xseq, min.n = 4) + 0.5, labels = pretty(xseq, min.n = 4))
    graphics::barplot(d1, col = "darkgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
    graphics::legend("topright", legend = c("Expected error free", "Expected errors"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
  }
}

#' @rdname jfa-methods
#' @method plot jfaSelection
#' @export
plot.jfaSelection <- function(x, ...) {
  if (x$units == "records")
    stop("No plotting method available for record sampling")
  name <- x[["bookValues"]]
  graphics::hist(x[["population"]][[name]], breaks = 30, main = "Histogram of population and sample book values", xlab = "Book values", las = 1, col = "lightgray")
  graphics::hist(x[["sample"]][[name]], breaks = 30, main = "Sample", xlab = "Book values", las = 1, add = TRUE, col = "darkgray")
  graphics::legend("topright", legend = c("Population", "Sample"), bty = "n", fill = c("lightgray", "darkgray"))
}

#' @rdname jfa-methods
#' @method plot jfaEvaluation
#' @export
plot.jfaEvaluation <- function(x, xmax = 0.5, ...) {
  if (x[["method"]] %in% c("stringer", "stringer-meikle", "stringer-lta", "stringer-pvz", "rohrbach", "moment", "coxsnell"))
    stop("No plotting method available for a confidence bound from this method.")
  if (x[["method"]] %in% c("direct", "difference", "quotient", "regression")) {
    ymin <- x[["mle"]] - 2 * x[["precision"]]
    ymax <- x[["mle"]] + 2 * x[["precision"]]
    graphics::plot(0, type = "n", ylim = c(ymin, ymax), ylab = expression(E), xlim = c(0, 1), bty = "n", xaxt = "n", xlab = "", yaxt = "n", main = paste0(round(x[["confidence"]] * 100, 2), "% Confidence interval"))
    yBreaks <- base::pretty(c(ymin, ymax), n = 6)
    graphics::axis(side = 2, at = yBreaks, labels = base::format(round(yBreaks), scientific = F, big.mark = ","), las = 1)
    graphics::segments(x0 = 0, x1 = 1, y0 = 0, y1 = 0, lty = 2, col = "gray")
    if (x[["materiality"]] != 1)
      graphics::segments(x0 = 0, x1 = 1, y0 = (x[["popBookvalue"]] * x[["materiality"]]), y1 = (x[["popBookvalue"]] * x[["materiality"]]), lty = 2, col = "red")
    graphics::points(x = 0.5, y = x[["mle"]], pch = 19)
    graphics::arrows(x0 = 0.5, x1 = 0.5, y0 = x[["lowerBound"]], y1 = x[["upperBound"]], code = 3, lwd = 2, col = "black", angle = 90)
    graphics::text(x = 0.86, y = x[["mle"]], labels = paste0("Most likely error = ", format(round(x[["mle"]], 2), scientific = FALSE, big.mark = ",")), cex = 0.75, adj = c(1, 0.5))
    graphics::text(x = 0.87, y = x[["lowerBound"]], labels = paste0("Lower bound = ", format(round(x[["lowerBound"]], 2), scientific = FALSE, big.mark = ",")), cex = 0.75, adj = c(1, 0.5))
    graphics::text(x = 0.87, y = x[["upperBound"]], labels = paste0("Upper bound = ", format(round(x[["upperBound"]], 2), scientific = FALSE, big.mark = ",")), cex = 0.75, adj = c(1, 0.5))
    graphics::segments(x0 = 0.40, x1 = 0.40, y0 = x[["mle"]], y1 = x[["upperBound"]], col = "black")
    graphics::segments(x0 = 0.40, x1 = 0.42, y0 = x[["mle"]], y1 = x[["mle"]], col = "black")
    graphics::segments(x0 = 0.40, x1 = 0.42, y0 = x[["upperBound"]], y1 = x[["upperBound"]], col = "black")
    graphics::text(x = 0.15, y = (x[["upperBound"]] - x[["precision"]]/2), labels = paste0("Precision = ", format(round(x[["precision"]], 2), scientific = FALSE, big.mark = ",")), cex = 0.75, adj = c(0, 0.5))
  } else {
    if (!is.null(x[["prior"]])) {
      xseq         <- seq(0, xmax, length.out = 1000)
      if (x[["method"]] == "poisson") {
        d          <- stats::dgamma(xseq, shape = x[["prior"]][["description"]]$alpha, rate = x[["prior"]][["description"]]$beta)
        d1         <- stats::dgamma(xseq, shape = x[["posterior"]][["description"]]$alpha, rate = x[["posterior"]][["description"]]$beta)
        bound      <- stats::qgamma(x$confidence, shape = x[["posterior"]][["description"]]$alpha, rate = x[["posterior"]][["description"]]$beta)
      } else if (x[["method"]] == "binomial") {
        d          <- stats::dbeta(xseq, shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
        d1         <- stats::dbeta(xseq, shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
        bound      <- stats::qbeta(x$confidence, shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
      } else if (x[["method"]] == "hypergeometric") {
        xlim_prior <- ceiling(xmax * x[["N"]])
        xseq_prior <- seq(0, xlim_prior, by = 1)
        xlim_post  <- min(xlim_prior, x[["N"]] - x[["n"]])
        xseq_post  <- seq(0, xlim_post, by = 1)
        d          <- .dBetaBinom(x = xseq_prior, N = x[["N"]], shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
        d1         <- c(rep(0, x[["k"]]), .dBetaBinom(x = xseq_post, N = x[["N"]] - x[["n"]], shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta))
        bound      <- .qBetaBinom(p = x[["confidence"]], N = x[["N"]] - x[["n"]], shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
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
        stop("Cannot plot a frequentist outcome when the materiality is unknown.")
      if (!(x[["method"]] %in% c("poisson", "binomial", "hypergeometric")))
        stop("Without a prior distribution, your method must either be 'poisson', 'binomial', or 'hypergeometric'.")
      xseq      <- seq(0, ceiling(xmax * x[["n"]]), by = 1)
      if (x[["method"]] == "poisson") {
        mainLab <- paste0("Poisson distribution (lambda = ", round(x[["materiality"]] * x[["n"]], 2), ")")
        d       <- stats::dpois(x = xseq, lambda = x[["materiality"]] * x[["n"]])
        d1      <- stats::dpois(x = 0:x[["k"]], lambda = x[["materiality"]] * x[["n"]])
      } else if (x[["method"]] == "binomial") {
        mainLab <- paste0("Binomial distribution (n = ", x$n, ", p = ", round(x[["materiality"]], 2),")")
        d       <- stats::dbinom(x = xseq, size = x[["n"]], prob = x[["materiality"]])
        d1      <- stats::dbinom(x = 0:x[["k"]], size = x[["n"]], prob = x[["materiality"]])
      } else if (x[["method"]] == "hypergeometric") {
        mainLab <- paste0("Hypergeometric distribution (N = ", x$N, ", n = ", x$n, ", K = ", x[["populationK"]], ")")
        d       <- stats::dhyper(x = xseq, m = x[["populationK"]], n = x[["N"]] - x[["populationK"]], k = x[["n"]])
        d1      <- stats::dhyper(x = 0:x[["n"]], m = x[["populationK"]], n = x[["N"]] - x[["populationK"]], k = x[["n"]])
      }
      graphics::barplot(d, xlab = "k", col = "lightgray", ylab = "Probability", las = 1, main = mainLab, width = 1, space = 0)
      graphics::axis(1, at = pretty(xseq, min.n = 4) + 0.5, labels = pretty(xseq, min.n = 4))
      graphics::barplot(d1, col = "darkgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
      graphics::legend("topright", legend = c("Error free", "Errors"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
    }
  }
}