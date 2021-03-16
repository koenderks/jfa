#' @method print jfaPrior
#' @export
print.jfaPrior <- function(x, digits = 2, ...) {
  cat("# ------------------------------------------------------------
#         jfa Prior Distribution Summary (Bayesian)
# ------------------------------------------------------------
# Input:
#
# Confidence:             ", paste0(round(x[["confidence"]] * 100, digits), "%"),"   
# Expected sample errors: ", paste0(round(x[["expectedError"]] * 100, digits), "%"),"      
# Likelihood:             ", x[["likelihood"]],"
# Specifics:              ", switch(x[["method"]],
                                    "none" = "None",
                                    "median" = paste0("p(\u0398 < ",round(x[["materiality"]], digits),") = p(\u0398 > ", round(x[["materiality"]], digits),") = 0.5"),
                                    "hypotheses" = paste0("p(\u0398 < ",round(x[["materiality"]], digits),") = ",round(x[["specifics"]]$pHmin, digits),"; p(\u0398 > ", round(x[["materiality"]], digits),") = ", round(x[["specifics"]]$pHplus, digits)),
                                    "arm" = paste0("Inherent risk = ", round(x[["specifics"]]$ir, digits), "; Internal control risk = ", round(x[["specifics"]]$cr, digits), "; Detection risk = ", round((1 - x[["confidence"]]) / (x[["specifics"]]$ir * x[["specifics"]]$cr), digits)),
                                    "sample" = paste0("Earlier sample of ", round(x[["specifics"]]$sampleN, digits), " transactions with ", round(x[["specifics"]]$sampleK, digits), " errors"),
                                    "factor" = paste0("Earlier sample of ", round(x[["specifics"]]$sampleN, digits), " transactions with ", round(x[["specifics"]]$sampleK, digits), " errors weighted by ", round(x[["specifics"]]$factor, digits))), "
# ------------------------------------------------------------
# Output: 
#
# Prior distribution:     ", x$prior,"
# Implicit sample size:   ", round(x[["description"]]$implicitn, digits),"
# Implicit errors:        ", round(x[["description"]]$implicitk, digits),"
# ------------------------------------------------------------
# Statistics: 
#
# Upper bound:            ", round(x[["statistics"]]$ub, digits),"
# Precision:              ", round(x[["statistics"]]$precision, digits),"
# Mode:                   ", round(x[["statistics"]]$mode, digits),"
# Mean:                   ", round(x[["statistics"]]$mean, digits),"
# Median:                 ", round(x[["statistics"]]$median, digits),"
# ------------------------------------------------------------"
  )
}

#' @method print jfaPosterior
#' @export 
print.jfaPosterior <- function(x, digits = 2, ...) {
  cat("# ------------------------------------------------------------
#             jfa Posterior Distribution (Bayesian)
# ------------------------------------------------------------
# Input: 
#
# Posterior distribution:  ", x[["posterior"]],"
# ------------------------------------------------------------
# Statistics:
#
# Upper bound:             ", round(x[["statistics"]]$ub, digits),"
# Precision:               ", round(x[["statistics"]]$precision, digits),"
# Mode:                    ", round(x[["statistics"]]$mode, digits),"
# Mean:                    ", round(x[["statistics"]]$mean, digits),"
# Median:                  ", round(x[["statistics"]]$median, digits),"
# ------------------------------------------------------------"
  )
}

#' @method print jfaPlanning
#' @export
print.jfaPlanning <- function(x, digits = 2, ...) {
  if (class(x[["prior"]]) == "jfaPrior") {
    cat("# ------------------------------------------------------------
#              jfa Planning Summary (Bayesian)
# ------------------------------------------------------------
# Input:
# 
# Confidence:             ", paste0(round(x[["confidence"]] * 100, digits), "%"),"     
# Materiality:            ", ifelse(x[["materiality"]] == 1, yes = "Not specified", no = paste0(round(x[["materiality"]] * 100, digits), "%")),"
# Minimum precision:      ", ifelse(x[["minPrecision"]] == 1, yes = "Not specified", no = paste0(round(x[["minPrecision"]] * 100, digits), "%")),"
# Likelihood:             ", x[["likelihood"]],"
# Prior distribution:     ", x[["prior"]]$prior, "
# Expected sample errors: ", round(x[["expectedSampleError"]], digits), "
# ------------------------------------------------------------
# Output:
#
# Sample size:            ", x[["sampleSize"]],"
# Posterior distribution: ", x[["expectedPosterior"]]$posterior,"
# ------------------------------------------------------------
# Statistics:
#
# Expected upper bound:   ", paste0(round(x[["expectedBound"]] * 100, digits), "%"),"
# Expected precision:     ", paste0(round(x[["expectedPrecision"]] * 100, digits), "%"),"
# Expected Bayes factor-+:", ifelse(x[["materiality"]] == 1, yes = "Requires materiality", no = round(x[["expectedPosterior"]][["hypotheses"]]$expectedBf, digits)),"
# ------------------------------------------------------------ ")
  } else {
    cat("# ------------------------------------------------------------
#              jfa Planning Summary (Frequentist)
# ------------------------------------------------------------     
# Input:
# 
# Confidence:             ", paste0(round(x[["confidence"]] * 100, digits), "%"),"
# Materiality:            ", ifelse(x[["materiality"]] == 1, yes = "Not specified", no = paste0(round(x[["materiality"]] * 100, digits), "%")),"
# Minimum precision:      ", ifelse(x[["minPrecision"]] == 1, yes = "Not specified", no = paste0(round(x[["minPrecision"]] * 100, digits), "%")),"
# Likelihood:             ", x[["likelihood"]],"
# Expected sample errors: ", round(x[["expectedSampleError"]], digits), "
# ------------------------------------------------------------
# Output:
#
# Sample size:            ", x[["sampleSize"]],"
# ------------------------------------------------------------
# Statistics:
#
# Expected upper bound:   ", paste0(round(x[["expectedBound"]] * 100, digits), "%"),"
# Expected precision:     ", paste0(round(x[["expectedPrecision"]] * 100, digits), "%"),"
# ------------------------------------------------------------ ")
  }
}

#' @method print jfaSelection
#' @export
print.jfaSelection <- function(x, digits = 2, ...) {
  if (x$units == "mus") {
    cat("# ------------------------------------------------------------
#                  jfa Selection Summary
# ------------------------------------------------------------
# Input:
#      
# Population size:        ", round(x$populationSize, digits),"
# Requested sample size:  ", x$requestedSampleSize,"
# Sampling units:         ", "Monetary units","
# Algorithm:              ", switch(x$algorithm, "random" = "Random sampling", "interval" = "Fixed interval sampling", "cell" = "Cell sampling"), 
        ifelse(x$algorithm == "interval", no = "", yes = paste0("
# Interval:                ", round(x$interval, digits),"
# Starting point:          ", x$intervalStartingPoint)), 
        ifelse(x$algorithm == "cell", no = "", yes = paste0("
# Interval:                ", round(x$interval, digits))),"
# ------------------------------------------------------------ 
# Output:
#
# Obtained sample size:   ", x$obtainedSampleSize,"
# ------------------------------------------------------------
# Statistics:
#
# Proportion n/N:         ", round(x$obtainedSampleSize / x$populationSize, digits), "
# Percentage of value:    ", paste0(round(sum(x$sample[, x$bookValues]) / sum(x$population[, x$bookValues]) * 100, digits), "%"),"
# ------------------------------------------------------------ ") 
  } else {
    cat("# ------------------------------------------------------------
#                  jfa Selection Summary
# ------------------------------------------------------------
# Input:
#       
# Population size:        ", round(x$populationSize, digits),"
# Requested sample size:  ", round(x$requestedSampleSize, digits),"
# Sampling units:         ", "Records","
# Algorithm:              ", switch(x$algorithm, "random" = "Random sampling", "interval" = "Fixed interval sampling", "cell" = "Cell sampling"), 
        ifelse(x$algorithm == "interval", no = "", yes = paste0("
# Interval:                ", round(x$interval, digits),"
# Starting point:          ", x$intervalStartingPoint)), 
        ifelse(x$algorithm == "cell", no = "", yes = paste0("
# Interval:                ", round(x$interval, digits))),"
# ------------------------------------------------------------ 
# Output:
# 
# Obtained sample size:   ", x$obtainedSampleSize,"
# ------------------------------------------------------------
# Statistics:
#
# Proportion n/N:         ", round(nrow(x$sample)/nrow(x$population), digits), "
# ------------------------------------------------------------ ") 
  }
}

#' @method print jfaEvaluation
#' @export
print.jfaEvaluation <- function(x, digits = 2, ...) {
  if (x[["method"]] %in% c("direct", "difference", "quotient", "regression")) {
    cat("# ------------------------------------------------------------ 
#             jfa Evaluation Summary (Frequentist)
# ------------------------------------------------------------
# Input:
#   
# Confidence:              ", paste0(round(x[["confidence"]] * 100, digits), "%"),"
# Materiality:             ", ifelse(x[["materiality"]] == 1, yes = "Not specified", no = paste0(round(x[["materiality"]] * 100, digits), "%")),"
# Minium precision:        ", ifelse(x[["minPrecision"]] == 1, yes = "Not specified", no = paste0(round(x[["minPrecision"]] * 100, digits), "%")),"
# Sample size:             ", round(x[["n"]], digits),"
# Sample errors:           ", round(x[["k"]], digits), "
# Method:                  ", x[["method"]], "
# Population book value:   ", round(x[["popBookvalue"]], digits), "
# ------------------------------------------------------------
# Output:
#
# Most likely error:       ", round(x[["mle"]], digits), "
# Lower bound:             ", round(x[["lowerBound"]], digits),"
# Upper bound:             ", round(x[["upperBound"]], digits),"
# Precision:               ", round(x[["precision"]], digits),"
# Conclusion:              ", x[["conclusion"]], "
# ------------------------------------------------------------ ")
  } else {
    if (!is.null(x[["prior"]])) {
      cat("# ------------------------------------------------------------ 
#             jfa Evaluation Summary (Bayesian)
# ------------------------------------------------------------
# Input: 
#
# Confidence:              ", paste0(round(x[["confidence"]] * 100, digits), "%"),"  
# Materiality:             ", ifelse(x[["materiality"]] == 1, yes = "Not specified", no = paste0(round(x[["materiality"]] * 100, digits), "%")),"
# Minium precision:        ", ifelse(x[["minPrecision"]] == 1, yes = "Not specified", no = paste0(round(x[["minPrecision"]] * 100, digits), "%")),"
# Sample size:             ", round(x[["n"]], digits),"
# Sample errors:           ", round(x[["k"]], digits), "
# Sum of taints:           ", round(x[["t"]], digits), " 
# Method:                  ", x[["method"]], "
# Prior distribution:      ", x[["prior"]][["prior"]], "
# ------------------------------------------------------------
# Output:
#
# Posterior distribution:  ", x[["posterior"]][["posterior"]], "
# Most likely error:       ", paste0(round(x[["mle"]] * 100, digits), "%"),"
# Upper bound:             ", paste0(round(x[["confBound"]] * 100, digits), "%"),"
# Precision:               ", paste0(round(x[["precision"]] * 100, digits), "%"),"
# Bayes factor-+:          ", ifelse(x[["materiality"]] == 1 || !(x[["method"]] %in% c("poisson", "binomial", "hypergeometric")), yes = "Not available", no = round(x[["posterior"]][["hypotheses"]]$bf, digits)),"
# Conclusion:              ", x[["conclusion"]], "
# ------------------------------------------------------------ ")
    } else {
      cat("# ------------------------------------------------------------ 
#             jfa Evaluation Summary (Frequentist)
# ------------------------------------------------------------ 
# Input: 
#
# Confidence:              ", paste0(round(x[["confidence"]] * 100, digits), "%"),"  
# Materiality:             ", ifelse(x[["materiality"]] == 1, yes = "Not specified", no = paste0(round(x[["materiality"]] * 100, digits), "%")),"
# Minium precision:        ", ifelse(x[["minPrecision"]] == 1, yes = "Not specified", no = paste0(round(x[["minPrecision"]] * 100, digits), "%")),"
# Sample size:             ", round(x[["n"]], digits),"
# Sample errors:           ", round(x[["k"]], digits), "
# Sum of taints:           ", round(x[["k"]], digits), "
# Method:                  ", x[["method"]], "
# ------------------------------------------------------------
# Output:
#
# Most likely error:       ", paste0(round(x[["mle"]] * 100, digits), "%"),"
# Upper bound:             ", paste0(round(x[["confBound"]] * 100, digits), "%"),"
# Precision:               ", paste0(round(x[["precision"]] * 100, digits), "%"),"
# Conclusion:              ", x[["conclusion"]], "
# ------------------------------------------------------------ ")
    }
  }
}

#' @method plot jfaPrior
#' @export
plot.jfaPrior <- function(x, ...) {
  if (is.null(x[["materiality"]])) {
    xlim <- 1
  } else {
    xlim <- x[["materiality"]] * 3
  }
  xseq <- seq(0, xlim, 0.00001)
  mainLab <- ifelse(x[["description"]]$implicitk == 0 && x[["description"]]$implicitn == 0, yes = "Uninformed", no = "Informed")
  if (x[["description"]]$density == "gamma") {
    d <- stats::dgamma(xseq, shape = x[["description"]]$alpha, rate = x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta") {
    d <- stats::dbeta(xseq, shape1 = x[["description"]]$alpha, shape2 = x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta-binomial") {
    xlim <- ceiling(xlim * x[["N"]])
    xseq <- seq(0, xlim, by = x[["N"]]/50)
    d <- .dBetaBinom(x = xseq, N = x[["N"]], shape1 = x[["description"]]$alpha, shape2 = x[["description"]]$beta)
  }
  mainLab <- paste0(mainLab, " ", x[["description"]]$density, " prior")
  if (x[["description"]]$density == "gamma" || x[["description"]]$density == "beta") {
    graphics::plot(x = xseq, y = d, type = "l", lwd = 2, bty = "n", xlab = expression(theta), ylab = "Probability density", las = 1, ylim = c(0, max(d)), main = mainLab, axes = FALSE, lty = 2)
    graphics::axis(1, at = pretty(seq(0, xlim, by = 0.01), min.n = 5), labels = paste0(round(pretty(seq(0, xlim, by = 0.01), min.n = 5) * 100, 2), "%"))
    graphics::axis(2, at = c(0, max(d)), labels = FALSE, las = 1, lwd.ticks = 0)
    graphics::legend("topright", legend = "Prior", lty = 2, bty = "n", cex = 1.2, lwd = 2)
  } else {
    graphics::barplot(d, bty = "n", xlab = expression(theta), ylab = "Probability density", las = 1, ylim = c(0, max(d)), width = 1, space = 0, main = mainLab, axes = FALSE, col = "darkgray")
    graphics::axis(1, at = seq(0, xlim, by = 10) + 0.5, labels = seq(0, xlim, by = 10))
    graphics::axis(2, at = c(0, max(d)), labels = FALSE, las = 1, lwd.ticks = 0)
    graphics::legend("topright", legend = "Prior", fill = "lightgray", bty = "n", cex = 1.2) 
  }
}

#' @method plot jfaPosterior
#' @export
plot.jfaPosterior <- function(x, ...) {
  xlim <- 1
  xseq <- seq(0, xlim, 0.00001)
  if (x[["description"]]$density == "gamma") {
    d <- stats::dgamma(xseq, shape = x[["description"]]$alpha, rate = x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta") {
    d <- stats::dbeta(xseq, shape1 = x[["description"]]$alpha, shape2 = x[["description"]]$beta)
  } else if (x[["description"]]$density == "beta-binomial") {
    xlim <- ceiling(xlim * x[["N"]])
    xseq <- seq(0, xlim, by = x[["N"]]/50)
    d <- .dBetaBinom(x = xseq, N = x[["N"]], shape1 = x[["description"]]$alpha, shape2 = x[["description"]]$beta)
  }
  mainLab <- paste0(x[["description"]]$density, " posterior")
  if (x[["description"]]$density == "gamma" || x[["description"]]$density == "beta") {
    graphics::plot(x = xseq, y = d, type = "l", lwd = 2, bty = "n", xlab = expression(theta), ylab = "Probability density", las = 1, ylim = c(0, max(d)), main = mainLab, axes = FALSE, lty = 1)
    graphics::axis(1, at = pretty(seq(0, xlim, by = 0.01), min.n = 5), labels = paste0(round(pretty(seq(0, xlim, by = 0.01), min.n = 5) * 100, 2), "%"))
    graphics::axis(2, at = c(0, max(d)), labels = FALSE, las = 1, lwd.ticks = 0)
  } else {
    graphics::barplot(d, bty = "n", xlab = expression(theta), ylab = "Probability density", las = 1, ylim = c(0, max(d)), width = 1, space = 0, main = mainLab, axes = FALSE, col = "darkgray")
    graphics::axis(1, at = seq(0, xlim, by = 10) + 0.5, labels = seq(0, xlim, by = 10))
    graphics::axis(2, at = c(0, max(d)), labels = FALSE, las = 1, lwd.ticks = 0)
  }
}

#' @method plot jfaPlanning
#' @export
plot.jfaPlanning <- function(x, ...) {
  limx <- length(0:x[["sampleSize"]])
  if (limx > 51)
    limx <- 51
  if (!is.null(x[["prior"]])) {
    if (x[["materiality"]] == 1) {
      xlim <- 1
    } else {
      xlim <- x[["materiality"]] * 3
    }
    xseq <- seq(0, xlim, 0.00001)
    mainLab <- ifelse(x[["prior"]][["description"]]$implicitk == 0 && x[["prior"]][["description"]]$implicitn == 0, yes = "Uninformed", no = "Informed")
    if (x[["likelihood"]] == "poisson") {
      d <- stats::dgamma(xseq, shape = x[["prior"]][["description"]]$alpha, rate = x[["prior"]][["description"]]$beta)
      d1 <- stats::dgamma(xseq, shape = x[["expectedPosterior"]][["description"]]$alpha, rate = x[["expectedPosterior"]][["description"]]$beta)
      bound <- stats::qgamma(x[["confidence"]], shape = x[["expectedPosterior"]][["description"]]$alpha, rate = x[["expectedPosterior"]][["description"]]$beta)
    } else if (x[["likelihood"]] == "binomial") {
      d <- stats::dbeta(xseq, shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
      d1 <- stats::dbeta(xseq, shape1 = x[["expectedPosterior"]][["description"]]$alpha, shape2 = x[["expectedPosterior"]][["description"]]$beta)
      bound <- stats::qbeta(x[["confidence"]], shape1 = x[["expectedPosterior"]][["description"]]$alpha, shape2 = x[["expectedPosterior"]][["description"]]$beta)
    } else if (x[["likelihood"]] == "hypergeometric") {
      xlim <- ceiling(xlim * x[["N"]])
      xseq <- seq(0, xlim, by = 1)
      d <- .dBetaBinom(x = xseq, N = x[["N"]] - x[["sampleSize"]] + x[["expectedSampleError"]], shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
      d1 <- .dBetaBinom(x = xseq, N = x[["N"]] - x[["sampleSize"]] + x[["expectedSampleError"]], shape1 = x[["expectedPosterior"]][["description"]]$alpha, shape2 = x[["expectedPosterior"]][["description"]]$beta)
      bound <- .qBetaBinom(p = x[["confidence"]], N = x[["N"]] - x[["sampleSize"]] + x[["expectedSampleError"]], shape1 = x[["expectedPosterior"]][["description"]]$alpha, shape2 = x[["expectedPosterior"]][["description"]]$beta)
    }
    if (x$likelihood == "poisson" || x$likelihood == "binomial") {
      if (x$likelihood == "poisson")
        mainLabPlus <- " gamma prior and expected posterior"
      if (x$likelihood == "binomial")
        mainLabPlus <- " beta prior and expected posterior"
      graphics::plot(x = xseq, y = d1, type = "l", lwd = 2, bty = "n", xlab = expression(theta), ylab = "Probability density", las = 1, ylim = c(0, max(d1)),
                     main = paste0(mainLab, mainLabPlus), axes = FALSE)
      graphics::polygon(x = c(0, xseq[xseq<=bound], xseq[xseq<=bound][length(xseq[xseq<=bound])]), y = c(0, d1[xseq<=bound], 0), col="lightgray", border = NA)
      graphics::lines(x = xseq, y = d, lwd = 2, lty = 2)
      graphics::axis(1, at = pretty(seq(0, xlim, by = 0.01), min.n = 5), labels = paste0(round(pretty(seq(0, xlim, by = 0.01), min.n = 5) * 100, 2), "%"))
      graphics::axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
      graphics::legend("topright", legend = c("Prior", "Expected posterior"), lty = c(2, 1), bty = "n", cex = 1.2, lwd = 2)
    } else {
      graphics::barplot(d1, bty = "n", xlab = "Errors", ylab = "Probability", las = 1, ylim = c(0, max(d1)), width = 1, space = 0,
                        main = paste0(mainLab, " beta-binomial prior and expected posterior"), axes = FALSE, col = "darkgray")
      graphics::barplot(d, col = "lightgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
      graphics::axis(1, at = seq(0, xlim, by = 10) + 0.5, labels = seq(0, xlim, by = 10))
      graphics::axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
      graphics::legend("topright", legend = c("Prior", "Expected posterior"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
    }
  } else {
    if (x[["likelihood"]] == "poisson") {
      mainLab <- paste0("Poisson distribution (lambda = ", round(x[["materiality"]] * x[["sampleSize"]], 2), ")")
      d <- stats::dpois(x = 0:x[["sampleSize"]], lambda = x[["materiality"]] * x[["sampleSize"]])[1:limx]
      d1 <- stats::dpois(x = 0:x[["expectedSampleError"]], lambda = x[["materiality"]] * x[["sampleSize"]])
    } else if (x[["likelihood"]] == "binomial") {
      mainLab <- paste0("Binomial distribution (n = ", x[["sampleSize"]], ", p = ", x[["materiality"]],")")
      d <- stats::dbinom(x = 0:x[["sampleSize"]], size = x[["sampleSize"]], prob = x[["materiality"]])[1:limx]
      d1 <- stats::dbinom(x = 0:x[["expectedSampleError"]], size = x[["sampleSize"]], prob = x[["materiality"]])
    } else if (x$likelihood == "hypergeometric") {
      mainLab <- paste0("Hypergeometric distribution (N = ", x[["N"]], ", n = ", x[["sampleSize"]], ", K = ", x[["populationK"]], ", k = ", x[["expectedSampleError"]],")")
      d <- stats::dhyper(x = 0:x[["sampleSize"]], m = x[["populationK"]], n = x[["N"]] - x[["populationK"]], k = x[["sampleSize"]])[1:limx]
      d1 <- stats::dhyper(x = 0:x[["expectedSampleError"]], m = x[["populationK"]], n = x[["N"]] - x[["populationK"]], k = x[["sampleSize"]])
    }
    graphics::barplot(d, xlab = "Errors", col = "lightgray", ylab = "Probability", las = 1, main = mainLab, width = 1, space = 0)
    graphics::axis(1, at = seq(0, limx, by = 10) + 0.5, labels = seq(0, limx, by = 10))
    graphics::barplot(d1, col = "darkgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
    graphics::legend("topright", legend = c("Expected error free", "Expected errors"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
  }
}

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

#' @method plot jfaEvaluation
#' @export
plot.jfaEvaluation <- function(x, ...) {
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
    limx <- length(0:x[["n"]])
    if (limx > 51) {
      limx <- 51
    }
    if (!is.null(x[["prior"]])) {
      xlim <- x[["materiality"]] * 3
      if (length(xlim) == 0)
        xlim <- 0.30
      xseq <- seq(0, xlim, 0.00001)
      mainLab <- ifelse(x[["prior"]][["description"]]$implicitk == 0 && x[["prior"]][["description"]]$implicitn == 0, yes = "Uninformed", no = "Informed")
      if (x[["method"]] == "poisson") {
        d <- stats::dgamma(xseq, shape = x[["prior"]][["description"]]$alpha, rate = x[["prior"]][["description"]]$beta)
        d1 <- stats::dgamma(xseq, shape = x[["posterior"]][["description"]]$alpha, rate = x[["posterior"]][["description"]]$beta)
        bound <- stats::qgamma(x$confidence, shape = x[["posterior"]][["description"]]$alpha, rate = x[["posterior"]][["description"]]$beta)
      } else if (x[["method"]] == "binomial") {
        d <- stats::dbeta(xseq, shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
        d1 <- stats::dbeta(xseq, shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
        bound <- stats::qbeta(x$confidence, shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
      } else if (x[["method"]] == "hypergeometric") {
        xlim <- ceiling(xlim * x[["N"]])
        xseq <- seq(0, xlim, by = 1)
        d <- .dBetaBinom(x = xseq, N = x[["N"]], shape1 = x[["prior"]][["description"]]$alpha, shape2 = x[["prior"]][["description"]]$beta)
        d1 <- .dBetaBinom(x = xseq, N = x[["N"]], shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
        bound <- .qBetaBinom(p = x[["confidence"]], N = x[["N"]], shape1 = x[["posterior"]][["description"]]$alpha, shape2 = x[["posterior"]][["description"]]$beta)
      }
      if (x[["method"]] == "poisson" || x[["method"]] == "binomial") {
        if (x[["method"]] == "poisson")
          mainLabPlus <- " gamma prior and posterior"
        if (x[["method"]] == "binomial")
          mainLabPlus <- " beta prior and posterior"
        graphics::plot(x = xseq, y = d1, type = "l", lwd = 2, bty = "n", xlab = expression(theta), ylab = "Probability density", las = 1, ylim = c(0, max(d1)),
                       main = paste0(mainLab, mainLabPlus), axes = FALSE)
        graphics::polygon(x = c(0, xseq[xseq<=bound], xseq[xseq<=bound][length(xseq[xseq<=bound])]), y = c(0, d1[xseq<=bound], 0), col="lightgray", border = NA)
        graphics::lines(x = xseq, y = d, lwd = 2, lty = 2)
        graphics::axis(1, at = pretty(seq(0, xlim, by = 0.01), min.n = 5), labels = paste0(round(pretty(seq(0, xlim, by = 0.01), min.n = 5) * 100, 2), "%"))
        graphics::axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
        graphics::legend("topright", legend = c("Prior", "Posterior"), lty = c(2, 1), bty = "n", cex = 1.2, lwd = 2)
      } else {
        graphics::barplot(d1, bty = "n", xlab = "Errors", ylab = "Probability", las = 1, ylim = c(0, max(d1)), width = 1, space = 0,
                          main = paste0(mainLab, " beta-binomial prior and posterior"), axes = FALSE, col = "darkgray")
        graphics::barplot(d, col = "lightgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
        graphics::axis(1, at = seq(0, xlim, by = 10) + 0.5, labels = seq(0, xlim, by = 10))
        graphics::axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
        graphics::legend("topright", legend = c("Prior", "Posterior"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
      }
    } else {
      if (x[["materiality"]] == 1)
        stop("Cannot plot a frequentist outcome when the materiality is unknown.")
      if (!(x[["method"]] %in% c("poisson", "binomial", "hypergeometric")))
        stop("Without a prior distribution, your method must either be 'poisson', 'binomial', or 'hypergeometric'.")
      if (x[["method"]] == "poisson") {
        mainLab <- paste0("Poisson distribution (lambda = ", round(x[["materiality"]] * x[["n"]], 2), ")")
        d <- stats::dpois(x = 0:x[["n"]], lambda = x[["materiality"]] * x[["n"]])[1:limx]
        d1 <- stats::dpois(x = 0:x[["k"]], lambda = x[["materiality"]] * x[["n"]])
      } else if (x[["method"]] == "binomial") {
        mainLab <- paste0("Binomial distribution (n = ", x$n, ", p = ", round(x[["materiality"]], 2),")")
        d <- stats::dbinom(x = 0:x[["n"]], size = x[["n"]], prob = x[["materiality"]])[1:limx]
        d1 <- stats::dbinom(x = 0:x[["k"]], size = x[["n"]], prob = x[["materiality"]])
      } else if (x[["method"]] == "hypergeometric") {
        mainLab <- paste0("Hypergeometric distribution (N = ", x$N, ", n = ", x$n, ", K = ", x[["populationK"]], ", k = ", x$k,")")
        d <- stats::dhyper(x = 0:x[["n"]], m = x[["populationK"]], n = x[["N"]] - x[["populationK"]], k = x[["n"]])[1:limx]
        d1 <- stats::dhyper(x = 0:x[["n"]], m = x[["populationK"]], n = x[["N"]] - x[["populationK"]], k = x[["n"]])
      }
      graphics::barplot(d, xlab = "Errors", col = "lightgray", ylab = "Probability", las = 1, main = mainLab, width = 1, space = 0)
      graphics::axis(1, at = seq(0, limx, by = 10) + 0.5, labels = seq(0, limx, by = 10))
      graphics::barplot(d1, col = "darkgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
      graphics::legend("topright", legend = c("Error free", "Errors"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
    }
  }
}