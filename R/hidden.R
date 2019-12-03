#' @method print jfaPlanning
#' @export
print.jfaPlanning <- function(x, ...){
    cat("# jfa results for sample size calculation with", x$likelihood,"likelihood
#      
# Materiality:            ", paste0(round(x$materiality * 100, 2), "%"),"
# Confidence:             ", paste0(round(x$confidence * 100, 2), "%"),"
# Sample size:            ", x$sampleSize,"
# Allowed sample errors:  ", x$expectedSampleError)
}

#' @method print jfaSampling
#' @export
print.jfaSampling <- function(x, ...){
  print(x$sample)
}

#' @method print jfaEvaluation
#' @export
print.jfaEvaluation <- function(x, ...){
  if(!is.null(x$materiality)){
    cat("# jfa results for evaluation with", x$method,"method
#   
# Materiality:          ", paste0(round(x$materiality * 100, 2), "%"),"
# Confidence:           ", paste0(round(x$confidence * 100, 2), "%"),"
# Upper bound:          ", paste0(round(x$confBound * 100, 3), "%"),"
# Sample size:          ", x$n,"
# Sample errors:        ", x$k, "
# Conclusion:           ", x$conclusion)
  } else {
    cat("# jfa results for evaluation with", x$method,"method
#      
# Confidence:           ", paste0(round(x$confidence * 100, 2), "%"),"
# Upper bound:          ", paste0(round(x$confBound * 100, 3), "%"),"
# Sample size:          ", x$n,"
# Sample errors:        ", x$k)
  }
}

#' @method plot jfaPlanning
#' @export
plot.jfaPlanning <- function(x, ...){
  limx <- length(0:x$sampleSize)
  if(limx > 51){
    limx <- 51
  }
  if(x$prior){
    xlim <- x$materiality * 3
    xseq <- seq(0, xlim, 0.00001)
    mainLab <- ifelse(x$priorK == 0 && x$priorN == 0, yes = "Uninformed", no = "Informed")
    if(x$likelihood == "poisson"){
      d <- stats::dgamma(xseq, shape = 1 + x$priorK, rate = x$priorN)
      d1 <- stats::dgamma(xseq, shape = 1 + x$priorK + x$expectedSampleError, rate = x$priorN + x$sampleSize)
      bound <- stats::qgamma(x$confidence, shape = 1 + x$priorK + x$expectedSampleError, rate = x$priorN + x$sampleSize)
    } else if(x$likelihood == "binomial"){
      d <- stats::dbeta(xseq, shape1 = 1 + x$priorK, shape2 = 1 + x$priorN - x$priorK)
      d1 <- stats::dbeta(xseq, shape1 = 1 + x$priorK + x$expectedSampleError, shape2 = 1 + x$priorN - x$priorK + x$sampleSize - x$expectedSampleError)
      bound <- stats::qbeta(x$confidence, shape1 = 1 + x$priorK + x$expectedSampleError, shape2 = 1 + x$priorN - x$priorK + x$sampleSize - x$expectedSampleError)
    } else if(x$likelihood == "hypergeometric"){
      xlim <- ceiling(xlim * x$N)
      xseq <- seq(0, xlim, by = 1)
      d <- jfa:::.dBetaBinom(x = xseq, N = x$N, shape1 = 1 + x$priorK, shape2 = 1 + x$priorN - x$priorK)
      d1 <- jfa:::.dBetaBinom(x = xseq, N = x$N, shape1 = 1 + x$priorK + x$expectedSampleError, shape2 = 1 + x$priorN - x$priorK + x$sampleSize - x$expectedSampleError)
      bound <- jfa:::.qBetaBinom(p = x$confidence, N = x$N - x$sampleSize, shape1 = 1 + x$priorK + x$expectedSampleError, shape2 = 1 + x$priorN - x$priorK + x$sampleSize - x$expectedSampleError)
    }
    if(x$likelihood == "poisson" || x$likelihood == "binomial"){
      plot(x = xseq, y = d1, type = "l", lwd = 2, bty = "n", xlab = "Misstatement", ylab = "Probability density", las = 1, ylim = c(0, max(d1)),
           main = paste0(mainLab, " gamma prior and expected posterior"), axes = FALSE)
      polygon(x = c(0, xseq[xseq<=bound], xseq[xseq<=bound][length(xseq[xseq<=bound])]), y = c(0, d1[xseq<=bound], 0), col="lightgray", border = NA)
      lines(x = xseq, y = d, lwd = 2, lty = 2)
      axis(1, at = pretty(seq(0, xlim, by = 0.01), min.n = 5), labels = paste0(round(pretty(seq(0, xlim, by = 0.01), min.n = 5) * 100, 2), "%"))
      axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
      legend("topright", legend = c("Prior", "Expected posterior"), lty = c(2, 1), bty = "n", cex = 1.2, lwd = 2)
    } else {
      barplot(d1, bty = "n", xlab = "Errors", ylab = "Probability", las = 1, ylim = c(0, max(d1)), width = 1, space = 0,
              main = paste0(mainLab, " beta-binomial prior and expected posterior"), axes = FALSE, col = "darkgray")
      barplot(d, col = "lightgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
      axis(1, at = seq(0, xlim, by = 10) + 0.5, labels = seq(0, xlim, by = 10))
      axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
      legend("topright", legend = c("Prior", "Expected posterior"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
    }
  } else {
    if(x$likelihood == "poisson"){
      mainLab <- paste0("Poisson distribution (lambda = ", round(x$materiality * x$sampleSize, 2), ")")
      d <- stats::dpois(x = 0:x$sampleSize, lambda = x$materiality * x$sampleSize)[1:limx]
      d1 <- stats::dpois(x = 0:x$expectedSampleError, lambda = x$materiality * x$sampleSize)
    } else if(x$likelihood == "binomial"){
      mainLab <- paste0("Binomial distribution (n = ", x$sampleSize, ", p = ", x$materiality,")")
      d <- stats::dbinom(x = 0:x$sampleSize, size = x$sampleSize, prob = x$materiality)[1:limx]
      d1 <- stats::dbinom(x = 0:x$expectedSampleError, size = x$sampleSize, prob = x$materiality)
    } else if(x$likelihood == "hypergeometric"){
      mainLab <- paste0("Hypergeometric distribution (N = ", x$N, ", n = ", x$sampleSize, ", K = ", 
                        x$populationK, ", k = ", x$expectedSampleError,")")
      d <- stats::dhyper(x = 0:x$sampleSize, m = x$populationK, n = x$N - x$populationK, k = x$sampleSize)[1:limx]
      d1 <- stats::dhyper(x = 0:x$expectedSampleError, m = x$populationK, n = x$N - x$populationK, k = x$sampleSize)
    }
    barplot(d, xlab = "Errors", col = "lightgray", ylab = "Probability", las = 1, main = mainLab, width = 1, space = 0)
    axis(1, at = seq(0, limx, by = 10) + 0.5, labels = seq(0, limx, by = 10))
    barplot(d1, col = "darkgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
    legend("topright", legend = c("Expected error free", "Expected errors"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
  }
}

#' @method plot jfaSampling
#' @export
plot.jfaSampling <- function(x, ...){
  name <- x$bookValues
  hist(x$population[[name]], breaks = 30, main = "Histogram of population and sample book values", xlab = "Book values", las = 1, col = "lightgray")
  hist(x$sample[[name]], breaks = 30, main = "Sample", xlab = "Book values", las = 1, add = TRUE, col = "darkgray")
  legend("topright", legend = c("Population", "Sample"), bty = "n", fill = c("lightgray", "darkgray"))
}