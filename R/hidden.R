#' @method print jfa
#' @export
print.jfa <- function(x, ...){
  if(x$jfaType == "planning"){
    cat("# jfa results for sample size calculation with", x$likelihood,"likelihood
#      
# Materiality:            ", paste0(round(x$materiality * 100, 2), "%"),"
# Confidence:             ", paste0(round(x$confidence * 100, 2), "%"),"
# Sample size:            ", x$sampleSize,"
# Allowed sample errors:  ", x$expectedSampleError)
  } else if(x$jfaType == "sampling"){
    print(x$sample)
  } else if(x$jfaType == "evaluation"){
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
}

#' @method plot jfa
#' @export
plot.jfa <- function(x, ...){
  if(x$jfaType == "planning"){
    limx <- length(0:x$sampleSize)
    if(limx > 51){
      limx <- 51
    }
    if(x$prior){
      xlim <- x$materiality * 3
      xseq <- seq(0, xlim, 0.00001)
      mainLab <- ifelse(x$priorK == 0 && x$priorN == 0, yes = "Uninformed", no = "Informed")
      if(x$likelihood == "poisson"){
        d <- dgamma(xseq, shape = 1 + x$priorK, rate = x$priorN)
        d1 <- dgamma(xseq, shape = 1 + x$priorK + x$expectedSampleError, rate = x$priorN + x$sampleSize)
        bound <- qgamma(x$confidence, shape = 1 + x$priorK + x$expectedSampleError, rate = x$priorN + x$sampleSize)
        plot(x = xseq, y = d1, type = "l", lwd = 2, bty = "n", xlab = "Misstatement", ylab = "Density", las = 1, ylim = c(0, max(d1)),
             main = paste0(mainLab, " Gamma prior and expected posterior"), axes = FALSE)
        polygon(x = c(0, xseq[xseq<=bound], xseq[xseq<=bound][length(xseq[xseq<=bound])]), y = c(0, d1[xseq<=bound], 0), col="lightgray", border = NA)
        lines(x = xseq, y = d, lwd = 2, lty = 2)
        axis(1, at = pretty(seq(0, xlim, by = 0.01), min.n = 5), labels = paste0(round(pretty(seq(0, xlim, by = 0.01), min.n = 5) * 100, 2), "%"))
        axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
        legend("topright", legend = c("Prior", "Expected posterior"), lty = c(2, 1), bty = "n", cex = 1.2, lwd = 2)
      } else if(x$likelihood == "binomial"){
        d <- dbeta(xseq, shape1 = 1 + x$priorK, shape2 = 1 + x$priorN - x$priorK)
        d1 <- dbeta(xseq, shape1 = 1 + x$priorK + x$expectedSampleError, shape2 = 1 + x$priorN - x$priorK + x$sampleSize - x$expectedSampleError)
        bound <- qbeta(x$confidence, shape1 = 1 + x$priorK + x$expectedSampleError, shape2 = 1 + x$priorN - x$priorK + x$sampleSize - x$expectedSampleError)
        plot(x = xseq, y = d1, type = "l", lwd = 2, bty = "n", xlab = "Misstatement", ylab = "Density", las = 1, ylim = c(0, max(d1)),
             main = paste0(mainLab, " Beta prior and expected posterior"), axes = FALSE)
        polygon(x = c(0, xseq[xseq<=bound], xseq[xseq<=bound][length(xseq[xseq<=bound])]), y = c(0, d1[xseq<=bound], 0), col="lightgray", border = NA)
        lines(x = xseq, y = d, lwd = 2, lty = 2)
        axis(1, at = pretty(seq(0, xlim, by = 0.01), min.n = 5), labels = paste0(round(pretty(seq(0, xlim, by = 0.01), min.n = 5) * 100, 2), "%"))
        axis(2, at = c(0, max(d1)), labels = FALSE, las = 1, lwd.ticks = 0)
        legend("topright", legend = c("Prior", "Expected posterior"), lty = c(2, 1), bty = "n", cex = 1.2, lwd = 2)
      } else if(x$likelihood == "hypergeometric"){
        # Hypergeometric missing
      }
    } else {
      if(x$likelihood == "poisson"){
        barplot(dpois(x = 0:x$sampleSize, lambda = x$materiality * x$sampleSize)[1:limx], xlab = "Errors", col = "lightgray",
                las = 1, main = paste0("Poisson distribution (lambda = ", round(x$materiality * x$sampleSize, 2), ")"), width = 1, space = 0)
        axis(1, at = seq(0, limx, by = 10) + 0.5, labels = seq(0, limx, by = 10))
        barplot(dpois(x = 0:x$expectedSampleError, lambda = x$materiality * x$sampleSize), col = "red", add = TRUE, 
                las = 1, axes = FALSE, width = 1, space = 0)
      } else if(x$likelihood == "binomial"){
        barplot(dbinom(x = 0:x$sampleSize, size = x$sampleSize, prob = x$materiality)[1:limx], xlab = "Errors", col = "lightgray",
                las = 1, main = paste0("Binomial distribution (n = ", x$sampleSize, ", p = ", x$materiality,")"), width = 1, space = 0)
        axis(1, at = seq(0, limx, by = 10) + 0.5, labels = seq(0, limx, by = 10))
        barplot(dbinom(x = 0:x$expectedSampleError, size = x$sampleSize, prob = x$materiality), col = "red", add = TRUE, 
                las = 1, axes = FALSE, width = 1, space = 0)
      } else if(x$likelihood == "hypergeometric"){
        barplot(dhyper(x = 0:x$sampleSize, m = x$populationK, n = x$N - x$populationK, k = x$sampleSize)[1:limx], xlab = "Errors", 
                col = "lightgray", las = 1, main = paste0("Hypergeometric distribution (N = ", x$N, ", n = ", x$sampleSize, ", K = ", 
                                                          x$populationK, ", k = ", x$expectedSampleError,")"), width = 1, space = 0)
        axis(1, at = seq(0, 50, by = 10) + 0.5, labels = seq(0, 50, by = 10))
        barplot(dhyper(x = 0:x$expectedSampleError, m = x$populationK, n = x$N - x$populationK, k = x$sampleSize), col = "red", add = TRUE, 
                las = 1, axes = FALSE, width = 1, space = 0)
      }
    }
  } else if(x$jfaType == "sampling"){
    name <- x$bookValues
    hist(x$population[[name]], breaks = 30, main = "Histogram of population and sample book values", xlab = "Book values", las = 1, col = "lightgray")
    hist(x$sample[[name]], breaks = 30, main = "Sample", xlab = "Book values", las = 1, add = TRUE, col = "darkgray")
    legend("topright", legend = c("Population", "Sample"), bty = "n", fill = c("lightgray", "darkgray"))
  } else if(x$jfaType == "evaluation"){
    stop("No plotting method for jfa class evaluation")
  }
}

.dBetaBinom <- function(x, N, shape1, shape2){
  logval <- lbeta(x + shape1, N - x + shape2) - lbeta(shape1, shape2) + lchoose(N, x)
  ret <- exp(logval)
  return(ret)
}

.qBetaBinom <- function(p, N, shape1, shape2){
  pp <- cumsum(jfa:::.dBetaBinom(0:N, N, shape1, shape2))
  return(sapply(p, function(x) sum(pp < x)))
}

.stringerBound <- function(taints, confidence, n, correction = NULL){
  t <- ifelse(taints < 0, yes = 0, no = taints)
  t <- ifelse(taints > 1, yes = 1, no = taints)
  t <- sort(subset(t, t > 0), decreasing = TRUE)
  bound <- 1 - (1 - confidence)^(1 / n)
  if(length(t) > 0){
    propSum <- 0
    for(i in 1:length(t)){
      propSum <- propSum + (qbeta(p = confidence, shape1 = i + 1, shape2 = n - i) - qbeta(p = confidence, shape1 = (i - 1) + 1, shape2 = n - (i - 1)))  * t[i]
    }
    bound <- bound + propSum
  }
  if(!is.null(correction) && correction == "meikle"){
    tmin <- sort(subset(taints, taints < 0), decreasing = FALSE)
    if(length(tmin) > 0){
      prop.sum.min  <- qbeta(1 + 1, n - 1, p = confidence) * abs(tmin[1])
      if(length(tmin) > 2){
        prop.sum.min.2  <- 0
        for(i in 2:length(tmin)){
          prop.sum.min.2 <- prop.sum.min.2 + (qbeta(i + 1, n - 1, p = confidence) - qbeta((i-1) + 1, n - 1, p = confidence)) * abs(zmin[i])
        }
        prop.sum.min    <- prop.sum.min + prop.sum.min.2
      }
      bound             <- bound - prop.sum.min
    }
  } else if(!is.null(correction) && correction == "lta"){
    tmin <- subset(taints, taints < 0)
    if(length(tmin) > 0){
      ltaCorrection <- (1/n) * sum(abs(tmin))
      bound <- bound - ltaCorrection
    }
  } else if(!is.null(correction) && correction == "pvz"){
    taints <- ifelse(taints < 0, 0, taints)
    tmin <- sort(subset(taints, taints > 0))
    if(length(tmin) > 0){
      constant <- 0
      for(i in 1:length(tmin)){
        constant <- constant + (((n - 2 * i + 1)/(2 * sqrt(i*(n - i + 1)))) * rev(tmin)[i])
      }
      constant <- (1/n) * constant
      sigma <- (1/n) * sum((tmin - mean(tmin))^2)
      pvzCorrection <- (constatnt - sqrt(sigma)) / sqrt(n) * qnorm(p = confidence, lower.tail = TRUE)
      bound <- bound - pvzCorrection
    }
  }
  return(bound)
}
