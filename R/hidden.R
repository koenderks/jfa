#' @method print jfaPlanning
#' @export
print.jfaPlanning <- function(x, ...){
  if(x[["prior"]]$prior){
    cat("# jfa planning results for", x[["prior"]]$priorD,"prior with", x$likelihood, "likelihood:
#      
# Materiality:            ", paste0(round(x$materiality * 100, 2), "%"),"
# Confidence:             ", paste0(round(x$confidence * 100, 2), "%"),"
# Sample size:            ", x$sampleSize,"
# Allowed sample errors:  ", x$expectedSampleError, "
# Prior parameter alpha:  ", x[["prior"]]$aPrior, "
# Prior parameter beta:   ", x[["prior"]]$bPrior)
  } else {
    cat("# jfa planning results for", x$likelihood,"likelihood:
#      
# Materiality:            ", paste0(round(x$materiality * 100, 2), "%"),"
# Confidence:             ", paste0(round(x$confidence * 100, 2), "%"),"
# Sample size:            ", x$sampleSize,"
# Allowed sample errors:  ", x$expectedSampleError)
  }
}

#' @method print jfaSampling
#' @export
print.jfaSampling <- function(x, ...){
  title <- paste0(x$algorithm, " ", ifelse(x$units == "mus", yes = "monetary unit sampling", no = "record sampling"), ":")
  if(x$units == "mus"){
    cat("# jfa sampling results for",title, "
#      
# Population size:        ", nrow(x$population),"
# Sample size:            ", nrow(x$sample),"
# Proportion n/N:         ", nrow(x$sample)/nrow(x$population), "
# Percentage of value:    ", paste0(round(sum(x$sample[, x$bookValues]) / sum(x$population[, x$bookValues]) * 100, 2), "%")) 
  } else {
    cat("# jfa sampling results for", x$algorithm, title, "
#      
# Population size:        ", nrow(x$population),"
# Sample size:            ", nrow(x$sample),"
# Proportion n/N:         ", nrow(x$sample)/nrow(x$population)) 
  }
}

#' @method print jfaEvaluation
#' @export
print.jfaEvaluation <- function(x, ...){
  if(x$method %in% c("direct", "difference", "quotient", "regression")){
    cat("# jfa evaluation results for", x$method,"estimation method:
#   
# Confidence:           ", paste0(round(x$confidence * 100, 2), "%"),"
# Population book value:", round(x$popBookvalue, 2), "
# Point estimate:       ", round(x$pointEstimate, 2), "
# Lower bound:          ", round(x$lowerBound, 2),"
# Upper bound:          ", round(x$upperBound, 2),"
# Conclusion:           ", x$conclusion)
  } else {
    if(x$prior){
      if(!is.null(x$materiality)){
        cat("# jfa evaluation results for", x$method,"likelihood with prior:
#   
# Materiality:          ", paste0(round(x$materiality * 100, 2), "%"),"
# Confidence:           ", paste0(round(x$confidence * 100, 2), "%"),"
# Upper bound:          ", paste0(round(x$confBound * 100, 3), "%"),"
# Sample size:          ", x$n,"
# Sample errors:        ", x$k, "
# Conclusion:           ", x$conclusion)
      } else {
        cat("# jfa evaluation results for", x$method,"likelihood with prior:
#      
# Confidence:           ", paste0(round(x$confidence * 100, 2), "%"),"
# Upper bound:          ", paste0(round(x$confBound * 100, 3), "%"),"
# Sample size:          ", x$n,"
# Sample errors:        ", x$k)
      }
    } else {
      if(!is.null(x$materiality)){
        cat("# jfa evaluation results for", x$method,"method:
#   
# Materiality:          ", paste0(round(x$materiality * 100, 2), "%"),"
# Confidence:           ", paste0(round(x$confidence * 100, 2), "%"),"
# Upper bound:          ", paste0(round(x$confBound * 100, 3), "%"),"
# Sample size:          ", x$n,"
# Sample errors:        ", x$k, "
# Conclusion:           ", x$conclusion)
      } else {
        cat("# jfa evaluation results for", x$method,"method:
#      
# Confidence:           ", paste0(round(x$confidence * 100, 2), "%"),"
# Upper bound:          ", paste0(round(x$confBound * 100, 3), "%"),"
# Sample size:          ", x$n,"
# Sample errors:        ", x$k)
      }
    }
  }
}

#' @method print jfaPrior
#' @export
print.jfaPrior <- function(x, ...){
  cat("# jfa prior distribution for", x$method,"method:
#      
# Prior sample size:    ", round(x$nPrior, 2), "
# Prior errors:         ", round(x$kPrior, 2),"
# Prior:                ", paste0(x$priorD, "(", x$aPrior, ", ", x$bPrior, ")")
  )
}

#' @method plot jfaPlanning
#' @export
plot.jfaPlanning <- function(x, ...){
  limx <- length(0:x$sampleSize)
  if(limx > 51){
    limx <- 51
  }
  if(x[["prior"]]$prior){
    xlim <- x$materiality * 3
    xseq <- seq(0, xlim, 0.00001)
    mainLab <- ifelse(x[["prior"]]$kPrior == 0 && x[["prior"]]$nPrior == 0, yes = "Uninformed", no = "Informed")
    if(x$likelihood == "poisson"){
      d <- stats::dgamma(xseq, shape = x[["prior"]]$aPrior, rate = x[["prior"]]$bPrior)
      d1 <- stats::dgamma(xseq, shape = x[["prior"]]$aPrior + x$expectedSampleError, rate = x[["prior"]]$bPrior + x$sampleSize)
      bound <- stats::qgamma(x$confidence, shape = x[["prior"]]$aPrior + x$expectedSampleError, rate = x[["prior"]]$bPrior + x$sampleSize)
    } else if(x$likelihood == "binomial"){
      d <- stats::dbeta(xseq, shape1 = x[["prior"]]$aPrior, shape2 = x[["prior"]]$bPrior)
      d1 <- stats::dbeta(xseq, shape1 = x[["prior"]]$aPrior + x$expectedSampleError, shape2 = x[["prior"]]$bPrior + x$sampleSize - x$expectedSampleError)
      bound <- stats::qbeta(x$confidence, shape1 = x[["prior"]]$aPrior + x$expectedSampleError, shape2 = x[["prior"]]$bPrior + x$sampleSize - x$expectedSampleError)
    } else if(x$likelihood == "hypergeometric"){
      xlim <- ceiling(xlim * x$N)
      xseq <- seq(0, xlim, by = 1)
      d <- .dBetaBinom(x = xseq, N = x$N, shape1 = x[["prior"]]$aPrior, shape2 = x[["prior"]]$bPrior)
      d1 <- .dBetaBinom(x = xseq, N = x$N, shape1 = x[["prior"]]$aPrior + x$expectedSampleError, shape2 = x[["prior"]]$bPrior + x$sampleSize - x$expectedSampleError)
      bound <- .qBetaBinom(p = x$confidence, N = x$N - x$sampleSize, shape1 = x[["prior"]]$aPrior + x$expectedSampleError, shape2 = x[["prior"]]$bPrior + x$sampleSize - x$expectedSampleError)
    }
    if(x$likelihood == "poisson" || x$likelihood == "binomial"){
      if(x$likelihood == "poisson")
        mainLabPlus <- " gamma prior and expected posterior"
      if(x$likelihood == "binomial")
        mainLabPlus <- " beta prior and expected posterior"
      graphics::plot(x = xseq, y = d1, type = "l", lwd = 2, bty = "n", xlab = "Misstatement", ylab = "Probability density", las = 1, ylim = c(0, max(d1)),
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
    graphics::barplot(d, xlab = "Errors", col = "lightgray", ylab = "Probability", las = 1, main = mainLab, width = 1, space = 0)
    graphics::axis(1, at = seq(0, limx, by = 10) + 0.5, labels = seq(0, limx, by = 10))
    graphics::barplot(d1, col = "darkgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
    graphics::legend("topright", legend = c("Expected error free", "Expected errors"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
  }
}

#' @method plot jfaSampling
#' @export
plot.jfaSampling <- function(x, ...){
  name <- x$bookValues
  graphics::hist(x$population[[name]], breaks = 30, main = "Histogram of population and sample book values", xlab = "Book values", las = 1, col = "lightgray")
  graphics::hist(x$sample[[name]], breaks = 30, main = "Sample", xlab = "Book values", las = 1, add = TRUE, col = "darkgray")
  graphics::legend("topright", legend = c("Population", "Sample"), bty = "n", fill = c("lightgray", "darkgray"))
}

#' @method plot jfaEvaluation
#' @export
plot.jfaEvaluation <- function(x, ...){
  if(x$method %in% c("stringer", "stringer-meikle", "stringer-lta", "stringer-pvz", "rohrbach", "moment"))
    stop("No plotting method available for a confidence bound from this method")
  if(x$method %in% c("direct", "difference", "quotient", "regression")){
    ymin <- x$lowerBound - (x$pointEstimate - x$lowerBound)
    ymax <- x$upperBound + (x$upperBound - x$pointEstimate)
    ticks <- pretty(ymin, ymax, min.n = 5)
    graphics::plot(x = 0, y = x$pointEstimate, bty = "n", cex = 2, pch = 19, xlab = "Population book value", ylab = "", 
                   ylim = c(min(ticks), max(ticks)), xlim = c(-0.1, 0.1), axes = FALSE)
    graphics::arrows(x0 = 0, x1 = 0, y0 = x$lowerBound, y1 = x$upperBound, code = 3, lwd = 2, col = "black", angle = 90)
    graphics::axis(2, at = ticks, las = 1)
    graphics::abline(h = x$popBookvalue, lty = 2)
  } else {
    limx <- length(0:x$n)
    if(limx > 51){
      limx <- 51
    }
    if(x$prior){
      xlim <- x$materiality * 3
      if(length(xlim) == 0)
        xlim <- 0.30
      xseq <- seq(0, xlim, 0.00001)
      mainLab <- ifelse(x$kPrior == 0 && x$nPrior == 0, yes = "Uninformed", no = "Informed")
      if(x$method == "poisson"){
        d <- stats::dgamma(xseq, shape = 1 + x$kPrior, rate = x$nPrior)
        d1 <- stats::dgamma(xseq, shape = 1 + x$kPrior + x$t, rate = x$nPrior + x$n)
        bound <- stats::qgamma(x$confidence, shape = 1 + x$kPrior + x$t, rate = x$nPrior + x$n)
      } else if(x$method == "binomial"){
        d <- stats::dbeta(xseq, shape1 = 1 + x$kPrior, shape2 = 1 + x$nPrior - x$kPrior)
        d1 <- stats::dbeta(xseq, shape1 = 1 + x$kPrior + x$t, shape2 = 1 + x$nPrior + x$n - x$t)
        bound <- stats::qbeta(x$confidence, shape1 = x$kPrior + x$t, shape2 = 1 + x$nPrior + x$n - x$t)
      } else if(x$method == "hypergeometric"){
        xlim <- ceiling(xlim * x$N)
        xseq <- seq(0, xlim, by = 1)
        d <- .dBetaBinom(x = xseq, N = x$N, shape1 = 1 + x$kPrior, shape2 = 1 + x$nPrior - x$kPrior)
        d1 <- .dBetaBinom(x = xseq, N = x$N, shape1 = 1 + x$kPrior + x$k, shape2 = 1 + x$nPrior + x$n - x$k)
        bound <- .qBetaBinom(p = x$confidence, N = x$N - x$n, shape1 = 1 + x$kPrior + x$k, shape2 = 1 + x$nPrior + x$n - x$k)
      }
      if(x$method == "poisson" || x$method == "binomial"){
        if(x$method == "poisson")
          mainLabPlus <- " gamma prior and posterior"
        if(x$method == "binomial")
          mainLabPlus <- " beta prior and posterior"
        graphics::plot(x = xseq, y = d1, type = "l", lwd = 2, bty = "n", xlab = "Misstatement", ylab = "Probability density", las = 1, ylim = c(0, max(d1)),
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
      if(is.null(x$materiality))
        stop("Cannot plot a frequentist outcome when the materiality is unknown.")
      if(x$method == "poisson"){
        mainLab <- paste0("Poisson distribution (lambda = ", round(x$materiality * x$n, 2), ")")
        d <- stats::dpois(x = 0:x$n, lambda = x$materiality * x$n)[1:limx]
        d1 <- stats::dpois(x = 0:x$k, lambda = x$materiality * x$n)
      } else if(x$method == "binomial"){
        mainLab <- paste0("Binomial distribution (n = ", x$n, ", p = ", round(x$materiality, 2),")")
        d <- stats::dbinom(x = 0:x$n, size = x$n, prob = x$materiality)[1:limx]
        d1 <- stats::dbinom(x = 0:x$k, size = x$n, prob = x$materiality)
      } else if(x$method == "hypergeometric"){
        mainLab <- paste0("Hypergeometric distribution (N = ", x$N, ", n = ", x$n, ", K = ", 
                          x$populationK, ", k = ", x$k,")")
        d <- stats::dhyper(x = 0:x$n, m = x$populationK, n = x$N - x$populationK, k = x$n)[1:limx]
        d1 <- stats::dhyper(x = 0:x$k, m = x$populationK, n = x$N - x$populationK, k = x$n)
      }
      graphics::barplot(d, xlab = "Errors", col = "lightgray", ylab = "Probability", las = 1, main = mainLab, width = 1, space = 0)
      graphics::axis(1, at = seq(0, limx, by = 10) + 0.5, labels = seq(0, limx, by = 10))
      graphics::barplot(d1, col = "darkgray", add = TRUE, las = 1, axes = FALSE, width = 1, space = 0)
      graphics::legend("topright", legend = c("Error free", "Errors"), fill = c("lightgray", "darkgray"), bty = "n", cex = 1.2) 
    }
  }
}

#' @method plot jfaPrior
#' @export
plot.jfaPrior <- function(x, ...){
  xlim <- x$materiality * 3
  xseq <- seq(0, xlim, 0.00001)
  mainLab <- ifelse(x$kPrior == 0 && x$nPrior == 0, yes = "Uninformed", no = "Informed")
  if(x$priorD == "gamma"){
    d <- stats::dgamma(xseq, shape = x$aPrior, rate = x$bPrior)
  } else if(x$priorD == "beta"){
    d <- stats::dbeta(xseq, shape1 = x$aPrior, shape2 = x$bPrior)
  } else if(x$priorD == "beta-binomial"){
    xlim <- ceiling(xlim * x$N)
    xseq <- seq(0, xlim, by = 1)
    d <- .dBetaBinom(x = xseq, N = x$N, shape1 = x$aPrior, shape2 = x$bPrior)
  }
  mainLab <- paste0(mainLab, " ", x$priorD, " prior")
  if(x$priorD == "gamma" || x$priorD == "beta"){
    graphics::plot(x = xseq, y = d, type = "l", lwd = 2, bty = "n", xlab = "Misstatement", ylab = "Probability density", las = 1, ylim = c(0, max(d)),
                   main = mainLab, axes = FALSE, lty = 2)
    graphics::axis(1, at = pretty(seq(0, xlim, by = 0.01), min.n = 5), labels = paste0(round(pretty(seq(0, xlim, by = 0.01), min.n = 5) * 100, 2), "%"))
    graphics::axis(2, at = c(0, max(d)), labels = FALSE, las = 1, lwd.ticks = 0)
    graphics::legend("topright", legend = "Prior", lty = 2, bty = "n", cex = 1.2, lwd = 2)
  } else {
    graphics::barplot(d, bty = "n", xlab = "Errors", ylab = "Probability", las = 1, ylim = c(0, max(d)), width = 1, space = 0,
                      main = mainLab, axes = FALSE, col = "darkgray")
    graphics::axis(1, at = seq(0, xlim, by = 10) + 0.5, labels = seq(0, xlim, by = 10))
    graphics::axis(2, at = c(0, max(d)), labels = FALSE, las = 1, lwd.ticks = 0)
    graphics::legend("topright", legend = "Prior", fill = "lightgray", bty = "n", cex = 1.2) 
  }
}
