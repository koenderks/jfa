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
  } else if(x$jfaType == "evaluation"){
    if(!is.null(x$materiality)){
      cat("# jfa results for evaluation with", x$method,"method
#   
# Materiality:          ", paste0(round(x$materiality * 100, 2), "%"),"
# Confidence:           ", paste0(round(x$confidence * 100, 2), "%"),"
# Bound:                ", paste0(round(x$confBound * 100, 3), "%"),"
# Sample size:          ", x$n,"
# Sample errors:        ", x$k, "
# Conclusion:           ", x$conclusion)
  } else {
      cat("# jfa results for evaluation with", x$method,"method
#      
# Confidence:           ", paste0(round(x$confidence * 100, 2), "%"),"
# Bound:                ", paste0(round(x$confBound * 100, 3), "%"),"
# Sample size:          ", x$n,"
# Sample errors:        ", x$k)
    }
  }
}

.dBetaBinom <- function (x, N, shape1, shape2){
  logval <- lbeta(x + shape1, N - x + shape2) - lbeta(shape1, shape2) + lchoose(N, x)
  ret <- exp(logval)
  return(ret)
}

.qBetaBinom <- function (p, N, shape1, shape2){
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