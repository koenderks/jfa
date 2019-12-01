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
    cat("# jfa results for evaluation with", x$method,"method
#      
# Confidence:           ", paste0(round(x$confidence * 100, 2), "%"),"
# Bound:                ", paste0(round(x$confBound * 100, 2), "%"),"
# Sample size:          ", x$n,"
# Sum of sample errors: ", x$k)
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