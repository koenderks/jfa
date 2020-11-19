.dBetaBinom <- function(x, N, shape1, shape2){
	logval <- lbeta(x + shape1, N - x + shape2) - lbeta(shape1, shape2) + lchoose(N, x)
	ret <- exp(logval)
	return(ret)
}

.qBetaBinom <- function(p, N, shape1, shape2){
	pp <- cumsum(.dBetaBinom(0:N, N, shape1, shape2))
	return(sapply(p, function(x) sum(pp < x)))
}

.pBetaBinom <- function(q, N, shape1, shape2){
	p <- sum(.dBetaBinom(0:q, N, shape1, shape2))
	return(p)
}

# .dCoxAndSnellF <- function(x, df1, df2, multiplicationFactor){
# 	# Rewritten using Wolfram Mathematica
# 	(df1 ** (df1 / 2) * df2**(df2 / 2) * (x / multiplicationFactor) ** (- 1 + df1 / 2) * (df2 + (df1 * x) / multiplicationFactor)**(( -df1 - df2) / 2))/(abs(multiplicationFactor) * beta(df1/2, df2/2))
# }

.getfun<-function(x) {
  if(length(grep("::", x))>0) {
    parts<-strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else {
    x
  }
}