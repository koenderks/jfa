# From https://github.com/jasp-stats/jaspDistributions/raw/master/R/ldBetaBinomial.R
.dBetaBinom <- function(x, N, shape1, shape2, log = FALSE) {
  out <- lchoose(N, x) + lbeta(x + shape1, N - x + shape2) - lbeta(shape1, shape2)
  if(!log) out <- exp(out)
  return(out)
}

# From https://github.com/jasp-stats/jaspDistributions/raw/master/R/ldBetaBinomial.R
.qBetaBinom <- function(p, N, shape1, shape2, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p
  .q <- function(p, N, shape1, shape2) {
    q <- 0
    cdf <- 0
    while(cdf < p) {
      cdf <- cdf + .dBetaBinom(q, N, shape1, shape2, log = FALSE)
      q   <- q + 1
    }
    return(q)
  }
  result <- sapply(p, .q, N = N, shape1 = shape1, shape2 = shape2)
  return(result)
}

# From https://github.com/jasp-stats/jaspDistributions/raw/master/R/ldBetaBinomial.R
.pBetaBinom <- function(q, N, shape1, shape2, lower.tail = TRUE, log.p = FALSE) {
  .p <- function(q, N, shape1, shape2) {
    if(q < 0) {
      0
    } else if (q >= N) {
      1
    } else {
      sum(.dBetaBinom(0:q, N, shape1, shape2, log = FALSE))
    }
  }
  out <- sapply(q, .p, N = N, shape1 = shape1, shape2 = shape2)
  if(!lower.tail) out <- 1-out
  if(log.p) out <- log(out)
  return(out)
}

.modeBetaBinom <- function(N, shape1, shape2) {
  if(shape1 == 1 && shape2 == 1)
	return(NA)
  index <- pcount <- pnextcount <- -1
  # Use the fact that the mode has the highest density to iteratively find it
  while (pnextcount >= pcount) {
    index <- index + 1
    pcount <- .dBetaBinom(x = index, N = N, shape1 = shape1, shape2 = shape2)
    pnextcount <- .dBetaBinom(x = index + 1, N = N, shape1 = shape1, shape2 = shape2)
  }
  return(index)
}

# .dCoxAndSnellF <- function(x, df1, df2, multiplicationFactor) {
# 	# Rewritten using Wolfram Mathematica
# 	(df1 ** (df1 / 2) * df2**(df2 / 2) * (x / multiplicationFactor) ** (- 1 + df1 / 2) * (df2 + (df1 * x) / multiplicationFactor)**(( -df1 - df2) / 2))/(abs(multiplicationFactor) * beta(df1/2, df2/2))
# }

.getfun <- function(x) {
  if (length(grep("::", x))>0) {
    parts<-strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else {
    x
  }
}