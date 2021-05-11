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
  # To calculate the mode of the beta-binomial distribution we can use the fact that the mode has 
  # the largest probability to iteratively find it.
  if(shape1 == 1 && shape2 == 1)
    return(NA)
  index <- pcount <- pnextcount <- -1
  while (pnextcount >= pcount) {
    index <- index + 1
    pcount <- .dBetaBinom(x = index, N = N, shape1 = shape1, shape2 = shape2)
    pnextcount <- .dBetaBinom(x = index + 1, N = N, shape1 = shape1, shape2 = shape2)
  }
  return(index)
}

.qHyper <- function(p, N, n, k) {
  # To calculate the p percent upper bound on the population errors (K) using the hypergeometric 
  # distribution we perform an inverted hypothesis test (a la binom.test) as described on page 63 of 
  # Talens, E. (2005). Statistical Auditing and the AOQL-method (https://core.ac.uk/download/pdf/232379784.pdf).
  K <- k:N
  cdf <- stats::phyper(q = k, m = K, n = N - K, k = n)
  return(max(K[cdf > (1 - p)]))
}

# This function is for internal calls to the markdown renderer
.getfun <- function(x) {
  if (length(grep('::', x)) > 0) {
    parts <- strsplit(x, '::')[[1]]
    return(getExportedValue(parts[1], parts[2]))
  } else {
    return(x)
  }
}