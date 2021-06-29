# Consistent with JASP: https://github.com/jasp-stats/jaspDistributions/raw/master/R/ldBetaBinomial.R
.dBetaBinom <- function(x, N, shape1, shape2, log = FALSE) {
  out <- lchoose(N, x) + lbeta(x + shape1, N - x + shape2) - lbeta(shape1, shape2)
  if(!log) out <- exp(out)
  return(out)
}

# Consistent with JASP: https://github.com/jasp-stats/jaspDistributions/raw/master/R/ldBetaBinomial.R
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

# Consistent with JASP: https://github.com/jasp-stats/jaspDistributions/raw/master/R/ldBetaBinomial.R
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

# This function iteratively finds the mode (= value with the largest probability) of the 
# beta-binomial distribution by checking if p(k = i) < p(k = i + 1)
.modeBetaBinom <- function(N, shape1, shape2) {
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

# This function performs an inverted hypothesis test in order to calculate the p percent upper 
# bound on the population errors (K) using the hypergeometric distribution. 
# For more information, see: Talens, E. (2005). Statistical Auditing and the AOQL-method (https://core.ac.uk/download/pdf/232379784.pdf).
.qHyper <- function(p, N, n, k) {
  K <- k:N
  cdf <- stats::phyper(q = k, m = K, n = N - K, k = n)
  return(max(K[cdf > (1 - p)]))
}

# This function takes a value and restricts it to the range [0, 1] and is needed for
# controlling certain edge cases in beta-binomial calculations.
.restrictprob <- function(x) {
  if (x > 1) {
    return(1)
  } else if (x < 0) {
    return(0)
  } else {
    return(x)
  }
}

# This function is for internal calls to the markdown renderer only.
.getfun <- function(x) {
  if (length(grep('::', x)) > 0) {
    parts <- strsplit(x, '::')[[1]]
    return(getExportedValue(parts[1], parts[2]))
  } else {
    return(x)
  }
}