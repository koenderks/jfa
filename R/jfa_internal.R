# Quantiles of the beta-binomial distribution
.qbbinom <- function(p, N, shape1, shape2, lower.tail = TRUE, log.p = FALSE) {
  if (shape1 == 0 || shape2 == 0)
    return(Inf)
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p
  x <- extraDistr::dbbinom(x = 0:N, size = N, alpha = shape1, beta = shape2)
  q <- which(cumsum(x) > p)[1]
  return(q)
}

# This function finds the mode (= value with the highest probability) of the beta-binomial distribution
.modebbinom <- function(N, shape1, shape2) {
  if((shape1 == 1 && shape2 == 1) || shape1 == 0 || shape2 == 0)
    return(NA)
  index <- which.max(extraDistr::dbbinom(x = 0:N, size = N, alpha = shape1, beta = shape2)) - 1
  return(index)
}

# This function performs an inverted hypothesis test in order to calculate the p percent upper 
# bound on the population errors (K) using the hypergeometric distribution. 
# For more information, see: Talens, E. (2005). Statistical Auditing and the AOQL-method (https://core.ac.uk/download/pdf/232379784.pdf).
.qhyper <- function(p, N, n, k) {
  K <- k:N
  cdf <- stats::phyper(q = k, m = K, n = N - K, k = n)
  return(max(K[cdf > (1 - p)]))
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