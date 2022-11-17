# Copyright (C) 2020-2022 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

.qbbinom <- function(p, N, shape1, shape2, lower.tail = TRUE, log.p = FALSE) {
  improper <- shape1 == 0 || shape2 == 0
  if (improper) {
    return(Inf)
  }
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  x <- extraDistr::dbbinom(x = 0:N, size = N, alpha = shape1, beta = shape2)
  q <- which(cumsum(x) > p)[1] - 1
  return(q)
}

.modebbinom <- function(N, shape1, shape2) {
  no_mode <- (shape1 == 1 && shape2 == 1) || shape1 == 0 || shape2 == 0
  if (no_mode) {
    return(NA)
  }
  if (shape1 == 1 && shape2 > 1) {
    return(0)
  }
  index <- which.max(extraDistr::dbbinom(x = 0:N, size = N, alpha = shape1, beta = shape2)) - 1
  return(index)
}

.entropy <- function(x) {
  freq <- as.numeric(table(x))
  prob <- freq / sum(freq)
  logProb <- log(prob)
  entropy <- -sum(ifelse(!is.infinite(logProb), prob * logProb, 0))
  return(entropy)
}

.average_frequency <- function(x) {
  counts <- table(x)
  average_frequency <- sum(counts^2) / sum(counts)
  return(average_frequency)
}

.compute_mode <- function(x) {
  dens <- stats::density(x)
  mode <- dens$x[which.max(dens$y)]
  return(mode)
}

.get_markdown_call <- function(x) {
  if (length(grep("::", x)) > 0) {
    parts <- strsplit(x, "::")[[1]]
    return(getExportedValue(parts[1], parts[2]))
  } else {
    return(x)
  }
}

.distribution_string <- function(likelihood, alpha, beta, N.units = NULL) {
  string <- switch(likelihood,
    "poisson" = paste0("gamma(\u03B1 = ", round(alpha, 3), ", \u03B2 = ", round(beta, 3), ")"),
    "binomial" = paste0("beta(\u03B1 = ", round(alpha, 3), ", \u03B2 = ", round(beta, 3), ")"),
    "hypergeometric" = paste0("beta-binomial(N = ", N.units, ", \u03B1 = ", round(alpha, 3), ", \u03B2 = ", round(beta, 3), ")")
  )
  return(string)
}

.hypothesis_string <- function(materiality) {
  h1_string <- paste0("H\u2081: \u0398 < ", materiality)
  h0_string <- paste0("H\u2080: \u0398 > ", materiality)
  strings <- c(h1_string, h0_string)
  return(strings)
}

.distribution_mode <- function(likelihood = NULL, alpha = NULL, beta = NULL, N.units = NULL, samples = NULL) {
  if (!is.null(samples)) {
    dens <- stats::density(samples)
    mode <- dens$x[which.max(dens$y)]
  } else {
    mode <- switch(likelihood,
      "poisson" = (alpha - 1) / beta,
      "binomial" = (alpha - 1) / (alpha + beta - 2),
      "hypergeometric" = .modebbinom(N.units, alpha, beta)
    )
  }
  return(mode)
}

.distribution_mean <- function(likelihood, alpha, beta, N.units = NULL) {
  mean <- switch(likelihood,
    "poisson" = alpha / beta,
    "binomial" = alpha / (alpha + beta),
    "hypergeometric" = alpha / (alpha + beta) * N.units
  )
  return(mean)
}

.distribution_median <- function(likelihood, alpha, beta, N.units = NULL) {
  median <- switch(likelihood,
    "poisson" = stats::qgamma(0.5, alpha, beta),
    "binomial" = stats::qbeta(0.5, alpha, beta),
    "hypergeometric" = .qbbinom(0.5, N.units, alpha, beta)
  )
  return(median)
}

.distribution_variance <- function(likelihood, alpha, beta, N.units = NULL) {
  variance <- switch(likelihood,
    "poisson" = alpha / beta^2,
    "binomial" = (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1)),
    "hypergeometric" = ((N.units * alpha * beta) * (alpha + beta + N.units)) / ((alpha + beta)^2 * (alpha + beta + 1))
  )
  return(variance)
}

.distribution_skewness <- function(likelihood, alpha, beta, N.units = NULL) {
  skewness <- switch(likelihood,
    "poisson" = 2 / sqrt(alpha),
    "binomial" = ((2 * (beta - alpha)) * sqrt(alpha + beta + 1)) / ((alpha + beta + 2) * sqrt(alpha * beta)),
    "hypergeometric" = (((alpha + beta + 2 * N.units) * (beta - alpha)) / (alpha + beta + 2)) * sqrt((1 + alpha + beta) / (N.units * alpha * beta * (N.units + alpha + beta)))
  )
  return(skewness)
}

.distribution_ub <- function(likelihood, conf.level, alpha, beta, N.units = NULL) {
  ub <- switch(likelihood,
    "poisson" = stats::qgamma(conf.level, alpha, beta),
    "binomial" = stats::qbeta(conf.level, alpha, beta),
    "hypergeometric" = .qbbinom(conf.level, N.units, alpha, beta)
  )
  return(ub)
}

.hypothesis_density <- function(materiality, likelihood, alpha, beta, N.units = NULL) {
  density <- switch(likelihood,
    "poisson" = stats::dgamma(materiality, alpha, beta),
    "binomial" = stats::dbeta(materiality, alpha, beta),
    "hypergeometric" = extraDistr::dbbinom(ceiling(materiality * N.units), N.units, alpha, beta)
  )
  return(density)
}

.hypothesis_probability <- function(hyp, materiality, likelihood, alpha, beta, N.units = NULL) {
  if (hyp == "tolerable") {
    prob <- switch(likelihood,
      "poisson" = stats::pgamma(materiality, alpha, beta),
      "binomial" = stats::pbeta(materiality, alpha, beta),
      "hypergeometric" = extraDistr::pbbinom(ceiling(materiality * N.units) - 1, N.units, alpha, beta)
    )
  } else {
    prob <- switch(likelihood,
      "poisson" = stats::pgamma(materiality, alpha, beta, lower.tail = FALSE),
      "binomial" = stats::pbeta(materiality, alpha, beta, lower.tail = FALSE),
      "hypergeometric" = extraDistr::pbbinom(ceiling(materiality * N.units) - 1, N.units, alpha, beta, lower.tail = FALSE)
    )
  }
}

.poststratify_samples <- function(samples, N.units) {
  n_strata <- ncol(samples)
  if (is.null(N.units)) {
    weights <- rep(1, n_strata) / n_strata
  } else {
    weights <- N.units[-1] / N.units[1]
  }
  poststratified_samples <- samples %*% weights
  return(poststratified_samples)
}

# This function performs an inverted hypothesis test in order to calculate the p percent upper
# bound on the population errors (K) using the hypergeometric distribution.
# For more information, see: Talens, E. (2005). Statistical Auditing and the AOQL-method (https://core.ac.uk/download/pdf/232379784.pdf).
.qhyper <- function(p, N, n, k) {
  K <- k:N
  cdf <- stats::phyper(q = k, m = K, n = N - K, k = n)
  return(max(K[cdf > (1 - p)]))
}

.extract_digits <- function(x, check = "first", include.zero = FALSE) {
  x <- x[!is.na(x)]
  x <- x[!is.infinite(x)]
  if (check == "first") {
    if (include.zero) {
      digits <- as.numeric(substring(abs(x), 1, 1))
    } else {
      digits <- as.numeric(substring(format(abs(x), scientific = TRUE), 1, 1))
      digits[x == 0] <- NA
    }
  } else if (check == "firsttwo") {
    if (include.zero) {
      x <- formatC(abs(x), digits = 10, format = "f")
      x <- gsub(x = x, pattern = "[.]", replacement = "")
      digits <- as.numeric(substring(x, 1, 2))
    } else {
      digits <- as.numeric(substring(format(abs(x), scientific = TRUE), 1, 3)) * 10
      digits[x == 0] <- NA
    }
  } else if (check == "before") {
    if (include.zero) {
      digits <- ifelse(x > 0, yes = floor(x), no = ceiling(x))
    } else {
      digits <- ifelse(x > 0, yes = floor(x), no = ceiling(x))
      digits[digits == 0] <- NA
    }
  } else if (check == "after") {
    if (include.zero) {
      stringedX <- format(abs(x) %% 1, drop0trailing = FALSE)
      digits <- ifelse(nchar(stringedX) == 1, yes = as.numeric(stringedX), no = as.numeric(substring(stringedX, 3, nchar(stringedX))))
    } else {
      stringedX <- format(abs(x) %% 1, drop0trailing = TRUE)
      digits <- as.numeric(substring(stringedX, 3, nchar(stringedX)))
      digits[x %% 2 == 0] <- NA
    }
  } else if (check == "lasttwo") {
    if (include.zero) {
      stringedX <- format(abs(x) %% 1, drop0trailing = FALSE)
      digits <- as.numeric(substring(stringedX, nchar(stringedX) - 1, nchar(stringedX)))
    } else {
      stringedX <- format(abs(x) %% 1, drop0trailing = FALSE)
      digits <- as.numeric(substring(stringedX, nchar(stringedX) - 1, nchar(stringedX)))
      digits[x %% 2 == 0] <- NA
    }
  } else if (check == "last") {
    if (include.zero) {
      x <- formatC(x, digits = 2, format = "f")
      digits <- as.numeric(substring(x, nchar(x), nchar(x)))
    } else {
      stringedX <- sub("0+$", "", as.character(abs(x)))
      digits <- as.numeric(substring(stringedX, nchar(stringedX), nchar(stringedX)))
      digits[x == 0] <- NA
    }
  } else {
    stop("specify a valid input for the 'check' argument")
  }
  return(digits)
}

# This function fits a stan model using partial pooling and returns samples from the stratum posteriors
.partial_pooling <- function(method, prior.x, prior.n, n.obs, t.obs, t, nstrata, stratum, likelihood) {
  stopifnot("'method = hypergeometric' does not support pooling" = method != "hypergeometric")
  data <- switch(likelihood,
    "binomial" = list(S = nstrata - 1, n = n.obs[-1], k = t.obs[-1], priorx = prior.x, priorn = prior.n, beta_prior = as.numeric(method == "binomial")),
    "beta" = list(S = nstrata - 1, t = (t[[1]] * (n.obs[1] - 1) + 0.5) / n.obs[1], n = n.obs[1], s = as.numeric(stratum), priorx = prior.x, priorn = prior.n, beta_prior = as.numeric(method == "binomial"))
  )
  utils::capture.output(
    file = "NUL",
    raw_prior <- rstan::sampling(
      object = stanmodels[[paste0("pp_", likelihood)]], data = c(data, use_likelihood = 0), pars = "theta_s", iter = getOption("mcmc.iterations", 2000),
      warmup = getOption("mcmc.warmup", 1000), chains = getOption("mcmc.chains", 4), cores = getOption("mcmc.cores", 1), seed = ceiling(stats::runif(1, -1000, 1000)),
      control = list(adapt_delta = 0.95)
    )
  )
  raw_posterior <- rstan::sampling(
    object = stanmodels[[paste0("pp_", likelihood)]], data = c(data, use_likelihood = 1), pars = "theta_s", iter = getOption("mcmc.iterations", 2000),
    warmup = getOption("mcmc.warmup", 1000), chains = getOption("mcmc.chains", 4), cores = getOption("mcmc.cores", 1), seed = ceiling(stats::runif(1, -1000, 1000)),
    control = list(adapt_delta = 0.95)
  )
  samples <- cbind(rstan::extract(raw_posterior)$theta_s, rstan::extract(raw_prior)$theta_s)
  stopifnot("stan model could not be fitted..." = ncol(samples) == (nstrata - 1) * 2)
  return(samples)
}

.sample_analytical <- function(method, nstrata, bayesian, prior.x, t.obs, prior.n, n.obs, N.units, iterations) {
  samples <- matrix(NA, ncol = (nstrata - 1) * 2, nrow = iterations)
  for (i in 2:nstrata) {
    samples[, i - 1] <- switch(method,
      "poisson" = stats::rgamma(n = iterations, 1 + prior.x + t.obs[i], prior.n + n.obs[i]),
      "binomial" = stats::rbeta(n = iterations, 1 + prior.x + t.obs[i], prior.n - prior.x + n.obs[i] - t.obs[i]),
      "hypergeometric" = extraDistr::rbbinom(n = iterations, N.units[i] - n.obs[i], 1 + prior.x + t.obs[i], prior.n - prior.x + n.obs[i] - t.obs[i]) / N.units[i]
    )
  }
  for (i in (nstrata + 1):((nstrata - 1) * 2 + 1)) {
    samples[, i - 1] <- switch(method,
      "poisson" = stats::rgamma(n = iterations, 1 + prior.x, prior.n),
      "binomial" = stats::rbeta(n = iterations, 1 + prior.x, prior.n - prior.x),
      "hypergeometric" = extraDistr::rbbinom(n = iterations, N.units[i - nstrata], 1 + prior.x, prior.n - prior.x) / N.units[i - nstrata]
    )
  }
  return(samples)
}
