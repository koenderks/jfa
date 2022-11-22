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

.markdown_call <- function(x) {
  if (length(grep("::", x)) > 0) {
    parts <- strsplit(x, "::")[[1]]
    return(getExportedValue(parts[1], parts[2]))
  } else {
    return(x)
  }
}

.functional_density <- function(likelihood, analytical = TRUE) {
  if (analytical) {
    form <- switch(likelihood,
      "poisson" = "gamma",
      "binomial" = "beta",
      "hypergeometric" = "beta-binomial"
    )
  } else {
    form <- "MCMC"
  }
  return(form)
}

.functional_form <- function(likelihood, alpha, beta, N.units, analytical = TRUE) {
  if (analytical) {
    string <- switch(likelihood,
      "poisson" = paste0("gamma(\u03B1 = ", round(alpha, 3), ", \u03B2 = ", round(beta, 3), ")"),
      "binomial" = paste0("beta(\u03B1 = ", round(alpha, 3), ", \u03B2 = ", round(beta, 3), ")"),
      "hypergeometric" = paste0("beta-binomial(N = ", N.units, ", \u03B1 = ", round(alpha, 3), ", \u03B2 = ", round(beta, 3), ")")
    )
  } else {
    string <- "Approximated via MCMC sampling"
  }
  return(string)
}

.comp_precision <- function(alternative, mle, lb, ub) {
  if (alternative == "greater") {
    precision <- mle - lb
  } else {
    precision <- ub - mle
  }
  return(precision)
}

.comp_mle_freq <- function(likelihood, n.obs, x.obs, t.obs, N.units) {
  if (is.null(N.units)) {
    N.units <- rep(1, length(n.obs))
  }
  if (likelihood == "hypergeometric") {
    mle <- (x.obs / n.obs) %*% N.units / sum(N.units)
  } else {
    mle <- (t.obs / n.obs) %*% N.units / sum(N.units)
  }
  return(mle)
}

.comp_mode_bayes <- function(likelihood, alpha, beta, N.units, analytical = TRUE, samples = NULL) {
  if (analytical) {
    mode <- switch(likelihood,
      "poisson" = (alpha - 1) / beta,
      "binomial" = (alpha - 1) / (alpha + beta - 2),
      "hypergeometric" = .modebbinom(N.units, alpha, beta)
    )
  } else {
    dens <- stats::density(samples)
    mode <- dens[["x"]][which.max(dens[["y"]])]
  }
  return(mode)
}

.modebbinom <- function(N, shape1, shape2) {
  no_mode <- (shape1 == 1 && shape2 == 1) || shape1 == 0 || shape2 == 0
  if (no_mode) {
    return(NA)
  }
  if (shape1 == 1 && shape2 > 1) {
    return(0)
  }
  mode <- which.max(extraDistr::dbbinom(x = 0:N, size = N, alpha = shape1, beta = shape2)) - 1
  return(mode)
}

.comp_mean_bayes <- function(likelihood, alpha, beta, N.units, analytical = TRUE, samples = NULL) {
  if (analytical) {
    mean <- switch(likelihood,
      "poisson" = alpha / beta,
      "binomial" = alpha / (alpha + beta),
      "hypergeometric" = alpha / (alpha + beta) * N.units
    )
  } else {
    mean <- mean(samples)
  }
  return(mean)
}

.comp_median_bayes <- function(likelihood, alpha, beta, N.units, analytical = TRUE, samples = NULL) {
  if (analytical) {
    median <- switch(likelihood,
      "poisson" = stats::qgamma(0.5, alpha, beta),
      "binomial" = stats::qbeta(0.5, alpha, beta),
      "hypergeometric" = .qbbinom(0.5, N.units, alpha, beta)
    )
  } else {
    median <- stats::median(samples)
  }
  return(median)
}

.comp_var_bayes <- function(likelihood, alpha, beta, N.units, analytical = TRUE, samples = NULL) {
  if (analytical) {
    variance <- switch(likelihood,
      "poisson" = alpha / beta^2,
      "binomial" = (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1)),
      "hypergeometric" = ((N.units * alpha * beta) * (alpha + beta + N.units)) / ((alpha + beta)^2 * (alpha + beta + 1))
    )
  } else {
    variance <- stats::var(samples)
  }
  return(variance)
}

.comp_skew_bayes <- function(likelihood, alpha, beta, N.units, analytical = TRUE, samples = NULL) {
  if (analytical) {
    skewness <- switch(likelihood,
      "poisson" = 2 / sqrt(alpha),
      "binomial" = ((2 * (beta - alpha)) * sqrt(alpha + beta + 1)) / ((alpha + beta + 2) * sqrt(alpha * beta)),
      "hypergeometric" = (((alpha + beta + 2 * N.units) * (beta - alpha)) / (alpha + beta + 2)) * sqrt((1 + alpha + beta) / (N.units * alpha * beta * (N.units + alpha + beta)))
    )
  } else {
    skewness <- moments::skewness(samples)
  }
  return(skewness)
}

.comp_ub_freq <- function(alternative, conf.level, likelihood, n.obs, x.obs, t.obs, N.units) {
  if (alternative == "greater") {
    ub <- 1
  } else {
    if (alternative == "less") {
      prob <- conf.level
    } else {
      prob <- conf.level + (1 - conf.level) / 2
    }
    ub <- switch(likelihood,
      "poisson" = stats::qgamma(prob, 1 + t.obs, n.obs),
      "binomial" = stats::qbeta(prob, 1 + t.obs, n.obs - t.obs),
      "hypergeometric" = .qhyper(prob, N.units, n.obs, x.obs)
    )
  }
  return(ub)
}

.comp_lb_freq <- function(alternative, conf.level, likelihood, n.obs, x.obs, t.obs, N.units) {
  if (alternative == "less") {
    lb <- 0
  } else {
    if (alternative == "greater") {
      prob <- 1 - conf.level
    } else {
      prob <- (1 - conf.level) / 2
    }
    lb <- switch(likelihood,
      "poisson" = stats::qgamma(prob, t.obs, 1 + n.obs),
      "binomial" = stats::qbeta(prob, t.obs, 1 + n.obs - t.obs),
      "hypergeometric" = .qhyper(prob, N.units, n.obs, x.obs)
    )
  }
  return(lb)
}

# Talens, E. (2005). Statistical Auditing and the AOQL-method
# https://core.ac.uk/download/pdf/232379784.pdf
.qhyper <- function(p, N, n, k) {
  K <- k:N
  cdf <- stats::phyper(q = k, m = K, n = N - K, k = n)
  return(max(K[cdf > (1 - p)]))
}

.comp_ub_bayes <- function(alternative, conf.level, likelihood, alpha, beta, N.units, analytical = TRUE, samples = NULL) {
  if (alternative == "greater") {
    ub <- 1
  } else {
    if (alternative == "less") {
      prob <- conf.level
    } else {
      prob <- conf.level + (1 - conf.level) / 2
    }
    if (analytical) {
      ub <- switch(likelihood,
        "poisson" = stats::qgamma(prob, alpha, beta),
        "binomial" = stats::qbeta(prob, alpha, beta),
        "hypergeometric" = .qbbinom(prob, N.units, alpha, beta)
      )
    } else {
      ub <- as.numeric(stats::quantile(samples, prob))
    }
  }
  return(ub)
}

.comp_lb_bayes <- function(alternative, conf.level, likelihood, alpha, beta, N.units, analytical = TRUE, samples = NULL) {
  if (alternative == "less") {
    lb <- 0
  } else {
    if (alternative == "greater") {
      prob <- 1 - conf.level
    } else {
      prob <- (1 - conf.level) / 2
    }
    if (analytical) {
      lb <- switch(likelihood,
        "poisson" = stats::qgamma(prob, alpha, beta),
        "binomial" = stats::qbeta(prob, alpha, beta),
        "hypergeometric" = .qbbinom(prob, N.units, alpha, beta)
      )
    } else {
      lb <- as.numeric(stats::quantile(samples, prob))
    }
  }
  return(lb)
}

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

.hyp_string <- function(materiality, alternative) {
  if (alternative == "less") {
    h1_string <- paste0("H\u2081: \u0398 < ", materiality)
    h0_string <- paste0("H\u2080: \u0398 > ", materiality)
  } else if (alternative == "greater") {
    h1_string <- paste0("H\u2081: \u0398 > ", materiality)
    h0_string <- paste0("H\u2080: \u0398 < ", materiality)
  } else if (alternative == "two.sided") {
    h1_string <- paste0("H\u2081: \u0398 \u2260 ", materiality)
    h0_string <- paste0("H\u2080: \u0398 = ", materiality)
  }
  strings <- c(h1_string, h0_string)
  return(strings)
}

.comp_pval <- function(alternative, materiality, likelihood, n.obs, x.obs, t.obs, N.units, K) {
  if (alternative == "two.sided") {
    pval <- switch(likelihood,
      "poisson" = stats::poisson.test(ceiling(t.obs), n.obs, materiality, "two.sided")$p.value,
      "binomial" = stats::binom.test(ceiling(t.obs), n.obs, materiality, "two.sided")$p.value,
      "hypergeometric" = stats::fisher.test(matrix(c(x.obs, n.obs - x.obs, K - x.obs, N.units - n.obs - K + x.obs), nrow = 2), alternative = "two.sided")$p.value
    )
  } else if (alternative == "less") {
    pval <- switch(likelihood,
      "poisson" = stats::pgamma(materiality, 1 + t.obs, n.obs, lower.tail = FALSE),
      "binomial" = stats::pbeta(materiality, 1 + t.obs, n.obs - t.obs, lower.tail = FALSE),
      "hypergeometric" = stats::phyper(x.obs, K, N.units - K, n.obs)
    )
  } else {
    pval <- switch(likelihood,
      "poisson" = stats::pgamma(materiality, t.obs, 1 + n.obs),
      "binomial" = stats::pbeta(materiality, t.obs, 1 + n.obs - t.obs),
      "hypergeometric" = stats::phyper(x.obs - 1, K, N.units - K, n.obs, lower.tail = FALSE)
    )
  }
  return(pval)
}

.hyp_dens <- function(materiality, likelihood, alpha, beta, N.units, post_N, analytical = TRUE, samples = NULL) {
  if (analytical) {
    dens <- switch(likelihood,
      "poisson" = stats::dgamma(materiality, alpha, beta),
      "binomial" = stats::dbeta(materiality, alpha, beta),
      "hypergeometric" = extraDistr::dbbinom(ceiling(materiality * N.units), post_N, alpha, beta)
    )
  } else {
    densfit <- stats::density(samples, from = 0, to = 1)
    dens <- stats::approx(densfit[["x"]], densfit[["y"]], materiality)$y
  }
  return(dens)
}

.hyp_prob <- function(lower_tail, materiality, likelihood, alpha, beta, N.units, post_N, analytical = TRUE, samples = NULL) {
  if (analytical) {
    prob <- switch(likelihood,
      "poisson" = stats::pgamma(materiality, alpha, beta, lower.tail = lower_tail),
      "binomial" = stats::pbeta(materiality, alpha, beta, lower.tail = lower_tail),
      "hypergeometric" = extraDistr::pbbinom(ceiling(materiality * N.units) - 1, post_N, alpha, beta, lower.tail = lower_tail)
    )
  } else {
    if (lower_tail) {
      prob <- length(which(samples < materiality)) / length(samples)
    } else {
      prob <- length(which(samples > materiality)) / length(samples)
    }
  }
  return(prob)
}

.bf01_twosided_prior <- function(materiality, density, prior, analytical = TRUE, prior_samples = NULL) {
  if (analytical) {
    bf01 <- density / prior[["hypotheses"]]$density
  } else {
    densfit <- stats::density(prior_samples, from = 0, to = 1)
    bf01 <- density / stats::approx(densfit[["x"]], densfit[["y"]], materiality)$y
  }
  return(bf01)
}

.bf01_twosided_sumstats <- function(materiality, likelihood, prior.n, prior.x, n.obs, t.obs, N.units) {
  bf01 <- switch(likelihood,
    "poisson" = stats::dgamma(materiality, 1 + prior.x + t.obs, prior.n + n.obs) / stats::dgamma(materiality, 1 + prior.x, prior.n),
    "binomial" = stats::dbeta(materiality, 1 + prior.x + t.obs, prior.n - prior.x + n.obs - t.obs) / stats::dbeta(materiality, 1 + prior.x, prior.n - prior.x),
    "hypergeometric" = extraDistr::dbbinom(ceiling(materiality * N.units), N.units - n.obs, 1 + prior.x + t.obs, prior.n - prior.x + n.obs - t.obs) / extraDistr::dbbinom(ceiling(materiality * N.units), N.units, 1 + prior.x, prior.n - prior.x)
  )
  return(bf01)
}

.bf01_twosided_samples <- function(materiality, nstrata, stratum_samples) {
  bf01 <- numeric(nstrata - 1)
  for (i in 1:(nstrata - 1)) {
    prior_samples <- stratum_samples[, (nstrata - 1) + i]
    posterior_samples <- stratum_samples[, i]
    prior_densfit <- stats::density(prior_samples, from = 0, to = 1)
    post_densfit <- stats::density(posterior_samples, from = 0, to = 1)
    bf01[i] <- stats::approx(post_densfit[["x"]], post_densfit[["y"]], materiality)$y / stats::approx(prior_densfit[["x"]], prior_densfit[["y"]], materiality)$y
  }
  return(bf01)
}

.bf10_onesided_prior <- function(materiality, alternative, posterior_odds, prior, analytical = TRUE, prior_samples = NULL) {
  if (analytical) {
    prior_odds <- prior[["hypotheses"]]$odds.h1
  } else {
    if (alternative == "less") {
      prior_prob_h1 <- length(which(prior_samples < materiality)) / length(prior_samples)
      prior_prob_h0 <- length(which(prior_samples > materiality)) / length(prior_samples)
    } else {
      prior_prob_h1 <- length(which(prior_samples > materiality)) / length(prior_samples)
      prior_prob_h0 <- length(which(prior_samples < materiality)) / length(prior_samples)
    }
    prior_odds <- prior_prob_h1 / prior_prob_h0
  }
  bf10 <- posterior_odds / prior_odds
  return(bf10)
}

.bf10_onesided_sumstats <- function(materiality, alternative, likelihood, prior.n, prior.x, n.obs, t.obs, N.units) {
  prior_alpha <- 1 + prior.x
  post_alpha <- prior_alpha + t.obs
  if (likelihood == "poisson") {
    prior_beta <- prior.n
    post_beta <- prior.n + n.obs
  } else {
    prior_beta <- prior.n - prior.x
    post_beta <- prior_beta + n.obs - t.obs
  }
  bf10 <- switch(likelihood,
    "poisson" = (stats::pgamma(materiality, post_alpha, post_beta) / stats::pgamma(materiality, post_alpha, post_beta, lower.tail = FALSE)) / (stats::pgamma(materiality, prior_alpha, prior_beta) / stats::pgamma(materiality, prior_alpha, prior_beta, lower.tail = FALSE)),
    "binomial" = (stats::pbeta(materiality, post_alpha, post_beta) / stats::pbeta(materiality, post_alpha, post_beta, lower.tail = FALSE)) / (stats::pbeta(materiality, prior_alpha, prior_beta) / stats::pbeta(materiality, prior_alpha, prior_beta, lower.tail = FALSE)),
    "hypergeometric" = (extraDistr::pbbinom(ceiling(materiality * N.units) - 1, N.units - n.obs, post_alpha, post_beta) / extraDistr::pbbinom(ceiling(materiality * N.units) - 1, N.units - n.obs, post_alpha, post_beta, lower.tail = FALSE)) / (extraDistr::pbbinom(ceiling(materiality * N.units) - 1, N.units, prior_alpha, prior_beta) / extraDistr::pbbinom(ceiling(materiality * N.units) - 1, N.units, prior_alpha, prior_alpha, lower.tail = FALSE))
  )
  if (alternative == "greater") {
    bf10 <- 1 / bf10
  }
  return(bf10)
}

.bf10_onesided_samples <- function(materiality, alternative, nstrata, stratum_samples) {
  less <- function(x) length(which(x < materiality)) / length(x)
  greater <- function(x) length(which(x > materiality)) / length(x)
  prior_samples <- stratum_samples[, nstrata:ncol(stratum_samples)]
  post_samples <- stratum_samples[, 1:(nstrata - 1)]
  prior_odds_less <- apply(prior_samples, 2, less) / apply(prior_samples, 2, greater)
  post_odds_less <- apply(post_samples, 2, less) / apply(post_samples, 2, greater)
  bf10 <- post_odds_less / prior_odds_less
  if (alternative == "greater") {
    bf10 <- 1 / bf10
  }
  return(bf10)
}

.poststratification <- function(samples, N.units) {
  n_strata <- ncol(samples)
  if (is.null(N.units)) {
    weights <- rep(1, n_strata) / n_strata
  } else {
    weights <- N.units[-1] / N.units[1]
  }
  poststratified_samples <- samples %*% weights
  return(poststratified_samples)
}

.mcmc_stan <- function(method, prior.x, prior.n, n.obs, t.obs, t, nstrata, stratum, likelihood) {
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
  stopifnot("Stan model could not be fitted" = ncol(samples) == (nstrata - 1) * 2)
  return(samples)
}

.mcmc_analytical <- function(likelihood, nstrata, prior.x, t.obs, prior.n, n.obs, N.units, iterations) {
  samples <- matrix(NA, ncol = (nstrata - 1) * 2, nrow = iterations)
  stratum_indices_prior <- (nstrata + 1):((nstrata - 1) * 2 + 1)
  stratum_indices_post <- 2:nstrata
  for (i in stratum_indices_post) {
    samples[, i - 1] <- switch(likelihood,
      "poisson" = stats::rgamma(n = iterations, 1 + prior.x + t.obs[i], prior.n + n.obs[i]),
      "binomial" = stats::rbeta(n = iterations, 1 + prior.x + t.obs[i], prior.n - prior.x + n.obs[i] - t.obs[i]),
      "hypergeometric" = extraDistr::rbbinom(n = iterations, N.units[i] - n.obs[i], 1 + prior.x + t.obs[i], prior.n - prior.x + n.obs[i] - t.obs[i]) / N.units[i]
    )
  }
  for (i in stratum_indices_prior) {
    samples[, i - 1] <- switch(likelihood,
      "poisson" = stats::rgamma(n = iterations, 1 + prior.x, prior.n),
      "binomial" = stats::rbeta(n = iterations, 1 + prior.x, prior.n - prior.x),
      "hypergeometric" = extraDistr::rbbinom(n = iterations, N.units[i - nstrata], 1 + prior.x, prior.n - prior.x) / N.units[i - nstrata]
    )
  }
  return(samples)
}

.mcmc_emulate <- function(likelihood, alternative, nstrata, t.obs, n.obs, N.units, iterations) {
  samples <- matrix(NA, ncol = (nstrata - 1) * 2, nrow = iterations)
  stratum_indices_prior <- (nstrata + 1):((nstrata - 1) * 2 + 1)
  stratum_indices_post <- 2:nstrata
  if (alternative == "two.sided") {
    alpha <- c(rep(0, iterations / 2), rep(1, iterations / 2))
    beta <- 1 - alpha
  } else if (alternative == "less") {
    alpha <- 1
    beta <- 0
  } else {
    alpha <- 0
    beta <- 1
  }
  for (i in stratum_indices_post) {
    samples[, i - 1] <- switch(likelihood,
      "poisson" = stats::rgamma(n = iterations, alpha + t.obs[i], beta + n.obs[i]),
      "binomial" = stats::rbeta(n = iterations, alpha + t.obs[i], beta + n.obs[i] - t.obs[i]),
      "hypergeometric" = extraDistr::rbbinom(n = iterations, N.units[i] - n.obs[i], alpha + t.obs[i], beta + n.obs[i] - t.obs[i]) / N.units[i]
    )
  }
  for (i in stratum_indices_prior) {
    samples[, i - 1] <- switch(likelihood,
      "poisson" = stats::rgamma(n = iterations, alpha, beta),
      "binomial" = stats::rbeta(n = iterations, alpha, beta),
      "hypergeometric" = extraDistr::rbbinom(n = iterations, N.units[i - nstrata], alpha, beta) / N.units[i - nstrata]
    )
  }
  return(samples)
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
