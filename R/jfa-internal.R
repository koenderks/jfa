# Copyright (C) 2020-2023 Koen Derks

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

.theme_jfa <- function(p, ...) {
  p <- p + ggplot2::theme(
    axis.text = ggplot2::element_text(size = 12),
    axis.ticks.length = ggplot2::unit(2, "mm"),
    axis.title = ggplot2::element_text(size = 15),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 12),
    panel.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.background = ggplot2::element_blank(), ...
  )
  return(p)
}

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
      "hypergeometric" = "beta-binomial",
      "normal" = "normal",
      "uniform" = "uniform",
      "cauchy" = "Cauchy",
      "t" = "Student-t",
      "chisq" = "chi-squared",
      "exponential" = "exponential"
    )
  } else {
    form <- "MCMC"
  }
  return(form)
}

.functional_form <- function(family, alpha, beta, N.units, analytical = TRUE) {
  if (analytical) {
    string <- switch(family,
      "poisson" = paste0("gamma(\u03B1 = ", round(alpha, 3), ", \u03B2 = ", round(beta, 3), ")"),
      "binomial" = paste0("beta(\u03B1 = ", round(alpha, 3), ", \u03B2 = ", round(beta, 3), ")"),
      "hypergeometric" = paste0("beta-binomial(N = ", N.units, ", \u03B1 = ", round(alpha, 3), ", \u03B2 = ", round(beta, 3), ")"),
      "normal" = paste0("normal(\u03BC = ", round(alpha, 3), ", \u03C3 = ", round(beta, 3), ")T[0,1]"),
      "uniform" = paste0("uniform(min = ", round(alpha, 3), ", max = ", round(beta, 3), ")"),
      "cauchy" = paste0("Cauchy(x\u2080 = ", round(alpha, 3), ", \u03B3 = ", round(beta, 3), ")T[0,1]"),
      "t" = paste0("Student-t(df = ", round(alpha, 3), ")T[0,1]"),
      "chisq" = paste0("chi-squared(df = ", round(alpha, 3), ")T[0,1]"),
      "exponential" = paste0("exponential(\u03BB = ", round(alpha, 3), ")T[0,1]")
    )
  } else {
    string <- "Nonparametric"
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

.comp_mode_bayes <- function(family, alpha, beta, K, N.units, analytical = TRUE, samples = NULL) {
  if (analytical) {
    mode <- switch(family,
      "poisson" = (alpha - 1) / beta,
      "binomial" = (alpha - 1) / (alpha + beta - 2),
      "hypergeometric" = .modebbinom(N.units, K, alpha, beta),
      "normal" = alpha,
      "uniform" = NA,
      "cauchy" = alpha,
      "t" = alpha,
      "chisq" = alpha,
      "exponential" = 0
    )
  } else {
    if (all(is.infinite(samples))) {
      mode <- Inf
    } else {
      mode <- as.numeric(names(which.max(table(round(samples, 4)))))
    }
  }
  return(mode)
}

.modebbinom <- function(N, K, shape1, shape2) {
  no_mode <- (shape1 == 1 && shape2 == 1) || shape1 == 0 || shape2 == 0
  if (no_mode) {
    return(NA)
  }
  if (shape1 == 1 && shape2 > 1) {
    return(0)
  }
  mode <- which.max(extraDistr::dbbinom(x = K, size = N, alpha = shape1, beta = shape2)) - 1
  return(mode)
}

.comp_mean_bayes <- function(family, alpha, beta, N.units, analytical = TRUE, samples = NULL) {
  if (analytical) {
    a <- (0 - alpha) / beta
    b <- (1 - alpha) / beta
    Z <- stats::pnorm(b) - stats::pnorm(a)
    mean <- switch(family,
      "poisson" = alpha / beta,
      "binomial" = alpha / (alpha + beta),
      "hypergeometric" = alpha / (alpha + beta) * N.units,
      "normal" = alpha + ((stats::dnorm(a) - stats::dnorm(b)) / Z) * beta,
      "uniform" = (alpha + beta) / 2,
      "cauchy" = NA,
      "t" = NA,
      "chisq" = NA,
      "exponential" = NA
    )
  } else {
    mean <- mean(samples)
  }
  return(mean)
}

.comp_median_bayes <- function(family, alpha, beta, K, N.units, analytical = TRUE, samples = NULL) {
  if (analytical) {
    median <- switch(family,
      "poisson" = stats::qgamma(0.5, alpha, beta),
      "binomial" = stats::qbeta(0.5, alpha, beta),
      "hypergeometric" = .qbbinom(0.5, K, N.units, alpha, beta),
      "normal" = truncdist::qtrunc(0.5, spec = "norm", a = 0, b = 1, mean = alpha, sd = beta),
      "uniform" = truncdist::qtrunc(0.5, spec = "unif", a = 0, b = 1, min = alpha, max = beta),
      "cauchy" = truncdist::qtrunc(0.5, spec = "cauchy", a = 0, b = 1, location = alpha, scale = beta),
      "t" = truncdist::qtrunc(0.5, spec = "t", a = 0, b = 1, df = alpha),
      "chisq" = truncdist::qtrunc(0.5, spec = "chisq", a = 0, b = 1, df = alpha),
      "exponential" = truncdist::qtrunc(0.5, spec = "exp", a = 0, b = 1, rate = alpha)
    )
  } else {
    median <- stats::median(samples)
  }
  return(median)
}

.comp_var_bayes <- function(family, alpha, beta, N.units, analytical = TRUE, samples = NULL) {
  if (analytical) {
    a <- (0 - alpha) / beta
    b <- (1 - alpha) / beta
    Z <- stats::pnorm(b) - stats::pnorm(a)
    variance <- switch(family,
      "poisson" = alpha / beta^2,
      "binomial" = (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1)),
      "hypergeometric" = ((N.units * alpha * beta) * (alpha + beta + N.units)) / ((alpha + beta)^2 * (alpha + beta + 1)),
      "normal" = beta^2 * (1 + ((a * stats::dnorm(a) - b * stats::dnorm(b)) / Z) - ((stats::dnorm(a) - stats::dnorm(b)) / Z)^2),
      "uniform" = (1 / 12) * (beta - alpha)^2,
      "cauchy" = NA,
      "t" = NA,
      "chisq" = NA,
      "exponential" = NA
    )
  } else {
    variance <- as.numeric(stats::var(samples))
  }
  return(variance)
}

.comp_skew_bayes <- function(family, alpha, beta, N.units, analytical = TRUE, samples = NULL) {
  if (analytical) {
    skewness <- switch(family,
      "poisson" = 2 / sqrt(alpha),
      "binomial" = ((2 * (beta - alpha)) * sqrt(alpha + beta + 1)) / ((alpha + beta + 2) * sqrt(alpha * beta)),
      "hypergeometric" = (((alpha + beta + 2 * N.units) * (beta - alpha)) / (alpha + beta + 2)) * sqrt((1 + alpha + beta) / (N.units * alpha * beta * (N.units + alpha + beta))),
      "normal" = NA,
      "uniform" = 0,
      "cauchy" = NA,
      "t" = NA,
      "chisq" = NA,
      "exponential" = NA
    )
  } else {
    # See Joanes, D. N.; Gill, C. A. (1998). Comparing measures of sample skewness and kurtosis. Journal of the Royal Statistical Society, Series D. 47(1): 183–189.
    # The formula below is taken from the 'skewness()' function in the 'moments' package.
    skewness <- (sum((samples - mean(samples))^3) / length(samples)) / (sum((samples - mean(samples))^2) / length(samples))^(3 / 2)
  }
  return(skewness)
}

.comp_entropy_bayes <- function(family, alpha, beta, analytical = TRUE, samples = NULL) {
  if (analytical) {
    if (family %in% c("poisson", "binomial", "hypergeometric") && (alpha == 0 || beta == 0)) {
      entropy <- -Inf
    } else {
      a <- (0 - alpha) / beta
      b <- (1 - alpha) / beta
      Z <- stats::pnorm(b) - stats::pnorm(a)
      entropy <- switch(family,
        "poisson" = alpha - log(beta) + log(gamma(alpha)) + (1 - alpha) * digamma(alpha),
        "binomial" = log(beta(alpha, beta)) - (alpha - 1) * digamma(alpha) - (beta - 1) * digamma(beta) + (alpha + beta - 2) * digamma(alpha + beta),
        "hypergeometric" = log(beta(alpha, beta)) - (alpha - 1) * digamma(alpha) - (beta - 1) * digamma(beta) + (alpha + beta - 2) * digamma(alpha + beta),
        "normal" = log(sqrt(2 * pi * exp(1)) * beta * Z) + (a * stats::dnorm(a) - b * stats::dnorm(b)) / (2 * Z),
        "uniform" = log(beta - alpha),
        "cauchy" = NA,
        "t" = NA,
        "chisq" = NA,
        "exponential" = NA
      )
    }
  } else {
    entropy <- .entropy(round(samples, 3))
  }
  return(entropy)
}

.comp_ub_freq <- function(alternative, conf.level, family, n.obs, x.obs, t.obs, N.units, analytical = TRUE, samples = NULL) {
  if (alternative == "greater") {
    ub <- 1
  } else {
    if (alternative == "less") {
      prob <- conf.level
    } else {
      prob <- conf.level + (1 - conf.level) / 2
    }
    if (analytical) {
      ub <- switch(family,
        "poisson" = stats::qgamma(prob, 1 + t.obs, n.obs),
        "binomial" = stats::qbeta(prob, 1 + t.obs, n.obs - t.obs),
        "hypergeometric" = .qhyper(prob, N.units, n.obs, x.obs)
      )
    } else {
      ub <- as.numeric(stats::quantile(samples[1:(length(samples) / 2)], prob))
    }
  }
  return(ub)
}

.comp_lb_freq <- function(alternative, conf.level, family, n.obs, x.obs, t.obs, N.units, analytical = TRUE, samples = NULL) {
  if (alternative == "less") {
    lb <- 0
  } else {
    if (alternative == "greater") {
      prob <- 1 - conf.level
    } else {
      prob <- (1 - conf.level) / 2
    }
    if (analytical) {
      lb <- switch(family,
        "poisson" = stats::qgamma(prob, t.obs, 1 + n.obs),
        "binomial" = stats::qbeta(prob, t.obs, 1 + n.obs - t.obs),
        "hypergeometric" = .qhyper(prob, N.units, n.obs, x.obs)
      )
    } else {
      lb <- as.numeric(stats::quantile(samples[(length(samples) / 2 + 1):length(samples)], prob))
    }
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

.comp_ub_bayes <- function(alternative, conf.level, family, alpha, beta, K, N.units, analytical = TRUE, samples = NULL) {
  if (alternative == "greater") {
    ub <- 1
  } else {
    if (alternative == "less") {
      prob <- conf.level
    } else {
      prob <- conf.level + (1 - conf.level) / 2
    }
    if (analytical) {
      ub <- switch(family,
        "poisson" = stats::qgamma(prob, alpha, beta),
        "binomial" = stats::qbeta(prob, alpha, beta),
        "hypergeometric" = .qbbinom(prob, K, N.units, alpha, beta),
        "normal" = truncdist::qtrunc(prob, spec = "norm", a = 0, b = 1, mean = alpha, sd = beta),
        "uniform" = truncdist::qtrunc(prob, spec = "unif", a = 0, b = 1, min = alpha, max = beta),
        "cauchy" = truncdist::qtrunc(prob, spec = "cauchy", a = 0, b = 1, location = alpha, scale = beta),
        "t" = truncdist::qtrunc(prob, spec = "t", a = 0, b = 1, df = alpha),
        "chisq" = truncdist::qtrunc(prob, spec = "chisq", a = 0, b = 1, df = alpha),
        "exponential" = truncdist::qtrunc(prob, spec = "exp", a = 0, b = 1, rate = alpha)
      )
    } else {
      ub <- as.numeric(stats::quantile(samples, prob))
    }
  }
  return(ub)
}

.comp_lb_bayes <- function(alternative, conf.level, family, alpha, beta, K, N.units, analytical = TRUE, samples = NULL) {
  if (alternative == "less") {
    lb <- 0
  } else {
    if (alternative == "greater") {
      prob <- 1 - conf.level
    } else {
      prob <- (1 - conf.level) / 2
    }
    if (analytical) {
      lb <- switch(family,
        "poisson" = stats::qgamma(prob, alpha, beta),
        "binomial" = stats::qbeta(prob, alpha, beta),
        "hypergeometric" = .qbbinom(prob, K, N.units, alpha, beta),
        "normal" = truncdist::qtrunc(prob, spec = "norm", a = 0, b = 1, mean = alpha, sd = beta),
        "uniform" = truncdist::qtrunc(prob, spec = "unif", a = 0, b = 1, min = alpha, max = beta),
        "cauchy" = truncdist::qtrunc(prob, spec = "cauchy", a = 0, b = 1, location = alpha, scale = beta),
        "t" = truncdist::qtrunc(prob, spec = "t", a = 0, b = 1, df = alpha),
        "chisq" = truncdist::qtrunc(prob, spec = "chisq", a = 0, b = 1, df = alpha),
        "exponential" = truncdist::qtrunc(prob, spec = "exp", a = 0, b = 1, rate = alpha)
      )
    } else {
      lb <- as.numeric(stats::quantile(samples, prob))
    }
  }
  return(lb)
}

.qbbinom <- function(p, K, N, shape1, shape2, lower.tail = TRUE, log.p = FALSE) {
  improper <- shape1 == 0 || shape2 == 0
  if (improper) {
    return(Inf)
  }
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  x <- extraDistr::dbbinom(x = K, size = N, alpha = shape1, beta = shape2)
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

.hyp_dens <- function(materiality, family, alpha, beta, N.units, post_N, analytical = TRUE, samples = NULL) {
  if (analytical) {
    dens <- switch(family,
      "poisson" = stats::dgamma(materiality, alpha, beta),
      "binomial" = stats::dbeta(materiality, alpha, beta),
      "hypergeometric" = extraDistr::dbbinom(ceiling(materiality * N.units), post_N, alpha, beta),
      "normal" = truncdist::dtrunc(materiality, spec = "norm", a = 0, b = 1, mean = alpha, sd = beta),
      "uniform" = truncdist::dtrunc(materiality, spec = "unif", a = 0, b = 1, min = alpha, max = beta),
      "cauchy" = truncdist::dtrunc(materiality, spec = "cauchy", a = 0, b = 1, location = alpha, scale = beta),
      "t" = truncdist::dtrunc(materiality, spec = "t", a = 0, b = 1, df = alpha),
      "chisq" = truncdist::dtrunc(materiality, spec = "chisq", a = 0, b = 1, df = alpha),
      "exponential" = truncdist::dtrunc(materiality, spec = "exp", a = 0, b = 1, rate = alpha)
    )
  } else {
    if (all(is.infinite(samples))) {
      dens <- 0
    } else {
      densfit <- .bounded_density(as.numeric(samples))
      dens <- stats::approx(densfit[["x"]], densfit[["y"]], materiality)$y
    }
  }
  return(dens)
}

.hyp_prob <- function(lower_tail, materiality, family, alpha, beta, x, N.units, post_N, analytical = TRUE, samples = NULL) {
  if (analytical) {
    prob <- switch(family,
      "poisson" = stats::pgamma(materiality, alpha, beta, lower.tail = lower_tail),
      "binomial" = stats::pbeta(materiality, alpha, beta, lower.tail = lower_tail),
      "hypergeometric" = extraDistr::pbbinom((ceiling(materiality * N.units) - 1) - x, post_N, alpha, beta, lower.tail = lower_tail),
      "normal" = truncdist::ptrunc(materiality, spec = "norm", a = 0, b = 1, mean = alpha, sd = beta),
      "uniform" = truncdist::ptrunc(materiality, spec = "unif", a = 0, b = 1, min = alpha, max = beta),
      "cauchy" = truncdist::ptrunc(materiality, spec = "cauchy", a = 0, b = 1, location = alpha, scale = beta),
      "t" = truncdist::ptrunc(materiality, spec = "t", a = 0, b = 1, df = alpha),
      "chisq" = truncdist::ptrunc(materiality, spec = "chisq", a = 0, b = 1, df = alpha),
      "exponential" = truncdist::ptrunc(materiality, spec = "exp", a = 0, b = 1, rate = alpha)
    )
    if (family %in% c("normal", "uniform", "cauchy", "t", "chisq", "exponential") && !lower_tail) {
      prob <- 1 - prob
    }
  } else {
    if (lower_tail) {
      prob <- length(which(samples < materiality)) / length(samples)
    } else {
      prob <- length(which(samples > materiality)) / length(samples)
    }
  }
  return(prob)
}

.bf01_twosided_sumstats <- function(materiality, family, alpha, beta, n.obs, t.obs, N.units) {
  bf01 <- switch(family,
    "poisson" = stats::dgamma(materiality, alpha + t.obs, beta + n.obs) / stats::dgamma(materiality, alpha, beta),
    "binomial" = stats::dbeta(materiality, alpha + t.obs, beta + n.obs - t.obs) / stats::dbeta(materiality, alpha, beta),
    "hypergeometric" = extraDistr::dbbinom(ceiling(materiality * N.units), N.units - n.obs, alpha + t.obs, beta + n.obs - t.obs) / extraDistr::dbbinom(ceiling(materiality * N.units), N.units, alpha, beta)
  )
  return(bf01)
}

.bf01_twosided_samples <- function(materiality, nstrata, stratum_samples) {
  bf01 <- numeric(nstrata - 1)
  for (i in 1:(nstrata - 1)) {
    prior_samples <- stratum_samples[, (nstrata - 1) + i]
    posterior_samples <- stratum_samples[, i]
    prior_densfit <- .bounded_density(as.numeric(prior_samples))
    post_densfit <- .bounded_density(as.numeric(posterior_samples))
    bf01[i] <- stats::approx(post_densfit[["x"]], post_densfit[["y"]], materiality)$y / stats::approx(prior_densfit[["x"]], prior_densfit[["y"]], materiality)$y
  }
  return(bf01)
}

.bf10_onesided_sumstats <- function(materiality, alternative, family, alpha, beta, n.obs, t.obs, N.units) {
  bf10 <- switch(family,
    "poisson" = (stats::pgamma(materiality, alpha + t.obs, beta + n.obs) / stats::pgamma(materiality, alpha + t.obs, beta + n.obs, lower.tail = FALSE)) / (stats::pgamma(materiality, alpha, beta) / stats::pgamma(materiality, alpha, beta, lower.tail = FALSE)),
    "binomial" = (stats::pbeta(materiality, alpha + t.obs, beta + n.obs - t.obs) / stats::pbeta(materiality, alpha + t.obs, beta + n.obs - t.obs, lower.tail = FALSE)) / (stats::pbeta(materiality, alpha, beta) / stats::pbeta(materiality, alpha, beta, lower.tail = FALSE)),
    "hypergeometric" = (extraDistr::pbbinom(ceiling(materiality * N.units) - 1, N.units - n.obs, alpha + t.obs, beta + n.obs - t.obs) / extraDistr::pbbinom(ceiling(materiality * N.units) - 1, N.units - n.obs, alpha + t.obs, beta + n.obs - t.obs, lower.tail = FALSE)) / (extraDistr::pbbinom(ceiling(materiality * N.units) - 1, N.units, alpha, beta) / extraDistr::pbbinom(ceiling(materiality * N.units) - 1, N.units, alpha, beta, lower.tail = FALSE))
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
    weights <- N.units / sum(N.units)
  }
  poststratified_samples <- samples %*% weights
  return(poststratified_samples)
}

.mcmc_cp <- function(likelihood, x, n, prior) {
  theta <- seq(0, 1, length.out = 1001)
  if (prior[["description"]]$density == "gamma") {
    prior <- stats::dgamma(theta, prior[["description"]]$alpha, prior[["description"]]$beta)
  } else if (prior[["description"]]$density == "beta") {
    prior <- stats::dbeta(theta, prior[["description"]]$alpha, prior[["description"]]$beta)
  } else if (prior[["description"]]$density == "normal") {
    prior <- truncdist::dtrunc(theta, spec = "norm", a = 0, b = 1, mean = prior[["description"]]$alpha, sd = prior[["description"]]$beta)
  } else if (prior[["description"]]$density == "uniform") {
    prior <- truncdist::dtrunc(theta, spec = "unif", a = 0, b = 1, min = prior[["description"]]$alpha, max = prior[["description"]]$beta)
  } else if (prior[["description"]]$density == "Cauchy") {
    prior <- truncdist::dtrunc(theta, spec = "cauchy", a = 0, b = 1, location = prior[["description"]]$alpha, scale = prior[["description"]]$beta)
  } else if (prior[["description"]]$density == "Student-t") {
    prior <- truncdist::dtrunc(theta, spec = "t", a = 0, b = 1, df = prior[["description"]]$alpha)
  } else if (prior[["description"]]$density == "chi-squared") {
    prior <- truncdist::dtrunc(theta, spec = "chisq", a = 0, b = 1, df = prior[["description"]]$alpha)
  } else if (prior[["description"]]$density == "exponential") {
    prior <- truncdist::dtrunc(theta, spec = "exp", a = 0, b = 1, rate = prior[["description"]]$alpha)
  } else if (prior[["description"]]$density == "MCMC") {
    prior <- prior[["fitted.density"]]$y
  }
  likelihood <- switch(likelihood,
    "binomial" = stats::dbeta(theta, shape1 = 1 + x, shape2 = 1 + n - x),
    "poisson" = stats::dgamma(theta, shape = 1 + x, rate = n)
  )
  posterior <- prior * likelihood
  prior_indices <- !is.na(prior) & !is.infinite(prior)
  samples_prior <- sample(theta[prior_indices], size = getOption("jfa.iterations", 1e5), replace = TRUE, prob = prior[prior_indices])
  posterior_indices <- !is.na(posterior) & !is.infinite(posterior)
  samples_post <- sample(theta[posterior_indices], size = getOption("jfa.iterations", 1e5), replace = TRUE, prob = posterior[posterior_indices])
  samples <- cbind(samples_post, samples_prior)
  return(samples)
}

.mcmc_pp <- function(likelihood, n.obs, t.obs, t, nstrata, stratum, prior) {
  stopifnot("'method = hypergeometric' does not support pooling" = prior[["likelihood"]] != "hypergeometric")
  if (likelihood == "binomial" || likelihood == "poisson") {
    data <- list(
      S = nstrata - 1,
      n = n.obs[-1],
      k = t.obs[-1],
      alpha = prior[["description"]]$alpha,
      beta = prior[["description"]]$beta,
      beta_prior = as.numeric(prior[["likelihood"]] == "binomial"),
      gamma_prior = as.numeric(prior[["likelihood"]] == "poisson"),
      normal_prior = as.numeric(prior[["likelihood"]] == "normal"),
      uniform_prior = as.numeric(prior[["likelihood"]] == "uniform"),
      cauchy_prior = as.numeric(prior[["likelihood"]] == "cauchy"),
      t_prior = as.numeric(prior[["likelihood"]] == "t"),
      chisq_prior = as.numeric(prior[["likelihood"]] == "chisq"),
      binomial_likelihood = as.numeric(likelihood == "binomial"),
      poisson_likelihood = as.numeric(likelihood == "poisson"),
      exponential_prior = as.numeric(likelihood == "exponential")
    )
    model <- stanmodels[["pp_error"]]
  } else if (likelihood == "beta") {
    data <- list(
      S = nstrata - 1,
      t = (t[[1]] * (n.obs[1] - 1) + 0.5) / n.obs[1],
      n = n.obs[1], s = as.numeric(stratum),
      alpha = prior[["description"]]$alpha,
      beta = prior[["description"]]$beta,
      beta_prior = as.numeric(prior[["likelihood"]] == "binomial"),
      gamma_prior = as.numeric(prior[["likelihood"]] == "poisson"),
      normal_prior = as.numeric(prior[["likelihood"]] == "normal"),
      uniform_prior = as.numeric(prior[["likelihood"]] == "uniform"),
      cauchy_prior = as.numeric(prior[["likelihood"]] == "cauchy"),
      t_prior = as.numeric(prior[["likelihood"]] == "t"),
      chisq_prior = as.numeric(prior[["likelihood"]] == "chisq"),
      exponential_prior = as.numeric(likelihood == "exponential")
    )
    model <- stanmodels[["pp_taint"]]
  }
  suppressWarnings({
    raw_prior <- rstan::sampling(
      object = model,
      data = c(data, use_likelihood = 0),
      pars = "theta_s",
      iter = getOption("mc.iterations", 2000),
      warmup = getOption("mc.warmup", 1000),
      chains = getOption("mc.chains", 4),
      cores = getOption("mc.cores", 1),
      seed = sample.int(.Machine$integer.max, 1),
      control = list(adapt_delta = 0.95),
      refresh = 0
    )
    raw_posterior <- rstan::sampling(
      object = model,
      data = c(data, use_likelihood = 1),
      pars = "theta_s",
      iter = getOption("mc.iterations", 2000),
      warmup = getOption("mc.warmup", 1000),
      chains = getOption("mc.chains", 4),
      cores = getOption("mc.cores", 1),
      seed = sample.int(.Machine$integer.max, 1),
      control = list(adapt_delta = 0.95),
      refresh = 0
    )
  })
  samples <- cbind(rstan::extract(raw_posterior)$theta_s, rstan::extract(raw_prior)$theta_s)
  stopifnot("Stan model could not be fitted...check your priors" = !is.null(samples) && ncol(samples) == (nstrata - 1) * 2)
  return(samples)
}

.mcmc_analytical <- function(nstrata, t.obs, n.obs, N.units, prior) {
  iterations <- getOption("jfa.iterations", 1e5)
  samples <- matrix(NA, ncol = nstrata * 2, nrow = iterations)
  for (i in 1:nstrata) {
    samples[, i] <- switch(prior[["likelihood"]],
      "poisson" = stats::rgamma(n = iterations, prior[["description"]]$alpha + t.obs[i], prior[["description"]]$beta + n.obs[i]),
      "binomial" = stats::rbeta(n = iterations, prior[["description"]]$alpha + t.obs[i], prior[["description"]]$beta + n.obs[i] - t.obs[i]),
      "hypergeometric" = extraDistr::rbbinom(n = iterations, N.units[i] - n.obs[i], prior[["description"]]$alpha + t.obs[i], prior[["description"]]$beta + n.obs[i] - t.obs[i]) / N.units[i]
    )
  }
  for (i in (nstrata + 1):(nstrata * 2)) {
    samples[, i] <- switch(prior[["likelihood"]],
      "poisson" = stats::rgamma(n = iterations, prior[["description"]]$alpha, prior[["description"]]$beta),
      "binomial" = stats::rbeta(n = iterations, prior[["description"]]$alpha, prior[["description"]]$beta),
      "hypergeometric" = extraDistr::rbbinom(n = iterations, N.units[i - nstrata], prior[["description"]]$alpha, prior[["description"]]$beta) / N.units[i - nstrata]
    )
  }
  return(samples)
}

.mcmc_emulate <- function(likelihood, alternative, nstrata, t.obs, n.obs, N.units) {
  iterations <- getOption("jfa.iterations", 1e5)
  if (alternative == "two.sided") {
    alpha <- rep(1:0, each = iterations)
    beta <- 1 - alpha
    iterations <- iterations * 2
  } else if (alternative == "less") {
    alpha <- 1
    beta <- 0
  } else {
    alpha <- 0
    beta <- 1
  }
  samples <- matrix(NA, ncol = nstrata * 2, nrow = iterations)
  for (i in 1:nstrata) {
    samples[, i] <- switch(likelihood,
      "poisson" = stats::rgamma(n = iterations, alpha + t.obs[i], beta + n.obs[i]),
      "binomial" = stats::rbeta(n = iterations, alpha + t.obs[i], beta + n.obs[i] - t.obs[i]),
      "hypergeometric" = extraDistr::rbbinom(n = iterations, N.units[i] - n.obs[i], alpha + t.obs[i], beta + n.obs[i] - t.obs[i]) / N.units[i]
    )
  }
  for (i in (nstrata + 1):(nstrata * 2)) {
    samples[, i] <- switch(likelihood,
      "poisson" = stats::rgamma(n = iterations, alpha, beta),
      "binomial" = stats::rbeta(n = iterations, alpha, beta),
      "hypergeometric" = extraDistr::rbbinom(n = iterations, N.units[i - nstrata], alpha, beta) / N.units[i - nstrata]
    )
  }
  return(samples)
}

.bounded_density <- function(samples) {
  theta <- seq(0, 1, length = 1001)
  density <- bde::bde(
    dataPoints = as.numeric(ifelse(is.infinite(samples), 1, samples)),
    estimator = "boundarykernel",
    lower.limit = 0, upper.limit = 1,
    dataPointsCache = theta, b = 0.01
  )
  fit <- data.frame(x = theta, y = bde::getdensityCache(density))
  fit[["y"]] <- ifelse(fit[["y"]] < 0, 0, fit[["y"]])
  return(fit)
}

.rsample <- function(density, n) {
  samples <- sample(density[["x"]], size = n, replace = TRUE, prob = density[["y"]])
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

.contingencyTableBf <- function(y, prior_a) {
  C <- ncol(y)
  R <- nrow(y)
  ystardot <- rowSums(y)
  ydotstar <- colSums(y)
  alphastarstar <- matrix(prior_a, nrow = 2, ncol = 2)
  alphastardot <- rowSums(alphastarstar)
  alphadotstar <- colSums(alphastarstar)
  xistardot <- alphastardot - (C - 1)
  xidotstar <- alphadotstar - (R - 1)
  ldirichlet <- function(a) {
    sum(lgamma(a)) - lgamma(sum(a))
  }
  part1 <- ldirichlet(ystardot + xistardot) - ldirichlet(xistardot)
  part2 <- ldirichlet(ydotstar + xidotstar) - ldirichlet(xidotstar)
  part3 <- ldirichlet(alphastarstar) - ldirichlet(y + alphastarstar)
  logBF01 <- part1 + part2 + part3
  BF01 <- exp(logBF01)
  BF10 <- 1 / BF01
  return(BF10)
}

.mcmc_or <- function(counts, prior_a) {
  suppressWarnings({
    raw_prior <- rstan::sampling(
      object = stanmodels[["or_fairness"]],
      data = list(y = counts, prior_a = prior_a, use_likelihood = 0),
      pars = "OR",
      iter = getOption("mc.iterations", 2000),
      warmup = getOption("mc.warmup", 1000),
      chains = getOption("mc.chains", 4),
      cores = getOption("mc.cores", 1),
      seed = sample.int(.Machine$integer.max, 1),
      control = list(adapt_delta = 0.95),
      refresh = 0
    )
    raw_posterior <- rstan::sampling(
      object = stanmodels[["or_fairness"]],
      data = list(y = counts, prior_a = prior_a, use_likelihood = 1),
      pars = c("OR", "prob"),
      iter = getOption("mc.iterations", 2000),
      warmup = getOption("mc.warmup", 1000),
      chains = getOption("mc.chains", 4),
      cores = getOption("mc.cores", 1),
      seed = sample.int(.Machine$integer.max, 1),
      control = list(adapt_delta = 0.95),
      refresh = 0
    )
  })
  samples <- data.frame(
    prior = rstan::extract(raw_prior)$OR,
    OR = rstan::extract(raw_posterior)$OR,
    prob = rstan::extract(raw_posterior)$prob
  )
  stopifnot("Stan model could not be fitted...check your priors" = !is.null(samples))
  return(samples)
}
