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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Quantiles of the beta-binomial distribution
.qbbinom <- function(p, N, shape1, shape2, lower.tail = TRUE, log.p = FALSE) {
  if (shape1 == 0 || shape2 == 0) {
    return(Inf)
  }
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  x <- extraDistr::dbbinom(x = 0:N, size = N, alpha = shape1, beta = shape2)
  q <- which(cumsum(x) > p)[1] - 1
  return(q)
}

# This function finds the mode (= value with the highest probability) of the beta-binomial distribution
.modebbinom <- function(N, shape1, shape2) {
  if ((shape1 == 1 && shape2 == 1) || shape1 == 0 || shape2 == 0) {
    return(NA)
  }
  if (shape1 == 1 && shape2 > 1) {
    return(0)
  }
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
  if (length(grep("::", x)) > 0) {
    parts <- strsplit(x, "::")[[1]]
    return(getExportedValue(parts[1], parts[2]))
  } else {
    return(x)
  }
}

# This function extracts the requested digits
.extract_digits <- function(x, check = "first", include.zero = FALSE) {
  x <- x[!is.na(x)]
  x <- x[!is.infinite(x)]
  if (check == "first") {
    if (include.zero) {
      # Simply take the first character of the string of absolute x
      digits <- as.numeric(substring(abs(x), 1, 1))
    } else {
      # Take the first character of the scientific value of x
      digits <- as.numeric(substring(format(abs(x), scientific = TRUE), 1, 1))
      digits[x == 0] <- NA
    }
  } else if (check == "firsttwo") {
    if (include.zero) {
      x <- formatC(abs(x), digits = 10, format = "f")
      x <- gsub(x = x, pattern = "[.]", replacement = "")
      digits <- as.numeric(substring(x, 1, 2))
    } else {
      # Take the first and second characters of the scientific value of x * 10
      digits <- as.numeric(substring(format(abs(x), scientific = TRUE), 1, 3)) * 10
      digits[x == 0] <- NA
    }
  } else if (check == "before") {
    if (include.zero) {
      # Take the floored/ceiled numbers
      digits <- ifelse(x > 0, yes = floor(x), no = ceiling(x))
    } else {
      # Take the floored/ceiled numbers, but do not show the zeroes
      digits <- ifelse(x > 0, yes = floor(x), no = ceiling(x))
      digits[digits == 0] <- NA
    }
  } else if (check == "after") {
    if (include.zero) {
      # Subtract integer portion of number from the number itself
      stringedX <- format(abs(x) %% 1, drop0trailing = FALSE)
      digits <- ifelse(nchar(stringedX) == 1, yes = as.numeric(stringedX), no = as.numeric(substring(stringedX, 3, nchar(stringedX))))
    } else {
      # Subtract integer portion of number from the number itself
      stringedX <- format(abs(x) %% 1, drop0trailing = TRUE)
      digits <- as.numeric(substring(stringedX, 3, nchar(stringedX)))
      digits[x %% 2 == 0] <- NA
    }
  } else if (check == "lasttwo") {
    if (include.zero) {
      stringedX <- format(abs(x) %% 1, drop0trailing = FALSE)
      digits <- as.numeric(substring(stringedX, nchar(stringedX) - 1, nchar(stringedX)))
    } else {
      # Subtract integer portion of number from the number itself
      stringedX <- format(abs(x) %% 1, drop0trailing = FALSE)
      digits <- as.numeric(substring(stringedX, nchar(stringedX) - 1, nchar(stringedX)))
      digits[x %% 2 == 0] <- NA
    }
  } else if (check == "last") {
    if (include.zero) {
      x <- formatC(x, digits = 2, format = "f")
      digits <- as.numeric(substring(x, nchar(x), nchar(x)))
    } else {
      # Remove trailing zeroes from the number
      stringedX <- sub("0+$", "", as.character(abs(x)))
      digits <- as.numeric(substring(stringedX, nchar(stringedX), nchar(stringedX)))
      digits[x == 0] <- NA # Integers become NA
    }
  } else {
    stop("Specify a valid input for the check argument.")
  }
  return(digits)
}

# This function calculates the entropy in a variable
.entropy <- function(x) {
  frequencies <- as.numeric(table(x))
  # Probailities instead of frequencies
  prob <- frequencies / sum(frequencies)
  logProb <- log(prob)
  # Compute sum(p*log(p))
  entropy <- -sum(ifelse(!is.infinite(logProb), prob * logProb, 0))
  return(entropy)
}

# This function calculates the average frequency of numbers in a variable
.af <- function(x) {
  tx <- table(x)
  af <- sum(tx^2) / sum(tx)
  return(af)
}

# This function takes a vector x and returns the mode (the most occurring value) via density estimation
.getmode <- function(x, round = 7) {
  dens <- stats::density(x)
  mode <- round(dens$x[which.max(dens$y)], round)
  return(mode)
}

# This function fits a JAGS model using partial pooling and returns samples from the stratum posteriors
.fitjagsmodel <- function(method, prior.x, prior.n, n.obs, t.obs, t, nstrata, stratum, likelihood) {
  stopifnot("'method = hypergeometric' does not support pooling" = method != "hypergeometric")
  iterations <- switch(likelihood,
    "beta" = 1e4,
    "binomial" = 1e5
  )
  model <- switch(likelihood,
    "binomial" = switch(method,
      "binomial" = paste0("
    model {
      theta ~ dbeta(", 1 + prior.x, ", ", 1 + prior.n - prior.x, ")
      sigma ~ dnorm(0, 1)
      for (i in 1:S) {
        alpha_s[i] ~ dnorm(0, 1)
      }
      for (i in 1:S){
        theta_s[i] <- ilogit(logit(theta) + sigma * alpha_s[i])
        k[i] ~ dbinom(theta_s[i], n[i])
      }
    }"),
      "poisson" = paste0("
    model {
      theta ~ dgamma(", 1 + prior.x, ", ", prior.n, ")
      sigma ~ dnorm(0, 1)
      for (i in 1:S) {
        alpha_s[i] ~ dnorm(0, 1)
      }
      for (i in 1:S){
      theta_s[i] <- ilogit(logit(theta) + sigma * alpha_s[i])
      k[i] ~ dpois(theta_s[i] * n[i])
      }
    }")
    ),
    "beta" = switch(method,
      "binomial" = paste0("
    model {
      theta ~ dbeta(", 1 + prior.x, ", ", 1 + prior.n - prior.x, ")
      sigma ~ dnorm(0, 1)
      nu ~ dnorm(0, 10)I(0, )
      for (i in 1:S) {
        alpha_s[i] ~ dnorm(0, 1)
        theta_s[i] <- ilogit(logit(theta) + sigma * alpha_s[i])
      }
      for (i in 1:n){
        t[i] ~ dbeta(nu * theta_s[s[i]], nu * (1 - theta_s[s[i]]))
      }
    }"),
      "poisson" = paste0("
    model {
      theta ~ dgamma(", 1 + prior.x, ", ", prior.n, ")
      sigma ~ dnorm(0, 1)
      nu ~ dnorm(0, 100)I(0, )
      for (i in 1:S) {
        alpha_s[i] ~ dnorm(0, 1)
        theta_s[i] <- ilogit(logit(theta) + sigma * alpha_s[i])
      }
      for (i in 1:n){
        t[i] ~ dbeta(nu * theta_s[s[i]], nu * (1 - theta_s[s[i]]))
      }
    }")
    )
  )
  dataList <- switch(likelihood,
    "binomial" = list("n" = n.obs[-1], "k" = t.obs[-1], "S" = nstrata - 1),
    "beta" = list("t" = (t[[1]] * (n.obs[1] - 1) + 0.5) / n.obs[1], "n" = n.obs[1], "S" = nstrata - 1, "s" = as.numeric(stratum))
  )
  if (likelihood == "beta") {
    warning("taints are transformed by (t * (n - 1) + 0.5) / n")
  }
  p <- try(
    {
      utils::capture.output(
        file = "NUL",
        jagsmodel <- rjags::jags.model(file = textConnection(model), data = dataList, n.chains = 4, inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 120492), quiet = TRUE)
      )
      raw <- rjags::coda.samples(model = jagsmodel, variable.names = c("theta_s"), n.iter = iterations, burnin = iterations / 10)
    },
    silent = TRUE
  )
  if (inherits(p, "try-error")) {
    if (grepl("Failed to locate any version of JAGS", p[[1]], fixed = TRUE)) {
      stop("JAGS is missing but required, download it from http://www.sourceforge.net/projects/mcmc-jags/files")
    } else {
      stop(p[[1]])
    }
  }
  samples <- coda::mcmc(do.call(rbind, raw))
  return(samples)
}

# This function samples from an analytical posterior distribution
.posterior_samples <- function(method, nstrata, bayesian, prior.x, t.obs, prior.n, n.obs, N.units) {
  samples <- matrix(NA, ncol = nstrata - 1, nrow = 1e5)
  set.seed(120495)
  for (i in 2:nstrata) {
    if (bayesian) {
      samples[, i - 1] <- switch(method,
        "poisson" = stats::rgamma(1e5, 1 + prior.x + t.obs[i], prior.n + n.obs[i]),
        "binomial" = stats::rbeta(1e5, 1 + prior.x + t.obs[i], 1 + prior.n - prior.x + n.obs[i] - t.obs[i]),
        "hypergeometric" = extraDistr::rbbinom(1e5, N.units[i] - n.obs[i], 1 + prior.x + t.obs[i], 1 + prior.n - prior.x + n.obs[i] - t.obs[i]) / N.units[i]
      )
    } else {
      samples[, i - 1] <- switch(method,
        "poisson" = stats::rgamma(1e5, 1 + t.obs[i], n.obs[i]),
        "binomial" = stats::rbeta(1e5, 1 + t.obs[i], n.obs[i] - t.obs[i]),
        "hypergeometric" = extraDistr::rbbinom(1e5, N.units[i] - n.obs[i], 1 + t.obs[i], n.obs[i] - t.obs[i]) / N.units[i]
      )
    }
  }
  return(samples)
}

# This function implements poststratification
.poststratify_samples <- function(samples, N.units, nstrata) {
  if (is.null(N.units)) { # Create equal poststratification weights (1 / nstrata) in absence of N.units
    weights <- rep(1, nstrata - 1) / (nstrata - 1)
  } else {
    weights <- N.units[-1] / N.units[1]
  }
  psamples <- samples %*% weights # Weigh each stratum using the poststratification weights
}
