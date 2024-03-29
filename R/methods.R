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

.stringer <- function(taints, conf.level, n, likelihood = "binomial", correction = "none", N.units = NULL) {
  mle <- sum(taints) / n
  t <- ifelse(taints < 0, yes = 0, no = taints)
  t <- ifelse(taints > 1, yes = 1, no = taints)
  t <- sort(subset(t, t > 0), decreasing = TRUE)
  if (likelihood == "hypergeometric") {
    stopifnot("missing value for 'N.units'" = !is.null(N.units))
  }
  bound <- switch(likelihood,
    "binomial" = 1 - (1 - conf.level)^(1 / n),
    "poisson" = stats::qgamma(p = conf.level, shape = 1, rate = n),
    "hypergeometric" = .qhyper(p = conf.level, N = N.units, n = n, k = 0) / N.units
  )
  if (length(t) > 0) {
    propSum <- 0
    for (i in seq_along(t)) {
      propSum <- switch(likelihood,
        "binomial" = propSum + (stats::qbeta(p = conf.level, shape1 = i + 1, shape2 = n - i) - stats::qbeta(p = conf.level, shape1 = (i - 1) + 1, shape2 = n - (i - 1))) * t[i],
        "poisson" = propSum + (stats::qgamma(p = conf.level, shape = i + 1, rate = n) - stats::qgamma(p = conf.level, shape = (i - 1) + 1, rate = n)) * t[i],
        "hypergeometric" = propSum + (.qhyper(p = conf.level, N = N.units, k = i, n = n) - .qhyper(p = conf.level, N = N.units, k = i - 1, n = n)) / N.units * t[i]
      )
    }
    bound <- bound + propSum
  }
  if (correction == "meikle") {
    tmin <- sort(subset(taints, taints < 0), decreasing = FALSE)
    if (length(tmin) > 0) {
      prop.sum.min <- stats::qbeta(1 + 1, n - 1, p = conf.level) * abs(tmin[1])
      if (length(tmin) > 2) {
        prop.sum.min.2 <- 0
        for (i in 2:length(tmin)) {
          prop.sum.min.2 <- switch(likelihood,
            "binomial" = prop.sum.min.2 + (stats::qbeta(shape1 = i + 1, shape2 = n - 1, p = conf.level) - stats::qbeta(shape1 = (i - 1) + 1, shape2 = n - 1, p = conf.level)) * abs(tmin[i]),
            "poisson" = prop.sum.min.2 + (stats::qgamma(shape = i + 1, rate = n, p = conf.level) - stats::qgamma(shape = (i - 1) + 1, rate = n, p = conf.level)) * abs(tmin[i]),
            "hypergeometric" = prop.sum.min.2 + (.qhyper(p = conf.level, N = N.units, k = i, n = n) - .qhyper(p = conf.level, N = N.units, k = i - 1, n = n)) / N.units * abs(tmin[i])
          )
        }
        prop.sum.min <- prop.sum.min + prop.sum.min.2
      }
      bound <- bound - prop.sum.min
    }
  } else if (correction == "lta") {
    tmin <- subset(taints, taints < 0)
    if (length(tmin) > 0) {
      ltaCorrection <- (1 / n) * sum(abs(tmin))
      bound <- bound - ltaCorrection
    }
  } else if (correction == "pvz") {
    taints <- ifelse(taints < 0, 0, taints)
    tmin <- sort(subset(taints, taints > 0))
    if (length(tmin) > 0) {
      constant <- 0
      for (i in seq_along(tmin)) {
        constant <- constant + (((n - 2 * i + 1) / (2 * sqrt(i * (n - i + 1)))) * rev(tmin)[i])
      }
      constant <- (1 / n) * constant
      sigma <- (1 / n) * sum((tmin - mean(tmin))^2)
      pvzCorrection <- (constant - sqrt(sigma)) / sqrt(n) * stats::qnorm(p = conf.level, lower.tail = TRUE)
      bound <- bound - pvzCorrection
    }
  }
  result <- list()
  result[["ub"]] <- bound
  result[["lb"]] <- 0
  result[["mle"]] <- mle
  result[["precision"]] <- result[["ub"]] - result[["mle"]]
  return(result)
}

.rohrbach <- function(taints, conf.level, n, alternative, N.units = NULL, r.delta) {
  stopifnot("missing value for 'N.units'" = !is.null(N.units))
  w <- 1 - taints
  mu <- mean(taints)
  vars <- sum(w^2) / n - (2 - (r.delta / n)) * ((1 / 2) * ((sum(w^2) / n) - stats::var(w)))
  result <- list()
  result[["mle"]] <- sum(taints) / n
  result[["ub"]] <- switch(alternative,
    "two.sided" = mu + stats::qnorm(p = conf.level + ((1 - conf.level) / 2)) * sqrt((1 - (n / N.units)) * (vars / n)),
    "less" = mu + stats::qnorm(p = conf.level) * sqrt((1 - (n / N.units)) * (vars / n)),
    "greater" = 1
  )
  result[["lb"]] <- switch(alternative,
    "two.sided" = mu + stats::qnorm(p = ((1 - conf.level) / 2)) * sqrt((1 - (n / N.units)) * (vars / n)),
    "less" = 0,
    "greater" = mu + stats::qnorm(p = 1 - conf.level) * sqrt((1 - (n / N.units)) * (vars / n))
  )
  result[["precision"]] <- if (alternative == "greater") result[["mle"]] - result[["lb"]] else result[["ub"]] - result[["mle"]]
  return(result)
}

.moment <- function(taints, conf.level, n, alternative, m.type = c("inventory", "accounts")) {
  m.type <- match.arg(m.type)
  tall <- subset(taints, taints != 0)
  if (m.type == "inventory" && length(tall) > 0) {
    tstar <- 0.81 * (1 - 0.667 * tanh(10 * abs(mean(tall))))
  } else if (m.type == "inventory" && length(tall) == 0) {
    tstar <- 0.81 * (1 - 0.667 * tanh(10 * 0))
  }
  if (m.type == "accounts" && length(tall) > 0) {
    tstar <- 0.81 * (1 - 0.667 * tanh(10 * mean(tall))) * (1 + 0.667 * tanh(length(tall) / 10))
  } else if (m.type == "accounts" && length(tall) == 0) {
    tstar <- 0.81 * (1 - 0.667 * tanh(10 * 0)) * (1 + 0.667 * tanh(0 / 10))
  }
  ncm1_z <- (tstar^1 + sum(tall^1)) / (length(tall) + 1)
  ncm2_z <- (tstar^2 + sum(tall^2)) / (length(tall) + 1)
  ncm3_z <- (tstar^3 + sum(tall^3)) / (length(tall) + 1)
  ncm1_e <- (length(tall) + 1) / (n + 2)
  ncm2_e <- ((length(tall) + 2) / (n + 3)) * ncm1_e
  ncm3_e <- ((length(tall) + 3) / (n + 4)) * ncm2_e
  ncm1_t <- ncm1_e * ncm1_z
  ncm2_t <- (ncm1_e * ncm2_z + ((n - 1) * ncm2_e * ncm1_z^2)) / n
  ncm3_t <- ((ncm1_e * ncm3_z + (3 * (n - 1) * ncm2_e * ncm1_z * ncm2_z)) / n^2) + (((n - 1) * (n - 2) * ncm3_e * ncm1_z^3) / (n^2))
  cm2_t <- ncm2_t - ncm1_t^2
  cm3_t <- ncm3_t - (3 * ncm1_t * ncm2_t) + (2 * ncm1_t^3)
  A <- (4 * cm2_t^3) / (cm3_t^2)
  B <- cm3_t / (2 * cm2_t)
  G <- ncm1_t - ((2 * cm2_t^2) / cm3_t)
  result <- list()
  result[["mle"]] <- sum(taints) / n
  result[["ub"]] <- switch(alternative,
    "two.sided" = G + (A * B * (1 + (stats::qnorm(conf.level + ((1 - conf.level) / 2)) / sqrt(9 * A)) - (1 / (9 * A)))^3),
    "less" = G + (A * B * (1 + (stats::qnorm(conf.level) / sqrt(9 * A)) - (1 / (9 * A)))^3),
    "greater" = 1
  )
  result[["lb"]] <- switch(alternative,
    "two.sided" = G + (A * B * (1 + (stats::qnorm(((1 - conf.level) / 2)) / sqrt(9 * A)) - (1 / (9 * A)))^3),
    "less" = 0,
    "greater" = G + (A * B * (1 + (stats::qnorm(1 - conf.level) / sqrt(9 * A)) - (1 / (9 * A)))^3)
  )
  result[["precision"]] <- if (alternative == "greater") result[["mle"]] - result[["lb"]] else result[["ub"]] - result[["mle"]]
  return(result)
}

.coxsnell <- function(taints, conf.level, n, alternative, cs.a = 1, cs.b = 3, cs.mu = 0.5, a = 1, b = 1) {
  pi.prior <- a / (a + b)
  taints <- subset(taints, taints > 0)
  M <- length(taints)
  t_bar <- mean(taints)
  if (M == 0) {
    t_bar <- 0
  }
  result <- list()
  result[["mf"]] <- ((M + cs.a) / (M + cs.b)) * ((cs.mu * (cs.b - 1)) + (M * t_bar)) / (n + (cs.a / pi.prior))
  result[["df1"]] <- 2 * (M + cs.a)
  result[["df2"]] <- 2 * (M + cs.b)
  result[["mle"]] <- result[["mf"]] * (((result[["df1"]] - 2) / result[["df1"]]) * (result[["df2"]] / (result[["df2"]] + 2)))
  result[["ub"]] <- switch(alternative,
    "two.sided" = result[["mf"]] * stats::qf(p = conf.level + ((1 - conf.level) / 2), df1 = result[["df1"]], df2 = result[["df2"]]),
    "less" = result[["mf"]] * stats::qf(p = conf.level, df1 = result[["df1"]], df2 = result[["df2"]]),
    "greater" = 1
  )
  result[["lb"]] <- switch(alternative,
    "two.sided" = result[["mf"]] * stats::qf(p = ((1 - conf.level) / 2), df1 = result[["df1"]], df2 = result[["df2"]]),
    "less" = 0,
    "greater" = result[["mf"]] * stats::qf(p = 1 - conf.level, df1 = result[["df1"]], df2 = result[["df2"]])
  )
  result[["precision"]] <- if (alternative == "greater") result[["mle"]] - result[["lb"]] else result[["ub"]] - result[["mle"]]
  return(result)
}

.mpu <- function(t, conf.level, alternative, n) {
  result <- list()
  zVal <- if (alternative == "two.sided") stats::qnorm(p = conf.level + ((1 - conf.level) / 2)) else stats::qnorm(p = conf.level)
  result[["mle"]] <- sum(t) / n
  result[["lb"]] <- if (alternative == "less") 0 else mean(t) - zVal * (stats::sd(t) / sqrt(n))
  result[["ub"]] <- if (alternative == "greater") 1 else mean(t) + zVal * (stats::sd(t) / sqrt(n))
  result[["precision"]] <- if (alternative == "greater") result[["mle"]] - result[["lb"]] else result[["ub"]] - result[["mle"]]
  return(result)
}

.pps <- function(t, conf.level, alternative, n) {
  result <- list()
  tVal <- if (alternative == "two.sided") stats::qt(p = conf.level + ((1 - conf.level) / 2), df = sum(t != 0) - 1) else stats::qt(p = conf.level, df = sum(t != 0) - 1)
  result[["mle"]] <- sum(t) / n
  result[["lb"]] <- if (alternative == "less") 0 else mean(t) - tVal * (stats::sd(t) / sqrt(n))
  result[["ub"]] <- if (alternative == "greater") 1 else mean(t) + tVal * (stats::sd(t) / sqrt(n))
  result[["precision"]] <- if (alternative == "greater") result[["mle"]] - result[["lb"]] else result[["ub"]] - result[["mle"]]
  return(result)
}

.direct <- function(bookvalues, auditvalues, conf.level, alternative, N.items = NULL, n, N.units = NULL, correction = FALSE) {
  stopifnot(
    "missing value for 'N.items'" = !is.null(N.items),
    "missing value for 'N.units'" = !is.null(N.units)
  )
  w <- mean(auditvalues)
  s <- stats::sd(auditvalues)
  tVal <- if (alternative == "two.sided") stats::qt(p = conf.level + ((1 - conf.level) / 2), df = n - 1) else stats::qt(p = conf.level, df = n - 1)
  errorMargin <- tVal * (s / sqrt(n))
  result <- list()
  result[["mle"]] <- N.units - N.items * w
  if (correction) {
    result[["lb"]] <- if (alternative == "less") -Inf else N.units - N.items * (w + errorMargin * sqrt((N.items - n) / (N.items - 1)))
    result[["ub"]] <- if (alternative == "greater") Inf else N.units - N.items * (w - errorMargin * sqrt((N.items - n) / (N.items - 1)))
  } else {
    result[["lb"]] <- if (alternative == "less") -Inf else N.units - N.items * (w + errorMargin)
    result[["ub"]] <- if (alternative == "greater") Inf else N.units - N.items * (w - errorMargin)
  }
  result[["precision"]] <- if (alternative == "greater") result[["mle"]] - result[["lb"]] else result[["ub"]] - result[["mle"]]
  return(result)
}

.difference <- function(bookvalues, auditvalues, conf.level, alternative, N.items = NULL, n, correction = FALSE) {
  stopifnot("missing value for 'N.items'" = !is.null(N.items))
  we <- mean(bookvalues - auditvalues)
  s <- stats::sd(bookvalues - auditvalues)
  tVal <- if (alternative == "two.sided") stats::qt(p = conf.level + ((1 - conf.level) / 2), df = n - 1) else stats::qt(p = conf.level, df = n - 1)
  errorMargin <- tVal * (s / sqrt(n))
  result <- list()
  result[["mle"]] <- N.items * we
  if (correction) {
    result[["lb"]] <- if (alternative == "less") -Inf else N.items * (we - errorMargin * sqrt((N.items - n) / (N.items - 1)))
    result[["ub"]] <- if (alternative == "greater") Inf else N.items * (we + errorMargin * sqrt((N.items - n) / (N.items - 1)))
  } else {
    result[["lb"]] <- if (alternative == "less") -Inf else N.items * (we - errorMargin)
    result[["ub"]] <- if (alternative == "greater") Inf else N.items * (we + errorMargin)
  }
  result[["precision"]] <- if (alternative == "greater") result[["mle"]] - result[["lb"]] else result[["ub"]] - result[["mle"]]
  return(result)
}

.quotient <- function(bookvalues, auditvalues, conf.level, alternative, N.items = NULL, n, correction = FALSE) {
  stopifnot("missing value for 'N.items'" = !is.null(N.items))
  w <- mean(auditvalues)
  sw <- stats::sd(auditvalues)
  b <- mean(bookvalues)
  sb <- stats::sd(bookvalues)
  r <- stats::cor(bookvalues, auditvalues)
  q <- w / b
  s <- sqrt(sw^2 - 2 * q * r * sb * sw + q^2 * sb^2)
  tVal <- if (alternative == "two.sided") stats::qt(p = conf.level + ((1 - conf.level) / 2), df = n - 1) else stats::qt(p = conf.level, df = n - 1)
  errorMargin <- tVal * (s / sqrt(n))
  result <- list()
  result[["mle"]] <- N.items * ((1 - q) * b)
  if (correction) {
    result[["lb"]] <- if (alternative == "less") -Inf else N.items * ((1 - q) * b - errorMargin * sqrt((N.items - n) / (N.items - 1)))
    result[["ub"]] <- if (alternative == "greater") Inf else N.items * ((1 - q) * b + errorMargin * sqrt((N.items - n) / (N.items - 1)))
  } else {
    result[["lb"]] <- if (alternative == "less") -Inf else N.items * ((1 - q) * b - errorMargin)
    result[["ub"]] <- if (alternative == "greater") Inf else N.items * ((1 - q) * b + errorMargin)
  }
  result[["precision"]] <- if (alternative == "greater") result[["mle"]] - result[["lb"]] else result[["ub"]] - result[["mle"]]
  return(result)
}

.regression <- function(bookvalues, auditvalues, conf.level, alternative, N.items = NULL, n, N.units = NULL, correction = FALSE) {
  stopifnot(
    "missing value for 'N.items'" = !is.null(N.items),
    "missing value for 'N.units'" = !is.null(N.units)
  )
  w <- mean(auditvalues)
  sw <- stats::sd(auditvalues)
  b <- mean(bookvalues)
  r <- stats::cor(bookvalues, auditvalues)
  coefs <- stats::coef(stats::lm(auditvalues ~ bookvalues))
  b1 <- as.numeric(coefs[2])
  s <- sw * sqrt(1 - r^2)
  tVal <- if (alternative == "two.sided") stats::qt(p = conf.level + ((1 - conf.level) / 2), df = n - 1) else stats::qt(p = conf.level, df = n - 1)
  errorMargin <- tVal * (s / sqrt(n))
  result <- list()
  result[["mle"]] <- N.units - (N.items * w + b1 * (N.units - N.items * b))
  if (correction) {
    result[["lb"]] <- if (alternative == "less") -Inf else result[["mle"]] - N.items * errorMargin * sqrt((N.items - n) / (N.items - 1))
    result[["ub"]] <- if (alternative == "greater") Inf else result[["mle"]] + N.items * errorMargin * sqrt((N.items - n) / (N.items - 1))
  } else {
    result[["lb"]] <- if (alternative == "less") -Inf else result[["mle"]] - N.items * errorMargin
    result[["ub"]] <- if (alternative == "greater") Inf else result[["mle"]] + N.items * errorMargin
  }
  result[["precision"]] <- if (alternative == "greater") result[["mle"]] - result[["lb"]] else result[["ub"]] - result[["mle"]]
  return(result)
}
