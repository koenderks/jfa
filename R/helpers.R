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

.stringerBound <- function(taints, confidence, n, correction = NULL){
  t <- ifelse(taints < 0, yes = 0, no = taints)
  t <- ifelse(taints > 1, yes = 1, no = taints)
  t <- sort(subset(t, t > 0), decreasing = TRUE)
  bound <- 1 - (1 - confidence)^(1 / n)
  if(length(t) > 0){
    propSum <- 0
    for(i in 1:length(t)){
      propSum <- propSum + (stats::qbeta(p = confidence, shape1 = i + 1, shape2 = n - i) - stats::qbeta(p = confidence, shape1 = (i - 1) + 1, shape2 = n - (i - 1)))  * t[i]
    }
    bound <- bound + propSum
  }
  if(!is.null(correction) && correction == "meikle"){
    tmin <- sort(subset(taints, taints < 0), decreasing = FALSE)
    if(length(tmin) > 0){
      prop.sum.min  <- stats::qbeta(1 + 1, n - 1, p = confidence) * abs(tmin[1])
      if(length(tmin) > 2){
        prop.sum.min.2  <- 0
        for(i in 2:length(tmin)){
          prop.sum.min.2 <- prop.sum.min.2 + (stats::qbeta(i + 1, n - 1, p = confidence) - stats::qbeta((i-1) + 1, n - 1, p = confidence)) * abs(tmin[i])
        }
        prop.sum.min    <- prop.sum.min + prop.sum.min.2
      }
      bound             <- bound - prop.sum.min
    }
  } else if(!is.null(correction) && correction == "lta"){
    tmin <- subset(taints, taints < 0)
    if(length(tmin) > 0){
      ltaCorrection <- (1/n) * sum(abs(tmin))
      bound <- bound - ltaCorrection
    }
  } else if(!is.null(correction) && correction == "pvz"){
    taints <- ifelse(taints < 0, 0, taints)
    tmin <- sort(subset(taints, taints > 0))
    if(length(tmin) > 0){
      constant <- 0
      for(i in 1:length(tmin)){
        constant <- constant + (((n - 2 * i + 1)/(2 * sqrt(i*(n - i + 1)))) * rev(tmin)[i])
      }
      constant <- (1/n) * constant
      sigma <- (1/n) * sum((tmin - mean(tmin))^2)
      pvzCorrection <- (constant - sqrt(sigma)) / sqrt(n) * stats::qnorm(p = confidence, lower.tail = TRUE)
      bound <- bound - pvzCorrection
    }
  }
  return(bound)
}

.rohrbachBound <- function(taints, confidence, n, N = NULL, rohrbachDelta){
  if(is.null(N))
    stop("Rohrbach's bound requires that you specify the population size N")
  w <- 1 - taints
  mu <- mean(taints)
  vars <- sum(w^2)/n - (2-(rohrbachDelta/n)) * ((1/2) * ((sum(w^2)/n) - stats::var(w)))
  bound <- mu + stats::qnorm(p = confidence) * sqrt((1-(n/N)) * (vars/n))
  return(bound)
}

.momentBound <- function(taints, confidence, n, momentPoptype){
  if(!(momentPoptype %in% c("inventory", "accounts")))
    stop("Specify a valid population type. Either inventory or accounts.")
  tall <- subset(taints, taints != 0)
  if(momentPoptype == "inventory" & length(tall) > 0){
    tstar <- 0.81 * (1 - 0.667 * tanh(10 * abs(mean(tall))))
  } else if(momentPoptype == "inventory" & length(tall) == 0){
    tstar <- 0.81 * (1 - 0.667 * tanh(10 * 0))
  }
  if(momentPoptype == "accounts" & length(tall) > 0){
    tstar <- 0.81 * (1 - 0.667 * tanh(10 * mean(tall))) * (1 + 0.667 * tanh(length(tall) / 10))
  } else if(momentPoptype == "accounts" & length(tall) == 0){
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
  ncm3_t <- ((ncm1_e * ncm3_z + (3 * (n - 1) * ncm2_e * ncm1_z * ncm2_z)) / 
               n^2) + (((n - 1) * (n - 2) * ncm3_e * ncm1_z^3)/(n^2))
  cm2_t  <- ncm2_t - ncm1_t^2
  cm3_t  <- ncm3_t - (3 * ncm1_t * ncm2_t) + (2 * ncm1_t^3)
  A      <- (4 * cm2_t^3)/(cm3_t^2)
  B      <- cm3_t / (2 * cm2_t)
  G      <- ncm1_t - ((2 * cm2_t^2)/cm3_t)
  bound  <- G + (A * B * (1 + (stats::qnorm(confidence, mean = 0, sd = 1)/ sqrt(9 * A)) - (1 / (9 * A)))^3)
  return(bound)
}

.directMethod <- function(bookValues, auditValues, confidence, N = NULL, n, populationBookValue = NULL){
  if(is.null(N))
    stop("The direct method requires that you specify the population size N")
  if(is.null(populationBookValue))
    stop("The direct method requires that you specify the total population book value")
  w <- mean(auditValues)
  s <- stats::sd(auditValues)
  pointEstimate <- N * w
  lowerBound <- pointEstimate + stats::qt(p = (1 - confidence) / 2, df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1))
  upperBound <- pointEstimate - stats::qt(p = (1 - confidence) / 2, df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1))
  result <- list()
  result[["pointEstimate"]] <- pointEstimate
  result[["lowerBound"]] <- lowerBound
  result[["upperBound"]] <- upperBound
  return(result)
}

.differenceMethod <- function(bookValues, auditValues, confidence, N = NULL, n, populationBookValue = NULL){
  if(is.null(N))
    stop("The difference method requires that you specify the population size N")
  if(is.null(populationBookValue))
    stop("The difference method requires that you specify the total population book value")
  we <- mean(bookValues - auditValues)
  e <- N * we
  s <- stats::sd(bookValues - auditValues)
  pointEstimate <- populationBookValue - e
  lowerBound <- pointEstimate + stats::qt(p = (1 - confidence) / 2, df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1))
  upperBound <- pointEstimate - stats::qt(p = (1 - confidence) / 2, df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1))
  result <- list()
  result[["pointEstimate"]] <- pointEstimate
  result[["lowerBound"]] <- lowerBound
  result[["upperBound"]] <- upperBound
  return(result)
}

.quotientMethod <- function(bookValues, auditValues, confidence, N = NULL, n, populationBookValue = NULL){
  if(is.null(N))
    stop("The quotient method requires that you specify the population size N")
  if(is.null(populationBookValue))
    stop("The quotient method requires that you specify the total population book value")
  w <- mean(auditValues)
  sw <- stats::sd(auditValues)
  b <- mean(bookValues)
  sb <- stats::sd(bookValues)
  r <- stats::cor(bookValues, auditValues)
  q <- w / b
  s <- sqrt( sw^2 - 2 * q * r * sb * sw + q^2 * sb^2 )
  pointEstimate <- q * populationBookValue
  lowerBound <- pointEstimate + stats::qt(p = (1 - confidence) / 2, df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1))
  upperBound <- pointEstimate - stats::qt(p = (1 - confidence) / 2, df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1))
  result <- list()
  result[["pointEstimate"]] <- pointEstimate
  result[["lowerBound"]] <- lowerBound
  result[["upperBound"]] <- upperBound
  return(result)
}

.regressionMethod <- function(bookValues, auditValues, confidence, N = NULL, n, populationBookValue = NULL){
  if(is.null(N))
    stop("The regression method requires that you specify the population size N")
  if(is.null(populationBookValue))
    stop("The regression method requires that you specify the total population book value")
  w <- mean(auditValues)
  sw <- stats::sd(auditValues)
  b <- mean(bookValues)
  r <- stats::cor(bookValues, auditValues)
  b1 <- (sum(bookValues * auditValues) - n * b * w) / (sum(bookValues^2) - (sum(bookValues)^2) / n)
  s <- sw * sqrt(1 - r^2)
  pointEstimate <- N * w + b1 * (populationBookValue - N * b)
  lowerBound <- pointEstimate + stats::qt(p = (1 - confidence) / 2, df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1))
  upperBound <- pointEstimate - stats::qt(p = (1 - confidence) / 2, df = n - 1) * s * (N / sqrt(n)) * sqrt((N-n)/(N-1))
  result <- list()
  result[["pointEstimate"]] <- pointEstimate
  result[["lowerBound"]] <- lowerBound
  result[["upperBound"]] <- upperBound
  return(result)
}

.coxAndSnellBound <- function(taints, confidence, n, csA = 1, csB = 3, csMu = 0.5, aPrior = 1, bPrior = 1){
  piPrior <- aPrior / (aPrior + bPrior)
  taints <- subset(taints, taints > 0)
  M <- length(taints)
  t_bar <- mean(taints)
  if(M == 0)
    t_bar <- 0
  multiplicationFactor <- ((M + csA) / (M + csB)) * ((csMu * (csB - 1)) + (M * t_bar)) / (n + (csA / piPrior))
  df1 <- 2 * (M + csA)
  df2 <- 2 * (M + csB)
  bound <- multiplicationFactor * stats::qf(p = confidence, df1 = df1, df2 = df2)
  result <- list()
  result[["multiplicationFactor"]] <- multiplicationFactor
  result[["df1"]] <- df1
  result[["df2"]] <- df2
  result[["bound"]] <- bound
  return(result)
}

.dCoxAndSnellF <- function(x, df1, df2, multiplicationFactor){
  # Rewritten using Wolfram Mathematica
  (df1 ** (df1 / 2) * df2**(df2 / 2) * (x / multiplicationFactor) ** (- 1 + df1 / 2) * (df2 + (df1 * x) / multiplicationFactor)**(( -df1 - df2) / 2))/(abs(multiplicationFactor) * beta(df1/2, df2/2))
}