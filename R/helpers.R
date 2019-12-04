.dBetaBinom <- function(x, N, shape1, shape2){
  logval <- lbeta(x + shape1, N - x + shape2) - lbeta(shape1, shape2) + lchoose(N, x)
  ret <- exp(logval)
  return(ret)
}

.qBetaBinom <- function(p, N, shape1, shape2){
  pp <- cumsum(.dBetaBinom(0:N, N, shape1, shape2))
  return(sapply(p, function(x) sum(pp < x)))
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