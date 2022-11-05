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
