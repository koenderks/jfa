## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height=4, fig.width=6)
library(jfa)

## -----------------------------------------------------------------------------
confidence <- 0.90 # 90% confidence
tolerance  <- 0.05 # 5% tolerance

## ---- eval = F----------------------------------------------------------------
#  ISO28596:::CI.binom.shortest(size = 32, x = 2, level = confidence)$bounds
#  #      x estimate      lower     upper
#  # [1,] 2   0.0625 0.01674365 0.1866428

## ---- eval = F----------------------------------------------------------------
#  ISO28596:::CI.binom.shortest(size = 32 + 50, x = 2 + 0, level = confidence)$bounds
#  #      x   estimate       lower      upper
#  # [1,] 2 0.02439024 0.006504196 0.07341118

## -----------------------------------------------------------------------------
auditBF(tolerance, n = 32, k = 2)

## -----------------------------------------------------------------------------
auditBF(tolerance, n = 32 + 50, k = 2 + 0)

## ---- echo = F----------------------------------------------------------------
set.seed(2)
k <- 0
n <- 100
bf <- NULL
for (i in 1:n) {
  k <- k + sample(0:1, size = 1, prob = c(0.98, 0.02))
  bf[i] <- auditBF(tolerance, n = i, k = k)
}
plot(1:n, bf, type = "l", xlab = "n", las = 1, bty = "n", ylim = c(0, 30), xlim = c(0, 100), ylab = expression(BF['10']))

