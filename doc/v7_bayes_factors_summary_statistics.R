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
evaluation(materiality = tolerance, x = 2, n = 32, prior = TRUE)

## -----------------------------------------------------------------------------
evaluation(materiality = tolerance, x = 2 + 0, n = 32 + 50, prior = TRUE)

