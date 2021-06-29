## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height=4, fig.width=6)
library(jfa)

## -----------------------------------------------------------------------------
prior <- auditPrior(confidence = 0.95, materiality = 0.05, method = "none", likelihood = "binomial")
stage4 <- evaluation(confidence = 0.95, materiality = 0.05, nSumstats = 40, kSumstats = 1, prior = prior)
summary(stage4)

## -----------------------------------------------------------------------------
prior <- auditPrior(confidence = 0.95, materiality = 0.05, method = "median", likelihood = "binomial")
stage4 <- evaluation(confidence = 0.95, materiality = 0.05, nSumstats = 40, kSumstats = 1, prior = prior)
summary(stage4)

