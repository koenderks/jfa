## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height=4, fig.width=6)
library(jfa)

## -----------------------------------------------------------------------------
prior <- auditPrior(confidence = 0.95, materiality = 0.05, method = "none", likelihood = "binomial")
evaluation(confidence = 0.95, materiality = 0.05, nSumstats = 40, kSumstats = 1, prior = prior)

## -----------------------------------------------------------------------------
prior <- auditPrior(confidence = 0.95, materiality = 0.05, method = "median", likelihood = "binomial")
evaluation(confidence = 0.95, materiality = 0.05, nSumstats = 40, kSumstats = 1, prior = prior)

