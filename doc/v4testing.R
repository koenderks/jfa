## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height=4, fig.width=6)
library(jfa)

## -----------------------------------------------------------------------------
prior <- auditPrior(confidence = 0.95, likelihood = "binomial", method = "none", materiality = 0.05)
evaluation(confidence = 0.95, materiality = 0.05, nSumstats = 40, kSumstats = 1, prior = prior)

## -----------------------------------------------------------------------------
prior <- auditPrior(confidence = 0.95, likelihood = "binomial", method = "median", materiality = 0.05)
evaluation(confidence = 0.95, materiality = 0.05, nSumstats = 40, kSumstats = 1, prior = prior)

