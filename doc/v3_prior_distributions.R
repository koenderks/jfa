## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height=4, fig.width=6)
library(jfa)

## -----------------------------------------------------------------------------
confidence    <- 0.95       # 95% confidence
likelihood    <- 'binomial' # Binomial likelihood
materiality   <- 0.05       # Performance materiality of 5%
expectedError <- 0.01       # 1% errors expected in sample

## -----------------------------------------------------------------------------
prior1 <- auditPrior(confidence, materiality, expectedError, likelihood, method = 'none')
prior1

## -----------------------------------------------------------------------------
plot(prior1)

## -----------------------------------------------------------------------------
prior2 <- auditPrior(confidence, materiality, expectedError, likelihood, method = 'arm', 
                     ir = 0.9, cr = 0.6)
prior2

## -----------------------------------------------------------------------------
plot(prior2)

## -----------------------------------------------------------------------------
prior3 <- auditPrior(confidence, materiality, expectedError, likelihood, method = 'bram',
                     ub = 0.6)
prior3

## -----------------------------------------------------------------------------
plot(prior3)

## -----------------------------------------------------------------------------
prior4 <- auditPrior(confidence, materiality, expectedError, likelihood, method = 'median')
prior4

## -----------------------------------------------------------------------------
plot(prior4)

## -----------------------------------------------------------------------------
prior5 <- auditPrior(confidence, materiality, expectedError, likelihood, method = 'hypotheses', 
                     pHmin = 0.6)
prior5

## -----------------------------------------------------------------------------
plot(prior5)

## -----------------------------------------------------------------------------
prior6 <- auditPrior(confidence, materiality, expectedError, likelihood, method = 'sample',
                     sampleN = 30, sampleK = 0)
prior6

## -----------------------------------------------------------------------------
plot(prior6)

## -----------------------------------------------------------------------------
prior7 <- auditPrior(confidence, materiality, expectedError, likelihood, method = 'factor',
                     sampleN = 58, sampleK = 0, factor = 0.7)
prior7

## -----------------------------------------------------------------------------
plot(prior7)

## -----------------------------------------------------------------------------
jfa::planning(confidence, materiality, expectedError = expectedError, prior = prior7)

## -----------------------------------------------------------------------------
jfa::evaluation(confidence, materiality, nSumstats = 60, kSumstats = 1, prior = prior7)

