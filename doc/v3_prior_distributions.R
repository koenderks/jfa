## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height=4, fig.width=6)
library(jfa)

## -----------------------------------------------------------------------------
confidence    <- 0.95       # 95% confidence
likelihood    <- 'binomial' # Binomial likelihood
materiality   <- 0.05       # Performance materiality of 5%
expectedError <- 0.01       # 1% errors expected in sample

## -----------------------------------------------------------------------------
prior1 <- auditPrior(method = 'none', likelihood = likelihood, 
                     expectedError = expectedError, confidence = confidence)
summary(prior1)

## -----------------------------------------------------------------------------
plot(prior1)

## -----------------------------------------------------------------------------
prior2 <- auditPrior(method = 'arm', likelihood = likelihood, 
                     expectedError = expectedError, confidence = confidence,
                     materiality = materiality, ir = 0.9, cr = 0.6)
summary(prior2)

## -----------------------------------------------------------------------------
plot(prior2)

## -----------------------------------------------------------------------------
prior3 <- auditPrior(method = 'bram', likelihood = likelihood, 
                     expectedError = expectedError, confidence = confidence,
                     materiality = materiality, ub = 0.6)
summary(prior3)

## -----------------------------------------------------------------------------
plot(prior3)

## -----------------------------------------------------------------------------
prior4 <- auditPrior(method = 'median', likelihood = likelihood, 
                     expectedError = expectedError, confidence = confidence,
                     materiality = materiality)
summary(prior4)

## -----------------------------------------------------------------------------
plot(prior4)

## -----------------------------------------------------------------------------
prior5 <- auditPrior(method = 'hypotheses', likelihood = likelihood, 
                     expectedError = expectedError, confidence = confidence,
                     materiality = materiality, pHmin = 0.6)
summary(prior5)

## -----------------------------------------------------------------------------
plot(prior5)

## -----------------------------------------------------------------------------
prior6 <- auditPrior(method = 'sample', likelihood = likelihood, 
                     expectedError = expectedError, confidence = confidence,
                     materiality = materiality, sampleN = 30, sampleK = 0)
summary(prior6)

## -----------------------------------------------------------------------------
plot(prior6)

## -----------------------------------------------------------------------------
prior7 <- auditPrior(method = 'factor', likelihood = likelihood, 
                     expectedError = expectedError, confidence = confidence,
                     materiality = materiality, sampleN = 58, sampleK = 0, factor = 0.7)
summary(prior7)

## -----------------------------------------------------------------------------
plot(prior7)

## -----------------------------------------------------------------------------
prior8 <- auditPrior(method = 'custom', likelihood = likelihood, alpha = 1, beta = 5)
summary(prior8)

## -----------------------------------------------------------------------------
plot(prior8)

## -----------------------------------------------------------------------------
jfa::planning(materiality = materiality, expectedError = expectedError, 
              confidence = confidence, prior = prior7)

## -----------------------------------------------------------------------------
jfa::evaluation(materiality = materiality, confidence = confidence, 
                nSumstats = 60, kSumstats = 1, prior = prior7)

