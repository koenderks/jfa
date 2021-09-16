## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height=4, fig.width=6)
library(jfa)

## -----------------------------------------------------------------------------
confidence  <- 0.95       # 95% confidence
likelihood  <- 'binomial' # Binomial likelihood
materiality <- 0.05       # Performance materiality of 5%
expected    <- 0.01       # 1% errors expected in sample

## -----------------------------------------------------------------------------
prior1 <- auditPrior(method = 'default', likelihood = likelihood, expected = expected, conf.level = confidence)
summary(prior1)

## -----------------------------------------------------------------------------
plot(prior1)

## -----------------------------------------------------------------------------
prior2 <- auditPrior(method = 'strict', likelihood = likelihood)
summary(prior2)

## -----------------------------------------------------------------------------
plot(prior2)

## -----------------------------------------------------------------------------
prior3 <- auditPrior(method = 'param', likelihood = likelihood, alpha = 2, beta = 10)
summary(prior3)

## -----------------------------------------------------------------------------
plot(prior3)

## -----------------------------------------------------------------------------
prior4 <- auditPrior(method = 'impartial', likelihood = likelihood, expected = expected, conf.level = confidence, 
                     materiality = materiality)
summary(prior4)

## -----------------------------------------------------------------------------
plot(prior4)

## -----------------------------------------------------------------------------
prior5 <- auditPrior(method = 'hyp', likelihood = likelihood, expected = expected, conf.level = confidence,
                     materiality = materiality, p.hmin = 0.6)
summary(prior5)

## -----------------------------------------------------------------------------
plot(prior5)

## -----------------------------------------------------------------------------
prior6 <- auditPrior(method = 'arm', likelihood = likelihood, expected = expected, conf.level = confidence,
                     materiality = materiality, ir = 0.9, cr = 0.6)
summary(prior6)

## -----------------------------------------------------------------------------
plot(prior6)

## -----------------------------------------------------------------------------
prior7 <- auditPrior(method = 'bram', likelihood = likelihood, expected = expected, conf.level = confidence, 
                     materiality = materiality, ub = 0.6)
summary(prior7)

## -----------------------------------------------------------------------------
plot(prior7)

## -----------------------------------------------------------------------------
prior8 <- auditPrior(method = 'sample', likelihood = likelihood, expected = expected, conf.level = confidence,
                     materiality = materiality, x = 0, n = 30)
summary(prior8)

## -----------------------------------------------------------------------------
plot(prior8)

## -----------------------------------------------------------------------------
prior9 <- auditPrior(method = 'factor', likelihood = likelihood, expected = expected, conf.level = confidence,
                     materiality = materiality, x = 0, n = 58, factor = 0.7)
summary(prior9)

## -----------------------------------------------------------------------------
plot(prior9)

## -----------------------------------------------------------------------------
jfa::planning(materiality = materiality, expected = expected, conf.level = confidence, prior = prior9)

## -----------------------------------------------------------------------------
jfa::evaluation(materiality = materiality, conf.level = confidence, x = 1, n = 60, prior = prior9)

