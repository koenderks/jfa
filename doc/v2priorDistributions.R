## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height=4, fig.width=6)
library(jfa)

## -----------------------------------------------------------------------------
confidence <- 0.95        # 95% confidence
likelihood <- "binomial"  # Binomial likelihood
materiality <- 0.05       # Performance materiality of 5%
expectedError <- 0        # Zero errors expected in sample

## -----------------------------------------------------------------------------
prior1 <- auditPrior(confidence, materiality, expectedError, likelihood, method = "none")
prior1

## -----------------------------------------------------------------------------
plot(prior1)

## -----------------------------------------------------------------------------
prior2 <- auditPrior(confidence, materiality, expectedError, method = "arm", likelihood = likelihood, ir = 0.9, cr = 0.6)
prior2

## -----------------------------------------------------------------------------
plot(prior2)

## -----------------------------------------------------------------------------
prior3 <- auditPrior(confidence, materiality, expectedError, method = "median", likelihood = likelihood)
prior3

## -----------------------------------------------------------------------------
plot(prior3)

## -----------------------------------------------------------------------------
prior4 <- auditPrior(confidence, materiality, expectedError, method = "hypotheses", likelihood = likelihood, pHmin = 0.6)
prior4

## -----------------------------------------------------------------------------
plot(prior4)

## -----------------------------------------------------------------------------
prior5 <- auditPrior(confidence, materiality, expectedError, likelihood = likelihood, method = "sample", sampleN = 30, sampleK = 0)
prior5

## -----------------------------------------------------------------------------
plot(prior5)

## -----------------------------------------------------------------------------
prior6 <- auditPrior(confidence, materiality, expectedError, method = "factor", likelihood = likelihood, sampleN = 58, sampleK = 0, factor = 0.7)
prior6

## -----------------------------------------------------------------------------
plot(prior6)

## ---- collapse = TRUE, echo = FALSE-------------------------------------------
s1 <- planning(confidence, materiality, expectedError = expectedError, likelihood = likelihood, prior = prior1)$sampleSize
s2 <- planning(confidence, materiality, expectedError = expectedError, likelihood = likelihood, prior = prior2)$sampleSize
s3 <- planning(confidence, materiality, expectedError = expectedError, likelihood = likelihood, prior = prior3)$sampleSize
s4 <- planning(confidence, materiality, expectedError = expectedError, likelihood = likelihood, prior = prior4)$sampleSize
s5 <- planning(confidence, materiality, expectedError = expectedError, likelihood = likelihood, prior = prior5)$sampleSize
s6 <- planning(confidence, materiality, expectedError = expectedError, likelihood = likelihood, prior = prior6)$sampleSize

knitr::kable(data.frame("none" = s1, "arm" = s2, "median" = s3, "hypotheses" = s4, 
                  "sample" = s5, "factor" = s6, row.names = "Required sample size"))

