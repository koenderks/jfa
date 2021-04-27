context("10. Benchmark against SMASH21")

# SMASH21 [www.steekproeven.eu]

test_that(desc = "(id: f10-v0.5.3-t1) Test frequentist sample sizes", {
  theta <- 2000 / 20000 # materiality / N
  expectedError <- 0 / 20000 # exp.error / N
  plan <- planning(confidence = 0.95, materiality = theta, expectedError = expectedError, likelihood = "poisson")
  expect_equal(plan[["sampleSize"]], 30)
})

# SMASH21-Bayes [www.steekproeven.eu]

test_that(desc = "(id: f10-v0.5.3-t2) Test Bayesian sample sizes", {
  ub <- 5000 / 20000 # exp.ub / N
  theta <- 2000 / 20000 # materiality / N
  expectedError <- 300 / 20000 # exp.error / N
  prior <- auditPrior(confidence = 0.95, materiality = theta, likelihood = "poisson", method = "bram", expectedError = expectedError, ub = ub)
  plan <- planning(confidence = 0.95, materiality = theta, expectedError = expectedError, prior = prior)
  expect_equal(plan[["sampleSize"]], 28)
})