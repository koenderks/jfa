context("10. Benchmark against SMASH21")

# SMASH21 [www.steekproeven.eu]
# Retrieved on 27-04-2021 from https://steekproeven.eu/wp-content/uploads/2021/01/SMASH21-PRO-kopie.xlsx

test_that(desc = "(id: f10-v0.5.3-t1) Test frequentist sample sizes", {
  theta <- 2000 / 20000 # materiality / N
  expectedError <- 0 / 20000 # exp.error / N
  plan <- planning(confidence = 0.95, materiality = theta, expectedError = expectedError, likelihood = "poisson")
  expect_equal(plan[["sampleSize"]], 30)
})

# SMASH21-Bayes [www.steekproeven.eu]
# Retrieved on 27-04-2021 from https://steekproeven.eu/wp-content/uploads/2021/01/SMASH21-Bayes-kopie.xlsx

test_that(desc = "(id: f10-v0.5.3-t2) Test Bayesian sample sizes", {
  
  N <- 20000
  materiality <- 2000 / N
  expectedError <- c(300, 500, 700, 900) / N
  ub <- c(5000, 10000, 15000, 18000) / N
  sampleSizeMatrix <- matrix(NA, nrow = length(expectedError), ncol = length(ub))
  for (i in 1:nrow(sampleSizeMatrix)) {
    for (j in 1:ncol(sampleSizeMatrix)) {
      prior <- auditPrior(confidence = 0.95, materiality = materiality, likelihood = "poisson", method = "bram", expectedError = expectedError[i], ub = ub[j])
      plan <- planning(confidence = 0.95, materiality = materiality, expectedError = expectedError[i], prior = prior)
      sampleSizeMatrix[i, j] <- plan[["sampleSize"]]
    }
  }
  
  smash_matrix <- matrix(c(28, 35, 37, 38,
                           38, 46, 49, 49,
                           54, 63, 65, 66,
                           79, 89, 92, 93),
                         byrow = TRUE,
                         nrow = length(expectedError),
                         ncol = length(ub))
  expect_equal(sampleSizeMatrix, smash_matrix)
  
})