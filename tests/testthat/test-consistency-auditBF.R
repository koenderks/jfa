context("1. Test consistency of function auditBF()")

# jfa version 0.5.5

test_that(desc = "(id: f1-v0.5.5-t1) Test Bayes factors for beta prior", {
  # Compute a Bayes factor from a negligible beta prior, n = 160, k = 1
  # Derks, K., de Swart, J., Wagenmakers, E-.J., and Wetzels, R. (2021). The Bayesian approach to audit evidence: Quantifying statistical evidence using the Bayes factor.
  BF <- auditBF(materiality = 0.03, n = 160, k = 1, nPrior = 0, kPrior = 0)
  expect_equal(BF, 696.696)
  
  # Compute a Bayes factor from a negligible beta prior, n = 50, k = 1
  # Derks, K., de Swart, J., Wagenmakers, E-.J., and Wetzels, R. (2021). A default Bayesian hypothesis test for audit sampling.
  BF <- auditBF(materiality = 0.05, n = 50, k = 1, nPrior = 0, kPrior = 0)
  expect_equal(BF, 51.551344113750538)
  
  # Compute a default Bayes factor from an impartial beta prior
  # Derks, K., de Swart, J., Wagenmakers, E-.J., and Wetzels, R. (2021). A default Bayesian hypothesis test for audit sampling.
  BF <- auditBF(materiality = 0.05, n = 50, k = 1)
  expect_equal(BF, 4.9852019781149854)
})

test_that(desc = "(id: f1-v0.5.5-t6) Test Bayes factors for gamma prior", {
  
  # Compute a Bayes factor from a negligible gamma prior
  BF <- auditBF(materiality = 0.05, n = 50, k = 1, nPrior = 0, kPrior = 0, likelihood = "poisson")
  expect_equal(BF, Inf)
  
  # Compute a Bayes factor from a negligible gamma prior
  BF <- auditBF(materiality = 0.03, n = 160, k = 1, likelihood = "poisson")
  expect_equal(BF, 36.42728)
  
  # Compute a default Bayes factor from an impartial gamma prior
  BF <- auditBF(materiality = 0.05, n = 50, k = 1, likelihood = "poisson")
  expect_equal(BF, 4.810668425)
})

test_that(desc = "(id: f1-v0.5.5-t11) Test Bayes factors for beta-binomial prior", {
  # Compute a Bayes factor from a negligible beta prior
  BF <- auditBF(materiality = 0.05, n = 50, k = 1, nPrior = 0, kPrior = 0, likelihood = "hypergeometric", N = 1000)
  expect_equal(BF, 58.37849102)
  
  # Compute a Bayes factor from a negligible beta prior
  BF <- auditBF(materiality = 0.03, n = 160, k = 1, nPrior = 0, kPrior = 0, likelihood = "hypergeometric", N = 1000)
  expect_equal(BF, 1248.71551)
  
  # Compute a default Bayes factor from an impartial beta prior
  BF <- auditBF(materiality = 0.05, n = 50, k = 1, likelihood = "hypergeometric", N = 1000)
  expect_equal(BF, 5.676959268)
})