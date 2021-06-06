context("1. Test consistency of function auditBF()")

# jfa version 0.5.5

test_that(desc = "(id: f4-v0.5.5-t1) Test Bayes factors for beta prior", {
  # Compute a default Bayes factor from an impartial beta prior
  BF <- auditBF(materiality = 0.05, n = 50, k = 1)
  expect_equal(BF, 4.9852019781149854)
  
  # Compute a Bayes factor from a negligible beta prior
  BF <- auditBF(materiality = 0.05, n = 50, k = 1, nPrior = 0, kPrior = 0)
  expect_equal(BF, 51.551344113750538)
})

test_that(desc = "(id: f4-v0.5.5-t2) Test Bayes factors for gamma prior", {
  # Compute a default Bayes factor from an impartial beta prior
  BF <- auditBF(materiality = 0.05, n = 50, k = 1, likelihood = "poisson")
  expect_equal(BF, 5.0677738113216293)
  
  # Compute a Bayes factor from a negligible beta prior
  BF <- auditBF(materiality = 0.05, n = 50, k = 1, nPrior = 0, kPrior = 0, likelihood = "poisson")
  expect_equal(BF, Inf)
})

test_that(desc = "(id: f4-v0.5.5-t3) Test Bayes factors for beta-binomial prior", {
  # Compute a default Bayes factor from an impartial beta prior
  BF <- auditBF(materiality = 0.05, n = 50, k = 1, likelihood = "hypergeometric", N = 1000)
  expect_equal(BF, 4.8965459998491783)
  
  # Compute a Bayes factor from a negligible beta prior
  BF <- auditBF(materiality = 0.05, n = 50, k = 1, nPrior = 0, kPrior = 0, likelihood = "hypergeometric", N = 1000)
  expect_equal(BF, 50.540533444853317)
})