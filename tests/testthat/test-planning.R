context("Planning")

test_that(desc = "Frequentist Poisson 1% materiality", {
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "poisson")
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 300)
  expect_equal(jfaRes$expectedSampleError, 0)
})

test_that(desc = "Frequentist Poisson 5% materiality", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, likelihood = "poisson")
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 60)
  expect_equal(jfaRes$expectedSampleError, 0)
})

test_that(desc = "Frequentist Poisson 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "poisson")
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 231)
  expect_equal(jfaRes$expectedSampleError, 5.78)
})

test_that(desc = "Frequentist Poisson 5% materiality 2 errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 2, likelihood = "poisson")
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 126)
  expect_equal(jfaRes$expectedSampleError, 2)
})

test_that(desc = "Bayesian Poisson 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "poisson", prior = TRUE)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 231)
  expect_equal(jfaRes$expectedSampleError, 5.78)
})

test_that(desc = "Bayesian Poisson 5% materiality 2.5% errors custom prior", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "poisson", prior = TRUE, kPrior = 1, nPrior = 7)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 285)
  expect_equal(jfaRes$expectedSampleError, 7.12)
})

test_that(desc = "Frequentist binomial 1% materiality", {
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "binomial")
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 299)
  expect_equal(jfaRes$expectedSampleError, 0)
})

test_that(desc = "Frequentist binomial 5% materiality", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, likelihood = "binomial")
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 59)
  expect_equal(jfaRes$expectedSampleError, 0)
})

test_that(desc = "Frequentist binomial 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "binomial")
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 234)
  expect_equal(jfaRes$expectedSampleError, 6)
})

test_that(desc = "Frequentist binomial 5% materiality 2 errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 2, likelihood = "binomial")
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 124)
  expect_equal(jfaRes$expectedSampleError, 2)
})

test_that(desc = "Bayesian binomial 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "binomial", prior = TRUE)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 220)
  expect_equal(jfaRes$expectedSampleError, 5.5)
})

test_that(desc = "Bayesian binomial 5% materiality 2.5% errors custom prior", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "binomial", prior = TRUE, kPrior = 1, nPrior = 7)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 273)
})

test_that(desc = "Frequentist hypergeometric 1% materiality", {
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "hypergeometric", N = 1000)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 258)
  expect_equal(jfaRes$expectedSampleError, 0)
})

test_that(desc = "Frequentist hypergeometric 5% materiality", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, likelihood = "hypergeometric", N = 1000)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 57)
  expect_equal(jfaRes$expectedSampleError, 0)
})

test_that(desc = "Frequentist hypergeometric 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "hypergeometric", N = 1000)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 197)
  expect_equal(jfaRes$expectedSampleError, 5)
})

test_that(desc = "Frequentist hypergeometric 5% materiality 2 errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 2, likelihood = "hypergeometric", N = 1000)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 119)
  expect_equal(jfaRes$expectedSampleError, 2)
})

test_that(desc = "Bayesian hypergeometric 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "hypergeometric", N = 1000, prior = TRUE)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 160)
  expect_equal(jfaRes$expectedSampleError, 4)
})

test_that(desc = "Bayesian hypergeometric 5% materiality 2.5% errors custom prior", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "hypergeometric", N = 1000, prior = TRUE, kPrior = 1, nPrior = 7)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 193)
  expect_equal(jfaRes$expectedSampleError, 5)
})