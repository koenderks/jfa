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

test_that(desc = "Bayesian binomial 2% precision 5% materiality 2.5% errors median prior", {
  jfaPrior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", expectedError = 0.025)
  jfaRes <- planning(materiality = 0.05, minPrecision = 0.02, confidence = 0.95, expectedError = 0.025, likelihood = "binomial", N = 1000, prior = jfaPrior)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 283)
  expect_equal(jfaRes$expectedSampleError, 7.08)
})

test_that(desc = "Bayesian binomial 5% materiality 1% errors hypotheses prior", {
  jfaPrior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "hypotheses", pHmin = 0.7)
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.01, likelihood = "binomial", N = 1000, prior = jfaPrior)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 55)
  expect_equal(jfaRes$expectedSampleError, 0.55)
})

test_that(desc = "Bayesian poisson 5% materiality 1% errors factor prior", {
  jfaPrior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "factor", factor = 0.6, sampleN = 58, sampleK = 0, expectedError = 0.025)
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.01, likelihood = "binomial", N = 1000, prior = jfaPrior)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 36)
  expect_equal(jfaRes$expectedSampleError, 0.36)
})

test_that(desc = "Bayesian poisson 3% materiality 1.3% errors sample prior", {
  jfaPrior <- auditPrior(materiality = 0.03, confidence = 0.95, method = "sample", sampleN = 58, sampleK = 0, expectedError = 0.025)
  jfaRes <- planning(materiality = 0.03, confidence = 0.95, expectedError = 0.013, likelihood = "binomial", N = 1000, prior = jfaPrior)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 140)
  expect_equal(jfaRes$expectedSampleError, 1.82)
})

test_that(desc = "Frequentist poisson 3% materiality 1.3% errors 5% min precision", {
  jfaRes <- planning(materiality = 0.03, confidence = 0.95, expectedError = 0.013, minPrecision = 0.05, likelihood = "binomial", N = 1000)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 303)
  expect_equal(jfaRes$expectedSampleError, 4)
})

test_that(desc = "Bayesian poisson 3% materiality 1.3% errors 5% min precision standard prior", {
  jfaRes <- planning(materiality = 0.03, confidence = 0.95, expectedError = 0.013, minPrecision = 0.05, likelihood = "binomial", N = 1000, prior = T)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 293)
  expect_equal(jfaRes$expectedSampleError, 3.81)
})

test_that(desc = "Bayesian poisson 100% materiality 0% errors 2% min precision standard prior", {
  jfaRes <- planning(confidence = 0.95, minPrecision = 0.02, likelihood = "poisson", prior = T)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 150)
  expect_equal(jfaRes$expectedSampleError, 0)
})

test_that(desc = "Bayesian poisson 100% materiality 1% errors 2% min precision standard prior", {
  jfaRes <- planning(confidence = 0.95, minPrecision = 0.02, expectedError = 0.01, likelihood = "poisson", prior = T)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(jfaRes$sampleSize, 220)
  expect_equal(jfaRes$expectedSampleError, 2.2)
})