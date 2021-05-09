context("4. Test consistency of function planning()")

# jfa version 0.1.0

test_that(desc = "(id: f6-v0.1.0-t1) Frequentist Poisson 1% materiality", {
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "poisson")
  expect_equal(jfaRes[["sampleSize"]], 300)
  expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: f6-v0.1.0-t2) Frequentist Poisson 5% materiality", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, likelihood = "poisson")
  expect_equal(jfaRes[["sampleSize"]], 60)
  expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: f6-v0.1.0-t3) Frequentist Poisson 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "poisson")
  expect_equal(jfaRes[["sampleSize"]], 231)
  expect_equal(jfaRes[["expectedSampleError"]], 5.78, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.1.0-t4) Frequentist Poisson 5% materiality 2% errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 2, likelihood = "poisson")
  expect_equal(jfaRes[["sampleSize"]], 126)
  expect_equal(jfaRes[["expectedSampleError"]], 2)
})

test_that(desc = "(id: f6-v0.1.0-t5) Bayesian Poisson 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "poisson", prior = TRUE)
  expect_equal(jfaRes[["sampleSize"]], 231)
  expect_equal(jfaRes[["expectedSampleError"]], 5.78, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.1.0-t6) Bayesian Poisson 5% materiality 2.5% errors custom prior", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "poisson", prior = TRUE, kPrior = 1, nPrior = 7)
  expect_equal(jfaRes[["sampleSize"]], 285)
  expect_equal(jfaRes[["expectedSampleError"]], 7.12, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.1.0-t7) Frequentist binomial 1% materiality", {
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "binomial")
  expect_equal(jfaRes[["sampleSize"]], 299)
  expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: f6-v0.1.0-t8) Frequentist binomial 5% materiality", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, likelihood = "binomial")
  expect_equal(jfaRes[["sampleSize"]], 59)
  expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: f6-v0.1.0-t9) Frequentist binomial 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "binomial")
  expect_equal(jfaRes[["sampleSize"]], 234)
  expect_equal(jfaRes[["expectedSampleError"]], 6, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.1.0-t10) Frequentist binomial 5% materiality 2 errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 2, likelihood = "binomial")
  expect_equal(jfaRes[["sampleSize"]], 124)
  expect_equal(jfaRes[["expectedSampleError"]], 2)
})

test_that(desc = "(id: f6-v0.1.0-t11) Bayesian binomial 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "binomial", prior = TRUE)
  expect_equal(jfaRes[["sampleSize"]], 220)
  expect_equal(jfaRes[["expectedSampleError"]], 5.5, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.1.0-t12) Bayesian binomial 5% materiality 2.5% errors custom prior", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "binomial", prior = TRUE, kPrior = 1, nPrior = 7)
  expect_equal(jfaRes[["sampleSize"]], 273)
})

test_that(desc = "(id: f6-v0.1.0-t13) Frequentist hypergeometric 1% materiality", {
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "hypergeometric", N = 1000)
  expect_equal(jfaRes[["sampleSize"]], 258)
  expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: f6-v0.1.0-t14) Frequentist hypergeometric 5% materiality", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, likelihood = "hypergeometric", N = 1000)
  expect_equal(jfaRes[["sampleSize"]], 57)
  expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: f6-v0.1.0-t15) Frequentist hypergeometric 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "hypergeometric", N = 1000)
  expect_equal(jfaRes[["sampleSize"]], 197)
  expect_equal(jfaRes[["expectedSampleError"]], 5, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.1.0-t16) Frequentist hypergeometric 5% materiality 2 errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 2, likelihood = "hypergeometric", N = 1000)
  expect_equal(jfaRes[["sampleSize"]], 119)
  expect_equal(jfaRes[["expectedSampleError"]], 2)
})

test_that(desc = "(id: f6-v0.1.0-t17) Bayesian hypergeometric 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "hypergeometric", N = 1000, prior = TRUE)
  expect_equal(jfaRes[["sampleSize"]], 182)
  expect_equal(jfaRes[["expectedSampleError"]], 5, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.1.0-t18) Bayesian hypergeometric 5% materiality 2.5% errors custom prior", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "hypergeometric", N = 1000, prior = TRUE, kPrior = 1, nPrior = 7)
  expect_equal(jfaRes[["sampleSize"]], 196)
  expect_equal(jfaRes[["expectedSampleError"]], 5, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.1.0-t19) Bayesian binomial 2% precision 5% materiality 2.5% errors median prior", {
  jfaPrior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", expectedError = 0.025)
  jfaRes <- planning(materiality = 0.05, minPrecision = 0.02, confidence = 0.95, expectedError = 0.025, likelihood = "binomial", N = 1000, prior = jfaPrior)
  expect_equal(jfaRes[["sampleSize"]], 284)
  expect_equal(jfaRes[["expectedSampleError"]], 7.1, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.1.0-t20) Bayesian binomial 5% materiality 1% errors hypotheses prior", {
  jfaPrior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "hypotheses", pHmin = 0.7)
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.01, likelihood = "binomial", N = 1000, prior = jfaPrior)
  expect_equal(jfaRes[["sampleSize"]], 55)
  expect_equal(jfaRes[["expectedSampleError"]], 0.55, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.1.0-t21) Bayesian poisson 5% materiality 1% errors factor prior", {
  jfaPrior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "factor", factor = 0.6, sampleN = 58, sampleK = 0, expectedError = 0.025)
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.01, likelihood = "binomial", N = 1000, prior = jfaPrior)
  expect_equal(jfaRes[["sampleSize"]], 36)
  expect_equal(jfaRes[["expectedSampleError"]], 0.36, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.1.0-t22) Bayesian poisson 3% materiality 1.3% errors sample prior", {
  jfaPrior <- auditPrior(materiality = 0.03, confidence = 0.95, method = "sample", sampleN = 58, sampleK = 0, expectedError = 0.025)
  jfaRes <- planning(materiality = 0.03, confidence = 0.95, expectedError = 0.013, likelihood = "binomial", N = 1000, prior = jfaPrior)
  expect_equal(jfaRes[["sampleSize"]], 140)
  expect_equal(jfaRes[["expectedSampleError"]], 1.82, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.1.0-t23) Frequentist poisson 3% materiality 1.3% errors 5% min precision", {
  jfaRes <- planning(materiality = 0.03, confidence = 0.95, expectedError = 0.013, minPrecision = 0.05, likelihood = "binomial", N = 1000)
  expect_equal(jfaRes[["sampleSize"]], 303)
  expect_equal(jfaRes[["expectedSampleError"]], 4, tolerance = 0.001)
})

# jfa version 0.2.0

test_that(desc = "(id: f6-v0.2.0-t1) Bayesian poisson 3% materiality 1.3% errors 5% min precision standard prior", {
  jfaRes <- planning(materiality = 0.03, confidence = 0.95, expectedError = 0.013, minPrecision = 0.05, likelihood = "binomial", N = 1000, prior = T)
  expect_equal(jfaRes[["sampleSize"]], 293)
  expect_equal(jfaRes$expectedSampleError, 3.81, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.2.0-t2) Bayesian poisson 100% materiality 0% errors 2% min precision standard prior", {
  jfaRes <- planning(confidence = 0.95, minPrecision = 0.02, likelihood = "poisson", prior = TRUE)
  expect_equal(jfaRes[["sampleSize"]], 150)
  expect_equal(jfaRes$expectedSampleError, 0)
})

test_that(desc = "(id: f6-v0.2.0-t3) Bayesian poisson 100% materiality 1% errors 2% min precision standard prior", {
  jfaRes <- planning(confidence = 0.95, minPrecision = 0.02, expectedError = 0.01, likelihood = "poisson", prior = T)
  expect_equal(jfaRes[["sampleSize"]], 220)
  expect_equal(jfaRes$expectedSampleError, 2.2, tolerance = 0.001)
})

# jfa version 0.3.0
# No changes to be benchmarked

# jfa version 0.3.1
# No changes to be benchmarked

# jfa version 0.4.0

test_that(desc = "(id: f6-v0.4.0-t1) Expected Bayes factors for zero expected errors", {
  jfaRes <- planning(confidence = 0.95, materiality = 0.02, expectedError = 0, likelihood = "poisson", prior = T)
  expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, Inf, tolerance = 0.001)
  
  jfaRes <- planning(confidence = 0.95, materiality = 0.02, expectedError = 0, likelihood = "binomial", prior = T)
  expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, 945.2848, tolerance = 0.001)
  
  jfaRes <- planning(confidence = 0.95, materiality = 0.02, expectedError = 0, likelihood = "hypergeometric", prior = T, N = 1000)
  expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, 1247.05, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.4.0-t2) Expected Bayes factors for expected errors > 0", {
  jfaRes <- planning(confidence = 0.95, materiality = 0.02, expectedError = 0.01, likelihood = "poisson", prior = T)
  expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, Inf, tolerance = 0.001)
  
  jfaRes <- planning(confidence = 0.95, materiality = 0.02, expectedError = 0.01, likelihood = "binomial", prior = T)
  expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, 933.3458, tolerance = 0.001)
  
  jfaRes <- planning(confidence = 0.95, materiality = 0.02, expectedError = 0.01, likelihood = "hypergeometric", prior = T, N = 1000)
  expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, 1619.812, tolerance = 0.001)
})

test_that(desc = "(id: f6-v0.4.0-t3) Expected Bayes factors for median priors", {
  prior <- auditPrior(materiality = 0.02, confidence = 0.95, method = "median", likelihood = "poisson")
  jfaRes <- planning(confidence = 0.95, materiality = 0.02, prior = prior)
  expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, 19.35135, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.02, confidence = 0.95, method = "median", likelihood = "binomial")
  jfaRes <- planning(confidence = 0.95, materiality = 0.02, prior = prior)
  expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, 19.01047, tolerance = 0.001)
})

# jfa version 0.5.0

test_that(desc = "(id: f6-v0.5.0-t1) Test for frequentist print function", {
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "poisson")
  invisible(capture.output(print(jfaRes)))
  expect_equal(jfaRes[["sampleSize"]], 300)
  expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: f6-v0.5.0-t2) Test for Bayesian print function", {
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "poisson", prior = TRUE)
  invisible(capture.output(print(jfaRes)))
  invisible(capture.output(print(jfaRes[["expectedPosterior"]])))
  expect_equal(jfaRes[["sampleSize"]], 300)
  expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: f6-v0.5.0-t3) Test for frequentist plot function", {
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "poisson")
  invisible(capture.output(plot(jfaRes)))
  expect_equal(jfaRes[["expectedSampleError"]], 0)
  
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "binomial")
  invisible(capture.output(plot(jfaRes)))
  expect_equal(jfaRes[["expectedSampleError"]], 0)
  
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "hypergeometric", N = 1000)
  invisible(capture.output(plot(jfaRes)))
  expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: f6-v0.5.0-t4) Test for Bayesian plot function", {
  jfaRes <- planning(minPrecision = 0.02, confidence = 0.95, expectedError = 0, likelihood = "poisson", prior = TRUE)
  invisible(capture.output(plot(jfaRes)))
  invisible(capture.output(plot(jfaRes[["expectedPosterior"]])))
  expect_equal(jfaRes[["expectedSampleError"]], 0)
  
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "binomial", prior = TRUE)
  invisible(capture.output(plot(jfaRes)))
  invisible(capture.output(plot(jfaRes[["expectedPosterior"]])))
  expect_equal(jfaRes[["expectedSampleError"]], 0)
  
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "hypergeometric", prior = TRUE, N = 1000)
  invisible(capture.output(plot(jfaRes)))
  invisible(capture.output(plot(jfaRes[["expectedPosterior"]])))
  expect_equal(jfaRes[["expectedSampleError"]], 0)
})

# jfa version 0.5.1
# No changes to be benchmarked

# jfa version 0.5.2

test_that(desc = "(id: f6-v0.5.2-t1) Test for change in Hypergeometric mode calculation", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, likelihood = "hypergeometric", N = 10000)
  modeDist <- ceiling((jfaRes[["expectedBound"]] - jfaRes[["expectedPrecision"]]) * 10000)
  expect_equal(jfaRes[["sampleSize"]], 59)
  expect_equal(modeDist, 3)
})

test_that(desc = "(id: f6-v0.5.2-t2) Test for change in beta-binomial mode calculation", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, likelihood = "hypergeometric", N = 10000, prior = T)
  modeDist <- ceiling((jfaRes[["expectedBound"]] - jfaRes[["expectedPrecision"]]) * 10000)
  expect_equal(jfaRes[["sampleSize"]], 58)
  expect_equal(modeDist, 0)
})

# jfa version 0.5.3
# No changes to be benchmarked