context("6. Function test for planning()")

# jfa version 0.1.0

test_that(desc = "(id: 6.1) Frequentist Poisson 1% materiality", {
	jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "poisson")
	expect_equal(jfaRes[["sampleSize"]], 300)
	expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: 6.2) Frequentist Poisson 5% materiality", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, likelihood = "poisson")
	expect_equal(jfaRes[["sampleSize"]], 60)
	expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: 6.3) Frequentist Poisson 5% materiality 2.5% errors", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "poisson")
	expect_equal(jfaRes[["sampleSize"]], 231)
	expect_equal(jfaRes[["expectedSampleError"]], 5.78, tolerance = 0.001)
})

test_that(desc = "(id: 6.4) Frequentist Poisson 5% materiality 2% errors", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 2, likelihood = "poisson")
	expect_equal(jfaRes[["sampleSize"]], 126)
	expect_equal(jfaRes[["expectedSampleError"]], 2)
})

test_that(desc = "(id: 6.5) Bayesian Poisson 5% materiality 2.5% errors", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "poisson", prior = TRUE)
	expect_equal(jfaRes[["sampleSize"]], 231)
	expect_equal(jfaRes[["expectedSampleError"]], 5.78, tolerance = 0.001)
})

test_that(desc = "(id: 6.6) Bayesian Poisson 5% materiality 2.5% errors custom prior", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "poisson", prior = TRUE, kPrior = 1, nPrior = 7)
	expect_equal(jfaRes[["sampleSize"]], 285)
	expect_equal(jfaRes[["expectedSampleError"]], 7.12, tolerance = 0.001)
})

test_that(desc = "(id: 6.7) Frequentist binomial 1% materiality", {
	jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "binomial")
	expect_equal(jfaRes[["sampleSize"]], 299)
	expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: 6.8) Frequentist binomial 5% materiality", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, likelihood = "binomial")
	expect_equal(jfaRes[["sampleSize"]], 59)
	expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: 6.9) Frequentist binomial 5% materiality 2.5% errors", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "binomial")
	expect_equal(jfaRes[["sampleSize"]], 234)
	expect_equal(jfaRes[["expectedSampleError"]], 6, tolerance = 0.001)
})

test_that(desc = "(id: 6.10) Frequentist binomial 5% materiality 2 errors", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 2, likelihood = "binomial")
	expect_equal(jfaRes[["sampleSize"]], 124)
	expect_equal(jfaRes[["expectedSampleError"]], 2)
})

test_that(desc = "(id: 6.11) Bayesian binomial 5% materiality 2.5% errors", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "binomial", prior = TRUE)
	expect_equal(jfaRes[["sampleSize"]], 220)
	expect_equal(jfaRes[["expectedSampleError"]], 5.5, tolerance = 0.001)
})

test_that(desc = "(id: 6.12) Bayesian binomial 5% materiality 2.5% errors custom prior", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "binomial", prior = TRUE, kPrior = 1, nPrior = 7)
	expect_equal(jfaRes[["sampleSize"]], 273)
})

test_that(desc = "(id: 6.13) Frequentist hypergeometric 1% materiality", {
	jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "hypergeometric", N = 1000)
	expect_equal(jfaRes[["sampleSize"]], 258)
	expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: 6.14) Frequentist hypergeometric 5% materiality", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, likelihood = "hypergeometric", N = 1000)
	expect_equal(jfaRes[["sampleSize"]], 57)
	expect_equal(jfaRes[["expectedSampleError"]], 0)
})

test_that(desc = "(id: 6.15) Frequentist hypergeometric 5% materiality 2.5% errors", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "hypergeometric", N = 1000)
	expect_equal(jfaRes[["sampleSize"]], 197)
	expect_equal(jfaRes[["expectedSampleError"]], 5, tolerance = 0.001)
})

test_that(desc = "(id: 6.16) Frequentist hypergeometric 5% materiality 2 errors", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 2, likelihood = "hypergeometric", N = 1000)
	expect_equal(jfaRes[["sampleSize"]], 119)
	expect_equal(jfaRes[["expectedSampleError"]], 2)
})

test_that(desc = "(id: 6.17) Bayesian hypergeometric 5% materiality 2.5% errors", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "hypergeometric", N = 1000, prior = TRUE)
	expect_equal(jfaRes[["sampleSize"]], 160)
	expect_equal(jfaRes[["expectedSampleError"]], 4, tolerance = 0.001)
})

test_that(desc = "(id: 6.18) Bayesian hypergeometric 5% materiality 2.5% errors custom prior", {
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.025, likelihood = "hypergeometric", N = 1000, prior = TRUE, kPrior = 1, nPrior = 7)
	expect_equal(jfaRes[["sampleSize"]], 193)
	expect_equal(jfaRes[["expectedSampleError"]], 5, tolerance = 0.001)
})

test_that(desc = "(id: 6.19) Bayesian binomial 2% precision 5% materiality 2.5% errors median prior", {
	jfaPrior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", expectedError = 0.025)
	jfaRes <- planning(materiality = 0.05, minPrecision = 0.02, confidence = 0.95, expectedError = 0.025, likelihood = "binomial", N = 1000, prior = jfaPrior)
	expect_equal(jfaRes[["sampleSize"]], 285)
	expect_equal(jfaRes[["expectedSampleError"]], 7.12, tolerance = 0.001)
})

test_that(desc = "(id: 6.20) Bayesian binomial 5% materiality 1% errors hypotheses prior", {
	jfaPrior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "hypotheses", pHmin = 0.7)
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.01, likelihood = "binomial", N = 1000, prior = jfaPrior)
	expect_equal(jfaRes[["sampleSize"]], 55)
	expect_equal(jfaRes[["expectedSampleError"]], 0.55, tolerance = 0.001)
})

test_that(desc = "(id: 6.21) Bayesian poisson 5% materiality 1% errors factor prior", {
	jfaPrior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "factor", factor = 0.6, sampleN = 58, sampleK = 0, expectedError = 0.025)
	jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0.01, likelihood = "binomial", N = 1000, prior = jfaPrior)
	expect_equal(jfaRes[["sampleSize"]], 36)
	expect_equal(jfaRes[["expectedSampleError"]], 0.36, tolerance = 0.001)
})

test_that(desc = "(id: 6.22) Bayesian poisson 3% materiality 1.3% errors sample prior", {
	jfaPrior <- auditPrior(materiality = 0.03, confidence = 0.95, method = "sample", sampleN = 58, sampleK = 0, expectedError = 0.025)
	jfaRes <- planning(materiality = 0.03, confidence = 0.95, expectedError = 0.013, likelihood = "binomial", N = 1000, prior = jfaPrior)
	expect_equal(jfaRes[["sampleSize"]], 140)
	expect_equal(jfaRes[["expectedSampleError"]], 1.82, tolerance = 0.001)
})

test_that(desc = "(id: 6.23) Frequentist poisson 3% materiality 1.3% errors 5% min precision", {
	jfaRes <- planning(materiality = 0.03, confidence = 0.95, expectedError = 0.013, minPrecision = 0.05, likelihood = "binomial", N = 1000)
	expect_equal(jfaRes[["sampleSize"]], 303)
	expect_equal(jfaRes[["expectedSampleError"]], 4, tolerance = 0.001)
})

# jfa version 0.2.0

test_that(desc = "(id: 6.24) Bayesian poisson 3% materiality 1.3% errors 5% min precision standard prior", {
	jfaRes <- planning(materiality = 0.03, confidence = 0.95, expectedError = 0.013, minPrecision = 0.05, likelihood = "binomial", N = 1000, prior = T)
	expect_equal(jfaRes[["sampleSize"]], 293)
	expect_equal(jfaRes$expectedSampleError, 3.81, tolerance = 0.001)
})

test_that(desc = "(id: 6.25) Bayesian poisson 100% materiality 0% errors 2% min precision standard prior", {
	jfaRes <- planning(confidence = 0.95, minPrecision = 0.02, likelihood = "poisson", prior = TRUE)
	expect_equal(jfaRes[["sampleSize"]], 150)
	expect_equal(jfaRes$expectedSampleError, 0)
})

test_that(desc = "(id: 6.26) Bayesian poisson 100% materiality 1% errors 2% min precision standard prior", {
	jfaRes <- planning(confidence = 0.95, minPrecision = 0.02, expectedError = 0.01, likelihood = "poisson", prior = T)
	expect_equal(jfaRes[["sampleSize"]], 220)
	expect_equal(jfaRes$expectedSampleError, 2.2, tolerance = 0.001)
})

# jfa version 0.3.0
# No changes to be tested

# jfa version 0.3.1
# No changes to be tested

# jfa version 0.4.0

test_that(desc = "(id: 6.27) Expected Bayes factors for zero expected errors", {
	jfaRes <- planning(confidence = 0.95, materiality = 0.02, expectedError = 0, likelihood = "poisson", prior = T)
	expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, Inf, tolerance = 0.001)
	
	jfaRes <- planning(confidence = 0.95, materiality = 0.02, expectedError = 0, likelihood = "binomial", prior = T)
	expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, 945.2848, tolerance = 0.001)
	
	jfaRes <- planning(confidence = 0.95, materiality = 0.02, expectedError = 0, likelihood = "hypergeometric", prior = T, N = 1000)
	expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, 676.4327, tolerance = 0.001)
})

test_that(desc = "(id: 6.28) Expected Bayes factors for expected errors > 0", {
	jfaRes <- planning(confidence = 0.95, materiality = 0.02, expectedError = 0.01, likelihood = "poisson", prior = T)
	expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, Inf, tolerance = 0.001)
	
	jfaRes <- planning(confidence = 0.95, materiality = 0.02, expectedError = 0.01, likelihood = "binomial", prior = T)
	expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, 933.3458, tolerance = 0.001)
	
	jfaRes <- planning(confidence = 0.95, materiality = 0.02, expectedError = 0.01, likelihood = "hypergeometric", prior = T, N = 1000)
	expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, 191.7564, tolerance = 0.001)
})

test_that(desc = "(id: 6.29) Expected Bayes factors for median priors", {
	prior <- auditPrior(materiality = 0.02, confidence = 0.95, method = "median", likelihood = "poisson")
	jfaRes <- planning(confidence = 0.95, materiality = 0.02, prior = prior)
	expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, 19.35135, tolerance = 0.001)
	
	prior <- auditPrior(materiality = 0.02, confidence = 0.95, method = "median", likelihood = "binomial")
	jfaRes <- planning(confidence = 0.95, materiality = 0.02, prior = prior)
	expect_equal(jfaRes[["expectedPosterior"]][["hypotheses"]]$expectedBf, 19.01047, tolerance = 0.001)
})

# jfa version 0.5.0
# No changes to be tested