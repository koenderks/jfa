context("1. Consistency test for function auditPrior()")

# jfa version 0.2.0

test_that(desc = "(id: f4-v0.1.0-t1) Test for method = 'none'", {
  prior <- auditPrior(confidence = 0.95, likelihood = "binomial", method = "none")
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 1)
  
  prior <- auditPrior(confidence = 0.95, likelihood = "poisson", method = "none")
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 0)
  
  prior <- auditPrior(confidence = 0.95, likelihood = "hypergeometric", method = "none", N = 3500)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 1)
})

test_that(desc = "(id: f4-v0.1.0-t2) Test for method = 'median'", {
  prior <- auditPrior(confidence = 0.95, likelihood = "binomial", method = "median", materiality = 0.05)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 13.513, tolerance = 0.001)
  
  prior <- auditPrior(confidence = 0.95, likelihood = "binomial", method = "median", expectedError = 0.02, materiality = 0.05)
  expect_equal(prior[["description"]]$alpha, 1.4, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 20.6, tolerance = 0.001)
  
  prior <- auditPrior(confidence = 0.95, likelihood = "poisson", method = "median", ir = 0.6, cr = 0.6, materiality = 0.05, )
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 13.863, tolerance = 0.001)  
})

test_that(desc = "(id: f4-v0.1.0-t3) Test for method = 'hypotheses'", {
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "hypotheses", likelihood = "binomial", pHmin = 0.3)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 6.954, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "hypotheses", likelihood = "binomial", pHplus = 0.3)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 23.47232, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "hypotheses", likelihood = "poisson", pHmin = 0.3)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 7.133, tolerance = 0.001) 
})

test_that(desc = "(id: f4-v0.1.0-t4) Test for method = 'arm'", {
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "arm", likelihood = "binomial", ir = 0.6, cr = 0.6)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 21, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "arm", likelihood = "poisson", ir = 0.6, cr = 0.6)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 20, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "arm", likelihood = "hypergeometric", ir = 0.6, cr = 0.6, N = 3500)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 20, tolerance = 0.001)
})

test_that(desc = "(id: f4-v0.1.0-t5) Test for method = 'sample'", {
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "sample", likelihood = "binomial", sampleN = 30, sampleK = 1)
  expect_equal(prior[["description"]]$alpha, 2)
  expect_equal(prior[["description"]]$beta, 30)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "sample", likelihood = "poisson", sampleN = 30, sampleK = 1)
  expect_equal(prior[["description"]]$alpha, 2)
  expect_equal(prior[["description"]]$beta, 30)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "sample", likelihood = "hypergeometric", sampleN = 30, sampleK = 1, N = 3500)
  expect_equal(prior[["description"]]$alpha, 2)
  expect_equal(prior[["description"]]$beta, 30)
})

test_that(desc = "(id: f4-v0.1.0-t6) Test for method = 'factor'", {
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "factor", likelihood = "binomial", sampleN = 30, sampleK = 1, factor = 0.6)
  expect_equal(prior[["description"]]$alpha, 1.6, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 18.4, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "factor", likelihood = "poisson", sampleN = 30, sampleK = 1, factor = 0.6)
  expect_equal(prior[["description"]]$alpha, 1.6, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 18, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "factor", likelihood = "hypergeometric", sampleN = 30, sampleK = 1, N = 3500, factor = 0.6)
  expect_equal(prior[["description"]]$alpha, 1.6, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 18.4, tolerance = 0.001)
})

# jfa version 0.3.0
# No changes to be benchmarked

# jfa version 0.3.1
# No changes to be benchmarked

# jfa version 0.4.0

test_that(desc = "(id: f4-v0.4.0-t1) Test for method = 'median' with expected errors > 0", {
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", likelihood = "binomial", expectedError = 0.01)
  expect_equal(prior[["description"]]$alpha, 1.15, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 15.85, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", likelihood = "binomial", expectedError = 0.025)
  expect_equal(prior[["description"]]$alpha, 1.6, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 24.4, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", likelihood = "poisson", expectedError = 0.01)
  expect_equal(prior[["description"]]$alpha, 1.171, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 17.1, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", likelihood = "poisson", expectedError = 0.025)
  expect_equal(prior[["description"]]$alpha, 1.668, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 26.72, tolerance = 0.001)
})

# jfa version 0.5.0

test_that(desc = "(id: f4-v0.5.0-t1) Test for print function", {
  prior <- auditPrior(confidence = 0.95, likelihood = "binomial", method = "none")
  invisible(capture.output(print(prior)))
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 1)
})

test_that(desc = "(id: f4-v0.5.0-t2) Test for plot function", {
  prior <- auditPrior(confidence = 0.95, likelihood = "binomial", method = "none", materiality = 0.05)
  invisible(capture.output(plot(prior)))
  expect_equal(prior[["description"]]$alpha, 1)
  
  prior <- auditPrior(confidence = 0.95, likelihood = "hypergeometric", method = "none", N = 1000)
  invisible(capture.output(plot(prior)))
  expect_equal(prior[["description"]]$alpha, 1)
  
  prior <- auditPrior(confidence = 0.95, likelihood = "poisson", method = "none", materiality = 0.05)
  invisible(capture.output(plot(prior)))
  expect_equal(prior[["description"]]$alpha, 1)
})

# jfa version 0.5.1
# No changes to be benchmarked

# jfa version 0.5.2
# No changes to be benchmarked