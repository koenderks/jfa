context("2. Test consistency of function auditPrior()")

# jfa version 0.2.0

test_that(desc = "(id: f2-v0.1.0-t1) Test for method = 'none'", {
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

test_that(desc = "(id: f2-v0.1.0-t2) Test for method = 'median'", {
  prior <- auditPrior(confidence = 0.95, likelihood = "binomial", method = "median", materiality = 0.05)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 13.51341, tolerance = 0.001)
  
  prior <- auditPrior(confidence = 0.95, likelihood = "binomial", method = "median", expectedError = 0.02, materiality = 0.05)
  expect_equal(prior[["description"]]$alpha, 1.4114, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 21.1586, tolerance = 0.001)
  
  prior <- auditPrior(confidence = 0.95, likelihood = "poisson", method = "median", materiality = 0.05)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 13.86294, tolerance = 0.001)  
})

test_that(desc = "(id: f2-v0.1.0-t3) Test for method = 'hypotheses'", {
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

test_that(desc = "(id: f2-v0.1.0-t4) Test for method = 'arm'", {
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "arm", likelihood = "binomial", ir = 0.6, cr = 0.6)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 21, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "arm", likelihood = "poisson", ir = 0.6, cr = 0.6)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 20, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "arm", likelihood = "hypergeometric", ir = 0.6, cr = 0.6, N = 3500)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 21, tolerance = 0.001)
})

test_that(desc = "(id: f2-v0.1.0-t5) Test for method = 'sample'", {
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

test_that(desc = "(id: f2-v0.1.0-t6) Test for method = 'factor'", {
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

# jfa version 0.3.0 - 0.3.1
# No changes to be benchmarked

# jfa version 0.4.0

test_that(desc = "(id: f2-v0.4.0-t1) Test for method = 'median' with expected errors > 0", {
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", likelihood = "binomial", expectedError = 0.01)
  expect_equal(prior[["description"]]$alpha, 1.1554, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 16.3846, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", likelihood = "binomial", expectedError = 0.025)
  expect_equal(prior[["description"]]$alpha, 1.6146, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 24.9694, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", likelihood = "poisson", expectedError = 0.01)
  expect_equal(prior[["description"]]$alpha, 1.1722, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 17.22, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", likelihood = "poisson", expectedError = 0.025)
  expect_equal(prior[["description"]]$alpha, 1.681, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 27.24, tolerance = 0.001)
})

# jfa version 0.5.0

test_that(desc = "(id: f2-v0.5.0-t1) Test for summary and print function", {
  prior <- auditPrior(confidence = 0.95, likelihood = "binomial", method = "none")
  invisible(capture.output(print(prior)))
  invisible(capture.output(summary(prior)))
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 1)
})

test_that(desc = "(id: f2-v0.5.0-t2) Test for plot function", {
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

# jfa version 0.5.1 - 0.5.2
# No changes to be benchmarked

# jfa version 0.5.3

test_that(desc = "(id: f2-v0.5.3-t1) Test for bram method binomial", {
  N <- 20000
  materiality <- 2000
  expectedMisstatement <- 300
  expectedUpperBound <- 5000
  ub <- expectedUpperBound / N
  theta <- materiality / N
  expectedError <- expectedMisstatement / N
  prior <- auditPrior(confidence = 0.95, materiality = theta, likelihood = "binomial", method = "bram", expectedError = expectedError, ub = ub)
  expect_equal(prior[["description"]]$alpha, 1.1581)
  expect_equal(prior[["description"]]$beta, 11.3819)
})

test_that(desc = "(id: f2-v0.5.3-t2) Test for bram method poisson", {
  N <- 20000
  materiality <- 2000
  expectedMisstatement <- 300
  expectedUpperBound <- 5000
  ub <- expectedUpperBound / N
  theta <- materiality / N
  expectedError <- expectedMisstatement / N
  prior <- auditPrior(confidence = 0.95, materiality = theta, likelihood = "poisson", method = "bram", expectedError = expectedError, ub = ub)
  expect_equal(prior[["description"]]$alpha, 1.20259, tolerance = 0.00001)
  expect_equal(prior[["description"]]$beta, 13.50597, tolerance = 0.00001)
})

# jfa version 0.5.4 - 0.5.7
# No changes to be benchmarked