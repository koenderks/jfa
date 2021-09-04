context("Test consistency of function auditPrior()")

# jfa version 0.2.0

test_that(desc = "(id: f2-v0.1.0-t1) Test for method = 'none'", {
  prior <- auditPrior(conf.level = 0.95, likelihood = "binomial", method = "none")
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 1)
  
  prior <- auditPrior(conf.level = 0.95, likelihood = "poisson", method = "none")
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 0)
  
  prior <- auditPrior(conf.level = 0.95, likelihood = "hypergeometric", method = "none", N.units = 3500)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 1)
})

test_that(desc = "(id: f2-v0.1.0-t2) Test for method = 'median'", {
  prior <- auditPrior(conf.level = 0.95, likelihood = "binomial", method = "median", materiality = 0.05)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 13.51341, tolerance = 0.001)
  
  prior <- auditPrior(conf.level = 0.95, likelihood = "binomial", method = "median", expected = 0.02, materiality = 0.05)
  expect_equal(prior[["description"]]$alpha, 1.4114, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 21.1586, tolerance = 0.001)
  
  prior <- auditPrior(conf.level = 0.95, likelihood = "poisson", method = "median", materiality = 0.05)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 13.86294, tolerance = 0.001)  
})

test_that(desc = "(id: f2-v0.1.0-t3) Test for method = 'hypotheses'", {
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "hypotheses", likelihood = "binomial", p.hmin = 0.3)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 6.954, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "hypotheses", likelihood = "binomial", p.hmin = 1 - 0.3)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 23.47232, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "hypotheses", likelihood = "poisson", p.hmin = 0.3)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 7.133, tolerance = 0.001) 
})

test_that(desc = "(id: f2-v0.1.0-t4) Test for method = 'arm'", {
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "arm", likelihood = "binomial", ir = 0.6, cr = 0.6)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 21, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "arm", likelihood = "poisson", ir = 0.6, cr = 0.6)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 20, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "arm", likelihood = "hypergeometric", ir = 0.6, cr = 0.6, N.units = 3500)
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 21, tolerance = 0.001)
})

test_that(desc = "(id: f2-v0.1.0-t5) Test for method = 'sample'", {
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "sample", likelihood = "binomial", n = 30, x = 1)
  expect_equal(prior[["description"]]$alpha, 2)
  expect_equal(prior[["description"]]$beta, 30)
  
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "sample", likelihood = "poisson", n = 30, x = 1)
  expect_equal(prior[["description"]]$alpha, 2)
  expect_equal(prior[["description"]]$beta, 30)
  
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "sample", likelihood = "hypergeometric", n = 30, x = 1, N.units = 3500)
  expect_equal(prior[["description"]]$alpha, 2)
  expect_equal(prior[["description"]]$beta, 30)
})

test_that(desc = "(id: f2-v0.1.0-t6) Test for method = 'factor'", {
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "factor", likelihood = "binomial", n = 30, x = 1, factor = 0.6)
  expect_equal(prior[["description"]]$alpha, 1.6, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 18.4, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "factor", likelihood = "poisson", n = 30, x = 1, factor = 0.6)
  expect_equal(prior[["description"]]$alpha, 1.6, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 18, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "factor", likelihood = "hypergeometric", n = 30, x = 1, N.units = 3500, factor = 0.6)
  expect_equal(prior[["description"]]$alpha, 1.6, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 18.4, tolerance = 0.001)
})

# jfa version 0.3.0 - 0.3.1
# No changes to be benchmarked

# jfa version 0.4.0

test_that(desc = "(id: f2-v0.4.0-t1) Test for method = 'median' with expected errors > 0", {
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "median", likelihood = "binomial", expected = 0.01)
  expect_equal(prior[["description"]]$alpha, 1.1554, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 16.3846, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "median", likelihood = "binomial", expected = 0.025)
  expect_equal(prior[["description"]]$alpha, 1.6146, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 24.9694, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "median", likelihood = "poisson", expected = 0.01)
  expect_equal(prior[["description"]]$alpha, 1.1722, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 17.22, tolerance = 0.001)
  
  prior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "median", likelihood = "poisson", expected = 0.025)
  expect_equal(prior[["description"]]$alpha, 1.681, tolerance = 0.001)
  expect_equal(prior[["description"]]$beta, 27.24, tolerance = 0.001)
})

# jfa version 0.5.0

test_that(desc = "(id: f2-v0.5.0-t1) Test for summary and print function", {
  prior <- auditPrior(conf.level = 0.95, likelihood = "binomial", method = "none")
  invisible(capture.output(print(prior)))
  invisible(capture.output(summary(prior)))
  expect_equal(prior[["description"]]$alpha, 1)
  expect_equal(prior[["description"]]$beta, 1)
})

test_that(desc = "(id: f2-v0.5.0-t2) Test for plot function", {
  prior <- auditPrior(conf.level = 0.95, likelihood = "binomial", method = "none", materiality = 0.05)
  invisible(capture.output(plot(prior)))
  expect_equal(prior[["description"]]$alpha, 1)
  
  prior <- auditPrior(conf.level = 0.95, likelihood = "hypergeometric", method = "none", N.units = 1000)
  invisible(capture.output(plot(prior)))
  expect_equal(prior[["description"]]$alpha, 1)
  
  prior <- auditPrior(conf.level = 0.95, likelihood = "poisson", method = "none", materiality = 0.05)
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
  expected <- expectedMisstatement / N
  prior <- auditPrior(conf.level = 0.95, materiality = theta, likelihood = "binomial", method = "bram", expected = expected, ub = ub)
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
  expected <- expectedMisstatement / N
  prior <- auditPrior(conf.level = 0.95, materiality = theta, likelihood = "poisson", method = "bram", expected = expected, ub = ub)
  expect_equal(prior[["description"]]$alpha, 1.20259, tolerance = 0.00001)
  expect_equal(prior[["description"]]$beta, 13.50597, tolerance = 0.00001)
})

# jfa version 0.5.4 - 0.5.7
# No changes to be benchmarked

# jfa version 0.6.0

test_that(desc = "(id: f2-v0.6.0-t1) Test for custom method binomial", {
  prior <- auditPrior(materiality = 0.05, likelihood = "binomial", method = "custom", alpha = 5, beta = 10)
  expect_equal(prior[["description"]]$alpha, 5)
  expect_equal(prior[["description"]]$beta, 10)
})

test_that(desc = "(id: f2-v0.6.0-t2) Test for custom method poisson", {
  prior <- auditPrior(materiality = 0.05, likelihood = "poisson", method = "custom", alpha = 5, beta = 10)
  expect_equal(prior[["description"]]$alpha, 5)
  expect_equal(prior[["description"]]$beta, 10)
})

test_that(desc = "(id: f2-v0.6.0-t1) Test for custom method hypergeometric", {
  prior <- auditPrior(materiality = 0.05, likelihood = "hypergeometric", method = "custom", alpha = 5, beta = 10, N = 100)
  expect_equal(prior[["description"]]$alpha, 5)
  expect_equal(prior[["description"]]$beta, 10)
})