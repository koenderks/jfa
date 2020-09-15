context("Prior")

# jfa version 0.1.0

test_that(desc = "Audit workflow", {
  set.seed(1)
  # Generate some audit data (N = 1000).
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), 
                           bookValue = runif(n = 1000, min = 100, max = 500))
  
  # Specify materiality, confidence, and expected errors.
  materiality <- 0.05
  confidence <- 0.95
  expectedError <- 0.025
  
  # Create a prior on the assessments of inherent risk (100%) and control risk (60%).
  ir <- 1
  cr <- 0.6
  
  # Create a beta prior distribution according to the Audit Risk Model (arm).
  prior <- auditPrior(materiality = materiality, confidence = confidence, 
                      method = "arm", ir = ir, cr = cr, 
                      expectedError = expectedError, likelihood = "binomial")
  print(prior)
  plot(prior)
  
  # Calculate the sample size according to the binomial distribution with the specified prior
  sampleSize <- planning(materiality = materiality, confidence = confidence, 
                         expectedError = expectedError, prior = prior, likelihood = "binomial")
  print(sampleSize)
  plot(sampleSize)
  
  # Draw sample using random record sampling
  sampleResult <- sampling(population = population, sampleSize = sampleSize, 
                           algorithm = "random", units = "records", seed = 1)
  
  sample <- sampleResult$sample
  sample$trueValue <- sample$bookValue
  sample$trueValue[2] <- sample$trueValue[2] - 500 # One overstatement is found
  
  # Evaluate the sample using the posterior distribution.
  conclusion <- evaluation(sample = sample, bookValues = "bookValue", auditValues = "trueValue", 
                           prior = prior, materiality = 0.05)
  print(conclusion)
  plot(conclusion)
  expect_equal(conclusion$confBound, 0.03784768)
})

# jfa version 0.2.0

test_that(desc = "None prior", {
  
  prior <- auditPrior(confidence = 0.95, method = "none", likelihood = "binomial")
  expect_equal(prior$aPrior, 1)
  expect_equal(prior$bPrior, 1)
  
  prior <- auditPrior(confidence = 0.95, method = "none", likelihood = "poisson")
  expect_equal(prior$aPrior, 1)
  expect_equal(prior$bPrior, 0)
  
  prior <- auditPrior(confidence = 0.95, method = "none", likelihood = "hypergeometric", N = 3500)
  expect_equal(prior$aPrior, 1)
  expect_equal(prior$bPrior, 1)
  
})

test_that(desc = "median prior", {
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", likelihood = "binomial")
  expect_equal(prior$aPrior, 1)
  expect_equal(prior$bPrior, 13.513)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", likelihood = "binomial", expectedError = 0.02)
  expect_equal(prior$aPrior, 1.4)
  expect_equal(prior$bPrior, 21.6)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "median", likelihood = "poisson", ir = 0.6, cr = 0.6)
  expect_equal(prior$aPrior, 1)
  expect_equal(prior$bPrior, 13.863)
  
})

test_that(desc = "hypotheses prior", {
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "hypotheses", likelihood = "binomial", pHmin = 0.3)
  expect_equal(prior$aPrior, 1)
  expect_equal(prior$bPrior, 6.954)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "hypotheses", likelihood = "poisson", pHmin = 0.3)
  expect_equal(prior$aPrior, 1)
  expect_equal(prior$bPrior, 7.133)
  
})

test_that(desc = "arm prior", {
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "arm", likelihood = "binomial", ir = 0.6, cr = 0.6)
  expect_equal(prior$aPrior, 1)
  expect_equal(prior$bPrior, 21)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "arm", likelihood = "poisson", ir = 0.6, cr = 0.6)
  expect_equal(prior$aPrior, 1)
  expect_equal(prior$bPrior, 20)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "arm", likelihood = "hypergeometric", ir = 0.6, cr = 0.6, N = 3500)
  expect_equal(prior$aPrior, 1)
  expect_equal(prior$bPrior, 20)
  
})

test_that(desc = "sample prior", {
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "sample", likelihood = "binomial", sampleN = 30, sampleK = 1)
  expect_equal(prior$aPrior, 2)
  expect_equal(prior$bPrior, 30)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "sample", likelihood = "poisson", sampleN = 30, sampleK = 1)
  expect_equal(prior$aPrior, 2)
  expect_equal(prior$bPrior, 30)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "sample", likelihood = "hypergeometric", sampleN = 30, sampleK = 1, N = 3500)
  expect_equal(prior$aPrior, 2)
  expect_equal(prior$bPrior, 30)
  
})

test_that(desc = "factor prior", {
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "factor", likelihood = "binomial", sampleN = 30, sampleK = 1, factor = 0.6)
  expect_equal(prior$aPrior, 1.6)
  expect_equal(prior$bPrior, 18.4)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "factor", likelihood = "poisson", sampleN = 30, sampleK = 1, factor = 0.6)
  expect_equal(prior$aPrior, 1.6)
  expect_equal(prior$bPrior, 18)
  
  prior <- auditPrior(materiality = 0.05, confidence = 0.95, method = "factor", likelihood = "hypergeometric", sampleN = 30, sampleK = 1, N = 3500, factor = 0.6)
  expect_equal(prior$aPrior, 1.6)
  expect_equal(prior$bPrior, 18.4)
  
})

# jfa version 0.3.0
# No changes to be tested