context("8. Test consistency of workflow functionality")

# jfa version 0.1.0

test_that(desc = "(id: f8-v0.1.0-t1) Test for workflow elements", {
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
  
  # Calculate the sample size according to the binomial distribution with the specified prior
  sampleSize <- planning(materiality = materiality, confidence = confidence, 
                         expectedError = expectedError, prior = prior, likelihood = "binomial")
  
  # Draw sample using random record sampling
  sampleResult <- selection(population = population, sampleSize = sampleSize, 
                            algorithm = "random", units = "records", seed = 1)
  
  sample <- sampleResult$sample
  sample$trueValue <- sample$bookValue
  sample$trueValue[2] <- sample$trueValue[2] - 0.5 * sample$trueValue[2] # One overstatement is found
  
  # Evaluate the sample using the posterior distribution.
  conclusion <- evaluation(confidence = confidence, sample = sample, bookValues = "bookValue", auditValues = "trueValue", 
                           prior = prior, materiality = 0.05)
  expect_equal(conclusion[["confBound"]], 0.02669982, tolerance = 0.001)
})

test_that(desc = "(id: f8-v0.1.0-t1) Test for use of jfaPrior and jfaPosterior", {
  
  confidence <- 0.90 # 90% confidence
  tolerance  <- 0.05 # 5% tolerance (materiality)
  
  # Construct a prior distribution
  prior <- auditPrior(confidence, tolerance, method = 'median')
  # Use the prior distribution for planning
  plan <- planning(confidence, tolerance, expectedError = 0, prior = prior)
  # Use the prior distribution for evaluation
  result <- evaluation(confidence, tolerance, nSumstats = plan$sampleSize, kSumstats = plan$expectedSampleError, prior = prior)
  # Extract the posterior distribution
  posterior <- result$posterior
  # Use the posterior distribution for planning
  plan2 <- planning(confidence, tolerance, expectedError = 0, prior = result$posterior)
  # Use the posterior distribution for evaluation
  result2 <- evaluation(confidence, tolerance, nSumstats = plan2$sampleSize, kSumstats = plan2$expectedSampleError, prior = result$posterior)
  
  expect_equal(result2[["confBound"]], 0.04829835) # Upper bound of 4.8%
})