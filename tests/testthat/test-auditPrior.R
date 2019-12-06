context("Prior")

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
