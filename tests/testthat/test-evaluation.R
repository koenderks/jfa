context("Evaluation")

test_that(desc = "Evaluation with Poisson method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "poisson", materiality = 0.05)
  print(jfaEval)
  plot(jfaEval)
  expect_equal(jfaEval$confBound, 0.04992887)
})

test_that(desc = "Evaluation with Poisson method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "poisson", prior = TRUE, materiality = 0.05)
  print(jfaEval)
  plot(jfaEval)
  expect_equal(jfaEval$confBound, 0.04911037)
})

test_that(desc = "Evaluation with binomial method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "binomial", materiality = 0.05)
  print(jfaEval)
  plot(jfaEval)
  expect_equal(jfaEval$confBound, 0.04870291)
})

test_that(desc = "Evaluation with binomial method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "binomial", prior = TRUE, materiality = 0.05)
  print(jfaEval)
  plot(jfaEval)
  expect_equal(jfaEval$confBound, 0.04792395)
})

test_that(desc = "Evaluation with hypergeometric method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "hypergeometric", N = 1000, materiality = 0.05)
  print(jfaEval)
  plot(jfaEval)
  expect_equal(jfaEval$confBound, 0.04180536)
})

test_that(desc = "Evaluation with hypergeometric method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "hypergeometric", N = 1000, prior = TRUE, materiality = 0.05)
  print(jfaEval)
  plot(jfaEval)
  expect_equal(jfaEval$confBound, 0.049)
})

test_that(desc = "Evaluation with stringer method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer")
  print(jfaEval)
  expect_equal(jfaEval$confBound, 0.04870291)
})

test_that(desc = "Evaluation with stringer-meikle method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer-meikle")
  print(jfaEval)
  expect_equal(jfaEval$confBound, 0.04870291)
})

test_that(desc = "Evaluation with stringer-lta method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer-lta")
  print(jfaEval)
  expect_equal(jfaEval$confBound, 0.04870291)
})

test_that(desc = "Evaluation with stringer-pvz method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer-pvz")
  print(jfaEval)
  expect_equal(jfaEval$confBound, 0.04870291)
})

test_that(desc = "Evaluation with rohrbach method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "rohrbach", N = 1000)
  print(jfaEval)
  expect_equal(jfaEval$confBound, 0.0308821)
})

test_that(desc = "Evaluation with moment method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "moment", N = 1000)
  print(jfaEval)
  expect_equal(jfaEval$confBound, 0.04916021)
})

test_that(desc = "Evaluation with direct method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "direct", N = 1000, populationBookValue = sum(population$bookValue))
  expect_equal(jfaEval$pointEstimate, 294231.508)
  expect_equal(jfaEval$lowerBound, 262653.9205)
  expect_equal(jfaEval$upperBound, 325809.1-0.00547)
})

test_that(desc = "Evaluation with difference method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "difference", N = 1000, populationBookValue = sum(population$bookValue))
  expect_equal(jfaEval$pointEstimate, 297454 - 0.0243)
  expect_equal(jfaEval$lowerBound, 297454 - 0.0243)
  expect_equal(jfaEval$upperBound, 297454 - 0.0243)
})

test_that(desc = "Evaluation with quotient method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "quotient", N = 1000, populationBookValue = sum(population$bookValue))
  expect_equal(jfaEval$pointEstimate, 297454 - 0.0243)
  expect_equal(jfaEval$lowerBound, 297454 - 0.0243)
  expect_equal(jfaEval$upperBound, 297454 - 0.0243)
})

test_that(desc = "Evaluation with regression method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "regression", N = 1000, populationBookValue = sum(population$bookValue))
  expect_equal(jfaEval$pointEstimate, 297454 - 0.0243)
  expect_equal(jfaEval$lowerBound, 297454 - 0.0243)
  expect_equal(jfaEval$upperBound, 297454 - 0.0243)
})

test_that(desc = "Evaluation with Cox and Snell method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  p <- auditPrior(materiality = 0.05, confidence = 0.95, method = "arm", ir = 1, cr = 0.6, expectedError = 0.025)
  jfaRes <- planning(materiality = 0.05, prior = p, expectedError = 0.025, confidence = 0.95)
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  samp$auditValue[1:3] <- samp$bookValue[1:3] * 0.4
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "coxsnell", prior = p)
  expect_equal(jfaEval$confBound, 0.02765165)
})
