context("Evaluation")

test_that(desc = "Evaluation with Poisson method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "poisson", materiality = 0.05)
  expect_equal(jfaEval$confBound, 0.04992887)
})

test_that(desc = "Evaluation with Poisson method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "poisson", prior = TRUE, materiality = 0.05)
  expect_equal(jfaEval$confBound, 0.04911037)
})

test_that(desc = "Evaluation with binomial method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "binomial", materiality = 0.05)
  expect_equal(jfaEval$confBound, 0.04870291)
})

test_that(desc = "Evaluation with binomial method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "binomial", prior = TRUE, materiality = 0.05)
  expect_equal(jfaEval$confBound, 0.04792395)
})

test_that(desc = "Evaluation with hypergeometric method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "hypergeometric", N = 1000, materiality = 0.05)
  expect_equal(jfaEval$confBound, 0.04180536)
})

test_that(desc = "Evaluation with hypergeometric method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "hypergeometric", N = 1000, prior = TRUE, materiality = 0.05)
  expect_equal(jfaEval$confBound, 0.046)
})

test_that(desc = "Evaluation with stringer method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer")
  expect_equal(jfaEval$confBound, 0.04870291)
})

test_that(desc = "Evaluation with stringer-meikle method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer-meikle")
  expect_equal(jfaEval$confBound, 0.04870291)
})

test_that(desc = "Evaluation with stringer-lta method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer-lta")
  expect_equal(jfaEval$confBound, 0.04870291)
})

test_that(desc = "Evaluation with stringer-pvz method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer-pvz")
  expect_equal(jfaEval$confBound, 0.04870291)
})

test_that(desc = "Evaluation with rohrbach method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "rohrbach", N = 1000)
  expect_equal(jfaEval$confBound, 0.0308821)
})

test_that(desc = "Evaluation with moment method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "moment", N = 1000)
  expect_equal(jfaEval$confBound, 0.04916021)
})
