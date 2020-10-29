context("5. Function test for evaluation()")

# jfa version 0.1.0

test_that(desc = "Evaluation with Poisson method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "poisson", materiality = 0.05)
  expect_equal(jfaEval$confBound, 0.04992887, tolerance = 0.001)
})

test_that(desc = "Evaluation with Poisson method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "poisson", prior = TRUE, materiality = 0.05)
  expect_equal(jfaEval$confBound, 0.04911037, tolerance = 0.001)
})

test_that(desc = "Evaluation with binomial method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "binomial", materiality = 0.05)
  expect_equal(jfaEval$confBound, 0.04870291, tolerance = 0.001)
})

test_that(desc = "Evaluation with binomial method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "binomial", prior = TRUE, materiality = 0.05)
  expect_equal(jfaEval$confBound, 0.04792395, tolerance = 0.001)
})

test_that(desc = "Evaluation with hypergeometric method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "hypergeometric", N = 1000, materiality = 0.05)
  expect_equal(jfaEval$confBound, 0.04180536, tolerance = 0.001)
})

test_that(desc = "Evaluation with hypergeometric method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "hypergeometric", N = 1000, prior = TRUE, materiality = 0.05)
  expect_equal(jfaEval$confBound, 0.049, tolerance = 0.001)
})

test_that(desc = "Evaluation with stringer method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer")
  expect_equal(jfaEval$confBound, 0.04870291, tolerance = 0.001)
})

test_that(desc = "Evaluation with stringer-meikle method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer-meikle")
  expect_equal(jfaEval$confBound, 0.04870291, tolerance = 0.001)
})

test_that(desc = "Evaluation with stringer-lta method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer-lta")
  expect_equal(jfaEval$confBound, 0.04870291, tolerance = 0.001)
})

test_that(desc = "Evaluation with stringer-pvz method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer-pvz")
  expect_equal(jfaEval$confBound, 0.04870291, tolerance = 0.001)
})

test_that(desc = "Evaluation with rohrbach method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "rohrbach", N = 1000)
  expect_equal(jfaEval$confBound, 0.0308821, tolerance = 0.001)
})

test_that(desc = "Evaluation with moment method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "moment", N = 1000)
  expect_equal(jfaEval$confBound, 0.04916021, tolerance = 0.001)
})

test_that(desc = "Evaluation with direct method", {
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(materiality = 0.05, sample = BuildIt_sample, bookValues = "bookValue", auditValues = "auditValue", method = "direct", N = 3500, populationBookValue = sum(BuildIt$bookValue))
  expect_equal(jfaEval$pointEstimate, 1343640, tolerance = 0.001)
  expect_equal(jfaEval$lowerBound, 1226699, tolerance = 0.001)
  expect_equal(jfaEval$upperBound, 1460581, tolerance = 0.001)
})

test_that(desc = "Evaluation with difference method", {
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(materiality = 0.05, sample = BuildIt_sample, bookValues = "bookValue", auditValues = "auditValue", method = "difference", N = 3500, populationBookValue = sum(BuildIt$bookValue))
  expect_equal(jfaEval$pointEstimate, 1368922, tolerance = 0.001)
  expect_equal(jfaEval$lowerBound, 1338501, tolerance = 0.001)
  expect_equal(jfaEval$upperBound, 1399344, tolerance = 0.001)
})

test_that(desc = "Evaluation with quotient method", {
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(materiality = 0.05, sample = BuildIt_sample, bookValues = "bookValue", auditValues = "auditValue", method = "quotient", N = 3500, populationBookValue = sum(BuildIt$bookValue))
  expect_equal(jfaEval$pointEstimate, 1368293, tolerance = 0.001)
  expect_equal(jfaEval$lowerBound, 1337577, tolerance = 0.001)
  expect_equal(jfaEval$upperBound, 1399009, tolerance = 0.001)
})

test_that(desc = "Evaluation with regression method", {
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(materiality = 0.05, sample = BuildIt_sample, bookValues = "bookValue", auditValues = "auditValue", method = "regression", N = 3500, populationBookValue = sum(BuildIt$bookValue))
  expect_equal(jfaEval$pointEstimate, 1369349, tolerance = 0.001)
  expect_equal(jfaEval$lowerBound, 1338985, tolerance = 0.001)
  expect_equal(jfaEval$upperBound, 1399713, tolerance = 0.001)
})

test_that(desc = "Evaluation with Cox and Snell method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  p <- auditPrior(materiality = 0.05, confidence = 0.95, method = "arm", ir = 1, cr = 0.6, expectedError = 0.025)
  jfaRes <- planning(materiality = 0.05, prior = p, expectedError = 0.025, confidence = 0.95)
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  samp$auditValue[1:3] <- samp$bookValue[1:3] * 0.4
  jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "coxsnell", prior = p)
  expect_equal(jfaEval$confBound, 0.03518384, tolerance = 0.001)
})

# jfa version 0.2.0
# No changes to be tested

# jfa version 0.3.0

test_that(desc = "Evaluation with counts and stringer method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, expectedError = 0.025, confidence = 0.95)
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  samp$auditValue[1:3] <- samp$bookValue[1:3] * 0.4
  counts <- c(2, 2, 3, rep(1, nrow(samp) - 3))
  jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer", count = counts)
  expect_equal(jfaEval$confBound, 0.0326619, tolerance = 0.001)
})

# jfa version 0.3.1
# No changes to be tested

# jfa version 0.4.0
test_that(desc = "Bayes factors", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(materiality = 0.05, expectedError = 0.025, confidence = 0.95)
  samp <- sampling(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
  samp$auditValue <- samp$bookValue
  samp$auditValue[1:3] <- samp$bookValue[1:3] * 0.4
  counts <- c(2, 2, 3, rep(1, nrow(samp) - 3))
  jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "binomial", count = counts, prior = TRUE)
  expect_equal(jfaEval[["posterior"]][["hypotheses"]]$bf, 1997.977, tolerance = 0.001)
  jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "poisson", count = counts, prior = TRUE)
  expect_equal(jfaEval[["posterior"]][["hypotheses"]]$bf, Inf, tolerance = 0.001)
  jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "hypergeometric", count = counts, prior = TRUE, N = 1000)
  expect_equal(jfaEval[["posterior"]][["hypotheses"]]$bf, 4766.862, tolerance = 0.001)
})