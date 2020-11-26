context("5. Function test for evaluation()")

# jfa version 0.1.0

test_that(desc = "(id: f5-v0.1.0-t1) Evaluation with Poisson method", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "poisson", materiality = 0.05)
	expect_equal(jfaEval[["confBound"]], 0.04992887, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t2) Evaluation with Poisson method with prior", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "poisson", prior = TRUE, materiality = 0.05)
	expect_equal(jfaEval[["confBound"]], 0.04911037, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t3) Evaluation with binomial method", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "binomial", materiality = 0.05)
	expect_equal(jfaEval[["confBound"]], 0.04870291, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t4) Evaluation with binomial method with prior", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "binomial", prior = TRUE, materiality = 0.05)
	expect_equal(jfaEval[["confBound"]], 0.04792395, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t5) Evaluation with hypergeometric method", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "hypergeometric", N = 1000, materiality = 0.05)
	expect_equal(jfaEval[["confBound"]], 0.04180536, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t6) Evaluation with hypergeometric method with prior", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "hypergeometric", N = 1000, prior = TRUE, materiality = 0.05)
	expect_equal(jfaEval[["confBound"]], 0.049, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t7) Evaluation with stringer method", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer")
	expect_equal(jfaEval[["confBound"]], 0.04870291, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t8) Evaluation with stringer-meikle method", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	samp$auditValue[1] <- samp[["bookValue"]][1] + (0.5 * samp[["bookValue"]][1]) # Add an understatement
	samp$auditValue[2] <- samp[["bookValue"]][2] + (0.5 * samp[["bookValue"]][2]) # Add an understatement
	samp$auditValue[3] <- samp[["bookValue"]][3] + (0.5 * samp[["bookValue"]][3]) # Add an understatement
	samp$auditValue[4] <- samp[["bookValue"]][4] - (0.6 * samp[["bookValue"]][4]) # Add an overstatement
	samp$auditValue[5] <- samp[["bookValue"]][5] - (0.6 * samp[["bookValue"]][5]) # Add an overstatement
	samp$auditValue[6] <- samp[["bookValue"]][6] - (0.6 * samp[["bookValue"]][6]) # Add an overstatement
	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer-meikle")
	expect_equal(jfaEval[["confBound"]], 0.0338254, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t9) Evaluation with stringer-lta method", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	samp$auditValue[1] <- samp[["bookValue"]][1] + (0.5 * samp[["bookValue"]][1]) # Add an understatement
	samp$auditValue[2] <- samp[["bookValue"]][2] + (0.5 * samp[["bookValue"]][2]) # Add an understatement
	samp$auditValue[3] <- samp[["bookValue"]][3] + (0.5 * samp[["bookValue"]][3]) # Add an understatement
	samp$auditValue[4] <- samp[["bookValue"]][4] - (0.6 * samp[["bookValue"]][4]) # Add an overstatement
	samp$auditValue[5] <- samp[["bookValue"]][5] - (0.6 * samp[["bookValue"]][5]) # Add an overstatement
	samp$auditValue[6] <- samp[["bookValue"]][6] - (0.6 * samp[["bookValue"]][6]) # Add an overstatement
	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer-lta")
	expect_equal(jfaEval[["confBound"]], 0.06899349, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t10) Evaluation with stringer-pvz method", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	samp$auditValue[1] <- samp[["bookValue"]][1] + (0.5 * samp[["bookValue"]][1]) # Add an understatement
	samp$auditValue[2] <- samp[["bookValue"]][2] + (0.5 * samp[["bookValue"]][2]) # Add an understatement
	samp$auditValue[3] <- samp[["bookValue"]][3] + (0.5 * samp[["bookValue"]][3]) # Add an understatement
	samp$auditValue[4] <- samp[["bookValue"]][4] - (0.6 * samp[["bookValue"]][4]) # Add an overstatement
	samp$auditValue[5] <- samp[["bookValue"]][5] - (0.6 * samp[["bookValue"]][5]) # Add an overstatement
	samp$auditValue[6] <- samp[["bookValue"]][6] - (0.6 * samp[["bookValue"]][6]) # Add an overstatement
	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer-pvz")
	expect_equal(jfaEval[["confBound"]], 0.07590801, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t11) Evaluation with rohrbach method", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "rohrbach", N = 1000)
	expect_equal(jfaEval[["confBound"]], 0.0308821, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t12) Evaluation with moment method", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "moment", N = 1000, momentPoptype = "accounts")
	expect_equal(jfaEval[["confBound"]], 0.04916021, tolerance = 0.001)

	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "moment", N = 1000, momentPoptype = "inventory")
	expect_equal(jfaEval[["confBound"]], 0.04916021, tolerance = 0.001)

	samp$auditValue[1] <- samp[["bookValue"]][1] - (0.5 * samp[["bookValue"]][1]) # Add an overstatement
	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "moment", N = 1000, momentPoptype = "accounts")
	expect_equal(jfaEval[["confBound"]], 0.03730203, tolerance = 0.001)

	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "moment", N = 1000, momentPoptype = "inventory")
	expect_equal(jfaEval[["confBound"]], 0.03656485, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t13) Evaluation with direct method", {
	data("BuildIt")
	BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
	BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
	jfaEval <- evaluation(materiality = 0.05, sample = BuildIt_sample, bookValues = "bookValue", auditValues = "auditValue", method = "direct", N = 3500, populationBookValue = sum(BuildIt$bookValue))
	expect_equal(jfaEval[["mle"]], 59580.62, tolerance = 0.001)
	expect_equal(jfaEval[["lowerBound"]], -59050.61, tolerance = 0.001)
	expect_equal(jfaEval[["upperBound"]], 178211.9, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t14) Evaluation with difference method", {
	data("BuildIt")
	BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
	BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
	jfaEval <- evaluation(materiality = 0.05, sample = BuildIt_sample, bookValues = "bookValue", auditValues = "auditValue", method = "difference", N = 3500, populationBookValue = sum(BuildIt$bookValue))
	expect_equal(jfaEval[["mle"]], 34298.6, tolerance = 0.001)
	expect_equal(jfaEval[["lowerBound"]], 3437.26, tolerance = 0.001)
	expect_equal(jfaEval[["upperBound"]], 65159.94, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t15) Evaluation with quotient method", {
	data("BuildIt")
	BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
	BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
	jfaEval <- evaluation(materiality = 0.05, sample = BuildIt_sample, bookValues = "bookValue", auditValues = "auditValue", method = "quotient", N = 3500, populationBookValue = sum(BuildIt$bookValue))
	expect_equal(jfaEval[["mle"]], 34298.6, tolerance = 0.001)
	expect_equal(jfaEval[["lowerBound"]], 3138.557, tolerance = 0.001)
	expect_equal(jfaEval[["upperBound"]], 65458.64, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t16) Evaluation with regression method", {
	data("BuildIt")
	BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
	BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
	jfaEval <- evaluation(materiality = 0.05, sample = BuildIt_sample, bookValues = "bookValue", auditValues = "auditValue", method = "regression", N = 3500, populationBookValue = sum(BuildIt$bookValue))
	expect_equal(jfaEval[["mle"]], 33872, tolerance = 0.001)
	expect_equal(jfaEval[["lowerBound"]], 3069.264, tolerance = 0.001)
	expect_equal(jfaEval[["upperBound"]], 64674.73, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t17) Evaluation with Cox and Snell method", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, expectedError = 0.025, confidence = 0.95)
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp$bookValue
	samp$auditValue[1:3] <- samp$bookValue[1:3] * 0.4
	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "coxsnell")
	expect_equal(jfaEval[["confBound"]], 0.02282112, tolerance = 0.001)
})

# jfa version 0.2.0
# No changes to be tested

# jfa version 0.3.0

test_that(desc = "(id: f5-v0.3.0-t1) Evaluation with counts and stringer method", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, expectedError = 0.025, confidence = 0.95)
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	samp$auditValue[1:3] <- samp$bookValue[1:3] * 0.4
	counts <- c(2, 2, 3, rep(1, nrow(samp) - 3))
	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "stringer", count = counts)
	expect_equal(jfaEval[["confBound"]], 0.0326619, tolerance = 0.001)
})

# jfa version 0.3.1
# No changes to be tested

# jfa version 0.4.0
test_that(desc = "(id: f5-v0.4.0-t1) Bayes factors", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, expectedError = 0.025, confidence = 0.95)
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	samp$auditValue[1:3] <- samp$bookValue[1:3] * 0.4
	counts <- c(2, 2, 3, rep(1, nrow(samp) - 3))
	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "binomial", count = counts, prior = TRUE)
	expect_equal(jfaEval[["posterior"]][["hypotheses"]]$bf, 1997.977, tolerance = 0.001)
	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "poisson", count = counts, prior = TRUE)
	expect_equal(jfaEval[["posterior"]][["hypotheses"]]$bf, Inf, tolerance = 0.001)
	jfaEval <- evaluation(materiality = 0.05, sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "hypergeometric", count = counts, prior = TRUE, N = 1000)
	expect_equal(jfaEval[["posterior"]][["hypotheses"]]$bf, 4766.862, tolerance = 0.001)
})

# jfa version 0.5.0

test_that(desc = "(id: f5-v0.5.0-t1) Test for mpu estimator", {
	
	sample <- data.frame(ID = 1:100, ist = rnorm(mean = 1000, n = 100))
	sample$soll <- sample$ist
	sample$ist[1] <- 120
	sample$soll[1] <- 100
	sample$ist[2] <- 100
	sample$soll[2] <- 120
	jfaEval <- jfa::evaluation(confidence = 0.95, method = "mpu", sample = sample, materiality = 0.1, bookValues = "ist", auditValues = "soll")
	expect_equal(jfaEval[["confBound"]], 0.003970126, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.5.0-t1) Test for frequentist print function", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "poisson", materiality = 0.05)
	invisible(capture.output(print(jfaEval)))
	expect_equal(jfaEval[["confBound"]], 0.04992887, tolerance = 0.001)

	data("BuildIt")
	BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
	BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
	jfaEval <- evaluation(materiality = 0.05, sample = BuildIt_sample, bookValues = "bookValue", auditValues = "auditValue", method = "regression", N = 3500, populationBookValue = sum(BuildIt$bookValue))
	invisible(capture.output(print(jfaEval)))
	expect_equal(jfaEval[["mle"]], 33872, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.2.0-t2) Test for Bayesian print function", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "binomial", prior = TRUE, materiality = 0.05)
	invisible(capture.output(print(jfaEval)))
	invisible(capture.output(print(jfaEval[["expectedPosterior"]])))
	expect_equal(jfaEval[["confBound"]], 0.04792395, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.2.0-t3) Test for frequentist plot function", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	jfaRes <- planning(materiality = 0.05, likelihood = "poisson")
	samp <- selection(population, sampleSize = jfaRes, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "poisson", materiality = 0.05)
	invisible(capture.output(plot(jfaEval)))
	expect_equal(jfaEval[["confBound"]], 0.04992887, tolerance = 0.001)

	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "binomial", materiality = 0.05)
	invisible(capture.output(plot(jfaEval)))

	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "hypergeometric", materiality = 0.05, N = nrow(population))
	invisible(capture.output(plot(jfaEval)))

	data("BuildIt")
	BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
	BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
	jfaEval <- evaluation(materiality = 0.05, sample = BuildIt_sample, bookValues = "bookValue", auditValues = "auditValue", method = "regression", N = 3500, populationBookValue = sum(BuildIt$bookValue))
	invisible(capture.output(plot(jfaEval)))
})

test_that(desc = "(id: f5-v0.2.0-t4) Test for Bayesian plot function", {
	set.seed(1)
	population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
	samp <- selection(population, sampleSize = 100, units = "records", algorithm = "random", ordered = TRUE)$sample
	samp$auditValue <- samp[["bookValue"]]
	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "binomial", prior = TRUE, materiality = 0.05)
	invisible(capture.output(plot(jfaEval)))
	invisible(capture.output(plot(jfaEval[["posterior"]])))

	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "poisson", prior = TRUE, materiality = 0.05)
	invisible(capture.output(plot(jfaEval)))

	jfaEval <- evaluation(sample = samp, bookValues = "bookValue", auditValues = "auditValue", method = "hypergeometric", prior = TRUE, materiality = 0.05, N = nrow(population))
	invisible(capture.output(plot(jfaEval)))

	expect_equal(jfaEval[["confBound"]], 0.03, tolerance = 0.001)
})