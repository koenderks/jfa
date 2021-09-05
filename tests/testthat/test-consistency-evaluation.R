context("Test consistency of function evaluation()")

# jfa version 0.1.0

test_that(desc = "(id: f3-v0.1.0-t1) Evaluation with Poisson method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "poisson", materiality = 0.05)
  expect_equal(jfaEval[["ub"]], 0.04992887, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t2) Evaluation with Poisson method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "poisson", prior = TRUE, materiality = 0.05)
  expect_equal(jfaEval[["ub"]], 0.04911037, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t3) Evaluation with binomial method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "binomial", materiality = 0.05)
  expect_equal(jfaEval[["ub"]], 0.04870291, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t4) Evaluation with binomial method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "binomial", prior = TRUE, materiality = 0.05)
  expect_equal(jfaEval[["ub"]], 0.04792395, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t5) Evaluation with hypergeometric method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "hypergeometric", N = nrow(population))
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "hypergeometric", N.units = 1000, materiality = 0.05)
  expect_equal(jfaEval[["ub"]], 0.049, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t6) Evaluation with hypergeometric method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "hypergeometric", N.units = 1000, prior = TRUE, materiality = 0.05)
  expect_equal(jfaEval[["ub"]], 0.047, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t7) Evaluation with stringer method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "stringer")
  expect_equal(jfaEval[["ub"]], 0.04870291, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t8) Evaluation with stringer.meikle method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  samp$auditValue[1] <- samp[["bookValue"]][1] + (0.5 * samp[["bookValue"]][1]) # Add an understatement
  samp$auditValue[2] <- samp[["bookValue"]][2] + (0.5 * samp[["bookValue"]][2]) # Add an understatement
  samp$auditValue[3] <- samp[["bookValue"]][3] + (0.5 * samp[["bookValue"]][3]) # Add an understatement
  samp$auditValue[4] <- samp[["bookValue"]][4] - (0.6 * samp[["bookValue"]][4]) # Add an overstatement
  samp$auditValue[5] <- samp[["bookValue"]][5] - (0.6 * samp[["bookValue"]][5]) # Add an overstatement
  samp$auditValue[6] <- samp[["bookValue"]][6] - (0.6 * samp[["bookValue"]][6]) # Add an overstatement
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "stringer.meikle")
  expect_equal(jfaEval[["ub"]], 0.0338254, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t9) Evaluation with stringer.lta method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  samp$auditValue[1] <- samp[["bookValue"]][1] + (0.5 * samp[["bookValue"]][1]) # Add an understatement
  samp$auditValue[2] <- samp[["bookValue"]][2] + (0.5 * samp[["bookValue"]][2]) # Add an understatement
  samp$auditValue[3] <- samp[["bookValue"]][3] + (0.5 * samp[["bookValue"]][3]) # Add an understatement
  samp$auditValue[4] <- samp[["bookValue"]][4] - (0.6 * samp[["bookValue"]][4]) # Add an overstatement
  samp$auditValue[5] <- samp[["bookValue"]][5] - (0.6 * samp[["bookValue"]][5]) # Add an overstatement
  samp$auditValue[6] <- samp[["bookValue"]][6] - (0.6 * samp[["bookValue"]][6]) # Add an overstatement
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "stringer.lta")
  expect_equal(jfaEval[["ub"]], 0.06899349, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t10) Evaluation with stringer.pvz method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  samp$auditValue[1] <- samp[["bookValue"]][1] + (0.5 * samp[["bookValue"]][1]) # Add an understatement
  samp$auditValue[2] <- samp[["bookValue"]][2] + (0.5 * samp[["bookValue"]][2]) # Add an understatement
  samp$auditValue[3] <- samp[["bookValue"]][3] + (0.5 * samp[["bookValue"]][3]) # Add an understatement
  samp$auditValue[4] <- samp[["bookValue"]][4] - (0.6 * samp[["bookValue"]][4]) # Add an overstatement
  samp$auditValue[5] <- samp[["bookValue"]][5] - (0.6 * samp[["bookValue"]][5]) # Add an overstatement
  samp$auditValue[6] <- samp[["bookValue"]][6] - (0.6 * samp[["bookValue"]][6]) # Add an overstatement
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "stringer.pvz")
  expect_equal(jfaEval[["ub"]], 0.07590801, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t11) Evaluation with rohrbach method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "rohrbach", N.units = 1000)
  expect_equal(jfaEval[["ub"]], 0.0308821, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t12) Evaluation with moment method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "moment", N.units = 1000, m.type = "accounts")
  expect_equal(jfaEval[["ub"]], 0.04916021, tolerance = 0.001)
  
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "moment", N.units = 1000, m.type = "inventory")
  expect_equal(jfaEval[["ub"]], 0.04916021, tolerance = 0.001)
  
  samp$auditValue[1] <- samp[["bookValue"]][1] - (0.5 * samp[["bookValue"]][1]) # Add an overstatement
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "moment", N.units = 1000, m.type = "accounts")
  expect_equal(jfaEval[["ub"]], 0.03730203, tolerance = 0.001)
  
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "moment", N.units = 1000, m.type = "inventory")
  expect_equal(jfaEval[["ub"]], 0.03656485, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t13) Evaluation with direct method", {
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = BuildIt_sample, values = "bookValue", values.audit = "auditValue", method = "direct", N.items = 3500, N.units = sum(BuildIt$bookValue))
  expect_equal(jfaEval[["mle"]], 59580.62, tolerance = 0.001)
  expect_equal(jfaEval[["lb"]], -59050.61, tolerance = 0.001)
  expect_equal(jfaEval[["ub"]], 178211.9, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t14) Evaluation with difference method", {
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = BuildIt_sample, values = "bookValue", values.audit = "auditValue", method = "difference", N.items = 3500)
  expect_equal(jfaEval[["mle"]], 34298.6, tolerance = 0.001)
  expect_equal(jfaEval[["lb"]], 3437.26, tolerance = 0.001)
  expect_equal(jfaEval[["ub"]], 65159.94, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t15) Evaluation with quotient method", {
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = BuildIt_sample, values = "bookValue", values.audit = "auditValue", method = "quotient", N.items = 3500)
  expect_equal(jfaEval[["mle"]], 34298.6, tolerance = 0.001)
  expect_equal(jfaEval[["lb"]], 3138.557, tolerance = 0.001)
  expect_equal(jfaEval[["ub"]], 65458.64, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t16) Evaluation with regression method", {
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = BuildIt_sample, values = "bookValue", values.audit = "auditValue", method = "regression", N.items = 3500, N.units = sum(BuildIt$bookValue))
  expect_equal(jfaEval[["mle"]], 33872, tolerance = 0.001)
  expect_equal(jfaEval[["lb"]], 3069.264, tolerance = 0.001)
  expect_equal(jfaEval[["ub"]], 64674.73, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t17) Evaluation with Cox and Snell method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, expected = 0.025)
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp$bookValue
  samp$auditValue[1:3] <- samp$bookValue[1:3] * 0.4
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "coxsnell")
  expect_equal(jfaEval[["ub"]], 0.02282112, tolerance = 0.001)
})

# jfa version 0.2.0
# No changes to be benchmarked

# jfa version 0.3.0

test_that(desc = "(id: f3-v0.3.0-t1) Evaluation with counts and stringer method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, expected = 0.025)
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  samp$auditValue[1:3] <- samp$bookValue[1:3] * 0.4
  counts <- c(2, 2, 3, rep(1, nrow(samp) - 3))
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "stringer", times = counts)
  expect_equal(jfaEval[["ub"]], 0.0326619, tolerance = 0.001)
})

# jfa version 0.3.1
# No changes to be benchmarked

# jfa version 0.4.0
test_that(desc = "(id: f3-v0.4.0-t1) Bayes factors", {
  data("BuildIt")
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, expected = 0.025, likelihood = 'poisson')
  samp <- selection(BuildIt, size = jfaRes, units = "rows", method = "interval", order = TRUE)$sample
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "binomial", times = samp$count, prior = TRUE)
  expect_equal(jfaEval[["posterior"]][["hypotheses"]]$bf.hmin, 44957.32, tolerance = 0.001)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "poisson", times = samp$count, prior = TRUE)
  expect_equal(jfaEval[["posterior"]][["hypotheses"]]$bf.hmin, 1822.754944, tolerance = 0.001)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "hypergeometric", times = samp$count, prior = TRUE, N.units = 1000)
  expect_equal(jfaEval[["posterior"]][["hypotheses"]]$bf.hmin, 295149, tolerance = 0.001)
})

# jfa version 0.5.0

test_that(desc = "(id: f3-v0.5.0-t1) Test for mpu estimator", {
  sample <- data.frame(ID = 1:100, ist = rnorm(mean = 1000, n = 100))
  sample$soll <- sample$ist
  sample$ist[1] <- 120
  sample$soll[1] <- 100
  sample$ist[2] <- 100
  sample$soll[2] <- 120
  jfaEval <- jfa::evaluation(conf.level = 0.95, method = "mpu", data = sample, materiality = 0.1, values = "ist", values.audit = "soll")
  expect_equal(jfaEval[["ub"]], 0.003970126, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.5.0-t1) Test for frequentist summary and print function", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "poisson", materiality = 0.05)
  invisible(capture.output(print(jfaEval)))
  invisible(capture.output(summary(jfaEval)))
  expect_equal(jfaEval[["ub"]], 0.04992887, tolerance = 0.001)
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = BuildIt_sample, values = "bookValue", values.audit = "auditValue", method = "regression", N.items = 3500, N.units = sum(BuildIt$bookValue))
  invisible(capture.output(print(jfaEval)))
  invisible(capture.output(summary(jfaEval)))
  expect_equal(jfaEval[["mle"]], 33872, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.2.0-t2) Test for Bayesian summary function", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "binomial", prior = TRUE, materiality = 0.05)
  invisible(capture.output(print(jfaEval)))
  invisible(capture.output(summary(jfaEval)))
  invisible(capture.output(print(jfaEval[["expectedPosterior"]])))
  invisible(capture.output(summary(jfaEval[["expectedPosterior"]])))
  expect_equal(jfaEval[["ub"]], 0.04792395, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.2.0-t3) Test for frequentist plot function", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "poisson", materiality = 0.05)
  invisible(capture.output(plot(jfaEval)))
  expect_equal(jfaEval[["ub"]], 0.04992887, tolerance = 0.001)
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "binomial", materiality = 0.05)
  invisible(capture.output(plot(jfaEval)))
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "hypergeometric", materiality = 0.05, N.units = nrow(population))
  invisible(capture.output(plot(jfaEval)))
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = BuildIt_sample, values = "bookValue", values.audit = "auditValue", method = "regression", N.items = 3500, N.units = sum(BuildIt$bookValue))
  invisible(capture.output(plot(jfaEval)))
})

test_that(desc = "(id: f3-v0.2.0-t4) Test for Bayesian plot function", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  samp <- selection(population, size = 100, units = "rows", method = "random", order = TRUE)$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "binomial", prior = TRUE, materiality = 0.05)
  invisible(capture.output(plot(jfaEval)))
  invisible(capture.output(plot(jfaEval[["posterior"]])))
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "poisson", prior = TRUE, materiality = 0.05)
  invisible(capture.output(plot(jfaEval)))
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "hypergeometric", prior = TRUE, materiality = 0.05, N.units = nrow(population))
  invisible(capture.output(plot(jfaEval)))
  expect_equal(jfaEval[["ub"]], 0.028, tolerance = 0.001)
})

# jfa version 0.5.1 - 0.5.7
# No changes to be benchmarked

# jfa 0.6.0

test_that(desc = "(id: f3-v0.6.0-t1) Test Bayes factors for beta prior", {
  # Compute a Bayes factor from a noninformative beta(1, 1) prior, n = 160, k = 1
  # Derks, K., de Swart, J., Wagenmakers, E-.J., and Wetzels, R. (2021). The Bayesian approach to audit evidence: Quantifying statistical evidence using the Bayes factor.
  # BF-+ = 696.696
  BF <- evaluation(materiality = 0.03, n = 160, x = 1, prior = auditPrior(method = 'custom', alpha = 1, beta = 1, likelihood = 'binomial'))$posterior$hypotheses$bf.hmin
  expect_equal(BF, 696.696)
  
  # Compute a Bayes factor from a noninformative beta(1, 1) prior, n = 50, k = 1
  # Derks, K., de Swart, J., Wagenmakers, E-.J., and Wetzels, R. (2021). A default Bayesian hypothesis test for audit sampling.
  # BF-+ = 51.55
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = 'custom', alpha = 1, beta = 1, likelihood = 'binomial'))$posterior$hypotheses$bf.hmin
  expect_equal(BF, 51.551344113750538)
  
  # Compute a default Bayes factor from an impartial beta prior
  # Derks, K., de Swart, J., Wagenmakers, E-.J., and Wetzels, R. (2021). A default Bayesian hypothesis test for audit sampling.
  # BF-+ = 4.98
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = 'median', materiality = 0.05, likelihood = 'binomial'))$posterior$hypotheses$bf.hmin
  expect_equal(BF, 4.9852019781149854)

  # Compute a default Bayes factor from an improper beta prior
  # BF-+ = 2.58
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = 'custom', alpha = 1, beta = 0, likelihood = 'binomial'))$posterior$hypotheses$bf.hmin
  expect_equal(BF, 2.578691368)
})

test_that(desc = "(id: f3-v0.6.0-t2) Test Bayes factors for gamma prior", {
  
  # Compute a Bayes factor from a noninformative gamma(1, 0) prior
  BF <- evaluation(materiality = 0.03, n = 160, x = 1, prior = auditPrior(method = 'none', likelihood = 'poisson'))$posterior$hypotheses$bf.hmin
  expect_equal(BF, 19.95007199)

    # Compute a Bayes factor from a noninformative gamma(1, 0) prior
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = 'none', likelihood = 'poisson'))$posterior$hypotheses$bf.hmin
  expect_equal(BF, 2.48071256)
  
  # Compute a default Bayes factor from an impartial gamma prior
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = 'median', materiality = 0.05, likelihood = 'poisson'))$posterior$hypotheses$bf.hmin
  expect_equal(BF, 4.810668425)
})

test_that(desc = "(id: f3-v0.6.0-t3) Test Bayes factors for beta-binomial prior", {
  
  # Compute a Bayes factor from a noninformative beta-binomial prior
  BF <- evaluation(materiality = 0.03, n = 160, x = 1, prior = auditPrior(method = 'none', likelihood = "hypergeometric", N.units = 1000))$posterior$hypotheses$bf.hmin
  expect_equal(BF, 1248.71551)
  
  # Compute a Bayes factor from a noninformative beta-binomial prior
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = 'none', likelihood = "hypergeometric", N.units = 1000))$posterior$hypotheses$bf.hmin
  expect_equal(BF, 58.37849102)

  # Compute a default Bayes factor from an impartial beta-binomial prior
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = 'median', materiality = 0.05, likelihood = "hypergeometric", N.units = 1000))$posterior$hypotheses$bf.hmin
  expect_equal(BF, 5.676959268)
})