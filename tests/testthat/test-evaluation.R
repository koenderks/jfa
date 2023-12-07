# Copyright (C) 2020-2023 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

context("Validation of function evaluation")

# jfa version 0.1.0

test_that(desc = "(id: f3-v0.1.0-t1) Evaluation with Poisson method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "poisson", materiality = 0.05)
  expect_equal(jfaEval[["ub"]], 0.04992887, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t2) Evaluation with Poisson method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "poisson", prior = TRUE, materiality = 0.05)
  expect_equal(jfaEval[["ub"]], 0.04911037, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t3) Evaluation with binomial method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "binomial", materiality = 0.05)
  expect_equal(jfaEval[["ub"]], 0.04870291, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t4) Evaluation with binomial method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "binomial", prior = TRUE, materiality = 0.05)
  expect_equal(jfaEval[["ub"]], 0.04792395, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t5) Evaluation with hypergeometric method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "hypergeometric", N = nrow(population))
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "hypergeometric", N.units = 1000, materiality = 0.05)
  expect_equal(jfaEval[["ub"]], 0.049, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t6) Evaluation with hypergeometric method with prior", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  prior <- auditPrior(method = "default", likelihood = "hypergeometric", N.units = 1000)
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", prior = prior, materiality = 0.05)
  expect_equal(jfaEval[["ub"]], 0.046, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t7) Evaluation with stringer method", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "stringer.binomial")
  expect_equal(jfaEval[["ub"]], 0.04870291, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t8) Evaluation with stringer.meikle method", {
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
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
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
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
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
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
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "rohrbach", N.units = 1000)
  expect_equal(jfaEval[["ub"]], 0.0308821, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t12) Evaluation with moment method", {
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "moment", N.units = 1000)
  expect_equal(jfaEval[["ub"]], 0.04916021, tolerance = 0.001)
  samp$auditValue[1] <- samp[["bookValue"]][1] - (0.5 * samp[["bookValue"]][1]) # Add an overstatement
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "moment", N.units = 1000)
  expect_equal(jfaEval[["ub"]], 0.03730203, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t13) Evaluation with direct method", {
  testthat::skip_on_cran()
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = BuildIt_sample, values = "bookValue", values.audit = "auditValue", method = "direct", N.items = 3500, N.units = sum(BuildIt$bookValue), alternative = "two.sided")
  expect_equal(jfaEval[["mle"]], 59580.62, tolerance = 0.001)
  expect_equal(jfaEval[["lb"]], -59050.61, tolerance = 0.001)
  expect_equal(jfaEval[["ub"]], 178211.9, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t14) Evaluation with difference method", {
  testthat::skip_on_cran()
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = BuildIt_sample, values = "bookValue", values.audit = "auditValue", method = "difference", N.items = 3500, alternative = "two.sided")
  expect_equal(jfaEval[["mle"]], 34298.6, tolerance = 0.001)
  expect_equal(jfaEval[["lb"]], 3437.26, tolerance = 0.001)
  expect_equal(jfaEval[["ub"]], 65159.94, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t15) Evaluation with quotient method", {
  testthat::skip_on_cran()
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = BuildIt_sample, values = "bookValue", values.audit = "auditValue", method = "quotient", N.items = 3500, alternative = "two.sided")
  expect_equal(jfaEval[["mle"]], 34298.6, tolerance = 0.001)
  expect_equal(jfaEval[["lb"]], 3138.557, tolerance = 0.001)
  expect_equal(jfaEval[["ub"]], 65458.64, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t16) Evaluation with regression method", {
  data("BuildIt")
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = BuildIt_sample, values = "bookValue", values.audit = "auditValue", method = "regression", N.items = 3500, N.units = sum(BuildIt$bookValue), alternative = "two.sided")
  expect_equal(jfaEval[["mle"]], 33872, tolerance = 0.001)
  expect_equal(jfaEval[["lb"]], 3069.264, tolerance = 0.001)
  expect_equal(jfaEval[["ub"]], 64674.73, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.1.0-t17) Evaluation with Cox and Snell method", {
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, expected = 0.025)
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp$bookValue
  samp$auditValue[1:3] <- samp$bookValue[1:3] * 0.4
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "coxsnell")
  expect_equal(jfaEval[["ub"]], 0.02282112, tolerance = 0.001)
})

# jfa version 0.2.0
# No changes to be benchmarked

# jfa version 0.3.0

test_that(desc = "(id: f3-v0.3.0-t1) Evaluation with counts and stringer method", {
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, expected = 0.025)
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  samp$auditValue[1:3] <- samp$bookValue[1:3] * 0.4
  counts <- c(2, 2, 3, rep(1, nrow(samp) - 3))
  samp <- cbind(samp, counts)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "stringer.binomial", times = "counts")
  expect_equal(jfaEval[["ub"]], 0.0326619, tolerance = 0.001)
})

# jfa version 0.3.1
# No changes to be benchmarked

# jfa version 0.4.0
test_that(desc = "(id: f3-v0.4.0-t1) Bayes factors", {
  data("BuildIt")
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, expected = 0.025, likelihood = "poisson")
  samp <- selection(BuildIt, size = jfaRes, units = "items", method = "interval")$sample
  prior <- auditPrior(method = "default", likelihood = "binomial")
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "binomial", times = "times", prior = prior)
  expect_equal(jfaEval[["posterior"]][["hypotheses"]]$bf.h1, 1734.728, tolerance = 0.001)
  prior <- auditPrior(method = "strict", likelihood = "poisson")
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "poisson", times = "times", prior = prior)
  expect_equal(jfaEval[["posterior"]][["hypotheses"]]$odds.h1, 77.39047, tolerance = 0.001)
  prior <- auditPrior(method = "default", likelihood = "hypergeometric", N.units = 1000)
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "hypergeometric", times = "times", prior = prior, N.units = 1000)
  expect_equal(jfaEval[["posterior"]][["hypotheses"]]$bf.h1, 2103.842, tolerance = 0.001)
})

# jfa version 0.5.0

test_that(desc = "(id: f3-v0.5.0-t1) Test for mpu estimator", {
  testthat::skip_on_cran()
  sample <- data.frame(ID = 1:100, ist = rnorm(mean = 1000, n = 100))
  sample$soll <- sample$ist
  sample$ist[1] <- 120
  sample$soll[1] <- 100
  sample$ist[2] <- 100
  sample$soll[2] <- 120
  jfaEval <- evaluation(conf.level = 0.95, method = "mpu", data = sample, materiality = 0.1, values = "ist", values.audit = "soll")
  expect_equal(jfaEval[["ub"]], 0.003970126, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.5.0-t1) Test for frequentist summary and print function", {
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
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
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "binomial", prior = TRUE, materiality = 0.05)
  invisible(capture.output(print(jfaEval)))
  invisible(capture.output(summary(jfaEval)))
  invisible(capture.output(print(jfaEval[["posterior"]])))
  invisible(capture.output(summary(jfaEval[["posterior"]])))
  expect_equal(jfaEval[["ub"]], 0.04792395, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.2.0-t4) Test for Bayesian plot function", {
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  samp <- selection(population, size = 100, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "binomial", prior = TRUE, materiality = 0.05)
  p <- plot(jfaEval)
  expect_equal(is.null(p), FALSE)
  p <- plot(jfaEval[["prior"]])
  expect_equal(is.null(p), FALSE)
  p <- plot(jfaEval[["posterior"]])
  expect_equal(is.null(p), FALSE)
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "poisson", prior = TRUE, materiality = 0.05)
  p <- plot(jfaEval)
  expect_equal(is.null(p), FALSE)
  p <- plot(jfaEval[["prior"]])
  expect_equal(is.null(p), FALSE)
  p <- plot(jfaEval[["posterior"]])
  expect_equal(is.null(p), FALSE)
  prior <- auditPrior(method = "default", likelihood = "hypergeometric", N.units = nrow(population))
  jfaEval <- evaluation(conf.level = 0.95, data = samp, values = "bookValue", values.audit = "auditValue", method = "hypergeometric", prior = prior, materiality = 0.05, N.units = nrow(population))
  p <- plot(jfaEval)
  expect_equal(is.null(p), FALSE)
  p <- plot(jfaEval[["prior"]])
  expect_equal(is.null(p), FALSE)
  p <- plot(jfaEval[["posterior"]])
  expect_equal(is.null(p), FALSE)
})

# jfa version 0.5.1 - 0.5.7
# No changes to be benchmarked

# jfa 0.6.0

test_that(desc = "(id: f3-v0.6.0-t1) Test Bayes factors for beta prior", {
  # Compute a Bayes factor from a noninformative beta(1, 1) prior, n = 160, k = 1
  # Derks, K., de Swart, J., Wagenmakers, E-.J., and Wetzels, R. (2021). The Bayesian approach to audit evidence: Quantifying statistical evidence using the Bayes factor.
  # BF-+ = 696.696
  BF <- evaluation(materiality = 0.03, n = 160, x = 1, prior = auditPrior(method = "param", alpha = 1, beta = 1, likelihood = "binomial"))$posterior$hypotheses$bf.h1
  expect_equal(BF, 696.696)

  # Compute a Bayes factor from a noninformative beta(1, 1) prior, n = 50, k = 1
  # Derks, K., de Swart, J., Wagenmakers, E-.J., and Wetzels, R. (2022). An impartial Bayesian hypothesis test for audit sampling.
  # BF-+ = 51.55
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = "param", alpha = 1, beta = 1, likelihood = "binomial"))$posterior$hypotheses$bf.h1
  expect_equal(BF, 51.551344113750538)

  # Compute a default Bayes factor from an impartial beta prior
  # Derks, K., de Swart, J., Wagenmakers, E-.J., and Wetzels, R. (2022). An impartial Bayesian hypothesis test for audit sampling.
  # BF-+ = 4.98
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = "impartial", materiality = 0.05, likelihood = "binomial"))$posterior$hypotheses$bf.h1
  expect_equal(BF, 4.9852019781149854)

  # Compute a default Bayes factor from an improper beta prior
  # BF-+ = 2.58
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = "param", alpha = 1, beta = 0, likelihood = "binomial"))$posterior$hypotheses$odds.h1
  expect_equal(BF, 2.578691368)
})

test_that(desc = "(id: f3-v0.6.0-t2) Test Bayes factors for gamma prior", {
  # Compute a Bayes factor from a noninformative gamma(1, 0) prior
  BF <- evaluation(materiality = 0.03, n = 160, x = 1, prior = auditPrior(method = "strict", likelihood = "poisson"))$posterior$hypotheses$odds.h1
  expect_equal(BF, 19.95007199)

  # Compute a Bayes factor from a noninformative gamma(1, 0) prior
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = "strict", likelihood = "poisson"))$posterior$hypotheses$odds.h1
  expect_equal(BF, 2.48071256)

  # Compute a default Bayes factor from an impartial gamma prior
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = "impartial", materiality = 0.05, likelihood = "poisson"))$posterior$hypotheses$bf.h1
  expect_equal(BF, 4.810668425)
})

test_that(desc = "(id: f3-v0.6.0-t3) Test Bayes factors for beta-binomial prior", {
  testthat::skip_on_cran()
  # Compute a Bayes factor from a noninformative beta-binomial prior
  BF <- evaluation(materiality = 0.03, n = 160, x = 1, prior = auditPrior(method = "default", likelihood = "hypergeometric", N.units = 1000))$posterior$hypotheses$bf.h1
  expect_equal(BF, 943.4715321)

  # Compute a Bayes factor from a noninformative beta-binomial prior
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = "default", likelihood = "hypergeometric", N.units = 1000))$posterior$hypotheses$bf.h1
  expect_equal(BF, 53.59899136)

  # Compute a default Bayes factor from an impartial beta-binomial prior
  BF <- evaluation(materiality = 0.05, n = 50, x = 1, prior = auditPrior(method = "impartial", materiality = 0.05, likelihood = "hypergeometric", N.units = 1000))$posterior$hypotheses$bf.h1
  expect_equal(BF, 5.162566438)

  BF <- evaluation(materiality = 0.5, n = 2, x = 1, N.units = 11, method = "hypergeometric", prior = TRUE)$posterior$hypotheses$bf.h1
  expect_equal(BF, 1)
})

# jfa 0.6.1 - 0.6.4
# No changes to be benchmarked

# jfa 0.6.5

test_that(desc = "(id: f3-v0.6.5-t1) Test frequentist poisson stratification with summary statistics (Derks et al., 2022, Table 1)", {
  testthat::skip_on_cran()
  k <- c(2, 1, 0)
  n <- c(6, 7, 7)
  N <- c(8, 11, 11)
  # 1. Complete pooling
  res <- evaluation(materiality = 0.03, x = k, n = n, N.units = N, method = "poisson", pooling = "complete")
  expect_equal(res$mle, sum(k) / sum(n))
  expect_equal(res$ub, stats::qgamma(0.95, 1 + sum(k), sum(n)))
  expect_equal(res$p.value, stats::poisson.test(x = sum(k), T = sum(n), r = 0.03, alternative = "less")$p.value)
  # 2. No pooling
  set.seed(1) # Required because the population posterior is generated through sampling
  res <- evaluation(materiality = 0.03, x = k, n = n, N.units = N, method = "poisson", pooling = "none")
  expect_equal(res$mle, as.numeric((k / n) %*% N / sum(N)))
  expect_equal(res$ub, 0.5131687579)
  expect_equal(res$p.value, NA)
  expect_equal(res$strata$mle, k / n)
  expect_equal(res$strata$ub, stats::qgamma(0.95, 1 + k, n))
  expect_equal(res$strata$p.value, stats::pgamma(0.03, 1 + k, n, lower.tail = FALSE))
})

test_that(desc = "(id: f3-v0.6.5-t2) Test Bayesian poisson stratification with summary statistics (Derks et al., 2022, Table 1)", {
  testthat::skip_on_cran()
  options("mc.iterations" = 200, "mc.warmup" = 100, "mc.chains" = 1)
  k <- c(2, 1, 0)
  n <- c(6, 7, 7)
  N <- c(8, 11, 11)
  # 1. Complete pooling
  res <- evaluation(materiality = 0.03, x = k, n = n, N.units = N, method = "poisson", prior = auditPrior(method = "strict"), pooling = "complete")
  expect_equal(res$mle, sum(k) / sum(n))
  expect_equal(res$ub, stats::qgamma(0.95, 1 + sum(k), sum(n)))
  expect_equal(res$posterior$hypotheses$bf.h1, Inf)
  # 2. No pooling
  set.seed(1) # Required because the population posterior is generated through sampling
  res <- evaluation(materiality = 0.03, x = k, n = n, N.units = N, method = "poisson", prior = auditPrior(method = "strict"), pooling = "none")
  expect_equal(res$mle, 0.2783)
  expect_equal(res$ub, 0.5101278778)
  expect_equal(res$posterior$hypotheses$bf.h1, Inf)
  expect_equal(res$strata$mle, k / n)
  expect_equal(res$strata$ub, stats::qgamma(0.95, 1 + k, n))
  expect_equal(res$strata$bf10, c(Inf, Inf, Inf))
  # 3. Partial pooling
  res <- evaluation(materiality = 0.03, x = k, n = n, N.units = N, method = "poisson", prior = TRUE, pooling = "partial")
  expect_equal(is.data.frame(res$strata), TRUE)
  # We do not test these results because of differences in OS's due to sampling
})

test_that(desc = "(id: f3-v0.6.5-t3) Test frequentist binomial stratification with summary statistics (Derks et al., 2022, Table 1)", {
  testthat::skip_on_cran()
  k <- c(2, 1, 0)
  n <- c(6, 7, 7)
  N <- c(8, 11, 11)
  # 1. Complete pooling
  res <- evaluation(materiality = 0.03, x = k, n = n, N.units = N, method = "binomial", pooling = "complete")
  expect_equal(res$mle, sum(k) / sum(n))
  expect_equal(res$ub, stats::qbeta(0.95, 1 + sum(k), sum(n) - sum(k)))
  expect_equal(res$p.value, stats::binom.test(x = sum(k), n = sum(n), p = 0.03, alternative = "less")$p.value)
  # 2. No pooling
  set.seed(1) # Required because the population posterior is generated through sampling
  res <- evaluation(materiality = 0.03, x = k, n = n, N.units = N, method = "binomial", pooling = "none")
  expect_equal(res$mle, as.numeric((k / n) %*% N / sum(N)))
  expect_equal(res$ub, 0.3945103587)
  expect_equal(res$p.value, NA)
  expect_equal(res$strata$mle, k / n)
  expect_equal(res$strata$ub, stats::qbeta(0.95, 1 + k, n - k))
  expect_equal(res$strata$p.value, stats::pbeta(0.03, 1 + k, n - k, lower.tail = FALSE))
})

test_that(desc = "(id: f3-v0.6.5-t4) Test Bayesian binomial stratification with summary statistics (Derks et al., 2022, Table 1)", {
  testthat::skip_on_cran()
  options("mc.iterations" = 200, "mc.warmup" = 100, "mc.chains" = 1)
  k <- c(2, 1, 0)
  n <- c(6, 7, 7)
  N <- c(8, 11, 11)
  # 1. Complete pooling
  res <- evaluation(materiality = 0.03, x = k, n = n, N.units = N, method = "binomial", prior = TRUE, pooling = "complete")
  expect_equal(res$mle, sum(k) / sum(n))
  expect_equal(res$ub, stats::qbeta(0.95, 1 + sum(k), 1 + sum(n) - sum(k)))
  expect_equal(res$posterior$hypotheses$bf.h1, 0.1044176562)
  # 2. No pooling
  set.seed(1) # Required because the population posterior is generated through sampling
  res <- evaluation(materiality = 0.03, x = k, n = n, N.units = N, method = "binomial", prior = TRUE, pooling = "none")
  expect_equal(res$mle, 0.1784)
  expect_equal(res$ub, 0.3536624007)
  expect_equal(res$strata$mle, k / n)
  expect_equal(res$strata$ub, stats::qbeta(0.95, 1 + k, 1 + n - k))
  expect_equal(res$strata$bf10, c(0.02792650833, 0.73885781280, 8.92166799329))
  # 3. Partial pooling
  res <- evaluation(materiality = 0.03, x = k, n = n, N.units = N, method = "binomial", prior = TRUE, pooling = "partial")
  expect_equal(is.data.frame(res$strata), TRUE)
  # We do not test these results because of differences in OS's due to sampling
})

test_that(desc = "(id: f3-v0.6.5-t5) Test stratification with data (Derks et al., 2022, Table 4)", {
  testthat::skip_on_cran()
  options("mc.iterations" = 200, "mc.warmup" = 100, "mc.chains" = 1)
  data("BuildIt")
  BuildIt$stratum <- factor(c("high", "medium", rep(c("low", "medium", "high"), times = 1166)))
  BuildIt$inSample <- c(rep(1, 100), rep(0, 3400))
  BuildIt_sample <- subset(BuildIt, BuildIt$inSample == 1)
  BuildIt_sample$wholeError <- ifelse(BuildIt_sample$bookValue != BuildIt_sample$auditValue, yes = 0, no = BuildIt_sample$auditValue)
  # 1. Complete pooling
  res <- evaluation(
    materiality = 0.03, data = BuildIt_sample, prior = TRUE, method = "binomial",
    values = "bookValue", values.audit = "auditValue", strata = "stratum",
    N.units = as.numeric(table(BuildIt_sample$stratum)), pooling = "complete"
  )
  expect_equal(res$mle, 0.02999982414)
  expect_equal(res$ub, 0.07497867578)
  # 2. No pooling
  set.seed(1)
  res <- evaluation(
    materiality = 0.03, data = BuildIt_sample, prior = TRUE, method = "binomial",
    values = "bookValue", values.audit = "auditValue", strata = "stratum",
    N.units = as.numeric(table(BuildIt_sample$stratum)), pooling = "none"
  )
  expect_equal(res$mle, 0.0465)
  expect_equal(res$ub, 0.09724879894)
  expect_equal(res$strata$mle, c(0.01818169065, 0.03636289354, 0.03529444518))
  expect_equal(res$strata$ub, c(0.1140247217, 0.1407511113, 0.1369524323))
  # 3. Partial pooling (partial errors)
  res <- evaluation(
    materiality = 0.03, data = BuildIt_sample, prior = TRUE, method = "binomial",
    values = "bookValue", values.audit = "auditValue", strata = "stratum",
    N.units = as.numeric(table(BuildIt_sample$stratum)), pooling = "partial"
  )
  expect_equal(is.data.frame(res$strata), TRUE)
  # We do not test these results because of differences in OS's due to sampling
  # 4. Partial pooling (whole errors)
  res <- evaluation(
    materiality = 0.03, data = BuildIt_sample, prior = TRUE, method = "binomial",
    values = "bookValue", values.audit = "wholeError", strata = "stratum",
    N.units = as.numeric(table(BuildIt_sample$stratum)), pooling = "partial"
  )
  expect_equal(is.data.frame(res$strata), TRUE)
  # We do not test these results because of differences in OS's due to sampling
})

test_that(desc = "(id: f3-v0.6.5-t6) Validate poststratification with stan examples", {
  testthat::skip_on_cran()
  # https://mc-stan.org/docs/2_23/stan-users-guide/some-examples.html
  # 28.1.2 Polling
  set.seed(1)
  n <- c(490, 112, 47)
  x <- c(0.25 * n[1], 0.4 * n[2], 0.8 * n[3])
  N <- c(20000, 5000, 2000)
  prior <- auditPrior(method = "strict", likelihood = "binomial")
  res <- evaluation(materiality = 0.03, n = n, x = x, N.units = N, prior = prior)
  expect_equal(res$mle, 0.32, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.6.5-t7) Test evaluation with non-conjugate priors", {
  testthat::skip_on_cran()
  # Cannot test for consistency due to sampling differences on OS's
  prior <- auditPrior(method = "param", likelihood = "normal", alpha = 0.025, beta = 0.05)
  res <- evaluation(materiality = 0.03, x = 0, n = 53, prior = prior, method = "binomial")
  expect_equal(!is.null(res), TRUE)
  prior <- auditPrior(method = "param", likelihood = "uniform", alpha = 0, beta = 0.5)
  res <- evaluation(materiality = 0.03, x = 0, n = 53, prior = prior, method = "binomial")
  expect_equal(!is.null(res), TRUE)
  prior <- auditPrior(method = "param", likelihood = "cauchy", alpha = 0, beta = 0.5)
  res <- evaluation(materiality = 0.03, x = 0, n = 53, prior = prior, method = "binomial")
  expect_equal(!is.null(res), TRUE)
  prior <- auditPrior(method = "param", likelihood = "t", alpha = 1)
  res <- evaluation(materiality = 0.03, x = 0, n = 53, prior = prior, method = "binomial")
  expect_equal(!is.null(res), TRUE)
  prior <- auditPrior(method = "param", likelihood = "chisq", alpha = 1)
  res <- evaluation(materiality = 0.03, x = 0, n = 53, prior = prior, method = "binomial")
  expect_equal(!is.null(res), TRUE)
})

test_that(desc = "(id: f3-v0.6.5-t8) Test hypergeometric and beta-binomial", {
  testthat::skip_on_cran()
  N <- 10
  n <- 2
  x <- 1
  x1 <- evaluation(x = x, n = n, method = "hypergeometric", N.units = N)
  expect_equal(x1[["mle"]], 0.5)
  expect_equal(x1[["ub"]], 0.9)
  x2 <- evaluation(x = x, n = n, method = "hypergeometric", N.units = N, prior = TRUE)
  expect_equal(x1[["mle"]], x2[["mle"]])
  expect_equal(x1[["ub"]], x2[["ub"]])
  N <- 20
  n <- 10
  x <- 1
  x1 <- evaluation(x = x, n = n, method = "hypergeometric", N.units = N)
  expect_equal(x1[["mle"]], 0.1)
  expect_equal(x1[["ub"]], 0.3)
  x2 <- evaluation(x = x, n = n, method = "hypergeometric", N.units = N, prior = TRUE)
  expect_equal(x1[["mle"]], x2[["mle"]])
  expect_equal(x1[["ub"]], x2[["ub"]])
})

test_that(desc = "(id: f3-v0.6.5-t9) Test Bayesian evaluation with different uniform priors, 5% materiality", {
  testthat::skip_on_cran()
  set.seed(1)
  prior1 <- auditPrior(method = "default", likelihood = "binomial")
  prior2 <- auditPrior(method = "param", likelihood = "uniform", alpha = 0, beta = 1)
  prior3 <- auditPrior(method = "nonparam", samples = seq(0, 1, length.out = 10001))
  prior4 <- auditPrior(method = "nonparam", samples = stats::runif(1000000, 0, 1))
  prior5 <- auditPrior(method = "nonparam", samples = stats::rbeta(1000000, 1, 1))
  priors <- list(prior1, prior2, prior3, prior4, prior5)
  for (i in 1:5) {
    res <- evaluation(materiality = 0.05, prior = priors[[i]], method = "binomial", x = 1, n = 100)
    expect_equal(res[["mle"]], 0.01, tolerance = 0.01)
  }
})

# jfa 0.7.0

test_that(desc = "(id: f3-v0.7.0-t1) Evaluation with stringer.poisson method", {
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = stats::runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "stringer.poisson")
  expect_equal(jfaEval[["ub"]], 0.04992887, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.7.0-t2) Evaluation with stringer.hypergeometric method", {
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = stats::runif(n = 1000, min = 100, max = 500))
  jfaRes <- planning(conf.level = 0.95, materiality = 0.05, likelihood = "poisson")
  samp <- selection(population, size = jfaRes, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]]
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "stringer.hypergeometric", N.units = sum(population$bookValue))
  expect_equal(jfaEval[["ub"]], 0.04869673, tolerance = 0.001)
})

# jfa 0.7.1

test_that(desc = "(id: f3-v0.7.0-t1) Evaluation with inflated.poisson method", {
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = stats::runif(n = 1000, min = 100, max = 500))
  samp <- selection(population, size = 60, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]] * (1 - stats::rnorm(nrow(samp), 0.5, 0.3) * stats::rbinom(nrow(samp), 1, 0.03))
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "inflated.poisson", N.items = nrow(population), N.units = sum(population[["bookValue"]]))
  expect_equal(jfaEval[["mle"]], 0.04524783, tolerance = 0.001)
})

test_that(desc = "(id: f3-v0.7.0-t2) Evaluation with hurdle.beta method", {
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = stats::runif(n = 1000, min = 100, max = 500))
  samp <- selection(population, size = 60, units = "items", method = "random")$sample
  samp$auditValue <- samp[["bookValue"]] * (1 - stats::rnorm(nrow(samp), 0.5, 0.3) * stats::rbinom(nrow(samp), 1, 0.03))
  jfaEval <- evaluation(conf.level = 0.95, materiality = 0.05, data = samp, values = "bookValue", values.audit = "auditValue", method = "hurdle.beta")
  expect_equal(jfaEval[["mle"]], 0.03191938, tolerance = 0.001)
})
