# Copyright (C) 2020-2022 Koen Derks

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

context("Consistency of function planning()")

# jfa version 0.1.0

test_that(desc = "(id: f5-v0.1.0-t1) Frequentist Poisson 1% materiality", {
  jfaRes <- planning(materiality = 0.01, conf.level = 0.95, expected = 0, likelihood = "poisson")
  expect_equal(jfaRes[["n"]], 300)
  expect_equal(jfaRes[["x"]], 0)
})

test_that(desc = "(id: f5-v0.1.0-t2) Frequentist Poisson 5% materiality", {
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 0, likelihood = "poisson")
  expect_equal(jfaRes[["n"]], 60)
  expect_equal(jfaRes[["x"]], 0)
})

test_that(desc = "(id: f5-v0.1.0-t3) Frequentist Poisson 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 0.025, likelihood = "poisson")
  expect_equal(jfaRes[["n"]], 231)
  expect_equal(jfaRes[["x"]], 5.78, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t4) Frequentist Poisson 5% materiality 2% errors", {
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 2, likelihood = "poisson")
  expect_equal(jfaRes[["n"]], 126)
  expect_equal(jfaRes[["x"]], 2)
})

test_that(desc = "(id: f5-v0.1.0-t5) Bayesian Poisson 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 0.025, likelihood = "poisson", prior = TRUE)
  expect_equal(jfaRes[["n"]], 228)
  expect_equal(jfaRes[["x"]], 5.7, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t7) Frequentist binomial 1% materiality", {
  jfaRes <- planning(materiality = 0.01, conf.level = 0.95, expected = 0, likelihood = "binomial")
  expect_equal(jfaRes[["n"]], 299)
  expect_equal(jfaRes[["x"]], 0)
})

test_that(desc = "(id: f5-v0.1.0-t8) Frequentist binomial 5% materiality", {
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 0, likelihood = "binomial")
  expect_equal(jfaRes[["n"]], 59)
  expect_equal(jfaRes[["x"]], 0)
})

test_that(desc = "(id: f5-v0.1.0-t9) Frequentist binomial 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 0.025, likelihood = "binomial")
  expect_equal(jfaRes[["n"]], 234)
  expect_equal(jfaRes[["x"]], 6, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t10) Frequentist binomial 5% materiality 2 errors", {
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 2, likelihood = "binomial")
  expect_equal(jfaRes[["n"]], 124)
  expect_equal(jfaRes[["x"]], 2)
})

test_that(desc = "(id: f5-v0.1.0-t11) Bayesian binomial 5% materiality 2.5% errors", {
  prior <- auditPrior(method = "default", likelihood = "binomial")
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 0.025, likelihood = "binomial", prior = prior)
  expect_equal(jfaRes[["n"]], 220)
  expect_equal(jfaRes[["x"]], 5.5, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t13) Frequentist hypergeometric 1% materiality", {
  jfaRes <- planning(materiality = 0.01, conf.level = 0.95, expected = 0, likelihood = "hypergeometric", N.units = 1000)
  expect_equal(jfaRes[["n"]], 258)
  expect_equal(jfaRes[["x"]], 0)
})

test_that(desc = "(id: f5-v0.1.0-t14) Frequentist hypergeometric 5% materiality", {
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 0, likelihood = "hypergeometric", N.units = 1000)
  expect_equal(jfaRes[["n"]], 57)
  expect_equal(jfaRes[["x"]], 0)
})

test_that(desc = "(id: f5-v0.1.0-t15) Frequentist hypergeometric 5% materiality 2.5% errors", {
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 0.025, likelihood = "hypergeometric", N.units = 1000)
  expect_equal(jfaRes[["n"]], 197)
  expect_equal(jfaRes[["x"]], 5, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t16) Frequentist hypergeometric 5% materiality 2 errors", {
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 2, likelihood = "hypergeometric", N.units = 1000)
  expect_equal(jfaRes[["n"]], 119)
  expect_equal(jfaRes[["x"]], 2)
})

test_that(desc = "(id: f5-v0.1.0-t17) Bayesian hypergeometric 5% materiality 2.5% errors", {
  prior <- auditPrior(method = "default", likelihood = "hypergeometric", N.units = 1000)
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 0.025, likelihood = "hypergeometric", N.units = 1000, prior = prior)
  expect_equal(jfaRes[["n"]], 159)
  expect_equal(jfaRes[["x"]], 4, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t19) Bayesian binomial 2% precision 5% materiality 2.5% errors impartial prior", {
  jfaPrior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "impartial", expected = 0.025, likelihood = "binomial")
  jfaRes <- planning(materiality = 0.05, min.precision = 0.02, conf.level = 0.95, expected = 0.025, likelihood = "binomial", N.units = 1000, prior = jfaPrior)
  expect_equal(jfaRes[["n"]], 284)
  expect_equal(jfaRes[["x"]], 7.1, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t20) Bayesian binomial 5% materiality 1% errors hypotheses prior", {
  jfaPrior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "hyp", p.hmin = 0.7, likelihood = "binomial")
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 0.01, likelihood = "binomial", N.units = 1000, prior = jfaPrior)
  expect_equal(jfaRes[["n"]], 55)
  expect_equal(jfaRes[["x"]], 0.55, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t21) Bayesian poisson 5% materiality 1% errors factor prior", {
  jfaPrior <- auditPrior(materiality = 0.05, conf.level = 0.95, method = "factor", factor = 0.6, n = 58, x = 0, expected = 0.025, likelihood = "binomial")
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 0.01, likelihood = "binomial", N.units = 1000, prior = jfaPrior)
  expect_equal(jfaRes[["n"]], 38)
  expect_equal(jfaRes[["x"]], 0.38, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t22) Bayesian poisson 3% materiality 1.3% errors sample prior", {
  jfaPrior <- auditPrior(materiality = 0.03, conf.level = 0.95, method = "sample", n = 58, x = 0, expected = 0.025, likelihood = "binomial")
  jfaRes <- planning(materiality = 0.03, conf.level = 0.95, expected = 0.013, likelihood = "binomial", N.units = 1000, prior = jfaPrior)
  expect_equal(jfaRes[["n"]], 143)
  expect_equal(jfaRes[["x"]], 1.86, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.1.0-t23) Frequentist poisson 3% materiality 1.3% errors 5% min precision", {
  jfaRes <- planning(materiality = 0.03, conf.level = 0.95, expected = 0.013, min.precision = 0.05, likelihood = "binomial", N.units = 1000)
  expect_equal(jfaRes[["n"]], 303)
  expect_equal(jfaRes[["x"]], 4, tolerance = 0.001)
})

# jfa version 0.2.0

test_that(desc = "(id: f5-v0.2.0-t1) Bayesian poisson 3% materiality 1.3% errors 5% min precision standard prior", {
  prior <- auditPrior(method = "default", likelihood = "binomial")
  jfaRes <- planning(materiality = 0.03, conf.level = 0.95, expected = 0.013, min.precision = 0.05, likelihood = "binomial", N.units = 1000, prior = prior)
  expect_equal(jfaRes[["n"]], 293)
  expect_equal(jfaRes[["x"]], 3.81, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.2.0-t2) Bayesian poisson 100% materiality 0% errors 2% min precision standard prior", {
  jfaRes <- planning(conf.level = 0.95, min.precision = 0.02, likelihood = "poisson", prior = TRUE)
  expect_equal(jfaRes[["n"]], 149)
  expect_equal(jfaRes[["x"]], 0)
})

test_that(desc = "(id: f5-v0.2.0-t3) Bayesian poisson 100% materiality 1% errors 2% min precision standard prior", {
  jfaRes <- planning(conf.level = 0.95, min.precision = 0.02, expected = 0.01, likelihood = "poisson", prior = TRUE)
  expect_equal(jfaRes[["n"]], 219)
  expect_equal(jfaRes[["x"]], 2.19, tolerance = 0.001)
})

# jfa version 0.3.0 - 0.3.1
# No changes to be benchmarked

# jfa version 0.4.0

test_that(desc = "(id: f5-v0.4.0-t1) Expected Bayes factors for zero expected errors", {
  prior <- auditPrior(method = "strict", likelihood = "poisson")
  jfaRes <- planning(conf.level = 0.95, materiality = 0.02, expected = 0, likelihood = "poisson", prior = prior)
  expect_equal(jfaRes[["posterior"]][["hypotheses"]]$odds.h1, 19.08554, tolerance = 0.001)

  prior <- auditPrior(method = "default", likelihood = "binomial")
  jfaRes <- planning(conf.level = 0.95, materiality = 0.02, expected = 0, likelihood = "binomial", prior = prior)
  expect_equal(jfaRes[["posterior"]][["hypotheses"]]$bf.h1, 945.2848, tolerance = 0.001)

  prior <- auditPrior(method = "default", likelihood = "hypergeometric", N.units = 1000)
  jfaRes <- planning(conf.level = 0.95, materiality = 0.02, expected = 0, likelihood = "hypergeometric", prior = prior, N.units = 1000)
  expect_equal(jfaRes[["posterior"]][["hypotheses"]]$bf.h1, 933.7705249, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.4.0-t2) Expected Bayes factors for expected errors > 0", {
  prior <- auditPrior(method = "strict", likelihood = "poisson")
  jfaRes <- planning(conf.level = 0.95, materiality = 0.02, expected = 0.01, likelihood = "poisson", prior = prior)
  expect_equal(jfaRes[["posterior"]][["hypotheses"]]$odds.h1, 19.01191777, tolerance = 0.001)

  prior <- auditPrior(method = "default", likelihood = "binomial")
  jfaRes <- planning(conf.level = 0.95, materiality = 0.02, expected = 0.01, likelihood = "binomial", prior = prior)
  expect_equal(jfaRes[["posterior"]][["hypotheses"]]$bf.h1, 933.3458, tolerance = 0.001)

  prior <- auditPrior(method = "default", likelihood = "hypergeometric", N.units = 1000)
  jfaRes <- planning(conf.level = 0.95, materiality = 0.02, expected = 0.01, likelihood = "hypergeometric", prior = prior, N.units = 1000)
  expect_equal(jfaRes[["posterior"]][["hypotheses"]]$bf.h1, 948.2315255, tolerance = 0.001)
})

test_that(desc = "(id: f5-v0.4.0-t3) Expected Bayes factors for impartial priors", {
  prior <- auditPrior(materiality = 0.02, conf.level = 0.95, method = "impartial", likelihood = "poisson")
  jfaRes <- planning(conf.level = 0.95, materiality = 0.02, prior = prior)
  expect_equal(jfaRes[["posterior"]][["hypotheses"]]$bf.h1, 19.35135, tolerance = 0.001)

  prior <- auditPrior(materiality = 0.02, conf.level = 0.95, method = "impartial", likelihood = "binomial")
  jfaRes <- planning(conf.level = 0.95, materiality = 0.02, prior = prior)
  expect_equal(jfaRes[["posterior"]][["hypotheses"]]$bf.h1, 19.01047, tolerance = 0.001)
})

# jfa version 0.5.0

test_that(desc = "(id: f5-v0.5.0-t1) Test for frequentist summary and print function", {
  jfaRes <- planning(materiality = 0.01, conf.level = 0.95, expected = 0, likelihood = "poisson")
  invisible(capture.output(print(jfaRes)))
  invisible(capture.output(summary(jfaRes)))
  expect_equal(jfaRes[["n"]], 300)
  expect_equal(jfaRes[["x"]], 0)
})

test_that(desc = "(id: f5-v0.5.0-t2) Test for Bayesian summary and print function", {
  jfaRes <- planning(materiality = 0.01, conf.level = 0.95, expected = 0, likelihood = "poisson", prior = TRUE)
  invisible(capture.output(print(jfaRes)))
  invisible(capture.output(summary(jfaRes)))
  invisible(capture.output(print(jfaRes[["posterior"]])))
  invisible(capture.output(summary(jfaRes[["posterior"]])))
  expect_equal(jfaRes[["n"]], 299)
  expect_equal(jfaRes[["x"]], 0)
})

test_that(desc = "(id: f5-v0.5.0-t3) Test for frequentist plot function", {
  jfaRes <- planning(materiality = 0.01, conf.level = 0.95, expected = 0, likelihood = "poisson")
  invisible(capture.output(plot(jfaRes)))
  expect_equal(jfaRes[["x"]], 0)

  jfaRes <- planning(materiality = 0.01, conf.level = 0.95, expected = 0, likelihood = "binomial")
  invisible(capture.output(plot(jfaRes)))
  expect_equal(jfaRes[["x"]], 0)

  jfaRes <- planning(materiality = 0.01, conf.level = 0.95, expected = 0, likelihood = "hypergeometric", N.units = 1000)
  invisible(capture.output(plot(jfaRes)))
  expect_equal(jfaRes[["x"]], 0)
})

test_that(desc = "(id: f5-v0.5.0-t4) Test for Bayesian plot function", {
  jfaRes <- planning(min.precision = 0.02, conf.level = 0.95, expected = 0, likelihood = "poisson", prior = TRUE)
  invisible(capture.output(plot(jfaRes)))
  invisible(capture.output(plot(jfaRes[["posterior"]])))
  expect_equal(jfaRes[["x"]], 0)

  jfaRes <- planning(materiality = 0.01, conf.level = 0.95, expected = 0, likelihood = "binomial", prior = TRUE)
  invisible(capture.output(plot(jfaRes)))
  invisible(capture.output(plot(jfaRes[["posterior"]])))
  expect_equal(jfaRes[["x"]], 0)

  jfaRes <- planning(materiality = 0.01, conf.level = 0.95, expected = 0, likelihood = "hypergeometric", prior = TRUE, N.units = 1000)
  invisible(capture.output(plot(jfaRes)))
  invisible(capture.output(plot(jfaRes[["posterior"]])))
  expect_equal(jfaRes[["x"]], 0)
})

# jfa version 0.5.1
# No changes to be benchmarked

# jfa version 0.5.2

test_that(desc = "(id: f5-v0.5.2-t1) Test for change in Hypergeometric mode calculation", {
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 0, likelihood = "hypergeometric", N.units = 10000)
  modeDist <- ceiling((jfaRes[["ub"]] - jfaRes[["precision"]]) * 10000)
  expect_equal(jfaRes[["n"]], 59)
  expect_equal(modeDist, 0)
})

test_that(desc = "(id: f5-v0.5.2-t2) Test for change in beta-binomial mode calculation", {
  prior <- auditPrior(method = "default", likelihood = "hypergeometric", N.units = 10000)
  jfaRes <- planning(materiality = 0.05, conf.level = 0.95, expected = 0, likelihood = "hypergeometric", N = 10000, prior = prior)
  modeDist <- ceiling((jfaRes[["ub"]] - jfaRes[["precision"]]) * 10000)
  expect_equal(jfaRes[["n"]], 58)
  expect_equal(modeDist, 0)
})

# jfa version 0.5.3 - 0.6.1
# No changes to be benchmarked