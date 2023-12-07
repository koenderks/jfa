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

context("Benchmark against Touw and Hoogduin (2011)")

# Touw, P., and Hoogduin, L. (2011). Statistiek voor audit en controlling. Boom uitgevers, Amsterdam.

# Table 1.3 on page 17
# Touw & Hoogduin:
#            | Materiality
# Confidence   0.01
# 0.95          300
# 0.90          230
# 0.75          140
# 0.60           90
# 0.50           70

# jfa:
#            | Materiality
# Confidence   0.01
# 0.95          300
# 0.9           231
# 0.75          139
# 0.6            92
# 0.5            70

test_that(desc = "(id: f14-v0.5.1-t1) Test Sample sizes on page 17", {
  testthat::skip_on_cran()
  SR <- c(0.05, 0.10, 0.25, 0.40, 0.50)
  materiality <- 0.01
  n <- numeric(length(SR))
  for (i in seq_along(SR)) {
    n[i] <- planning(conf.level = 1 - SR[i], materiality = materiality, likelihood = "poisson")$n
  }
  expect_equal(n, c(300, 230 + 1, 140 - 1, 90 + 2, 70))
  # Second entry: 230 --> 231 as qgamma(p = 0.90, shape = 1, rate = 230) > 0.1
  # Third entry:  140 --> 139 as qgamma(p = 0.75, shape = 1, rate = 139) < 0.1
  # Fourth entry: 90  --> 92 as qgamma(p = 0.6, shape = 1, rate = 90) > 0.1
})

# Example on page 23
# Touw & Hoogduin:
#            | Materiality
# Confidence   0.02
# 0.95          150
# 0.875         104

# jfa:
#            | Materiality
# Confidence   0.02
# 0.95          150
# 0.875         104

test_that(desc = "(id: f14-v0.5.1-t2) Test Sample sizes on page 23", {
  n <- planning(conf.level = 1 - 0.05, materiality = 100000 / 5000000, likelihood = "poisson")$n
  expect_equal(n, 150)
  n <- planning(conf.level = 1 - 0.125, materiality = 100000 / 5000000, likelihood = "poisson")$n
  expect_equal(n, 104)
})

# Chapter 1
test_that(desc = "(id: f14-v0.7.1-t3) Test chapter 1", {
  testthat::skip_on_cran()
  # Page 31
  population <- data.frame(id = LETTERS[seq_len(5)], value = c(700, 750, 1000, 900, 6000))
  sample <- selection(data = population, size = 2, units = "items", start = 1.2)$sample
  expect_equal(sample[["id"]], c("B", "D"))
  # Page 34
  population <- data.frame(id = LETTERS[seq_len(10)], value = c(7, 3, 4, 9, 0, 1, 8, 2, 6, 5))
  sample <- selection(data = population, size = 5, units = "values", values = "value", start = 2)$sample
  expect_equal(sample[["id"]], c("A", "C", "D", "G", "I"))
})

# Chapter 5
test_that(desc = "(id: f14-v0.7.1-t4) Test chapter 5", {
  testthat::skip_on_cran()
  # Page 123
  # Example 1: book: n = 45, jfa: n = 45
  n <- planning(materiality = 60 / 1200, expected = 0, N.units = 1200, conf.level = 0.90, likelihood = "hypergeometric")$n
  expect_equal(n, 45)
  # Example 2: book: n = 102, jfa: n = 102
  n <- planning(materiality = 60 / 1200, expected = 2, N.units = 1200, conf.level = 0.90, likelihood = "hypergeometric")$n
  expect_equal(n, 102)
  # Page 125
  # Table row 1: book: ub = 0.0208, jfa: ub = 0.0208
  ub <- evaluation(n = 102, x = 0, N.units = 1200, conf.level = 0.90, method = "hypergeometric")$ub
  expect_equal(ub, 0.02083333)
  # Table row 2: book: ub = 0.0358, jfa: ub = 0.03583333
  ub <- evaluation(n = 102, x = 1, N.units = 1200, conf.level = 0.90, method = "hypergeometric")$ub
  expect_equal(ub, 0.03583333)
  # Table row 3: book: ub = 0.0492, jfa: ub = 0.04916667
  ub <- evaluation(n = 102, x = 2, N.units = 1200, conf.level = 0.90, method = "hypergeometric")$ub
  expect_equal(ub, 0.04916667)
  # Table row 4: book: ub = 0.0625, jfa: ub = 0.0625
  ub <- evaluation(n = 102, x = 3, N.units = 1200, conf.level = 0.90, method = "hypergeometric")$ub
  expect_equal(ub, 0.0625)

  # Page 126
  # Table 1 first column: n = 299, 473, 628, 773
  n <- planning(materiality = 0.01, expected = 0, likelihood = "binomial")$n
  expect_equal(n, 299)
  n <- planning(materiality = 0.01, expected = 1, likelihood = "binomial")$n
  expect_equal(n, 473)
  n <- planning(materiality = 0.01, expected = 2, likelihood = "binomial")$n
  expect_equal(n, 628)
  n <- planning(materiality = 0.01, expected = 3, likelihood = "binomial")$n
  expect_equal(n, 773)
  # Second column: n = 59, 93, 124, 153
  n <- planning(materiality = 0.05, expected = 0, likelihood = "binomial")$n
  expect_equal(n, 59)
  n <- planning(materiality = 0.05, expected = 1, likelihood = "binomial")$n
  expect_equal(n, 93)
  n <- planning(materiality = 0.05, expected = 2, likelihood = "binomial")$n
  expect_equal(n, 124)
  n <- planning(materiality = 0.05, expected = 3, likelihood = "binomial")$n
  expect_equal(n, 153)
  # Third column: n = 29, 46, 61, 76
  n <- planning(materiality = 0.1, expected = 0, likelihood = "binomial")$n
  expect_equal(n, 29)
  n <- planning(materiality = 0.1, expected = 1, likelihood = "binomial")$n
  expect_equal(n, 46)
  n <- planning(materiality = 0.1, expected = 2, likelihood = "binomial")$n
  expect_equal(n, 61)
  n <- planning(materiality = 0.1, expected = 3, likelihood = "binomial")$n
  expect_equal(n, 76)
  # Fourth column: n = 19, 30, 40, 50
  n <- planning(materiality = 0.15, expected = 0, likelihood = "binomial")$n
  expect_equal(n, 19)
  n <- planning(materiality = 0.15, expected = 1, likelihood = "binomial")$n
  expect_equal(n, 30)
  n <- planning(materiality = 0.15, expected = 2, likelihood = "binomial")$n
  expect_equal(n, 40)
  n <- planning(materiality = 0.15, expected = 3, likelihood = "binomial")$n
  expect_equal(n, 50)
  # Fifth column: n = 14, 22, 30, 37
  n <- planning(materiality = 0.2, expected = 0, likelihood = "binomial")$n
  expect_equal(n, 14)
  n <- planning(materiality = 0.2, expected = 1, likelihood = "binomial")$n
  expect_equal(n, 22)
  n <- planning(materiality = 0.2, expected = 2, likelihood = "binomial")$n
  expect_equal(n, 30)
  n <- planning(materiality = 0.2, expected = 3, likelihood = "binomial")$n
  expect_equal(n, 37)
  # Table 2 first column: n = 230, 388, 531, 667
  n <- planning(materiality = 0.01, expected = 0, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 230)
  n <- planning(materiality = 0.01, expected = 1, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 388)
  n <- planning(materiality = 0.01, expected = 2, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 531)
  n <- planning(materiality = 0.01, expected = 3, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 667)
  # Second column: n = 45, 77, 105, 132
  n <- planning(materiality = 0.05, expected = 0, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 45)
  n <- planning(materiality = 0.05, expected = 1, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 77)
  n <- planning(materiality = 0.05, expected = 2, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 105)
  n <- planning(materiality = 0.05, expected = 3, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 132)
  # Third column: n = 22, 38, 52, 65
  n <- planning(materiality = 0.1, expected = 0, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 22)
  n <- planning(materiality = 0.1, expected = 1, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 38)
  n <- planning(materiality = 0.1, expected = 2, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 52)
  n <- planning(materiality = 0.1, expected = 3, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 65)
  # Fourth column: n = 15, 25, 34, 43
  n <- planning(materiality = 0.15, expected = 0, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 15)
  n <- planning(materiality = 0.15, expected = 1, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 25)
  n <- planning(materiality = 0.15, expected = 2, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 34)
  n <- planning(materiality = 0.15, expected = 3, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 43)
  # Fifth column: n = 11, 18, 25, 32
  n <- planning(materiality = 0.2, expected = 0, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 11)
  n <- planning(materiality = 0.2, expected = 1, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 18)
  n <- planning(materiality = 0.2, expected = 2, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 25)
  n <- planning(materiality = 0.2, expected = 3, likelihood = "binomial", conf.level = 0.9)$n
  expect_equal(n, 32)

  # Page 129
  # Example 1: book: n_1 and n_2 = 49, jfa: n_1 and n_2 = 49
  res <- planning(materiality = 0.05, expected = c(1, 0), conf.level = 0.9, likelihood = "binomial")
  expect_equal(res$n, 98)
  expect_equal(res$n_staged, 49)

  # Page 131
  # Example 1: book: n = 299, jfa: n = 299
  n <- planning(materiality = 120000 / 12000000, expected = 0, likelihood = "binomial")$n
  expect_equal(n, 299)

  # Page 132
  # Example 1: book: n = 299, jfa: n = 299
  n <- planning(materiality = 0.01, expected = 0, likelihood = "binomial")$n
  expect_equal(n, 299)
  # Example 2: bookk: n = 473, jfa: n = 473
  n <- planning(materiality = 120000 / 12000000, expected = 1, likelihood = "binomial")$n
  expect_equal(n, 473)

  # Page 134
  # Example 1: book: n = 145, jfa: n = 144 --> DOES NOT MATCH BUT IS PROBABLY DUE TO BOOK USING ROUNDING AND INTERPOLATION
  n <- planning(materiality = 450000 / 13500000, expected = 100000 / 13500000, likelihood = "binomial", prior = auditPrior(method = "strict", likelihood = "binomial"))$n
  expect_equal(n, 145 - 1)
  n <- planning(materiality = 0.0334, expected = 1.098871, prior = auditPrior(method = "strict", likelihood = "binomial"))$n
  expect_equal(n, 145)
  # Example 2: book: n = 141, jfa: n = 141
  n <- planning(materiality = 450000 / 13500000, expected = 1, likelihood = "binomial")$n
  expect_equal(n, 141)
  # Example 3: book: n = 187, jfa: n = 187
  n <- planning(materiality = 450000 / 13500000, expected = 2, likelihood = "binomial")$n
  expect_equal(n, 187)

  # Page 135
  # Example 1: book: n = 1100, jfa: n = 1100
  n <- planning(materiality = 36730 / 13500000, likelihood = "binomial", prior = auditPrior(method = "strict", likelihood = "binomial"))$n
  expect_equal(n, 1100)

  # Page 137
  # Example 1: book: ub = 276050 / 13500000 = 0.02044815, jfa: ub = 0.02044826
  ub <- evaluation(n = 145, x = 0, method = "binomial")$ub
  expect_equal(ub, 0.02044826)
  # Example 2: book: ub = 436007 / 13500000 = 0.03229681, jfa: ub = 0.03229696
  ub <- evaluation(n = 145, x = 1, method = "binomial")$ub
  expect_equal(ub, 0.03229696)
  # Example 3: book: ub = 577535 / 13500000 = 0.04278037, jfa: ub = 0.04278054
  ub <- evaluation(n = 145, x = 2, method = "binomial")$ub
  expect_equal(ub, 0.04278054)
  # Example 4: book: ub = 710132 / 13500000 = 0.05260237, jfa: ub = 0.0526026
  ub <- evaluation(n = 145, x = 3, method = "binomial")$ub
  expect_equal(ub, 0.0526026)

  # Page 138
  # Example 1: book: mle = 115448.3 / 13500000 = 0.008551726; ub = 469616.5 / 13500000 = 0.03478641, jfa: mle = 0.008551724, ub = 0.03478656
  sample <- data.frame(id = seq_len(145), taint = 0)
  sample$taint[seq_len(3)] <- c(1.00000000, 0.20000072, 0.03999931)
  sample$book <- rep(1000, 145)
  sample$audit <- sample$book - (sample$book * sample$taint)
  res <- evaluation(data = sample, method = "stringer", values = "book", values.audit = "audit")
  expect_equal(res$mle, 0.008551724)
  expect_equal(res$ub, 0.03478656)

  # Page 140
  # Example 1: jfa does not support cell bound

  # Page 141
  # Example 1: jfa does not support pps bound
  res <- evaluation(data = sample, method = "pps", values = "book", values.audit = "audit", alternative = "two.sided")
  expect_equal(res$mle * 13500000, 115448.2787)
  expect_equal(13500000 - res$mle * 13500000, 13384551.72)
  expect_equal(13500000 - res$ub * 13500000, 12976389.01)
  expect_equal(13500000 - res$lb * 13500000, 13792714.43)

  # Page 142
  # Example 1: book: n = 187, jfa: n = 187
  n <- planning(materiality = 450000 / 13500000, expected = 2, likelihood = "binomial")$n
  expect_equal(n, 187)

  # Page 143
  # Example 1: book: n = 145, jfa: n = 144 --> DOES NOT MATCH BUT IS PROBABLY DUE TO BOOK USING ROUNDING AND INTERPOLATION
  n <- planning(materiality = 450000 / 13500000, expected = 100000 / 13500000, likelihood = "binomial", prior = auditPrior(method = "strict", likelihood = "binomial"))$n
  expect_equal(n, 145 - 1)
  n <- planning(materiality = 0.0334, expected = 1.098871, prior = auditPrior(method = "strict", likelihood = "binomial"))$n
  expect_equal(n, 145)
  # Example 2: book: ub = 449999.9 / 13500000 = 0.03333333, jfa: ub = 0.03333348
  sample <- data.frame(id = seq_len(145), taint = 0)
  sample$taint[seq_len(2)] <- c(1, .09887)
  sample$book <- rep(1000, 145)
  sample$audit <- sample$book - (sample$book * sample$taint)
  ub <- evaluation(data = sample, method = "stringer", values = "book", values.audit = "audit")$ub
  expect_equal(ub, 0.03333348)
  # Example 3: jfa does not support cell method

  # Page 144
  # Example 1: book: ub = 439009.3 / 13500000 = 0.03251921, jfa: ub = 0.03251936
  sample <- data.frame(id = seq_len(145), taint = 0)
  sample$taint[seq_len(3)] <- c(0.5, 0.4, 0.19887)
  sample$book <- rep(1000, 145)
  sample$audit <- sample$book - (sample$book * sample$taint)
  ub <- evaluation(data = sample, method = "stringer", values = "book", values.audit = "audit")$ub
  expect_equal(ub, 0.03251936)
  # Example 2: jfa does not support cell method
  # Example 3: book: n = 746, jfa: n = 744 --> DOES NOT MATCH BUT IS PROBABLY DUE TO BOOK USING ROUNDING AND INTERPOLATION
  n <- planning(materiality = 450000 / 13500000, expected = 300000 / 13500000, likelihood = "binomial", prior = auditPrior(method = "strict", likelihood = "binomial"))$n
  expect_equal(n, 746 - 2)
})
