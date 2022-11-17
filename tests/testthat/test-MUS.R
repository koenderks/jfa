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

context("Benchmark against R package MUS")

# MUS R package (version 0.1.6)

test_that(desc = "(id: f12-v0.5.4-t1) Test Sample sizes for poisson distribution", {
  set.seed(1)
  data <- data.frame(book.value = round(stats::runif(n = 500, min = 1, max = 1000)))
  m <- seq(10000, 20000, 1000)
  e <- seq(100, 900, 100)
  ss_MUS <- ss_jfa <- matrix(NA, nrow = length(e), ncol = length(m))
  for (j in seq_along(m)) {
    for (i in seq_along(e)) {
      ss_MUS[i, j] <- MUS::MUS.calc.n.conservative(confidence.level = 0.95, tolerable.error = m[j], expected.error = e[i], book.value = sum(data$book.value)) # MUS::calc.n.conservative is used in MUS::MUS.planning()
      ss_jfa[i, j] <- jfa::planning(conf.level = 0.95, materiality = m[j] / sum(data$book.value), expected = e[i] / sum(data$book.value), likelihood = "poisson")$n
    }
  }

  ss_MUS[1, 3] <- ss_MUS[1, 3] - 1 # 63 instead of 64 --> pgamma(q = m[3] / sum(data$book.value), shape = 1 + (e[1] / sum(data$book.value)) * 63, rate = 63) > 0.95
  ss_MUS[2, 3] <- ss_MUS[2, 3] - 1 # 64 instead of 65 --> pgamma(q = m[3] / sum(data$book.value), shape = 1 + (e[2] / sum(data$book.value)) * 64, rate = 64) > 0.95
  ss_MUS[5, 5] <- ss_MUS[5, 5] - 1 # 57 instead of 58 --> pgamma(q = m[5] / sum(data$book.value), shape = 1 + (e[5] / sum(data$book.value)) * 57, rate = 57) > 0.95
  ss_MUS[9, 7] <- ss_MUS[9, 7] - 1 # 52 instead of 53 --> pgamma(q = m[7] / sum(data$book.value), shape = 1 + (e[9] / sum(data$book.value)) * 52, rate = 52) > 0.95
  ss_MUS[8, 8] <- ss_MUS[8, 8] - 1 # 48 instead of 49 --> pgamma(q = m[8] / sum(data$book.value), shape = 1 + (e[8] / sum(data$book.value)) * 48, rate = 48) > 0.95
  ss_MUS[9, 10] <- ss_MUS[9, 10] - 1 # 43 instead of 44 --> pgamma(q = m[10] / sum(data$book.value), shape = 1 + (e[9] / sum(data$book.value)) * 43, rate = 43) > 0.95
  expect_equal(ss_MUS, ss_jfa)
})

# MUS R package (version 0.1.6)

test_that(desc = "(id: f12-v0.5.4-t2) Test most likely error and upper bound using stringer bound", {
  set.seed(1)
  data <- data.frame(book.value = round(stats::runif(n = 1000, min = 1, max = 1000)))
  m <- seq(10000, 20000, 500)
  e <- seq(500, 4500, 100)
  mle_MUS <- ub_MUS <- mle_jfa <- ub_jfa <- matrix(NA, nrow = length(e), ncol = length(m))
  for (j in seq_along(m)) {
    for (i in seq_along(e)) {
      plan <- MUS::MUS.planning(data = data, tolerable.error = m[j], expected.error = e[i])
      # Extract a sample and cache it (no high values exist in this example)
      extract <- MUS::MUS.extraction(plan)
      # Copy book value into a new column audit values, and inject some error
      audited <- extract$sample$book.value * (1 - stats::rbinom(n = nrow(extract$sample), size = 1, prob = 0.01))
      audited <- cbind(extract$sample, audit.value = audited)
      # Evaluate the sample, cache and print it
      e_MUS <- MUS::MUS.evaluation(extract = extract, filled.sample = audited)
      e_jfa <- jfa::evaluation(conf.level = 0.95, materiality = m[j] / sum(data$book.value), method = "stringer", data = audited, values = "book.value", values.audit = "audit.value")
      mle_MUS[i, j] <- as.numeric(e_MUS$MLE.final / sum(data$book.value))
      ub_MUS[i, j] <- as.numeric(e_MUS$UEL.low.error.rate / sum(data$book.value))
      mle_jfa[i, j] <- e_jfa$mle
      ub_jfa[i, j] <- e_jfa$ub
    }
  }

  expect_equal(mle_MUS, mle_jfa, tolerance = 0.00001)
  expect_equal(ub_MUS, ub_jfa, tolerance = 0.00001)
})
