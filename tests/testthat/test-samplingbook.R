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

context("Benchmark against R package samplingbook")

# samplingbook R package (version 1.2.4)

test_that(desc = "(id: f15-v0.6.3-t1) Validate hypergeometric 99% upper bounds", {
  ub_level <- 0.99
  interval_level <- ub_level - (1 - ub_level)
  index <- 1
  ub_jfa <- numeric()
  ub_sb <- numeric()
  for (N in seq(400, 1000, 50)) {
    for (n in seq(20, 300, 10)) {
      for (k in 0:10) {
        ub_jfa[index] <- jfa::evaluation(materiality = 0.99, n = n, x = k, method = "hypergeometric", N.units = N, conf.level = ub_level, alternative = "less")$ub
        ub_sb[index] <- suppressWarnings(expr = {
          samplingbook::Sprop(m = k, n = n, N = N, level = interval_level)$ci$exact[2]
        })
        index <- index + 1
      }
    }
  }
  expect_equal(ub_jfa, ub_sb, tolerance = 0.005)
})

test_that(desc = "(id: f15-v0.6.3-t2) Validate hypergeometric 95% upper bounds", {
  ub_level <- 0.95
  interval_level <- ub_level - (1 - ub_level)
  index <- 1
  ub_jfa <- numeric()
  ub_sb <- numeric()
  for (N in seq(400, 1000, 50)) {
    for (n in seq(20, 300, 10)) {
      for (k in 0:10) {
        ub_jfa[index] <- jfa::evaluation(materiality = 0.99, n = n, x = k, method = "hypergeometric", N.units = N, conf.level = ub_level, alternative = "less")$ub
        ub_sb[index] <- suppressWarnings(expr = {
          samplingbook::Sprop(m = k, n = n, N = N, level = interval_level)$ci$exact[2]
        })
        index <- index + 1
      }
    }
  }
  expect_equal(ub_jfa, ub_sb, tolerance = 0.005)
})

test_that(desc = "(id: f15-v0.6.3-t3) Validate hypergeometric 90% upper bounds", {
  ub_level <- 0.90
  interval_level <- ub_level - (1 - ub_level)
  index <- 1
  ub_jfa <- numeric()
  ub_sb <- numeric()
  for (N in seq(400, 1000, 50)) {
    for (n in seq(20, 300, 10)) {
      for (k in 0:10) {
        ub_jfa[index] <- jfa::evaluation(materiality = 0.99, n = n, x = k, method = "hypergeometric", N.units = N, conf.level = ub_level, alternative = "less")$ub
        ub_sb[index] <- suppressWarnings(expr = {
          samplingbook::Sprop(m = k, n = n, N = N, level = interval_level)$ci$exact[2]
        })
        index <- index + 1
      }
    }
  }
  expect_equal(ub_jfa, ub_sb, tolerance = 0.005)
})
