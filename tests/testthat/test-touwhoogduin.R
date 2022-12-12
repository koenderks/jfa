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
  SR <- c(0.05, 0.10, 0.25, 0.40, 0.50)
  materiality <- 0.01
  n <- numeric(length(SR))
  for (i in seq_along(SR)) {
    n[i] <- jfa::planning(conf.level = 1 - SR[i], materiality = materiality, likelihood = "poisson")$n
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
  n <- jfa::planning(conf.level = 1 - 0.05, materiality = 100000 / 5000000, likelihood = "poisson")$n
  expect_equal(n, 150)
  n <- jfa::planning(conf.level = 1 - 0.125, materiality = 100000 / 5000000, likelihood = "poisson")$n
  expect_equal(n, 104)
})
