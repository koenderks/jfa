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

context("Validation of function fairness_selection")

test_that(desc = "Validation of fairness selection", {
  testthat::skip_on_cran()
  # Disparate impact
  outcome <- fairness_selection(q1 = 2)
  expect_equal(outcome$measure, "pp")
  # Equalized odds
  outcome <- fairness_selection(q1 = 1, q2 = 3)
  expect_equal(outcome$measure, "dp")
  # False negative rate parity
  outcome <- fairness_selection(q1 = 1, q2 = 2, q3 = NULL, q4 = 2)
  expect_equal(outcome$measure, "fnrp")
  # False positive rate parity
  outcome <- fairness_selection(q1 = 1, q2 = 2, q3 = NULL, q4 = 1)
  expect_equal(outcome$measure, "fprp")
  # Accuracy parity
  outcome <- fairness_selection(q1 = 1, q2 = 1, q3 = 2, q4 = 3)
  expect_equal(outcome$measure, "ap")
  # Negative predictive rate parity
  outcome <- fairness_selection(q1 = 1, q2 = 1, q3 = 2, q4 = 2)
  expect_equal(outcome$measure, "npvp")
  # Specificity parity
  outcome <- fairness_selection(q1 = 1, q2 = 1, q3 = 2, q4 = 1)
  expect_equal(outcome$measure, "sp")
  # Equal opportunity
  outcome <- fairness_selection(q1 = 1, q2 = 1, q3 = 1, q4 = 2)
  expect_equal(outcome$measure, "tprp")
  # Predictive rate parity
  outcome <- fairness_selection(q1 = 1, q2 = 1, q3 = 1, q4 = 1)
  expect_equal(outcome$measure, "prp")
  # Plot
  p <- plot(outcome)
  expect_equal(is.null(p), FALSE)
})
