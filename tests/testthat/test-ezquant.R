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

context("Benchmark against EZ-quant software")

# EZ-quant statistical sampling software (version 2.0.6)
# Retrieved on 02-03-2022 from https://www.dcaa.mil/Checklists-Tools/EZ-Quant-Applications/

test_that(desc = "(id: f12-v0.5.4-t1) Test Sample sizes for hypergeometric distribution", {
  # Plan for 0 expected errors
  populationSize <- rep(c(rep(500, 4), rep(1000, 4), rep(5000, 4)), 3)
  tolerableErrorRate <- rep(rep(c(0.08, 0.06, 0.04, 0.02), times = 3), 3)
  confidenceLevel <- rep(c(0.9, 0.95, 0.99), each = 12)
  sampleSizeMatrix <- matrix(NA, nrow = length(populationSize), ncol = 4)
  sampleSizeMatrix[, 1] <- populationSize
  sampleSizeMatrix[, 2] <- tolerableErrorRate
  sampleSizeMatrix[, 3] <- confidenceLevel
  for (i in seq_len(nrow(sampleSizeMatrix))) {
    sampleSizeMatrix[i, 4] <- planning(conf.level = sampleSizeMatrix[i, 3], expected = 0, likelihood = "hypergeometric", N.units = sampleSizeMatrix[i, 1], materiality = sampleSizeMatrix[i, 2])$n
  }

  ezquantMatrix <- matrix(NA, nrow = length(populationSize), ncol = 4)
  ezquantMatrix[, 1] <- populationSize
  ezquantMatrix[, 2] <- tolerableErrorRate
  ezquantMatrix[, 3] <- confidenceLevel

  ezquantMatrix[, 4] <- c(
    27, 36, 54, 102,
    28, 37, 55, 108,
    28, 38, 57, 113,
    35, 47, 69, 129,
    36, 48, 71, 138,
    36, 49, 73, 147,
    53, 70, 101, 183,
    54, 72, 107, 204,
    55, 74, 112, 223
  )

  expect_equal(sampleSizeMatrix, ezquantMatrix)
})
