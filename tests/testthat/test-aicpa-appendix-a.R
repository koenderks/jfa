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

context("Benchmark against Appendix A (AICPA 2017)")

# Audit Guide: Audit Sampling [https://future.aicpa.org/cpe-learning/publication/audit-sampling-audit-guide-OPL]
# Retrieved on 28-04-2021 from https://onlinelibrary.wiley.com/doi/pdf/10.1002/9781119448617.app1

test_that(desc = "(id: f9-v0.4.0-t1) Test sample sizes for 5 percent risk of overreliance (AICPA 2017 - Appendix A: Table A-1)", {
  testthat::skip_on_cran()
  expectedDeviationRate <- c(seq(0, 4, 0.25), 5:10, 12.50, 15.00, 17.50) / 100
  tolerableDeivationRate <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20) / 100
  sampleSizeMatrix <- matrix(NA, nrow = length(expectedDeviationRate), ncol = length(tolerableDeivationRate))
  rownames(sampleSizeMatrix) <- expectedDeviationRate
  colnames(sampleSizeMatrix) <- tolerableDeivationRate
  for (i in seq_along(tolerableDeivationRate)) {
    for (j in seq_along(expectedDeviationRate)) {
      if (i == 1 && j > 6) {
        next
      }
      if (i == 2 && j > 10) {
        next
      }
      if (i == 3 && j > 14) {
        next
      }
      if (i == 4 && j > 17) {
        next
      }
      if (i == 5 && j > 18) {
        next
      }
      if (i == 6 && j > 19) {
        next
      }
      if (i == 7 && j > 19) {
        next
      }
      if (i == 8 && j > 20) {
        next
      }
      if (i == 9 && j > 21) {
        next
      }
      if (i == 10 && j > 24) {
        next
      }
      if (i == 11 && j > 26) {
        next
      }
      jfaRes <- planning(conf.level = 0.95, expected = expectedDeviationRate[j], likelihood = "binomial", materiality = tolerableDeivationRate[i])
      sampleSizeMatrix[j, i] <- jfaRes[["n"]]
    }
  }

  aicpaMatrix <- matrix(
    data = c(
      149, 236, 313, 386, 590, 1030, rep(NA, 20), # 2%
      99, 157, 157, 208, 257, 303, 392, 562, 846, 1466, rep(NA, 16), # 3%
      74, 117, 117, 117, 156, 156, 192, 227, 294, 390, 513, 722, 1098, 1936, rep(NA, 12), # 4%
      59, 93, 93, 93, 93, 124, 124, 153, 181, 208, 234, 286, 361, 458, 624, 877, 1348, rep(NA, 9), # 5%
      49, 78, 78, 78, 78, 78, 103, 103, 127, 127, 150, 173, 195, 238, 280, 341, 421, 1580, rep(NA, 8), # 6%
      42, 66, 66, 66, 66, 66, 66, 88, 88, 88, 109, 109, 129, 148, 167, 185, 221, 478, 1832, rep(NA, 7), # 7%
      36, 58, 58, 58, 58, 58, 58, 77, 77, 77, 77, 95, 95, 112, 112, 129, 146, 240, 532, rep(NA, 7), # 8%
      32, 51, 51, 51, 51, 51, 51, 51, 68, 68, 68, 68, 84, 84, 84, 100, 100, 158, 266, 585, rep(NA, 6), # 9%
      29, 46, 46, 46, 46, 46, 46, 46, 46, 61, 61, 61, 61, 61, 76, 76, 89, 116, 179, 298, 649, rep(NA, 5), # 10%
      19, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 40, 40, 40, 40, 50, 68, 85, 110, 150, 576, rep(NA, 2), # 15%
      14, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 30, 30, 37, 37, 44, 50, 88, 193, 720
    ), # 20%
    nrow = length(expectedDeviationRate), ncol = length(tolerableDeivationRate), byrow = FALSE
  )
  rownames(aicpaMatrix) <- expectedDeviationRate
  colnames(aicpaMatrix) <- tolerableDeivationRate

  expect_equal(sampleSizeMatrix, aicpaMatrix)
})

test_that(desc = "(id: f9-v0.4.0-t2) Test sample sizes for 10 percent risk of overreliance (AICPA 2017 - Appendix A: Table A-2)", {
  testthat::skip_on_cran()
  expectedDeviationRate <- c(seq(0, 4, 0.25), 5:10, 12.50, 15.00, 17.50) / 100
  tolerableDeivationRate <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20) / 100
  sampleSizeMatrix <- matrix(NA, nrow = length(expectedDeviationRate), ncol = length(tolerableDeivationRate))
  rownames(sampleSizeMatrix) <- expectedDeviationRate
  colnames(sampleSizeMatrix) <- tolerableDeivationRate
  for (i in seq_along(tolerableDeivationRate)) {
    for (j in seq_along(expectedDeviationRate)) {
      if (i == 1 && j > 7) {
        next
      }
      if (i == 2 && j > 10) {
        next
      }
      if (i == 3 && j > 14) {
        next
      }
      if (i == 4 && j > 17) {
        next
      }
      if (i == 5 && j > 18) {
        next
      }
      if (i == 6 && j > 19) {
        next
      }
      if (i == 7 && j > 20) {
        next
      }
      if (i == 8 && j > 21) {
        next
      }
      if (i == 9 && j > 22) {
        next
      }
      if (i == 10 && j > 24) {
        next
      }
      if (i == 11 && j > 26) {
        next
      }
      jfaRes <- planning(conf.level = 0.90, expected = expectedDeviationRate[j], likelihood = "binomial", materiality = tolerableDeivationRate[i])
      sampleSizeMatrix[j, i] <- jfaRes[["n"]]
    }
  }

  # 5 percent tolerable deviation rate and 3.5 percent expected deviation rate results in n = 423, where AICPA (2017) gives 200. JfA is correct here since the expected errors in this case are actually ceiling(14.000000000000002) = 15 and not 14.
  # 7 percent tolerable deviation rate and 6 percent expected deviation rate results in n = 1313, where AICPA (2017) gives 1300. JfA is correct here since the expected errors in this case are actually ceiling(91.000000000000014) = 92 and not 91.

  aicpaMatrix <- matrix(
    data = c(
      114, 194, 194, 265, 398, 708, 1463, rep(NA, 19), # 2%
      76, 129, 129, 129, 176, 221, 265, 390, 590, 974, rep(NA, 16), # 3%
      57, 96, 96, 96, 96, 132, 132, 166, 198, 262, 353, 471, 730, 1258, rep(NA, 12), # 4%
      45, 77, 77, 77, 77, 77, 105, 105, 132, 132, 158, 209, 258, 306, 423, 583, 873, rep(NA, 9), # 5%
      38, 64, 64, 64, 64, 64, 64, 88, 88, 88, 110, 132, 132, 153, 194, 235, 274, 1019, rep(NA, 8), # 6%
      32, 55, 55, 55, 55, 55, 55, 55, 75, 75, 75, 94, 94, 112 + 1, 112 + 1, 131, 149, 318, 1150, rep(NA, 7), # 7% --> AICPA gives 112 but sum(dbinom(0:4, size = 112, prob = 0.07)) > 0.10
      28, 48, 48, 48, 48, 48, 48, 48, 48, 65, 65, 65, 65, 82, 82, 98, 98, 160, 349, 1313, rep(NA, 6), # 8%
      25, 42, 42, 42, 42, 42, 42, 42, 42, 42, 58, 58, 58, 58, 73, 73, 73, 115, 182, 385, 1437, rep(NA, 5), # 9%
      22, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 52, 52, 52, 52, 52, 65, 78, 116, 199, 424, 1577, rep(NA, 4), # 10%
      15, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 34, 43, 52, 60, 77, 100, 368, rep(NA, 2), # 15%
      11, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 25, 25, 25, 32, 38, 63, 126, 457
    ), # 20%
    nrow = length(expectedDeviationRate), ncol = length(tolerableDeivationRate), byrow = FALSE
  )
  rownames(aicpaMatrix) <- expectedDeviationRate
  colnames(aicpaMatrix) <- tolerableDeivationRate

  expect_equal(sampleSizeMatrix, aicpaMatrix)
})

test_that(desc = "(id: f9-v0.4.0-t3) Test upper bounds for 5 percent risk of overreliance (AICPA 2017 - Appendix A: Table A-3)", {
  testthat::skip_on_cran()
  sampleSize <- c(seq(20, 80, 5), 90, 100, 125, 150, 200, 300, 400, 500)
  numberOfDeviations <- 0:10
  evaluationMatrix <- matrix(NA, nrow = length(sampleSize), ncol = length(numberOfDeviations))
  rownames(evaluationMatrix) <- sampleSize
  colnames(evaluationMatrix) <- numberOfDeviations
  for (i in seq_along(numberOfDeviations)) {
    for (j in seq_along(sampleSize)) {
      jfaRes <- evaluation(conf.level = 0.95, method = "binomial", materiality = 0.05, n = sampleSize[j], x = numberOfDeviations[i])
      evaluationMatrix[j, i] <- ceiling(jfaRes[["ub"]] * 100 * 10) / 10
    }
  }


  aicpaMatrix <- matrix(
    data = c(
      14.0, 21.7, 28.3, 34.4, 40.2, 45.6, 50.8, 55.9, 60.7, 65.4, 69.9,
      11.3, 17.7, 23.2, 28.2, 33.0, 37.6, 42.0, 46.3, 50.4, 54.4, 58.4,
      9.6, 14.9, 19.6, 23.9, 28.0, 31.9, 35.8, 39.4, 43.0, 46.6, 50.0,
      8.3, 12.9, 17.0, 20.7, 24.3, 27.8, 31.1, 34.4, 37.5, 40.6, 43.7,
      7.3, 11.4, 15.0, 18.3, 21.5, 24.6, 27.5, 30.4, 33.3, 36.0, 38.8,
      6.5, 10.2, 13.4, 16.4, 19.2, 22.0, 24.7, 27.3, 29.8, 32.4, 34.8,
      5.9, 9.2, 12.1, 14.8, 17.4, 19.9, 22.4, 24.7, 27.1, 29.4, 31.6,
      5.4, 8.4, 11.1, 13.5, 15.9, 18.2, 20.5, 22.6, 24.8, 26.9, 28.9,
      4.9, 7.7, 10.2, 12.5, 14.7, 16.8, 18.8, 20.8, 22.8, 24.8, 26.7,
      4.6, 7.1, 9.4, 11.5, 13.6, 15.5, 17.5, 19.3, 21.2, 23.0, 24.7,
      4.2, 6.6, 8.8, 10.8, 12.7, 14.5, 16.3, 18.0, 19.7, 21.4, 23.1,
      4.0, 6.2, 8.2, 10.1, 11.8, 13.6, 15.2, 16.9, 18.5, 20.1, 21.6,
      3.7, 5.8, 7.7, 9.5, 11.1, 12.7, 14.3, 15.9, 17.4, 18.9, 20.3,
      3.3, 5.2, 6.9, 8.4, 9.9, 11.4, 12.8, 14.2, 15.5, 16.9, 18.2,
      3.0, 4.7, 6.2, 7.6, 9.0, 10.3, 11.5, 12.8, 14.0, 15.2, 16.4,
      2.4, 3.8, 5.0, 6.1, 7.2, 8.3, 9.3, 10.3, 11.3, 12.3, 13.2,
      2.0, 3.2, 4.2, 5.1, 6.0, 6.9, 7.8, 8.6, 9.5, 10.3, 11.1,
      1.5, 2.4, 3.2, 3.9, 4.6, 5.2, 5.9, 6.5, 7.2, 7.8, 8.4,
      1.0, 1.6, 2.1, 2.6, 3.1, 3.5, 4.0, 4.4, 4.8, 5.2, 5.6,
      0.8, 1.2, 1.6, 2.0, 2.3, 2.7, 3.0, 3.3, 3.6, 3.9, 4.3,
      0.6, 1.0, 1.3, 1.6, 1.9, 2.1, 2.4, 2.7, 2.9, 3.2, 3.4
    ),
    nrow = length(sampleSize), ncol = length(numberOfDeviations), byrow = TRUE
  )
  rownames(aicpaMatrix) <- sampleSize
  colnames(aicpaMatrix) <- numberOfDeviations

  expect_equal(evaluationMatrix, aicpaMatrix)
})

test_that(desc = "(id: f9-v0.4.0-t4) Test upper bounds for 10 percent risk of overreliance (AICPA 2017 - Appendix A: Table A-4)", {
  testthat::skip_on_cran()
  sampleSize <- c(seq(20, 80, 5), 90, 100, 125, 150, 200, 300, 400, 500)
  numberOfDeviations <- 0:10
  evaluationMatrix <- matrix(NA, nrow = length(sampleSize), ncol = length(numberOfDeviations))
  rownames(evaluationMatrix) <- sampleSize
  colnames(evaluationMatrix) <- numberOfDeviations
  for (i in seq_along(numberOfDeviations)) {
    for (j in seq_along(sampleSize)) {
      jfaRes <- evaluation(conf.level = 0.90, method = "binomial", materiality = 0.05, n = sampleSize[j], x = numberOfDeviations[i])
      evaluationMatrix[j, i] <- ceiling(jfaRes[["ub"]] * 100 * 10) / 10
    }
  }

  aicpaMatrix <- matrix(
    data = c(
      10.9, 18.1, 24.5, 30.5, 36.1, 41.5, 46.8, 51.9, 56.8, 61.6, 66.2,
      8.8, 14.7, 20.0, 24.9, 29.5, 34.0, 38.4, 42.6, 46.8, 50.8, 54.8,
      7.4, 12.4, 16.8, 21.0, 24.9, 28.8, 32.5, 36.2, 39.7, 43.2, 46.7,
      6.4, 10.7, 14.5, 18.2, 21.6, 24.9, 28.2, 31.4, 34.5, 37.6, 40.6,
      5.6, 9.4, 12.8, 16.0, 19.0, 22.0, 24.9, 27.7, 30.5, 33.2, 35.9,
      5.0, 8.4, 11.4, 14.3, 17.0, 19.7, 22.3, 24.8, 27.3, 29.8, 32.2,
      4.6, 7.6, 10.3, 12.9, 15.4, 17.8, 20.2, 22.5, 24.7, 27.0, 29.2,
      4.2, 6.9, 9.4, 11.8, 14.1, 16.3, 18.4, 20.5, 22.6, 24.6, 26.7,
      3.8, 6.4, 8.7, 10.8, 12.9, 15.0, 16.9, 18.9, 20.8, 22.7, 24.6,
      3.5, 5.9, 8.0, 10.0, 12.0, 13.9, 15.7, 17.5, 19.3, 21.0, 22.8,
      3.3, 5.5, 7.5, 9.3, 11.1, 12.9, 14.6, 16.3, 18.0, 19.6, 21.2,
      3.1, 5.1, 7.0, 8.7, 10.4, 12.1, 13.7, 15.2, 16.8, 18.3, 19.8,
      2.9, 4.8, 6.6, 8.2, 9.8, 11.3, 12.8, 14.3, 15.8, 17.2, 18.7,
      2.6, 4.3, 5.9, 7.3, 8.7, 10.1, 11.5, 12.8, 14.1, 15.4, 16.7,
      2.3, 3.9, 5.3, 6.6, 7.9, 9.1, 10.3, 11.5, 12.7, 13.9, 15.0,
      1.9, 3.1, 4.3, 5.3, 6.3, 7.3, 8.3, 9.3, 10.2, 11.2, 12.1,
      1.6, 2.6, 3.6, 4.4, 5.3, 6.1, 7.0, 7.8, 8.6, 9.4, 10.1,
      1.2, 2.0, 2.7, 3.4, 4.0, 4.6, 5.3, 5.9, 6.5, 7.1, 7.6,
      0.8, 1.3, 1.8, 2.3, 2.7, 3.1, 3.5, 3.9, 4.3, 4.7, 5.1,
      0.6, 1.0, 1.4, 1.7, 2.0, 2.4, 2.7, 3.0, 3.3, 3.6, 3.9,
      0.5, 0.8, 1.1, 1.4, 1.6, 1.9, 2.1, 2.4, 2.6, 2.9, 3.1
    ),
    nrow = length(sampleSize), ncol = length(numberOfDeviations), byrow = TRUE
  )
  rownames(aicpaMatrix) <- sampleSize
  colnames(aicpaMatrix) <- numberOfDeviations

  expect_equal(evaluationMatrix, aicpaMatrix)
})
