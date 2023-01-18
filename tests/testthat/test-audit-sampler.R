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

context("Benchmark against AuditSampler software")

# AuditSampler [https://cplusglobal.wordpress.com/2015/02/14/auditsampler-statistical-audit-sampling-software/]
# Retrieved on 28-04-2021 from https://cplusglobal.wordpress.com/2014/04/15/attributes-sampling/

test_that(desc = "(id: f11-v0.4.0-t1) Test Sample sizes for binomial distribution", {
  expectedDeviationRate <- c(seq(0, 4, 0.25), 4.5, 5) / 100
  tolerableDeivationRate <- c(2, 3, 4, 5, 6, 7, 8, 9, 10) / 100
  sampleSizeMatrix <- matrix(NA, nrow = length(expectedDeviationRate), ncol = length(tolerableDeivationRate))
  rownames(sampleSizeMatrix) <- expectedDeviationRate
  colnames(sampleSizeMatrix) <- tolerableDeivationRate
  for (i in seq_along(tolerableDeivationRate)) {
    for (j in seq_along(expectedDeviationRate)) {
      if (i == 1 && j > 5) {
        next
      }
      if (i == 2 && j > 8) {
        next
      }
      if (i == 3 && j > 11) {
        next
      }
      if (i == 4 && j > 14) {
        next
      }
      if (i == 5 && j > 16) {
        next
      }
      if (i == 6 && j > 18) {
        next
      }
      jfaRes <- planning(conf.level = 0.90, expected = expectedDeviationRate[j], likelihood = "binomial", materiality = tolerableDeivationRate[i])
      sampleSizeMatrix[j, i] <- jfaRes[["n"]]
    }
  }

  auditSamplerMatrix <- matrix(
    data = c(
      114, 194, 194, 265, 398, rep(NA, 14), # 2%
      76, 129, 129, 129, 176, 221, 265, 390, rep(NA, 11), # 3%
      57, 96, 96, 96, 96, 132, 132, 166, 198, 262, 353, rep(NA, 8), # 4%
      45, 77, 77, 77, 77, 77, 105, 105, 132, 132, 158, 209, 258, 306, rep(NA, 5), # 5%
      38, 64, 64, 64, 64, 64, 64, 88, 88, 88, 110, 132, 132, 153, 194, 235, rep(NA, 3), # 6%
      32, 55, 55, 55, 55, 55, 55, 55, 75, 75, 75, 94, 94, 112 + 1, 112 + 1, 131, 149, 218, rep(NA, 1), # 7% --> AuditSampler gives 112 but sum(dbinom(0:4, size = 112, prob = 0.07)) > 0.10
      28, 48, 48, 48, 48, 48, 48, 48, 48, 65, 65, 65, 65, 82, 82, 98, 98, 130, 160, # 8%
      25, 42, 42, 42, 42, 42, 42, 42, 42, 42, 58, 58, 58, 58, 73, 73, 73, 87, 115, # 9%
      22, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 52, 52, 52, 52, 52, 65, 65, 78
    ), # 10%
    nrow = length(expectedDeviationRate), ncol = length(tolerableDeivationRate), byrow = FALSE
  )
  rownames(auditSamplerMatrix) <- expectedDeviationRate
  colnames(auditSamplerMatrix) <- tolerableDeivationRate

  expect_equal(sampleSizeMatrix, auditSamplerMatrix)
})

# AuditSampler [https://cplusglobal.wordpress.com/2015/02/14/auditsampler-statistical-audit-sampling-software/]
# Retrieved on 28-04-2021 from https://cplusglobal.wordpress.com/2015/11/13/attributes-sample-size-using-the-hypergeometric-distribution/

test_that(desc = "(id: f11-v0.4.0-t1) Test Sample sizes for Hypergeometric distribution", {
  populationSize <- c(rep(500, 12), rep(5000, 3), 15000, 36000)
  expectedErrorRate <- c(rep(1, 3), 2, rep(1, 3), 2, 3, 4, 5, 6, rep(1, 5)) / 100
  tolerableErrorRate <- c(8, 6, 4, 5, 8, 6, 4, 5, 6, 7, 8, 9, 8, 6, 4, 6, 4) / 100
  confidenceLevel <- c(rep(90, 4), rep(95, 13)) / 100
  sampleSizeMatrix <- matrix(NA, nrow = length(populationSize), ncol = 7)
  sampleSizeMatrix[, 1] <- populationSize
  sampleSizeMatrix[, 2] <- expectedErrorRate
  sampleSizeMatrix[, 3] <- tolerableErrorRate
  sampleSizeMatrix[, 4] <- confidenceLevel
  for (i in seq_len(nrow(sampleSizeMatrix))) {
    jfaRes <- planning(conf.level = sampleSizeMatrix[i, 4], expected = sampleSizeMatrix[i, 2], likelihood = "hypergeometric", N.units = sampleSizeMatrix[i, 1], materiality = sampleSizeMatrix[i, 3])
    sampleSizeMatrix[i, 5] <- jfaRes[["n"]]
    jfaRes <- planning(conf.level = sampleSizeMatrix[i, 4], expected = sampleSizeMatrix[i, 2], likelihood = "binomial", N.units = sampleSizeMatrix[i, 1], materiality = sampleSizeMatrix[i, 3])
    sampleSizeMatrix[i, 6] <- jfaRes[["n"]]
    sampleSizeMatrix[i, 7] <- sampleSizeMatrix[i, 6] - sampleSizeMatrix[i, 5]
  }

  auditSamplerMatrix <- matrix(NA, nrow = length(populationSize), ncol = 7)
  auditSamplerMatrix[, 1] <- populationSize
  auditSamplerMatrix[, 2] <- expectedErrorRate
  auditSamplerMatrix[, 3] <- tolerableErrorRate
  auditSamplerMatrix[, 4] <- confidenceLevel

  auditSamplerMatrix[, 5:7] <- matrix(
    c(
      46, 48, 2,
      61, 64, 3,
      90, 96, 6,
      99, 132, 33,
      55, 58, 3,
      73, 78, 5,
      140, 156, 16,
      139, 181, 42,
      158, 195, 37,
      170, 221, 51,
      178, 240, 62,
      196, 266, 70,
      58, 58, 0,
      77, 78, 1,
      154, 156, 2,
      78, 78, 0,
      156, 156, 0
    ),
    ncol = 3, nrow = 17, byrow = TRUE
  )

  expect_equal(sampleSizeMatrix, auditSamplerMatrix)
})
