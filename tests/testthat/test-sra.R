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

context("Benchmark against SRA")

# SRA steekproefmodel https://www.sra.nl/vaktechniek/accountancy/controle/praktijkhandreikingen/praktijkhandreiking-steekproeven-sra-steekproefmodel
# Retrieved on 10-11-2022

# SRA:
#            | Materiality
# Confidence | 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5
# 0.95       |   60  30   20  15   12  10    9   8    7   6
# 0.96       |   64  32   22  16   13  11   10   8    8   7
# 0.97       |   70  35   24  18   14  12   10   9    8   8
# 0.975      |   74  37   25  19   15  13   11  10    9   8
# 0.98       |   78  39   26  20   16  13   12  10    9   8
# 0.99       |   92  46   31  23   19  16   14  12   11  10

# jfa:
#            | Materiality
# Confidence | 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5
# 0.95       |   60  30   20  15   12  10    9   8    7   6
# 0.96       |   65  33   22  17   13  11   10   9    8   7
# 0.97       |   71  36   24  18   15  12   11   9    8   8
# 0.975      |   74  37   25  19   15  13   11  10    9   8
# 0.98       |   79  40   27  20   16  14   12  10    9   8
# 0.99       |   93  47   31  24   19  16   14  12   11  10

test_that(desc = "(id: f13-v0.6.5-t1) Test frequentist sample sizes", {
  testthat::skip_on_cran()
  theta <- c(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000) / 20000 # materiality divided by N
  confidence <- c(0.95, 0.96, 0.97, 0.975, 0.98, 0.99)
  sampleSizeMatrix <- matrix(NA, nrow = length(confidence), ncol = length(theta))
  for (i in seq_len(nrow(sampleSizeMatrix))) {
    for (j in seq_len(ncol(sampleSizeMatrix))) {
      plan <- planning(materiality = theta[j], conf.level = confidence[i], likelihood = "poisson")
      sampleSizeMatrix[i, j] <- plan[["n"]]
    }
  }
  sra_matrix <- matrix(
    c(
      60, 30, 20, 15, 12, 10, 9, 8, 7, 6, # 95%
      64 + 1, 32 + 1, 22, 16 + 1, 13, 11, 10, 8 + 1, 8, 7, # 96%: 64 -> 65 as qgamma(p = 0.96, shape = 1, rate = 64) > 0.05; 32 -> 33; 16 -> 17; 8 -> 9
      70 + 1, 35 + 1, 24, 18, 14 + 1, 12, 10 + 1, 9, 8, 8, # 97%: 70 -> 71 as qgamma(p = 0.97, shape = 1, rate = 70) > 0.05; 35 -> 36; 14 -> 15; 10 -> 11
      74, 37, 25, 19, 15, 13, 11, 10, 9, 8, # 97.5%
      78 + 1, 39 + 1, 26 + 1, 20, 16, 13 + 1, 12, 10, 9, 8, # 98%: 78 -> 79 as qgamma(p = 0.98, shape = 1, rate = 78) > 0.05; 39 -> 40; 26 -> 27; 13 -> 14
      92 + 1, 46 + 1, 31, 23 + 1, 19, 16, 14, 12, 11, 10 # 99%: 92 -> 93 as qgamma(p = 0.99, shape = 1, rate = 92) > 0.05; 46 -> 47; 23 -> 24
    ),
    byrow = TRUE,
    nrow = length(confidence),
    ncol = length(theta)
  )
  expect_equal(sampleSizeMatrix, sra_matrix)
})
