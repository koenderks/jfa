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

context("Validation of function selection")

# jfa version 0.1.0

test_that(desc = "(id: f6-v0.1.0-t1) Test for units = 'items' and method = 'random'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, size = 100, units = "items", method = "random")
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: f6-v0.1.0-t2) Test for units = 'values' and method = 'random'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, size = 100, units = "values", method = "random", values = "bookValue")
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 94)
})

test_that(desc = "(id: f6-v0.1.0-t3) Test for units = 'items' and method = 'cell'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, size = 100, units = "items", method = "cell")
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: f6-v0.1.0-t4) Test for units = 'values' and method = 'cell'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, size = 100, units = "values", method = "cell", values = "bookValue")
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: f6-v0.1.0-t5) Test for units = 'items' and method = 'interval'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, size = 100, units = "items", method = "interval", start = 3)
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
  sample_rows <- c(
    3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 103, 113, 123, 133, 143,
    153, 163, 173, 183, 193, 203, 213, 223, 233, 243, 253, 263, 273,
    283, 293, 303, 313, 323, 333, 343, 353, 363, 373, 383, 393, 403,
    413, 423, 433, 443, 453, 463, 473, 483, 493, 503, 513, 523, 533,
    543, 553, 563, 573, 583, 593, 603, 613, 623, 633, 643, 653, 663,
    673, 683, 693, 703, 713, 723, 733, 743, 753, 763, 773, 783, 793,
    803, 813, 823, 833, 843, 853, 863, 873, 883, 893, 903, 913, 923,
    933, 943, 953, 963, 973, 983, 993
  )
  expect_equal(jfaRes[["sample"]]$row, sample_rows)
})

test_that(desc = "(id: f6-v0.1.0-t6) Test for units = 'values' and method = 'interval'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, size = 100, units = "values", method = "interval", values = "bookValue", start = 3)
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
  sample_rows <- c(
    1, 11, 21, 32, 45, 54, 65, 76, 86, 97, 107, 118, 128, 139, 151,
    162, 171, 180, 192, 203, 213, 220, 228, 239, 248, 257, 269, 278,
    289, 297, 306, 316, 327, 338, 347, 357, 367, 377, 386, 396, 408,
    419, 430, 440, 450, 457, 468, 477, 487, 497, 508, 516, 524, 533,
    546, 556, 567, 576, 585, 594, 604, 613, 623, 634, 646, 656, 665,
    673, 682, 691, 700, 713, 723, 734, 741, 753, 763, 774, 784, 794,
    803, 813, 823, 831, 842, 850, 860, 868, 878, 887, 899, 907, 916,
    926, 936, 948, 962, 971, 980, 991
  )
  expect_equal(jfaRes[["sample"]]$row, sample_rows)
})

test_that(desc = "(id: f6-v0.1.0-t7) Test for units = 'values' and method = 'interval' using negative book values", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = c(-100, runif(n = 999, min = 100, max = 500)))
  jfaRes <- expect_message(selection(population, size = 100, units = "values", method = "interval", values = "bookValue", start = 3))
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
  sample_rows <- c(
    2, 12, 22, 33, 46, 55, 66, 77, 86, 98, 108, 119, 129, 140, 152,
    163, 172, 181, 192, 204, 214, 221, 229, 240, 249, 258, 269, 279,
    290, 298, 307, 317, 328, 339, 348, 358, 367, 378, 387, 396, 409,
    420, 431, 441, 451, 458, 469, 478, 488, 498, 509, 516, 525, 534,
    546, 557, 567, 577, 586, 595, 605, 614, 624, 635, 647, 657, 665,
    674, 683, 692, 700, 713, 724, 734, 742, 754, 764, 775, 784, 795,
    804, 813, 824, 832, 842, 850, 861, 868, 879, 888, 899, 907, 916,
    926, 937, 949, 962, 971, 980, 991
  )
  expect_equal(jfaRes[["sample"]]$row, sample_rows)
})


# jfa version 0.2.0 - 0.4.0
# No changes to be benchmarked

# jfa version 0.5.0

test_that(desc = "(id: f6-v0.5.0-t1) Test for summary and print function", {
  testthat::skip_on_cran()
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, size = 100, units = "values", method = "random", order = "bookValue", values = "bookValue")
  invisible(capture.output(print(jfaRes)))
  invisible(capture.output(summary(jfaRes)))
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 96)
  jfaRes <- selection(population, size = 100, units = "items", method = "random")
  invisible(capture.output(print(jfaRes)))
  invisible(capture.output(summary(jfaRes)))
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

# jfa version 0.5.1 - 0.5.7
# No changes to be benchmarked

# jfa 0.6.0

test_that(desc = "(id: f6-v0.1.0-t5) Test for units = 'values' and method = 'sieve'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, size = 100, units = "values", method = "sieve", values = "bookValue")
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

# jfa 0.6.1
test_that(desc = "(id: f6-v0.6.1-t1) Test sample randomization", {
  set.seed(1)
  population <- data.frame(ID = 1:100)
  jfaRes <- selection(population, size = 10, randomize = TRUE)
  expect_equal(nrow(jfaRes[["sample"]]), 10)
})

test_that(desc = "(id: f6-v0.6.1-t1) Test interval selection consistency", {
  population <- data.frame(ID = 1:238)
  population$Ist <- c(
    8449, 1887, 437.5, 525, 3086, 1312.5, 1137.5, 1400, 1137.5, 2199, 962.5, 1050, 1487.5, 1837.5,
    1575, 962.5, 1400, 437.5, 2062, 4950, 7800, 11220, 292.5, 5046.389, 1839.722, 5539.722, 4994.073,
    5539.722, 1377.222, 348.5789, 1839.722, 3237.5, 3700, 3693.161, 4573.611, 1839.722, 1839.722, 1459.444,
    3391.667, 754.1249, 1839.722, 931.9159, 5539.722, 1377.222, 1366.944, 1654.722, 1839.722, 1603.333, 1839.722,
    1839.722, 1839.722, 2456.389, 1839.722, 1839.722, 1366.944, 1531.389, 1839.722, 931.9159, 1839.722, 1839.722,
    1366.944, 1839.722, 1839.722, 931.9159, 1839.722, 5539.722, 5539.722, 931.9159, 5539.722, 4563.333, 5539.722,
    5539.722, 3196.389, 4306.389, 5539.722, 5539.722, 931.9159, 3700, 2158.333, 3327.531, 3782.222, 5539.722, 5539.722,
    5539.722, 1839.722, 5539.722, 931.9159, 5539.722, 3700, 931.9159, 5539.722, 931.9159, 5539.722, 5539.722, 1839.722,
    1839.722, 1377.222, 754.1249, 900.0471, 931.9159, 5241.667, 931.9159, 4141.944, 1839.722, 1839.722, 5539.722, 931.9159,
    931.9159, 5539.722, 1839.722, 2590, 3782.222, 3782.222, 931.9159, 5539.722, 931.9159, 5046.389, 917.4844, 1839.722, 3062.778,
    1839.722, 1366.944, 1377.222, 4316.667, 4316.667, 4316.667, 3700, 610.2966, 4316.667, 3700, 1531.389, 3473.889, 4316.667, 610.2966,
    3083.333, 3278.611, 3391.667, 659.1884, 610.2966, 4316.667, 3700, 3484.167, 3700, 603.7037, 610.2966, 2637.464, 659.1884, 2775, 659.1884,
    1901.389, 3278.611, 659.1884, 610.2966, 3093.611, 543.5663, 3391.667, 543.5663, 543.5663, 560.4174, 509.5206, 610.2966, 560.4174, 2980.556,
    3391.667, 659.1884, 659.1884, 543.5663, 543.5663, 3700, 3700, 659.1884, 610.2966, 610.2966, 402.7604, 1439.459, 659.1884, 3391.667, 1850,
    457.5747, 2148.056, 3700, 3391.667, 1366.944, 3350.556, 1993.889, 4316.667, 4255, 3206.667, 3391.667, 3103.889, 3782.222, 4316.667, 543.5663,
    4316.667, 4316.667, 1603.333, 3905.556, 931.9159, 1839.722, 5539.722, 1798.611, 3700, 1850, 602.0527, 5539.722, 3689.722, 180.9127, 1921.944,
    3700, 3700, 1455.738, 1541.667, 1850, 1921.944, 1850, 925, 1850, 1223.056, 3700, 1223.056, 2960, 3700, 3391.667, 1192.222, 1066.597, 1223.056,
    2960, 2220, 3700, 815.3495, 2678.125, 513.8889, 585.8333, 308.3333, 5539.722, 4178.003, 382.1974, 534.1875
  )
  jfaRes <- selection(population, size = 130, units = "values", values = "Ist", method = "interval", start = 3773)
  res <- jfaRes[["sample"]]$times
  reference <- c(
    2, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 2, 2, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1,
    0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 2, 1, 1, 1, 1, 1, 1, 1, 0, 1,
    1, 0, 1, 1, 1, 2, 0, 1, 0, 2, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1,
    0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0,
    1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1,
    0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0,
    2, 0, 1, 0
  )
  reference <- reference[reference > 0]
  expect_equal(res, reference)
})
