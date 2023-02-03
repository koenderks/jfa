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

context("Validation of function digit_test")

test_that(desc = "Print and plot call", {
  data("sinoForest")
  res <- digit_test(x = sinoForest$value, check = "first", reference = "benford")
  p <- plot(res)
  expect_equal(is.null(p), FALSE)
})

test_that(desc = "Validate Derks et al. (2020)", {
  data("sinoForest")
  res <- digit_test(x = sinoForest$value, check = "first", reference = "benford")
  expect_equal(as.numeric(res$n), 772)
  expect_equal(as.numeric(res$statistic), 7.6517426989499855)
  expect_equal(as.numeric(res$parameter), 8)
  expect_equal(as.numeric(res$p.value), 0.46820638130036729)
})

test_that(desc = "Validate uniform distribution", {
  res <- digit_test(x = 1:9, check = "first", reference = "uniform")
  expect_equal(as.numeric(res$n), 9)
  expect_equal(as.numeric(res$statistic), 0)
  expect_equal(as.numeric(res$parameter), 8)
  expect_equal(as.numeric(res$p.value), 1)
})

test_that(desc = "Validate benford.analysis package first digits", {
  ba <- benford.analysis::benford(data = sinoForest$value, number.of.digits = 1)
  dt <- digit_test(x = sinoForest$value, check = "first")
  expect_equal(as.numeric(ba$bfd$data.dist.freq), as.numeric(dt$observed))
  expect_equal(as.numeric(ba[["stats"]]$chisq$statistic), as.numeric(dt$statistic))
  expect_equal(as.numeric(ba[["stats"]]$chisq$parameter), as.numeric(dt$parameter))
  expect_equal(as.numeric(ba[["stats"]]$chisq$p.value), as.numeric(dt$p.value))
})

test_that(desc = "Validate benford.analysis package first and second digits", {
  ba <- benford.analysis::benford(data = sinoForest$value, number.of.digits = 2)
  dt <- digit_test(x = sinoForest$value, check = "firsttwo")
  expect_equal(as.numeric(ba$bfd$data.dist.freq), as.numeric(dt$observed))
  expect_equal(as.numeric(ba[["stats"]]$chisq$statistic), as.numeric(dt$statistic))
  expect_equal(as.numeric(ba[["stats"]]$chisq$parameter), as.numeric(dt$parameter))
  expect_equal(as.numeric(ba[["stats"]]$chisq$p.value), as.numeric(dt$p.value))
})

test_that(desc = "Validate BenfordTests package first digits", {
  bt <- BenfordTests::chisq.benftest(x = sinoForest$value, digits = 1)
  dt <- digit_test(x = sinoForest$value, check = "first")
  expect_equal(as.numeric(bt$statistic), as.numeric(dt$statistic))
  expect_equal(as.numeric(bt$p.value), as.numeric(dt$p.value))
})

test_that(desc = "Validate BeyondBenford package first digits", {
  bb <- BeyondBenford::chi2(sinoForest$value, mod = "ben", dig = 1, pval = 1)
  dt <- digit_test(x = sinoForest$value, check = "first")
  expect_equal(as.numeric(bb$chi2[2]), as.numeric(dt$statistic))
  expect_equal(as.numeric(bb$pval[2]), as.numeric(dt$p.value))
})

test_that(desc = "Validate Derks et al. (2020)", {
  data("sinoForest")
  res <- digit_test(x = sinoForest$value, check = "first", reference = "benford", prior = TRUE)
  expect_equal(1 / as.numeric(res$bf), 6899678.1488)
})

test_that(desc = "Validate uniform distribution", {
  res <- digit_test(x = 1:9, check = "first", reference = "uniform", prior = TRUE)
  expect_equal(1 / as.numeric(res$bf), 22.77012458)
})

test_that(desc = "Validate benford.analysis package", {
  ba <- benford.analysis::benford(data = sinoForest$value, number.of.digits = 1)
  dt <- digit_test(x = sinoForest$value, check = "first")
  expect_equal(as.numeric(ba$bfd$data.dist.freq), as.numeric(dt$observed))
})
