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

context("Tests for function repeated.test()")

test_that(desc = "Print and plot call", {
  data("sanitizer")
  res <- repeated.test(x = sanitizer$value, check = "last", method = "af", B = 500)
  invisible({
    capture.output({
      print(res)
    })
  })
  invisible({
    capture.output({
      plot(res)
    })
  })
  expect_equal(length(res$statistic), 1)
})

test_that(desc = "Validate Datacolada[77]", {
  data("sanitizer")
  res <- repeated.test(x = sanitizer$value, check = "last", method = "af", B = 500)
  expect_equal(as.numeric(res$statistic), 1.5225)
  res <- repeated.test(x = sanitizer$value, check = "last", method = "entropy", B = 500)
  expect_equal(as.numeric(res$statistic), 7.065769174)
})
