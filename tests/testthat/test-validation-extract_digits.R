context("Tests for function extract_digits")

# First digits

test_that(desc = "Test for extraction of first digit (without 0)", {
  values <- c(0.00, 0.20, 1.23, 40.00, 54.04)
  digits <- jfa:::.extract_digits(x = values, check = "first", include.zero = FALSE)
  expect_equal(digits, c(NA, 2, 1, 4, 5))
})

test_that(desc = "Test for extraction of first digit (with 0)", {
  values <- c(0.00, 0.20, 1.23, 40.00, 54.04)
  digits <- jfa:::.extract_digits(x = values, check = "first", include.zero = TRUE)
  expect_equal(digits, c(0, 0, 1, 4, 5))
})

# First two digits

test_that(desc = "Test for extraction of first two digits (without 0)", {
  values <- c(0.00, 0.20, 1.23, 40.00, 54.04)
  digits <- jfa:::.extract_digits(x = values, check = "firsttwo", include.zero = FALSE)
  expect_equal(digits, c(NA, 20, 12, 40, 54))
})

test_that(desc = "Test for extraction of first two digits (with 0)", {
  values <- c(0.00, 0.20, 1.23, 40.00, 54.04)
  digits <- jfa:::.extract_digits(x = values, check = "firsttwo", include.zero = TRUE)
  expect_equal(digits, c(00, 02, 12, 40, 54))
})

# All digits before the dot

test_that(desc = "Test for extraction of all digits before the dot (without 0)", {
  values <- c(0.00, 0.20, 1.23, 40.00, 54.04)
  digits <- jfa:::.extract_digits(x = values, check = "before", include.zero = FALSE)
  expect_equal(digits, c(NA, NA, 1, 40, 54))
})

test_that(desc = "Test for extraction of all digits before the dot (with 0)", {
  values <- c(0.00, 0.20, 1.23, 40.00, 54.04)
  digits <- jfa:::.extract_digits(x = values, check = "before", include.zero = TRUE)
  expect_equal(digits, c(0, 0, 1, 40, 54))
})

# All digits after the dot

test_that(desc = "Test for extraction of all digits after the dot (without 0)", {
  values <- c(0.00, 0.20, 1.23, 40.00, 54.04)
  digits <- jfa:::.extract_digits(x = values, check = "after", include.zero = FALSE)
  expect_equal(digits, c(NA, 2, 23, NA, 4))
})

test_that(desc = "Test for extraction of all digits after the dot (with 0)", {
  values <- c(0.00, 0.20, 1.23, 40.00, 54.04)
  digits <- jfa:::.extract_digits(x = values, check = "after", include.zero = TRUE)
  expect_equal(digits, c(0, 20, 23, 0, 04))
})

# Last two digits

test_that(desc = "Test for extraction of last two digits (without 0)", {
  values <- c(0.00, 0.20, 1.23, 40.00, 54.04)
  digits <- jfa:::.extract_digits(x = values, check = "lasttwo", include.zero = FALSE)
  expect_equal(digits, c(NA, 20, 23, NA, 4))
})

test_that(desc = "Test for extraction of last two digits (with 0)", {
  values <- c(0.00, 0.20, 1.23, 40.00, 54.04)
  digits <- jfa:::.extract_digits(x = values, check = "lasttwo", include.zero = TRUE)
  expect_equal(digits, c(0, 20, 23, 0, 04))
})

# Last digits

test_that(desc = "Test for extraction of last digit (without 0)", {
  values <- c(0.00, 0.20, 1.23, 40.00, 54.04)
  digits <- jfa:::.extract_digits(x = values, check = "last", include.zero = FALSE)
  expect_equal(digits, c(NA, 2, 3, 4, 4))
})

test_that(desc = "Test for extraction of last digit (with 0)", {
  values <- c(0.00, 0.20, 1.23, 40.00, 54.04)
  digits <- jfa:::.extract_digits(x = values, check = "last", include.zero = TRUE)
  expect_equal(digits, c(0, 0, 3, 0, 4))
})
