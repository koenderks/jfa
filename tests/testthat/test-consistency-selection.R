context("Consistency of function selection()")

# jfa version 0.1.0

test_that(desc = "(id: f6-v0.1.0-t1) Test for units = 'items' and method = 'random'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, size = 100, units = "items", method = "random", order = TRUE)
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: f6-v0.1.0-t2) Test for units = 'values' and method = 'random'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, size = 100, units = "values", method = "random", values = "bookValue")
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
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
})

test_that(desc = "(id: f6-v0.1.0-t6) Test for units = 'values' and method = 'interval'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, size = 100, units = "values", method = "interval", values = "bookValue", start = 3)
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: f6-v0.1.0-t7) Test for units = 'values' and method = 'interval' using negative book values", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = c(-100, runif(n = 999, min = 100, max = 500)))
  jfaRes <- expect_warning(selection(population, size = 100, units = "values", method = "interval", values = "bookValue", start = 3))
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})


# jfa version 0.2.0 - 0.4.0
# No changes to be benchmarked

# jfa version 0.5.0

test_that(desc = "(id: f6-v0.5.0-t1) Test for summary and print function", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, size = 100, units = "values", method = "random", order = TRUE, values = "bookValue")
  invisible(capture.output(print(jfaRes)))
  invisible(capture.output(summary(jfaRes)))
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
  jfaRes <- selection(population, size = 100, units = "items", method = "random", order = TRUE)
  invisible(capture.output(print(jfaRes)))
  invisible(capture.output(summary(jfaRes)))
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: f6-v0.5.0-t2) Test for plot function", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, size = 100, units = "values", method = "random", order = TRUE, values = "bookValue")
  invisible(capture.output(plot(jfaRes)))
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

# jfa version 0.5.1 - 0.6.0
# No changes to be benchmarked