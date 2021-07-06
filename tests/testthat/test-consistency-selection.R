context("6. Test consistency of function selection()")

# jfa version 0.1.0

test_that(desc = "(id: f6-v0.1.0-t1) Test for units = 'records' and algorithm = 'random'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "records", algorithm = "random", ordered = TRUE)
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: f6-v0.1.0-t2) Test for units = 'mus' and algorithm = 'random'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "mus", algorithm = "random", bookValues = "bookValue")
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: f6-v0.1.0-t3) Test for units = 'records' and algorithm = 'cell'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "records", algorithm = "cell")
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: f6-v0.1.0-t4) Test for units = 'mus' and algorithm = 'cell'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "mus", algorithm = "cell", bookValues = "bookValue")
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: f6-v0.1.0-t5) Test for units = 'records' and algorithm = 'interval'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "records", algorithm = "interval", intervalStartingPoint = 3)
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: f6-v0.1.0-t6) Test for units = 'mus' and algorithm = 'interval'", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "mus", algorithm = "interval", bookValues = "bookValue", intervalStartingPoint = 3)
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: f6-v0.1.0-t7) Test for units = 'mus' and algorithm = 'interval' using negative book values", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = c(-100, runif(n = 999, min = 100, max = 500)))
  jfaRes <- expect_warning(selection(population, sampleSize = 100, units = "mus", algorithm = "interval", bookValues = "bookValue", intervalStartingPoint = 3))
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})


# jfa version 0.2.0
# No changes to be benchmarked

# jfa version 0.3.0
# No changes to be benchmarked

# jfa version 0.3.1
# No changes to be benchmarked

# jfa version 0.4.0
# No changes to be benchmarked

# jfa version 0.5.0

test_that(desc = "(id: f6-v0.5.0-t1) Test for summary function", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "mus", algorithm = "random", ordered = TRUE, bookValues = "bookValue")
  invisible(capture.output(summary(jfaRes)))
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
  
  jfaRes <- selection(population, sampleSize = 100, units = "records", algorithm = "random", ordered = TRUE)
  invisible(capture.output(summary(jfaRes)))
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: f6-v0.5.0-t2) Test for plot function", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "mus", algorithm = "random", ordered = TRUE, bookValues = "bookValue")
  invisible(capture.output(plot(jfaRes)))
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

# jfa version 0.5.1
# No changes to be benchmarked

# jfa version 0.5.2
# No changes to be benchmarked

# jfa version 0.5.3
# No changes to be benchmarked

# jfa version 0.5.4
# No changes to be benchmarked

# jfa version 0.5.5
# No changes to be benchmarked

# jfa version 0.5.5.1
# No changes to be benchmarked