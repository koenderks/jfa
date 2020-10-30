context("7. Function test for selection()")

# jfa version 0.1.0

test_that(desc = "(id: 7.1) Random record sampling", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "records", algorithm = "random", ordered = TRUE)
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: 7.2) Random monetary unit sampling", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "mus", algorithm = "random", bookValues = "bookValue")
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: 7.3) Cell record sampling", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "records", algorithm = "cell")
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: 7.4) Cell monetary unit sampling", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "mus", algorithm = "cell", bookValues = "bookValue")
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: 7.5) Interval record sampling", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "records", algorithm = "interval", intervalStartingPoint = 3)
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

test_that(desc = "(id: 7.6) Interval monetary unit sampling", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- selection(population, sampleSize = 100, units = "mus", algorithm = "interval", bookValues = "bookValue", intervalStartingPoint = 3)
  expect_equal(ncol(jfaRes[["sample"]]), 4)
  expect_equal(nrow(jfaRes[["sample"]]), 100)
})

# jfa version 0.2.0
# No changes to be tested

# jfa version 0.3.0
# No changes to be tested

# jfa version 0.3.1
# No changes to be tested

# jfa version 0.4.0
# No changes to be tested