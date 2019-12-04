context("Sampling")

test_that(desc = "Random record sampling", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- sampling(population, sampleSize = 100, units = "records", algorithm = "random", ordered = TRUE)
  print(jfaRes)
  expect_equal(ncol(jfaRes$sample), 4)
  expect_equal(nrow(jfaRes$sample), 100)
})

test_that(desc = "Random monetary unit sampling", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- sampling(population, sampleSize = 100, units = "mus", algorithm = "random", bookValues = "bookValue")
  print(jfaRes)
  plot(jfaRes)
  expect_equal(ncol(jfaRes$sample), 4)
  expect_equal(nrow(jfaRes$sample), 100)
})

test_that(desc = "Cell record sampling", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- sampling(population, sampleSize = 100, units = "records", algorithm = "cell")
  print(jfaRes)
  expect_equal(ncol(jfaRes$sample), 4)
  expect_equal(nrow(jfaRes$sample), 100)
})

test_that(desc = "Cell monetary unit sampling", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- sampling(population, sampleSize = 100, units = "mus", algorithm = "cell", bookValues = "bookValue")
  print(jfaRes)
  plot(jfaRes)
  expect_equal(ncol(jfaRes$sample), 4)
  expect_equal(nrow(jfaRes$sample), 100)
})

test_that(desc = "Interval record sampling", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- sampling(population, sampleSize = 100, units = "records", algorithm = "interval", intervalStartingPoint = 3)
  print(jfaRes)
  expect_equal(ncol(jfaRes$sample), 4)
  expect_equal(nrow(jfaRes$sample), 100)
})

test_that(desc = "Interval monetary unit sampling", {
  set.seed(1)
  population <- data.frame(ID = sample(1000:100000, size = 1000, replace = FALSE), bookValue = runif(n = 1000, min = 100, max = 500))
  jfaRes <- sampling(population, sampleSize = 100, units = "mus", algorithm = "interval", bookValues = "bookValue", intervalStartingPoint = 3)
  print(jfaRes)
  plot(jfaRes)
  expect_equal(ncol(jfaRes$sample), 4)
  expect_equal(nrow(jfaRes$sample), 100)
})