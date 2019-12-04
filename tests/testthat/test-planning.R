context("Planning")

test_that(desc = "1% materiality", {
  jfaRes <- planning(materiality = 0.01, confidence = 0.95, expectedError = 0, likelihood = "binomial")
  expect_equal(jfaRes$sampleSize, 299)
})

test_that(desc = "5% materiality", {
  jfaRes <- planning(materiality = 0.05, confidence = 0.95, expectedError = 0, likelihood = "binomial")
  expect_equal(jfaRes$sampleSize, 59)
})
