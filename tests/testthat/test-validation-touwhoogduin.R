context("12. Benchmark against Touw and Hoogduin (2011)")

# Touw, P., and Hoogduin, L. (2011). Statistiek voor audit en controlling. Boom uitgevers, Amsterdam.

# Table 1.3 on page 17

test_that(desc = "(id: f12-v0.5.1-t1) Test Sample sizes on page 17", {
  SR <- c(0.05, 0.10, 0.25, 0.40, 0.50)
  materiality <- 0.01
  n <- numeric(length(SR))
  for(i in 1:length(SR))
    n[i] <- jfa::planning(confidence = 1 - SR[i], materiality = materiality, likelihood = "poisson")$sampleSize
  expect_equal(n, c(300, 230 + 1, 140 - 1, 90 + 2, 70)) # Table on page 17 is inconsistent with the rest of the book.
})

# Example on page 23

test_that(desc = "(id: f12-v0.5.1-t2) Test Sample sizes on page 23", {
  n <- jfa::planning(confidence = 1 - 0.05, materiality =  100000 / 5000000, likelihood = "poisson")$sampleSize
  expect_equal(n, 150)
  n <- jfa::planning(confidence = 1 - 0.125, materiality =  100000 / 5000000, likelihood = "poisson")$sampleSize
  expect_equal(n, 104)
})