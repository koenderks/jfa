context("Tests for function rv.test")

test_that(desc = "Print and plot call", {
  data("sanitizer")
  res <- digitTests::rv.test(x = sanitizer$value, check = "last", method = "af", B = 500)
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
  res <- digitTests::rv.test(x = sanitizer$value, check = "last", method = "af", B = 500)
  expect_equal(as.numeric(res$statistic), 1.5225)
  res <- digitTests::rv.test(x = sanitizer$value, check = "last", method = "entropy", B = 500)
  expect_equal(as.numeric(res$statistic), 7.065769174)
})
