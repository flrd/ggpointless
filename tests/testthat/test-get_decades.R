test_that("returns correct decade", {
  expect_equal(get_decades(c(1, -1, 2019:2021)), c("00's", "00's", "2010's", "2020's", "2020's"))
  expect_equal(get_decades(c(NA, 113, 1992, 2001, -9)), c(NA, "110's", "1990's", "2000's", "00's"))
  expect_equal(get_decades(c(NA, 113, 1992, 2001, -1999), anno_domini = FALSE), c(NA, "110's", "1990's", "2000's", "-1990's"))
})

test_that("only numeric vectors are allowed", {
  expect_error(get_decades("2022"))
  expect_error(get_decades(list(a = 1)))
  expect_error(get_decades(NULL))
})
