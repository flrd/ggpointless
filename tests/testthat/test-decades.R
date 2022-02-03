test_that("returns correct decade", {
  expect_equal(decades(c(1, -1, 2019:2021)), c("00's", "00's", "2010's", "2020's", "2020's"))
})

test_that("only numeric vectors are allowed", {
  expect_error(decades("2022"))
})
