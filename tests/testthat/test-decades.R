test_that("multiplication works", {
  expect_equal(decades(c(1, -1, 2019:2021)), c("00's", "00's", "2010's", "2020's", "2020's"))
})
