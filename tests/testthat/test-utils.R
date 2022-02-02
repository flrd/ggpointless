library(testthat)
test_that("get_locations(..., first) returns first row", {
  expect_equal(
    get_locations(iris, location = "first"),
    cbind(iris[1,,drop = FALSE], location = "First")
  )
})

test_that("emtpy data argument, or non-data.frame gives error", {
  expect_error(get_locations(location = "all"))
})
