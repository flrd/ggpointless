test_that("location returned in desired order", {
  expect_equal(
    get_location_order(c("first", "last", "minimum", "maximum")), c("maximum", "minimum", "last", "first"))
  expect_equal(get_location_order(c("first", "maximum")), c("maximum", "first"))
  expect_equal(get_location_order(c("first")), c("first"))
  expect_equal(get_location_order(c("maximum", "minimum", "last", "first")), c("maximum", "minimum", "last", "first"))
})
