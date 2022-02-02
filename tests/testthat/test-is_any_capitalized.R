test_that("Capitalized strings are detected", {
  expect_equal(is_any_capitalized(c("Foo", "bar")), TRUE)
  expect_equal(is_any_capitalized(c("foo", "Bar")), TRUE)
  expect_equal(is_any_capitalized(c("foo", "bar")), FALSE)
})
