test_that("is_integer works", {
  expect_equal(is_integer(3), TRUE)
  expect_equal(is_integer(-3), TRUE)
  expect_equal(is_integer(3L), TRUE)
  expect_equal(is_integer(3.1), FALSE)
  expect_equal(is_integer("3"), FALSE)
})
