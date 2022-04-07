test_that("operator works", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
})
