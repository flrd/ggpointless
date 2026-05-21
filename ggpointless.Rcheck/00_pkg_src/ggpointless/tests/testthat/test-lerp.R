test_that("linear interpolation works", {
  expect_equal(2, lerp(a = 2, b = 1, ratio = 0))
  expect_equal(1, lerp(a = 2, b = 1, ratio = 1))
  expect_equal(1.75, lerp(a = 2, b = 1, ratio = 0.25))
})
