test_that("catenary param is calculated corrected", {
  expect_equal(round(getCatenaryParameter(h = 1, v = 0, L = 2), 6), 0.229640)
})
