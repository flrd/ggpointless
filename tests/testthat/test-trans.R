sd <- second_day_trans()
dw <- day_week_trans()

test_that("transformations work", {
  expect_equal(sd$invers(60 * 60 * 24), 1)
  expect_equal(dw$invers(7), 1)
})

test_that("transformations don't work for non-numeric inputwork", {
  expect_error(dw$invers("7"), regexp = "non-numeric argument to binary operator")
})
