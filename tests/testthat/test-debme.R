test_that("reverse embed works", {
  expect_equal(debme(1:3), matrix(c(1, 2, 2, 3), nrow = 2))
})
