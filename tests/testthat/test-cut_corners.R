open <- list(
  x = c(4, 4, 4.25, 4.75, 5, 5),
  y = c(2, 2.75, 3, 3, 2.75, 2)
)

closed <- list(
  x = c(4.25, 4, 4, 4.25, 4.75, 5, 5, 4.75),
  y = c(2, 2.25, 2.75, 3, 3, 2.75, 2.25, 2)
)

test_that("multiplication works", {
  expect_equal(open, cut_corners(x = c(4, 4, 5, 5), y = c(2, 3, 3, 2), ratio = .25, closed = FALSE))
  expect_equal(closed, cut_corners(x = c(4, 4, 5, 5), y = c(2, 3, 3, 2), ratio = .25, closed = TRUE))
})
