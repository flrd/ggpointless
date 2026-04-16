df1 <- data.frame(
  x = c(1, 2, 2),
  xend = c(2, 2, 4),
  y = c(0, 1, 1),
  yend = c(1, 1, 3),
  type = c("solid", "dotted", "solid")
)
df2 <- data.frame(x = 0, xend = 5, y = 0, yend = 5, type = "solid")

test_that("get_lexis works", {
  expect_equal(df1, get_lexis(1:2, c(2, 4)))
  expect_equal(df2, get_lexis(0, 5))
})


test_that("rows where x > xend are swapped silently (handles scale_x_reverse)", {
  # Previously an error; now get_lexis swaps the pair so the algorithm
  # works correctly regardless of scale direction.
  expect_no_error(get_lexis(c(-1, 0), c(-4, 5)))
  # Row 1 is effectively swapped to x=-4, xend=-1; row 2 is unchanged.
  # The result must still be a data frame with the correct columns.
  out <- get_lexis(c(-1, 0), c(-4, 5))
  expect_true(is.data.frame(out))
  expect_named(out, c("x", "xend", "y", "yend", "type"))
})

test_that("works only for numeric input", {
  expect_error(get_lexis(1, "2"),
    regexp = "`x` and `xend` must be continuous."
  )
})
