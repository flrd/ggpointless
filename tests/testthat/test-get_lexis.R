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


test_that("xend must be NA or greater than x", {
  expect_error(
    get_lexis(c(-1, 0), c(-4, 5)),
    "For each row in your data, `xend` must be greater than `x`"
  )
})

test_that("works only for numeric input", {
  expect_error(get_lexis(1, "2"),
    regexp = "`x` and `xend` must be continuous."
  )
})
