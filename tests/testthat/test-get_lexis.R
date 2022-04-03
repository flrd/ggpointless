df1 <- data.frame(
  x = 1:2,
  xend = c(2, 4),
  y = c(0, 3),
  yend = c(3, 3),
  type = c("solid", "dotted")
)
df2 <- data.frame(x = 0, xend = 5, y = 0, yend = 5, type = "solid")

test_that("missing xend values will be set to max(x)", {
  expect_equal(get_lexis(1:2, c(4, NA)), df1)
  expect_equal(get_lexis(0, 5), df2)
})

test_that("xend must be NA or greater than x", {
  expect_error(
    get_lexis(c(-1, 0), c(-4, 5)),
    "or each row in your data, `xend` must either be greater than `x`, or NA"
  )
})
