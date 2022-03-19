df1 <- structure(list(x = c(1, 2), xend = c(2, 4), y = c(0, 3), yend = c(3,
3), type = c("solid", "11")), row.names = c(NA, -2L), class = "data.frame")

df2 <- data.frame(x = 0, xend = 5, y = 0, yend = 5, type = "solid")

test_that("missing xend values will be set to max(x)", {
  expect_equal(get_lexis(1:2, c(4, NA)), df1)
  expect_equal(get_lexis(0, 5), df2)
})
