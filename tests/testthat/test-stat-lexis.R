test_that("stat_lexis requires an x and xend aesthetic", {
  df1 <- data.frame(x = 1, xend = 2, y = 3)
  expect_message(ggplot_build(ggplot(df1, aes(x, y, xend = xend)) +
    stat_lexis()),
  regexp = "calculates y and yend aesthetics for you."
  )
})
