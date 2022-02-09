test_that("order of ploting is first > last > minimum > maximum", {
  df1 <- data.frame(x = c(1, 2, 3, 4, 5),
                    y = c(1, 2, 1, 4, 1))

  p <- ggplot(df1, aes(x, y)) + stat_pointless(aes(colour = after_stat(location)), location = "all")
  x <- layer_data(p)
  expect_equal(x$location,
               factor(c("minimum", "first", "minimum", "maximum", "minimum", "last"),
                      levels = c("first", "last", "minimum", "maximum"))
  )
})
