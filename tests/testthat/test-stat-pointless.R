test_that("collisions produce composite location labels", {
  df1 <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(1, 2, 1, 4, 1)
  )

  p <- ggplot(df1, aes(x, y)) +
    stat_pointless(aes(colour = after_stat(location)), location = "all")
  x <- layer_data(p)
  # x=1 is first AND minimum; x=5 is last AND minimum; x=3 is minimum only;
  # x=4 is maximum only. Row order follows the canonical iteration order:
  # first, last, minimum, maximum.
  expect_equal(
    x$location,
    factor(
      c("first, minimum", "last, minimum", "minimum", "maximum"),
      levels = c("first, minimum", "last, minimum", "minimum", "maximum")
    )
  )
})

test_that("geom_pointless and stat_pointless produce same layer data", {
  set.seed(42)
  df1 <- data.frame(x = 1:10, y = sample(1:10))
  p1 <- ggplot(df1, aes(x, y)) +
    stat_pointless(aes(color = after_stat(location)),
      location = c("first", "last")
    )
  p2 <- ggplot(df1, aes(x, y)) +
    geom_pointless(aes(color = after_stat(location)),
      location = c("first", "last")
    )

  expect_equal(
    layer_data(p1),
    layer_data(p2)
  )
})
