test_that("order of ploting is first > last > minimum > maximum", {
  df1 <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(1, 2, 1, 4, 1)
  )

  p <- ggplot(df1, aes(x, y)) +
    stat_pointless(aes(colour = after_stat(location)), location = "all")
  x <- layer_data(p)
  # Deduplication: first (x=1, y=1) and last (x=5, y=1) are both minimum,
  # so x=1 is labelled "first", x=5 "last", and only x=3 gets "minimum".
  expect_equal(
    x$location,
    factor(c("first", "last", "minimum", "maximum"),
      levels = c("first", "last", "minimum", "maximum")
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
