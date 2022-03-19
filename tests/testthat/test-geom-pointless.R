test_that("geom_pointless accepts only 'first', 'last', 'minimum', 'maximum', and 'all'", {
  df1 <- data.frame(x = 1:3, y = 1:3)
  p <- ggplot(df1, aes(x, y)) +
    geom_pointless(location = c("foo", "bar"))
  expect_warning(print(p))
})

test_that("location = 'all' is equal to c('first', 'last', 'minimum', 'maximum')", {
  set.seed(42)
  df2 <- data.frame(x = 1:10, y = sample(1:10))
  p1 <- ggplot(df2, aes(x, y)) +
    geom_pointless(location = c("all"))
  p2 <- ggplot(df2, aes(x, y)) +
    geom_pointless(location = c("first", "last", "minimum", "maximum"))
  expect_equal(layer_data(p1), layer_data(p2))

  df3 <- data.frame(
    var1 = 1:2,
    var2 = 1:2
  )
  p <- ggplot(df3, aes(x = var1, y = var2))
  p1 <- p + geom_pointless(aes(color = after_stat(location)),
    location = c("first", "last", "minimum", "maximum")
  )
  p2 <- p + geom_pointless(aes(color = after_stat(location)),
    location = c("maximum", "minimum", "last", "first", "all")
  )
  expect_equal(layer_data(p1), layer_data(p2))
})

test_that("geom_pointless works in both directions", {
  df3 <- data.frame(
    x = c(1, 2, 3),
    y = c(1, 2, 1)
  )

  p <- ggplot(df3, aes(x, y)) +
    geom_line() +
    geom_pointless(location = "all")
  x <- layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(df3, aes(y, x)) +
    geom_line(orientation = "y") +
    geom_pointless(location = "all", orientation = "y")
  y <- layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, ggplot2::flip_data(y, TRUE)[names(x)])
})
