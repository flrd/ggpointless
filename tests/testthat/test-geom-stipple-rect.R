# geom_stipple_rect() --------------------------------------------------------

test_that(".stipple_points_in_rect respects the boundary mode", {
  grid <- data.frame(x = c(1, 2, 3), y = c(1, 2, 3))
  inside <- .stipple_points_in_rect(grid, 1, 3, 1, 3, boundary = "inside")
  on     <- .stipple_points_in_rect(grid, 1, 3, 1, 3, boundary = "on")
  expect_equal(inside, c(FALSE, TRUE, FALSE)) # corners excluded
  expect_equal(on,     c(TRUE,  TRUE, TRUE))  # corners included
})

test_that("a rectangle is filled with dots", {
  df <- data.frame(xmin = 1, xmax = 5, ymin = 1, ymax = 4)
  p <- ggplot2::ggplot(df) +
    geom_stipple_rect(ggplot2::aes(
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
    ))
  expect_no_error(ggplot2::ggplotGrob(p))
})

test_that("missing required aesthetics raise a clean error", {
  df <- data.frame(xmin = 1, xmax = 5, ymin = 1)
  p <- ggplot2::ggplot(df) +
    geom_stipple_rect(ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin))
  expect_error(ggplot2::ggplotGrob(p))
})

# --- Grammar-of-graphics stress test ----------------------------------------

test_that("rect geom renders under scales and coord", {
  df <- data.frame(xmin = 1, xmax = 5, ymin = 1, ymax = 4)
  base <- ggplot2::ggplot(df) +
    geom_stipple_rect(ggplot2::aes(
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
    ), dot_spacing = "coarse")
  expect_no_error(ggplot2::ggplotGrob(base + ggplot2::coord_flip()))
  expect_no_error(ggplot2::ggplotGrob(base + ggplot2::scale_x_reverse()))
})

test_that("unit() dot_spacing is accepted", {
  df <- data.frame(xmin = 1, xmax = 5, ymin = 1, ymax = 4)
  p <- ggplot2::ggplot(df) +
    geom_stipple_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      dot_spacing = grid::unit(6, "mm")
    )
  expect_no_error(ggplot2::ggplotGrob(p))
})
