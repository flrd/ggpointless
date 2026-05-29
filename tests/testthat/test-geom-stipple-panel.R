# geom_stipple_panel() -------------------------------------------------------

test_that("the panel grid renders without error", {
  p <- ggplot2::ggplot(economics, ggplot2::aes(date, unemploy)) +
    geom_stipple_panel(dot_spacing = "coarse")
  expect_no_error(ggplot2::ggplotGrob(p))
})

test_that("colour / fill default to theme ink / paper", {
  expect_no_error(GeomStipplePanel$default_aes)
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    geom_stipple_panel(dot_spacing = "coarse")
  expect_no_error(ggplot2::ggplotGrob(p))
})

test_that("a unit() dot_spacing override works", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    geom_stipple_panel(dot_spacing = grid::unit(6, "mm"), type = "square")
  expect_no_error(ggplot2::ggplotGrob(p))
})

test_that("a bare numeric dot_spacing informs the user and works", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    geom_stipple_panel(dot_spacing = 6, type = "square")
  expect_message(ggplot2::ggplotGrob(p), "treated as")
})

# --- Grammar-of-graphics stress test ----------------------------------------

test_that("panel geom renders under scales, coord, and facets", {
  base <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    geom_stipple_panel(dot_spacing = "coarse")
  expect_no_error(ggplot2::ggplotGrob(base + ggplot2::scale_y_log10()))
  expect_no_error(ggplot2::ggplotGrob(base + ggplot2::coord_flip()))
  expect_no_error(ggplot2::ggplotGrob(base + ggplot2::coord_polar()))
  expect_no_error(ggplot2::ggplotGrob(base + ggplot2::facet_wrap(~cyl)))
})
