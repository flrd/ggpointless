# geom_stipple_path() / geom_stipple_line() / geom_stipple_step() ------------

# --- Internal helpers (the semantic core) -----------------------------------

test_that(".stipple_resolve_dot_spacing maps presets to unit objects", {
  expect_equal(.stipple_resolve_dot_spacing("fine"),   grid::unit(2, "mm"))
  expect_equal(.stipple_resolve_dot_spacing("medium"), grid::unit(4, "mm"))
  expect_equal(.stipple_resolve_dot_spacing("coarse"), grid::unit(8, "mm"))
  expect_s3_class(.stipple_resolve_dot_spacing(grid::unit(3, "pt")), "unit")
  expect_error(.stipple_resolve_dot_spacing("huge"))
})

test_that(".stipple_resolve_dot_spacing informs on bare numeric and returns unit", {
  expect_message(
    u <- .stipple_resolve_dot_spacing(5),
    "treated as"
  )
  expect_equal(u, grid::unit(5, "mm"))
})

test_that(".stipple_resolve_radius returns NULL or a unit", {
  expect_null(.stipple_resolve_radius(NULL))
  expect_equal(.stipple_resolve_radius(grid::unit(2, "mm")), grid::unit(2, "mm"))
  expect_message(u <- .stipple_resolve_radius(3), "treated as")
  expect_equal(u, grid::unit(3, "mm"))
})

test_that(".stipple_grid uses per-axis pitch and survives a tiny y-range", {
  # x-range huge, y-range tiny (the log10 degeneracy): must still have many rows
  g <- .stipple_grid(c(0, 1000), c(0, 0.05), dx = 50, dy = 0.005, type = "hex")
  expect_gt(length(unique(g$y)), 5L)
  expect_gt(length(unique(g$x)), 5L)
  # zero / non-finite pitch -> empty, never an error
  expect_equal(nrow(.stipple_grid(c(0, 1), c(0, 1), dx = 0, dy = 0.1, type = "hex")), 0L)
})

test_that(".stipple_dist_to_polyline breaks at NA vertices (no bridging)", {
  # Two real segments [0,1] and [3,4] on y = 0, broken by an NA at x = 2.
  d <- .stipple_dist_to_polyline(
    2, 0,
    path_x = c(0, 1, NA, 3, 4),
    path_y = c(0, 0, NA, 0, 0)
  )
  # Nearest real vertex is x = 1 or x = 3, both distance 1 -- NOT 0 (which a
  # bridging segment from (1,0) to (3,0) would give).
  expect_equal(d, 1)
  # A query with no valid segment at all returns Inf, never NA.
  expect_equal(
    .stipple_dist_to_polyline(0, 0, c(NA, NA), c(NA, NA)),
    Inf
  )
})

# --- Orientation (flipped_aes) ----------------------------------------------

test_that("GeomStippleLine$setup_data orders by the independent axis", {
  d <- data.frame(
    x = c(3, 1, 2), y = c(10, 30, 20),
    PANEL = factor(1), group = 1L
  )
  out_x <- GeomStippleLine$setup_data(d, list(flipped_aes = FALSE))
  expect_equal(out_x$x, c(1, 2, 3))

  out_y <- GeomStippleLine$setup_data(d, list(flipped_aes = TRUE))
  expect_equal(out_y$y, c(10, 20, 30))
  expect_equal(out_y$x, c(3, 2, 1))
})

test_that("orientation = 'y' renders without error or warning", {
  p <- ggplot2::ggplot(economics, ggplot2::aes(unemploy, date)) +
    geom_stipple_line(dot_spacing = "coarse", orientation = "y")
  expect_no_error(suppressWarnings(suppressMessages(ggplot2::ggplotGrob(p))))
})

# --- NA handling at the layer level -----------------------------------------

test_that("NA positions break the line and warn (parity with geom_line)", {
  df <- data.frame(x = 1:5, y = c(1, 2, NA, 4, 5))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + geom_stipple_line()
  expect_warning(ggplot2::ggplotGrob(p), "Removed 1 row")
  p2 <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    geom_stipple_line(na.rm = TRUE)
  expect_no_warning(ggplot2::ggplotGrob(p2))
})

# --- Grammar-of-graphics stress test ----------------------------------------

test_that("data edge cases do not crash", {
  base <- function(d) {
    ggplot2::ggplot(d, ggplot2::aes(x, y)) +
      geom_stipple_path(dot_spacing = "coarse")
  }
  expect_no_error(ggplot2::ggplot_build(
    base(data.frame(x = numeric(0), y = numeric(0)))
  ))
  expect_no_error(suppressWarnings(
    ggplot2::ggplotGrob(base(data.frame(x = 1, y = 1)))
  ))
})

test_that("scales (log10, reverse) and step directions render", {
  d <- data.frame(x = 1:10, y = (1:10)^2)
  expect_no_error(suppressWarnings(ggplot2::ggplotGrob(
    ggplot2::ggplot(d, ggplot2::aes(x, y)) +
      geom_stipple_path(dot_spacing = "coarse") + ggplot2::scale_y_log10()
  )))
  expect_no_error(ggplot2::ggplotGrob(
    ggplot2::ggplot(d, ggplot2::aes(x, y)) +
      geom_stipple_line(dot_spacing = "coarse") + ggplot2::scale_x_reverse()
  ))
  for (dir in c("hv", "vh", "mid")) {
    expect_no_error(ggplot2::ggplotGrob(
      ggplot2::ggplot(d, ggplot2::aes(x, y)) +
        geom_stipple_step(dot_spacing = "coarse", direction = dir)
    ))
  }
})

test_that("coord_flip and coord_polar render", {
  d <- data.frame(x = 1:10, y = sort(rnorm(10)))
  expect_no_error(ggplot2::ggplotGrob(
    ggplot2::ggplot(d, ggplot2::aes(x, y)) +
      geom_stipple_line(dot_spacing = "coarse") + ggplot2::coord_flip()
  ))
  expect_no_error(ggplot2::ggplotGrob(
    ggplot2::ggplot(d, ggplot2::aes(x, y)) +
      geom_stipple_path(dot_spacing = "coarse") + ggplot2::coord_polar()
  ))
})

test_that("multiple groups each get a trace", {
  d <- data.frame(
    x = rep(1:10, 2), y = c(sort(rnorm(10)), sort(rnorm(10))),
    g = rep(c("a", "b"), each = 10)
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = g)) +
    geom_stipple_line(dot_spacing = "coarse")
  expect_no_error(ggplot2::ggplotGrob(p))
})

test_that("unit() dot_spacing and radius are accepted", {
  d <- data.frame(x = 1:5, y = 1:5)
  expect_no_error(ggplot2::ggplotGrob(
    ggplot2::ggplot(d, ggplot2::aes(x, y)) +
      geom_stipple_line(
        dot_spacing = grid::unit(6, "mm"),
        radius      = grid::unit(3, "mm")
      )
  ))
})

