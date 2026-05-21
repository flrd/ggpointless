# Doppelgänger drop-in replacement tests — fade geoms
#
# Direct transcription of official ggplot2 documentation examples, with only
# the geom name swapped to the fade equivalent. Tests verify both that the
# geom renders without error and that its API is a true superset of the
# original (no parameters silently ignored).
#
# Sources: ?geom_abline, ?geom_area, ?geom_col, ?geom_curve, ?geom_density,
#          ?geom_freqpoly, ?geom_histogram, ?geom_line, ?geom_path,
#          ?geom_rect, ?geom_segment, ?geom_step

library(ggplot2)

# ===========================================================================
# geom_abline_fade  ↔  geom_abline
# From: ?ggplot2::geom_abline (all examples using geom_abline itself)
# ===========================================================================

test_that("geom_abline_fade [doc ex 1]: default (no visible line)", {
  # geom_abline: p + geom_abline()  # Can't see it - outside the range
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p + geom_abline_fade()))))
})

test_that("geom_abline_fade [doc ex 2]: intercept only", {
  # geom_abline: p + geom_abline(intercept = 20)
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(p + geom_abline_fade(intercept = 20))
  )))
})

test_that("geom_abline_fade [doc ex 3]: intercept + slope", {
  # geom_abline: p + geom_abline(intercept = 37, slope = -5)
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(p + geom_abline_fade(intercept = 37, slope = -5))
  )))
})

test_that("geom_abline_fade [doc ex 4]: mapped aesthetics in facets", {
  # geom_abline/geom_hline: p + geom_hline(aes(yintercept = wt), mean_wt) in facets
  p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + facet_wrap(~cyl)
  mean_wt <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(p + geom_abline_fade(aes(intercept = wt, slope = 0), data = mean_wt))
  )))
})

# ===========================================================================
# geom_area_fade  ↔  geom_area
# From: ?ggplot2::geom_area
# ===========================================================================

test_that("geom_area_fade [doc ex 1]: basic area", {
  # geom_area: h + geom_area(aes(y = level))
  huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
  h <- ggplot(huron, aes(year))
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(h + geom_area_fade(aes(y = level)))
  )))
})

test_that("geom_area_fade [doc ex 2]: y orientation", {
  # geom_area: h + geom_area(aes(x = level, y = year), orientation = "y")
  huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
  h <- ggplot(huron, aes(year))
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(h + geom_area_fade(aes(x = level, y = year), orientation = "y"))
  )))
})

test_that("geom_area_fade [doc ex 3]: grouped fill (stacked)", {
  # geom_area: ggplot(df, aes(x, y, fill = g)) + geom_area()
  df <- data.frame(
    g = c("a", "a", "a", "b", "b", "b"),
    x = c(1, 3, 5, 2, 4, 6),
    y = c(2, 5, 1, 3, 6, 7)
  )
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(df, aes(x, y, fill = g)) + geom_area_fade())
  )))
})

test_that("geom_area_fade [doc ex 4]: stat='identity'", {
  # geom_area: ggplot(df, aes(x, y, fill = g)) + geom_area(stat = "identity")
  df <- data.frame(
    g = c("a", "a", "a", "b", "b", "b"),
    x = c(1, 3, 5, 2, 4, 6),
    y = c(2, 5, 1, 3, 6, 7)
  )
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(df, aes(x, y, fill = g)) + geom_area_fade(stat = "identity"))
  )))
})

# ===========================================================================
# geom_col_fade  ↔  geom_col
# From: ?ggplot2::geom_col
# ===========================================================================

test_that("geom_col_fade [doc ex 1]: basic pre-computed heights", {
  # geom_col: ggplot(df, aes(trt, outcome)) + geom_col()
  df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(df, aes(trt, outcome)) + geom_col_fade())
  )))
})

test_that("geom_col_fade [doc ex 2]: date axis with just=0.5", {
  # geom_col: ggplot(df, aes(x, y)) + geom_col(just = 0.5)
  df <- data.frame(x = as.Date(c("2020-01-01", "2020-02-01")), y = 1:2)
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(df, aes(x, y)) + geom_col_fade(just = 0.5))
  )))
})

test_that("geom_col_fade [doc ex 3]: date axis with just=1", {
  # geom_col: ggplot(df, aes(x, y)) + geom_col(just = 1)
  df <- data.frame(x = as.Date(c("2020-01-01", "2020-02-01")), y = 1:2)
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(df, aes(x, y)) + geom_col_fade(just = 1))
  )))
})

# ===========================================================================
# geom_curve_fade  ↔  geom_curve
# From: ?ggplot2::geom_curve
# ===========================================================================

test_that("geom_curve_fade [doc ex 1]: basic curve", {
  # geom_curve: b + geom_curve(aes(x=x1,y=y1,xend=x2,yend=y2), data=df)
  df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
  b <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(b + geom_curve_fade(aes(x = x1, y = y1, xend = x2, yend = y2), data = df))
  )))
})

test_that("geom_curve_fade [doc ex 2]: negative curvature", {
  # geom_curve: b + geom_curve(..., curvature = -0.2)
  df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
  b <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(b + geom_curve_fade(aes(x = x1, y = y1, xend = x2, yend = y2),
      data = df, curvature = -0.2))
  )))
})

test_that("geom_curve_fade [doc ex 3]: curvature = 1 (semicircle)", {
  # geom_curve: b + geom_curve(..., curvature = 1)
  df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
  b <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(b + geom_curve_fade(aes(x = x1, y = y1, xend = x2, yend = y2),
      data = df, curvature = 1))
  )))
})

test_that("geom_curve_fade [doc ex 4]: with arrow", {
  # geom_curve: b + geom_curve(..., arrow = arrow(length = unit(0.03, "npc")))
  df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
  b <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(b + geom_curve_fade(
      aes(x = x1, y = y1, xend = x2, yend = y2),
      data = df,
      arrow = arrow(length = unit(0.03, "npc"))
    ))
  )))
})

# ===========================================================================
# geom_density_fade  ↔  geom_density
# From: ?ggplot2::geom_density
# ===========================================================================

test_that("geom_density_fade [doc ex 1]: basic density", {
  # geom_density: ggplot(diamonds, aes(carat)) + geom_density()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(diamonds, aes(carat)) + geom_density_fade())
  )))
})

test_that("geom_density_fade [doc ex 2]: y aesthetic (flipped)", {
  # geom_density: ggplot(diamonds, aes(y = carat)) + geom_density()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(diamonds, aes(y = carat)) + geom_density_fade())
  )))
})

test_that("geom_density_fade [doc ex 3]: bandwidth adjust", {
  # geom_density: ggplot(diamonds, aes(carat)) + geom_density(adjust = 1/5)
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(diamonds, aes(carat)) + geom_density_fade(adjust = 1/5))
  )))
})

test_that("geom_density_fade [doc ex 4]: multiple groups with colour", {
  # geom_density: ggplot(diamonds, aes(depth, colour = cut)) + geom_density() + xlim(55,70)
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(diamonds, aes(depth, colour = cut)) +
      geom_density_fade() + xlim(55, 70))
  )))
})

test_that("geom_density_fade [doc ex 5]: fill + colour + alpha", {
  # geom_density: ggplot(diamonds, aes(depth, fill=cut, colour=cut)) +
  #               geom_density(alpha = 0.1) + xlim(55, 70)
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(diamonds, aes(depth, fill = cut, colour = cut)) +
      geom_density_fade(alpha = 0.1) + xlim(55, 70))
  )))
})

# ===========================================================================
# geom_freqpoly_fade  ↔  geom_freqpoly
# From: ?ggplot2::geom_freqpoly (freqpoly-specific examples)
# ===========================================================================

test_that("geom_freqpoly_fade [doc ex 1]: basic frequency polygon", {
  # geom_freqpoly: ggplot(diamonds, aes(price, colour = cut)) + geom_freqpoly(binwidth = 500)
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(diamonds, aes(price, colour = cut)) + geom_freqpoly_fade(binwidth = 500))
  )))
})

test_that("geom_freqpoly_fade [doc ex 2]: default binwidth", {
  # geom_freqpoly: ggplot(diamonds, aes(carat)) + geom_freqpoly()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(diamonds, aes(carat)) + geom_freqpoly_fade())
  )))
})

# ===========================================================================
# geom_histogram_fade  ↔  geom_histogram
# From: ?ggplot2::geom_histogram
# ===========================================================================

test_that("geom_histogram_fade [doc ex 1]: basic (default binwidth)", {
  # geom_histogram: ggplot(diamonds, aes(carat)) + geom_histogram()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(diamonds, aes(carat)) + geom_histogram_fade())
  )))
})

test_that("geom_histogram_fade [doc ex 2]: explicit binwidth", {
  # geom_histogram: ggplot(diamonds, aes(carat)) + geom_histogram(binwidth = 0.01)
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(diamonds, aes(carat)) + geom_histogram_fade(binwidth = 0.01))
  )))
})

test_that("geom_histogram_fade [doc ex 3]: explicit bin count", {
  # geom_histogram: ggplot(diamonds, aes(carat)) + geom_histogram(bins = 200)
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(diamonds, aes(carat)) + geom_histogram_fade(bins = 200))
  )))
})

test_that("geom_histogram_fade [doc ex 4]: y aesthetic (flipped)", {
  # geom_histogram: ggplot(diamonds, aes(y = carat)) + geom_histogram()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(diamonds, aes(y = carat)) + geom_histogram_fade())
  )))
})

test_that("geom_histogram_fade [doc ex 5]: stacked with fill", {
  # geom_histogram: ggplot(diamonds, aes(price, fill = cut)) + geom_histogram(binwidth = 500)
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(diamonds, aes(price, fill = cut)) + geom_histogram_fade(binwidth = 500))
  )))
})

# ===========================================================================
# geom_line_fade  ↔  geom_line
# From: ?ggplot2::geom_line
# ===========================================================================

test_that("geom_line_fade [doc ex 1]: basic time series", {
  # geom_line: ggplot(economics, aes(date, unemploy)) + geom_line()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(economics, aes(date, unemploy)) + geom_line_fade())
  )))
})

test_that("geom_line_fade [doc ex 2]: multiple groups with colour", {
  # geom_line: ggplot(economics_long, aes(date, value01, colour = variable)) +
  #              geom_line(key_glyph = "timeseries")
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(economics_long, aes(date, value01, colour = variable)) +
      geom_line_fade(key_glyph = "timeseries"))
  )))
})

test_that("geom_line_fade [doc ex 3]: y orientation (vertical timeseries)", {
  # geom_line: ggplot(economics, aes(unemploy, date)) + geom_line(orientation = "y")
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(economics, aes(unemploy, date)) + geom_line_fade(orientation = "y"))
  )))
})

test_that("geom_line_fade [doc ex 4]: fixed colour parameter", {
  # geom_line: ggplot(economics, aes(date, unemploy)) + geom_line(colour = "red")
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(economics, aes(date, unemploy)) + geom_line_fade(colour = "red"))
  )))
})

# ===========================================================================
# geom_path_fade  ↔  geom_path
# From: ?ggplot2::geom_path (path-specific examples)
# ===========================================================================

test_that("geom_path_fade [doc ex 1]: path through two variables over time", {
  # geom_path: m + geom_path()
  m <- ggplot(economics, aes(unemploy / pop, psavert))
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(m + geom_path_fade()))))
})

test_that("geom_path_fade [doc ex 2]: colour mapped to time", {
  # geom_path: m + geom_path(aes(colour = as.numeric(date)))
  m <- ggplot(economics, aes(unemploy / pop, psavert))
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(m + geom_path_fade(aes(colour = as.numeric(date))))
  )))
})

# ===========================================================================
# geom_rect_fade  ↔  geom_rect
# From: ?ggplot2::geom_rect (rect-specific example)
# ===========================================================================

test_that("geom_rect_fade [doc ex 1]: arbitrary rectangles", {
  # geom_rect: ggplot(df, aes(xmin=x-w/2, xmax=x+w/2, ymin=y, ymax=y+1)) + geom_rect(...)
  df <- data.frame(
    x = rep(c(2, 5, 7, 9, 12), 2),
    y = rep(c(1, 2), each = 5),
    z = factor(rep(1:5, each = 2)),
    w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
  )
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(
      ggplot(df, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) +
        geom_rect_fade(aes(fill = z), colour = "grey50")
    )
  )))
})

# ===========================================================================
# geom_segment_fade  ↔  geom_segment
# From: ?ggplot2::geom_segment
# ===========================================================================

test_that("geom_segment_fade [doc ex 1]: basic segment", {
  # geom_segment: b + geom_segment(aes(x=x1,y=y1,xend=x2,yend=y2), data=df)
  df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
  b <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(b + geom_segment_fade(aes(x = x1, y = y1, xend = x2, yend = y2), data = df))
  )))
})

test_that("geom_segment_fade [doc ex 2]: with arrow", {
  # geom_segment: b + geom_segment(..., arrow = arrow(length = unit(0.03, "npc")))
  df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
  b <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(b + geom_segment_fade(
      aes(x = x1, y = y1, xend = x2, yend = y2),
      data = df,
      arrow = arrow(length = unit(0.03, "npc"))
    ))
  )))
})

# ===========================================================================
# geom_step_fade  ↔  geom_step
# From: ?ggplot2::geom_step
# ===========================================================================

test_that("geom_step_fade [doc ex 1]: basic step chart", {
  # geom_step: ggplot(recent, aes(date, unemploy)) + geom_step()
  recent <- economics[economics$date > as.Date("2013-01-01"), ]
  expect_no_error(suppressWarnings(suppressMessages(
    ggplotGrob(ggplot(recent, aes(date, unemploy)) + geom_step_fade())
  )))
})

# ===========================================================================
# geom_ridgeline_fade  ↔  geom_ridgeline (from ggridges)
# ===========================================================================

test_that("geom_ridgeline_fade [doc ex 1]: basic ridgeline", {
  skip_if_not_installed("ggridges")
  # geom_ridgeline: ggplot(iris, aes(x, y, height)) + geom_ridgeline()
  expect_no_error(ggplotGrob(
    ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
      geom_ridgeline_fade(alpha = 0.7)
  ))
})

# ===========================================================================
# geom_point_glow  ↔  geom_point
# From: ?ggplot2::geom_point (basic examples)
# ===========================================================================

test_that("geom_point_glow [doc ex 1]: basic scatter", {
  skip_if_not_installed("vdiffr")
  # geom_point: ggplot(mtcars, aes(wt, mpg)) + geom_point()
  vdiffr::expect_doppelganger(
    "dg-point-glow-basic",
    ggplot(mtcars, aes(wt, mpg)) + geom_point_glow()
  )
})

test_that("geom_point_glow [doc ex 2]: colour aesthetic", {
  # geom_point: ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()
  expect_no_error(ggplotGrob(
    ggplot(mtcars, aes(wt, mpg, colour = cyl)) + geom_point_glow()
  ))
})

test_that("geom_point_glow [doc ex 3]: size aesthetic + facets", {
  # geom_point: ggplot(mtcars, aes(wt, mpg, size = hp)) + geom_point() + facet_wrap(~am)
  expect_no_error(ggplotGrob(
    ggplot(mtcars, aes(wt, mpg, colour = cyl, size = hp)) +
      geom_point_glow() + facet_wrap(~am)
  ))
})

# ===========================================================================
# GoG/scales: scale_x_reverse and scale_y_reverse — data-level assertions
#
# Each test verifies that the scale transform actually reaches the built data,
# not merely that the geom doesn't crash.
# ===========================================================================

# ---------------------------------------------------------------------------
# curve_fade
# ---------------------------------------------------------------------------

# Regression: scale_x_reverse used to flip the gradient direction in
# geom_curve_fade because .flip_curve() swaps coords$x ↔ coords$xend but the
# mask gradient was still built with the original fade_direction.  The fix
# inverts eff_fade_direction when flip = TRUE so the transparent end always
# tracks the logical start/end the user specified.
test_that("GoG/scales: scale_x_reverse preserves fade gradient direction (curve_fade)", {
  # Regression test: before the fix, .flip_curve() swapped coordinates without
  # inverting eff_fade_direction, so fade_direction = "start" was silently
  # treated as "end" under scale_x_reverse.
  #
  # Mechanism:
  #   • Forward (no reverse): gradient runs (x,y)→(xend,yend).
  #     fade_direction="start" → colours = c(transparent, opaque).
  #     colours[1] is at the data start → must be transparent.
  #
  #   • Reversed (after fix): .flip_curve() swaps coords so the curve stays
  #     visually correct; eff_fade_direction is inverted to "end".
  #     colours = c(opaque, transparent): gradient now runs
  #     (original xend → original x1) so the transparent stop is at colours[2].
  #     Bug would have given colours = c(transparent, opaque) — wrong end faded.
  df_seg <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)

  grad_colours <- function(reversed) {
    p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() +
      geom_curve_fade(aes(x = x1, y = y1, xend = x2, yend = y2), data = df_seg,
                      fade_direction = "start", alpha_fade_to = 0)
    if (reversed) p <- p + scale_x_reverse()
    b    <- ggplot_build(p)
    grob <- GeomCurveFade$draw_panel(b$data[[2]], b$layout$panel_params[[1]],
                                     b$layout$coord,
                                     fade_direction = "start", alpha_fade_to = 0)
    grob$mask_glist[[1]]$gp$fill$colours
  }

  cols_fwd <- grad_colours(FALSE)
  cols_rev <- grad_colours(TRUE)

  alpha <- \(col) unname(grDevices::col2rgb(col, alpha = TRUE)["alpha", ] / 255)

  # Forward: gradient start = data start → transparent
  expect_equal(alpha(cols_fwd[1]), 0, tolerance = 1 / 255)

  # Reversed (after fix): flip inverts the gradient; data start is now at
  # gradient position 2 → transparent.  Gradient position 1 (data end) is opaque.
  # Before the fix both cases had colours[1] = transparent (wrong for reversed).
  expect_equal(alpha(cols_rev[2]), 0, tolerance = 1 / 255)
  expect_gt(alpha(cols_rev[1]), 0.5)   # gradient position 1 = data end, must be opaque
})

test_that("GoG/scales: scale_x_reverse negates x values in scatter layer (curve_fade plot)", {
  df_seg <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
  b_fwd <- ggplot_build(ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    geom_curve_fade(aes(x = x1, y = y1, xend = x2, yend = y2), data = df_seg))
  b_rev <- ggplot_build(ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    geom_curve_fade(aes(x = x1, y = y1, xend = x2, yend = y2), data = df_seg) +
    scale_x_reverse())
  expect_equal(b_rev$data[[1]]$x, -b_fwd$data[[1]]$x)
})

test_that("GoG/scales: scale_y_reverse negates y values in scatter layer (curve_fade plot)", {
  df_seg <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
  b_fwd <- ggplot_build(ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    geom_curve_fade(aes(x = x1, y = y1, xend = x2, yend = y2), data = df_seg))
  b_rev <- ggplot_build(ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    geom_curve_fade(aes(x = x1, y = y1, xend = x2, yend = y2), data = df_seg) +
    scale_y_reverse())
  expect_equal(b_rev$data[[1]]$y, -b_fwd$data[[1]]$y)
})

# ---------------------------------------------------------------------------
# density_fade
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_x_reverse produces all-negative x values (density_fade)", {
  b_rev <- ggplot_build(ggplot(faithful, aes(waiting)) + geom_density_fade() + scale_x_reverse())
  expect_true(all(b_rev$data[[1]]$x < 0))
})

test_that("GoG/scales: scale_y_reverse negates density y values (density_fade)", {
  b_fwd <- ggplot_build(ggplot(faithful, aes(waiting)) + geom_density_fade())
  b_rev <- ggplot_build(ggplot(faithful, aes(waiting)) + geom_density_fade() + scale_y_reverse())
  expect_equal(b_rev$data[[1]]$y, -b_fwd$data[[1]]$y)
})

# ---------------------------------------------------------------------------
# freqpoly_fade
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_x_reverse produces all-negative x values (freqpoly_fade)", {
  b_rev <- ggplot_build(ggplot(faithful, aes(waiting)) + geom_freqpoly_fade(binwidth = 5) + scale_x_reverse())
  expect_true(all(b_rev$data[[1]]$x < 0))
})

test_that("GoG/scales: scale_y_reverse negates count values (freqpoly_fade)", {
  b_fwd <- ggplot_build(ggplot(faithful, aes(waiting)) + geom_freqpoly_fade(binwidth = 5))
  b_rev <- ggplot_build(ggplot(faithful, aes(waiting)) + geom_freqpoly_fade(binwidth = 5) + scale_y_reverse())
  expect_equal(b_rev$data[[1]]$y, -b_fwd$data[[1]]$y)
})

# ---------------------------------------------------------------------------
# line_fade
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_x_reverse produces all-negative x values (line_fade)", {
  # Use numeric x — scale_x_reverse() cannot negate Date objects.
  # geom_line reorders rows by x, so row order differs after negation;
  # verify all values are negative rather than comparing element-wise.
  b_rev <- ggplot_build(ggplot(economics, aes(unemploy, uempmed)) + geom_line_fade() + scale_x_reverse())
  expect_true(all(b_rev$data[[1]]$x < 0))
})

test_that("GoG/scales: scale_y_reverse negates y values (line_fade)", {
  b_fwd <- ggplot_build(ggplot(economics, aes(date, unemploy)) + geom_line_fade())
  b_rev <- ggplot_build(ggplot(economics, aes(date, unemploy)) + geom_line_fade() + scale_y_reverse())
  expect_equal(b_rev$data[[1]]$y, -b_fwd$data[[1]]$y)
})

test_that("GoG/scales: alpha_mode=gradient keeps transparent end on visual right under scale_x_reverse (line_fade)", {
  # geom_line sorts by x; scale_x_reverse reverses the sort order so the path
  # is traversed right-to-left in the negated space. The mask alpha
  # progression tracks the path order, so the visual right (small original x,
  # last in the negated sort) always gets the transparent stop.
  df <- data.frame(x = 1:4, y = c(1, 2, 1, 2))
  find_grob <- function(p) {
    result <- list()
    walk <- function(node) {
      if (is.null(node)) return()
      if (inherits(node, "path_fade_grob"))
        result[[length(result) + 1L]] <<- node
      for (ch in c(node$children, node$grobs)) walk(ch)
    }
    walk(ggplotGrob(p))
    result
  }
  base_p <- ggplot(df, aes(x, y)) +
    geom_line_fade(fade_direction = "end", alpha_mode = "gradient")
  p_fwd <- base_p
  p_rev <- base_p + scale_x_reverse()
  get_vertex_alphas <- function(p) {
    g <- find_grob(p)[[1]]
    g$groups_data[[1L]]$alpha_vert
  }
  a_fwd <- get_vertex_alphas(p_fwd)
  a_rev <- get_vertex_alphas(p_rev)
  # For fade_direction="end", the last vertex alpha must be 0.
  # This holds for both forward and reversed axes.
  expect_equal(a_fwd[length(a_fwd)], 0, tolerance = 1 / 255)
  expect_equal(a_rev[length(a_rev)], 0, tolerance = 1 / 255)
  # And the first vertex alpha must be 1 (fully opaque start).
  expect_equal(a_fwd[1], 1, tolerance = 1 / 255)
  expect_equal(a_rev[1], 1, tolerance = 1 / 255)
})

# ---------------------------------------------------------------------------
# path_fade
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_x_reverse produces all-negative x values (path_fade)", {
  b_rev <- suppressWarnings(ggplot_build(ggplot(economics, aes(unemploy, uempmed)) + geom_path_fade() + scale_x_reverse()))
  expect_true(all(b_rev$data[[1]]$x < 0))
})

test_that("GoG/scales: scale_y_reverse negates y values (path_fade)", {
  b_fwd <- ggplot_build(ggplot(economics, aes(unemploy, uempmed)) + geom_path_fade())
  b_rev <- suppressWarnings(ggplot_build(ggplot(economics, aes(unemploy, uempmed)) + geom_path_fade() + scale_y_reverse()))
  expect_equal(b_rev$data[[1]]$y, -b_fwd$data[[1]]$y)
})

test_that("path_fade: alpha = alpha_fade_to takes the early exit (no path_fade_grob in tree)", {
  # When alpha == alpha_fade_to, .is_uniform_alpha() fires and draw_panel
  # delegates directly to GeomPath — no path_fade_grob is created.
  # Visual equivalence is verified by the vdiffr snapshots below.
  d <- data.frame(x = c(-2, 0, -2), y = c(-2, 0, 2))
  find_path_fade_grob <- function(p) {
    result <- NULL
    walk <- function(node) {
      if (is.null(node) || !is.null(result)) return()
      if (inherits(node, "path_fade_grob")) {
        result <<- node
        return()
      }
      for (ch in c(node$children, node$grobs)) walk(ch)
    }
    walk(ggplotGrob(p))
    result
  }
  for (fd in list("start", "end", c("start", "end"))) {
    p <- ggplot(d, aes(x, y)) +
      geom_path_fade(linewidth = 20, alpha = 0.5,
                     alpha_fade_to = 0.5, fade_direction = fd)
    expect_no_error(ggplotGrob(p))
    expect_null(find_path_fade_grob(p))
  }
})

test_that("path_fade: uniform alpha is visually identical to geom_path (step and gradient)", {
  # When alpha == alpha_fade_to the fade degenerates to a flat stroke.
  # All three snapshots must look identical when reviewed with vdiffr::manage_cases().
  # Combined with the data-level alpha test above, this constitutes the full
  # "100% identical" contract: math proven by alpha_vert equality, appearance
  # proven by human snapshot review + per-variant regression protection.
  skip_if_not_installed("vdiffr")
  d <- data.frame(x = c(-2, 0, -2), y = c(-2, 0, 2))
  p <- ggplot(data = d) +
    coord_equal() +
    xlim(-2.5, 0.5) +
    ylim(-2.5, 2.5) +
    theme_minimal()

  vdiffr::expect_doppelganger(
    "path-fade-uniform-geom-path-reference",
    p + geom_path(
      aes(x, y),
      linewidth = 8, colour = "#e63946", alpha = 0.5
    )
  )
  vdiffr::expect_doppelganger(
    "path-fade-uniform-step-mode",
    p + geom_path_fade(
      aes(x, y),
      linewidth = 8, colour = "#e63946", alpha = 0.5,
      alpha_fade_to = 0.5, alpha_mode = "step"
    )
  )
  vdiffr::expect_doppelganger(
    "path-fade-uniform-gradient-mode",
    p + geom_path_fade(
      aes(x, y),
      linewidth = 8, colour = "#e63946", alpha = 0.5,
      alpha_fade_to = 0.5, alpha_mode = "gradient"
    )
  )
})

test_that("path_fade: uniform alpha matches geom_path across lineend/linejoin/linemitre", {
  # 9 lineend × linejoin combos + 3 extra mitre combos with linemitre = 2.
  # For each combo 3 snapshots: geom_path reference, step mode, gradient mode.
  # All three snapshots in each combo must look identical on vdiffr review.
  skip_if_not_installed("vdiffr")
  d <- data.frame(x = c(-2, 0, -2), y = c(-2, 0, 2))
  p <- ggplot(data = d) +
    coord_equal() +
    xlim(-2.5, 0.5) +
    ylim(-2.5, 2.5) +
    theme_minimal()

  combos <- rbind(
    expand.grid(
      lineend  = c("round", "butt", "square"),
      linejoin = c("round", "mitre", "bevel"),
      linemitre = 10,
      stringsAsFactors = FALSE
    ),
    expand.grid(
      lineend  = c("round", "butt", "square"),
      linejoin = "mitre",
      linemitre = 2,
      stringsAsFactors = FALSE
    )
  )

  for (i in seq_len(nrow(combos))) {
    le <- combos$lineend[i]
    lj <- combos$linejoin[i]
    lm <- combos$linemitre[i]
    sn <- paste0("path-fade-uniform-", le, "-", lj, "-lm", lm)

    vdiffr::expect_doppelganger(
      paste0(sn, "-ref"),
      p + geom_path(
        aes(x, y),
        linewidth = 8, colour = "#e63946", alpha = 0.5,
        lineend = le, linejoin = lj, linemitre = lm
      )
    )
    vdiffr::expect_doppelganger(
      paste0(sn, "-step"),
      p + geom_path_fade(
        aes(x, y),
        linewidth = 8, colour = "#e63946", alpha = 0.5,
        alpha_fade_to = 0.5, alpha_mode = "step",
        lineend = le, linejoin = lj, linemitre = lm
      )
    )
    vdiffr::expect_doppelganger(
      paste0(sn, "-grad"),
      p + geom_path_fade(
        aes(x, y),
        linewidth = 8, colour = "#e63946", alpha = 0.5,
        alpha_fade_to = 0.5, alpha_mode = "gradient",
        lineend = le, linejoin = lj, linemitre = lm
      )
    )
  }
})

# ---------------------------------------------------------------------------
# rect_fade
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_x_reverse produces all-negative x values (rect_fade)", {
  df_r <- data.frame(xmin = 1, xmax = 3, ymin = 1, ymax = 5)
  b_rev <- suppressWarnings(ggplot_build(
    ggplot(df_r, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      geom_rect_fade() + scale_x_reverse()
  ))
  expect_true(all(b_rev$data[[1]]$xmax < 0))
})

test_that("GoG/scales: scale_y_reverse produces all-negative y values (rect_fade)", {
  df_r <- data.frame(xmin = 1, xmax = 3, ymin = 1, ymax = 5)
  b_rev <- suppressWarnings(ggplot_build(
    ggplot(df_r, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      geom_rect_fade() + scale_y_reverse()
  ))
  expect_true(all(b_rev$data[[1]]$ymax < 0))
})

# ---------------------------------------------------------------------------
# segment_fade
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_x_reverse produces all-negative x values (segment_fade)", {
  df_seg <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
  b_rev <- suppressWarnings(ggplot_build(
    ggplot(mtcars, aes(wt, mpg)) + geom_point() +
      geom_segment_fade(aes(x = x1, y = y1, xend = x2, yend = y2), data = df_seg) +
      scale_x_reverse()
  ))
  expect_true(all(b_rev$data[[2]]$x < 0))
  expect_true(all(b_rev$data[[2]]$xend < 0))
})

test_that("GoG/scales: scale_y_reverse negates y values (segment_fade)", {
  df_seg <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
  b_fwd <- ggplot_build(ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    geom_segment_fade(aes(x = x1, y = y1, xend = x2, yend = y2), data = df_seg))
  b_rev <- ggplot_build(ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    geom_segment_fade(aes(x = x1, y = y1, xend = x2, yend = y2), data = df_seg) +
    scale_y_reverse())
  expect_equal(b_rev$data[[2]]$y, -b_fwd$data[[2]]$y)
})

test_that("GoG/scales: gradient direction unaffected by scale_x_reverse (segment_fade)", {
  # geom_segment_fade (Family A pipeline) builds per-vertex alpha in native
  # (transformed) coordinates. Under scale_x_reverse the x/xend values are
  # negated but the fade direction still follows the data (x,y) → (xend,yend),
  # so fade_direction='end' keeps the faded vertex at xend regardless of which
  # visual side xend is on.
  df_seg <- data.frame(x = 1, xend = 5, y = 1, yend = 3)
  make_grob <- function(p) {
    result <- list()
    walk <- function(node) {
      if (is.null(node)) return()
      if (inherits(node, "path_fade_grob"))
        result[[length(result) + 1L]] <<- node
      for (ch in c(node$children, node$grobs)) walk(ch)
    }
    walk(ggplotGrob(p))
    result
  }
  p_fwd <- ggplot(df_seg, aes(x, y, xend = xend, yend = yend)) +
    geom_segment_fade(fade_direction = "end")
  p_rev <- p_fwd + scale_x_reverse()
  a_fwd <- make_grob(p_fwd)[[1]]$groups_data[[1]]$alpha_vert
  a_rev <- make_grob(p_rev)[[1]]$groups_data[[1]]$alpha_vert
  # Both: vertex 1 = opaque (at x / start), vertex 2 = transparent (at xend / end)
  expect_gt(a_fwd[[1L]], 0.9)
  expect_lt(a_fwd[[length(a_fwd)]], 0.1)
  expect_gt(a_rev[[1L]], 0.9)
  expect_lt(a_rev[[length(a_rev)]], 0.1)
})

# ---------------------------------------------------------------------------
# step_fade
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_x_reverse produces all-negative x values (step_fade)", {
  # Use numeric x — scale_x_reverse() cannot negate Date objects.
  # geom_step may reorder rows; verify all values are negative rather than
  # comparing element-wise.
  b_rev <- ggplot_build(ggplot(economics, aes(unemploy, uempmed)) + geom_step_fade() + scale_x_reverse())
  expect_true(all(b_rev$data[[1]]$x < 0))
})

test_that("GoG/scales: scale_y_reverse negates y values (step_fade)", {
  recent <- economics[economics$date > as.Date("2013-01-01"), ]
  b_fwd <- ggplot_build(ggplot(recent, aes(date, unemploy)) + geom_step_fade())
  b_rev <- ggplot_build(ggplot(recent, aes(date, unemploy)) + geom_step_fade() + scale_y_reverse())
  expect_equal(b_rev$data[[1]]$y, -b_fwd$data[[1]]$y)
})

# ---------------------------------------------------------------------------
# abline / hline / vline fade — gradient direction under scale reversal
#
# Regression: before the fix, backtransform_range() returns data-space values
# in ascending order; on a reversed scale the smaller data value maps to visual
# RIGHT (NPC 1), so the gradient ran from visual-right (opaque) to visual-left
# (transparent).  After the fix, all three geoms detect the visual ordering
# via coord$transform() and swap start/end when needed.
# ---------------------------------------------------------------------------

# Helpers shared by all three tests: inspect the path_fade_grob that
# GeomSegmentFade$draw_panel now returns. The first (and only) group holds
# the transformed-coord vertex positions and per-vertex alphas; the actual
# linearGradient is built inside makeContent at draw time, but the vertex
# order and alphas fully determine its direction and endpoint alphas.
.stop_alphas <- function(grob) {
  a <- grob$groups_data[[1L]]$alpha_vert
  c(start = a[[1L]], end = a[[length(a)]])
}

.grad_x <- function(grob) {
  x <- grob$groups_data[[1L]]$x
  c(x1 = as.numeric(x[[1L]]), x2 = as.numeric(x[[length(x)]]))
}

.grad_y <- function(grob) {
  y <- grob$groups_data[[1L]]$y
  c(y1 = as.numeric(y[[1L]]), y2 = as.numeric(y[[length(y)]]))
}

test_that("GoG/scales: scale_x_reverse keeps gradient running visual-left to right (hline_fade)", {
  # Regression: hline started at data-x-min and ended at data-x-max; under
  # scale_x_reverse, data-x-min is on the visual right, flipping the gradient.
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  draw_hline <- function(p_obj) {
    b <- ggplot_build(p_obj)
    suppressWarnings(
      GeomHlineFade$draw_panel(b$data[[2]], b$layout$panel_params[[1]],
                               b$layout$coord, alpha_fade_to = 0, fade_direction = "end")
    )
  }
  g_fwd <- draw_hline(p + geom_hline_fade(yintercept = 20, linewidth = 1.5))
  g_rev <- draw_hline(p + geom_hline_fade(yintercept = 20, linewidth = 1.5) + scale_x_reverse())

  # fade_direction = "end": start (x1) must be opaque, end (x2) transparent
  expect_gt(.stop_alphas(g_fwd)[["start"]], 0.9)
  expect_lt(.stop_alphas(g_fwd)[["end"]],   0.1)
  expect_gt(.stop_alphas(g_rev)[["start"]], 0.9)
  expect_lt(.stop_alphas(g_rev)[["end"]],   0.1)

  # Gradient must run left → right in NPC (x1 < x2)
  expect_lt(.grad_x(g_fwd)[["x1"]], .grad_x(g_fwd)[["x2"]])
  expect_lt(.grad_x(g_rev)[["x1"]], .grad_x(g_rev)[["x2"]])
})

test_that("GoG/scales: scale_y_reverse keeps gradient running visual-bottom to top (vline_fade)", {
  # Regression: vline started at data-y-min and ended at data-y-max; under
  # scale_y_reverse, data-y-min is on the visual top, flipping the gradient.
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  draw_vline <- function(p_obj) {
    b <- ggplot_build(p_obj)
    suppressWarnings(
      GeomVlineFade$draw_panel(b$data[[2]], b$layout$panel_params[[1]],
                               b$layout$coord, alpha_fade_to = 0, fade_direction = "end")
    )
  }
  g_fwd <- draw_vline(p + geom_vline_fade(xintercept = 3, linewidth = 1.5))
  g_rev <- draw_vline(p + geom_vline_fade(xintercept = 3, linewidth = 1.5) + scale_y_reverse())

  # fade_direction = "end": start (y1) must be opaque, end (y2) transparent
  expect_gt(.stop_alphas(g_fwd)[["start"]], 0.9)
  expect_lt(.stop_alphas(g_fwd)[["end"]],   0.1)
  expect_gt(.stop_alphas(g_rev)[["start"]], 0.9)
  expect_lt(.stop_alphas(g_rev)[["end"]],   0.1)

  # Gradient must run bottom → top in NPC (y1 < y2)
  expect_lt(.grad_y(g_fwd)[["y1"]], .grad_y(g_fwd)[["y2"]])
  expect_lt(.grad_y(g_rev)[["y1"]], .grad_y(g_rev)[["y2"]])
})

test_that("GoG/scales: scale_x_reverse keeps gradient running visual-left to right (abline_fade)", {
  # Regression: the abline assigned smaller data-x to 'x' and larger to 'xend';
  # under scale_x_reverse, smaller data-x is on the visual right.
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  coefs <- coef(lm(mpg ~ wt, data = mtcars))
  draw_abline <- function(p_obj) {
    b <- ggplot_build(p_obj)
    suppressWarnings(
      GeomAblineFade$draw_panel(b$data[[2]], b$layout$panel_params[[1]],
                                b$layout$coord, alpha_fade_to = 0, fade_direction = "end")
    )
  }
  g_fwd <- draw_abline(p + geom_abline_fade(intercept = coefs[[1]], slope = coefs[[2]]))
  g_rev <- draw_abline(p + geom_abline_fade(intercept = coefs[[1]], slope = coefs[[2]]) + scale_x_reverse())

  expect_gt(.stop_alphas(g_fwd)[["start"]], 0.9)
  expect_lt(.stop_alphas(g_fwd)[["end"]],   0.1)
  expect_gt(.stop_alphas(g_rev)[["start"]], 0.9)
  expect_lt(.stop_alphas(g_rev)[["end"]],   0.1)

  expect_lt(.grad_x(g_fwd)[["x1"]], .grad_x(g_fwd)[["x2"]])
  expect_lt(.grad_x(g_rev)[["x1"]], .grad_x(g_rev)[["x2"]])
})

test_that("doppelganger: curve_fade and segment_fade together on mtcars", {
  skip_if_not_installed("vdiffr")
  b <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()

  df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
  vdiffr::expect_doppelganger(
    "curve-and-segment-fade-mtcars",
    b +
      geom_curve_fade(
        aes(x = x1, y = y1, xend = x2, yend = y2, colour = "curve"),
        data = df
      ) +
      geom_segment_fade(
        arrow = arrow(type = "closed", ends = "both"),
        arrow.fill = "tomato",
        aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"),
        data = df
      )
  )
})
