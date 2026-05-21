library(ggplot2)

# -----------------------------------------------------------------------
# Verify that all fade geoms resolve from_theme() defaults correctly.
#
# When no explicit fill/colour is mapped the geom inherits its colour
# from `default_aes` which uses `from_theme()`.  A custom
# `element_geom(ink, paper)` must therefore change the resolved value.
# -----------------------------------------------------------------------

custom_theme <- theme(
  geom = element_geom(ink = "navy", paper = "cornsilk")
)

# Shared tiny data sets
df_xy   <- data.frame(x = 1:3, y = 1:3)
df_seg  <- data.frame(x = 1:3, y = 1:3, xend = 2:4, yend = 2:4)
df_rect <- data.frame(xmin = 1:3, xmax = 2:4, ymin = 1:3, ymax = 2:4)
df_bar  <- data.frame(x = c("a", "b"), y = c(2, 3))

# Helper: build two versions of a plot (default theme vs custom theme)
# and return the resolved fill or colour column.
resolve_aes <- function(p, aes_name = "fill") {
  b_def <- ggplot_build(p)
  b_cus <- ggplot_build(p + custom_theme)
  list(
    default = unique(b_def$data[[1]][[aes_name]]),
    custom  = unique(b_cus$data[[1]][[aes_name]])
  )
}

# --- Fill-based geoms ----------------------------------------------------

test_that("geom_area_fade respects element_geom(ink, paper) for fill", {
  p <- ggplot(df_xy, aes(x, y)) + geom_area_fade()
  res <- resolve_aes(p, "fill")
  expect_false(identical(res$default, res$custom))
})

test_that("geom_col_fade respects element_geom(ink, paper) for fill", {
  p <- ggplot(df_bar, aes(x, y)) + geom_col_fade()
  res <- resolve_aes(p, "fill")
  expect_false(identical(res$default, res$custom))
})

test_that("geom_rect_fade respects element_geom(ink, paper) for fill", {
  p <- ggplot(df_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
    geom_rect_fade()
  res <- resolve_aes(p, "fill")
  expect_false(identical(res$default, res$custom))
})

# --- Colour-based geoms --------------------------------------------------

test_that("geom_path_fade respects element_geom(ink) for colour", {
  p <- ggplot(df_xy, aes(x, y)) + geom_path_fade()
  res <- resolve_aes(p, "colour")
  expect_false(identical(res$default, res$custom))
})

test_that("geom_line_fade respects element_geom(ink) for colour", {
  p <- ggplot(df_xy, aes(x, y)) + geom_line_fade()
  res <- resolve_aes(p, "colour")
  expect_false(identical(res$default, res$custom))
})

test_that("geom_step_fade respects element_geom(ink) for colour", {
  p <- ggplot(df_xy, aes(x, y)) + geom_step_fade()
  res <- resolve_aes(p, "colour")
  expect_false(identical(res$default, res$custom))
})

test_that("geom_segment_fade respects element_geom(ink) for colour", {
  p <- ggplot(df_seg, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_segment_fade()
  res <- resolve_aes(p, "colour")
  expect_false(identical(res$default, res$custom))
})

# --- Ridgeline (fill-based) -----------------------------------------------
# GeomRidgelineFade inherits GeomRibbon$default_aes, which uses
# from_theme(fill %||% col_mix(ink, paper, 0.2)).  An earlier version
# hard-coded fill = "grey70"; that was removed in favour of theme-aware
# resolution to match the rest of the fade family.

test_that("geom_ridgeline_fade respects element_geom(ink, paper) for fill", {
  df_ridge <- data.frame(
    x      = rep(seq(0, 4, length.out = 7), 2),
    y      = rep(c(1, 2), each = 7),
    height = c(0, 1, 3, 4, 3, 1, 0, 0, 2, 5, 6, 5, 2, 0),
    group  = rep(c("a", "b"), each = 7)
  )
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade()
  res <- resolve_aes(p, "fill")
  expect_false(identical(res$default, res$custom))
})
