library(ggplot2)

# ===========================================================================
# Grammar of Graphics adversarial stress tests for geom_path_fade
# ===========================================================================
# Theme stress is omitted because the geom does not read the theme.

df_path <- data.frame(
  x = c(1, 2, 4, 7, 10),
  y = c(2, 5, 3, 8, 6)
)

# Multi-group fixture so position adjustments / fill mappings have something
# to interact with.
df_groups <- data.frame(
  x   = rep(1:5, 2),
  y   = c(1, 3, 2, 5, 4,  2, 4, 3, 6, 5),
  grp = rep(c("a", "b"), each = 5)
)

# --------------------------------------------------------------------------
# Data
# --------------------------------------------------------------------------

test_that("GoG/data: empty dataset does not error", {
  p <- ggplot(data.frame(x = numeric(), y = numeric()), aes(x, y)) +
    geom_path_fade()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/data: single point does not error", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_path_fade()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/data: two points render (single-segment path)", {
  p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) +
    geom_path_fade()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/data: NA in x/y is dropped (na.rm = TRUE) without warning", {
  df_na <- df_path
  df_na$y[3L] <- NA_real_
  p <- ggplot(df_na, aes(x, y)) + geom_path_fade(na.rm = TRUE)
  expect_no_warning(suppressMessages(ggplotGrob(p)))
})

# --------------------------------------------------------------------------
# Mapping
# --------------------------------------------------------------------------

test_that("GoG/mapping: aes(group = ...) draws separate sub-paths", {
  p <- ggplot(df_groups, aes(x, y, group = grp)) + geom_path_fade()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/mapping: inherit.aes = FALSE isolates from plot mapping", {
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point() +
    geom_path_fade(data = df_path, mapping = aes(x, y), inherit.aes = FALSE)
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# Layer
# --------------------------------------------------------------------------

test_that("GoG/layer: two path_fade layers compose", {
  p <- ggplot(df_path, aes(x, y)) +
    geom_path_fade(fade_direction = "start") +
    geom_path_fade(fade_direction = "end", colour = "red")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/layer: standalone (no other geom) renders", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# Scales — log, reverse, sqrt, explicit limits, expand
# --------------------------------------------------------------------------

test_that("GoG/scale: scale_x_reverse / scale_y_reverse render", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade() +
    scale_x_reverse() + scale_y_reverse()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/scale: scale_y_log10 with strictly positive y", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade() + scale_y_log10()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/scale: scale_y_sqrt", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade() + scale_y_sqrt()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/scale: explicit limits do not error", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade() +
    scale_x_continuous(limits = c(0, 12)) +
    scale_y_continuous(limits = c(0, 10))
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/scale: expand = c(0, 0)", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# Coord — cartesian zoom, fixed, transform, flip, polar/radial
# --------------------------------------------------------------------------

test_that("GoG/coord: coord_cartesian zoom does not error", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade() +
    coord_cartesian(xlim = c(2, 8), ylim = c(2, 7))
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_fixed renders", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade() + coord_fixed()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_flip renders", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade() + coord_flip()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_transform(y = 'log10') with positive data", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade() +
    coord_transform(y = "log10")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_polar renders", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade() + coord_polar()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_radial renders", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade() + coord_radial()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# Facets — free scales in wrap and grid
# --------------------------------------------------------------------------

test_that("GoG/facet: facet_wrap free scales", {
  p <- ggplot(df_groups, aes(x, y)) + geom_path_fade() +
    facet_wrap(~ grp, scales = "free")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/facet: facet_grid free scales", {
  p <- ggplot(df_groups, aes(x, y)) + geom_path_fade() +
    facet_grid(~ grp, scales = "free")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# alpha_mode — "auto" / "step" / "gradient"
# --------------------------------------------------------------------------

test_that("alpha_mode: 'auto' (default) renders both small and large n", {
  for (n in c(5, 100)) {
    df <- data.frame(x = seq_len(n), y = sin(seq_len(n)))
    p <- ggplot(df, aes(x, y)) + geom_path_fade()
    expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
  }
})

test_that("alpha_mode: 'step' renders", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade(alpha_mode = "step")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("alpha_mode: 'gradient' renders", {
  p <- ggplot(df_path, aes(x, y)) + geom_path_fade(alpha_mode = "gradient")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# fade_direction
# --------------------------------------------------------------------------

test_that("fade_direction: 'start', 'end', and both render", {
  for (fd in list("start", "end", c("start", "end"))) {
    p <- ggplot(df_path, aes(x, y)) + geom_path_fade(fade_direction = fd)
    expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
  }
})

# --------------------------------------------------------------------------
# Drop-in parity at the layer-data level
# --------------------------------------------------------------------------

test_that("layer-data parity: x/y match geom_path", {
  build <- function(p) suppressWarnings(suppressMessages(
    ggplot_build(p)$data[[1L]]
  ))
  d_ref  <- build(ggplot(df_path, aes(x, y)) + geom_path())
  d_ours <- build(ggplot(df_path, aes(x, y)) + geom_path_fade())
  expect_equal(d_ours$x, d_ref$x, tolerance = 1e-9)
  expect_equal(d_ours$y, d_ref$y, tolerance = 1e-9)
})
