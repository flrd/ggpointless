library(ggplot2)

# ===========================================================================
# Grammar of Graphics adversarial stress tests for geom_step_fade
# ===========================================================================
# Theme stress is omitted because the geom does not read the theme.

df_step <- data.frame(
  x = c(1, 2, 4, 7, 10),
  y = c(2, 5, 3, 8, 6)
)

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
    geom_step_fade()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/data: single point does not error", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_step_fade()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/data: two points (single-step path) renders", {
  p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) +
    geom_step_fade()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/data: NA in y dropped silently with na.rm = TRUE", {
  df_na <- df_step
  df_na$y[3L] <- NA_real_
  p <- ggplot(df_na, aes(x, y)) + geom_step_fade(na.rm = TRUE)
  expect_no_warning(suppressMessages(ggplotGrob(p)))
})

# --------------------------------------------------------------------------
# Layer
# --------------------------------------------------------------------------

test_that("GoG/layer: two step layers compose", {
  p <- ggplot(df_step, aes(x, y)) +
    geom_step_fade(direction = "hv") +
    geom_step_fade(direction = "vh", colour = "red")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# Scale — log, reverse, sqrt, limits, expand
# --------------------------------------------------------------------------

test_that("GoG/scale: scale_*_reverse render", {
  p <- ggplot(df_step, aes(x, y)) + geom_step_fade() +
    scale_x_reverse() + scale_y_reverse()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/scale: scale_y_log10 with strictly positive y", {
  p <- ggplot(df_step, aes(x, y)) + geom_step_fade() + scale_y_log10()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/scale: scale_y_sqrt", {
  p <- ggplot(df_step, aes(x, y)) + geom_step_fade() + scale_y_sqrt()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/scale: explicit limits do not error", {
  p <- ggplot(df_step, aes(x, y)) + geom_step_fade() +
    scale_x_continuous(limits = c(0, 12)) +
    scale_y_continuous(limits = c(0, 10))
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/scale: expand = c(0, 0)", {
  p <- ggplot(df_step, aes(x, y)) + geom_step_fade() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# Coord — cartesian zoom, fixed, flip, transform, polar/radial
# --------------------------------------------------------------------------

test_that("GoG/coord: coord_cartesian zoom", {
  p <- ggplot(df_step, aes(x, y)) + geom_step_fade() +
    coord_cartesian(xlim = c(2, 8), ylim = c(2, 7))
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_fixed renders", {
  p <- ggplot(df_step, aes(x, y)) + geom_step_fade() + coord_fixed()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_flip renders", {
  p <- ggplot(df_step, aes(x, y)) + geom_step_fade() + coord_flip()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_transform(y = 'log10') with positive data", {
  p <- ggplot(df_step, aes(x, y)) + geom_step_fade() +
    coord_transform(y = "log10")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_polar renders", {
  p <- ggplot(df_step, aes(x, y)) + geom_step_fade() + coord_polar()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_radial renders", {
  p <- ggplot(df_step, aes(x, y)) + geom_step_fade() + coord_radial()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# Facet
# --------------------------------------------------------------------------

test_that("GoG/facet: facet_wrap free", {
  p <- ggplot(df_groups, aes(x, y)) + geom_step_fade() +
    facet_wrap(~ grp, scales = "free")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/facet: facet_grid free", {
  p <- ggplot(df_groups, aes(x, y)) + geom_step_fade() +
    facet_grid(~ grp, scales = "free")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# direction-specific (hv / vh / mid)
# --------------------------------------------------------------------------

test_that("direction: 'hv', 'vh', 'mid' all render", {
  for (d in c("hv", "vh", "mid")) {
    p <- ggplot(df_step, aes(x, y)) + geom_step_fade(direction = d)
    expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
  }
})

# --------------------------------------------------------------------------
# Drop-in parity vs geom_step
# --------------------------------------------------------------------------

test_that("layer-data parity: matches geom_step for each direction", {
  build <- function(p) suppressWarnings(suppressMessages(
    ggplot_build(p)$data[[1L]]
  ))
  for (d in c("hv", "vh", "mid")) {
    d_ref  <- build(ggplot(df_step, aes(x, y)) + geom_step(direction = d))
    d_ours <- build(ggplot(df_step, aes(x, y)) + geom_step_fade(direction = d))
    expect_equal(d_ours$x, d_ref$x, tolerance = 1e-9)
    expect_equal(d_ours$y, d_ref$y, tolerance = 1e-9)
  }
})
