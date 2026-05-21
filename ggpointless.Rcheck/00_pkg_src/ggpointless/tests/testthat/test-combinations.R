library(ggplot2)

# -----------------------------------------------------------------------
# Shared test data
# -----------------------------------------------------------------------

# Smooth sine-like curve for area/path geoms
df_wave <- data.frame(
  x = seq(0, 2 * pi, length.out = 30),
  y = sin(seq(0, 2 * pi, length.out = 30))
)

# Multimodal series for Chaikin smoothing
df_bumps <- data.frame(
  x = 1:8,
  y = c(1, 3, 2.5, 5, 4.8, 6, 5.2, 2)
)

# Ridgeline data
df_ridge <- data.frame(
  x      = rep(seq(0, 4, length.out = 7), 3),
  y      = rep(c(1, 2, 3), each = 7),
  height = c(0, 1, 3, 4, 3, 1, 0,
             0, 2, 5, 6, 5, 2, 0,
             0, 1, 2, 3, 2, 1, 0),
  group  = rep(c("a", "b", "c"), each = 7)
)

# Bar data
df_bar <- data.frame(
  x = c("a", "b", "c", "d"),
  y = c(3, 7, 2, 8)
)


# -----------------------------------------------------------------------
# stat_fourier + geom_area_fade
# -----------------------------------------------------------------------

test_that("geom_area_fade(stat = 'fourier') renders without error", {
  p <- ggplot(df_wave, aes(x, y)) +
    geom_area_fade(stat = "fourier", n_harmonics = 3)
  expect_no_error(ggplotGrob(p))
  expect_s3_class(ggplotGrob(p), "gtable")
})

test_that("geom_area_fade(stat = 'fourier') produces a different shape than raw data", {
  # Fourier smoothing changes the x-range (stat resamples), so layer data differs
  p_raw    <- ggplot(df_wave, aes(x, y)) + geom_area_fade()
  p_smooth <- ggplot(df_wave, aes(x, y)) + geom_area_fade(stat = "fourier", n_harmonics = 4)
  raw_data    <- ggplot_build(p_raw)$data[[1]]
  smooth_data <- ggplot_build(p_smooth)$data[[1]]
  # Fourier stat outputs a different number of rows
  expect_false(nrow(raw_data) == nrow(smooth_data))
})


# -----------------------------------------------------------------------
# stat_chaikin + geom_area_fade
# -----------------------------------------------------------------------

test_that("geom_area_fade(stat = 'chaikin') renders without error", {
  p <- ggplot(df_bumps, aes(x, y)) +
    geom_area_fade(stat = "chaikin", iterations = 3)
  expect_no_error(ggplotGrob(p))
})

test_that("geom_area_fade(stat = 'chaikin') produces more points than the input", {
  p <- ggplot(df_bumps, aes(x, y)) +
    geom_area_fade(stat = "chaikin", iterations = 3)
  d <- ggplot_build(p)$data[[1]]
  expect_gt(nrow(d), nrow(df_bumps))
})

test_that("geom_area_fade(stat = 'chaikin', iterations = 0) leaves point count unchanged", {
  p <- ggplot(df_bumps, aes(x, y)) +
    geom_area_fade(stat = "chaikin", iterations = 0)
  d <- ggplot_build(p)$data[[1]]
  expect_equal(nrow(d), nrow(df_bumps))
})


# -----------------------------------------------------------------------
# stat_chaikin + geom_ridgeline_fade
# -----------------------------------------------------------------------

test_that("geom_ridgeline_fade(stat = 'chaikin') renders without error", {
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group, fill = group)) +
    geom_ridgeline_fade(stat = "chaikin", iterations = 3, alpha_scope = "group") +
    guides(fill = "none")
  expect_no_error(ggplotGrob(p))
})

test_that("geom_ridgeline_fade(stat = 'chaikin') produces more x points than the input", {
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group, fill = group)) +
    geom_ridgeline_fade(stat = "chaikin", iterations = 2) +
    guides(fill = "none")
  d <- ggplot_build(p)$data[[1]]
  # 3 groups × 7 input points → 3 groups × more smoothed points
  expect_gt(nrow(d), 3 * 7)
})

test_that("geom_ridgeline_fade(stat = 'chaikin') interpolates height alongside x/y", {
  # height should be present in the built data after Chaikin smoothing
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(stat = "chaikin", iterations = 2) +
    guides(fill = "none")
  d <- ggplot_build(p)$data[[1]]
  expect_true("height" %in% names(d))
  # No NA heights introduced by smoothing
  expect_false(any(is.na(d$height)))
})


# -----------------------------------------------------------------------
# geom_area_fade + stat_pointless (highlight extremes)
# -----------------------------------------------------------------------

test_that("geom_area_fade + stat_pointless renders without error", {
  p <- ggplot(df_wave, aes(x, y)) +
    geom_area_fade() +
    stat_pointless(geom = "point_glow", location = c("minimum", "maximum"))
  expect_no_error(ggplotGrob(p))
})

test_that("stat_pointless returns exactly 2 rows for a single-group wave (min + max)", {
  p <- ggplot(df_wave, aes(x, y)) +
    stat_pointless(location = c("minimum", "maximum"))
  d <- ggplot_build(p)$data[[1]]
  expect_equal(nrow(d), 2L)
})


# -----------------------------------------------------------------------
# geom_point_glow + geom_area_fade(stat = "fourier")
# -----------------------------------------------------------------------

test_that("geom_point_glow + geom_area_fade(stat = 'fourier') renders without error", {
  p <- ggplot(df_wave, aes(x, y)) +
    geom_point_glow() +
    geom_area_fade(stat = "fourier", n_harmonics = 5, alpha = 0.4)
  expect_no_error(ggplotGrob(p))
})

test_that("geom_point_glow + geom_area_fade(stat = 'fourier') has two layers", {
  p <- ggplot(df_wave, aes(x, y)) +
    geom_point_glow() +
    geom_area_fade(stat = "fourier", n_harmonics = 5)
  expect_equal(length(p$layers), 2L)
})


# -----------------------------------------------------------------------
# geom_col_fade + stat_pointless (highlight max/min bar)
# -----------------------------------------------------------------------

test_that("geom_col_fade + stat_pointless renders without error", {
  p <- ggplot(df_bar, aes(x, y)) +
    geom_col_fade(alpha_scope = "global") +
    stat_pointless(geom = "point_glow", location = c("maximum", "minimum"))
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_col_fade + stat_pointless: pointless layer returns a row per bar tip", {
  p <- ggplot(df_bar, aes(x, y)) +
    geom_col_fade(alpha_scope = "global") +
    stat_pointless(location = "maximum")
  # stat_pointless warns that each bar is a single observation (expected for bar charts)
  d <- suppressWarnings(ggplot_build(p))$data[[2]]
  # stat_pointless marks the top of each bar (one maximum per group/bar)
  expect_equal(nrow(d), nrow(df_bar))
})


# -----------------------------------------------------------------------
# Snapshot tests
# -----------------------------------------------------------------------

test_that("geom_area_fade + stat_fourier snapshot", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(df_wave, aes(x, y)) +
    geom_area_fade(stat = "fourier", n_harmonics = 4, fill = "steelblue") +
    theme_minimal()

  vdiffr::expect_doppelganger("combo-area-fade-fourier", p)
})

test_that("geom_area_fade + stat_chaikin snapshot", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(df_bumps, aes(x, y)) +
    geom_area_fade(stat = "chaikin", iterations = 4, fill = "seagreen") +
    theme_minimal()

  vdiffr::expect_doppelganger("combo-area-fade-chaikin", p)
})

test_that("geom_ridgeline_fade + stat_chaikin snapshot", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(df_ridge, aes(x, y, height = height, group = group, fill = group)) +
    geom_ridgeline_fade(stat = "chaikin", iterations = 3, alpha_scope = "group") +
    scale_fill_viridis_d(guide = "none") +
    theme_minimal()

  vdiffr::expect_doppelganger("combo-ridgeline-fade-chaikin", p)
})

test_that("geom_area_fade + stat_pointless highlight snapshot", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(df_wave, aes(x, y)) +
    geom_area_fade(fill = "steelblue") +
    stat_pointless(geom = "point_glow", location = c("minimum", "maximum")) +
    theme_minimal()

  vdiffr::expect_doppelganger("combo-area-fade-pointless", p)
})

# -----------------------------------------------------------------------
# Integration: multiple doppelgänger geoms in one plot
# -----------------------------------------------------------------------

test_that("multiple fade geoms coexist in same plot", {
  df <- data.frame(
    x  = 1:10,
    y1 = cumsum(rnorm(10, 0, 2)),
    y2 = cumsum(rnorm(10, 0, 2))
  )
  p <- ggplot(df, aes(x)) +
    geom_line_fade(aes(y = y1), colour = "red") +
    geom_line_fade(aes(y = y2), colour = "blue")
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_point_glow + geom_smooth coexist in same plot", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point_glow(aes(colour = cyl), size = 3) +
    geom_smooth(method = "lm", se = FALSE, alpha = 0.3)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})
