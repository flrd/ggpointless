library(ggplot2)

set.seed(1)
n <- 40L
df_sine <- data.frame(
  x = seq(0, 2 * pi, length.out = n),
  y = sin(seq(0, 2 * pi, length.out = n)) + rnorm(n, sd = 0.2)
)

set.seed(2)
x_drift <- seq(0, 4 * pi, length.out = n)
df_drift <- data.frame(
  x = x_drift,
  y = sin(x_drift) + x_drift * 0.3 + rnorm(n, sd = 0.15)
)

set.seed(3)
x_grp <- seq(0, 2 * pi, length.out = n / 2L)
df_two <- rbind(
  data.frame(x = x_grp, y = sin(x_grp) + rnorm(n / 2L, sd = 0.15), g = "sine"),
  data.frame(x = x_grp, y = cos(x_grp) + rnorm(n / 2L, sd = 0.15), g = "cosine")
)

# --- setup_params validation -------------------------------------------------

test_that("invalid detrend value is rejected", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier(detrend = "spline")
  expect_error(ggplotGrob(p), "detrend")
})

test_that("negative n_harmonics errors informatively", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier(n_harmonics = -1)
  expect_error(ggplotGrob(p), "n_harmonics")
})

test_that("n_harmonics exceeding Nyquist limit triggers a warning", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier(n_harmonics = 10000L)
  expect_warning(ggplotGrob(p), "Nyquist")
})

# --- NA / missing value handling ---------------------------------------------

test_that("non-finite values emit a warning without na.rm", {
  df_na <- df_sine
  df_na$y[5L] <- NA_real_
  p <- ggplot(df_na, aes(x, y)) + geom_fourier()
  expect_warning(ggplotGrob(p), "non-finite")
})

test_that("non-finite values are silently dropped with na.rm = TRUE", {
  df_na <- df_sine
  df_na$y[5L] <- NA_real_
  p <- ggplot(df_na, aes(x, y)) + geom_fourier(na.rm = TRUE)
  expect_no_warning(ggplotGrob(p))
})

# --- visual tests ------------------------------------------------------------

test_that("geom_fourier default (all harmonics)", {
  p <- ggplot(df_sine, aes(x, y)) +
    geom_point(alpha = 0.4) +
    geom_fourier() +
    theme_minimal()
  vdiffr::expect_doppelganger("fourier default", p)
})

test_that("n_harmonics = 1 produces a smooth single-frequency curve", {
  p <- ggplot(df_sine, aes(x, y)) +
    geom_fourier(n_harmonics = 1L) +
    theme_minimal()
  vdiffr::expect_doppelganger("fourier n_harmonics 1", p)
})

test_that("detrend = 'lm' removes a linear trend before fitting", {
  p <- ggplot(df_drift, aes(x, y)) +
    geom_point(alpha = 0.4) +
    geom_fourier(detrend = "lm") +
    theme_minimal()
  vdiffr::expect_doppelganger("fourier detrend lm", p)
})

test_that("detrend = 'loess' removes a smooth trend before fitting", {
  p <- ggplot(df_drift, aes(x, y)) +
    geom_point(alpha = 0.4) +
    geom_fourier(detrend = "loess") +
    theme_minimal()
  vdiffr::expect_doppelganger("fourier detrend loess", p)
})

test_that("geom_fourier handles two groups independently", {
  p <- ggplot(df_two, aes(x, y, colour = g)) +
    geom_point(alpha = 0.4) +
    geom_fourier() +
    theme_minimal()
  vdiffr::expect_doppelganger("fourier two groups", p)
})

test_that("non-uniform x-spacing emits a warning", {
  df_gap <- data.frame(
    x = c(1:10, 19:20),
    y = sin(seq_len(12))
  )
  p <- ggplot(df_gap, aes(x, y)) + geom_fourier()
  expect_warning(ggplotGrob(p), "irregular x-spacing")
})

test_that("stat_fourier is equivalent to geom_fourier", {
  p <- ggplot(df_sine, aes(x, y)) +
    stat_fourier(geom = "line") +
    theme_minimal()
  vdiffr::expect_doppelganger("stat_fourier direct", p)
})


# ===========================================================================
# Grammar of Graphics adversarial stress tests
# ===========================================================================

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

test_that("GoG/data: empty dataset does not error", {
  p <- ggplot(data.frame(x = numeric(), y = numeric()), aes(x, y)) +
    geom_fourier()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: single point does not error", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_fourier()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: two points does not error", {
  p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) +
    geom_fourier()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: all-NA y values do not error", {
  p <- ggplot(data.frame(x = 1:5, y = NA_real_), aes(x, y)) +
    geom_fourier()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Mapping
# ---------------------------------------------------------------------------

test_that("GoG/mapping: colour aesthetic mapping per group does not error", {
  p <- ggplot(df_two, aes(x, y, colour = g)) + geom_fourier()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/mapping: inherit.aes = FALSE isolates from plot mapping", {
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point() +
    geom_fourier(data = df_sine, mapping = aes(x, y), inherit.aes = FALSE)
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Layer
# ---------------------------------------------------------------------------

test_that("GoG/layer: multiple geom_fourier layers do not error", {
  p <- ggplot(df_sine, aes(x, y)) +
    geom_fourier(n_harmonics = 2, colour = "red") +
    geom_fourier(n_harmonics = 10, colour = "blue")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/layer: geom_fourier standalone does not error", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Scales
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_y_reverse does not error", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier() + scale_y_reverse()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: explicit limits do not error", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier() +
    scale_y_continuous(limits = c(-5, 5))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: expand = c(0, 0) does not error", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier() +
    scale_y_continuous(expand = c(0, 0))
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Coord
# ---------------------------------------------------------------------------

test_that("GoG/coord: coord_cartesian zoom does not error", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier() +
    coord_cartesian(ylim = c(-0.5, 0.5))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_fixed does not error", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier() + coord_fixed()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_flip does not error", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier() + coord_flip()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_polar does not error", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier() + coord_polar()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Facets
# ---------------------------------------------------------------------------

test_that("GoG/facets: facet_wrap with free scales does not error", {
  p <- ggplot(df_two, aes(x, y)) + geom_fourier() +
    facet_wrap(~g, scales = "free")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/facets: facet_grid with free scales does not error", {
  p <- ggplot(df_two, aes(x, y)) + geom_fourier() +
    facet_grid(~g, scales = "free")
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------

test_that("GoG/theme: theme_void does not error", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier() + theme_void()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: theme_classic does not error", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier() + theme_classic()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: theme_bw does not error", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier() + theme_bw()
  expect_no_error(ggplotGrob(p))
})
