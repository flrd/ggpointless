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

test_that("non-positive n_harmonics triggers a warning and falls back to 1", {
  p <- ggplot(df_sine, aes(x, y)) + geom_fourier(n_harmonics = -1)
  expect_warning(ggplotGrob(p), "n_harmonics")
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
