# Tests for geom_ridgeline_histogram_fade() and geom_ridgeline_freqpoly_fade()
# and the private StatBinRidges they share.

set.seed(1)
df_bin <- data.frame(
  group = factor(rep(c("a", "b", "c"), each = 50)),
  value = c(rnorm(50, 0), rnorm(50, 2), rnorm(50, 4))
)

# ---------------------------------------------------------------------------
# Construction & basic build
# ---------------------------------------------------------------------------

test_that("geom_ridgeline_histogram_fade builds without error", {
  p <- ggplot2::ggplot(df_bin, ggplot2::aes(value, group)) +
    geom_ridgeline_histogram_fade(bins = 8, scale = 4)
  expect_no_error(suppressWarnings(ggplot2::ggplotGrob(p)))
})

test_that("geom_ridgeline_freqpoly_fade builds without error", {
  p <- ggplot2::ggplot(df_bin, ggplot2::aes(value, group)) +
    geom_ridgeline_freqpoly_fade(bins = 8, scale = 4)
  expect_no_error(suppressWarnings(ggplot2::ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# StatBinRidges: per-(group, y) binning
# ---------------------------------------------------------------------------

test_that("StatBinRidges output: one ridge per y level", {
  p <- ggplot2::ggplot(df_bin, ggplot2::aes(value, group)) +
    geom_ridgeline_freqpoly_fade(bins = 8, scale = 4)
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
  # Output should contain rows for all three y-levels
  expect_true(length(unique(b$data[[1]]$y)) == 3L)
  # Each row should have a `height` column
  expect_true("height" %in% names(b$data[[1]]))
})

test_that("StatBinRidges height equals count per (y, bin)", {
  p <- ggplot2::ggplot(df_bin, ggplot2::aes(value, group)) +
    geom_ridgeline_freqpoly_fade(bins = 8, scale = 4)
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
  d <- b$data[[1]]
  expect_true(all(is.finite(d$height)) && all(d$height >= 0))
  # heights should sum (per group) close to the number of observations per
  # group (with `pad = TRUE` adds a couple of zero-count bins at edges,
  # so the sum is exact).
  by_group <- tapply(d$height, d$y, sum)
  expect_equal(unname(as.numeric(by_group)), rep(50, 3L))
})

# ---------------------------------------------------------------------------
# Histogram form: setup_data expands each bin into two points
# ---------------------------------------------------------------------------

test_that("histogram form: each bin yields two consecutive points", {
  p <- ggplot2::ggplot(df_bin, ggplot2::aes(value, group)) +
    geom_ridgeline_histogram_fade(bins = 8, scale = 4)
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
  d <- b$data[[1]]
  # Histogram form expands each bin to 2 rows (xmin, xmax with same height)
  # so total rows per group should be even and roughly twice the freqpoly form
  d_freq <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(
    ggplot2::ggplot(df_bin, ggplot2::aes(value, group)) +
      geom_ridgeline_freqpoly_fade(bins = 8, scale = 4)
  )))$data[[1]]
  expect_true(nrow(d) == 2L * nrow(d_freq))
})

# ---------------------------------------------------------------------------
# GoG: data / mapping / scales / coord / facets
# ---------------------------------------------------------------------------

test_that("GoG/data: empty data does not error", {
  p <- ggplot2::ggplot(
    df_bin[0, ],
    ggplot2::aes(value, group)
  ) +
    geom_ridgeline_freqpoly_fade(bins = 5)
  expect_no_error(suppressWarnings(ggplot2::ggplotGrob(p)))
})

test_that("GoG/scales: log10 x scale works", {
  df_pos <- data.frame(
    group = factor(rep(c("a", "b"), each = 30)),
    value = c(exp(rnorm(30, 1)), exp(rnorm(30, 2)))
  )
  p <- ggplot2::ggplot(df_pos, ggplot2::aes(value, group)) +
    geom_ridgeline_histogram_fade(bins = 6, scale = 4) +
    ggplot2::scale_x_log10()
  expect_no_error(suppressWarnings(ggplot2::ggplotGrob(p)))
})

test_that("GoG/coord: coord_flip does not error", {
  p <- ggplot2::ggplot(df_bin, ggplot2::aes(value, group)) +
    geom_ridgeline_freqpoly_fade(bins = 8, scale = 4) +
    ggplot2::coord_flip()
  expect_no_error(suppressWarnings(ggplot2::ggplotGrob(p)))
})

test_that("GoG/facets: facet_wrap with free scales", {
  df_facet <- df_bin
  df_facet$panel <- rep(c("p1", "p2"), length.out = nrow(df_facet))
  p <- ggplot2::ggplot(df_facet, ggplot2::aes(value, group)) +
    geom_ridgeline_freqpoly_fade(bins = 6, scale = 4) +
    ggplot2::facet_wrap(~panel, scales = "free_x")
  expect_no_error(suppressWarnings(ggplot2::ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Stat is locked: user cannot pass `stat = ...`
# ---------------------------------------------------------------------------

test_that("stat argument is not exposed in either constructor", {
  expect_false("stat" %in% names(formals(geom_ridgeline_histogram_fade)))
  expect_false("stat" %in% names(formals(geom_ridgeline_freqpoly_fade)))
})

# ---------------------------------------------------------------------------
# Auto-scale (`scale = NULL`)
# ---------------------------------------------------------------------------

test_that("scale = NULL (default) auto-scales to mountain-range ridges", {
  # Default: tallest ridge reaches 2 (~50% overlap into the next
  # baseline). Verified by inspecting the post-position ymax extent.
  p <- ggplot2::ggplot(df_bin, ggplot2::aes(value, group)) +
    geom_ridgeline_freqpoly_fade(bins = 8) # scale defaults to NULL
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
  d <- b$data[[1]]
  excursion <- max(d$ymax - d$ymin, na.rm = TRUE)
  expect_equal(excursion, 2, tolerance = 1e-9)
})

test_that("scale = NULL emits a one-shot inform with the resolved value", {
  p <- ggplot2::ggplot(df_bin, ggplot2::aes(value, group)) +
    geom_ridgeline_freqpoly_fade(bins = 8)
  expect_message(
    suppressWarnings(ggplot2::ggplot_build(p)),
    regexp = "scale = "
  )
})

test_that("scale = N (literal) does NOT emit the auto-scale inform", {
  p <- ggplot2::ggplot(df_bin, ggplot2::aes(value, group)) +
    geom_ridgeline_freqpoly_fade(bins = 8, scale = 0.5)
  # No `scale = ...` informational message when the user supplied scale.
  msgs <- testthat::capture_messages(
    suppressWarnings(ggplot2::ggplot_build(p))
  )
  expect_false(any(grepl("Using auto-computed", msgs, fixed = TRUE)))
})

test_that("scale = N (literal) bypasses auto-scaling", {
  p <- ggplot2::ggplot(df_bin, ggplot2::aes(value, group)) +
    geom_ridgeline_freqpoly_fade(bins = 8, scale = 0.2)
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
  d <- b$data[[1]]
  expected_max <- 0.2 * max(d$height, na.rm = TRUE)
  expect_equal(
    max(d$ymax - d$ymin, na.rm = TRUE),
    expected_max,
    tolerance = 1e-9
  )
})

test_that("auto-scale handles all-zero heights without dividing by zero", {
  df_zero <- data.frame(group = c("a", "a"), value = c(NA_real_, NA_real_))
  p <- ggplot2::ggplot(df_zero, ggplot2::aes(value, group)) +
    geom_ridgeline_freqpoly_fade(bins = 4)
  expect_no_error(suppressWarnings(ggplot2::ggplotGrob(p)))
})
