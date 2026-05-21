library(ggplot2)

# ---------------------------------------------------------------------------
# Shared data
# ---------------------------------------------------------------------------

df_xy   <- data.frame(x = 1:5, y = c(1, 3, 2, 4, 2))
df_1row <- data.frame(x = 1,   y = 1)
df_0row <- data.frame(x = numeric(0), y = numeric(0))
df_na   <- data.frame(x = 1:4, y = c(NA_real_, NA_real_, NA_real_, NA_real_))
df_inf  <- data.frame(x = 1:4, y = c(1, Inf, 2, -Inf))
df_same <- data.frame(x = 1:5, y = rep(3, 5))
df_dup  <- data.frame(x = c(1, 1, 2, 2), y = c(1, 2, 3, 4))

df_ridge <- data.frame(
  x = rep(0:4, 2), y = rep(c(1, 2), each = 5),
  height = c(0, 1, 2, 1, 0,  0, 2, 3, 2, 0),
  group  = rep(c("a", "b"), each = 5)
)

df_col <- data.frame(x = c("A", "B", "C"), y = c(3, 7, 5))


# ===========================================================================
# geom_area_fade
# ===========================================================================

test_that("geom_area_fade: alpha_fade_to rejects wrong types", {
  p <- ggplot(df_xy, aes(x, y))
  expect_error(ggplotGrob(p + geom_area_fade(alpha_fade_to = TRUE)),     "alpha_fade_to")
  expect_no_error(ggplotGrob(p + geom_area_fade(alpha_fade_to = 0L)))
  expect_error(ggplotGrob(p + geom_area_fade(alpha_fade_to = "zero")),   "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_area_fade(alpha_fade_to = list(0.5))),"alpha_fade_to")
  expect_error(ggplotGrob(p + geom_area_fade(alpha_fade_to = c(0, 0.5))),"alpha_fade_to")
  expect_error(ggplotGrob(p + geom_area_fade(alpha_fade_to = NaN)),      "alpha_fade_to")
})

test_that("geom_area_fade: outline.type rejects a length-2 vector", {
  p <- ggplot(df_xy, aes(x, y))
  expect_error(
    ggplotGrob(p + geom_area_fade(outline.type = c("upper", "lower"))),
    "outline.type"
  )
})

test_that("geom_area_fade: alpha_scope length-2 silently uses first match", {
  # rlang::arg_match() takes the first valid match; no error is issued
  p <- ggplot(df_xy, aes(x, y))
  expect_no_error(suppressWarnings(ggplotGrob(p + geom_area_fade(alpha_scope = c("global", "group")))))
})

test_that("geom_area_fade: single-row data renders without error (zeroGrob path)", {
  p <- ggplot(df_1row, aes(x, y)) + geom_area_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_area_fade: zero-row data renders without error", {
  p <- ggplot(df_0row, aes(x, y)) + geom_area_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_area_fade: all-NA y renders without error", {
  p <- ggplot(df_na, aes(x, y)) + geom_area_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_area_fade: Inf in y renders without error", {
  p <- ggplot(df_inf, aes(x, y)) + geom_area_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_area_fade: all-same y renders without error", {
  p <- ggplot(df_same, aes(x, y)) + geom_area_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("geom_area_fade: stat = 'density' errors (density drops y, which geom_area_fade requires)", {
  p <- ggplot(df_xy, aes(x, y)) + geom_area_fade(stat = "density")
  expect_error(suppressWarnings(ggplotGrob(p)))
})


# ===========================================================================
# geom_col_fade / geom_bar_fade / geom_histogram_fade
# ===========================================================================

test_that("geom_col_fade: alpha_fade_to rejects wrong types", {
  p <- ggplot(df_col, aes(x, y))
  expect_error(ggplotGrob(p + geom_col_fade(alpha_fade_to = TRUE)),     "alpha_fade_to")
  expect_no_error(ggplotGrob(p + geom_col_fade(alpha_fade_to = 0L)))
  expect_error(ggplotGrob(p + geom_col_fade(alpha_fade_to = "zero")),   "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_col_fade(alpha_fade_to = list(0.5))),"alpha_fade_to")
  expect_error(ggplotGrob(p + geom_col_fade(alpha_fade_to = c(0, 0.5))),"alpha_fade_to")
  expect_error(ggplotGrob(p + geom_col_fade(alpha_fade_to = NaN)),      "alpha_fade_to")
})

test_that("geom_col_fade: alpha_scope rejects 'area' (valid for area/ridgeline, not col)", {
  p <- ggplot(df_col, aes(x, y))
  expect_error(ggplotGrob(p + geom_col_fade(alpha_scope = "area")), "alpha_scope")
})

test_that("geom_col_fade: alpha_scope rejects a length-2 vector", {
  p <- ggplot(df_col, aes(x, y))
  # arg_match0() errors with "Problem while setting up geom" wrapper
  expect_error(ggplotGrob(p + geom_col_fade(alpha_scope = c("bar", "global"))))
})

test_that("geom_col_fade: radius as plain numeric is silently coerced to unit", {
  p <- ggplot(df_col, aes(x, y)) + geom_col_fade(radius = 5)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_col_fade: radius as length-2 numeric vector does not crash", {
  p <- ggplot(df_col, aes(x, y)) + geom_col_fade(radius = c(3, 6))
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_col_fade: radius as list errors or coerces safely", {
  p <- ggplot(df_col, aes(x, y)) + geom_col_fade(radius = list(grid::unit(3, "pt")))
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_col_fade: all-zero heights render without error", {
  p <- ggplot(data.frame(x = c("A", "B"), y = c(0, 0)), aes(x, y)) +
    geom_col_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_col_fade: single bar renders without error", {
  p <- ggplot(data.frame(x = "A", y = 5), aes(x, y)) + geom_col_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("geom_col_fade: very large values (1e15) render without error", {
  p <- ggplot(data.frame(x = c("A","B"), y = c(1e15, 2e15)), aes(x, y)) +
    geom_col_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("geom_col_fade: stat = 'density' errors (density drops y, which geom_col_fade requires)", {
  p <- ggplot(df_xy, aes(x, y)) + geom_col_fade(stat = "density")
  expect_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_bar_fade: alpha_scope = 'area' is rejected", {
  expect_error(
    ggplotGrob(ggplot(df_xy, aes(x)) + geom_bar_fade(alpha_scope = "area")),
    "alpha_scope"
  )
})

test_that("geom_histogram_fade: alpha_scope = 'area' is rejected", {
  expect_error(
    ggplotGrob(ggplot(df_xy, aes(x)) + geom_histogram_fade(alpha_scope = "area")),
    "alpha_scope"
  )
})

test_that("geom_histogram_fade: bins = 1 renders without error", {
  p <- ggplot(df_xy, aes(x)) + geom_histogram_fade(bins = 1)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_histogram_fade: stat = 'identity' requires xmin/xmax but errors informatively", {
  # When stat = 'identity' is used, GeomColFade needs pre-computed xmin/xmax.
  # Without them ggplot2 should warn or error; it must not crash silently.
  p <- ggplot(df_xy, aes(x, y)) + geom_histogram_fade(stat = "identity")
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_freqpoly_fade: bins = 1 renders without error", {
  p <- ggplot(df_xy, aes(x)) + geom_freqpoly_fade(bins = 1)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})


# ===========================================================================
# geom_density_fade
# ===========================================================================

test_that("geom_density_fade: alpha_fade_to rejects wrong types", {
  p <- ggplot(df_xy, aes(x))
  expect_error(ggplotGrob(p + geom_density_fade(alpha_fade_to = TRUE)),   "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_density_fade(alpha_fade_to = "zero")), "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_density_fade(alpha_fade_to = 2)),      "alpha_fade_to")
})

test_that("geom_density_fade: single unique value renders without crashing", {
  p <- ggplot(data.frame(x = rep(3, 5)), aes(x)) +
    geom_density_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_density_fade: very small sample (n = 2) renders without crashing", {
  p <- ggplot(data.frame(x = c(1, 2)), aes(x)) + geom_density_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_density_fade: stat = 'bin' (unusual combo) renders without crashing", {
  p <- ggplot(df_xy, aes(x)) + geom_density_fade(stat = "bin")
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})


# ===========================================================================
# geom_ridgeline_fade / geom_ridgeline_density_fade
# ===========================================================================

test_that("geom_ridgeline_fade: alpha_fade_to rejects wrong types", {
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group))
  expect_error(ggplotGrob(p + geom_ridgeline_fade(alpha_fade_to = TRUE)),     "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_ridgeline_fade(alpha_fade_to = "zero")),   "alpha_fade_to")
  expect_no_error(ggplotGrob(p + geom_ridgeline_fade(alpha_fade_to = 0L)))
  expect_error(ggplotGrob(p + geom_ridgeline_fade(alpha_fade_to = list(0))),  "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_ridgeline_fade(alpha_fade_to = c(0, 0.5))),"alpha_fade_to")
  expect_error(ggplotGrob(p + geom_ridgeline_fade(alpha_fade_to = NaN)),      "alpha_fade_to")
})

test_that("geom_ridgeline_fade: alpha_scope rejects 'bar' (col-only value)", {
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group))
  expect_error(ggplotGrob(p + geom_ridgeline_fade(alpha_scope = "bar")), "alpha_scope")
})

test_that("geom_ridgeline_fade: outline.type rejects a length-2 vector", {
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group))
  expect_error(
    ggplotGrob(p + geom_ridgeline_fade(outline.type = c("upper", "lower"))),
    "outline.type"
  )
})

test_that("geom_ridgeline_fade: outline.type rejects invalid string", {
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group))
  expect_error(
    ggplotGrob(p + geom_ridgeline_fade(outline.type = "diagonal")),
    "outline.type"
  )
})

test_that("position_ridgeline: scale = 'big' errors at build time (not construction)", {
  # Construction is lenient — the error fires when the position is actually used.
  pos <- position_ridgeline(scale = "big")
  p   <- ggplot(df_ridge, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(scale = "big")
  expect_error(ggplotGrob(p))
})

test_that("position_ridgeline: scale = Inf errors at build time", {
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(scale = Inf)
  expect_error(ggplotGrob(p))
})

test_that("position_ridgeline: scale = NA errors at build time", {
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(scale = NA_real_)
  expect_error(ggplotGrob(p))
})

test_that("position_ridgeline: scale = c(1, 2) errors at build time", {
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(scale = c(1, 2))
  expect_error(ggplotGrob(p))
})

test_that("position_ridgeline: min_height = Inf errors at build time", {
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(min_height = Inf)
  expect_error(ggplotGrob(p))
})

test_that("position_ridgeline: min_height = 'zero' errors at build time", {
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(min_height = "zero")
  expect_error(ggplotGrob(p))
})

test_that("geom_ridgeline_fade: all-zero height renders without error (zeroGrob path)", {
  df_zero <- data.frame(x = 0:4, y = 1, height = 0, group = "a")
  p <- ggplot(df_zero, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_ridgeline_fade: all-NA height renders without error (zeroGrob path)", {
  df_naheight <- data.frame(x = 0:4, y = 1, height = NA_real_, group = "a")
  p <- ggplot(df_naheight, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_ridgeline_fade: single-row group renders without error (zeroGrob path)", {
  df_1 <- data.frame(x = 1, y = 1, height = 2, group = "a")
  p <- ggplot(df_1, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_ridgeline_fade: stat = 'bin' errors (stat_bin requires only x or y)", {
  p <- ggplot(df_xy, aes(x, y = 1, height = after_stat(count), group = 1)) +
    geom_ridgeline_fade(stat = "bin")
  expect_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_ridgeline_fade: stat = 'fourier' errors (fourier drops height, which geom requires)", {
  p <- ggplot(df_ridge, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(stat = "fourier")
  expect_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_ridgeline_density_fade: alpha_fade_to rejects wrong types", {
  p <- ggplot(iris, aes(Sepal.Length, y = as.numeric(Species), group = Species))
  expect_error(
    ggplotGrob(p + geom_ridgeline_density_fade(alpha_fade_to = "zero")),
    "alpha_fade_to"
  )
})

test_that("geom_ridgeline_density_fade: scale = Inf errors at build time", {
  p <- ggplot(iris, aes(Sepal.Length, y = as.numeric(Species), group = Species)) +
    geom_ridgeline_density_fade(scale = Inf)
  expect_error(ggplotGrob(p))
})

test_that("geom_ridgeline_density_fade: single-species data renders without error", {
  p <- ggplot(
    subset(iris, Species == "setosa"),
    aes(Sepal.Length, y = 1, group = 1)
  ) + geom_ridgeline_density_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})


# ===========================================================================
# geom_catenary / stat_catenary
# ===========================================================================

test_that("geom_catenary: chainLength (deprecated) now errors", {
  expect_error(geom_catenary(chainLength = 2), "deprecated")
})

test_that("geom_catenary: chain_length shorter than segment emits warning (straight line drawn)", {
  p <- ggplot(data.frame(x = c(0, 1), y = c(0, 0)), aes(x, y)) +
    geom_catenary(chain_length = 0.5)
  expect_warning(ggplotGrob(p))
})

test_that("geom_catenary: chain_length = Inf emits warning and does not crash", {
  p <- ggplot(data.frame(x = c(0, 1), y = c(0, 0)), aes(x, y)) +
    geom_catenary(chain_length = Inf)
  expect_warning(ggplotGrob(p))
})

test_that("geom_catenary: sag = -1 renders without crash (warn or silently)", {
  p <- ggplot(data.frame(x = c(0, 1), y = c(0, 0)), aes(x, y)) +
    geom_catenary(sag = -1)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_catenary: both chain_length and sag renders without error (sag wins silently)", {
  # No warning is issued; sag silently takes precedence over chain_length
  p <- ggplot(data.frame(x = c(0, 1), y = c(0, 0)), aes(x, y)) +
    geom_catenary(chain_length = 2, sag = 0.2)
  expect_no_error(suppressMessages(suppressWarnings(ggplotGrob(p))))
})

test_that("geom_catenary: single-point data renders without crash", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_catenary()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_catenary: all-NA y renders without crash", {
  p <- ggplot(data.frame(x = 1:3, y = c(NA, NA, NA)), aes(x, y)) +
    geom_catenary()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_catenary: vectorised chain_length longer than number of segments warns", {
  # 3 values for 2 segments — extra value should be dropped with a message
  p <- ggplot(data.frame(x = c(0, 1, 2), y = c(0, 0, 0)), aes(x, y)) +
    geom_catenary(chain_length = c(1.5, 2, 3))
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})


# ===========================================================================
# geom_chaikin / stat_chaikin
# ===========================================================================

test_that("stat_chaikin: iterations = -1 errors", {
  p <- ggplot(df_xy, aes(x, y)) + geom_chaikin(iterations = -1)
  expect_error(ggplotGrob(p), "iterations")
})

test_that("stat_chaikin: iterations = 11 errors (> 10 limit)", {
  p <- ggplot(df_xy, aes(x, y)) + geom_chaikin(iterations = 11)
  expect_error(ggplotGrob(p), "iterations")
})

test_that("stat_chaikin: iterations = Inf errors", {
  p <- ggplot(df_xy, aes(x, y)) + geom_chaikin(iterations = Inf)
  expect_error(ggplotGrob(p), "iterations")
})

test_that("stat_chaikin: iterations = 'many' errors", {
  p <- ggplot(df_xy, aes(x, y)) + geom_chaikin(iterations = "many")
  expect_error(ggplotGrob(p), "iterations")
})

test_that("stat_chaikin: iterations = 1.5 (non-integer) errors", {
  p <- ggplot(df_xy, aes(x, y)) + geom_chaikin(iterations = 1.5)
  expect_error(ggplotGrob(p), "iterations")
})

test_that("stat_chaikin: ratio = -0.1 errors", {
  p <- ggplot(df_xy, aes(x, y)) + geom_chaikin(ratio = -0.1)
  expect_error(ggplotGrob(p), "ratio")
})

test_that("stat_chaikin: ratio = 1.1 errors", {
  p <- ggplot(df_xy, aes(x, y)) + geom_chaikin(ratio = 1.1)
  expect_error(ggplotGrob(p), "ratio")
})

test_that("stat_chaikin: ratio = Inf errors", {
  p <- ggplot(df_xy, aes(x, y)) + geom_chaikin(ratio = Inf)
  expect_error(ggplotGrob(p), "ratio")
})

test_that("stat_chaikin: ratio > 0.5 warns (complement conversion)", {
  p <- ggplot(df_xy, aes(x, y)) + geom_chaikin(ratio = 0.7)
  expect_snapshot_warning(ggplotGrob(p))
})

test_that("stat_chaikin: mode = 'diagonal' errors", {
  p <- ggplot(df_xy, aes(x, y)) + geom_chaikin(mode = "diagonal")
  expect_error(ggplotGrob(p), "mode")
})

test_that("stat_chaikin: data with 2 points warns (needs >= 3)", {
  p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) +
    geom_chaikin()
  expect_warning(ggplotGrob(p))
})

test_that("stat_chaikin: data with 1 point warns", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_chaikin()
  expect_warning(ggplotGrob(p))
})

test_that("stat_chaikin: Inf values in data warn (non-finite removed)", {
  p <- ggplot(data.frame(x = c(1, 2, Inf, 4, 5), y = c(1, 2, 3, 4, 5)),
              aes(x, y)) +
    geom_chaikin()
  expect_warning(ggplotGrob(p))
})

test_that("stat_chaikin: NA values in data warn (non-finite removed)", {
  p <- ggplot(data.frame(x = c(1, 2, NA, 4, 5), y = c(1, 2, 3, 4, 5)),
              aes(x, y)) +
    geom_chaikin()
  expect_warning(ggplotGrob(p))
})

test_that("stat_chaikin: closed = TRUE (deprecated) errors", {
  p <- ggplot(df_xy, aes(x, y)) + geom_chaikin(closed = TRUE)
  expect_error(ggplotGrob(p))
})



# ===========================================================================
# geom_fourier / stat_fourier
# ===========================================================================

test_that("stat_fourier: n_harmonics = 0 renders without crash (flat line)", {
  p <- ggplot(df_xy, aes(x, y)) + geom_fourier(n_harmonics = 0)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("stat_fourier: n_harmonics larger than data points does not crash", {
  p <- ggplot(df_xy, aes(x, y)) + geom_fourier(n_harmonics = 999)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("stat_fourier: n_harmonics = 'all' errors informatively", {
  p <- ggplot(df_xy, aes(x, y)) + geom_fourier(n_harmonics = "all")
  expect_error(ggplotGrob(p))
})

test_that("stat_fourier: detrend = 'polynomial' errors informatively", {
  p <- ggplot(df_xy, aes(x, y)) + geom_fourier(detrend = "polynomial")
  expect_error(ggplotGrob(p), "detrend")
})

test_that("stat_fourier: single-row data renders without crash", {
  p <- ggplot(df_1row, aes(x, y)) + geom_fourier()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("stat_fourier: two-row data renders without crash", {
  p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) + geom_fourier()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("stat_fourier: all-same y renders without crash (zero-variance signal)", {
  p <- ggplot(df_same, aes(x, y)) + geom_fourier(n_harmonics = 2)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("stat_fourier: n_harmonics = -1 errors", {
  p <- ggplot(df_xy, aes(x, y)) + geom_fourier(n_harmonics = -1)
  expect_error(ggplotGrob(p))
})

test_that("stat_fourier: n_harmonics = Inf errors", {
  p <- ggplot(df_xy, aes(x, y)) + geom_fourier(n_harmonics = Inf)
  expect_error(ggplotGrob(p))
})


# ===========================================================================
# geom_lexis / stat_lexis
# ===========================================================================

df_lexis <- data.frame(key = c("A", "B"), x = c(0, 2), xend = c(5, 6))

test_that("geom_lexis: gap_filler = 'yes' errors", {
  p <- ggplot(df_lexis, aes(x, xend = xend, colour = key)) +
    geom_lexis(gap_filler = "yes")
  expect_error(ggplotGrob(p), "gap_filler")
})

test_that("geom_lexis: gap_filler = 1 (numeric) errors", {
  p <- ggplot(df_lexis, aes(x, xend = xend, colour = key)) +
    geom_lexis(gap_filler = 1)
  expect_error(ggplotGrob(p), "gap_filler")
})

test_that("geom_lexis: gap_filler = NA errors", {
  p <- ggplot(df_lexis, aes(x, xend = xend, colour = key)) +
    geom_lexis(gap_filler = NA)
  expect_error(ggplotGrob(p), "gap_filler")
})

test_that("geom_lexis: point_show = 'no' errors", {
  p <- ggplot(df_lexis, aes(x, xend = xend)) +
    geom_lexis(point_show = "no")
  expect_error(ggplotGrob(p), "point_show")
})

test_that("geom_lexis: x == xend (zero-length segment) renders without crash", {
  p <- ggplot(data.frame(x = c(1, 2), xend = c(1, 2)), aes(x, xend = xend)) +
    geom_lexis()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_lexis: x > xend (reversed segment) renders without crash", {
  p <- ggplot(data.frame(x = c(5, 3), xend = c(2, 1)), aes(x, xend = xend)) +
    geom_lexis()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_lexis: Inf in x renders without crash", {
  p <- ggplot(data.frame(x = c(0, Inf), xend = c(1, 2)), aes(x, xend = xend)) +
    geom_lexis()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_lexis: single observation renders without crash", {
  p <- ggplot(data.frame(x = 0, xend = 3), aes(x, xend = xend)) +
    geom_lexis()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})


# ===========================================================================
# geom_point_glow
# ===========================================================================

test_that("geom_point_glow: glow_alpha < 0 warns or errors (currently unguarded)", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point_glow(glow_alpha = -0.1)
  # Current behaviour: silently renders.  This test documents the gap —
  # out-of-range glow_alpha should at minimum warn.
  expect_warning(ggplotGrob(p))
})

test_that("geom_point_glow: glow_alpha > 1 warns or errors (currently unguarded)", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point_glow(glow_alpha = 2)
  expect_warning(ggplotGrob(p))
})

test_that("geom_point_glow: glow_alpha = 'bright' errors (non-numeric)", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point_glow(glow_alpha = "bright")
  expect_error(ggplotGrob(p))
})

test_that("geom_point_glow: glow_size < 0 warns or errors (currently unguarded)", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point_glow(glow_size = -1)
  expect_warning(ggplotGrob(p))
})

test_that("geom_point_glow: glow_size = 0 renders without crash (invisible glow)", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point_glow(glow_size = 0)
  expect_no_error(ggplotGrob(p))
})

test_that("geom_point_glow: glow_colour = 123 (numeric palette index) accepted", {
  # farver treats bare numerics as palette indices (same as ggplot2's
  # internal colour handling); we delegate validation to farver.
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point_glow(glow_colour = 123)
  expect_no_error(ggplotGrob(p))
})

test_that("geom_point_glow: glow_colour = list('red') (unwrappable) accepted", {
  # farver unwraps a list-of-strings into colour specs; we follow.
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point_glow(glow_colour = list("red"))
  expect_no_error(ggplotGrob(p))
})

test_that("geom_point_glow: glow_colour = list('lorem') errors with farver hint", {
  # farver rejects unknown colour names with "Unknown colour name: lorem";
  # our validator surfaces that as the parent cause.
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point_glow(glow_colour = list("lorem"))
  expect_error(ggplotGrob(p), "glow_colour")
  expect_error(ggplotGrob(p), "Unknown colour name")
})

test_that("geom_point_glow: zero-row data renders without crash", {
  p <- ggplot(df_0row, aes(x, y)) + geom_point_glow()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_point_glow: single point renders without crash", {
  p <- ggplot(df_1row, aes(x, y)) + geom_point_glow()
  expect_no_error(ggplotGrob(p))
})

test_that("geom_point_glow: all-NA coordinates renders without crash", {
  p <- ggplot(df_na, aes(x, y)) + geom_point_glow()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})


# ===========================================================================
# geom_pointless / stat_pointless
# ===========================================================================

test_that("stat_pointless: invalid location value warns and falls back to 'last'", {
  p <- ggplot(df_xy, aes(x, y)) + geom_line() +
    geom_pointless(location = "everywhere")
  expect_warning(ggplotGrob(p))
  # Invalid location is dropped; stat falls back to "last", returning 1 point
  d <- suppressWarnings(ggplot_build(p))$data[[2]]
  expect_equal(nrow(d), 1L)
})

test_that("stat_pointless: location = NULL renders without crash (no points drawn)", {
  p <- ggplot(df_xy, aes(x, y)) + geom_line() +
    geom_pointless(location = NULL)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("stat_pointless: location = integer vector errors informatively", {
  p <- ggplot(df_xy, aes(x, y)) + geom_line() +
    geom_pointless(location = 1:3)
  expect_error(ggplotGrob(p))
})

test_that("stat_pointless: mixed valid/invalid location keeps valid, warns about invalid", {
  p <- ggplot(df_xy, aes(x, y)) + geom_line() +
    geom_pointless(location = c("first", "everywhere"))
  expect_warning(ggplotGrob(p))
})

test_that("stat_pointless: single-row data renders without crash", {
  p <- ggplot(df_1row, aes(x, y)) + geom_pointless(location = "first")
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("stat_pointless: all-same y renders without crash (min == max)", {
  p <- ggplot(df_same, aes(x, y)) + geom_line() +
    geom_pointless(location = c("minimum", "maximum"))
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("stat_pointless: zero-row data renders without crash", {
  p <- ggplot(df_0row, aes(x, y)) + geom_line() +
    geom_pointless(location = "all")
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})
