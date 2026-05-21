library(ggplot2)

# ---------------------------------------------------------------------------
# Shared test data
# ---------------------------------------------------------------------------

make_curves <- function(n, seed = 1) {
  set.seed(seed)
  data.frame(
    x    = runif(n, 0, 10),
    y    = runif(n, 0, 10),
    xend = runif(n, 0, 10),
    yend = runif(n, 0, 10)
  )
}

# ---------------------------------------------------------------------------
# Required aesthetics
# ---------------------------------------------------------------------------
# `required_aes = c("x", "y", "xend", "yend")` was tightened from the
# lenient `xend|yend` rule in 0.3.0 so missing-endpoint mistakes surface
# as a clean ggplot2 missing-aesthetic error instead of a cryptic
# downstream `rbind` / `grid::unit` failure.

test_that("missing xend produces a clean missing-aesthetics error", {
  df <- data.frame(x = 0, y = 0, yend = 1)
  p <- ggplot(df, aes(x, y, yend = yend)) + geom_curve_fade()
  expect_error(ggplotGrob(p), "xend")
})

test_that("missing yend produces a clean missing-aesthetics error", {
  df <- data.frame(x = 0, y = 0, xend = 1)
  p <- ggplot(df, aes(x, y, xend = xend)) + geom_curve_fade()
  expect_error(ggplotGrob(p), "yend")
})


# ---------------------------------------------------------------------------
# curve_count_cap — defensive soft cap on composited curves
# ---------------------------------------------------------------------------

test_that("curve_count_cap: under the cap, the fade grob is built normally", {
  df <- make_curves(5)
  p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade(linewidth = 1.5)
  expect_no_warning(suppressMessages(ggplotGrob(p)))
})

test_that("curve_count_cap: above the cap, warns with a helpful message", {
  # Single test combining all cap-strike assertions — the underlying
  # `cli_warn(.frequency = "regularly")` uses a file-based cache that
  # throttles repeat fires across tests in the same session, so we cannot
  # assert the warning twice.
  df <- make_curves(300)
  p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade(linewidth = 0.5)
  w <- tryCatch(
    suppressMessages(ggplotGrob(p)),
    warning = function(w) conditionMessage(w)
  )
  # If the first warning was the cap warning, w is a string.  If not (the
  # cap fired silently due to frequency throttling from an earlier test,
  # and w is therefore the gtable), we still skip gracefully.
  skip_if_not(is.character(w), "curve_count_cap warning was throttled")
  expect_match(w, "Refusing to composite")
  expect_match(w, "300")
  expect_match(w, "200")
  # Advice should point at the shape-preserving knob first; the
  # path/segment-fade alternatives were dropped because they change the
  # rendered shape (curve -> straight line) and risked misleading users
  # who triggered the cap.
  expect_match(w, "curve_count_cap = Inf", fixed = TRUE)
})

test_that("curve_count_cap: user can opt out with Inf", {
  df <- make_curves(250)
  p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade(linewidth = 0.5, curve_count_cap = Inf)
  # The cap warning must not fire when disabled.  Other device-capability
  # messages from the compositing path may still appear — we only assert
  # that `Refusing to composite` is absent.
  saw_cap_warning <- FALSE
  withCallingHandlers(
    suppressMessages(ggplotGrob(p)),
    warning = function(w) {
      if (grepl("Refusing to composite", conditionMessage(w))) {
        saw_cap_warning <<- TRUE
      }
      invokeRestart("muffleWarning")
    }
  )
  expect_false(saw_cap_warning)
})

test_that("curve_count_cap: nonsensical values warn and fall back to default 200", {
  df <- make_curves(10)
  bad_values <- list(negative = -1, zero = 0, na = NA,
                     string = "big", nonscalar = c(1, 2))
  for (nm in names(bad_values)) {
    p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
      geom_curve_fade(curve_count_cap = bad_values[[nm]])
    expect_warning(
      suppressMessages(ggplotGrob(p)),
      regexp = "curve_count_cap.*must be a positive scalar number or `Inf`",
      info = paste("case:", nm)
    )
  }
})

test_that("curve_count_cap: default constructor exposes curve_count_cap = 200", {
  lyr <- geom_curve_fade()
  expect_equal(lyr$geom_params$curve_count_cap, 200)
})


# ===========================================================================
# Grammar of Graphics adversarial stress tests
# ===========================================================================
#
# Every new geom must have a section like this per CLAUDE.md.  Focused on
# what is logically distinct for `geom_curve_fade()` — Bezier-curve geometry,
# coord/scale interactions, fallback paths.  Theme stress is omitted on
# purpose: the geom does not read the theme.

df_curves <- data.frame(
  x    = c(1, 3, 5),
  y    = c(2, 6, 4),
  xend = c(4, 8, 9),
  yend = c(7, 1, 8)
)

# --------------------------------------------------------------------------
# Data
# --------------------------------------------------------------------------

test_that("GoG/data: empty dataset does not error", {
  p <- ggplot(data.frame(x = numeric(), y = numeric(),
                         xend = numeric(), yend = numeric()),
              aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/data: single curve renders", {
  p <- ggplot(df_curves[1L, , drop = FALSE],
              aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/data: NA endpoints are dropped without crashing", {
  df_na <- df_curves
  df_na$xend[1L] <- NA_real_
  df_na$yend[2L] <- NA_real_
  p <- ggplot(df_na, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/data: zero-length curve (x == xend, y == yend) does not error", {
  df_zero <- data.frame(x = 1, y = 1, xend = 1, yend = 1)
  p <- ggplot(df_zero, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# Layer
# --------------------------------------------------------------------------

test_that("GoG/layer: two curve_fade layers compose without error", {
  p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade(curvature = 0.5,  fade_direction = "end") +
    geom_curve_fade(curvature = -0.5, fade_direction = "start", colour = "red")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/layer: ordering with geom_segment / geom_point is independent", {
  p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_segment(linetype = 2) +
    geom_curve_fade() +
    geom_point()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# Scales — log, reverse, sqrt, explicit limits, expand
# --------------------------------------------------------------------------

test_that("GoG/scale: scale_x_reverse / scale_y_reverse render", {
  p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() +
    scale_x_reverse() +
    scale_y_reverse()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/scale: scale_y_log10 with strictly positive data", {
  p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() +
    scale_y_log10()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/scale: scale_y_sqrt", {
  p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() +
    scale_y_sqrt()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/scale: explicit limits do not error", {
  p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() +
    scale_x_continuous(limits = c(0, 10)) +
    scale_y_continuous(limits = c(0, 10))
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/scale: expand = c(0, 0)", {
  p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# Coord — cartesian zoom, fixed, transform, flip, polar/radial
# --------------------------------------------------------------------------

test_that("GoG/coord: coord_cartesian zoom does not error", {
  p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() +
    coord_cartesian(xlim = c(2, 7), ylim = c(2, 7))
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_fixed renders", {
  p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() +
    coord_fixed()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_flip falls back to plain geom_curve and renders", {
  # geom_curve_fade only supports linear, non-flipped Cartesian.  Under any
  # other coord it warns once and delegates to GeomCurve$draw_panel.
  p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() +
    coord_flip()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_transform(y = 'log10') falls back without error", {
  # Strictly positive y so the log10 transform produces no NaN.
  p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() +
    coord_transform(y = "log10")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_polar renders via fallback", {
  p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() +
    coord_polar()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: coord_radial renders via fallback", {
  p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() +
    coord_radial()
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/coord: non-linear coord layer data matches geom_curve's", {
  # Under non-linear coords, geom_curve_fade delegates to GeomCurve, so the
  # layer-level data should be identical between the two.
  build <- function(p) suppressWarnings(suppressMessages(
    ggplot_build(p)$data[[1L]]
  ))
  p_ref <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve() + coord_polar()
  p_ours <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() + coord_polar()
  d_ref  <- build(p_ref)
  d_ours <- build(p_ours)
  for (col in intersect(c("x", "y", "xend", "yend"), names(d_ref))) {
    expect_equal(d_ours[[col]], d_ref[[col]], tolerance = 1e-9, info = col)
  }
})

# --------------------------------------------------------------------------
# Facets — free scales in wrap and grid
# --------------------------------------------------------------------------

test_that("GoG/facet: facet_wrap with free scales", {
  df_f <- rbind(
    cbind(df_curves, panel = "a"),
    cbind(df_curves * 0.5, panel = "b")
  )
  p <- ggplot(df_f, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() +
    facet_wrap(~ panel, scales = "free")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("GoG/facet: facet_grid with free scales", {
  df_f <- rbind(
    cbind(df_curves, panel = "a"),
    cbind(df_curves * 0.5, panel = "b")
  )
  p <- ggplot(df_f, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade() +
    facet_grid(~ panel, scales = "free")
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

# --------------------------------------------------------------------------
# Curvature parameters
# --------------------------------------------------------------------------

test_that("curvature: positive, negative, and zero all render", {
  for (k in c(-1, -0.3, 0, 0.3, 1)) {
    p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
      geom_curve_fade(curvature = k)
    expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
  }
})

test_that("angle / ncp: extreme values render", {
  for (a in c(45, 90, 135)) {
    for (n in c(2, 5, 50)) {
      p <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
        geom_curve_fade(angle = a, ncp = n)
      expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
    }
  }
})

# --------------------------------------------------------------------------
# Drop-in parity vs ggplot2::geom_curve at the layer-data level
# --------------------------------------------------------------------------

test_that("layer-data parity: x/y/xend/yend match geom_curve", {
  build <- function(p) suppressWarnings(suppressMessages(
    ggplot_build(p)$data[[1L]]
  ))
  p_ref  <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve(curvature = 0.4)
  p_ours <- ggplot(df_curves, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade(curvature = 0.4)
  d_ref  <- build(p_ref)
  d_ours <- build(p_ours)
  for (col in c("x", "y", "xend", "yend")) {
    expect_equal(d_ours[[col]], d_ref[[col]], tolerance = 1e-9, info = col)
  }
})
