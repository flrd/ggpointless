library(ggplot2)

# Helper: extract the first group's vertex data from a path_fade_grob.
# GeomAblineFade -> GeomSegmentFade reshapes each segment into a 2-point
# (or 3-point when fade_direction = c("start","end")) path and delegates to
# GeomPathFade. The resulting grob carries per-group vertex data:
#   $x, $y          — NPC-space coordinates of each vertex
#   $alpha_vert     — per-vertex alpha encoding the fade (0 = transparent)
.abline_fade_probe <- function(p) {
  b     <- ggplot_build(p)
  pp    <- b$layout$panel_params[[1]]
  coord <- b$layout$coord
  idx   <- which(vapply(p$layers, \(l) inherits(l$geom, "GeomAblineFade"), logical(1L)))
  data  <- b$data[[idx]]
  layer <- p$layers[[idx]]
  grob  <- GeomAblineFade$draw_panel(
    data, pp, coord,
    alpha_fade_to  = layer$geom_params$alpha_fade_to  %||% 0,
    fade_direction = layer$geom_params$fade_direction %||% "start"
  )
  grob$groups_data[[1]]
}

base_p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

# ---------------------------------------------------------------------------
# Parameter validation
# ---------------------------------------------------------------------------

test_that("geom_abline_fade: alpha_fade_to rejects out-of-range values", {
  expect_error(
    ggplotGrob(base_p + geom_abline_fade(slope = -5, intercept = 37, alpha_fade_to = 2)),
    "alpha_fade_to"
  )
  expect_error(
    ggplotGrob(base_p + geom_abline_fade(slope = -5, intercept = 37, alpha_fade_to = -0.1)),
    "alpha_fade_to"
  )
})

test_that("geom_abline_fade: alpha_fade_to accepts integer input", {
  expect_no_error(
    ggplotGrob(base_p + geom_abline_fade(slope = -5, intercept = 37, alpha_fade_to = 0L))
  )
})

test_that("geom_abline_fade: fade_direction warns and falls back for invalid values", {
  expect_warning(
    ggplotGrob(base_p + geom_abline_fade(slope = -5, intercept = 37, fade_direction = "up")),
    "fade_direction"
  )
})

# ---------------------------------------------------------------------------
# Gradient anchor points are within [0, 1] NPC (regression test for the
# range-doubling bug: previously the segment extended 2x beyond the panel,
# pushing gradient anchors to NPC ≈ -1 and 2, making the fade invisible)
# ---------------------------------------------------------------------------

test_that("geom_abline_fade: vertex coords are within panel NPC [0, 1]", {
  p    <- base_p + geom_abline_fade(intercept = 37.285126, slope = -5.344472)
  info <- .abline_fade_probe(p)
  expect_true(all(info$x >= -0.01 & info$x <= 1.01),
    label = paste("x in range, got", paste(info$x, collapse = ", ")))
  expect_true(all(info$y >= -0.01 & info$y <= 1.01),
    label = paste("y in range, got", paste(info$y, collapse = ", ")))
})

test_that("geom_abline_fade: line spans the full panel (endpoints near opposite edges)", {
  p    <- base_p + geom_abline_fade(intercept = 37.285126, slope = -5.344472)
  info <- .abline_fade_probe(p)
  # The line enters near the left edge (x[1] ≈ 0) and exits near the bottom
  # (y[last] ≈ 0). Neither endpoint should sit in the interior of the panel.
  span_x <- abs(info$x[length(info$x)] - info$x[1])
  span_y <- abs(info$y[length(info$y)] - info$y[1])
  expect_gt(span_x, 0.5)
  expect_gt(span_y, 0.5)
})

# ---------------------------------------------------------------------------
# fade_direction encodes the correct per-vertex alpha stops
# ---------------------------------------------------------------------------

test_that("geom_abline_fade: fade_direction = 'end' is opaque at start, transparent at end", {
  p    <- base_p + geom_abline_fade(intercept = 37.285126, slope = -5.344472,
                                     alpha_fade_to = 0, fade_direction = "end")
  info <- .abline_fade_probe(p)
  a    <- info$alpha_vert
  expect_equal(a[1],           1, tolerance = 0.01)  # start: fully opaque
  expect_equal(a[length(a)],   0, tolerance = 0.01)  # end:   fully transparent
})

test_that("geom_abline_fade: fade_direction = 'start' reverses the gradient", {
  p    <- base_p + geom_abline_fade(intercept = 37.285126, slope = -5.344472,
                                     alpha_fade_to = 0, fade_direction = "start")
  info <- .abline_fade_probe(p)
  a    <- info$alpha_vert
  expect_equal(a[1],           0, tolerance = 0.01)
  expect_equal(a[length(a)],   1, tolerance = 0.01)
})

test_that("geom_abline_fade: alpha_fade_to is encoded in per-vertex alpha", {
  p    <- base_p + geom_abline_fade(intercept = 37.285126, slope = -5.344472,
                                     alpha_fade_to = 0.4)
  info <- .abline_fade_probe(p)
  a    <- info$alpha_vert
  expect_equal(min(a), 0.4, tolerance = 0.01)
  expect_equal(max(a), 1.0, tolerance = 0.01)
})

# ---------------------------------------------------------------------------
# Multiple ablines
# ---------------------------------------------------------------------------

test_that("geom_abline_fade: multiple lines produce one group per line", {
  p <- base_p + geom_abline_fade(
    intercept = c(37, 30), slope = c(-5, -4)
  )
  b     <- ggplot_build(p)
  pp    <- b$layout$panel_params[[1]]
  coord <- b$layout$coord
  idx   <- which(vapply(p$layers, \(l) inherits(l$geom, "GeomAblineFade"), logical(1L)))
  data  <- b$data[[idx]]
  grob  <- GeomAblineFade$draw_panel(data, pp, coord)
  expect_equal(length(grob$groups_data), 2L)
})

# ---------------------------------------------------------------------------
# geom_hline_fade / geom_vline_fade: gradient anchors also within [0, 1]
# ---------------------------------------------------------------------------

# geom_hline_fade / geom_vline_fade set anchors directly from the panel range
# (ranges$x[1]/ranges$x[2] and ranges$y[1]/ranges$y[2]) so they are always at
# the panel edges by construction — no regression risk.  A build check suffices.
test_that("geom_hline_fade builds without error", {
  expect_no_error(ggplotGrob(base_p + geom_hline_fade(yintercept = 20)))
})

test_that("geom_vline_fade builds without error", {
  expect_no_error(ggplotGrob(base_p + geom_vline_fade(xintercept = 3)))
})

# ---------------------------------------------------------------------------
# Basic rendering: no errors in common configurations
# ---------------------------------------------------------------------------

test_that("geom_abline_fade builds without error (default identity line)", {
  expect_no_error(ggplotGrob(base_p + geom_abline_fade()))
})

test_that("geom_hline_fade builds without error", {
  expect_no_error(ggplotGrob(base_p + geom_hline_fade(yintercept = 20)))
})

test_that("geom_vline_fade builds without error", {
  expect_no_error(ggplotGrob(base_p + geom_vline_fade(xintercept = 3)))
})

test_that("geom_abline_fade: fade_direction both-ends builds without error", {
  expect_no_error(
    ggplotGrob(base_p + geom_abline_fade(slope = -5, intercept = 37,
                                          fade_direction = c("start", "end")))
  )
})


# ===========================================================================
# Grammar of Graphics adversarial stress tests
# ===========================================================================

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

test_that("GoG/data: empty dataset with abline_fade does not error", {
  p <- ggplot(data.frame(x = numeric(), y = numeric()), aes(x, y)) +
    geom_abline_fade(slope = 1, intercept = 0)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: single-point dataset with hline_fade does not error", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_point() + geom_hline_fade(yintercept = 1)
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/data: vline_fade with all-NA data does not error", {
  p <- ggplot(data.frame(x = 1:3, y = NA_real_), aes(x, y)) +
    geom_vline_fade(xintercept = 2)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Mapping
# ---------------------------------------------------------------------------

test_that("GoG/mapping: inherit.aes = FALSE by default (no data leakage)", {
  # abline_fade uses its own data — plot aes should not interfere
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point() + geom_abline_fade(slope = -5, intercept = 37)
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Layer
# ---------------------------------------------------------------------------

test_that("GoG/layer: multiple reference line layers do not error", {
  p <- base_p +
    geom_hline_fade(yintercept = 20, colour = "red") +
    geom_vline_fade(xintercept = 3, colour = "blue") +
    geom_abline_fade(slope = -5, intercept = 37, colour = "green")
  expect_no_error(ggplotGrob(p))
})


# ---------------------------------------------------------------------------
# Scales
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_y_log10 with hline_fade does not error", {
  p <- ggplot(data.frame(x = 1:5, y = c(1, 10, 100, 10, 1)), aes(x, y)) +
    geom_point() + geom_hline_fade(yintercept = 10) + scale_y_log10()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/scales: scale_y_reverse negates hline_fade yintercept in built data", {
  b_fwd <- ggplot_build(base_p + geom_hline_fade(yintercept = 20))
  b_rev <- ggplot_build(base_p + geom_hline_fade(yintercept = 20) + scale_y_reverse())
  expect_equal(b_rev$data[[2]]$y[1], -b_fwd$data[[2]]$y[1])
})

test_that("GoG/scales: scale_x_reverse negates x positions of all layers (abline_fade)", {
  b_fwd <- ggplot_build(base_p + geom_abline_fade(intercept = 37, slope = -5))
  b_rev <- ggplot_build(base_p + geom_abline_fade(intercept = 37, slope = -5) +
                          scale_x_reverse())
  expect_equal(b_rev$data[[1]]$x, -b_fwd$data[[1]]$x)
})

test_that("GoG/scales: scale_x_reverse does not shift hline_fade yintercept", {
  b_fwd <- ggplot_build(base_p + geom_hline_fade(yintercept = 20))
  b_rev <- ggplot_build(base_p + geom_hline_fade(yintercept = 20) + scale_x_reverse())
  expect_equal(b_rev$data[[2]]$y[1], b_fwd$data[[2]]$y[1])
})

test_that("GoG/scales: scale_x_reverse negates vline_fade xintercept in built data", {
  b_fwd <- ggplot_build(base_p + geom_vline_fade(xintercept = 3))
  b_rev <- ggplot_build(base_p + geom_vline_fade(xintercept = 3) + scale_x_reverse())
  expect_equal(b_rev$data[[2]]$x[1], -b_fwd$data[[2]]$x[1])
})

test_that("GoG/scales: explicit limits do not error", {
  p <- base_p + geom_hline_fade(yintercept = 20) +
    scale_y_continuous(limits = c(0, 50))
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Coord
# ---------------------------------------------------------------------------

test_that("GoG/coord: coord_cartesian zoom does not error", {
  p <- base_p + geom_abline_fade(slope = -5, intercept = 37) +
    coord_cartesian(ylim = c(10, 30))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_flip does not error", {
  p <- base_p + geom_hline_fade(yintercept = 20) + coord_flip()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_fixed does not error", {
  p <- base_p + geom_hline_fade(yintercept = 20) + coord_fixed()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_polar does not error", {
  p <- base_p + geom_hline_fade(yintercept = 20) + coord_polar()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/coord: coord_radial does not error (hline / vline / abline)", {
  expect_no_error(suppressWarnings(ggplotGrob(
    base_p + geom_hline_fade(yintercept = 20) + coord_radial()
  )))
  expect_no_error(suppressWarnings(ggplotGrob(
    base_p + geom_vline_fade(xintercept = 3) + coord_radial()
  )))
  expect_no_error(suppressWarnings(ggplotGrob(
    base_p + geom_abline_fade(slope = -5, intercept = 37) + coord_radial()
  )))
})

test_that("coord_radial subdivides the line so the path has many vertices", {
  # Regression: linear coord produces a 2-vertex group; polar must subdivide
  # (>> 2 vertices) so the fade follows the curve rather than a chord.
  p <- base_p + geom_hline_fade(yintercept = 30) + coord_radial()
  b <- ggplot_build(p)
  pp <- b$layout$panel_params[[1]]
  co <- b$layout$coord
  idx <- which(vapply(p$layers, \(l) inherits(l$geom, "GeomHlineFade"), logical(1L)))
  data <- b$data[[idx]]
  grob <- GeomHlineFade$draw_panel(
    data, pp, co,
    alpha_fade_to = 0, fade_direction = "start"
  )
  # Dense subdivision (>> 2 vertices) confirms we took the non-linear branch.
  expect_gt(length(grob$groups_data[[1]]$x), 10L)
})

# ---------------------------------------------------------------------------
# Facets
# ---------------------------------------------------------------------------

test_that("GoG/facets: facet_wrap with free scales does not error", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    geom_hline_fade(yintercept = 20) +
    facet_wrap(~cyl, scales = "free")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/facets: facet_grid does not error", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() +
    geom_hline_fade(yintercept = 20) +
    facet_grid(~cyl)
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------




test_that("GoG/theme: element_line colour propagates without error", {
  p <- base_p +
    geom_hline_fade(yintercept = 20) +
    theme(line = element_line(colour = "red", linewidth = 2))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: element_blank on line does not crash", {
  p <- base_p +
    geom_hline_fade(yintercept = 20) +
    theme(line = element_blank())
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Scales (additional)
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_y_sqrt with hline_fade does not error", {
  p <- ggplot(data.frame(x = 1:5, y = c(1, 4, 9, 16, 25)), aes(x, y)) +
    geom_point() + geom_hline_fade(yintercept = 9) + scale_y_sqrt()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/scales: secondary axis with hline_fade does not error", {
  p <- base_p +
    geom_hline_fade(yintercept = 20) +
    scale_y_continuous(sec.axis = sec_axis(~ . * 1.6, name = "km/L"))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: NULL breaks with vline_fade does not error", {
  p <- base_p +
    geom_vline_fade(xintercept = 3) +
    scale_x_continuous(breaks = NULL)
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: expand = expansion(0) with abline_fade does not error", {
  p <- base_p +
    geom_abline_fade(slope = -5, intercept = 37) +
    scale_x_continuous(expand = expansion(0))
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# slope = 0 guard
# ---------------------------------------------------------------------------

test_that("geom_abline_fade: slope = 0 (intercept within range) does not warn or error", {
  p <- base_p + geom_abline_fade(slope = 0, intercept = 20)
  expect_no_warning(expect_no_error(ggplotGrob(p)))
})

test_that("geom_abline_fade: slope = 0 (intercept outside range) does not warn or error", {
  p <- base_p + geom_abline_fade(slope = 0, intercept = 100)
  expect_no_warning(expect_no_error(ggplotGrob(p)))
})

test_that("geom_abline_fade: slope = 0 spans full panel width", {
  p   <- base_p + geom_abline_fade(slope = 0, intercept = 20)
  info <- .abline_fade_probe(p)
  expect_equal(info$x[1],              0, tolerance = 0.01)
  expect_equal(info$x[length(info$x)], 1, tolerance = 0.01)
})

# ---------------------------------------------------------------------------
# na.rm forwarding
# ---------------------------------------------------------------------------

test_that("geom_abline_fade: na.rm = TRUE does not error", {
  p <- base_p + geom_abline_fade(slope = -5, intercept = 37, na.rm = TRUE)
  expect_no_error(ggplotGrob(p))
})

test_that("geom_hline_fade: na.rm = TRUE does not error", {
  p <- base_p + geom_hline_fade(yintercept = 20, na.rm = TRUE)
  expect_no_error(ggplotGrob(p))
})

test_that("geom_vline_fade: na.rm = TRUE does not error", {
  p <- base_p + geom_vline_fade(xintercept = 3, na.rm = TRUE)
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Visual regression (vdiffr)
# ---------------------------------------------------------------------------

test_that("vdiffr: geom_hline_fade default appearance", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    geom_hline_fade(yintercept = 20, linewidth = 1.5) +
    theme_minimal()
  vdiffr::expect_doppelganger("hline-fade-default", p)
})

test_that("vdiffr: geom_vline_fade both ends fading", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    geom_vline_fade(xintercept = 3, linewidth = 1.5,
                    fade_direction = c("start", "end")) +
    theme_minimal()
  vdiffr::expect_doppelganger("vline-fade-both-ends", p)
})

test_that("vdiffr: geom_hline_fade under coord_radial", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    geom_hline_fade(yintercept = 30, colour = "tomato", linewidth = 1.5) +
    coord_radial() +
    theme_minimal()
  vdiffr::expect_doppelganger("hline-fade-coord-radial", p)
})

test_that("vdiffr: geom_vline_fade under coord_radial", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    geom_vline_fade(xintercept = 5, linewidth = 1.5) +
    coord_radial() +
    theme_minimal()
  vdiffr::expect_doppelganger("vline-fade-coord-radial", p)
})

test_that("vdiffr: geom_abline_fade under coord_radial", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    geom_abline_fade(slope = -5, intercept = 37, linewidth = 1.5) +
    coord_radial() +
    theme_minimal()
  vdiffr::expect_doppelganger("abline-fade-coord-radial", p)
})

test_that("vdiffr: geom_vline_fade under coord_radial (bare defaults)", {
  skip_if_not_installed("vdiffr")
  # User-reported example — keeps default linewidth / colour / theme so the
  # snapshot acts as a regression on the bare-bones render path.
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    geom_vline_fade(xintercept = 5) +
    coord_radial()
  vdiffr::expect_doppelganger("vline-fade-coord-radial-defaults", p)
})

test_that("vdiffr: all three reference-line fades together under coord_radial", {
  skip_if_not_installed("vdiffr")
  # Note: ggplot2::geom_abline() draws a straight line under coord_radial()
  # (it does not follow the coord curve). Our geom_abline_fade() subdivides
  # in data space so the line bends with the coord — this snapshot pins that
  # behaviour.
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    geom_abline_fade(slope = 1, colour = "black") +
    geom_hline_fade(yintercept = 30, colour = "forestgreen") +
    geom_vline_fade(xintercept = 5, colour = "tomato") +
    coord_radial()
  vdiffr::expect_doppelganger("refline-fades-coord-radial-combined", p)
})

test_that("vdiffr: geom_abline_fade with alpha_fade_to", {
  skip_if_not_installed("vdiffr")
  coefs <- coef(lm(mpg ~ wt, data = mtcars))
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    geom_abline_fade(intercept = coefs[1], slope = coefs[2],
                     linewidth = 1.5, alpha_fade_to = 0.4) +
    theme_minimal()
  vdiffr::expect_doppelganger("abline-fade-alpha-fade-to", p)
})
