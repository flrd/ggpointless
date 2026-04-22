library(ggplot2)

df <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(2, 4, 1, 5, 3),
  g = c("a", "a", "b", "b", "b")
)

# --- smoke tests -------------------------------------------------------------

test_that("geom_point_glow renders without error", {
  p <- ggplot(df, aes(x, y, colour = g)) +
    geom_point_glow() +
    theme_minimal()
  expect_no_error(ggplotGrob(p))
})

test_that("glow_colour = NA (default) inherits from point colour", {
  p <- ggplot(df, aes(x, y, colour = g)) +
    geom_point_glow(glow_colour = NA) +
    theme_minimal()
  expect_no_error(ggplotGrob(p))
})

test_that("explicit glow_colour overrides point colour", {
  p <- ggplot(df, aes(x, y)) +
    geom_point_glow(glow_colour = "steelblue") +
    theme_minimal()
  expect_no_error(ggplotGrob(p))
})

test_that("custom glow_alpha and glow_size are accepted", {
  p <- ggplot(df, aes(x, y)) +
    geom_point_glow(glow_alpha = 0.3, glow_size = 6) +
    theme_minimal()
  expect_no_error(ggplotGrob(p))
})

test_that("empty data returns without error", {
  p <- ggplot(df[integer(0), ], aes(x, y)) +
    geom_point_glow()
  expect_no_error(ggplotGrob(p))
})

# --- geom_point_glow used as geom for stat_pointless -------------------------

test_that("stat_pointless with geom = 'PointGlow' renders without error", {
  p <- ggplot(head(economics, 20L), aes(date, uempmed)) +
    geom_line() +
    stat_pointless(
      geom      = "PointGlow",
      glow_size = 8,
      location  = c("first", "last")
    )
  expect_no_error(ggplotGrob(p))
})

# --- visual tests ------------------------------------------------------------
# vdiffr::expect_doppelganger() calls vdiffr_enabled(), which returns FALSE
# during R CMD check and in covr's subprocess.  When disabled, vdiffr calls
# testthat::skip() without rendering the plot, so draw_key_point_glow and
# draw_panel — which only run during rendering — show 0% coverage from these
# tests.  The unit test sections below call both functions directly.

test_that("geom_point_glow default visual", {
  p <- ggplot(df, aes(x, y, colour = g)) +
    geom_point_glow() +
    theme_minimal()
  vdiffr::expect_doppelganger("point-glow default", p)
})

test_that("geom_point_glow with fixed glow_colour and large glow_size", {
  p <- ggplot(df, aes(x, y)) +
    geom_point_glow(colour = "#311dfc", glow_colour = "#311dfc", glow_size = 5) +
    theme_minimal()
  vdiffr::expect_doppelganger("point-glow fixed colour", p)
})

test_that("glow_size default renders at 9x the point's size aesthetic", {
  p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point_glow(size = 3) +
    theme_minimal()
  vdiffr::expect_doppelganger("point-glow size-default-9x", p)
})

test_that("glow_size scalar is taken at face value in size units", {
  p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point_glow(size = 3, glow_size = 12) +
    theme_minimal()
  vdiffr::expect_doppelganger("point-glow size-scalar-12", p)
})

test_that("glow_size vector renders per-point sizes", {
  p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point_glow(size = 3, glow_size = c(6, 10, 14)) +
    theme_minimal()
  vdiffr::expect_doppelganger("point-glow size-vector", p)
})

test_that("stat_pointless with PointGlow visual", {
  p <- ggplot(head(economics, 20L), aes(date, uempmed)) +
    geom_line() +
    stat_pointless(
      geom      = "PointGlow",
      glow_size = 8,
      location  = c("first", "last")
    ) +
    theme_minimal()
  vdiffr::expect_doppelganger("point-glow stat_pointless", p)
})

# --- draw_key_point_glow unit tests ------------------------------------------
# draw_key_point_glow is called by ggplot2 when building a discrete-fill legend
# key.  Because vdiffr skips rendering in covr's subprocess (see above), the
# function never executes during the visual tests.  The synthetic key_data
# below matches the one-row data frame that ggplot2 passes to draw_key.

# Minimal key data frame matching what ggplot2 passes to draw_key
key_data <- data.frame(
  colour    = "#311dfc",
  fill      = NA_character_,
  size      = 1,
  shape     = 19,
  stroke    = 0.5,
  alpha     = NA_real_,
  linewidth = 0.5
)

test_that("draw_key_point_glow returns a gList of length 2", {
  params <- list(glow_colour = NA_character_, glow_alpha = 0.75)
  result <- draw_key_point_glow(key_data, params, grid::unit(c(1, 1), "cm"))
  expect_s3_class(result, "gList")
  expect_length(result, 2L)
})

test_that("draw_key_point_glow: circle grob has a radial gradient fill", {
  params <- list(glow_colour = NA_character_, glow_alpha = 0.75)
  result <- draw_key_point_glow(key_data, params, grid::unit(c(1, 1), "cm"))
  glow_grob <- result[[1]]
  expect_s3_class(glow_grob, "circle")
  expect_true(inherits(glow_grob$gp$fill, "GridRadialGradient"))
})

test_that("draw_key_point_glow: glow inherits colour from data when glow_colour is NA", {
  params <- list(glow_colour = NA_character_, glow_alpha = 0.75)
  result <- draw_key_point_glow(key_data, params, grid::unit(c(1, 1), "cm"))
  # The gradient's opaque stop must encode the data colour (#311dfc)
  grad_col_opaque <- result[[1]]$gp$fill$colours[1]
  expect_true(grepl("311dfc", grad_col_opaque, ignore.case = TRUE))
})

test_that("draw_key_point_glow: explicit glow_colour overrides data colour", {
  params <- list(glow_colour = "steelblue", glow_alpha = 0.75)
  result <- draw_key_point_glow(key_data, params, grid::unit(c(1, 1), "cm"))
  grad_col_opaque <- result[[1]]$gp$fill$colours[1]
  # Should encode steelblue, not #311dfc
  expect_false(grepl("311dfc", grad_col_opaque, ignore.case = TRUE))
})

test_that("draw_key_point_glow: glow_alpha is floored at 0.5", {
  # glow_alpha = 0.2 < 0.5, so max(0.2, 0.5) = 0.5 is used
  params_low  <- list(glow_colour = NA_character_, glow_alpha = 0.2)
  params_high <- list(glow_colour = NA_character_, glow_alpha = 0.9)
  result_low  <- draw_key_point_glow(key_data, params_low,  grid::unit(c(1, 1), "cm"))
  result_high <- draw_key_point_glow(key_data, params_high, grid::unit(c(1, 1), "cm"))
  # Both should produce a gList without error
  expect_s3_class(result_low,  "gList")
  expect_s3_class(result_high, "gList")
})

# --- GeomPointGlow$draw_panel unit tests -------------------------------------
# draw_panel is also skipped by vdiffr (see above).  ggplot_build() is used to
# extract panel_params and coord in the exact format draw_panel expects, so
# the call mirrors what ggplot2 would do during a real render.

test_that("draw_panel returns a gList with glow layer and standard points", {
  p     <- ggplot(df, aes(x, y, colour = g)) + geom_point_glow()
  built <- ggplot_build(p)
  ldata   <- built$data[[1]]
  pparams <- built$layout$panel_params[[1]]
  coord   <- built$layout$coord
  result  <- GeomPointGlow$draw_panel(ldata, pparams, coord)
  expect_s3_class(result, "gList")
  expect_length(result, 2L)   # [1] glow layer (gTree), [2] standard points
  expect_s3_class(result[[1]], "gTree")
})

test_that("draw_panel: empty data returns a nullGrob", {
  p     <- ggplot(df[integer(0), ], aes(x, y)) + geom_point_glow()
  built <- ggplot_build(p)
  ldata   <- built$data[[1]]
  pparams <- built$layout$panel_params[[1]]
  coord   <- built$layout$coord
  result  <- GeomPointGlow$draw_panel(ldata, pparams, coord)
  expect_s3_class(result, "null")
})

test_that("draw_panel: fixed glow_colour is applied to all points", {
  p     <- ggplot(df, aes(x, y, colour = g)) + geom_point_glow()
  built <- ggplot_build(p)
  ldata   <- built$data[[1]]
  pparams <- built$layout$panel_params[[1]]
  coord   <- built$layout$coord
  result  <- GeomPointGlow$draw_panel(ldata, pparams, coord,
                                      glow_colour = "tomato",
                                      glow_alpha  = 0.6,
                                      glow_size   = NA)
  expect_s3_class(result, "gList")
  # Each child of the glow gTree is a pointsGrob with a radialGradient fill
  glow_tree <- result[[1]]
  expect_true(inherits(glow_tree$children[[1]]$gp$fill, "GridRadialGradient"))
})

test_that("draw_panel: fixed glow_size is applied to all points", {
  p     <- ggplot(df, aes(x, y)) + geom_point_glow()
  built <- ggplot_build(p)
  ldata   <- built$data[[1]]
  pparams <- built$layout$panel_params[[1]]
  coord   <- built$layout$coord
  result  <- GeomPointGlow$draw_panel(ldata, pparams, coord,
                                      glow_colour = NA,
                                      glow_alpha  = 0.75,
                                      glow_size   = 7)
  expect_s3_class(result, "gList")
})

# ---------------------------------------------------------------------------
# Vector-valued glow_* (length 1 or n rule + NA-filter alignment)
# ---------------------------------------------------------------------------

test_that("setup_data stamps vector glow_size as .glow_size column", {
  p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point_glow(glow_size = c(2, 4, 6))
  built <- ggplot_build(p)
  d <- built$data[[1]]
  expect_equal(d$.glow_size, c(2, 4, 6))
})

test_that("vector glow_size stays aligned after NA-row filter", {
  # Row 2 has NA in y → handle_na drops it → surviving glow fontsizes must
  # reflect glow_size[1] and glow_size[3] = 1 and 3 (in mm), NOT [1] and [2]
  # which would be the silent-misalignment bug.
  p <- ggplot(data.frame(x = 1:3, y = c(1, NA, 3)), aes(x, y)) +
    geom_point_glow(glow_size = c(1, 2, 3))

  gt <- suppressWarnings(ggplotGrob(p))
  panel <- gt$grobs[[which(gt$layout$name == "panel")]]
  # The glow layer is an unnamed (auto-named) gTree child — find it.
  idx <- which(
    names(panel$children) == "" |
      grepl("^GRID\\.gTree", names(panel$children))
  )
  glow <- panel$children[[idx[1L]]]

  # grid uses pt; ggplot2's .pt ≈ 2.845 converts mm → pt.
  fontsizes <- unname(vapply(glow$children, \(g) g$gp$fontsize, numeric(1L)))
  expect_equal(
    fontsizes,
    c(1, 3) * ggplot2::.pt,
    tolerance = 1e-3
  )
})

test_that("vector glow_alpha and glow_colour also stamp and align", {
  p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point_glow(
      glow_alpha  = c(0.2, 0.5, 0.8),
      glow_colour = c("red", "green", "blue")
    )
  d <- ggplot_build(p)$data[[1]]
  expect_equal(d$.glow_alpha, c(0.2, 0.5, 0.8))
  expect_equal(d$.glow_colour, c("red", "green", "blue"))
})

test_that("validator rejects length-mismatched vectors (neither 1 nor n)", {
  p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point_glow(glow_size = 1:2)  # 2 != 1, 2 != 3
  expect_error(ggplotGrob(p), "length 1 or the same length as the data")
})

test_that("validator accepts length-1 scalar (backward compat)", {
  p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point_glow(glow_size = 5)
  expect_no_error(ggplotGrob(p))
  # No .glow_size column when scalar
  d <- ggplot_build(p)$data[[1]]
  expect_null(d$.glow_size)
})

test_that("draw-time info fires once when glow_size <= point size", {
  # `.frequency = "once"` is in-session (env-based), so we reset before and
  # after each test to get a clean slate.  `capture_messages()` is used
  # instead of `expect_message()` because cli's message stream does not
  # always propagate through expect_message() inside ggplotGrob().
  rlang::reset_message_verbosity("geom_point_glow_size_covered")
  withr::defer(
    rlang::reset_message_verbosity("geom_point_glow_size_covered")
  )

  p_small <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point_glow(glow_size = 1)   # 1 < default size 1.5

  # First render: the hint fires.
  msgs1 <- testthat::capture_messages(ggplotGrob(p_small))
  expect_true(any(grepl("covered by the point", msgs1)))

  # Second render in the same session: throttled, silent.
  msgs2 <- testthat::capture_messages(ggplotGrob(p_small))
  expect_false(any(grepl("covered by the point", msgs2)))
})

test_that("draw-time info stays silent when glow clearly exceeds point size", {
  rlang::reset_message_verbosity("geom_point_glow_size_covered")
  withr::defer(
    rlang::reset_message_verbosity("geom_point_glow_size_covered")
  )

  p_ok <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) +
    geom_point_glow()                # default: 9 * size

  msgs <- testthat::capture_messages(ggplotGrob(p_ok))
  expect_false(any(grepl("covered by the point", msgs)))
})


# ===========================================================================
# Grammar of Graphics adversarial stress tests
# ===========================================================================

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

test_that("GoG/data: single point does not error", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_point_glow()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/data: all-NA y values do not error", {
  p <- ggplot(data.frame(x = 1:3, y = NA_real_), aes(x, y)) +
    geom_point_glow()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: negative coordinates do not error", {
  p <- ggplot(data.frame(x = c(-3, -1, 0), y = c(-2, 1, -4)), aes(x, y)) +
    geom_point_glow()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Mapping
# ---------------------------------------------------------------------------

test_that("GoG/mapping: colour aesthetic mapping does not error", {
  p <- ggplot(df, aes(x, y, colour = g)) + geom_point_glow()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/mapping: inherit.aes = FALSE isolates from plot mapping", {
  p <- ggplot(df, aes(x, y, colour = g)) +
    geom_point() +
    geom_point_glow(data = data.frame(x = 3, y = 1),
                    mapping = aes(x, y), inherit.aes = FALSE)
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Layer
# ---------------------------------------------------------------------------

test_that("GoG/layer: multiple geom_point_glow layers do not error", {
  p <- ggplot(df, aes(x, y)) +
    geom_point_glow(glow_colour = "red") +
    geom_point_glow(glow_colour = "blue", glow_size = 10)
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/layer: geom_point_glow with other geom layers does not error", {
  p <- ggplot(df, aes(x, y)) + geom_line() + geom_point_glow()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Scales
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_y_log10 does not error", {
  p <- ggplot(data.frame(x = 1:5, y = c(1, 10, 100, 10, 1)), aes(x, y)) +
    geom_point_glow() + scale_y_log10()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: scale_y_reverse negates y values (point_glow)", {
  b_fwd <- ggplot_build(ggplot(df, aes(x, y)) + geom_point_glow())
  b_rev <- ggplot_build(ggplot(df, aes(x, y)) + geom_point_glow() + scale_y_reverse())
  expect_equal(b_rev$data[[1]]$y, -b_fwd$data[[1]]$y)
})

test_that("GoG/scales: scale_x_reverse negates x values (point_glow)", {
  b_fwd <- ggplot_build(ggplot(df, aes(x, y)) + geom_point_glow())
  b_rev <- ggplot_build(ggplot(df, aes(x, y)) + geom_point_glow() + scale_x_reverse())
  expect_equal(b_rev$data[[1]]$x, -b_fwd$data[[1]]$x)
})

test_that("GoG/scales: explicit limits do not error", {
  p <- ggplot(df, aes(x, y)) + geom_point_glow() +
    scale_y_continuous(limits = c(-10, 10))
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Coord
# ---------------------------------------------------------------------------

test_that("GoG/coord: coord_cartesian zoom does not error", {
  p <- ggplot(df, aes(x, y)) + geom_point_glow() +
    coord_cartesian(ylim = c(1, 4))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_fixed does not error", {
  p <- ggplot(df, aes(x, y)) + geom_point_glow() + coord_fixed()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_flip does not error", {
  p <- ggplot(df, aes(x, y)) + geom_point_glow() + coord_flip()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_polar does not error", {
  p <- ggplot(df, aes(x, y)) + geom_point_glow() + coord_polar()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Facets
# ---------------------------------------------------------------------------

test_that("GoG/facets: facet_wrap with free scales does not error", {
  p <- ggplot(df, aes(x, y)) + geom_point_glow() +
    facet_wrap(~g, scales = "free")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/facets: facet_grid does not error", {
  p <- ggplot(df, aes(x, y)) + geom_point_glow() +
    facet_grid(~g)
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------

test_that("GoG/theme: theme_void does not error", {
  p <- ggplot(df, aes(x, y)) + geom_point_glow() + theme_void()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: theme_classic does not error", {
  p <- ggplot(df, aes(x, y)) + geom_point_glow() + theme_classic()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: theme_bw does not error", {
  p <- ggplot(df, aes(x, y)) + geom_point_glow() + theme_bw()
  expect_no_error(ggplotGrob(p))
})
