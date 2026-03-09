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
