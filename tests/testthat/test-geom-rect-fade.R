library(ggplot2)

# -----------------------------------------------------------------------
# Shared test data
# -----------------------------------------------------------------------

df_corners <- data.frame(
  xmin = c(0, 5),
  xmax = c(4, 9),
  ymin = c(0, 2),
  ymax = c(3, 7)
)

df_tile <- data.frame(
  x      = c(2, 7),
  y      = c(1.5, 4.5),
  width  = c(4, 4),
  height = c(3, 5)
)

df_tile_single <- data.frame(x = 50, y = 50, width = 50, height = 50)

# Helper: build a plot and extract the rect_fade_grob from draw_panel.
build_rect_grob <- function(p) {
  b     <- ggplot_build(p)
  ldata <- b$data[[1]]
  pp    <- b$layout$panel_params[[1]]
  coord <- b$layout$coord
  gp    <- p$layers[[1]]$geom_params
  GeomRectFade$draw_panel(
    ldata, pp, coord,
    alpha_fade_to  = gp$alpha_fade_to  %||% 0,
    fade_direction = gp$fade_direction %||% "vertical",
    radius         = gp$radius         %||% grid::unit(0, "pt")
  )
}

# Decode alpha channel from a colour vector.
col_alpha <- function(cols) {
  grDevices::col2rgb(cols, alpha = TRUE)["alpha", ] / 255
}


# -----------------------------------------------------------------------
# setup_data: width/height -> corner conversion
# -----------------------------------------------------------------------

test_that("setup_data converts (x, y, width, height) for single-row input", {
  d   <- data.frame(x = 50, y = 50, width = 50, height = 50)
  out <- GeomRectFade$setup_data(d, list())
  expect_equal(out$xmin, 25)
  expect_equal(out$xmax, 75)
  expect_equal(out$ymin, 25)
  expect_equal(out$ymax, 75)
})

test_that("setup_data converts (x, y, width, height) for multi-row input", {
  d   <- data.frame(x = c(10, 30), y = c(10, 30), width = 10, height = 10)
  out <- GeomRectFade$setup_data(d, list())
  expect_equal(out$xmin, c(5, 25))
  expect_equal(out$xmax, c(15, 35))
  expect_equal(out$ymin, c(5, 25))
  expect_equal(out$ymax, c(15, 35))
})

test_that("setup_data leaves explicit corners unchanged", {
  d   <- data.frame(xmin = 1, xmax = 4, ymin = 2, ymax = 5)
  out <- GeomRectFade$setup_data(d, list())
  expect_equal(out$xmin, 1)
  expect_equal(out$xmax, 4)
  expect_equal(out$ymin, 2)
  expect_equal(out$ymax, 5)
})

test_that("aes(width, height) single-row no longer crashes", {
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(data = df_tile_single, aes(width = width, height = height))
  expect_no_error(ggplot_build(p))
})

test_that("aes(width, height) multi-row renders correctly", {
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(data = df_tile, aes(width = width, height = height))
  expect_no_error(ggplotGrob(p))
})

test_that("aes(width, height) and explicit corners produce same grob structure", {
  # Both paths should produce a rect_fade_grob with the same number of rects.
  p_tile <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(data = df_tile, aes(width = width, height = height))
  p_corners <- ggplot() +
    geom_rect_fade(
      data = df_corners,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE
    )
  grob_tile    <- suppressWarnings(build_rect_grob(p_tile))
  grob_corners <- suppressWarnings(build_rect_grob(p_corners))
  expect_s3_class(grob_tile,    "rect_fade_grob")
  expect_s3_class(grob_corners, "rect_fade_grob")
  expect_equal(length(grob_tile$gradient_glist),    nrow(df_tile))
  expect_equal(length(grob_corners$gradient_glist), nrow(df_corners))
})


# -----------------------------------------------------------------------
# Validation
# -----------------------------------------------------------------------

test_that("alpha_fade_to must be a finite scalar in [0, 1]", {
  p <- ggplot() +
    geom_rect_fade(
      data = df_corners,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE
    )
  expect_error(ggplotGrob(p + geom_rect_fade(
    data = df_corners,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    alpha_fade_to = -0.1, inherit.aes = FALSE
  )), "alpha_fade_to")
  expect_error(ggplotGrob(ggplot() + geom_rect_fade(
    data = df_corners,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    alpha_fade_to = 1.1, inherit.aes = FALSE
  )), "alpha_fade_to")
  expect_error(ggplotGrob(ggplot() + geom_rect_fade(
    data = df_corners,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    alpha_fade_to = NA_real_, inherit.aes = FALSE
  )), "alpha_fade_to")
})

test_that("alpha_fade_to boundary values 0 and 1 are accepted", {
  rect_layer <- function(v) geom_rect_fade(
    data = df_corners,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    alpha_fade_to = v, inherit.aes = FALSE
  )
  expect_no_error(ggplotGrob(ggplot() + rect_layer(0)))
  expect_no_error(ggplotGrob(ggplot() + rect_layer(1)))
})

test_that("fade_direction rejects invalid values", {
  expect_error(
    ggplotGrob(ggplot() + geom_rect_fade(
      data = df_corners,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fade_direction = "diagonal", inherit.aes = FALSE
    )),
    "fade_direction"
  )
})

test_that("radius as bare number is coerced to unit", {
  expect_no_error(
    ggplotGrob(ggplot() + geom_rect_fade(
      data = df_corners,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      radius = 5, inherit.aes = FALSE
    ))
  )
})


# -----------------------------------------------------------------------
# Grob structure
# -----------------------------------------------------------------------

test_that("empty data returns zeroGrob", {
  grob <- GeomRectFade$draw_panel(
    data.frame(),
    list(),
    ggplot2::coord_cartesian()
  )
  expect_true(grid::is.grob(grob))
  expect_s3_class(grob, "zeroGrob")
})

test_that("normal data produces a rect_fade_grob with correct list lengths", {
  p    <- ggplot() + geom_rect_fade(
    data = df_corners,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE
  )
  grob <- build_rect_grob(p)
  expect_s3_class(grob, "rect_fade_grob")
  expect_equal(length(grob$gradient_glist), nrow(df_corners))
  expect_equal(length(grob$flat_glist),     nrow(df_corners))
})

test_that("each gradient fill is a GridLinearGradient", {
  p    <- ggplot() + geom_rect_fade(
    data = df_corners,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE
  )
  grob <- build_rect_grob(p)
  for (i in seq_along(grob$gradient_glist)) {
    fill <- grob$gradient_glist[[i]]$gp$fill
    expect_true(
      inherits(fill, "GridLinearGradient"),
      info = paste0("rect ", i)
    )
  }
})

test_that("flat fill is a plain colour (not a gradient)", {
  p    <- ggplot() + geom_rect_fade(
    data = df_corners,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE
  )
  grob <- build_rect_grob(p)
  for (i in seq_along(grob$flat_glist)) {
    fill <- grob$flat_glist[[i]]$gp$fill
    expect_false(inherits(fill, "GridLinearGradient"), info = paste0("rect ", i))
  }
})


# -----------------------------------------------------------------------
# Gradient direction and alpha values
# -----------------------------------------------------------------------

test_that("vertical fade: first colour (y=0 end) is more transparent than second", {
  p    <- ggplot() + geom_rect_fade(
    data = df_corners,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    alpha_fade_to = 0, inherit.aes = FALSE, fade_direction = "vertical"
  )
  grob <- build_rect_grob(p)
  for (i in seq_along(grob$gradient_glist)) {
    cols   <- grob$gradient_glist[[i]]$gp$fill$colours
    alphas <- col_alpha(cols)
    expect_lt(alphas[1], alphas[2], label = paste0("rect ", i, " bottom < top"))
  }
})

test_that("horizontal fade: xmin end (colours[1]) is opaque, xmax end is transparent", {
  # linearGradient(x1=0, x2=1): colours[1] at x=0 side (xmin → opaque),
  # colours[2] at x=1 side (xmax → transparent).
  p    <- ggplot() + geom_rect_fade(
    data = df_corners,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    alpha_fade_to = 0, inherit.aes = FALSE, fade_direction = "horizontal"
  )
  grob <- build_rect_grob(p)
  for (i in seq_along(grob$gradient_glist)) {
    cols   <- grob$gradient_glist[[i]]$gp$fill$colours
    alphas <- col_alpha(cols)
    expect_gt(alphas[1], alphas[2], label = paste0("rect ", i, " xmin opaque > xmax transparent"))
  }
})

test_that("alpha_fade_to = 1 produces two stops with equal alpha", {
  p    <- ggplot() + geom_rect_fade(
    data = df_corners,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    alpha_fade_to = 1, inherit.aes = FALSE
  )
  grob <- build_rect_grob(p)
  for (i in seq_along(grob$gradient_glist)) {
    cols   <- grob$gradient_glist[[i]]$gp$fill$colours
    alphas <- col_alpha(cols)
    expect_equal(alphas[1], alphas[2], tolerance = 0.01,
                 label = paste0("rect ", i, " both stops equal"))
  }
})

test_that("alpha aes scales peak alpha proportionally", {
  df  <- data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  p   <- ggplot() + geom_rect_fade(
    data = df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    alpha = 0.5, alpha_fade_to = 0, inherit.aes = FALSE
  )
  grob   <- build_rect_grob(p)
  cols   <- grob$gradient_glist[[1]]$gp$fill$colours
  alphas <- col_alpha(cols)
  expect_equal(alphas[2], 0.5, tolerance = 0.01)
  expect_equal(alphas[1], 0,   tolerance = 0.01)
})


# -----------------------------------------------------------------------
# Non-linear coord fallback
# -----------------------------------------------------------------------

test_that("coord_transform (non-polar, non-linear) falls back to GeomRect", {
  # Non-polar non-linear coords (e.g. coord_transform with a log transform)
  # can't express a linear gradient through a transformed viewport, so we
  # drop back to plain geom_rect — neither a rect_fade_grob nor a polar one.
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(
      data = df_tile_single, aes(width = width, height = height)
    ) +
    coord_transform(x = "log10")
  grob <- suppressMessages(build_rect_grob(p))
  expect_false(inherits(grob, "rect_fade_grob"))
  expect_false(inherits(grob, "rect_fade_polar_grob"))
})

test_that("coord_polar default (theta='x', vertical) routes to polar grob", {
  # theta = "x" + fade_direction = "vertical" → radial gradient (ymin inner
  # ring fades, ymax outer ring opaque). Rendering is deferred to
  # makeContent.rect_fade_polar_grob.
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(
      data = df_tile_single, aes(width = width, height = height)
    ) +
    coord_polar()
  grob <- build_rect_grob(p)
  expect_s3_class(grob, "rect_fade_polar_grob")
})

test_that("coord_radial default (theta='x', vertical) routes to polar grob", {
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(
      data = df_tile_single, aes(width = width, height = height)
    ) +
    coord_radial()
  grob <- build_rect_grob(p)
  expect_s3_class(grob, "rect_fade_polar_grob")
})

test_that("coord_polar theta='y' + horizontal routes to polar grob", {
  # The other radial case: theta = "y" + fade_direction = "horizontal".
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(
      data = df_tile_single,
      aes(width = width, height = height),
      fade_direction = "horizontal"
    ) +
    coord_polar(theta = "y")
  grob <- build_rect_grob(p)
  expect_s3_class(grob, "rect_fade_polar_grob")
})

test_that("coord_polar with angular combo informs and falls back to GeomRect", {
  # theta = "x" + fade_direction = "horizontal" would require a conic /
  # angular gradient, which grid does not support. Emit an informational
  # message and fall back to plain geom_rect rendering.
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(
      data = df_tile_single,
      aes(width = width, height = height),
      fade_direction = "horizontal"
    ) +
    coord_polar()
  expect_message(
    grob <- build_rect_grob(p),
    "angular fade is not yet supported"
  )
  expect_false(inherits(grob, "rect_fade_polar_grob"))
  expect_false(inherits(grob, "rect_fade_grob"))
})

test_that("coord_polar radial: uniform alpha bypasses polar grob", {
  # When no actual fade is requested (alpha = 1, alpha_fade_to = 1), polar
  # rendering degenerates to plain geom_rect rather than a polar gTree.
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(
      data = df_tile_single,
      aes(width = width, height = height),
      alpha_fade_to = 1
    ) +
    coord_polar()
  grob <- build_rect_grob(p)
  expect_false(inherits(grob, "rect_fade_polar_grob"))
})

test_that("coord_polar radial: empty data returns zeroGrob", {
  empty <- df_tile_single[0, ]
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(data = empty, aes(width = width, height = height)) +
    coord_polar()
  grob <- build_rect_grob(p)
  expect_s3_class(grob, "zeroGrob")
})

test_that("polar radial grob carries equal-length gradient + flat lists", {
  # Structural invariant mirroring the linear rect_fade_grob: each rect is
  # represented once in gradient_glist and once in flat_glist.
  df_two <- data.frame(
    xmin = c(10, 30), xmax = c(20, 40),
    ymin = c(10, 30), ymax = c(20, 40)
  )
  p <- ggplot() +
    geom_rect_fade(
      data = df_two, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE
    ) +
    coord_polar()
  grob <- build_rect_grob(p)
  expect_s3_class(grob, "rect_fade_polar_grob")
  expect_equal(length(grob$gradient_glist), 2L)
  expect_equal(length(grob$flat_glist), 2L)
})

test_that("polar radial gradient_glist wraps radialGradient rectGrob", {
  # Structural invariant on the deferred side (before makeContent runs):
  # each gradient entry is a gTree wrapping a single rectGrob whose fill is
  # a GridRadialGradient. This is what the capable-device branch of
  # makeContent.rect_fade_polar_grob emits as-is. Skipping the makeContent
  # step avoids cross-test interference from grid's device-capability
  # lookup, which cannot be cleanly mocked across S3 dispatch boundaries.
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(
      data = df_tile_single, aes(width = width, height = height)
    ) +
    coord_polar()
  grob <- build_rect_grob(p)
  g1   <- grob$gradient_glist[[1]]
  kids <- g1$children %||% list()
  expect_gt(length(kids), 0L)
  expect_s3_class(kids[[1]]$gp$fill, "GridRadialGradient")
  # And the flat fallback entry is a plain polygon-shaped fill (not a pattern).
  f1 <- grob$flat_glist[[1]]
  expect_false(inherits(f1$gp$fill, "GridPattern"))
})


# -----------------------------------------------------------------------
# Legend key
# -----------------------------------------------------------------------

test_that("draw_key produces a grob", {
  key_data <- data.frame(
    fill      = "steelblue",
    colour    = NA_character_,
    linewidth = 0.5,
    linetype  = 1,
    alpha     = NA_real_
  )
  grob <- .draw_key_rect_fade(
    key_data,
    list(alpha_fade_to = 0, fade_direction = "vertical"),
    size = c(1, 1)
  )
  expect_true(grid::is.grob(grob))
})

test_that("draw_key horizontal fade uses a horizontal gradient", {
  key_data <- data.frame(
    fill = "tomato", colour = NA_character_,
    linewidth = 0.5, linetype = 1, alpha = NA_real_
  )
  grob  <- .draw_key_rect_fade(
    key_data,
    list(alpha_fade_to = 0, fade_direction = "horizontal"),
    size = c(1, 1)
  )
  grad  <- grob$gp$fill
  expect_true(inherits(grad, "GridLinearGradient"))
  # Horizontal gradient: y1 == y2
  expect_equal(grad$y1, grad$y2)
})


# -----------------------------------------------------------------------
# Grammar of Graphics adversarial stress tests
# -----------------------------------------------------------------------

p_base <- ggplot() +
  geom_rect_fade(
    data       = df_corners,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE
  )

## data --------------------------------------------------------------------

test_that("GoG/data: empty data frame returns zeroGrob without error", {
  expect_no_error(
    suppressMessages(ggplotGrob(
      ggplot() + geom_rect_fade(
        data = data.frame(xmin=numeric(0), xmax=numeric(0),
                          ymin=numeric(0), ymax=numeric(0)),
        aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
        inherit.aes = FALSE
      )
    ))
  )
})

test_that("GoG/data: single rect renders without error", {
  df <- data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  expect_no_error(
    ggplotGrob(ggplot() + geom_rect_fade(
      data = df, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
      inherit.aes = FALSE
    ))
  )
})

test_that("GoG/data: aes(width, height) single-row renders without error", {
  expect_no_error(
    ggplotGrob(ggplot(mapping = aes(x, y)) +
      geom_rect_fade(data = df_tile_single, aes(width = width, height = height)))
  )
})

test_that("GoG/data: rects with non-finite bounds are dropped without crashing", {
  # ggplot2's scale system may filter -Inf before draw_panel; regardless
  # the plot must not crash.
  df <- data.frame(xmin = c(0, -Inf), xmax = c(1, 2), ymin = 0, ymax = 1)
  expect_no_error(
    suppressWarnings(ggplotGrob(ggplot() + geom_rect_fade(
      data = df, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
      inherit.aes = FALSE
    )))
  )
})

## mapping -----------------------------------------------------------------

test_that("GoG/mapping: inherit.aes = FALSE isolates aesthetics", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_rect_fade(
      data = df_corners,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE
    )
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/mapping: alpha aes is accepted", {
  df <- data.frame(xmin = c(0, 2), xmax = c(1, 3),
                   ymin = 0, ymax = 1, a = c(0.3, 0.8))
  expect_no_error(
    ggplotGrob(ggplot() + geom_rect_fade(
      data = df, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax, alpha=a),
      inherit.aes = FALSE
    ))
  )
})

## layer -------------------------------------------------------------------

test_that("GoG/layer: two rect_fade layers stack without error", {
  p <- ggplot() +
    geom_rect_fade(
      data = df_corners[1, ],
      aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
      inherit.aes = FALSE, fill = "steelblue"
    ) +
    geom_rect_fade(
      data = df_corners[2, ],
      aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
      inherit.aes = FALSE, fill = "tomato"
    )
  expect_no_error(ggplotGrob(p))
})

## scales ------------------------------------------------------------------

test_that("GoG/scales: log10 x-scale renders without error", {
  df <- data.frame(xmin = 1, xmax = 10, ymin = 0, ymax = 1)
  expect_no_error(
    suppressWarnings(ggplotGrob(
      ggplot() +
        geom_rect_fade(
          data = df, aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
          inherit.aes = FALSE
        ) +
        scale_x_log10()
    ))
  )
})

test_that("GoG/scales: reversed x-scale renders without error", {
  expect_no_error(
    ggplotGrob(p_base + scale_x_reverse())
  )
})

test_that("GoG/scales: reversed y-scale renders without error", {
  expect_no_error(
    ggplotGrob(p_base + scale_y_reverse())
  )
})

test_that("GoG/scales: reversed x-scale flips gradient direction", {
  p_rev <- ggplot() +
    geom_rect_fade(
      data = df_corners[1, ],
      aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
      fade_direction = "horizontal", alpha_fade_to = 0,
      inherit.aes = FALSE
    ) +
    scale_x_reverse()
  p_fwd <- ggplot() +
    geom_rect_fade(
      data = df_corners[1, ],
      aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
      fade_direction = "horizontal", alpha_fade_to = 0,
      inherit.aes = FALSE
    )
  grob_rev <- build_rect_grob(p_rev)
  grob_fwd <- build_rect_grob(p_fwd)
  alphas_rev <- col_alpha(grob_rev$gradient_glist[[1]]$gp$fill$colours)
  alphas_fwd <- col_alpha(grob_fwd$gradient_glist[[1]]$gp$fill$colours)
  # Under reversal the colour order at the visual edges flips.
  expect_equal(alphas_rev[1], alphas_fwd[2], tolerance = 0.01)
  expect_equal(alphas_rev[2], alphas_fwd[1], tolerance = 0.01)
})

test_that("GoG/scales: explicit limits render without error", {
  expect_no_error(
    ggplotGrob(p_base + xlim(-5, 15) + ylim(-5, 15))
  )
})

## coord -------------------------------------------------------------------

test_that("GoG/coord: coord_cartesian zoom renders without error", {
  expect_no_error(
    ggplotGrob(p_base + coord_cartesian(xlim = c(1, 8), ylim = c(0, 5)))
  )
})

test_that("GoG/coord: coord_flip renders without error", {
  expect_no_error(ggplotGrob(p_base + coord_flip()))
})

test_that("coord_flip: fade_direction rotates with the rendering", {
  # Behavioural pin for the 2026-05 coord_flip parity fix. Default
  # `fade_direction = "vertical"` paints a vertical gradient; coord_flip
  # must rotate that gradient to horizontal in NPC.
  g_normal <- .collect_gradient_axes(p_base)
  g_flip <- .collect_gradient_axes(p_base + coord_flip())
  expect_true(!is.null(g_normal) && nrow(g_normal) > 0)
  expect_true(!is.null(g_flip) && nrow(g_flip) > 0)
  # Default fade_direction = "vertical": gradient runs y0 -> y1.
  expect_true(all(as.numeric(g_normal[, "x1"]) == as.numeric(g_normal[, "x2"])))
  expect_true(all(as.numeric(g_normal[, "y1"]) != as.numeric(g_normal[, "y2"])))
  # Under coord_flip: gradient axis swaps to horizontal in NPC.
  expect_true(all(as.numeric(g_flip[, "y1"]) == as.numeric(g_flip[, "y2"])))
  expect_true(all(as.numeric(g_flip[, "x1"]) != as.numeric(g_flip[, "x2"])))
})

test_that("coord_flip: vdiffr snapshot pins the rotated rendering", {
  vdiffr::expect_doppelganger("rect-fade-coord-flip", p_base + coord_flip())
})

test_that("GoG/coord: coord_fixed renders without error", {
  expect_no_error(ggplotGrob(p_base + coord_fixed()))
})

test_that("GoG/coord: coord_polar does not error (falls back gracefully)", {
  expect_no_error(suppressMessages(ggplotGrob(p_base + coord_polar())))
})

test_that("GoG/coord: coord_radial does not error (falls back gracefully)", {
  expect_no_error(suppressMessages(ggplotGrob(p_base + coord_radial())))
})

## facets ------------------------------------------------------------------

test_that("GoG/facets: facet_wrap renders without error", {
  df <- rbind(
    transform(df_corners, panel = "A"),
    transform(df_corners, panel = "B")
  )
  p <- ggplot() +
    geom_rect_fade(
      data = df,
      aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
      inherit.aes = FALSE
    ) +
    facet_wrap(~panel)
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/facets: facet_grid renders without error", {
  df <- rbind(
    transform(df_corners, r = "top",    c = "left"),
    transform(df_corners, r = "bottom", c = "right")
  )
  p <- ggplot() +
    geom_rect_fade(
      data = df,
      aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
      inherit.aes = FALSE
    ) +
    facet_grid(r ~ c)
  expect_no_error(ggplotGrob(p))
})

## theme -------------------------------------------------------------------

test_that("GoG/theme: theme_void renders without error", {
  expect_no_error(ggplotGrob(p_base + theme_void()))
})

test_that("GoG/theme: theme_classic renders without error", {
  expect_no_error(ggplotGrob(p_base + theme_classic()))
})

test_that("GoG/theme: theme_bw renders without error", {
  expect_no_error(ggplotGrob(p_base + theme_bw()))
})
