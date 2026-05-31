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

test_that("coord_transform keeps the fade (rects stay axis-aligned)", {
  # `coord_transform()` is separable per axis, so rectangles stay axis-aligned
  # -- only their edges move. The gradient path handles them (reading
  # post-transform corners) rather than falling back to plain geom_rect, which
  # would drop the fade.
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(
      data = df_tile_single, aes(width = width, height = height)
    ) +
    coord_transform(x = "log10")
  grob <- suppressMessages(build_rect_grob(p))
  expect_s3_class(grob, "rect_fade_grob")
})

test_that("coord_transform keeps the pattern fill", {
  # Regression: a `pattern` fill must survive coord_transform too -- the
  # fallback used to drop both the fade and the hatch.
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(
      data = df_tile_single, aes(width = width, height = height),
      pattern = hatch()
    ) +
    coord_transform(x = "log10")
  b     <- ggplot_build(p)
  ld    <- b$data[[1]]
  pp    <- b$layout$panel_params[[1]]
  coord <- b$layout$coord
  grob  <- suppressMessages(GeomRectFade$draw_panel(
    ld, pp, coord,
    alpha_fade_to = 0, fade_direction = "vertical",
    radius = grid::unit(0, "pt"), pattern = hatch()
  ))
  expect_s3_class(grob, "rect_fade_pattern_grob")
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

# --- Polar + pattern fill -----------------------------------------------------
# As in the col-fade tests, svglite (vdiffr's renderer) lacks "dest.in"
# compositing, so an image snapshot would capture the flat fallback rather than
# the hatch. Assert the grob structure under mocked device capabilities.

build_rect_polar_pattern_grob <- function(p, pattern) {
  b     <- ggplot_build(p)
  ldata <- b$data[[1]]
  pp    <- b$layout$panel_params[[1]]
  coord <- b$layout$coord
  gp    <- p$layers[[1]]$geom_params
  GeomRectFade$draw_panel(
    ldata, pp, coord,
    alpha_fade_to  = gp$alpha_fade_to  %||% 0,
    fade_direction = gp$fade_direction %||% "vertical",
    radius         = gp$radius         %||% grid::unit(0, "pt"),
    pattern        = pattern
  )
}

test_that("polar rect_fade pattern composites hatch rings on a compositing device", {
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(
      data = df_tile_single, aes(width = width, height = height),
      pattern = hatch()
    ) +
    coord_polar()
  grob <- build_rect_polar_pattern_grob(p, hatch())
  expect_s3_class(grob, "rect_fade_polar_grob")
  expect_s3_class(grob$pattern, "ggpointless_pattern")

  local_mocked_bindings(dev.cur = \(...) c(png = 2L), .package = "grDevices")
  local_mocked_bindings(
    dev.capabilities = \(...) list(
      clippingPaths = TRUE,
      patterns      = c("LinearGradient", "RadialGradient"),
      compositing   = c("clear", "source", "over", "dest.in")
    ),
    .package = "grDevices"
  )

  res <- grid::makeContent(grob)
  is_polygon <- vapply(res$children, inherits, logical(1), "polygon")
  expect_false(any(is_polygon))
  has_clip <- vapply(
    res$children,
    \(ch) !is.null(ch$vp) && !is.null(ch$vp$clip),
    logical(1)
  )
  expect_true(all(has_clip))
})

test_that("polar rect_fade pattern falls back to flat without compositing", {
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(
      data = df_tile_single, aes(width = width, height = height),
      pattern = hatch()
    ) +
    coord_polar()
  grob <- build_rect_polar_pattern_grob(p, hatch())

  local_mocked_bindings(dev.cur = \(...) c(png = 2L), .package = "grDevices")
  local_mocked_bindings(
    dev.capabilities = \(...) list(
      clippingPaths = TRUE,
      patterns      = c("LinearGradient", "RadialGradient"),
      compositing   = character(0)
    ),
    .package = "grDevices"
  )

  res <- suppressMessages(grid::makeContent(grob))
  is_polygon <- vapply(res$children, inherits, logical(1), "polygon")
  expect_true(all(is_polygon))
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

test_that("coord_polar angular + pattern keeps the pattern (no fade)", {
  # The angular fade can't be expressed in grid, but a pattern is mm-space and
  # independent of the fade -- it must still render, clipped to each wedge at a
  # uniform alpha, rather than falling back to a plain (pattern-less) geom_rect.
  p <- ggplot(mapping = aes(x, y)) +
    geom_rect_fade(
      data = df_tile_single, aes(width = width, height = height),
      fade_direction = "horizontal", pattern = hatch()
    ) +
    coord_polar()
  b     <- ggplot_build(p)
  ld    <- b$data[[1]]
  pp    <- b$layout$panel_params[[1]]
  coord <- b$layout$coord
  expect_message(
    grob <- GeomRectFade$draw_panel(
      ld, pp, coord,
      alpha_fade_to = 0, fade_direction = "horizontal",
      radius = grid::unit(0, "pt"), pattern = hatch()
    ),
    "without a fade"
  )
  expect_s3_class(grob, "rect_fade_polar_grob")
  expect_s3_class(grob$pattern, "ggpointless_pattern")
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

test_that("non-finite bounds warning is consolidated across layers", {
  df <- data.frame(xmin = 1, xmax = 5, ymin = -Inf, ymax = 10)
  layer_for <- function(fill) {
    geom_rect_fade(
      data = df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = fill
    )
  }
  p <- ggplot() + layer_for("grey50") + layer_for("tomato") + scale_y_log10()

  warns <- character()
  withCallingHandlers(
    ggplotGrob(p),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  ours <- grep("non-finite bounds", warns, value = TRUE)
  # Two layers each drop one rect, but exactly ONE warning fires, naming
  # the geom once and summing the count.
  expect_length(ours, 1L)
  expect_match(ours, "removed 2 rect")
  expect_match(ours, "geom_rect_fade")
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

# -----------------------------------------------------------------------
# pattern fills
# -----------------------------------------------------------------------

# Build the draw_panel grob with an explicit `pattern` argument.
build_rect_grob_pattern <- function(p, pattern) {
  b     <- ggplot_build(p)
  ldata <- b$data[[1]]
  pp    <- b$layout$panel_params[[1]]
  coord <- b$layout$coord
  GeomRectFade$draw_panel(
    ldata, pp, coord,
    alpha_fade_to = 0, fade_direction = "vertical",
    radius = grid::unit(0, "pt"), pattern = pattern
  )
}

p_pat <- ggplot(df_corners) +
  geom_rect_fade(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))

test_that("pattern = hatch() produces a deferred rect_fade_pattern_grob", {
  g <- build_rect_grob_pattern(p_pat, hatch())
  expect_s3_class(g, "rect_fade_pattern_grob")
  # one dst per rect, plus matching alpha_ref / outline / flat lists
  expect_length(g$dst_glist, nrow(df_corners))
  expect_length(g$alpha_ref_glist, nrow(df_corners))
})

test_that("hatch() validates its arguments and stores the spec", {
  h <- hatch(angle = 30, style = "crossed", spacing = 3)
  expect_s3_class(h, "ggpointless_pattern")
  expect_equal(h$angle, 30)
  expect_equal(h$style, "crossed")
  expect_equal(as.numeric(grid::convertUnit(h$spacing, "mm")), 3)
  expect_equal(hatch()$style, "parallel") # default
  expect_error(hatch(angle = c(1, 2)), "single number")
  expect_error(hatch(style = "diagonal")) # arg_match: not a valid option
  expect_error(hatch(spacing = "x"), "unit")
})

test_that("pattern preset strings expand to the matching hatch() spec", {
  # `position = "dodge"` idiom: each string is sugar for a hatch() call.
  expect_equal(.hatch_from_string("stripe"),     hatch())
  expect_equal(.hatch_from_string("crossed"),    hatch(style = "crossed"))
  expect_equal(.hatch_from_string("vertical"),   hatch(90))
  expect_equal(.hatch_from_string("horizontal"), hatch(0))
  # "grid" is the fifth preset: crossed at 90 deg (vertical + horizontal).
  expect_equal(.hatch_from_string("grid"),       hatch(90, style = "crossed"))
})

test_that("pattern = '<preset>' renders identically to the hatch() call", {
  # End-to-end: the string is coerced in setup_params, so the drawn grob must
  # match the explicit hatch() spec byte-for-byte (same crossed flag + angle).
  g_str <- build_rect_grob_pattern(p_pat, "grid")
  g_obj <- build_rect_grob_pattern(p_pat, hatch(90, style = "crossed"))
  expect_equal(g_str$dst_glist[[1]], g_obj$dst_glist[[1]])
})

test_that("pattern coercion is visible in the built layer params", {
  p <- ggplot(df_corners) +
    geom_rect_fade(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      pattern = "crossed"
    )
  b <- ggplot_build(p)
  pat <- b$plot$layers[[1]]$computed_geom_params$pattern
  expect_s3_class(pat, "ggpointless_pattern")
  expect_equal(pat, hatch(style = "crossed"))
})

test_that("pattern = unknown string aborts with an arg_match hint", {
  p <- ggplot(df_corners) +
    geom_rect_fade(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      pattern = "diagonal"  # not a preset; "stripe" is the diagonal one
    )
  expect_error(ggplot2::ggplot_build(p), "pattern")
})

test_that('style = "crossed" sets the crossed flag (two line families)', {
  g1 <- build_rect_grob_pattern(p_pat, hatch(style = "parallel"))
  g2 <- build_rect_grob_pattern(p_pat, hatch(style = "crossed"))
  # dst is a lightweight `hatch_spec`; the segment families are materialised
  # at draw time from the rect's physical size (see `.hatch_materialize()`).
  expect_s3_class(g1$dst_glist[[1]], "hatch_spec")
  expect_false(g1$dst_glist[[1]]$crossed)
  expect_true(g2$dst_glist[[1]]$crossed)
})

test_that("hatch colour follows the fill aesthetic", {
  # `hatch()` exposes no colour argument: the lines always track `fill`.
  p_fill <- ggplot(df_corners) +
    geom_rect_fade(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                   fill = "tomato")
  g <- build_rect_grob_pattern(p_fill, hatch())
  expect_equal(
    grDevices::col2rgb(g$dst_glist[[1]]$colour),
    grDevices::col2rgb("tomato")
  )
})

test_that("pattern alpha mask and outline share identical roundrect geometry", {
  # The alpha mask clips the pattern to the rounded shape via dest.in, so it
  # MUST be a roundrect matching the outline (same radius and dimensions). A
  # sharp rectGrob mask would let the pattern spill past rounded corners, and
  # an unclamped radius would degenerate into a lens on short rects -- the
  # `cut = "Fair"` artifact. Equal geometry guarantees equal clamping, so the
  # masked pattern and the drawn border always share one shape.
  df <- data.frame(xmin = 0, xmax = 4, ymin = 0, ymax = 3)
  p <- ggplot(df) +
    geom_rect_fade(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "steelblue", colour = "#333333",
      radius = grid::unit(10, "mm"), pattern = hatch()
    )
  b     <- ggplot_build(p)
  ld    <- b$data[[1]]
  pp    <- b$layout$panel_params[[1]]
  coord <- b$layout$coord
  gp    <- p$layers[[1]]$geom_params
  grob  <- GeomRectFade$draw_panel(
    ld, pp, coord,
    alpha_fade_to = 0, fade_direction = "vertical",
    radius = gp$radius, pattern = hatch()
  )
  expect_s3_class(grob, "rect_fade_pattern_grob")
  expect_gt(length(grob$alpha_ref_glist), 0L)
  for (i in seq_along(grob$alpha_ref_glist)) {
    mask <- grob$alpha_ref_glist[[i]]
    out  <- grob$outline_glist[[i]]
    expect_s3_class(mask, "roundrect")
    expect_s3_class(out,  "roundrect")
    expect_identical(as.numeric(mask$r),      as.numeric(out$r))
    expect_identical(as.numeric(mask$width),  as.numeric(out$width))
    expect_identical(as.numeric(mask$height), as.numeric(out$height))
  }
})

# Walk a grob tree for a Porter-Duff compositing group (the dest.in result).
.has_grid_group <- function(grob) {
  if (inherits(grob, "GridGroup")) return(TRUE)
  ch <- grob$children
  if (is.null(ch) || length(ch) == 0L) return(FALSE)
  any(vapply(ch, .has_grid_group, logical(1)))
}

test_that("pattern, sharp corners (radius 0): mask and outline are r = 0 roundrects", {
  # With no rounding the mask must still match the outline -- both r = 0
  # roundrects (equivalent to sharp rects), so the pattern fills the full
  # rectangle and the border traces it exactly.
  g <- build_rect_grob_pattern(p_pat, hatch())  # build helper uses radius = 0
  expect_s3_class(g, "rect_fade_pattern_grob")
  for (i in seq_along(g$alpha_ref_glist)) {
    mask <- g$alpha_ref_glist[[i]]
    out  <- g$outline_glist[[i]]
    expect_s3_class(mask, "roundrect")
    expect_equal(as.numeric(mask$r), 0)
    if (!inherits(out, "zeroGrob")) {
      expect_equal(as.numeric(out$r), 0)
      expect_identical(as.numeric(mask$width), as.numeric(out$width))
    }
  }
})

test_that("pattern device tiers: compositing groups vs flat fallback", {
  g <- build_rect_grob_pattern(p_pat, hatch())

  # Compositing-capable device -> dest.in groups (the masked pattern).
  local_mocked_bindings(dev.cur = \(...) c(png = 2L), .package = "grDevices")
  local_mocked_bindings(
    dev.capabilities = \(...) list(
      clippingPaths = TRUE,
      patterns      = c("LinearGradient", "RadialGradient"),
      compositing   = c("clear", "source", "over", "dest.in")
    ),
    .package = "grDevices"
  )
  res_comp <- grid::makeContent(g)
  expect_true(any(vapply(res_comp$children, .has_grid_group, logical(1))))

  # Device without dest.in -> flat semi-transparent roundrects, no group.
  local_mocked_bindings(
    dev.capabilities = \(...) list(
      clippingPaths = TRUE,
      patterns      = c("LinearGradient", "RadialGradient"),
      compositing   = character(0)
    ),
    .package = "grDevices"
  )
  res_flat <- suppressMessages(grid::makeContent(g))
  expect_false(any(vapply(res_flat$children, .has_grid_group, logical(1))))
  expect_true(all(vapply(res_flat$children, inherits, logical(1), "roundrect")))
})

test_that("a user grob pattern is clipped (dst is a gTree, not a roundrect)", {
  wave <- grid::linesGrob(
    x = grid::unit(c(0, 1), "npc"), y = grid::unit(c(0.5, 0.5), "npc")
  )
  g <- build_rect_grob_pattern(p_pat, wave)
  expect_s3_class(g, "rect_fade_pattern_grob")
  expect_true(grid::is.grob(g$dst_glist[[1]]))
})

test_that("a user grid::pattern() is accepted (tiled dst)", {
  pat <- grid::pattern(
    grid::circleGrob(r = grid::unit(1, "mm")),
    width = grid::unit(5, "mm"), height = grid::unit(5, "mm"),
    extend = "repeat"
  )
  g <- build_rect_grob_pattern(p_pat, pat)
  expect_s3_class(g, "rect_fade_pattern_grob")
})

test_that("an invalid pattern TYPE aborts; valid preset strings do not", {
  base <- ggplot(df_corners) +
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  # A wrong type (here a number) still aborts with the type class.
  expect_error(
    ggplot_build(base + geom_rect_fade(pattern = 42)),
    class = "ggpointless_rect_fade_pattern_type"
  )
  # Preset strings ARE accepted now (sugar for hatch()); no abort.
  expect_no_error(
    ggplot_build(base + geom_rect_fade(pattern = "stripe"))
  )
})

test_that("GoG: pattern fills render without error (all three modes)", {
  wave <- grid::gTree(children = grid::gList(grid::linesGrob(
    x = grid::unit(c(0, 1), "npc"), y = grid::unit(c(0.4, 0.6), "npc")
  )))
  dots <- grid::pattern(
    grid::circleGrob(r = grid::unit(1, "mm")),
    width = grid::unit(5, "mm"), height = grid::unit(5, "mm"), extend = "repeat"
  )
  base <- ggplot(df_corners) +
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  expect_no_error(ggplotGrob(base + geom_rect_fade(pattern = hatch())))
  expect_no_error(ggplotGrob(base + geom_rect_fade(pattern = hatch(style = "crossed"))))
  expect_no_error(ggplotGrob(base + geom_rect_fade(pattern = wave)))
  expect_no_error(ggplotGrob(base + geom_rect_fade(pattern = dots)))
})

test_that("pattern recolours the user grob stroke to the fill aesthetic", {
  line <- grid::linesGrob(
    x = grid::unit(c(0, 1), "npc"), y = grid::unit(c(0.5, 0.5), "npc"),
    gp = grid::gpar(col = "black")
  )
  p <- ggplot(df_corners) +
    geom_rect_fade(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "tomato"
    )
  g <- build_rect_grob_pattern(p, line)
  dst1 <- g$dst_glist[[1]]
  # the clipped grob's child line should carry the fill colour, not black
  child <- dst1$children[[1]]
  expect_equal(
    grDevices::col2rgb(child$gp$col),
    grDevices::col2rgb("tomato")
  )
})

test_that("vdiffr snapshot: hatch() pattern highlight (NYT-style)", {
  df_econ <- head(economics, 25)
  p <- ggplot(df_econ, aes(date, unemploy)) +
    geom_rect_fade(
      data = data.frame(
        xmin = min(df_econ$date),
        xmax = max(df_econ$date),
        ymin = -Inf, ymax = 2800
      ),
      inherit.aes = FALSE,
      fill = "tomato",
      alpha = 0.75,
      alpha_fade_to = 0,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      pattern = hatch()
    ) +
    stat_fourier(
      geom = "line_fade", fade_direction = "start", alpha_fade_to = 0.2
    ) +
    geom_point(size = 3, alpha = 0.2) +
    theme_minimal()
  vdiffr::expect_doppelganger("rect-fade-hatch-pattern", p)
})
