# Safety-net tests for geom_line_fade / geom_path_fade (coord interactions).
#
# These tests capture CURRENT behaviour of the path-fade pipeline under
# non-default coord systems, before any fixes are applied. Two known issues
# are pinned here so post-fix behaviour can be compared:
#
#   (A) coord_fixed(): draw_panel still produces a `path_fade_grob` with the
#       correct per-vertex alpha (alpha_vert = [0, 1] for a two-point line),
#       but the visible fade is lost at render time — the rendered line is a
#       uniform solid stroke. Bug lives in makeContent.path_fade_grob, not in
#       draw_panel.
#
#   (B) coord_transform(y = "log10") with a sibling layer contaminating the
#       y-scale (e.g. a point at y = 0 → log10(0) = -Inf): draw_panel returns
#       zeroGrob() because total_dist is non-finite. The line silently
#       disappears, which is poor UX.
#
# The file also pins a linear reference (the "base" case) so that any future
# fix does not regress ordinary rendering.

library(ggplot2)

line_df <- data.frame(x = c(1, 200), y = c(100, 1))

# Helpers ------------------------------------------------------------------

# Build a geom_line_fade plot, run ggplot_build, and call the Geom's
# draw_panel() directly. Returns the grob it produces. Mirrors the
# `.abline_fade_probe` pattern used in test-geom-abline-fade.R.
.line_fade_probe <- function(p, layer_idx = 1L) {
  b     <- ggplot_build(p)
  pp    <- b$layout$panel_params[[1]]
  coord <- b$layout$coord
  layer <- p$layers[[layer_idx]]
  data  <- b$data[[layer_idx]]
  GeomLineFade$draw_panel(
    data, pp, coord,
    alpha_fade_to  = layer$geom_params$alpha_fade_to  %||% 0,
    fade_direction = layer$geom_params$fade_direction %||% "start",
    alpha_mode     = layer$geom_params$alpha_mode     %||% "auto"
  )
}

# ---------------------------------------------------------------------------
# (A) coord_fixed — data-level correctness is in place
# ---------------------------------------------------------------------------

test_that("coord_fixed: draw_panel produces a path_fade_grob with correct alpha_vert", {
  # Data-level correctness: the per-vertex alpha vector reaches makeContent
  # unchanged. The fade-loss bug is downstream (in render-time unit
  # conversions), not here. This test pins that fact so a future fix can
  # demonstrate it does NOT regress draw_panel output.
  p <- ggplot(line_df, aes(x, y)) + geom_line_fade() + coord_fixed()
  grob <- .line_fade_probe(p)
  expect_s3_class(grob, "path_fade_grob")
  expect_equal(length(grob$groups_data), 1L)
  expect_equal(grob$groups_data[[1]]$alpha_vert, c(0, 1))
})

test_that("coord_fixed: plot builds and renders without error", {
  # Early-warning: catches future regressions that would make coord_fixed
  # crash rather than merely render a uniform line.
  p <- ggplot(line_df, aes(x, y)) + geom_line_fade() + coord_fixed()
  expect_no_error(ggplotGrob(p))
})

test_that("vdiffr: coord_fixed rendering (pin current output)", {
  # Note on safety-net scope: vdiffr's default svglite device does NOT
  # report compositing or clippingPaths capabilities, so makeContent falls
  # through to `.build_flat_fallback_grobs` — a per-segment segmentsGrob
  # with each segment getting its mean endpoint alpha. For a two-point
  # line this collapses to a single stroke at alpha = 0.5, identical to
  # the linear-reference snapshot except for panel aspect. The snapshot
  # therefore pins layout (not the fade itself); the real coord_fixed
  # fade-loss bug lives in the gradient/composite branches that ragg and
  # cairo_svg exercise but svglite does not. Visual verification of a
  # fix needs ragg::agg_png() manually — see inst/scripts/rect_fade_coords.
  skip_if_not_installed("vdiffr")
  p <- ggplot(line_df, aes(x, y)) +
    geom_line_fade(linewidth = 6, colour = "#e63946") +
    coord_fixed()
  vdiffr::expect_doppelganger("line-fade-coord-fixed", p)
})

# ---------------------------------------------------------------------------
# (B) coord_transform(y = "log10") — line silently disappears when the
# y-scale is contaminated by a sibling layer
# ---------------------------------------------------------------------------

test_that("coord_log10 with contaminating sibling: draw_panel returns zeroGrob", {
  # The geom_point layer introduces log10(0) = -Inf, forcing the shared
  # y-scale to c(-Inf, Inf). Rescaling any finite y through that range
  # produces NaN, and the `!is.finite(total_dist)` guard in draw_panel
  # returns zeroGrob. User-data issue, but currently silent.
  point_df <- data.frame(x = 100, y = c(0, 100))
  p <- ggplot(mapping = aes(x, y)) +
    geom_line_fade(data = line_df) +
    geom_point(data = point_df) +
    coord_transform(y = "log10")
  idx <- which(vapply(
    p$layers, \(l) inherits(l$geom, "GeomLineFade"), logical(1L)
  ))
  suppressWarnings(
    grob <- .line_fade_probe(p, layer_idx = idx)
  )
  expect_s3_class(grob, "zeroGrob")
})

test_that("coord_log10 on its own (clean positive y): line renders normally", {
  # Sanity: the log10 transform itself is fine; it is the scale
  # contamination from a sibling layer that produces zeroGrob. This test
  # pins that distinction so that a future fix targeting the
  # contamination case does not accidentally change the clean case.
  p <- ggplot(line_df, aes(x, y)) +
    geom_line_fade() +
    coord_transform(y = "log10")
  grob <- suppressWarnings(.line_fade_probe(p))
  expect_s3_class(grob, "path_fade_grob")
  expect_equal(grob$groups_data[[1]]$alpha_vert, c(0, 1))
})

# ---------------------------------------------------------------------------
# Step mode single-segment promotion: alpha_mode = "step" (explicit) on a
# two-observation line (one segment) would otherwise collapse to a uniform
# mean-alpha stroke because the mask assigns each segment a single alpha.
# makeContent silently promotes to gradient mode when n_seg == 1, so the
# fade is actually visible.  (Under the default "auto", n = 2 <= 50 also
# resolves to gradient.)
# ---------------------------------------------------------------------------

# Walk a grob tree and collect every element matching a predicate.
.collect_grobs <- function(g, pred) {
  out <- list()
  if (pred(g)) out <- c(out, list(g))
  kids <- g$children %||% list()
  for (k in kids) out <- c(out, .collect_grobs(k, pred))
  out
}

test_that("single-segment step path promotes to gradient rendering", {
  # Mock a device that advertises clippingPaths + dest.in so the
  # gradient branch of makeContent.path_fade_grob is reachable.
  local_mocked_bindings(
    dev.capabilities = \(...) list(
      clippingPaths = TRUE,
      compositing   = c("dest.in"),
      patterns      = c("LinearGradient", "RadialGradient", "TilingPattern")
    ),
    .package = "grDevices"
  )

  p <- ggplot(line_df, aes(x, y)) + geom_line_fade()
  grob <- .line_fade_probe(p)
  rendered <- grid::makeContent(grob)

  # Under gradient mode, each segment's content is a rectGrob with a
  # linearGradient fill. Step mode's fast-composite path would instead
  # emit a groupGrob (Porter-Duff compositor) with no linearGradient fill.
  grads <- .collect_grobs(rendered, \(node) {
    inherits(node, "rect") &&
      inherits(node$gp$fill, "GridLinearGradient")
  })
  expect_gt(length(grads), 0L)

  # Sanity: stop / start alpha of the gradient must straddle 0 and 1.
  gr <- grads[[1]]$gp$fill
  stops_alpha <- farver::decode_colour(gr$colours, alpha = TRUE)[, "alpha"]
  expect_equal(sort(range(stops_alpha)), c(0, 1))
})

test_that("multi-segment step path keeps step rendering", {
  # Paths with 3+ observations and explicit alpha_mode = "step" retain
  # step semantics (cheaper path, per-segment uniform alpha). Promotion
  # only fires for n_seg == 1.  Explicit "step" (not "auto") because
  # at n = 5 `auto` would resolve to gradient.
  local_mocked_bindings(
    dev.capabilities = \(...) list(
      clippingPaths = TRUE,
      compositing   = c("dest.in"),
      patterns      = c("LinearGradient", "RadialGradient", "TilingPattern")
    ),
    .package = "grDevices"
  )

  multi_df <- data.frame(x = 1:5, y = c(1, 3, 2, 4, 1))
  p <- ggplot(multi_df, aes(x, y)) + geom_line_fade(alpha_mode = "step")
  grob <- .line_fade_probe(p)
  rendered <- grid::makeContent(grob)

  # Multi-segment step mode uses a groupGrob(op="dest.in", …) composited
  # mask — the outermost child is an "xform" node (groupGrob subclass) and
  # there are no linearGradient fills in the tree.
  grads <- .collect_grobs(rendered, \(node) {
    inherits(node, "rect") &&
      inherits(node$gp$fill, "GridLinearGradient")
  })
  expect_equal(length(grads), 0L)
})

test_that("explicit alpha_mode = 'gradient' still routes to gradient path", {
  # Regression guard: the promotion shortcut must not break the existing
  # alpha_mode = "gradient" path.
  local_mocked_bindings(
    dev.capabilities = \(...) list(
      clippingPaths = TRUE,
      compositing   = c("dest.in"),
      patterns      = c("LinearGradient", "RadialGradient", "TilingPattern")
    ),
    .package = "grDevices"
  )

  multi_df <- data.frame(x = 1:5, y = c(1, 3, 2, 4, 1))
  p <- ggplot(multi_df, aes(x, y)) +
    geom_line_fade(alpha_mode = "gradient")
  grob <- .line_fade_probe(p)
  rendered <- grid::makeContent(grob)

  grads <- .collect_grobs(rendered, \(node) {
    inherits(node, "rect") &&
      inherits(node$gp$fill, "GridLinearGradient")
  })
  # One linearGradient rect per segment (n_seg = 4).
  expect_equal(length(grads), 4L)
})

# ---------------------------------------------------------------------------
# alpha_mode = "auto" — resolves per sub-path based on vertex count.
# Threshold: n <= 50 → gradient, n > 50 → step.
# ---------------------------------------------------------------------------

test_that("auto: resolves to gradient for small n (n <= 50)", {
  # 5-vertex path: auto should pick gradient.
  df_small <- data.frame(x = 1:5, y = c(1, 3, 2, 4, 1))
  p <- ggplot(df_small, aes(x, y)) + geom_line_fade(alpha_mode = "auto")
  layer <- p$layers[[1]]
  b <- ggplot_build(p)
  grob <- GeomLineFade$draw_panel(
    b$data[[1]], b$layout$panel_params[[1]], b$layout$coord,
    alpha_mode = "auto"
  )
  expect_s3_class(grob, "path_fade_grob")
  # Resolved mode stamped per group:
  expect_equal(grob$groups_data[[1]]$alpha_mode, "gradient")
})

test_that("auto: resolves to step for large n (n > 50)", {
  # 80-vertex path: auto should pick step.
  df_big <- data.frame(x = seq_len(80), y = sin(seq_len(80) / 4))
  p <- ggplot(df_big, aes(x, y)) + geom_line_fade(alpha_mode = "auto")
  b <- ggplot_build(p)
  grob <- GeomLineFade$draw_panel(
    b$data[[1]], b$layout$panel_params[[1]], b$layout$coord,
    alpha_mode = "auto"
  )
  expect_s3_class(grob, "path_fade_grob")
  expect_equal(grob$groups_data[[1]]$alpha_mode, "step")
})

test_that("auto: threshold is n <= 50 exactly", {
  for (n in c(50L, 51L)) {
    df <- data.frame(x = seq_len(n), y = sin(seq_len(n) / 4))
    p <- ggplot(df, aes(x, y)) + geom_line_fade(alpha_mode = "auto")
    b <- ggplot_build(p)
    grob <- GeomLineFade$draw_panel(
      b$data[[1]], b$layout$panel_params[[1]], b$layout$coord,
      alpha_mode = "auto"
    )
    expected <- if (n <= 50L) "gradient" else "step"
    expect_equal(grob$groups_data[[1]]$alpha_mode, expected,
                 info = paste("n =", n))
  }
})

test_that("auto: is the default alpha_mode for geom_line_fade", {
  # Constructor default exposed via the layer's geom_params.
  lyr <- geom_line_fade()
  expect_equal(lyr$geom_params$alpha_mode, "auto")
})

# ---------------------------------------------------------------------------
# Linear reference — pin healthy behaviour so a fix does not regress it
# ---------------------------------------------------------------------------

test_that("base (linear) reference: path_fade_grob + alpha_vert = [0, 1]", {
  p <- ggplot(line_df, aes(x, y)) + geom_line_fade()
  grob <- .line_fade_probe(p)
  expect_s3_class(grob, "path_fade_grob")
  expect_equal(grob$groups_data[[1]]$alpha_vert, c(0, 1))
})

test_that("vdiffr: base (linear) reference rendering", {
  # Pinned healthy render: two-point line fades from transparent at x=1
  # to opaque at x=200 under default fade_direction = "start".
  skip_if_not_installed("vdiffr")
  p <- ggplot(line_df, aes(x, y)) +
    geom_line_fade(linewidth = 6, colour = "#e63946")
  vdiffr::expect_doppelganger("line-fade-base-linear", p)
})
