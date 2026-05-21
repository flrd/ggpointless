# Grob-tree helpers
#
# GeomRibbon$draw_group (the parent) returns one of two structures:
#
#   outline.type = "full":
#     ggname("geom_ribbon", polygonGrob(...)) -- polygon grob directly
#
#   all other outline.type values:
#     ggname("geom_ribbon", gTree(
#       children = gList(
#         polygonGrob(...),    # the area fill
#         polylineGrob(...)    # the outline(s)
#       )
#     ))
#
# `ggname()` just sets the `name` attribute; it does NOT add a gTree wrapper.
# Both helpers below recurse into gTree children so they handle both forms.
#
# Why we need to touch the grob tree at all
#
# ggplot2 requires that fill colours arrive in `gp$fill` as a GridLinearGradient
# object (class "GridLinearGradient"), NOT wrapped in a list (class
# "GridPatternList"). Assigning `data$fill <- list(gradient)` creates a
# GridPatternList, which the PDF/SVG backend renders as a solid fill.
# Patching `grob$gp$fill` directly after the parent has built the grob is the
# only reliable way to inject a gradient without list-wrapping.
#
# For the compositing path the polygon's gp$fill is left entirely untouched --
# the parent may already have built a HORIZONTAL linearGradient there (when
# fill is mapped to a variable). The vertical alpha schedule is applied as a
# separate compositing step (see .composite_poly_fill below).

# Walk a ggplot2 polygon/gTree result and apply `action()` to every polygon
# grob, leaving non-polygon grobs (notably polylineGrob outlines) untouched.
#
# The parent GeomRibbon$draw_group returns one of two shapes:
#   outline.type = "full": a bare polygonGrob
#   other values:          a gTree whose children are one polygon + one polyline
#
# Recursing through gTree children covers both cases uniformly; both
# .patch_poly_fill and .composite_poly_fill need exactly this traversal.
#' @noRd
#' @keywords internal
.walk_polygons <- function(grob, action) {
  if (inherits(grob, "gTree")) {
    grob$children <- do.call(
      grid::gList,
      lapply(grob$children, .walk_polygons, action = action)
    )
  } else if (inherits(grob, "polygon")) {
    grob <- action(grob)
  }
  grob
}

# Fallback path helper: replace gp$fill in every polygon grob in the tree.
# Polyline grobs (outlines) are left unchanged.
#' @noRd
#' @keywords internal
.patch_poly_fill <- function(grob, fill) {
  .walk_polygons(grob, function(g) {
    g$gp$fill <- fill
    g
  })
}

# Primary path helper: wrap every polygon grob in a Porter-Duff "dest.in"
# group, leaving polyline grobs (outlines) at full opacity.
#
# outline.type = "full" special case
#
# For outline.type values "upper"/"lower"/"both", the parent returns a gTree
# with a polygon (fill) and a separate polyline (outline).  The recursion
# naturally composites only the polygon and leaves the polyline untouched.
#
# For outline.type = "full" the parent returns a single polygonGrob whose
# gp$col IS the outline -- there is no separate polyline.  Compositing the
# whole polygon fades the outline together with the fill, making it invisible
# near y = 0.  Fix: detach gp$col before compositing, composite the fill
# alone, then re-add the outline on top at full opacity.
#
# How grid::groupGrob() avoids a viewport+gradient bug
#
# Naive approach (does NOT work):
#   grid::grobTree(polygon_with_gradient, vp = grid::viewport(mask = mask))
# When a polygon whose gp$fill is a linearGradient is drawn inside a masked
# viewport, the device backend (ragg, Cairo) processes the gradient and the
# mask in the *same* rendering pass. The result: the gradient is silently
# dropped and the polygon is filled with a solid colour.
#
# groupGrob() avoids this by rendering each grob to an *independent offscreen
# buffer* before compositing them:
#   Step 1 -- render `dst` (the area polygon) to buffer A. Because this is an
#             isolated render, any linearGradient fill in gp$fill is produced
#             correctly with no mask interference.
#   Step 2 -- render `src` (a plain black rectGrob with a vertical alpha
#             gradient) to buffer B.
#   Step 3 -- composite: dest.in == result = dst_colour x src_alpha.
#             Only the alpha channel of `src` is used; its colour is irrelevant.
#
# The compositing rule "dest.in" is the Porter-Duff "Destination In Source"
# operator: the destination (area polygon) is made visible only where the
# source (alpha_ref) has non-zero alpha, scaled proportionally. This is
# identical in effect to what ggfx::with_mask() achieves via ImageMagick
# raster compositing, but without any external dependency.
#
# Requires R >= 4.2 grid::groupGrob added in R 4.2.0) and a device that
# reports "dest.in" in dev.capabilities()[["compositing"]].
#' @noRd
#' @keywords internal
.composite_poly_fill <- function(grob, src) {
  .walk_polygons(grob, function(g) {
    # When outline.type = "full" the outline lives on the polygon itself
    # (gp$col) rather than in a separate polyline grob.  Detach the outline,
    # composite the fill alone, then re-add the outline on top at full opacity.
    outline_col <- g$gp$col
    has_outline <- !is.null(outline_col) &&
      !identical(outline_col, NA) &&
      !all(is.na(outline_col))

    if (has_outline) {
      outline_grob <- g
      outline_grob$gp$fill <- NA
      g$gp$col <- NA
      grid::gTree(
        children = grid::gList(
          grid::groupGrob(src, op = "dest.in", dst = g),
          outline_grob
        )
      )
    } else {
      grid::groupGrob(src, op = "dest.in", dst = g)
    }
  })
}

# Custom grob that defers device detection to render time.
#
# Grobs built in draw_group() may be replayed on a different device
# (e.g. dev.copy2pdf(), RStudio "Export > Save as PDF"). By deferring the
# device capability check to makeContent() -- which fires at render time --
# we pick the correct rendering tier for the *actual* output device rather
# than the device that was active during plot construction.
#' @noRd
#' @keywords internal
.area_fade_grob <- function(
  poly_grob,
  alpha_ref,
  fallback_gradient,
  flat_fill,
  has_multi_fill
) {
  grid::gTree(
    poly_grob = poly_grob,
    alpha_ref = alpha_ref,
    fallback_gradient = fallback_gradient,
    flat_fill = flat_fill,
    has_multi_fill = has_multi_fill,
    cl = "area_fade_grob"
  )
}

#' @export
makeContent.area_fade_grob <- function(x) {
  dev_name <- names(grDevices::dev.cur())

  # Three rendering tiers (see draw_group comments for details):
  #   Tier 1 -- Porter-Duff "dest.in" compositing (ragg, svg, cairo on-screen)
  #   Tier 2 -- single-colour vertical linearGradient (cairo_pdf, png, etc.)
  #   Tier 3 -- flat semi-transparent fill (base pdf(), postscript)
  no_gradient <- dev_name %in% c("pdf", "postscript")
  no_composite <- dev_name %in% c("pdf", "cairo_pdf", "postscript")
  can_composite <- !no_composite && .has_compositing_op("dest.in")

  if (no_gradient) {
    # Base `pdf()` / `postscript()` cannot render any gradient, so the vertical
    # fade collapses to a flat mid-alpha fill.  Users always lose fidelity
    # here, regardless of whether `fill` was mapped to a variable -- inform in
    # both cases, but pick wording that reflects what was actually lost.
    # Per-grob calls are consolidated into one message at the end of the
    # render via `.queue_area_fade_no_gradient()`.
    .queue_area_fade_no_gradient(x$has_multi_fill)
    grob <- .patch_poly_fill(x$poly_grob, x$flat_fill)
  } else if (!can_composite) {
    # Tier 2: single-colour linearGradient.  Solid-fill groups render the
    # vertical fade perfectly here -- no fidelity loss worth flagging.  Only
    # the 2D case (multi-fill within one group) is degraded.
    if (x$has_multi_fill) {
      .queue_area_fade_no_composite()
    }
    grob <- .patch_poly_fill(x$poly_grob, x$fallback_gradient)
  } else {
    grob <- .composite_poly_fill(x$poly_grob, x$alpha_ref)
  }

  grid::setChildren(x, grid::gList(grob))
}

#' @noRd
#' @keywords internal
.draw_key_area_fade <- function(data, params, size) {
  flipped <- params$flipped_aes %||% FALSE
  fill_colour <- data$fill %||% "grey20"
  a_start <- data$alpha %||% 1
  # setup_params has normally already validated this, but draw_key can be
  # invoked standalone (e.g. guide_legend building a key without the geom's
  # setup_params pathway), so revalidate defensively -- cheap, and keeps the
  # whole-package validation consistent.
  a_end <- params$alpha_fade_to %||% 0
  .check_alpha_fade_to(a_end)

  # Legend key: opaque at the data-boundary end, transparent at the y = 0 end.
  # For the typical positive-values case this means transparent at bottom.
  # Coordinates are bbox-relative npc: 0 = bottom/left, 1 = top/right.
  if (flipped) {
    x1 <- 1
    y1 <- 0.5
    x2 <- 0
    y2 <- 0.5
  } else {
    x1 <- 0.5
    y1 <- 1
    x2 <- 0.5
    y2 <- 0
  }

  grid::rectGrob(
    gp = ggplot2::gg_par(
      fill = grid::linearGradient(
        colours = c(
          ggplot2::alpha(fill_colour, a_start),
          ggplot2::alpha(fill_colour, a_end)
        ),
        x1 = x1,
        y1 = y1,
        x2 = x2,
        y2 = y2
      ),
      col = NA
    )
  )
}

# Build all three rendering tiers for a fade-area grob.
#
# Both GeomAreaFade and GeomRidgelineFade share identical tier logic:
#   Tier 1 -- Porter-Duff dest.in compositing (best quality)
#   Tier 2 -- single-colour linearGradient fallback
#   Tier 3 -- flat semi-transparent fill (last resort)
# makeContent.area_fade_grob() selects the appropriate tier at draw time.
#
# The two geoms differ only in how they define their anchor point and
# excursion alphas (area-fade anchors at y = 0; ridgeline anchors at
# the ridge baseline).  This helper centralises the construction so both
# geoms stay in sync if the rendering logic ever needs to change.
#
# @param poly_grob     Polygon grob returned by the parent's draw_group().
# @param fill_col      Scalar fill colour (data$fill[1L]).
# @param has_multi_fill TRUE if fill varies within the group.
# @param a_start       Scaled peak alpha (determines the flat-fill midpoint).
# @param alpha_fade_to Alpha at the anchor (baseline / y = 0).
# @param alpha_lo      Alpha at the lower extreme of the polygon (val_lo).
# @param alpha_hi      Alpha at the upper extreme of the polygon (val_hi).
# @param anchor_bbox   Position of the anchor in the polygon bounding box
#   [0, 1]: 0 = anchor at the bottom (all-positive), 1 = anchor at the
#   top (all-negative), intermediate = mixed-sign.
# @param pos_npc       Length-3 numeric vector of panel NPC coordinates
#   corresponding to c(val_lo, anchor, val_hi).
# @param gx1,gy1,gx2,gy2  Gradient direction vectors (default: vertical).
# @param is_degen      TRUE when the polygon has zero extent (val_lo ==
#   val_hi).  Produces a flat transparent fill without gradient logic.
#' @noRd
#' @keywords internal
.build_area_fade_grob <- function(
  poly_grob,
  fill_col,
  has_multi_fill,
  a_start,
  alpha_fade_to,
  alpha_lo,
  alpha_hi,
  anchor_bbox,
  pos_npc,
  gx1 = 0.5,
  gy1 = 0,
  gx2 = 0.5,
  gy2 = 1,
  is_degen = FALSE
) {
  # Tolerance for "anchor sits on the bbox boundary": anchor_bbox values
  # within ANCHOR_EPS of 0 or 1 mean the polygon is effectively all-positive
  # (anchor at bottom) or all-negative (anchor at top); otherwise it's mixed.
  ANCHOR_EPS <- 1e-6

  # --- Tier 3: flat fill ---------------------------------------------------
  flat_fill <- ggplot2::alpha(fill_col, (a_start + alpha_fade_to) / 2)

  if (is_degen) {
    # Degenerate polygon (zero extent) -> fully-transparent flat gradient.
    col_fade <- ggplot2::alpha(fill_col, alpha_fade_to)
    fallback_gradient <- grid::linearGradient(
      colours = c(col_fade, col_fade),
      x1 = gx1,
      y1 = gy1,
      x2 = gx2,
      y2 = gy2
    )
    alpha_ref <- grid::rectGrob(
      gp = grid::gpar(
        fill = grid::linearGradient(
          colours = ggplot2::alpha(rep("black", 2L), alpha_fade_to),
          stops = c(0, 1),
          x1 = gx1,
          y1 = gy1,
          x2 = gx2,
          y2 = gy2
        ),
        col = NA
      )
    )
    return(.area_fade_grob(
      poly_grob,
      alpha_ref,
      fallback_gradient,
      flat_fill,
      has_multi_fill
    ))
  }

  # --- Tier 2: single-colour gradient (polygon bbox NPC coordinates) ------
  if (anchor_bbox < ANCHOR_EPS) {
    # All-positive: anchor at the bottom of the bbox.
    fallback_gradient <- grid::linearGradient(
      colours = c(
        ggplot2::alpha(fill_col, alpha_fade_to),
        ggplot2::alpha(fill_col, alpha_hi)
      ),
      x1 = gx1,
      y1 = gy1,
      x2 = gx2,
      y2 = gy2
    )
  } else if (anchor_bbox > 1 - ANCHOR_EPS) {
    # All-negative: anchor at the top of the bbox.
    fallback_gradient <- grid::linearGradient(
      colours = c(
        ggplot2::alpha(fill_col, alpha_lo),
        ggplot2::alpha(fill_col, alpha_fade_to)
      ),
      x1 = gx1,
      y1 = gy1,
      x2 = gx2,
      y2 = gy2
    )
  } else {
    # Mixed: three-stop gradient with the anchor in the middle.
    fallback_gradient <- grid::linearGradient(
      colours = c(
        ggplot2::alpha(fill_col, alpha_lo),
        ggplot2::alpha(fill_col, alpha_fade_to),
        ggplot2::alpha(fill_col, alpha_hi)
      ),
      stops = c(0, anchor_bbox, 1),
      x1 = gx1,
      y1 = gy1,
      x2 = gx2,
      y2 = gy2
    )
  }

  # --- Tier 1: alpha_ref for dest.in compositing ---------------------------
  if (anchor_bbox < ANCHOR_EPS) {
    comp_colours <- ggplot2::alpha("black", c(alpha_fade_to, alpha_hi))
    comp_stops <- pos_npc[2:3]
  } else if (anchor_bbox > 1 - ANCHOR_EPS) {
    comp_colours <- ggplot2::alpha("black", c(alpha_lo, alpha_fade_to))
    comp_stops <- pos_npc[1:2]
  } else {
    comp_colours <- ggplot2::alpha(
      "black",
      c(alpha_lo, alpha_fade_to, alpha_hi)
    )
    comp_stops <- pos_npc
  }

  ord <- order(comp_stops)
  comp_stops <- comp_stops[ord]
  comp_colours <- comp_colours[ord]

  # De-duplicate stops that can become identical when clipped to [0, 1]

  # (e.g. val_hi far above panel -> both anchor and val_hi clipped to 1.0).
  # linearGradient() with duplicate stops is undefined on some devices.

  dups <- duplicated(comp_stops)
  if (any(dups)) {
    comp_stops <- comp_stops[!dups]
    comp_colours <- comp_colours[!dups]
  }

  # Guard: linearGradient() requires at least 2 stops.  All stops can collapse
  # to a single value when the entire polygon is outside the visible panel
  # (e.g. all data is above the panel top so both anchor and val_hi clip to
  # NPC = 1.0, or below the panel bottom so both clip to 0.0).  In that case
  # the polygon is invisible anyway; a flat alpha_ref avoids a potential error.
  if (length(comp_stops) < 2L) {
    comp_stops <- c(0, 1)
    comp_colours <- rep(comp_colours[1L], 2L)
  }

  alpha_ref <- grid::rectGrob(
    gp = grid::gpar(
      fill = grid::linearGradient(
        colours = comp_colours,
        stops = comp_stops,
        x1 = gx1,
        y1 = gy1,
        x2 = gx2,
        y2 = gy2
      ),
      col = NA
    )
  )

  .area_fade_grob(
    poly_grob,
    alpha_ref,
    fallback_gradient,
    flat_fill,
    has_multi_fill
  )
}


#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAreaFade <- ggplot2::ggproto(
  "GeomAreaFade",
  ggplot2::GeomArea,

  extra_params = c(
    ggplot2::GeomArea$extra_params,
    "alpha_fade_to",
    "alpha_scope"
  ),

  draw_key = .draw_key_area_fade,

  # Stamp alpha_scope into data so draw_panel can read it -- ggplot2 v4
  # filters non-formal params out of the draw_panel call via `parameters()`.
  setup_data = \(self, data, params) {
    data <- ggplot2::ggproto_parent(ggplot2::GeomArea, self)$setup_data(
      data,
      params
    )
    data$.alpha_scope <- params$alpha_scope %||% "global"
    data
  },

  # Cross-panel reference for `alpha_scope = "global"`.  Same problem as in
  # GeomColFade: computing the max in `draw_panel` is per-panel only, so
  # under `facet_grid()` each panel re-normalises to its own tallest area
  # and breaks the "scaled relative to the tallest in the entire layer"
  # contract.  draw_layer sees ALL panels post-position (so position_stack
  # cumulative ymax values are already in place), so we compute once here
  # and stamp `global_max_abs` on every row.  draw_panel keeps a fallback
  # for the rare case where it's invoked directly with hand-built data.
  draw_layer = \(self, data, params, layout, coord) {
    flipped <- isTRUE(params$flipped_aes) ||
      isTRUE(any(data$flipped_aes %||% FALSE))
    coord_flipped <- inherits(coord, "CoordFlip")
    value_axis <- if (xor(flipped, coord_flipped)) "x" else "y"
    trans <- .get_scale_transformer(
      layout$panel_params[[1L]], value_axis
    )
    data <- .fade_stamp_global_max(
      data,
      value_fn = function(d) {
        if (flipped) c(d$xmin, d$xmax) else c(d$ymin, d$ymax)
      },
      slot = "global_max_abs",
      default_scope = "global",
      trans = trans
    )
    ggplot2::ggproto_parent(ggplot2::GeomArea, self)$draw_layer(
      data, params, layout, coord
    )
  },

  # Validation in setup_params() keeps the constructor body clean.
  # Defaults must be set here (not only in make_constructor) so that the
  # geom works when invoked via stat_function(geom = "area_fade") without
  # going through the constructor.
  setup_params = \(self, data, params) {
    params <- ggplot2::ggproto_parent(ggplot2::GeomArea, self)$setup_params(
      data,
      params
    )
    params <- .fade_setup_params(
      params,
      scopes = c("global", "group"),
      default_scope = "global"
    )
    .check_outline_type(params$outline.type)
    params
  },

  draw_panel = \(
    self,
    data,
    panel_params,
    coord,
    flipped_aes = FALSE,
    alpha_fade_to = 0,
    outline.type = "upper",
    ...
  ) {
    .check_panel_range(panel_params, "geom_area_fade")
    # For alpha_scope = "global", `data$global_max_abs` is stamped once in
    # `draw_layer` (cross-panel, post-position).  Falling back here only
    # when the column is missing -- typically when draw_panel is called
    # directly in tests with hand-built data.  For alpha_scope = "group",
    # we deliberately leave the column unstamped so draw_group's `%||%`
    # fallback computes a per-group max.
    if (
      identical(data$.alpha_scope[1L], "global") &&
        is.null(data$global_max_abs)
    ) {
      val <- if (isTRUE(flipped_aes)) {
        c(data$xmin, data$xmax)
      } else {
        c(data$ymin, data$ymax)
      }
      # Direct-call fallback: inv-transform into data space so the
      # local max matches the value-scale contract under non-linear
      # scales (mirrors the `draw_layer` stamp). Use `.layer_max_abs()`
      # for the Date/POSIXct/zero/NA fallback semantics -- the helper's
      # docstring explicitly warns against the inline copy that used
      # to live here.
      coord_flipped <- inherits(coord, "CoordFlip")
      value_axis <- if (xor(isTRUE(flipped_aes), coord_flipped)) "x" else "y"
      trans <- .get_scale_transformer(panel_params, value_axis)
      if (!identical(trans$name, "identity")) {
        val <- trans$inv(val)
      }
      data$global_max_abs <- .layer_max_abs(val)
    }

    # `...` is expected to be empty here: ggplot2 v4's Layer pipeline consumes
    # all extra_params (`alpha_scope`, `na.rm`, `orientation`) through
    # setup_params/setup_data before draw_panel is called, and the remaining
    # formals are named above.  Forwarding the (empty) dots to GeomRibbon's
    # draw_panel / draw_group is therefore a no-op in practice; kept here as a
    # safety net in case future ggplot2 versions reintroduce passthrough
    # parameters that match GeomRibbon's formals (lineend / linejoin / etc.).
    ggplot2::ggproto_parent(ggplot2::GeomArea, self)$draw_panel(
      data,
      panel_params,
      coord,
      flipped_aes = flipped_aes,
      alpha_fade_to = alpha_fade_to,
      outline.type = outline.type,
      ...
    )
  },

  draw_group = \(
    self,
    data,
    panel_params,
    coord,
    flipped_aes = FALSE,
    alpha_fade_to = 0,
    outline.type = "upper",
    ...
  ) {
    if (nrow(data) < 2L) {
      cli::cli_warn(
        c(
          "!" = "{.fn geom_area_fade}: dropping group with fewer than 2 observations.",
          "i" = "Consider filtering single-observation groups before plotting."
        ),
        .frequency = "regularly",
        .frequency_id = "geom_area_fade_short_group"
      )
      return(ggplot2::zeroGrob())
    }

    if (inherits(coord, "CoordPolar") || inherits(coord, "CoordRadial")) {
      .queue_area_polar_no_gradient()
      return(
        ggplot2::ggproto_parent(ggplot2::GeomArea, self)$draw_group(
          data,
          panel_params,
          coord,
          flipped_aes = flipped_aes,
          outline.type = outline.type,
          ...
        )
      )
    }

    if (.is_uniform_alpha(data, alpha_fade_to)) {
      return(ggplot2::ggproto_parent(ggplot2::GeomArea, self)$draw_group(
        data, panel_params, coord,
        flipped_aes = flipped_aes,
        outline.type = outline.type,
        ...
      ))
    }

    # Alpha at the data line; NA (no mapping) -> fully opaque.
    a_start <- data$alpha[1L]
    if (is.na(a_start)) {
      a_start <- 1
    }

    # Resolve outline colour.
    # `outline.type = "none"` -> remap to "full" with NA colour so the parent
    # returns a single polygonGrob (no separate polyline grob). This is
    # cheaper than remapping to "upper" with NA colour, which leaves an
    # invisible polyline in the tree. `.composite_poly_fill` already takes
    # the `has_outline = FALSE` branch for NA col, so no further handling.
    # For the other outline types, fall back to the fill colour when no
    # colour is supplied so the outline is visible by default.
    if (identical(outline.type, "none")) {
      data$colour <- NA
      outline.type <- "full"
    } else if (is.na(data$colour[1L])) {
      data$colour <- data$fill[1L]
    }

    # Two distinct "flipped" notions (mirrors the geom_col_fade fix):
    #   `flipped_aes`     - which DATA axis is the value axis (set by
    #                        ggplot2's orientation handling).
    #   `flipped_visual`  - which RENDERED axis the value axis ends up on.
    #                        `coord_flip()` rotates the rendering without
    #                        touching `flipped_aes`, so we OR-detect it.
    flipped_visual <- xor(isTRUE(flipped_aes), inherits(coord, "CoordFlip"))

    # Value-axis range of the polygon (setup_data guarantees this always
    # includes 0: ymin = 0, ymax = y; or xmin = 0, xmax = x if flipped).
    # Reads the data axis -> uses `flipped_aes` (data orientation).
    #
    # val_lo / val_hi are PANEL-space (used to position the gradient
    # anchor in NPC further down). val_lo_data / val_hi_data are
    # DATA-space (used only for the alpha-ratio numerators so they
    # match the data-space `max_abs` denominator under non-linear value
    # scales).
    if (flipped_aes) {
      val_lo <- min(c(data$xmin, data$xmax), na.rm = TRUE)
      val_hi <- max(c(data$xmin, data$xmax), na.rm = TRUE)
    } else {
      val_lo <- min(c(data$ymin, data$ymax), na.rm = TRUE)
      val_hi <- max(c(data$ymin, data$ymax), na.rm = TRUE)
    }
    coord_flipped <- inherits(coord, "CoordFlip")
    value_axis <- if (xor(flipped_aes, coord_flipped)) "x" else "y"
    trans <- .get_scale_transformer(panel_params, value_axis)
    if (!identical(trans$name, "identity")) {
      val_lo_data <- trans$inv(val_lo)
      val_hi_data <- trans$inv(val_hi)
    } else {
      val_lo_data <- val_lo
      val_hi_data <- val_hi
    }

    # Gradient direction vectors -- shared by both rendering paths.
    # Uses `flipped_visual` so `coord_flip()` rotates the gradient with
    # the rendered polygon.
    if (flipped_visual) {
      gx1 <- 0
      gy1 <- 0.5
      gx2 <- 1
      gy2 <- 0.5
    } else {
      gx1 <- 0.5
      gy1 <- 0
      gx2 <- 0.5
      gy2 <- 1
    }

    # Layer-wide max |value| in DATA space, stamped by draw_layer (also
    # in data space). The %||% fallback covers direct draw_group
    # invocation (e.g. tests, or `alpha_scope = "group"` where draw_layer
    # leaves the slot unstamped and each ridge normalises to itself).
    max_abs <- data$global_max_abs[1L] %||%
      max(abs(val_lo_data), abs(val_hi_data))
    if (!is.finite(max_abs) || max_abs == 0) {
      max_abs <- 1
    }

    # Case flags. `is_degen` and `zero_npc` use PANEL-space val_lo/val_hi
    # (they position the gradient anchor in NPC).
    is_degen <- val_hi <= val_lo
    zero_npc <- if (is_degen) {
      0.5
    } else {
      max(0, min(1, -val_lo / (val_hi - val_lo)))
    }

    # Alpha at each extreme, proportional to data-space |val| / data-space
    # max. Numerator and denominator must live in the same space; under
    # non-linear value scales we use `val_lo_data` / `val_hi_data` so the
    # ratio reflects data magnitude (matching the documented `alpha_scope`
    # contract), not panel-space log values. Clamp to [0, 1] -- defensive
    # against any residual numerical overshoot.
    alpha_lo <- alpha_fade_to +
      (a_start - alpha_fade_to) * abs(val_lo_data) / max_abs
    alpha_hi <- alpha_fade_to +
      (a_start - alpha_fade_to) * abs(val_hi_data) / max_abs
    alpha_lo <- max(0, min(1, alpha_lo))
    alpha_hi <- max(0, min(1, alpha_hi))

    # Alpha is baked into the gradient or mask; clear it so the parent
    # does not apply it a second time.
    data$alpha <- NA

    # Pre-compute all three rendering tiers.  The actual device check is
    # deferred to makeContent.area_fade_grob() so that grobs copied to a
    # different device (e.g. dev.copy2pdf(), RStudio "Export > Save as PDF")
    # pick the correct tier for the *output* device, not the device that was
    # active during plot construction.
    #
    # Three tiers:
    #   1. "dest.in" compositing (ragg, svg, cairo on-screen):
    #      Full 2D gradient -- ggplot2's horizontal colour gradient combined
    #      with a vertical alpha fade via Porter-Duff compositing.
    #   2. linearGradient fallback (cairo_pdf, png, most other devices):
    #      Fill collapsed to one colour; linearGradient encodes the vertical
    #      alpha fade.
    #   3. Flat-fill fallback (base pdf(), postscript):
    #      No gradient support; plain semi-transparent fill.

    fill_col <- data$fill[1L]
    has_multi_fill <- length(unique(data$fill[!is.na(data$fill)])) > 1L

    # Transform anchor (y = 0) and extremes to panel NPC coordinates.
    # `ref_df` construction follows `flipped_aes` (which DATA column carries
    # the value); `pos_npc` extraction follows `flipped_visual` (which
    # PANEL axis the values landed on after `coord$transform()` --
    # `coord_flip()` swaps them).
    #
    # We only read the NPC coordinate of the *value* axis (the one we fade
    # along).  The other axis (`data$x[1L]` / `data$y[1L]`) is filler --
    # coord$transform needs both columns present, but under the linear
    # Cartesian coords where this code runs (polar falls back earlier) the
    # axes transform independently, so the filler value cannot influence
    # pos_npc.  `data` has already been through setup_data, which drops
    # NA-only groups, so `data$x[1L]` / `data$y[1L]` is guaranteed non-NA.
    if (flipped_aes) {
      ref_df <- data.frame(x = c(val_lo, 0, val_hi), y = data$y[1L])
    } else {
      ref_df <- data.frame(x = data$x[1L], y = c(val_lo, 0, val_hi))
    }
    ref_npc <- coord$transform(ref_df, panel_params)
    pos_npc <- if (flipped_visual) {
      pmax(0, pmin(1, ref_npc$x))
    } else {
      pmax(0, pmin(1, ref_npc$y))
    }

    # --- Build polygon grob from parent ------------------------------------
    grob <- ggplot2::ggproto_parent(ggplot2::GeomArea, self)$draw_group(
      data,
      panel_params,
      coord,
      flipped_aes = flipped_aes,
      outline.type = outline.type,
      ...
    )

    # Delegate tier construction to shared helper; makeContent() picks the
    # rendering tier at actual draw time based on the output device.
    .build_area_fade_grob(
      poly_grob = grob,
      fill_col = fill_col,
      has_multi_fill = has_multi_fill,
      a_start = a_start,
      alpha_fade_to = alpha_fade_to,
      alpha_lo = alpha_lo,
      alpha_hi = alpha_hi,
      anchor_bbox = zero_npc,
      pos_npc = pos_npc,
      gx1 = gx1,
      gy1 = gy1,
      gx2 = gx2,
      gy2 = gy2,
      is_degen = is_degen
    )
  }
)

#' @title Areas, Densities, and Frequency Polygons with Fading Gradient
#' @description
#' `geom_area_fade()` behaves like [ggplot2::geom_area()] but uses [grid::linearGradient()]
#' to create area plots. The gradient is always anchored at `y = 0`: maximum
#' transparency there, fading to opaque at the data values. Opacity scales
#' with the absolute distance from zero, so equal `|y|` values always receive
#' the same alpha -- full opacity is reached only at the extreme with the largest
#' absolute value. This works for positive values, negative values, and groups
#' that cross zero (where a three-stop gradient is used).
#'
#' When `fill` is mapped to a variable (e.g. `aes(fill = z)`), the geom
#' combines the horizontal colour gradient produced by ggplot2 with the
#' vertical alpha fade, creating a two-dimensional gradient effect. This
#' requires a device that supports Porter-Duff compositing
#' (e.g. [ragg::agg_png()], [grDevices::svg()]). On unsupported devices the
#' geom falls back to a single-colour vertical fade and emits an informational
#' message.
#'
#' @section Coordinate systems:
#' `geom_area_fade()`, `geom_density_fade()`, and `geom_freqpoly_fade()`
#' only support linear gradients. When used with [ggplot2::coord_polar()] or
#' [ggplot2::coord_radial()], they fall back to standard area rendering
#' (equivalent to [ggplot2::geom_area()]), which means no gradient fill is
#' added. A warning is emitted in this case.
#'
#' @section alpha_scope = "global" under faceting:
#' `alpha_scope = "global"` ties opacity to absolute height across the whole
#' layer, so two ridges / areas / bars of equal height render at equal
#' alpha regardless of which panel they're in. This is meaningful only when
#' panels share a common y scale. Under
#' `facet_wrap(scales = "free_y")` (or `facet_grid(rows = ..., scales = "free")`)
#' each panel rescales y independently, so the visual height of a shape no
#' longer reflects its data height; the alpha encoding then conflicts with
#' what the eye reads from the panel size. For comparable alpha across
#' free-y panels you have two options: stick to the default `scales = "fixed"`,
#' or accept that under free scales `alpha_scope = "group"` is the more
#' honest choice (each shape independently uses its own alpha range).
#'
#' @section Legend key under coord_flip:
#' The legend key glyph always shows the canonical (data-axis) fade
#' direction -- vertical for the default orientation, horizontal under
#' `orientation = "y"`. Under [ggplot2::coord_flip()] the rendered geom
#' rotates correctly but the legend key does *not*: ggplot2's legend
#' builder is coord-independent by design (`draw_key` has no access to
#' the coord). For a legend key that matches a horizontal layout, prefer
#' `aes(y = ...)` with auto-detected `orientation = "y"` over
#' `aes(x = ...) + coord_flip()`.
#'
#' @concept fading gradient
#' @concept area plot
#'
#' @aesthetics GeomAreaFade
#'
#' @seealso
#'   [ggplot2::geom_area()], [ggplot2::geom_density()], [ggplot2::geom_freqpoly()].
#'
#' @inheritSection ggplot2::geom_area Orientation
#'
#' @inheritParams ggplot2::geom_area
#' @param alpha_fade_to A single finite number between 0 and 1. The alpha value
#'   at `y = 0` (the baseline). Defaults to `0` (fully transparent).
#' @param alpha_scope How to scale alpha across groups. `"global"` (default)
#'   computes the maximum absolute y value across **all** groups in the panel so
#'   that equal `|y|` always maps to equal alpha. `"group"` computes the maximum
#'   per group, giving each group the full alpha range independently -- useful
#'   with `position = "identity"` when groups have very different amplitudes.
#' @param outline.type Which edges of the area to draw an outline on. One of
#'   `"upper"` (default), `"lower"`, `"both"` (`"upper"` and `"lower"`),
#'   `"full"` (closed polygon outline), or `"none"`. When no `colour`
#'   is specified explicitly the outline inherits the `fill` colour.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @references
#' Murrell, P. (2021). "Luminance Masks in R Graphics." Technical Report
#' 2021-04, Department of Statistics, The University of Auckland. Version 1.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/masks/masks.html}
#'
#' Murrell, P. (2022). "Vectorised Pattern Fills in R Graphics." Technical
#' Report 2022-01, Department of Statistics, The University of Auckland.
#' Version 1.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/vecpat/vecpat.html}
#'
#' Murrell, P., Pedersen, T. L., and Skintzos, P. (2023). "Porter-Duff
#' Compositing Operators in R Graphics." Department of Statistics, The
#' University of Auckland. Version 1.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/compositing/compositing.html}
#'
#' Murrell, P. (2023). "Groups, Compositing Operators, and Affine
#' Transformations in R Graphics." Technical Report 2021-02, Department of
#' Statistics, The University of Auckland. Version 3.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html}
#'
#' @export
#' @examples
#' library(ggplot2)
#' df1 <- data.frame(
#'   g = c("a", "a", "a", "b", "b", "b"),
#'   x = c(1, 3, 5, 2, 4, 6),
#'   y = c(2, 5, 1, 3, 6, 7)
#' )
#'
#' a <- ggplot(df1, aes(x, y, fill = g)) +
#'   theme_minimal()
#'
#' # Default behaviour: opaque at data line, transparent at y = 0
#' # the outline colour remains unaffected
#' a + geom_area_fade()
#'
#' # Change overall opacity
#' a + geom_area_fade(alpha = .25)
#'
#' # Keep some opacity at the baseline
#' a + geom_area_fade(alpha_fade_to = .25)
#'
#' # Suppress the default upper outline
#' a + geom_area_fade(outline.type = "none")
#'
#' # Closed outline (all four edges)
#' a + geom_area_fade(outline.type = "full")
#'
#' # Horizontal orientation
#' a + geom_area_fade(aes(y, x), orientation = "y")
#'
#' # Disable stat alignment (useful when x values are already aligned)
#' a + geom_area_fade(stat = "identity")
#'
#' # Draw upper and lower outlines (no left/right edges)
#' a + geom_area_fade(outline.type = "both", stat = "identity")
#'
#' # Use the "alpha_scope" argument to scale the alpha
#' # value of the gradients separately for each group
#' df2 <- data.frame(
#'   g = c("a", "a", "a", "b", "b", "b"),
#'   x = c(1, 3, 5, 2, 4, 6),
#'   y = c(1, 2, 1, 9, 10, 8)
#' )
#' b <- ggplot(df2, aes(x, y, fill = g)) +
#'   theme_minimal()
#'
#' # With alpha_scope = "group", each group uses the alpha range independently
#' b + geom_area_fade(
#'   alpha_scope = "group",
#'   position = "identity"
#'   )
#'
#' # Compare with the default where small groups appear washed out
#' # next to dominant groups, especially when position = "identity"
#' b + geom_area_fade(
#'   alpha_scope = "global", # default
#'   position = "identity"
#'   )
#'
#' # Negative values are supported too:
#' # the gradient fades towards y = 0 from both sides
#' d <- ggplot(df2, aes(x, y - mean(y))) +
#'   theme_minimal()
#' d + geom_area_fade()
#'
#' # Overwrite both fill and colour
#' d + geom_area_fade(
#'   fill = "#0833F5",
#'   colour = "#d77e7b",
#'   outline.type = "lower"
#'   )
#'
#' # A 2D-gradient is produced when fill is mapped to a variable
#' # this may not work on all graphic devices, see vignette for details
#' d + geom_area_fade(
#'   aes(fill = y),
#'   colour = "#333333",
#'   outline.type = "both"
#'   )
#'
geom_area_fade <- make_constructor(
  GeomAreaFade,
  stat = "align",
  position = "stack",
  alpha_fade_to = 0,
  alpha_scope = "global",
  orientation = NA
)

