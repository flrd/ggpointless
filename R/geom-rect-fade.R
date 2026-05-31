# Walk a grob tree and set gp$col on every leaf grob to `colour`.
# Used to inject the `fill` aesthetic colour into the tile grob of a
# user-supplied grid::pattern so hatch lines track the mapped fill.
# gp$fill is intentionally left unchanged (tile backgrounds stay
# transparent).
#' @noRd
#' @keywords internal
.recolour_grob_col <- function(grob, colour) {
  if (inherits(grob, "gTree")) {
    grob$children <- do.call(
      grid::gList,
      lapply(grob$children, .recolour_grob_col, colour = colour)
    )
  } else if (!is.null(grob$gp)) {
    grob$gp$col <- colour
  }
  grob
}

# Recolour the tile grob inside a GridTilingPattern.
#' @noRd
#' @keywords internal
.recolour_pattern <- function(pat, colour) {
  pat$grob <- .recolour_grob_col(pat$grob, colour)
  pat
}


# Build a hatch *spec* (one or two line families) covering a rectangle.
#
# This returns a lightweight S3 list, NOT a grob. The actual segment geometry
# is generated later by `.hatch_materialize()`, called from
# `makeContent.rect_fade_pattern_grob` at draw time. Deferring to draw time
# lets us size SPAN and the line count from the rectangle's true physical
# dimensions (mm), rather than a fixed worst-case budget: for a 15 × 60 mm bar
# at 2 mm spacing that is ~32 segments per family instead of 601 -- a ~18×
# reduction in grob count.
#
# A spec (not a deferred gTree) is used because the materialised hatch becomes
# the `dst` of a Porter-Duff `groupGrob()`, and grid computes `grobCoords()` on
# that dst *before* any `makeContent()` would run -- an empty deferred gTree has
# no valid coordinates and errors. Materialising eagerly inside the parent's
# `makeContent` (where the viewport's mm size is known) keeps the dst a
# fully-populated, coords-valid gTree.
#
# Why grid::pattern() is NOT used: tiling clips every tile at its own bounds,
# so diagonal lines come out as disconnected dashes. Drawing real parallel
# lines and clipping the whole family keeps them continuous.
#' @noRd
#' @keywords internal
.hatch_grob <- function(xc, yc, w, h, colour,
                        angle = 45, crossed = FALSE,
                        spacing_mm = 2, linewidth = 0.5, linetype = 1,
                        units = "native") {
  structure(
    list(
      xc = xc, yc = yc, w = w, h = h, units = units,
      colour = colour, angle = angle, crossed = crossed,
      spacing_mm = spacing_mm, linewidth = linewidth,
      linetype = linetype
    ),
    class = "hatch_spec"
  )
}

# Materialise a `hatch_spec` into a concrete clipped gTree. MUST be called from
# within the target viewport (panel or legend key) so that `convertWidth/Height`
# resolve the rectangle's `units` (native / npc) to physical mm.
#' @noRd
#' @keywords internal
.hatch_materialize <- function(spec, clip = "on") {
  w_mm <- abs(grid::convertWidth(
    grid::unit(spec$w, spec$units), "mm", valueOnly = TRUE
  ))
  h_mm <- abs(grid::convertHeight(
    grid::unit(spec$h, spec$units), "mm", valueOnly = TRUE
  ))

  if (w_mm <= 0 || h_mm <= 0) return(ggplot2::zeroGrob())

  # Draw in a viewport whose native scale IS millimetres, isotropically:
  # width = w_mm mm spans an xscale of width w_mm, so 1 native unit = 1 mm on
  # both axes (the angle stays visually true). Segment endpoints are then plain
  # numerics in a single coordinate system -- much cheaper for grid to resolve
  # than `unit(0.5, "npc") + unit(.., "mm")` compound units, which force a
  # per-segment solve of two unit systems.
  #
  # Each line family is sized to ITS orientation, not the box diagonal: the
  # perpendicular half-extent of the w x h box (`hp`) bounds the number of
  # lines, the parallel half-extent (`lp`) bounds their length. For a thin
  # dodged bar with vertical hatch this yields a handful of lines instead of
  # one-per-pitch across the diagonal (most of which would be clipped away).
  s  <- spec$spacing_mm
  cap <- 2000L
  family <- function(ang) {
    a   <- ang * pi / 180
    dx  <- cos(a);  dy  <- sin(a)
    nx  <- -dy;     ny  <- dx
    lp  <- (abs(w_mm * dx) + abs(h_mm * dy)) / 2 + s   # half-length (parallel)
    hp  <- (abs(w_mm * nx) + abs(h_mm * ny)) / 2 + s   # half-extent (perp.)
    n   <- min(ceiling(hp / s), cap)
    off <- seq.int(-n, n) * s
    grid::segmentsGrob(
      x0 = off * nx - lp * dx,
      y0 = off * ny - lp * dy,
      x1 = off * nx + lp * dx,
      y1 = off * ny + lp * dy,
      default.units = "native",
      gp = ggplot2::gg_par(
        col = spec$colour, lwd = spec$linewidth, lty = spec$linetype
      )
    )
  }

  children <- if (spec$crossed) {
    grid::gList(family(spec$angle), family(spec$angle + 90))
  } else {
    grid::gList(family(spec$angle))
  }

  grid::gTree(
    children = children,
    vp = grid::viewport(
      x = grid::unit(spec$xc, spec$units), y = grid::unit(spec$yc, spec$units),
      width  = grid::unit(spec$w, spec$units),
      height = grid::unit(spec$h, spec$units),
      xscale = c(-w_mm / 2, w_mm / 2),
      yscale = c(-h_mm / 2, h_mm / 2),
      just = "centre", clip = clip
    )
  )
}


# Resolve the pattern stroke colour, one value per row.
#
# Pattern colour (hatch lines) always follows the `fill` aesthetic, falling
# back to `colour` only when `fill` is unset. The `explicit` argument is kept
# for the (rare) grob-pattern path but is always NULL for hatch() since it
# exposes no colour param.
#' @noRd
#' @keywords internal
.hatch_resolve_colour <- function(data, explicit) {
  if (!is.null(explicit)) return(rep(explicit, length.out = nrow(data)))
  fill   <- data$fill
  colour <- data$colour
  if (!is.null(fill)   && !all(is.na(fill)))   return(fill)
  if (!is.null(colour) && !all(is.na(colour))) return(colour)
  fill
}

# Wrap a user-supplied grob so it is clipped to a single rectangle
# (`xc`/`yc`/`w`/`h`, in `units`).  Unlike grid::pattern() tiling -- which
# breaks continuous lines into per-tile dashes -- clipping draws the grob
# once and shows only the part inside the rectangle, so continuous custom
# hatching (e.g. wavy lines built in "snpc") stays continuous.  The grob's
# stroke colour is recoloured to the mapped `fill`.
#' @noRd
#' @keywords internal
.clip_pattern_grob <- function(grob, xc, yc, w, h, colour, units = "native",
                               clip = "on") {
  grid::gTree(
    children = grid::gList(.recolour_grob_col(grob, colour)),
    vp = grid::viewport(
      x = xc, y = yc, width = w, height = h,
      just = "centre", default.units = units, clip = clip
    )
  )
}


# Deferred grob for pattern + fade via Porter-Duff "dest.in" compositing.
#
# Ingredients are stored separately so makeContent can apply radius
# clamping (requires physical dimensions) to the dst grobs before
# assembling the groupGrobs.
#
# Tiers:
#   Tier 1 -- dest.in compositing: pattern fill x alpha_ref alpha
#   Tier 2 -- flat semi-transparent fill (no compositing support)
#' @noRd
#' @keywords internal
.rect_fade_pattern_grob <- function(dst_glist, alpha_ref_glist,
                                     outline_glist, flat_glist) {
  grid::gTree(
    dst_glist       = dst_glist,
    alpha_ref_glist = alpha_ref_glist,
    outline_glist   = outline_glist,
    flat_glist      = flat_glist,
    cl = "rect_fade_pattern_grob"
  )
}

#' @export
makeContent.rect_fade_pattern_grob <- function(x) {
  dev_name <- names(grDevices::dev.cur())
  no_comp  <- dev_name %in% c("pdf", "cairo_pdf", "postscript")
  can_comp <- !no_comp && .has_compositing_op("dest.in")

  if (!can_comp) {
    .queue_rect_fade_pattern_no_composite("geom_rect_fade")
    grobs <- .clamp_roundrect_radius(x$flat_glist, arg = "radius")
    return(grid::setChildren(x, grobs))
  }

  # Clamp radius on dst (pattern fill), the alpha mask, and the outline so all
  # three share the same rounded shape. The mask in particular MUST match the
  # outline: it is what clips the hatch to rounded corners (dest.in), and an
  # unclamped radius on a short bar degenerates into a lens, mis-shaping the
  # pattern. `quiet = TRUE` on dst/mask so only the outline pass reports.
  dst_cl       <- .clamp_roundrect_radius(x$dst_glist,       arg = "radius", quiet = TRUE)
  alpha_ref_cl <- .clamp_roundrect_radius(x$alpha_ref_glist, arg = "radius", quiet = TRUE)
  outline_cl   <- .clamp_roundrect_radius(x$outline_glist,   arg = "radius")

  # One dest.in group per rectangle. Batching the whole layer into a single
  # group is NOT possible: grid's group compositing collapses a multi-child
  # gTree dst/src to an empty render for spatially separated content, so each
  # rectangle must be composited on its own.
  n      <- length(dst_cl)
  result <- vector("list", n)
  for (i in seq_len(n)) {
    # Hatch dsts arrive as lightweight specs; materialise them here, inside
    # the panel / key viewport, so SPAN is sized from the rect's true mm
    # dimensions and the resulting dst gTree has valid grobCoords.
    dst <- dst_cl[[i]]
    if (inherits(dst, "hatch_spec")) dst <- .hatch_materialize(dst)
    comp <- grid::groupGrob(alpha_ref_cl[[i]], op = "dest.in", dst = dst)
    ol   <- outline_cl[[i]]
    result[[i]] <- if (inherits(ol, "zeroGrob")) {
      comp
    } else {
      grid::gTree(children = grid::gList(comp, ol))
    }
  }
  grid::setChildren(x, do.call(grid::gList, result))
}


# Deferred grob for device-aware rectangle rendering.
#
# Two tiers (rectangles never need the compositing path -- each rect has a
# single fill colour):
#   Tier 1 -- linearGradient fill (ragg, cairo, svg, png, ...)
#   Tier 2 -- flat semi-transparent (base pdf(), postscript)
#' @noRd
#' @keywords internal
.rect_fade_grob <- function(gradient_glist, flat_glist) {
  grid::gTree(
    gradient_glist = gradient_glist,
    flat_glist = flat_glist,
    cl = "rect_fade_grob"
  )
}

#' @export
makeContent.rect_fade_grob <- function(x) {
  dev_name <- names(grDevices::dev.cur())
  no_gradient <- dev_name %in% c("pdf", "postscript")

  if (no_gradient) {
    .queue_rect_col_no_gradient("geom_rect_fade")
    grobs <- x$flat_glist
  } else {
    grobs <- x$gradient_glist
  }

  grobs <- .clamp_roundrect_radius(grobs, arg = "radius")
  grid::setChildren(x, grobs)
}

# Deferred grob for polar rectangles with a radial alpha gradient.
#
# Build one polar ring's pattern fill at draw time: a panel-covering pattern
# (hatch lines / user grob / tiled grid::pattern), radially faded via dest.in
# compositing and clipped to the wedge polygon. MUST run inside the panel
# viewport because hatch materialisation converts npc -> mm.
#
# `ring` is an ingredient list: poly_grob, r_in, r_out, a_inner, a_outer,
# fill_col (assembled eagerly in the polar draw path; only the hatch dst needs
# this deferred, draw-time context).
# Build the panel-covering pattern fill (hatch lines / user grob / tiled
# grid::pattern), recoloured to the wedge's fill. Shared by the radial
# (compositing) and flat (clip-only) ring builders.
#' @noRd
#' @keywords internal
# `clip` is the clip region for the pattern's own viewport. The radial path
# leaves it "on" (the group handles wedge clipping); the flat path passes the
# wedge polygon so the pattern clips to the wedge in a SINGLE viewport (nesting
# a second clip viewport would reset, not intersect, the wedge clip).
#' @noRd
#' @keywords internal
.polar_pattern_dst <- function(pattern, fill_col, clip = "on") {
  if (inherits(pattern, "ggpointless_pattern")) {
    .hatch_materialize(
      .hatch_grob(
        xc = 0.5, yc = 0.5, w = 1, h = 1, units = "npc",
        colour     = fill_col,
        angle      = pattern$angle,
        crossed    = identical(pattern$style, "crossed"),
        spacing_mm = as.numeric(grid::convertUnit(pattern$spacing, "mm"))
      ),
      clip = clip
    )
  } else if (grid::is.grob(pattern)) {
    .clip_pattern_grob(pattern, 0.5, 0.5, 1, 1, fill_col, units = "npc",
                       clip = clip)
  } else {
    tile <- grid::rectGrob(gp = grid::gpar(
      fill = .recolour_pattern(pattern, fill_col), col = NA
    ))
    if (identical(clip, "on")) {
      tile
    } else {
      grid::gTree(children = grid::gList(tile),
                  vp = grid::viewport(clip = clip))
    }
  }
}

# Radially-faded pattern ring: pattern x radial alpha mask via dest.in,
# clipped to the wedge. Needs clipping paths, a radial gradient, AND Porter-Duff
# compositing.
#' @noRd
#' @keywords internal
.polar_pattern_ring <- function(pattern, ring) {
  dst <- .polar_pattern_dst(pattern, ring$fill_col)

  # Radial alpha mask (black; only its alpha channel is used by dest.in).
  alpha_mask <- grid::rectGrob(gp = grid::gpar(
    fill = grid::radialGradient(
      colours = ggplot2::alpha("black", c(ring$a_inner, ring$a_outer)),
      cx1 = 0.5, cy1 = 0.5, r1 = ring$r_in,
      cx2 = 0.5, cy2 = 0.5, r2 = ring$r_out
    ),
    col = NA
  ))

  comp <- grid::groupGrob(alpha_mask, op = "dest.in", dst = dst)
  clipped <- grid::gTree(
    children = grid::gList(comp),
    vp = grid::viewport(clip = ring$poly_grob)
  )
  .polar_ring_with_outline(clipped, ring$poly_grob)
}

# Flat (uniform-alpha) pattern ring for the angular case: the fade can't be
# expressed (no conic gradient), but the pattern is mm-space, so just clip it to
# the wedge at a single alpha. Needs ONLY clipping paths -- no radial gradient,
# no compositing -- so it renders on far more devices (svglite, RStudioGD, ...)
# than the radial path.
#' @noRd
#' @keywords internal
.polar_pattern_ring_flat <- function(pattern, ring) {
  # The wedge clip must live on an OUTER viewport in panel coordinates -- a clip
  # path set on the pattern's own mm-scaled viewport would resolve in the wrong
  # coordinate system. So build the pattern's inner viewport with
  # `clip = "inherit"` (it keeps the enclosing wedge clip instead of resetting
  # it to its own rectangle), then wrap it in the wedge-clipping viewport.
  dst <- .polar_pattern_dst(pattern, ring$fill_col, clip = "inherit")
  fill_grob <- grid::gTree(
    children = grid::gList(dst),
    vp = grid::viewport(clip = ring$poly_grob),
    gp = grid::gpar(alpha = ring$a_outer)  # a_inner == a_outer in the flat case
  )
  .polar_ring_with_outline(fill_grob, ring$poly_grob)
}

# Draw the wedge border on top of a clipped polar fill. The fill is clipped to
# the wedge polygon, which also clips its own stroke to half-width; drawing the
# polygon outline separately (unclipped, fill = NA) restores a crisp border.
# A no-op when `colour` is NA (the default), so default-colour plots are
# byte-identical to the outline-free rendering.
#' @noRd
#' @keywords internal
.polar_ring_with_outline <- function(fill_grob, poly_grob) {
  col <- poly_grob$gp$col
  if (is.null(col) || all(is.na(col))) {
    return(fill_grob)
  }
  outline <- poly_grob
  outline$gp$fill <- NA
  grid::gTree(children = grid::gList(fill_grob, outline))
}

# Polar renders require clipping paths (to shape the fill to the annular
# segment) AND a radialGradient. Pattern fills additionally need Porter-Duff
# "dest.in" compositing. Devices missing the required capabilities fall back to
# flat semi-transparent annular segments.
#
# `pattern` + `rings` are non-NULL only when a pattern fill is requested; each
# ring's grob is assembled here (not at build time) so hatch materialisation
# sees the panel's physical size.
#' @noRd
#' @keywords internal
.rect_fade_polar_grob <- function(gradient_glist, flat_glist,
                                  pattern = NULL, rings = NULL,
                                  pattern_radial = TRUE) {
  grid::gTree(
    gradient_glist = gradient_glist,
    flat_glist = flat_glist,
    pattern = pattern,
    rings = rings,
    pattern_radial = pattern_radial,
    cl = "rect_fade_polar_grob"
  )
}

#' @export
makeContent.rect_fade_polar_grob <- function(x) {
  dev_name <- names(grDevices::dev.cur())
  caps <- tryCatch(grDevices::dev.capabilities(), error = \(e) list())
  base_ok <- !dev_name %in% c("pdf", "postscript") &&
    isTRUE(caps[["clippingPaths"]]) &&
    "RadialGradient" %in% caps[["patterns"]]

  if (!is.null(x$pattern)) {
    clip_ok <- !dev_name %in% c("pdf", "postscript") &&
      isTRUE(caps[["clippingPaths"]])

    if (isTRUE(x$pattern_radial)) {
      # Radial fade: needs clip + radial gradient + dest.in compositing.
      ok  <- base_ok && .has_compositing_op("dest.in", caps)
      fun <- .polar_pattern_ring
    } else {
      # Angular case: uniform alpha, so only clipping is needed (the pattern is
      # mm-space; the fade is dropped). Renders on far more devices.
      ok  <- clip_ok
      fun <- .polar_pattern_ring_flat
    }

    if (ok) {
      grobs <- do.call(grid::gList, lapply(seq_along(x$rings), function(i) {
        ring <- x$rings[[i]]
        if (is.null(ring)) x$flat_glist[[i]] else fun(x$pattern, ring)
      }))
    } else {
      .queue_rect_col_polar_no_clip_pattern("geom_rect_fade")
      grobs <- x$flat_glist
    }
    return(grid::setChildren(x, grobs))
  }

  if (base_ok) {
    grobs <- x$gradient_glist
  } else {
    .queue_rect_col_polar_no_clip_pattern("geom_rect_fade")
    grobs <- x$flat_glist
  }

  grid::setChildren(x, grobs)
}

# For a rect under CoordPolar / CoordRadial, build the arc-interpolated
# polygon (via GeomPolygon's non-linear path, which internally calls
# coord_munch), then lay a panel-sized radialGradient rectGrob clipped to
# that polygon via viewport(clip = poly_grob) -- mirroring the pattern used
# by .draw_panel_bar_fade_polar(). `radius` (rounded corners) is
# geometrically meaningless on an arc and is ignored here.
#' @noRd
#' @keywords internal
.draw_panel_rect_fade_polar <- function(
  data,
  panel_params,
  coord,
  alpha_fade_to,
  fade_direction,
  lineend,
  linejoin,
  pattern = NULL,
  radial = TRUE
) {
  theta <- coord$theta %||% "x"
  n <- nrow(data)

  gradient_list <- vector("list", n)
  flat_list <- vector("list", n)
  rings <- if (is.null(pattern)) NULL else vector("list", n)

  for (i in seq_len(n)) {
    a_start <- data$alpha[i]
    if (is.na(a_start)) {
      a_start <- 1
    }
    fill_col <- data$fill[i]

    # Under theta = "x", y is the radial axis. fade_direction = "vertical"
    # means ymax (outer ring) is opaque and ymin (inner ring) fades.
    # Under theta = "y", x is the radial axis. fade_direction = "horizontal"
    # means xmin (inner ring) is opaque and xmax (outer ring) fades.
    #
    # `radial = FALSE` is the angular case: the fade would run around theta,
    # which grid can't express (no conic gradient). We still draw a `pattern`
    # here (it lives in mm space, independent of the fade), just at a uniform
    # alpha -- equal inner/outer stops make the radial mask flat.
    if (!radial) {
      a_inner <- a_start
      a_outer <- a_start
    } else if (identical(theta, "x")) {
      a_inner <- alpha_fade_to
      a_outer <- a_start
    } else {
      a_inner <- a_start
      a_outer <- alpha_fade_to
    }

    # Build the 4-corner rect polygon and let GeomPolygon munch it into an
    # arc polygon.  group = 1 so the corners form a single ring.  alpha = 1
    # so the polygon carries a solid fill; the per-rect alpha is baked into
    # the radialGradient colour stops below.
    poly_data <- data[rep(i, 4L), , drop = FALSE]
    poly_data$x <- c(
      data$xmin[i],
      data$xmax[i],
      data$xmax[i],
      data$xmin[i]
    )
    poly_data$y <- c(
      data$ymax[i],
      data$ymax[i],
      data$ymin[i],
      data$ymin[i]
    )
    poly_data$group <- 1L
    poly_data$alpha <- 1

    poly_grob <- ggplot2::GeomPolygon$draw_panel(
      poly_data,
      panel_params,
      coord,
      lineend = lineend,
      linejoin = linejoin
    )

    if (!inherits(poly_grob, "polygon")) {
      gradient_list[[i]] <- ggplot2::zeroGrob()
      flat_list[[i]] <- ggplot2::zeroGrob()
      next
    }

    xs <- as.numeric(poly_grob$x)
    ys <- as.numeric(poly_grob$y)
    radii <- sqrt((xs - 0.5)^2 + (ys - 0.5)^2)
    r_in <- min(radii, na.rm = TRUE)
    r_out <- max(radii, na.rm = TRUE)

    mid_alpha <- (a_inner + a_outer) / 2
    flat_grob <- poly_grob
    flat_grob$gp$fill <- ggplot2::alpha(fill_col, mid_alpha)

    # Degenerate ring (zero height / zero width collapsed to a point) -- fall
    # back to a solid mid-alpha polygon.
    if (!is.finite(r_in) || !is.finite(r_out) || r_out <= r_in) {
      gradient_list[[i]] <- flat_grob
      flat_list[[i]] <- flat_grob
      next
    }

    gradient_rect <- grid::rectGrob(
      gp = grid::gpar(
        fill = grid::radialGradient(
          colours = c(
            ggplot2::alpha(fill_col, a_inner),
            ggplot2::alpha(fill_col, a_outer)
          ),
          cx1 = 0.5,
          cy1 = 0.5,
          r1 = r_in,
          cx2 = 0.5,
          cy2 = 0.5,
          r2 = r_out
        ),
        col = NA
      )
    )

    clip_vp <- grid::viewport(clip = poly_grob)

    gradient_list[[i]] <- .polar_ring_with_outline(
      grid::gTree(
        children = grid::gList(gradient_rect),
        vp = clip_vp,
        name = paste0("rect_fade_polar_ring_", i)
      ),
      poly_grob
    )
    flat_list[[i]] <- flat_grob

    if (!is.null(pattern)) {
      rings[[i]] <- list(
        poly_grob = poly_grob, r_in = r_in, r_out = r_out,
        a_inner = a_inner, a_outer = a_outer, fill_col = fill_col
      )
    }
  }

  .rect_fade_polar_grob(
    do.call(grid::gList, gradient_list),
    do.call(grid::gList, flat_list),
    pattern = pattern,
    rings = rings,
    pattern_radial = radial
  )
}

# Legend key -- rounded rect with alpha gradient (vertical or horizontal).
# When `pattern` is provided, a deferred .rect_fade_pattern_grob is
# returned so the key picks the correct rendering tier at draw time.
#' @noRd
#' @keywords internal
.draw_key_rect_fade <- function(data, params, size) {
  radius     <- .validate_radius(params$radius)
  fill_colour <- data$fill %||% "grey35"
  a_start    <- data$alpha %||% 1
  a_end      <- params$alpha_fade_to %||% 0
  fade_dir   <- params$fade_direction %||% "vertical"
  pattern    <- params$pattern

  # Cap the key's corner radius (see .draw_key_col_fade): a large bar radius on
  # the tiny key cell would round it into a near-circle that cuts across the
  # pattern. Keep the outline a minimal rounded-rect frame around the pattern.
  key_radius <- if (as.numeric(grid::convertUnit(radius, "pt")) > 0) {
    grid::unit(min(as.numeric(grid::convertUnit(radius, "pt")), 2.5), "pt")
  } else {
    radius
  }

  if (identical(fade_dir, "horizontal")) {
    gx1 <- 0; gy1 <- 0.5; gx2 <- 1; gy2 <- 0.5
    # left (x=0) opaque, right (x=1) transparent
    a_grad_0 <- a_start; a_grad_1 <- a_end
  } else {
    gx1 <- 0.5; gy1 <- 0; gx2 <- 0.5; gy2 <- 1
    # bottom (y=0) transparent, top (y=1) opaque
    a_grad_0 <- a_end; a_grad_1 <- a_start
  }

  if (!is.null(pattern)) {
    dst <- if (inherits(pattern, "ggpointless_pattern")) {
      # hatch() spec: clipped line family filling the key cell.
      .hatch_grob(
        xc = 0.5, yc = 0.5, w = 1, h = 1,
        colour     = pattern$colour %||% fill_colour,
        angle      = pattern$angle,
        crossed    = identical(pattern$style, "crossed"),
        spacing_mm = as.numeric(grid::convertUnit(pattern$spacing, "mm")),
        linewidth  = 0.5,
        linetype   = 1L,
        units = "npc"
      )
    } else if (grid::is.grob(pattern)) {
      .clip_pattern_grob(
        pattern, xc = 0.5, yc = 0.5, w = 1, h = 1,
        colour = fill_colour, units = "npc"
      )
    } else {
      recoloured <- .recolour_pattern(pattern, fill_colour)
      grid::roundrectGrob(
        r = key_radius,
        gp = ggplot2::gg_par(fill = recoloured, col = NA)
      )
    }
    alpha_ref <- grid::rectGrob(
      gp = grid::gpar(
        fill = grid::linearGradient(
          colours = c(
            ggplot2::alpha("black", a_grad_0),
            ggplot2::alpha("black", a_grad_1)
          ),
          x1 = gx1, y1 = gy1, x2 = gx2, y2 = gy2
        ),
        col = NA
      )
    )
    outline <- if (is.na(data$colour %||% NA)) {
      ggplot2::zeroGrob()
    } else {
      grid::roundrectGrob(
        r = key_radius,
        gp = ggplot2::gg_par(col = data$colour, fill = NA)
      )
    }
    flat <- grid::roundrectGrob(
      r = key_radius,
      gp = ggplot2::gg_par(
        fill = ggplot2::alpha(fill_colour, (a_start + a_end) / 2),
        col  = data$colour %||% NA
      )
    )
    return(.rect_fade_pattern_grob(
      list(dst),
      grid::gList(alpha_ref),
      grid::gList(outline),
      grid::gList(flat)
    ))
  }

  grad <- grid::linearGradient(
    colours = c(
      ggplot2::alpha(fill_colour, a_grad_0),
      ggplot2::alpha(fill_colour, a_grad_1)
    ),
    x1 = gx1, y1 = gy1, x2 = gx2, y2 = gy2
  )
  grid::roundrectGrob(
    r = radius,
    gp = ggplot2::gg_par(
      fill = grad,
      col  = data$colour %||% NA
    )
  )
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRectFade <- ggplot2::ggproto(
  "GeomRectFade",
  ggplot2::GeomRect,

  extra_params = c(
    ggplot2::GeomRect$extra_params,
    "alpha_fade_to",
    "fade_direction",
    "radius",
    "pattern"
  ),

  draw_key = .draw_key_rect_fade,

  # GeomRect$setup_data (ggplot2 v4) uses resolve_rect() to convert
  # (x, y, width, height) -> (xmin, xmax, ymin, ymax), but guards the
  # assignment with `lengths(result) > 1`.  For single-row input the
  # result vectors have length 1, the guard is FALSE, and corners are
  # never written -- a silent ggplot2 bug.  Call the parent first (which
  # handles multi-row data correctly), then fill in any still-missing
  # corners explicitly.
  setup_data = \(self, data, params) {
    data <- ggplot2::ggproto_parent(ggplot2::GeomRect, self)$setup_data(
      data,
      params
    )
    if (is.null(data$xmin) && all(c("x", "width") %in% names(data))) {
      data$xmin <- data$x - data$width / 2
      data$xmax <- data$x + data$width / 2
    }
    if (is.null(data$ymin) && all(c("y", "height") %in% names(data))) {
      data$ymin <- data$y - data$height / 2
      data$ymax <- data$y + data$height / 2
    }
    data
  },

  setup_params = \(self, data, params) {
    params <- ggplot2::ggproto_parent(ggplot2::GeomRect, self)$setup_params(
      data,
      params
    )

    params$alpha_fade_to <- params$alpha_fade_to %||% 0

    .check_alpha_fade_to(params$alpha_fade_to)

    params$fade_direction <- rlang::arg_match0(
      params$fade_direction %||% "vertical",
      values = c("vertical", "horizontal"),
      arg_nm = "fade_direction"
    )

    params$radius <- .validate_radius(params$radius)

    pat <- params$pattern
    # String shortcuts: `pattern = "crossed"` is sugar for
    # `hatch(style = "crossed")`, mirroring `position = "dodge"` vs
    # `position_dodge()`. Coerce before the type check below; an unknown string
    # aborts inside `.hatch_from_string()` with a "did you mean" hint.
    if (is.character(pat)) {
      pat <- .hatch_from_string(pat)
      params$pattern <- pat
    }
    if (
      !is.null(pat) &&
        !inherits(pat, "ggpointless_pattern") &&
        !inherits(pat, "GridTilingPattern") &&
        !grid::is.grob(pat)
    ) {
      presets <- .hatch_presets  # local: cli reads a leading-dot name as a span
      cli::cli_abort(
        c(
          "{.arg pattern} must be a {.fn hatch} specification, a preset \\
           string ({.or {.val {presets}}}), a {.cls grob} to clip to \\
           each rectangle, or a {.cls GridTilingPattern} from \\
           {.fn grid::pattern}.",
          "x" = "Got {.obj_type_friendly {pat}}."
        ),
        class = "ggpointless_rect_fade_pattern_type"
      )
    }

    params
  },

  draw_panel = \(
    self,
    data,
    panel_params,
    coord,
    lineend = "butt",
    linejoin = "mitre",
    alpha_fade_to = 0,
    fade_direction = "vertical",
    radius = NULL,
    pattern = NULL
  ) {
    .check_panel_range(panel_params, "geom_rect_fade")
    radius <- .validate_radius(radius)

    is_polar <- inherits(coord, "CoordPolar") ||
      inherits(coord, "CoordRadial")

    if (is_polar) {
      theta <- coord$theta %||% "x"
      radial <- (identical(theta, "x") &&
        identical(fade_direction, "vertical")) ||
        (identical(theta, "y") && identical(fade_direction, "horizontal"))

      if (nrow(data) == 0L) {
        return(ggplot2::zeroGrob())
      }

      if (.is_uniform_alpha(data, alpha_fade_to) && is.null(pattern)) {
        return(ggplot2::ggproto_parent(ggplot2::GeomRect, self)$draw_panel(
          data,
          panel_params,
          coord,
          lineend = lineend,
          linejoin = linejoin
        ))
      }

      if (radial) {
        return(.draw_panel_rect_fade_polar(
          data,
          panel_params,
          coord,
          alpha_fade_to = alpha_fade_to,
          fade_direction = fade_direction,
          lineend = lineend,
          linejoin = linejoin,
          pattern = pattern,
          radial = TRUE
        ))
      }

      # Angular fade (theta-aligned gradient): grid has no conic gradient
      # primitive, so the *fade* cannot be expressed. A `pattern` fill, however,
      # lives in mm space and is independent of the fade -- so still draw it,
      # clipped to each wedge at a uniform alpha, rather than dropping it.
      if (!is.null(pattern)) {
        cli::cli_inform(
          c(
            "i" = "{.fn geom_rect_fade}: angular fade is not yet supported in \\
                   {.pkg grid}; drawing the {.arg pattern} without a fade.",
            "i" = "For a radial fade use {.code fade_direction = \"vertical\"} \\
                   with {.code theta = \"x\"} or {.code fade_direction = \\
                   \"horizontal\"} with {.code theta = \"y\"}."
          )
        )
        return(.draw_panel_rect_fade_polar(
          data,
          panel_params,
          coord,
          alpha_fade_to = alpha_fade_to,
          fade_direction = fade_direction,
          lineend = lineend,
          linejoin = linejoin,
          pattern = pattern,
          radial = FALSE
        ))
      }

      # No pattern and no expressible fade -> plain geom_rect.
      cli::cli_inform(
        c(
          "i" = "{.fn geom_rect_fade}: angular fade is not yet supported in \\
                 {.pkg grid}.",
          "i" = "Falling back to {.fn geom_rect} (no gradient). For a radial \\
                 fade under {.fn coord_polar} / {.fn coord_radial}, use \\
                 {.code fade_direction = \"vertical\"} with {.code theta = \"x\"} \\
                 or {.code fade_direction = \"horizontal\"} with \\
                 {.code theta = \"y\"}."
        )
      )
      return(
        ggplot2::ggproto_parent(ggplot2::GeomRect, self)$draw_panel(
          data,
          panel_params,
          coord,
          lineend = lineend,
          linejoin = linejoin
        )
      )
    }

    # `coord_transform()` keeps rectangles axis-aligned (separable per axis),
    # so the gradient/pattern path handles it; only other non-linear coords
    # fall back to the plain parent. See `geom_col_fade()` for the rationale.
    if (!coord$is_linear() && !inherits(coord, "CoordTransform")) {
      .queue_rounded_corner_fallback("geom_rect_fade")
      return(
        ggplot2::ggproto_parent(ggplot2::GeomRect, self)$draw_panel(
          data,
          panel_params,
          coord,
          lineend = lineend,
          linejoin = linejoin
        )
      )
    }

    if (nrow(data) == 0L) {
      return(ggplot2::zeroGrob())
    }

    # Fast path: same logic as `geom_col_fade()`. Skip when the user
    # requested rounded corners OR a pattern fill, since the parent would
    # lose both.
    if (
      is.null(pattern) &&
      .is_uniform_alpha(data, alpha_fade_to) &&
        identical(as.numeric(radius), 0)
    ) {
      return(ggplot2::ggproto_parent(ggplot2::GeomRect, self)$draw_panel(
        data,
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin
      ))
    }

    coords <- coord$transform(data, panel_params)
    # Drop rows with non-finite rect bounds -- can occur when -Inf/Inf hits a
    # log scale (produces NaN) or any other scale that can't represent them.
    finite <- is.finite(coords$xmin) &
      is.finite(coords$xmax) &
      is.finite(coords$ymin) &
      is.finite(coords$ymax)
    n_dropped <- sum(!finite)
    if (n_dropped > 0L) {
      # Consolidated across layers / panels into one warning per render.
      .queue_rect_fade_nonfinite("geom_rect_fade", n_dropped)
      coords <- coords[finite, , drop = FALSE]
    }
    if (nrow(coords) == 0L) {
      return(ggplot2::zeroGrob())
    }
    n <- nrow(coords)

    # `fade_direction` is in data-axis semantics. Under `coord_flip()` the
    # x/y axes swap visually, so what the user called "vertical" should
    # render horizontally. Translate once here; the loop branches on the
    # rendered direction.
    rendered_dir <- if (inherits(coord, "CoordFlip")) {
      switch(
        fade_direction,
        vertical = "horizontal",
        horizontal = "vertical",
        fade_direction
      )
    } else {
      fade_direction
    }

    # For the pattern path we need four lists; for the gradient path two.
    use_pattern  <- !is.null(pattern)
    is_hatch     <- inherits(pattern, "ggpointless_pattern")
    gradient_list <- vector("list", n)
    flat_list     <- vector("list", n)
    if (use_pattern) {
      dst_list      <- vector("list", n)
      alpha_ref_list <- vector("list", n)
      outline_list  <- vector("list", n)
    }
    if (is_hatch) {
      # Resolve hatch spacing (mm) and per-row colour once for the layer.
      hatch_spacing_mm <- as.numeric(grid::convertUnit(pattern$spacing, "mm"))
      hatch_cols <- .hatch_resolve_colour(coords, NULL)
    }

    for (i in seq_len(n)) {
      a_start <- coords$alpha[i]
      if (is.na(a_start)) a_start <- 1

      fill_col <- coords$fill[i]

      # Detect reversed axes: after coord$transform, a reversed scale causes
      # xmin > xmax or ymin > ymax in NPC space.  We always use visual min/max
      # for the grob geometry (positive dimensions), then flip the gradient
      # colours so the opaque/transparent sides track the data coordinates
      # (xmin-side opaque for horizontal; ymax-side opaque for vertical).
      x_rev <- coords$xmin[i] > coords$xmax[i]
      y_rev <- coords$ymin[i] > coords$ymax[i]

      x_vis_lo <- min(coords$xmin[i], coords$xmax[i])
      x_vis_hi <- max(coords$xmin[i], coords$xmax[i])
      y_vis_lo <- min(coords$ymin[i], coords$ymax[i])
      y_vis_hi <- max(coords$ymin[i], coords$ymax[i])

      # Gradient direction (data semantics) and the two endpoint alpha
      # values for bbox-relative NPC coordinates (0 = start, 1 = end).
      # These are reused for both the linearGradient fill path and the
      # alpha_ref gradient in the compositing (pattern) path.
      if (identical(rendered_dir, "horizontal")) {
        gx1 <- 0; gy1 <- 0.5; gx2 <- 1; gy2 <- 0.5
        # xmin -> opaque (a_start), xmax -> transparent (alpha_fade_to).
        # x_rev: xmin is at visual right (bbox x=1), xmax at visual left (x=0).
        a_grad_0 <- if (x_rev) alpha_fade_to else a_start
        a_grad_1 <- if (x_rev) a_start        else alpha_fade_to
      } else {
        gx1 <- 0.5; gy1 <- 0; gx2 <- 0.5; gy2 <- 1
        # ymax -> opaque (a_start), ymin -> transparent (alpha_fade_to).
        # y_rev: ymin is at visual top (bbox y=1), ymax at visual bottom (y=0).
        a_grad_0 <- if (y_rev) a_start        else alpha_fade_to
        a_grad_1 <- if (y_rev) alpha_fade_to  else a_start
      }

      mid_alpha <- (a_start + alpha_fade_to) / 2
      flat_fill <- ggplot2::alpha(fill_col, mid_alpha)

      x_pos <- grid::unit(x_vis_lo, "native")
      y_pos <- grid::unit(y_vis_hi, "native")
      w     <- grid::unit(x_vis_hi - x_vis_lo, "native")
      h     <- grid::unit(y_vis_hi - y_vis_lo, "native")

      rr_linejoin <- .roundrect_linejoin(radius, linejoin)
      common_gp_args <- list(
        lwd      = coords$linewidth[i],
        lty      = coords$linetype[i],
        linejoin = rr_linejoin,
        lineend  = lineend
      )

      if (use_pattern) {
        # --- Pattern + fade via compositing --------------------------------
        # dst: the textured fill (preset hatch or user pattern), no outline
        # (the outline is added on top after compositing so it stays at full
        # opacity).
        # alpha_ref: plain rectGrob carrying the same gradient as the
        # solid-fill path, but in black (only alpha channel is used by
        # dest.in).
        if (is_hatch) {
          # hatch() spec: clipped line family at a true visual angle.
          dst_list[[i]] <- .hatch_grob(
            xc = (x_vis_lo + x_vis_hi) / 2,
            yc = (y_vis_lo + y_vis_hi) / 2,
            w = x_vis_hi - x_vis_lo,
            h = y_vis_hi - y_vis_lo,
            colour     = hatch_cols[i],
            angle      = pattern$angle,
            crossed    = identical(pattern$style, "crossed"),
            spacing_mm = hatch_spacing_mm,
            linewidth  = 0.5,
            linetype   = 1L,
            units = "native"
          )
        } else if (grid::is.grob(pattern)) {
          # User grob: clipped to the rectangle (continuous custom hatching).
          dst_list[[i]] <- .clip_pattern_grob(
            pattern,
            xc = (x_vis_lo + x_vis_hi) / 2,
            yc = (y_vis_lo + y_vis_hi) / 2,
            w = x_vis_hi - x_vis_lo,
            h = y_vis_hi - y_vis_lo,
            colour = fill_col,
            units = "native"
          )
        } else {
          # User grid::pattern(): tiled roundrect, stroke recoloured to fill.
          recoloured <- .recolour_pattern(pattern, fill_col)
          dst_list[[i]] <- do.call(
            grid::roundrectGrob,
            c(list(x = x_pos, y = y_pos, width = w, height = h,
                   just = c("left", "top"), r = radius,
                   gp = do.call(ggplot2::gg_par,
                                c(list(fill = recoloured, col = NA),
                                  common_gp_args))),
              list())
          )
        }
        # The alpha mask must be a roundrect with the SAME radius as the
        # outline: dest.in clips the (sharp) hatch to this mask's shape, so a
        # sharp mask would let the pattern spill past rounded corners. Radius
        # is clamped to each rect's displayable max in makeContent, matching
        # the outline exactly.
        alpha_ref_list[[i]] <- grid::roundrectGrob(
          x = x_pos, y = y_pos, width = w, height = h,
          just = c("left", "top"), r = radius,
          gp = grid::gpar(
            fill = grid::linearGradient(
              colours = c(
                ggplot2::alpha("black", a_grad_0),
                ggplot2::alpha("black", a_grad_1)
              ),
              x1 = gx1, y1 = gy1, x2 = gx2, y2 = gy2
            ),
            col = NA
          )
        )
        outline_list[[i]] <- if (is.na(coords$colour[i])) {
          ggplot2::zeroGrob()
        } else {
          do.call(
            grid::roundrectGrob,
            c(list(x = x_pos, y = y_pos, width = w, height = h,
                   just = c("left", "top"), r = radius,
                   gp = do.call(ggplot2::gg_par,
                                c(list(col = coords$colour[i], fill = NA),
                                  common_gp_args))),
              list())
          )
        }
        # Flat fallback (no compositing): standard semi-transparent fill.
        flat_list[[i]] <- do.call(
          grid::roundrectGrob,
          c(list(x = x_pos, y = y_pos, width = w, height = h,
                 just = c("left", "top"), r = radius,
                 gp = do.call(ggplot2::gg_par,
                              c(list(col = coords$colour[i], fill = flat_fill),
                                common_gp_args))),
            list())
        )
      } else {
        # --- Existing linearGradient path -----------------------------------
        grad <- grid::linearGradient(
          colours = c(
            ggplot2::alpha(fill_col, a_grad_0),
            ggplot2::alpha(fill_col, a_grad_1)
          ),
          x1 = gx1, y1 = gy1, x2 = gx2, y2 = gy2
        )
        gradient_list[[i]] <- do.call(
          grid::roundrectGrob,
          c(list(x = x_pos, y = y_pos, width = w, height = h,
                 just = c("left", "top"), r = radius,
                 gp = do.call(ggplot2::gg_par,
                              c(list(col = coords$colour[i], fill = grad),
                                common_gp_args))),
            list())
        )
        flat_list[[i]] <- do.call(
          grid::roundrectGrob,
          c(list(x = x_pos, y = y_pos, width = w, height = h,
                 just = c("left", "top"), r = radius,
                 gp = do.call(ggplot2::gg_par,
                              c(list(col = coords$colour[i], fill = flat_fill),
                                common_gp_args))),
            list())
        )
      }
    }

    if (use_pattern) {
      .rect_fade_pattern_grob(
        dst_list,
        do.call(grid::gList, alpha_ref_list),
        do.call(grid::gList, outline_list),
        do.call(grid::gList, flat_list)
      )
    } else {
      .rect_fade_grob(
        do.call(grid::gList, gradient_list),
        do.call(grid::gList, flat_list)
      )
    }
  }
)

#' Rectangles with a Fading Gradient and Rounded Corners
#'
#' @description
#' `geom_rect_fade()` draws axis-aligned rectangles and fills each one with a
#' linear gradient that fades one edge to transparent. The direction is
#' controlled by `fade_direction`. Corners can be rounded via the `radius`
#' argument, enabling rounded rectangles and smooth-cornered visual elements.
#' The default of `0 pt` produces plain rectangles.
#'
#' @concept rounded corners
#' @concept fading gradient
#'
#' @aesthetics GeomRectFade
#'
#' @inheritSection geom_area_fade Legend key under coord_flip
#'
#' @inheritParams ggplot2::geom_rect
#' @param alpha_fade_to A single finite number between 0 and 1. The alpha
#'   value at the fading edge of each rectangle. Defaults to `0`
#'   (fully transparent).
#' @param fade_direction Direction of the alpha gradient. One of:
#'   \describe{
#'     \item{`"vertical"`}{(default) Top edge is opaque (`ymax`), bottom edge
#'       fades to `alpha_fade_to` (`ymin`).}
#'     \item{`"horizontal"`}{Left edge is opaque (`xmin`), right edge fades to
#'       `alpha_fade_to` (`xmax`).}
#'   }
#' @param radius Corner radius passed to [grid::roundrectGrob()]. A
#'   [grid::unit()] object (e.g. `unit(4, "pt")`); a bare number is
#'   interpreted as points. Defaults to `unit(0, "pt")` (sharp corners).
#' @param pattern An optional textured fill, applied *underneath* the alpha
#'   fade. One of:
#'   \describe{
#'     \item{a preset string}{`"stripe"` (diagonal, the default), `"crossed"`
#'       (diagonal crosshatch), `"vertical"`, `"horizontal"`, or `"grid"`
#'       (vertical + horizontal). Each is shorthand for the matching [hatch()]
#'       call -- for example `"crossed"` is `hatch(style = "crossed")` and
#'       `"grid"` is `hatch(90, style = "crossed")` -- the same way
#'       `position = "dodge"` stands in for [ggplot2::position_dodge()]. Reach
#'       for [hatch()] directly to tune `angle` or `spacing`.}
#'     \item{a [hatch()] specification}{Line hatching at any angle, optionally
#'       crossed -- diagonal stripes, crosshatch, vertical stripes, grids. The
#'       recommended way to add a textured fill.}
#'     \item{a [grid::grob()]}{Drawn once and clipped to each rectangle. For
#'       continuous custom hatching (e.g. wavy lines built in `"npc"` units),
#'       which `grid::pattern()` tiling cannot render without breaking lines
#'       into dashes. See the vignette for a worked "wave" example.}
#'     \item{a [grid::pattern()] object}{A tiled pattern -- best for
#'       self-contained motifs (dots, shapes). Tiling renders continuous
#'       diagonal/wavy lines as dashes, so prefer [hatch()] or a grob for those.}
#'   }
#'   All pattern types -- [hatch()] and user grobs -- are
#'   recoloured to the `fill` aesthetic of each rectangle. The alpha
#'   fade is applied on top via Porter-Duff `"dest.in"` compositing; devices
#'   without compositing support fall back to a flat semi-transparent fill.
#'   Patterns also work under polar coordinates (clipped to each annular
#'   segment); see the *Polar coordinates* section. `NULL` (default) uses the
#'   plain gradient fill.
#' @param stat Use to override the default connection between
#'   `geom_rect_fade()` and `stat_identity()`.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @section Polar coordinates:
#' Under [ggplot2::coord_polar()] / [ggplot2::coord_radial()] each rectangle is
#' bent into an annular segment. A radial alpha gradient -- transparent at the
#' inner radius, opaque at the outer -- is rendered when the fade direction
#' aligns with the radial axis:
#'
#' - `theta = "x"` (default) + `fade_direction = "vertical"`: `ymin`/`ymax`
#'   map to inner/outer radius and fade radially.
#' - `theta = "y"` + `fade_direction = "horizontal"`: `xmin`/`xmax` map to
#'   inner/outer radius and fade radially.
#'
#' Any other combination (for example `theta = "x"` with
#' `fade_direction = "horizontal"`) would require an angular / conic gradient,
#' which `grid` does not yet expose, so the *fade* is dropped (with a one-time
#' message). A `pattern` fill still renders -- it lives in millimetre space,
#' independent of the fade -- clipped to each annular segment at a uniform
#' alpha. Without a pattern, such plots fall back to plain
#' [ggplot2::geom_rect()]. Rounded corners (`radius`) are ignored in polar
#' coordinates since arcs do not carry corner geometry.
#'
#' @section Patterns:
#' [hatch()] builds line-hatch fills -- diagonal stripes (`hatch()`), crosshatch
#' (`hatch(style = "crossed")`), vertical stripes (`hatch(90)`), grids
#' (`hatch(0, style = "crossed")`) -- at a true visual angle, recoloured to the
#' `fill` aesthetic and faded with the alpha gradient.
#'
#' Unlike `grid::pattern()` tiling -- which renders diagonal or wavy lines as
#' disconnected dashes, because each tile is clipped at its own bounds --
#' [hatch()] draws real parallel lines clipped to the rectangle, so they stay
#' continuous.
#'
#' For your own *continuous* hatching beyond straight lines (e.g. wavy lines),
#' build a [grid::grob()] in `"npc"` units and pass it as `pattern`: it is drawn
#' once and clipped to each rectangle, preserving line continuity. For
#' self-contained *motifs* (dots, small shapes) pass a [grid::pattern()] object,
#' which tiles cleanly; its stroke colour is recoloured to the `fill` aesthetic.
#' The article `vignette("ggpointless")` walks through building a wavy-line grob
#' and injecting it. For a far richer set of patterns and controls, use the
#' \pkg{ggpattern} package by Trevor L. Davis
#' (\url{https://trevorldavis.com/R/ggpattern/}).
#'
#' @seealso [hatch()] for the line-hatch helper. [ggplot2::geom_rect()] for
#'   plain rectangles, [geom_col_fade()] for bar charts with per-bar gradient
#'   scaling and orientation support. The \pkg{ggpattern} package
#'   (\url{https://trevorldavis.com/R/ggpattern/}) for comprehensive pattern
#'   fills.
#'
#' @references
#' Murrell, P. (2022). "Vectorised Pattern Fills in R Graphics." Technical
#' Report 2022-01, Department of Statistics, The University of Auckland.
#' Version 1.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/vecpat/vecpat.html}
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' # Highlight a time period with a fading rectangle
#' df_rect <- data.frame(
#'   xmin = as.Date("1968-03-01"),
#'   xmax = as.Date("1969-07-01"),
#'   ymin = -Inf,
#'   ymax = 2800
#' )
#'
#' p <- ggplot(head(economics, 25), aes(date, unemploy)) +
#'   stat_fourier(geom = "line_fade", fade_direction = "start", alpha_fade_to = 0.2) +
#'   geom_point(size = 3, alpha = 0.2) +
#'   theme_minimal()
#'
#' p +
#'   geom_rect_fade(
#'     data = df_rect,
#'     inherit.aes = FALSE,
#'     alpha = 0,
#'     alpha_fade_to = 0.3,
#'     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
#'   )
#'
#' # Fill with a fading pattern using the string shortcut
#' p +
#'   geom_rect_fade(
#'     data = df_rect,
#'     inherit.aes = FALSE,
#'     alpha = 0,
#'     alpha_fade_to = 0.3,
#'     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#'     pattern = "crossed"
#'   )
#'
#' # Customize the pattern further by calling hatch() directly
#' p +
#'   geom_rect_fade(
#'     data = df_rect,
#'     inherit.aes = FALSE,
#'     alpha = 0,
#'     alpha_fade_to = 0.3,
#'     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#'     pattern = hatch(angle = 60, style = "parallel", spacing = unit(1, "mm"))
#'   )
#'
geom_rect_fade <- make_constructor(
  GeomRectFade,
  stat = "identity",
  position = "identity",
  alpha_fade_to = 0,
  fade_direction = "vertical",
  radius = grid::unit(0, "pt"),
  pattern = NULL
)


#' Hatch pattern for `geom_rect_fade()`
#'
#' @description
#' `hatch()` builds a line-hatch specification to pass to the `pattern` argument
#' of [geom_rect_fade()]. The lines are drawn at a true visual angle, clipped to
#' each rectangle, and faded along with the alpha gradient. A single helper
#' covers the common CAD-style fills:
#' \describe{
#'   \item{diagonal stripe}{`hatch()` (the default, 45 degrees)}
#'   \item{diagonal crosshatch}{`hatch(style = "crossed")`}
#'   \item{vertical stripe}{`hatch(90)`}
#'   \item{horizontal stripe}{`hatch(0)`}
#'   \item{square grid}{`hatch(0, style = "crossed")`}
#' }
#'
#' @details
#' Line spacing is a physical distance (millimetres), so it stays constant when
#' the plot is resized rather than scaling with the panel.
#'
#' Hatch line colour always follows the `fill` aesthetic -- a fixed colour
#' independent of the fill would break the mapping. Line weight and dash
#' pattern are fixed at sensible defaults (`linewidth = 0.5`, solid lines)
#' and are not exposed as parameters for the same reason.
#' The hatch transparency is owned by the fade (`alpha` / `alpha_fade_to` on
#' the geom), so `hatch()` has no `alpha` argument.
#'
#' @param angle Line angle in degrees (default `45`).
#' @param style `"parallel"` (default) draws a single family of parallel lines;
#'   `"crossed"` overlays a second family at `angle + 90` (a crosshatch when
#'   diagonal, a grid when axis-aligned).
#' @param spacing Distance between adjacent lines, as a [grid::unit()]
#'   (default `unit(2, "mm")`). A bare number is treated as millimetres.
#'
#' @return A `ggpointless_pattern` object to pass to the `pattern` argument of
#'   [geom_rect_fade()].
#' @seealso [geom_rect_fade()]
#' @concept fading gradient
#' @export
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(xmin = 1, xmax = 5, ymin = 0, ymax = 4)
#' base <- ggplot(df) +
#'   aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
#'
#' # Diagonal stripe (default), faded toward the baseline
#' base + geom_rect_fade(fill = "tomato", alpha = 0.9, pattern = hatch())
#'
#' # Crosshatch at a custom angle
#' base + geom_rect_fade(
#'   fill = "steelblue", pattern = hatch(30, style = "crossed")
#' )
#'
#' # Square grid with wider spacing
#' base + geom_rect_fade(
#'   pattern = hatch(0, style = "crossed", spacing = unit(4, "mm"))
#' )
hatch <- function(angle = 45, style = c("parallel", "crossed"),
                  spacing = grid::unit(2, "mm")) {
  if (!is.numeric(angle) || length(angle) != 1L) {
    cli::cli_abort("{.arg angle} must be a single number (degrees).")
  }
  style <- rlang::arg_match(style)
  if (is.numeric(spacing)) {
    spacing <- grid::unit(spacing, "mm")
  }
  if (!grid::is.unit(spacing)) {
    cli::cli_abort(
      "{.arg spacing} must be a number (millimetres) or a {.cls unit}."
    )
  }
  structure(
    list(
      angle = angle, style = style,
      spacing = spacing
    ),
    class = "ggpointless_pattern"
  )
}

# Preset string vocabulary for `geom_rect_fade(pattern = ...)`. Each name is a
# shorthand for the matching `hatch()` call -- the `position = "dodge"` vs
# `position_dodge()` idiom. `hatch()` remains the full-control constructor for
# tuning `angle` / `spacing`.
#' @noRd
#' @keywords internal
.hatch_presets <- c("stripe", "crossed", "vertical", "horizontal", "grid")

# Coerce a preset string to its `hatch()` specification. Unknown strings abort
# via `arg_match0()` with a "did you mean" hint.
#' @noRd
#' @keywords internal
.hatch_from_string <- function(x, arg = "pattern") {
  preset <- rlang::arg_match0(x, .hatch_presets, arg_nm = arg)
  switch(
    preset,
    stripe     = hatch(),                       # diagonal, 45 deg, parallel
    crossed    = hatch(style = "crossed"),      # diagonal crosshatch
    vertical   = hatch(90),                     # vertical stripe
    horizontal = hatch(0),                      # horizontal stripe
    grid       = hatch(90, style = "crossed")   # vertical + horizontal grid
  )
}
