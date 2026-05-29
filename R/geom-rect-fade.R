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


# Built-in pattern presets.  The "stripe" name is borrowed, with thanks,
# from the {ggpattern} package by Trevor L. Davis; the implementation here
# is our own.  We deliberately do NOT use grid::pattern() tiling for the
# preset: tiling renders diagonal lines as disconnected dashes (each tile
# is clipped at its bounds, so corner-to-corner segments meet only at a
# point).  Instead we draw real parallel lines clipped to the rectangle,
# which gives genuinely continuous diagonals at a true visual angle.
# User-supplied grid::pattern() objects still go through the tiling path.
.fade_pattern_presets <- c("stripe")

# Build a clipped diagonal-hatch grob for the "stripe" preset.
#
# Parallel lines covering the rectangle (`xc`/`yc`/`w`/`h`, in `units`),
# clipped to it via a viewport.  Endpoints are in "snpc" so the angle is
# visually true regardless of the rectangle's aspect ratio, and the line
# spacing scales with the rectangle's smaller dimension.
#
# `snpc` is the *smaller* of the viewport's width/height, so for a wide
# (or tall) rectangle the lines must be laid out far beyond [0, 1] to
# reach the far edge before clipping.  `REACH` is the half-extent of the
# perpendicular offsets and the line half-length, both in snpc: REACH = 10
# covers aspect ratios up to roughly 20:1 (offset projection grows like
# ~0.43 * aspect), comfortably beyond any realistic rectangle.
#' @noRd
#' @keywords internal
.stripe_hatch_grob <- function(xc, yc, w, h, colour,
                               angle = 60, spacing = 0.06,
                               lwd = 1, lineend = "butt",
                               units = "native") {
  vp <- grid::viewport(
    x = xc, y = yc, width = w, height = h,
    just = "centre", default.units = units, clip = "on"
  )
  ang <- angle * pi / 180
  dx <- cos(ang)
  dy <- sin(ang)
  nx <- -dy
  ny <- dx
  REACH <- 10
  rng <- seq(-REACH, REACH, by = spacing)
  seg <- grid::segmentsGrob(
    x0 = grid::unit(0.5 + rng * nx - REACH * dx, "snpc"),
    y0 = grid::unit(0.5 + rng * ny - REACH * dy, "snpc"),
    x1 = grid::unit(0.5 + rng * nx + REACH * dx, "snpc"),
    y1 = grid::unit(0.5 + rng * ny + REACH * dy, "snpc"),
    gp = ggplot2::gg_par(col = colour, lwd = lwd, lineend = lineend)
  )
  grid::gTree(children = grid::gList(seg), vp = vp)
}

# Wrap a user-supplied grob so it is clipped to a single rectangle
# (`xc`/`yc`/`w`/`h`, in `units`).  Unlike grid::pattern() tiling -- which
# breaks continuous lines into per-tile dashes -- clipping draws the grob
# once and shows only the part inside the rectangle, so continuous custom
# hatching (e.g. wavy lines built in "snpc") stays continuous.  The grob's
# stroke colour is recoloured to the mapped `fill`.
#' @noRd
#' @keywords internal
.clip_pattern_grob <- function(grob, xc, yc, w, h, colour, units = "native") {
  grid::gTree(
    children = grid::gList(.recolour_grob_col(grob, colour)),
    vp = grid::viewport(
      x = xc, y = yc, width = w, height = h,
      just = "centre", default.units = units, clip = "on"
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

  # Clamp radius on both dst (pattern fill) and outline roundrects at
  # render time when physical dimensions are known.
  dst_cl     <- .clamp_roundrect_radius(x$dst_glist,     arg = "radius")
  outline_cl <- .clamp_roundrect_radius(x$outline_glist, arg = "radius")

  n      <- length(dst_cl)
  result <- vector("list", n)
  for (i in seq_len(n)) {
    comp <- grid::groupGrob(x$alpha_ref_glist[[i]], op = "dest.in", dst = dst_cl[[i]])
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
# Polar renders require both clipping paths (to shape the gradient to the
# annular segment) AND a radialGradient pattern. Devices missing either
# capability fall back to flat semi-transparent annular segments.
#' @noRd
#' @keywords internal
.rect_fade_polar_grob <- function(gradient_glist, flat_glist) {
  grid::gTree(
    gradient_glist = gradient_glist,
    flat_glist = flat_glist,
    cl = "rect_fade_polar_grob"
  )
}

#' @export
makeContent.rect_fade_polar_grob <- function(x) {
  dev_name <- names(grDevices::dev.cur())
  can_gradient <- !dev_name %in% c("pdf", "postscript") &&
    tryCatch(
      {
        caps <- grDevices::dev.capabilities()
        isTRUE(caps[["clippingPaths"]]) &&
          "RadialGradient" %in% caps[["patterns"]]
      },
      error = \(e) FALSE
    )

  if (can_gradient) {
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
  linejoin
) {
  theta <- coord$theta %||% "x"
  n <- nrow(data)

  gradient_list <- vector("list", n)
  flat_list <- vector("list", n)

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
    if (identical(theta, "x")) {
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

    gradient_list[[i]] <- grid::gTree(
      children = grid::gList(gradient_rect),
      vp = clip_vp,
      name = paste0("rect_fade_polar_ring_", i)
    )
    flat_list[[i]] <- flat_grob
  }

  .rect_fade_polar_grob(
    do.call(grid::gList, gradient_list),
    do.call(grid::gList, flat_list)
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
    dst <- if (is.character(pattern)) {
      # "stripe" preset: clipped diagonal hatch filling the key cell.
      .stripe_hatch_grob(
        xc = 0.5, yc = 0.5, w = 1, h = 1,
        colour = fill_colour, units = "npc"
      )
    } else if (grid::is.grob(pattern)) {
      .clip_pattern_grob(
        pattern, xc = 0.5, yc = 0.5, w = 1, h = 1,
        colour = fill_colour, units = "npc"
      )
    } else {
      recoloured <- .recolour_pattern(pattern, fill_colour)
      grid::roundrectGrob(
        r = radius,
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
        r = radius,
        gp = ggplot2::gg_par(col = data$colour, fill = NA)
      )
    }
    flat <- grid::roundrectGrob(
      r = radius,
      gp = ggplot2::gg_par(
        fill = ggplot2::alpha(fill_colour, (a_start + a_end) / 2),
        col  = data$colour %||% NA
      )
    )
    return(.rect_fade_pattern_grob(
      grid::gList(dst),
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
    if (!is.null(pat)) {
      if (is.character(pat)) {
        # Validate the preset name (kept as a string; the clipped-hatch
        # grob is built per-rect in draw_panel). arg_match0 gives a helpful
        # "did you mean" error.
        params$pattern <- rlang::arg_match0(
          pat,
          values = .fade_pattern_presets,
          arg_nm = "pattern"
        )
      } else if (
        !inherits(pat, "GridTilingPattern") && !grid::is.grob(pat)
      ) {
        cli::cli_abort(
          c(
            "{.arg pattern} must be {.val stripe}, a {.cls GridTilingPattern} \\
             from {.fn grid::pattern}, or a {.cls grob} to clip to each \\
             rectangle.",
            "x" = "Got {.obj_type_friendly pat}."
          ),
          class = "ggpointless_rect_fade_pattern_type"
        )
      }
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

      if (.is_uniform_alpha(data, alpha_fade_to)) {
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
          linejoin = linejoin
        ))
      }

      # Angular fade (theta-aligned gradient): grid has no conic gradient
      # primitive, so we fall back to a flat geom_rect render and emit an
      # informational message.
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

    if (!coord$is_linear()) {
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
    gradient_list <- vector("list", n)
    flat_list     <- vector("list", n)
    if (use_pattern) {
      dst_list      <- vector("list", n)
      alpha_ref_list <- vector("list", n)
      outline_list  <- vector("list", n)
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
        if (is.character(pattern)) {
          # "stripe" preset: clipped diagonal hatch in the fill colour.
          dst_list[[i]] <- .stripe_hatch_grob(
            xc = (x_vis_lo + x_vis_hi) / 2,
            yc = (y_vis_lo + y_vis_hi) / 2,
            w = x_vis_hi - x_vis_lo,
            h = y_vis_hi - y_vis_lo,
            colour = fill_col,
            lwd = coords$linewidth[i],
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
        alpha_ref_list[[i]] <- grid::rectGrob(
          x = x_pos, y = y_pos, width = w, height = h,
          just = c("left", "top"),
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
        do.call(grid::gList, dst_list),
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
#'     \item{`"stripe"`}{The built-in preset: continuous diagonal hatching
#'       (see *Patterns* below).}
#'     \item{a [grid::grob()]}{Drawn once and clipped to each rectangle. Best
#'       for continuous custom hatching (e.g. wavy lines built in `"npc"`
#'       units), which `grid::pattern()` tiling cannot render without breaking
#'       lines into dashes. See the vignette for a worked "wave" example.}
#'     \item{a [grid::pattern()] object}{A tiled pattern -- best for
#'       self-contained motifs (dots, shapes). Tiling renders continuous
#'       diagonal/wavy lines as dashes, so prefer a grob for those.}
#'   }
#'   In every case the texture's stroke colour (`gp$col`) is automatically
#'   recoloured to match the `fill` aesthetic. The alpha fade is applied on top
#'   via Porter-Duff `"dest.in"` compositing; devices without compositing
#'   support fall back to a flat semi-transparent fill. Patterns are not
#'   supported under polar coordinates. `NULL` (default) uses the plain
#'   gradient fill.
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
#' which `grid` does not yet expose. Such plots fall back to plain
#' [ggplot2::geom_rect()] rendering and emit a one-time warning.
#' Rounded corners (`radius`) are ignored in polar coordinates since arcs do
#' not carry corner geometry.
#'
#' @section Patterns:
#' One built-in preset is available: `pattern = "stripe"` draws continuous
#' diagonal hatching, recoloured to the `fill` aesthetic and faded with the
#' alpha gradient. The `"stripe"` name is borrowed, with thanks, from the
#' \pkg{ggpattern} package by Trevor L. Davis
#' (\url{https://trevorldavis.com/R/ggpattern/}); the implementation here is an
#' independent \pkg{grid} one.
#'
#' Unlike `grid::pattern()` tiling -- which renders diagonal or wavy lines as
#' disconnected dashes, because each tile is clipped at its own bounds -- the
#' `"stripe"` preset draws real parallel lines clipped to the rectangle, so the
#' diagonals stay continuous at a true visual angle.
#'
#' For your own *continuous* hatching (e.g. wavy lines), build a [grid::grob()]
#' in `"npc"` units and pass it as `pattern`: it is drawn once and clipped to
#' each rectangle, preserving line continuity. For self-contained *motifs*
#' (dots, small shapes) pass a [grid::pattern()] object, which tiles cleanly.
#' In both cases the stroke colour is recoloured to the `fill` aesthetic. The
#' article `vignette("ggpointless")` walks through building a wavy-line grob and
#' injecting it. For a far richer set of patterns and controls, use
#' \pkg{ggpattern} directly.
#'
#' @seealso [ggplot2::geom_rect()] for plain rectangles,
#'   [geom_col_fade()] for bar charts with per-bar gradient scaling and
#'   orientation support. The \pkg{ggpattern} package
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
#' # With geom_rect_fade() you can draw arbitrary rectangles
#' ggplot(head(economics, 25), aes(date, unemploy)) +
#'   geom_rect_fade(
#'     data = data.frame(
#'       xmin = as.Date("1968-07-01"),
#'       xmax = as.Date("1969-07-01"),
#'       ymin = -Inf, ymax = 2800
#'     ),
#'     inherit.aes = FALSE,
#'     alpha = 0,
#'     alpha_fade_to = 0.3,
#'     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
#'   ) +
#'   stat_fourier(geom = "line_fade", fade_direction = "start", alpha_fade_to = 0.2) +
#'   geom_point(size = 3, alpha = 0.2) +
#'   theme_minimal()
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
