# Legend key for path/line/segment/curve fade geoms -- fading segment with
# Porter-Duff compositing, same fallback as the geoms themselves.
#
# The gradient stops use ABSOLUTE combined alpha: `alpha_fade_to` at the
# faded end and `a` (aes alpha) at the opaque end. Under dest.in compositing
# with a fully opaque dst this yields a final alpha that matches the geom's
# own draw_panel semantics -- in particular, `alpha = alpha_fade_to` renders
# as a uniform stroke matching plain `geom_path(alpha = X)`.
#' @noRd
#' @keywords internal
.draw_key_path_fade <- function(data, params, size) {
  fade_direction <- params$fade_direction %||% "start"
  alpha_fade_to <- params$alpha_fade_to %||% 0

  a <- data$alpha %||% 1
  if (is.na(a)) {
    a <- 1
  }
  colour <- data$colour %||% data$fill %||% "black"
  af <- params$arrow.fill %||% colour
  lwd <- data$linewidth %||% 0.5
  arrow <- params[["arrow"]]

  fade_start <- "start" %in% fade_direction
  fade_end <- "end" %in% fade_direction
  if (fade_start && fade_end) {
    grad_colours <- c(
      ggplot2::alpha("black", alpha_fade_to),
      ggplot2::alpha("black", a),
      ggplot2::alpha("black", alpha_fade_to)
    )
    grad_stops <- c(0, 0.5, 1)
  } else if (fade_start) {
    grad_colours <- c(
      ggplot2::alpha("black", alpha_fade_to),
      ggplot2::alpha("black", a)
    )
    grad_stops <- c(0, 1)
  } else {
    grad_colours <- c(
      ggplot2::alpha("black", a),
      ggplot2::alpha("black", alpha_fade_to)
    )
    grad_stops <- c(0, 1)
  }

  # Fully opaque dst -- all alpha comes from the mask gradient via dest.in.
  seg_grob <- grid::segmentsGrob(
    0.1,
    0.5,
    0.9,
    0.5,
    gp = ggplot2::gg_par(
      col = colour,
      fill = af,
      lwd = lwd,
      lty = data$linetype %||% 1,
      lineend = params$lineend %||% "butt"
    ),
    arrow = arrow
  )

  mask_grob <- grid::rectGrob(
    gp = grid::gpar(
      fill = grid::linearGradient(
        colours = grad_colours,
        stops = grad_stops,
        x1 = 0.1,
        y1 = 0.5,
        x2 = 0.9,
        y2 = 0.5
      ),
      col = NA
    )
  )

  mid_alpha <- (a + alpha_fade_to) / 2
  flat_grob <- grid::segmentsGrob(
    0.1,
    0.5,
    0.9,
    0.5,
    gp = ggplot2::gg_par(
      col = ggplot2::alpha(colour, mid_alpha),
      fill = ggplot2::alpha(af, mid_alpha),
      lwd = lwd,
      lty = data$linetype %||% 1,
      lineend = params$lineend %||% "butt"
    ),
    arrow = arrow
  )

  key_grob <- .segment_fade_grob(
    grid::gList(seg_grob),
    grid::gList(mask_grob),
    grid::gList(flat_grob)
  )

  if (!is.null(arrow)) {
    angle <- arrow$angle * pi / 180
    length <- grid::convertUnit(arrow$length[1L], "cm", valueOnly = TRUE)
    attr(key_grob, "width") <- cos(angle) * length * 1.25
    attr(key_grob, "height") <- sin(angle) * length * 2
  }

  key_grob
}


# Compute the fade factor at each vertex along a path.
#
# Returns a numeric vector in [0, 1] where 1 = opaque end (keep aes alpha
# unchanged) and 0 = faded end (replace with `alpha_fade_to`). The final
# combined alpha at a vertex is then:
#   alpha_fade_to + (aes_alpha - alpha_fade_to) * factor
# This direct interpolation means `alpha = alpha_fade_to` (aes alpha equal
# to the fade target) renders as a uniform stroke at that alpha, matching
# `geom_path(alpha = X)` exactly.
#
# @param pos Numeric vector in [0, 1] -- normalised cumulative distance along
#   the path (0 = start vertex, 1 = end vertex).
# @param fade_direction Character vector, subset of `c("start", "end")`.
# @return Numeric vector of fade factors (same length as `pos`).
#' @noRd
#' @keywords internal
.fade_factor_along_path <- function(pos, fade_direction) {
  fade_start <- "start" %in% fade_direction
  fade_end <- "end" %in% fade_direction

  if (fade_start && fade_end) {
    ifelse(pos <= 0.5, pos / 0.5, (1 - pos) / 0.5)
  } else if (fade_start) {
    pos
  } else {
    1 - pos
  }
}


# Build bisector-cut polygon outlines for a path.
#
# At each internal vertex the shared edge lies along the angle bisector of
# the two incident segments, so adjacent polygons meet exactly -- no overlap
# and no gap. Path endpoints are capped according to `lineend`:
#   - "butt"   : perpendicular cut at the vertex (no extension)
#   - "square" : extends `half_lw` past the vertex along the segment
#   - "round"  : same polygon as "square" (a polygonGrob can't draw a
#                rounded cap, so we approximate with square geometry;
#                a polylineGrob drawn inside this clip will still render
#                its native round lineend, trimmed to the square bounds)
#
# Mitre-limit caveat: sharp `linejoin = "miter"` joins extend the stroke
# outward past the bisector. The polygon stops at the bisector, so the
# mitre spike is clipped. Visible only at very sharp angles -- recommend
# `linejoin = "bevel"` or `"round"` in such cases.
#
# @param xn,yn numeric vectors -- vertex coordinates (any consistent unit).
# @param half_lw numeric -- half the stroke width, in the same unit as xn/yn.
# @param lineend one of `"butt"`, `"square"`, `"round"`.
# @return list of length `length(xn) - 1L`; each element is a list with
#   `x`, `y` (length-4 CCW polygon vertices).
#' @noRd
#' @keywords internal
.bisector_polygons <- function(xn, yn, half_lw, lineend = "butt") {
  cap_extend <- if (identical(lineend, "butt")) 0 else 1
  n <- length(xn)
  if (n < 2L) {
    return(list())
  }

  dx <- diff(xn)
  dy <- diff(yn)
  seg_len <- sqrt(dx^2 + dy^2)
  ok <- seg_len > .EPS_ZERO
  dxn <- ifelse(ok, dx / seg_len, 0)
  dyn <- ifelse(ok, dy / seg_len, 0)
  # Left-normal (rotate direction 90 deg CCW)
  nxn <- -dyn
  nyn <- dxn

  # Bisector unit vector at each internal vertex: b = norm(-d_{k-1} + d_k)
  bxn <- numeric(n)
  byn <- numeric(n)
  for (k in seq_len(n - 2L) + 1L) {
    ux <- -dxn[k - 1L] + dxn[k]
    uy <- -dyn[k - 1L] + dyn[k]
    ul <- sqrt(ux^2 + uy^2)
    if (ul < .EPS_ZERO) {
      bxn[k] <- nxn[k - 1L]
      byn[k] <- nyn[k - 1L]
    } else {
      bxn[k] <- ux / ul
      byn[k] <- uy / ul
    }
  }

  # Intersect a segment's offset edges (+/-half-linewidth along n) with the
  # bisector line through V. Returns the left/right corner positions.
  bisect_corners <- function(vx, vy, dx, dy, nx, ny, bx, by, hw) {
    det <- bx * dy - dx * by
    if (abs(det) < .EPS_ZERO) {
      return(list(
        left = c(vx + hw * nx, vy + hw * ny),
        right = c(vx - hw * nx, vy - hw * ny)
      ))
    }
    cross_bn <- bx * ny - nx * by
    t_r <- hw * cross_bn / det
    t_l <- -t_r
    list(
      left = c(vx + t_l * dx + hw * nx, vy + t_l * dy + hw * ny),
      right = c(vx + t_r * dx - hw * nx, vy + t_r * dy - hw * ny)
    )
  }

  # End-cap corners. `cap_extend = 0` -> butt (perpendicular cut at V);
  # `cap_extend = 1` -> square / round approximation (hw past V along d).
  end_corners <- function(vx, vy, dx, dy, nx, ny, hw, forward) {
    s <- if (forward) 1 else -1
    ex <- vx + s * cap_extend * hw * dx
    ey <- vy + s * cap_extend * hw * dy
    list(
      left = c(ex + hw * nx, ey + hw * ny),
      right = c(ex - hw * nx, ey - hw * ny)
    )
  }

  n_seg <- n - 1L
  polys <- vector("list", n_seg)
  for (i in seq_len(n_seg)) {
    if (i == 1L) {
      sc <- end_corners(
        xn[i],
        yn[i],
        dxn[i],
        dyn[i],
        nxn[i],
        nyn[i],
        half_lw,
        forward = FALSE
      )
    } else {
      sc <- bisect_corners(
        xn[i],
        yn[i],
        dxn[i],
        dyn[i],
        nxn[i],
        nyn[i],
        bxn[i],
        byn[i],
        half_lw
      )
    }
    if (i == n_seg) {
      ec <- end_corners(
        xn[i + 1L],
        yn[i + 1L],
        dxn[i],
        dyn[i],
        nxn[i],
        nyn[i],
        half_lw,
        forward = TRUE
      )
    } else {
      ec <- bisect_corners(
        xn[i + 1L],
        yn[i + 1L],
        dxn[i],
        dyn[i],
        nxn[i],
        nyn[i],
        bxn[i + 1L],
        byn[i + 1L],
        half_lw
      )
    }
    # CCW winding: start_left -> start_right -> end_right -> end_left
    polys[[i]] <- list(
      x = c(sc$left[1L], sc$right[1L], ec$right[1L], ec$left[1L]),
      y = c(sc$left[2L], sc$right[2L], ec$right[2L], ec$left[2L])
    )
  }
  polys
}


# Deferred grob for path fade -- per-segment bisector clipping or compositing.
#
# Three tiers:
# Tier 1 -- Porter-Duff dest.in compositing for step mode. One polyline dst +
#          multi-polygon bisector mask pre-wrapped in an inner groupGrob (to
#          avoid per-polygon alpha erosion under dest.in). Single groupGrob
#          call per path group regardless of segment count -- O(1) vs O(n_seg).
# Tier 2 -- viewport clipping. Step mode: per-segment 3-4 pt polylines clipped
#          to bisector polygons. Gradient mode: per-segment linearGradient
#          rectGrobs clipped to bisector polygons, wrapped in an outer mask
#          viewport (the whole-path polyline stroke) for correct linejoin
#          corners. Used when compositing is unavailable.
# Tier 3 -- flat fallback: per-segment segmentsGrob with mid-alpha. No linejoin
#          rendering, but preserves alpha stepping. Used on devices that
#          support neither compositing nor clipping paths.
#
# `groups_data` is a list of per-group records carrying the raw inputs
# needed by any tier. Deferred because converting pt-based linewidth to
# NPC requires the target viewport, which only exists at draw time.
#' @noRd
#' @keywords internal
.path_fade_grob <- function(groups_data) {
  grid::gTree(
    groups_data = groups_data,
    cl = "path_fade_grob"
  )
}

#' @export
makeContent.path_fade_grob <- function(x) {
  dev_caps <- grDevices::dev.capabilities()
  # Base pdf() / postscript() devices have an upstream heap-corruption bug
  # at dev.off() once enough clipping viewports or gradient patterns
  # accumulate (R-core, observed up to R 4.5.3). Force the flat fallback
  # tier on those devices so pages stay simple per-segment polylines.
  dev_name <- names(grDevices::dev.cur())
  unsafe_dev <- dev_name %in% c("pdf", "postscript")
  can_clip <- !unsafe_dev && isTRUE(dev_caps[["clippingPaths"]])
  can_composite <- !unsafe_dev && .has_compositing_op("dest.in", dev_caps)

  all_children <- list()
  any_flat <- FALSE

  for (gi in seq_along(x$groups_data)) {
    grp <- x$groups_data[[gi]]
    n_pts <- length(grp$x)
    if (n_pts < 2L) {
      next
    }

    # Geometry runs in "inches" (isotropic device units) so that the
    # bisector polygon's perpendicular thickness matches grid's native
    # stroke thickness in every direction. Using NPC would give an
    # aspect-distorted (elliptical) polygon on non-square viewports.
    x_in <- grid::convertX(
      grid::unit(grp$x, "native"),
      "inches",
      valueOnly = TRUE
    )
    y_in <- grid::convertY(
      grid::unit(grp$y, "native"),
      "inches",
      valueOnly = TRUE
    )
    half_lw_in <- grid::convertWidth(
      grid::unit(grp$linewidth_pt / 2, "pt"),
      "inches",
      valueOnly = TRUE
    )

    polys_in <- .bisector_polygons(x_in, y_in, half_lw_in, grp$lineend)
    n_seg <- length(polys_in)

    # Batch-convert all polygon corner coordinates (4 per segment) to NPC in
    # two vectorised calls instead of 2 x n_seg individual conversions.
    all_poly_x_in <- unlist(lapply(polys_in, `[[`, "x"), use.names = FALSE)
    all_poly_y_in <- unlist(lapply(polys_in, `[[`, "y"), use.names = FALSE)
    all_poly_x_npc <- grid::convertX(
      grid::unit(all_poly_x_in, "inches"),
      "npc",
      valueOnly = TRUE
    )
    all_poly_y_npc <- grid::convertY(
      grid::unit(all_poly_y_in, "inches"),
      "npc",
      valueOnly = TRUE
    )

    # Step mode represents a fade by assigning each segment its endpoint-mean
    # alpha -- a discrete per-segment approximation of a continuous gradient.
    # On a single-segment path this collapses to one uniform stroke at alpha
    # `(start + end) / 2`, so a two-observation line (`geom_line_fade()` on
    # `data.frame(x = c(a, b), y = c(c, d))`) renders flat. Promote such
    # paths to gradient mode so the fade is actually visible. Multi-segment
    # paths keep step's cheaper rendering path.
    is_gradient <- identical(grp$alpha_mode, "gradient") ||
      (identical(grp$alpha_mode, "step") && n_seg == 1L)

    if (is_gradient && can_clip) {
      # gradient mode: per-segment linearGradient fills clipped to bisector
      # polygons, wrapped in an outer mask for correct linejoin corner shape.
      x_npc <- grid::convertX(
        grid::unit(x_in, "inches"),
        "npc",
        valueOnly = TRUE
      )
      y_npc <- grid::convertY(
        grid::unit(y_in, "inches"),
        "npc",
        valueOnly = TRUE
      )

      seg_children <- vector("list", n_seg)
      for (i in seq_len(n_seg)) {
        idx4 <- ((i - 1L) * 4L) + 1L:4L
        clip_grob <- grid::polygonGrob(
          x = all_poly_x_npc[idx4],
          y = all_poly_y_npc[idx4],
          default.units = "npc",
          gp = grid::gpar(fill = "black", col = NA)
        )
        content <- grid::rectGrob(
          gp = grid::gpar(
            fill = grid::linearGradient(
              colours = c(
                ggplot2::alpha(grp$colour, grp$alpha_vert[i]),
                ggplot2::alpha(grp$colour, grp$alpha_vert[i + 1L])
              ),
              x1 = x_npc[i],
              y1 = y_npc[i],
              x2 = x_npc[i + 1L],
              y2 = y_npc[i + 1L],
              default.units = "npc"
            ),
            col = NA
          )
        )
        seg_children[[i]] <- grid::gTree(
          children = grid::gList(content),
          vp = grid::viewport(clip = clip_grob)
        )
      }
      mask_grob <- grid::polylineGrob(
        x = grp$x,
        y = grp$y,
        default.units = "native",
        gp = ggplot2::gg_par(
          col = "white",
          lwd = grp$linewidth,
          lty = grp$linetype,
          lineend = grp$lineend,
          linejoin = grp$linejoin,
          linemitre = grp$linemitre
        )
      )
      group_grob <- grid::gTree(
        children = do.call(grid::gList, seg_children),
        vp = grid::viewport(mask = mask_grob)
      )
      all_children <- c(all_children, list(group_grob))
    } else if (!is_gradient && can_composite) {
      # step mode fast path: ONE polyline dst composited with a pre-buffered
      # multi-polygon alpha mask via dest.in. The inner groupGrob wrapper
      # prevents per-polygon alpha erosion that occurs when polygonGrob with
      # id.lengths is used directly as a mask under dest.in.
      uniform_alphas <- (grp$alpha_vert[-n_pts] + grp$alpha_vert[-1L]) / 2
      mask_buffered <- grid::groupGrob(
        grid::polygonGrob(
          x = all_poly_x_npc,
          y = all_poly_y_npc,
          id.lengths = rep(4L, n_seg),
          default.units = "npc",
          gp = grid::gpar(
            fill = ggplot2::alpha("black", uniform_alphas),
            col = NA
          )
        )
      )
      dst <- grid::polylineGrob(
        x = grp$x,
        y = grp$y,
        default.units = "native",
        gp = ggplot2::gg_par(
          col = grp$colour,
          lwd = grp$linewidth,
          lty = grp$linetype,
          lineend = grp$lineend,
          linejoin = grp$linejoin,
          linemitre = grp$linemitre
        )
      )
      all_children <- c(
        all_children,
        list(grid::groupGrob(mask_buffered, op = "dest.in", dst = dst))
      )
    } else if (can_clip) {
      # step mode clip fallback: per-segment 3-4 pt polylines clipped to
      # bisector polygons (O(n_seg) viewport push/pops, but no compositing).
      seg_children <- vector("list", n_seg)
      for (i in seq_len(n_seg)) {
        idx4 <- ((i - 1L) * 4L) + 1L:4L
        clip_grob <- grid::polygonGrob(
          x = all_poly_x_npc[idx4],
          y = all_poly_y_npc[idx4],
          default.units = "npc",
          gp = grid::gpar(fill = "black", col = NA)
        )
        uniform_a <- (grp$alpha_vert[i] + grp$alpha_vert[i + 1L]) / 2
        sub_idx <- max(1L, i - 1L):min(n_seg + 1L, i + 2L)
        content <- grid::polylineGrob(
          x = grp$x[sub_idx],
          y = grp$y[sub_idx],
          default.units = "native",
          gp = ggplot2::gg_par(
            col = ggplot2::alpha(grp$colour, uniform_a),
            lwd = grp$linewidth,
            lty = grp$linetype,
            lineend = grp$lineend,
            linejoin = grp$linejoin,
            linemitre = grp$linemitre
          )
        )
        seg_children[[i]] <- grid::gTree(
          children = grid::gList(content),
          vp = grid::viewport(clip = clip_grob)
        )
      }
      all_children <- c(all_children, seg_children)
    } else {
      any_flat <- TRUE
      all_children <- c(all_children, .build_flat_fallback_grobs(grp))
    }

    # Arrow heads are drawn unclipped so they aren't trimmed by the
    # bisector polygons. Each requested end (first / last) is drawn as its
    # own short stub -- a ~0.1% sub-pixel fraction of the incident
    # sub-segment -- with an arrow attribute. Drawing the full sub-segment
    # would erase the visible fade on 2-point paths from segment_fade, and
    # a single `ends = "both"` stub would cluster both arrows at the same
    # endpoint.
    if (!is.null(grp$arrow)) {
      arr <- grp$arrow
      ends_int <- as.integer(arr$ends)
      draw_first <- ends_int %in% c(1L, 3L)
      draw_last <- ends_int %in% c(2L, 3L)
      arr_type <- c("open", "closed")[as.integer(arr$type)]
      frac <- 0.001

      if (draw_last) {
        a_end <- grp$alpha_vert[n_pts]
        x0_stub <- grp$x[n_pts] - frac * (grp$x[n_pts] - grp$x[n_pts - 1L])
        y0_stub <- grp$y[n_pts] - frac * (grp$y[n_pts] - grp$y[n_pts - 1L])
        all_children <- c(
          all_children,
          list(grid::segmentsGrob(
            x0 = x0_stub,
            y0 = y0_stub,
            x1 = grp$x[n_pts],
            y1 = grp$y[n_pts],
            default.units = "native",
            gp = ggplot2::gg_par(
              col = ggplot2::alpha(grp$colour, a_end),
              fill = ggplot2::alpha(
                grp$arrow.fill %||% grp$colour,
                a_end
              ),
              lwd = grp$linewidth,
              lty = grp$linetype,
              lineend = grp$lineend,
              linejoin = grp$linejoin
            ),
            arrow = grid::arrow(
              angle = arr$angle,
              length = arr$length,
              ends = "last",
              type = arr_type
            )
          ))
        )
      }

      if (draw_first) {
        a_start <- grp$alpha_vert[1L]
        x0_stub <- grp$x[1L] - frac * (grp$x[1L] - grp$x[2L])
        y0_stub <- grp$y[1L] - frac * (grp$y[1L] - grp$y[2L])
        all_children <- c(
          all_children,
          list(grid::segmentsGrob(
            x0 = x0_stub,
            y0 = y0_stub,
            x1 = grp$x[1L],
            y1 = grp$y[1L],
            default.units = "native",
            gp = ggplot2::gg_par(
              col = ggplot2::alpha(grp$colour, a_start),
              fill = ggplot2::alpha(
                grp$arrow.fill %||% grp$colour,
                a_start
              ),
              lwd = grp$linewidth,
              lty = grp$linetype,
              lineend = grp$lineend,
              linejoin = grp$linejoin
            ),
            arrow = grid::arrow(
              angle = arr$angle,
              length = arr$length,
              ends = "last",
              type = arr_type
            )
          ))
        )
      }
    }
  }

  if (any_flat) {
    .queue_path_no_composite_clipping()
  }

  grid::setChildren(x, do.call(grid::gList, all_children))
}


# Per-segment flat fallback used when the device does not support
# clipping paths.  Segments get their combined (mean endpoint) alpha.
# Arrow drawing is intentionally omitted here because the outer arrow
# overdraw in `makeContent.path_fade_grob` always runs after this and
# stamps the arrow with the endpoint alpha (consistent with gradient
# mode).  Drawing it here too would render the arrow twice.
#' @noRd
#' @keywords internal
.build_flat_fallback_grobs <- function(grp) {
  n_pts <- length(grp$x)
  if (n_pts < 2L) {
    return(list())
  }
  n_seg <- n_pts - 1L

  list(
    grid::segmentsGrob(
      x0 = grp$x[-n_pts],
      y0 = grp$y[-n_pts],
      x1 = grp$x[-1L],
      y1 = grp$y[-1L],
      default.units = "native",
      gp = ggplot2::gg_par(
        col = ggplot2::alpha(grp$seg_colours, grp$combined_alpha),
        lwd = rep(grp$linewidth, n_seg),
        lty = rep(grp$linetype, n_seg),
        lineend = grp$lineend,
        linejoin = grp$linejoin
      )
    )
  )
}


#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPathFade <- ggplot2::ggproto(
  "GeomPathFade",
  ggplot2::GeomPath,

  draw_key = .draw_key_path_fade,

  extra_params = c(
    ggplot2::GeomPath$extra_params,
    "alpha_fade_to",
    "fade_direction",
    "alpha_mode"
  ),

  setup_params = \(self, data, params) {
    params <- .setup_fade_params(self, ggplot2::GeomPath, data, params)
    params$alpha_mode <- params$alpha_mode %||% "auto"
    params$alpha_mode <- rlang::arg_match0(
      params$alpha_mode,
      values = c("auto", "step", "gradient"),
      arg_nm = "alpha_mode"
    )
    params
  },

  draw_panel = \(
    self,
    data,
    panel_params,
    coord,
    arrow = NULL,
    arrow.fill = NULL,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    na.rm = FALSE,
    alpha_fade_to = 0,
    fade_direction = "start",
    alpha_mode = "auto"
  ) {
    .check_panel_range(panel_params, "geom_path_fade")
    # NAs are treated as path splitters (matching geom_path/geom_line), NOT
    # rows to drop. We emit the same "Removed N rows" warning as ggplot2 for
    # UX parity, but keep the rows so the loop below can detect gaps.
    missing_mask <- is.na(data$x) |
      is.na(data$y) |
      is.na(data$linetype) |
      is.na(data$linewidth)
    n_missing <- sum(missing_mask)
    if (!isTRUE(na.rm) && n_missing > 0L) {
      cli::cli_warn(
        "Removed {n_missing} row{?s} containing missing values or values \\
         outside the scale range ({.fn geom_path_fade})."
      )
    }

    if (nrow(data) - n_missing < 2L) {
      return(ggplot2::zeroGrob())
    }

    coords <- coord$transform(data, panel_params)

    # coord$transform may introduce additional NAs (e.g. squish_infinite on
    # out-of-range values), so recompute the mask on the transformed data.
    missing_mask <- is.na(coords$x) |
      is.na(coords$y) |
      is.na(coords$linetype) |
      is.na(coords$linewidth)

    if (.is_uniform_alpha(data, alpha_fade_to)) {
      return(ggplot2::GeomPath$draw_panel(
        data,
        panel_params,
        coord,
        arrow = arrow,
        arrow.fill = arrow.fill,
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre,
        na.rm = na.rm
      ))
    }

    groups <- split(seq_len(nrow(coords)), coords$group)
    groups_data <- list()

    for (grp_idx in groups) {
      grp_missing <- missing_mask[grp_idx]
      valid_local <- which(!grp_missing)
      if (length(valid_local) < 2L) {
        next
      }

      # Split valid_local into runs of consecutive positions. Each run is a
      # sub-path of non-NA vertices, separated from its neighbours by gaps.
      gaps <- which(diff(valid_local) > 1L)
      run_starts <- c(1L, gaps + 1L)
      run_ends <- c(gaps, length(valid_local))

      runs <- list()
      for (r in seq_along(run_starts)) {
        rs <- run_starts[r]
        re <- run_ends[r]
        if (re - rs + 1L < 2L) {
          next
        }
        abs_idx <- grp_idx[valid_local[rs:re]]
        rd <- coords[abs_idx, , drop = FALSE]
        dx <- diff(rd$x)
        dy <- diff(rd$y)
        runs[[length(runs) + 1L]] <- list(
          data = rd,
          seg_lengths = sqrt(dx^2 + dy^2)
        )
      }

      n_runs <- length(runs)
      if (n_runs == 0L) {
        next
      }

      # Concatenated arc length across all sub-paths of this group: the
      # denominator is the sum of visible segment lengths. All runs share
      # this total, so the fade progresses continuously across the gap --
      # the alpha just before a gap and just after are (almost) identical,
      # as if the gap had zero width.
      total_dist <- sum(vapply(runs, \(r) sum(r$seg_lengths), 0))
      if (!is.finite(total_dist) || total_dist == 0) {
        total_dist <- 1
      }

      cum_offset <- 0
      for (r_idx in seq_along(runs)) {
        r <- runs[[r_idx]]
        rd <- r$data
        n_pts <- nrow(rd)

        pos <- (cum_offset + c(0, cumsum(r$seg_lengths))) / total_dist
        cum_offset <- cum_offset + sum(r$seg_lengths)

        # Combined (final) alpha at each vertex is interpolated DIRECTLY
        # between the aes alpha (opaque end) and alpha_fade_to (faded end).
        # `alpha = alpha_fade_to` yields a uniform stroke at that alpha,
        # matching plain `geom_path(alpha = X)`.
        fade_factor <- .fade_factor_along_path(pos, fade_direction)
        a_vert <- rd$alpha
        a_vert[is.na(a_vert)] <- 1
        alpha_vert <- alpha_fade_to + (a_vert - alpha_fade_to) * fade_factor
        combined_alpha <- (alpha_vert[-n_pts] + alpha_vert[-1L]) / 2

        # Resolve `alpha_mode = "auto"` per sub-path: small n -> gradient
        # (smooth within each thick segment), large n -> step (cheaper;
        # per-segment stepping is invisible at high density).
        # Threshold is n = 50 vertices: gradient render cost stays under
        # ~0.4 s per panel at that n, and step becomes visually
        # indistinguishable from gradient well before it.  See
        # benchmark notes in memory/ggAudit.md (item 3.16).
        resolved_mode <- if (identical(alpha_mode, "auto")) {
          if (n_pts <= 50L) "gradient" else "step"
        } else {
          alpha_mode
        }

        groups_data[[length(groups_data) + 1L]] <- list(
          x = rd$x,
          y = rd$y,
          linewidth = rd$linewidth[1L],
          linewidth_pt = rd$linewidth[1L] * ggplot2::.pt,
          linetype = rd$linetype[1L],
          colour = rd$colour[1L],
          alpha_vert = alpha_vert,
          combined_alpha = combined_alpha,
          seg_colours = rd$colour[-n_pts],
          # Arrow belongs only to the very last sub-path's tip.
          arrow = if (r_idx == n_runs) arrow else NULL,
          arrow.fill = arrow.fill,
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre,
          alpha_mode = resolved_mode
        )
      }
    }

    if (length(groups_data) == 0L) {
      return(ggplot2::zeroGrob())
    }

    .path_fade_grob(groups_data)
  }
)


#' Paths and Lines with a Fading Gradient
#'
#' @description
#' `geom_path_fade()` connects observations in the order they appear in the
#' data (like [ggplot2::geom_path()]) and applies a linear alpha gradient so
#' that one or both ends of the path fade to transparent.
#'
#' @concept fading path
#'
#' @section Rendering modes:
#'
#' The `alpha_mode` argument controls how the alpha gradient is rendered. All
#' three values compute a target alpha at each vertex from cumulative distance
#' along the path -- the fade follows the path regardless of direction or shape.
#'
#' **`"auto"`** (default): pick per sub-path based on vertex count `n`.
#' `n <= 50` -> `"gradient"` (smooth within-segment fade; protects the common
#' "few thick segments" case). `n > 50` -> `"step"` (stepping is invisible at
#' that density and gradient's per-segment cost grows linearly with `n`).
#' The threshold is chosen so that worst-case `"gradient"` render time stays
#' under ~0.4 s per panel. Resolved per sub-path -- a multi-group plot can mix
#' modes. Users who want deterministic rendering (snapshots, reproducible
#' builds) should set `"step"` or `"gradient"` explicitly.
#'
#' **`"step"`**: each segment is drawn inside a viewport clipped to its
#' bisector-cut polygon, carrying a single uniform alpha -- the average of
#' its two endpoint alphas. The fade is an illusion created by stepping
#' through discrete alpha values across adjacent segments. Fast (~0.4 s for
#' a 200-point path).
#'
#' **`"gradient"`**: each segment's clipped viewport contains a panel-sized
#' `rectGrob` with its own direction-aligned `linearGradient` fill. The alpha
#' transitions smoothly within each segment -- a real continuous gradient, not
#' a step approximation. Slower (~1 s for 200 points, ~4 s for 1000 points);
#' scales linearly with `n` and multiplies under facets.
#'
#' All modes require a device that supports clipping paths (e.g.
#' [ragg::agg_png()], [grDevices::svg()]). On devices that don't, the geom
#' falls back to per-segment `segmentsGrob` rendering with combined alpha
#' (no linejoin).
#'
#' The base [grDevices::pdf()] and [grDevices::postscript()] devices
#' *advertise* clipping-path support but have an upstream R heap-corruption
#' bug at `dev.off()` once enough clipping or gradient operations
#' accumulate (reproducible with pure `grid` on R 4.5.3). To keep these
#' devices safe the geom routes through the flat per-segment fallback on
#' `pdf()` / `postscript()` regardless of `alpha_mode`. Use
#' [grDevices::cairo_pdf()] (or [ragg::agg_png()] / [grDevices::svg()] for
#' raster/vector output) if you need the smooth-gradient rendering.
#'
#' The visual difference between `"step"` and `"gradient"` is only noticeable
#' with very few, thick segments: in `"step"` mode each segment is visibly
#' a solid colour, while `"gradient"` interpolates smoothly within each
#' segment too. At typical point counts (>= ~50) both modes look identical --
#' which is what `"auto"` exploits.
#'
#' As a special case, a single-segment path (exactly two observations) is
#' always rendered as a gradient even when `alpha_mode = "step"`, because
#' step mode's per-segment alpha would otherwise collapse the fade to a
#' uniform mid-alpha stroke.
#'
#' @section RStudio "Unknown group" warning:
#'
#' On the RStudio plot pane (and any device that caches and replays the
#' rendered grob tree), step-mode rendering may emit grid warnings of the
#' form `Warning in .useGroup(ref, NULL) : Unknown group, N` on repeat
#' draws -- typically when the pane is resized and replays a cached tree.
#' The plot itself is correct; the warning is cosmetic noise.
#'
#' What is happening: step mode uses grid's compositing primitive
#' (`groupGrob()` with operator `"dest.in"`) to trim the alpha mask onto
#' the polyline. Each draw allocates fresh device-level group IDs. When
#' the plot pane replays a cached grob tree, that tree still references
#' the old IDs while grid's device table has been reset -- the lookup
#' fails and grid prints `Unknown group, N`. The pixels you see are
#' correct because the composited result was already resolved when the
#' cache was built; grid is just reporting that the replay couldn't
#' repeat the lookup, not that anything is missing on screen.
#'
#' The warning fires inside grid's drawing machinery after our render
#' code has already returned, so we cannot wrap it in `suppressWarnings()`
#' from within the package. The only clean alternative would gate the
#' fast compositing path off in RStudio, which makes the interactive
#' plot pane roughly five times slower (~1.5 s -> ~7.5 s on a five-group
#' \eqn{\times} 573-segment dataset) -- we judged that the worse trade-off.
#' If the warnings are intolerable for a particular layer, set
#' `alpha_mode = "gradient"`; gradient mode uses viewport clipping
#' instead of compositing and does not trigger the warning.
#'
#' @section Device-independent alternative:
#'
#' A device-independent alternative to this geom is to densify the path before
#' plotting by interpolating `x`, `y` with `stats::approx()` , and a matching
#' alpha column along the path; then pass the result to a plain
#' [ggplot2::geom_path()]. Because ggplot2 draws each segment between adjacent
#' points with a uniform aesthetic value (see the ggplot2 book,
#' [section 4.4](https://ggplot2-book.org/collective-geoms.html#sec-matching)),
#' enough interpolated points make the stepping invisible and produce a smooth
#' apparent fade without any compositing:
#'
#' ```r
#' df <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0))
#'
#' # Parameterise by cumulative arc length, then densify
#' arc <- with(df, c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2))))
#' arc <- arc / max(arc)
#' grid_pos <- seq(0, 1, length.out = 200)
#'
#' dense <- data.frame(
#'   x     = approx(arc, df$x, xout = grid_pos)$y,
#'   y     = approx(arc, df$y, xout = grid_pos)$y,
#'   alpha = 1 - grid_pos   # fade towards end
#' )
#'
#' ggplot(dense, aes(x, y, alpha = alpha)) +
#'   geom_path(linewidth = 2) +
#'   scale_alpha_identity()
#' ```
#'
#' The trade-off: this requires manual labour, and it does not generalise to paths
#' with multiple groups or to `fade_direction = c("start", "end")` without
#' additional bookkeeping. `geom_path_fade()` handles all of this internally.
#'
#' @section Self-crossing paths:
#'
#' When a faded path crosses itself, the pixels at the crossing are
#' rasterised once per overlapping segment, and the per-segment alpha values
#' compound. Where the two strands carry different alphas (one near the
#' faded end, one near the opaque end), the crossing appears noticeably
#' darker than either strand alone.
#'
#' This behaviour is inherent to how semi-transparent strokes are
#' alpha-blended at the device level, not specific to `geom_path_fade()` --
#' the same effect appears with `ggplot2::geom_path(alpha = 0.5)`. There is
#' no general workaround at the rendering layer; if a clean intersection
#' matters for your plot, the practical options are:
#'
#' * Raise `alpha_fade_to` so the strands at both ends are closer in
#'   opacity (smaller delta -> less visible darkening).
#' * Use a fully opaque stroke (no fade) for paths known to self-cross.
#' * Restructure the data so the crossing is split across separate layers.
#'
#' Applies equally to [geom_segment_fade()] and [geom_curve_fade()] when
#' two segments / curves overlap at the same pixel.
#'
#' @aesthetics GeomPathFade
#'
#' @inheritParams ggplot2::geom_path
#' @param alpha_fade_to A single finite number between 0 and 1. The alpha
#'   value at the fading end(s). Defaults to `0` (fully transparent).
#' @param fade_direction Which end(s) of the path fade out. A character
#'   vector containing `"start"`, `"end"`, or both `c("start", "end")`.
#'   Defaults to `"start"`. Invalid values are silently dropped with a
#'   warning; if nothing valid remains, falls back to `"start"`.
#' @param alpha_mode How the alpha gradient is rendered. One of `"step"`
#'   (default) or `"gradient"`. See section **Rendering modes** for details.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @seealso [geom_line_fade()] which sorts by x first,
#'   [geom_segment_fade()] for individual fading segments,
#'   [ggplot2::geom_path()] for the unfaded version.
#'
#' @references
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
#'
#' # Path that doubles back -- fade follows the drawing order
#' theta <- seq(1.3, -1.3, length.out = 101)
#' df_ichthys <- data.frame(
#'   x = theta^2,
#'   y = 0.5 * theta * (theta^2 - 1)
#' )
#'
#' p <- ggplot(df_ichthys, aes(x, y)) +
#'   geom_pointless(
#'     location = c("first", "last"),
#'     aes(colour = after_stat(location)),
#'     size = 4
#'   ) +
#'   coord_fixed() +
#'   theme_minimal()
#'
#' p + geom_path_fade(
#'     linewidth = 1.5,
#'     fade_direction = "start" # default
#'   )
#'
#' p + geom_path_fade(
#'     linewidth = 1.5,
#'     fade_direction = c("start", "end")
#'   )
#'
#' # With few thick segments the default `"auto"` picks `"gradient"` for
#' # you, because at n <= 50 the smoother within-segment fade matters more
#' # than the (negligible) extra compute time.
#' df_thick <- data.frame(
#'   x = c(0, 1, 1.5, 1, 0),
#'   y = c(0, 0.5, 1, 1.5, 1)
#' )
#'
#' p <- ggplot(df_thick, aes(x, y)) +
#'   coord_equal() +
#'   theme_minimal()
#'
#' # Auto -> gradient (n = 5, well below the 50-vertex threshold)
#' p + geom_path_fade(
#'   linewidth = 8,
#'   colour = "#e63946"
#'   )
#'
#' # Force `"step"` to see the per-segment stepping for comparison.
#' p + geom_path_fade(
#'   linewidth = 8,
#'   colour = "#e63946",
#'   alpha_mode = "step"
#'   )
#'
#' # Explicit `"gradient"`, in this example, does the same thing
#' # `"auto"` picked above; for large n (> 200) this gets slow with
#' # not much gain visually.
#' p + geom_path_fade(
#'   linewidth = 8,
#'   colour = "#e63946",
#'   alpha_mode = "gradient"
#'   )
#'
#' # Using stat_function
#' ggplot() +
#'   stat_function(
#'     alpha = 0.5,
#'     fun = dnorm,
#'     n = 100,
#'     xlim = c(-4, 4),
#'     geom = "area_fade",
#'     outline.type = "none" # remove solid outline
#'   ) +
#'   # Add fading outline instead
#'   stat_function(
#'     fun = dnorm, n = 100,
#'     xlim = c(-4, 4),
#'     geom = "path_fade",
#'     fade_direction = c("start", "end")
#'   )
#'
geom_path_fade <- make_constructor(
  GeomPathFade,
  stat = "identity",
  position = "identity",
  alpha_fade_to = 0,
  fade_direction = "start",
  alpha_mode = "auto"
)
