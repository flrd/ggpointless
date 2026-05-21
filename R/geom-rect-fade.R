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
#' @noRd
#' @keywords internal
.draw_key_rect_fade <- function(data, params, size) {
  radius <- .validate_radius(params$radius)

  fill_colour <- data$fill %||% "grey35"
  a_start <- data$alpha %||% 1
  a_end <- params$alpha_fade_to %||% 0
  fade_direction <- params$fade_direction %||% "vertical"

  if (identical(fade_direction, "horizontal")) {
    # Left (opaque) -> right (transparent)
    grad <- grid::linearGradient(
      colours = c(
        ggplot2::alpha(fill_colour, a_start),
        ggplot2::alpha(fill_colour, a_end)
      ),
      x1 = 0,
      y1 = 0.5,
      x2 = 1,
      y2 = 0.5
    )
  } else {
    # Bottom (transparent) -> top (opaque)
    grad <- grid::linearGradient(
      colours = c(
        ggplot2::alpha(fill_colour, a_end),
        ggplot2::alpha(fill_colour, a_start)
      ),
      x1 = 0.5,
      y1 = 0,
      x2 = 0.5,
      y2 = 1
    )
  }

  grid::roundrectGrob(
    r = radius,
    gp = ggplot2::gg_par(
      fill = grad,
      col = data$colour %||% NA
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
    "radius"
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
    radius = NULL
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
    # requested rounded corners, since `GeomRect$draw_panel` would draw
    # plain rectangles and lose the `radius`.
    if (
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
      cli::cli_warn(
        c(
          "!" = "Removed {n_dropped} rect{?s} with non-finite bounds.",
          "i" = "This usually means {.code -Inf} or {.code Inf} was used \\
                 together with a transformed scale (e.g. {.fn scale_y_log10}) \\
                 that has no representation for those values. Use finite \\
                 {.field xmin}/{.field xmax}/{.field ymin}/{.field ymax} \\
                 values instead."
        )
      )
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

    gradient_list <- vector("list", n)
    flat_list <- vector("list", n)

    for (i in seq_len(n)) {
      a_start <- coords$alpha[i]
      if (is.na(a_start)) {
        a_start <- 1
      }

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

      # Gradient direction:
      #   "vertical"   -- ymax side opaque, ymin side fades (data semantics).
      #   "horizontal" -- xmin side opaque, xmax side fades (data semantics).
      # Under scale reversal the colours are swapped so the opaque side always
      # tracks the intended data edge, not the visual edge.
      if (identical(rendered_dir, "horizontal")) {
        # xmin -> opaque, xmax -> transparent.
        # x_rev: xmin is at visual right (bbox x = 1), xmax at visual left (x = 0).
        col_x0 <- if (x_rev) {
          ggplot2::alpha(fill_col, alpha_fade_to)
        } else {
          ggplot2::alpha(fill_col, a_start)
        }
        col_x1 <- if (x_rev) {
          ggplot2::alpha(fill_col, a_start)
        } else {
          ggplot2::alpha(fill_col, alpha_fade_to)
        }
        grad <- grid::linearGradient(
          colours = c(col_x0, col_x1),
          x1 = 0,
          y1 = 0.5,
          x2 = 1,
          y2 = 0.5
        )
      } else {
        # ymax -> opaque, ymin -> transparent.
        # y_rev: ymin is at visual top (bbox y = 1), ymax at visual bottom (y = 0).
        col_y0 <- if (y_rev) {
          ggplot2::alpha(fill_col, a_start)
        } else {
          ggplot2::alpha(fill_col, alpha_fade_to)
        }
        col_y1 <- if (y_rev) {
          ggplot2::alpha(fill_col, alpha_fade_to)
        } else {
          ggplot2::alpha(fill_col, a_start)
        }
        grad <- grid::linearGradient(
          colours = c(col_y0, col_y1),
          x1 = 0.5,
          y1 = 0,
          x2 = 0.5,
          y2 = 1
        )
      }

      mid_alpha <- (a_start + alpha_fade_to) / 2
      flat_fill <- ggplot2::alpha(fill_col, mid_alpha)

      x_pos <- grid::unit(x_vis_lo, "native")
      y_pos <- grid::unit(y_vis_hi, "native")
      w <- grid::unit(x_vis_hi - x_vis_lo, "native")
      h <- grid::unit(y_vis_hi - y_vis_lo, "native")

      rr_linejoin <- .roundrect_linejoin(radius, linejoin)
      gradient_list[[i]] <- grid::roundrectGrob(
        x = x_pos,
        y = y_pos,
        width = w,
        height = h,
        just = c("left", "top"),
        r = radius,
        gp = ggplot2::gg_par(
          col = coords$colour[i],
          fill = grad,
          lwd = coords$linewidth[i],
          lty = coords$linetype[i],
          linejoin = rr_linejoin,
          lineend = lineend
        )
      )

      flat_list[[i]] <- grid::roundrectGrob(
        x = x_pos,
        y = y_pos,
        width = w,
        height = h,
        just = c("left", "top"),
        r = radius,
        gp = ggplot2::gg_par(
          col = coords$colour[i],
          fill = flat_fill,
          lwd = coords$linewidth[i],
          lty = coords$linetype[i],
          linejoin = rr_linejoin,
          lineend = lineend
        )
      )
    }

    .rect_fade_grob(
      do.call(grid::gList, gradient_list),
      do.call(grid::gList, flat_list)
    )
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
#' @seealso [ggplot2::geom_rect()] for plain rectangles,
#'   [geom_col_fade()] for bar charts with per-bar gradient scaling and
#'   orientation support.
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
  radius = grid::unit(0, "pt")
)
