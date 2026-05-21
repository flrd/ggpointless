# Deferred grob for curve fade -- one compositing group per curve.
#
# Two tiers:
#   Tier 1 -- Porter-Duff "dest.in" compositing per curve
#   Tier 2 -- flat semi-transparent curves (no fade effect)
#' @include geom-path-fade.R
#' @noRd
#' @keywords internal
.curve_fade_grob <- function(curve_glist, mask_glist, flat_glist) {
  grid::gTree(
    curve_glist = curve_glist,
    mask_glist = mask_glist,
    flat_glist = flat_glist,
    cl = "curve_fade_grob"
  )
}

#' @export
makeContent.curve_fade_grob <- function(x) {
  dev_name <- names(grDevices::dev.cur())
  no_composite <- dev_name %in% c("pdf", "cairo_pdf", "postscript")
  can_composite <- !no_composite && .has_compositing_op("dest.in")

  if (can_composite) {
    n <- length(x$curve_glist)
    composited <- vector("list", n)
    for (i in seq_len(n)) {
      composited[[i]] <- grid::groupGrob(
        x$mask_glist[[i]],
        op = "dest.in",
        dst = x$curve_glist[[i]]
      )
    }
    grobs <- do.call(grid::gList, composited)
  } else {
    .queue_curve_no_composite()
    grobs <- x$flat_glist
  }

  grid::setChildren(x, grobs)
}


# Internal copy of ggplot2:::flip_curve to avoid ::: dependency.
# Determines whether start/end points should be swapped so that curvature
# remains visually consistent under coordinate flips and reversed scales.
# Source: https://github.com/tidyverse/ggplot2/blob/v4.0.2/R/geom-curve.R
#' @noRd
#' @keywords internal
.flip_curve <- function(data, coord, params) {
  flip <- FALSE

  if (inherits(coord, "CoordFlip")) {
    flip <- !flip
  } else if (inherits(coord, "CoordTrans")) {
    if (identical(coord$trans$x$name, "reverse")) {
      flip <- !flip
    }
    if (identical(coord$trans$y$name, "reverse")) flip <- !flip
  }

  if ((coord$reverse %||% "none") %in% c("x", "y")) {
    flip <- !flip
  }

  if (identical(.get_scale_transformer(params, "x")$name, "reverse")) flip <- !flip
  if (identical(.get_scale_transformer(params, "y")$name, "reverse")) flip <- !flip

  flip
}


#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomCurveFade <- ggplot2::ggproto(
  "GeomCurveFade",
  ggplot2::GeomCurve,

  # Promote `xend|yend` (the parent `GeomCurve` rule) to require BOTH;
  # the curve renderer needs both endpoints. A missing one previously
  # crashed inside `grid::unit()` with a cryptic length-0 error.
  required_aes = c("x", "y", "xend", "yend"),

  draw_key = .draw_key_path_fade,

  extra_params = c(
    ggplot2::GeomCurve$extra_params,
    "alpha_fade_to",
    "fade_direction",
    "curve_count_cap"
  ),

  setup_params = \(self, data, params) {
    .setup_fade_params(self, ggplot2::GeomCurve, data, params)
  },

  draw_panel = \(
    self,
    data,
    panel_params,
    coord,
    curvature = 0.5,
    angle = 90,
    ncp = 5,
    arrow = NULL,
    arrow.fill = NULL,
    lineend = "butt",
    na.rm = FALSE,
    alpha_fade_to = 0,
    fade_direction = "start",
    curve_count_cap = 200
  ) {
    .check_panel_range(panel_params, "geom_curve_fade")
    # `curve_count_cap` must be a positive scalar (or Inf to disable).  Nonsensical
    # values warn and fall back to the default rather than fail -- mirrors
    # `cell_count_cap` in `geom_unit_*()`.
    if (
      !is.numeric(curve_count_cap) ||
        length(curve_count_cap) != 1L ||
        is.na(curve_count_cap) ||
        (is.finite(curve_count_cap) && curve_count_cap < 1)
    ) {
      cli::cli_warn(
        c(
          "{.arg curve_count_cap} must be a positive scalar number or {.code Inf}.",
          "x" = "Got {.val {curve_count_cap}}.",
          "i" = "Falling back to the default ({.val 200})."
        ),
        call = NULL
      )
      curve_count_cap <- 200
    }

    # Non-linear coords (polar, radial, transformed) -- `coord$transform` would
    # produce ill-defined endpoints for a Bezier curve.  Fall back to the
    # parent `geom_curve()` rendering (no fade) with a one-shot informational
    # message rather than rendering garbage.
    if (!coord$is_linear()) {
      cli::cli_inform(
        c(
          "!" = "{.fn geom_curve_fade} does not support non-linear coordinates.",
          "i" = "Falling back to {.fn ggplot2::geom_curve} rendering (no fade)."
        )
      )
      return(ggplot2::GeomCurve$draw_panel(
        data, panel_params, coord,
        curvature = curvature, angle = angle, ncp = ncp,
        arrow = arrow, arrow.fill = arrow.fill,
        lineend = lineend, na.rm = na.rm
      ))
    }

    data <- ggplot2::remove_missing(
      data,
      na.rm = na.rm,
      c("x", "y", "xend", "yend", "linetype", "linewidth"),
      name = "geom_curve_fade"
    )

    if (nrow(data) == 0L) {
      return(ggplot2::zeroGrob())
    }

    # Defensive cap: one `groupGrob(op = "dest.in")` is built per curve, so
    # render cost scales ~linearly at ~30 ms/curve.  Past 200 curves the
    # compositing becomes the bottleneck and the user almost certainly wants
    # `geom_path_fade()` or `geom_segment_fade()` instead.  Warn loudly and
    # fall back to plain `geom_curve()` (no fade) rather than silently making
    # their session hang.
    if (is.finite(curve_count_cap) && nrow(data) > curve_count_cap) {
      cli::cli_warn(
        c(
          "Refusing to composite {.val {nrow(data)}} curves (cap: {.val {curve_count_cap}}).",
          "i" = "Falling back to plain {.fn ggplot2::geom_curve} rendering (no fade).",
          "i" = "To keep the fade, reduce the row count or pass {.code curve_count_cap = Inf}."
        ),
        call = NULL,
        .frequency = "regularly",
        .frequency_id = "geom_curve_fade_curve_count_cap"
      )
      return(ggplot2::GeomCurve$draw_panel(
        data, panel_params, coord,
        curvature = curvature, angle = angle, ncp = ncp,
        arrow = arrow, arrow.fill = arrow.fill,
        lineend = lineend, na.rm = na.rm
      ))
    }

    if (.is_uniform_alpha(data, alpha_fade_to)) {
      return(ggplot2::GeomCurve$draw_panel(
        data, panel_params, coord,
        curvature = curvature, angle = angle, ncp = ncp,
        arrow = arrow, arrow.fill = arrow.fill,
        lineend = lineend, na.rm = na.rm
      ))
    }

    coords <- coord$transform(data, panel_params)

    # Swap start/end when needed to keep curvature visually consistent
    # under coordinate flips and reversed scales (same logic as GeomCurve).
    flip <- .flip_curve(coords, coord, panel_params)
    if (flip) {
      tmp_x <- coords$x
      tmp_y <- coords$y
      coords$x <- coords$xend
      coords$y <- coords$yend
      coords$xend <- tmp_x
      coords$yend <- tmp_y
      if (!is.null(arrow)) {
        # Flip the arrow end: 2 (last) <-> 1 (first), 3 (both) stays 3
        arrow$ends <- match(arrow$ends, c(2, 1, 3))
      }
    }

    n <- nrow(coords)
    curve_list <- vector("list", n)
    mask_list <- vector("list", n)
    flat_list <- vector("list", n)

    # When the curve is flipped (x/y <-> xend/yend swapped above), the gradient
    # endpoints move with the coordinates but the user's fade_direction still
    # refers to the logical start/end before the swap.  Invert the direction so
    # the mask tracks the right end after the flip.
    eff_fade_direction <- if (flip) {
      d <- character(0)
      if ("start" %in% fade_direction) d <- c(d, "end")
      if ("end"   %in% fade_direction) d <- c(d, "start")
      d
    } else {
      fade_direction
    }

    for (i in seq_len(n)) {
      a_start <- coords$alpha[i]
      if (is.na(a_start)) {
        a_start <- 1
      }

      seg_colour <- coords$colour[i]
      af <- arrow.fill %||% seg_colour

      # Dst grob is fully opaque -- all alpha comes from the mask via dest.in.
      curve_list[[i]] <- grid::curveGrob(
        coords$x[i],
        coords$y[i],
        coords$xend[i],
        coords$yend[i],
        default.units = "native",
        curvature = curvature,
        angle = angle,
        ncp = ncp,
        square = FALSE,
        squareShape = 1,
        inflect = FALSE,
        open = TRUE,
        gp = ggplot2::gg_par(
          col = seg_colour,
          fill = af,
          lwd = coords$linewidth[i],
          lty = coords$linetype[i],
          lineend = lineend
        ),
        arrow = arrow
      )

      # Alpha mask: gradient from alpha_fade_to to a_start along curve direction.
      mask_colours <- .fade_mask_colours(eff_fade_direction, alpha_fade_to, a_start)
      mask_list[[i]] <- grid::rectGrob(
        gp = grid::gpar(
          fill = grid::linearGradient(
            colours = mask_colours$colours,
            stops   = mask_colours$stops,
            x1 = coords$x[i],
            y1 = coords$y[i],
            x2 = coords$xend[i],
            y2 = coords$yend[i]
          ),
          col = NA
        )
      )

      # Flat fallback: midpoint of the two absolute alpha endpoints
      mid_alpha <- (a_start + alpha_fade_to) / 2
      flat_list[[i]] <- grid::curveGrob(
        coords$x[i],
        coords$y[i],
        coords$xend[i],
        coords$yend[i],
        default.units = "native",
        curvature = curvature,
        angle = angle,
        ncp = ncp,
        square = FALSE,
        squareShape = 1,
        inflect = FALSE,
        open = TRUE,
        gp = ggplot2::gg_par(
          col = ggplot2::alpha(seg_colour, mid_alpha),
          fill = ggplot2::alpha(af, mid_alpha),
          lwd = coords$linewidth[i],
          lty = coords$linetype[i],
          lineend = lineend
        ),
        arrow = arrow
      )
    }

    .curve_fade_grob(
      do.call(grid::gList, curve_list),
      do.call(grid::gList, mask_list),
      do.call(grid::gList, flat_list)
    )
  }
)


#' @rdname geom_segment_fade
#' @inheritParams ggplot2::geom_curve
#' @param curve_count_cap Soft cap on the number of curves `geom_curve_fade()` will
#'   composite per panel. One `groupGrob(op = "dest.in")` is built per curve,
#'   so render cost scales roughly linearly at ~30 ms / curve. Past the cap
#'   the layer falls back to plain [ggplot2::geom_curve()] (no fade) and
#'   emits a warning. Default `200` (~6 s worst case). Pass `Inf` to disable.
#'   For bulk fading, use [geom_path_fade()] or [geom_segment_fade()] instead.
#'
#' @concept fading curve
#'
#' @export
geom_curve_fade <- make_constructor(
  GeomCurveFade,
  stat = "identity",
  position = "identity",
  alpha_fade_to = 0,
  fade_direction = "start",
  curve_count_cap = 200
)
