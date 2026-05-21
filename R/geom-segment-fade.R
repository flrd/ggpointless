# Segment fade -- delegates to the path_fade pipeline by reshaping each
# segment row into a 2-point path.  This gives us Family A's clipping-path
# fallback (works on svg devices) for free, instead of requiring Porter-Duff
# compositing.
#
# `.segment_fade_grob` / `makeContent.segment_fade_grob` remain defined
# because they are still used by the shared legend key (.draw_key_path_fade),
# which is a single straight stroke where Porter-Duff compositing is the
# simpler option.
#' @include geom-path-fade.R
#' @noRd
#' @keywords internal
.segment_fade_grob <- function(seg_glist, mask_glist, flat_glist) {
  grid::gTree(
    seg_glist = seg_glist,
    mask_glist = mask_glist,
    flat_glist = flat_glist,
    cl = "segment_fade_grob"
  )
}

#' @export
makeContent.segment_fade_grob <- function(x) {
  dev_name <- names(grDevices::dev.cur())
  no_composite <- dev_name %in% c("pdf", "cairo_pdf", "postscript")
  can_composite <- !no_composite && .has_compositing_op("dest.in")

  if (can_composite) {
    n <- length(x$seg_glist)
    composited <- vector("list", n)
    for (i in seq_len(n)) {
      composited[[i]] <- grid::groupGrob(
        x$mask_glist[[i]],
        op = "dest.in",
        dst = x$seg_glist[[i]]
      )
    }
    grobs <- do.call(grid::gList, composited)
  } else {
    grobs <- x$flat_glist
  }

  grid::setChildren(x, grobs)
}


# Reshape a segment data frame (one row per segment with x/y/xend/yend) into
# a path data frame with x/y, each segment its own group. Any existing
# `group` column is replaced -- segment-fade treats every row as its own
# independent path so fade never leaks across segments.
#
# When `fade_direction = c("start", "end")` a midpoint vertex is inserted so
# the fade peak actually lands on a vertex. Without this a 2-point path
# evaluates the fade factor at the endpoints only -- both zero -- and the
# entire segment collapses to `alpha_fade_to`.
#' @noRd
#' @keywords internal
.segment_to_path_data <- function(data, fade_direction = "start") {
  n <- nrow(data)
  starts <- data
  starts$group <- seq_len(n)
  ends <- data
  ends$x <- data$xend
  ends$y <- data$yend
  ends$group <- seq_len(n)

  fade_both <- "start" %in% fade_direction && "end" %in% fade_direction
  if (fade_both) {
    mids <- data
    mids$x <- (data$x + data$xend) / 2
    mids$y <- (data$y + data$yend) / 2
    mids$group <- seq_len(n)
    out <- rbind(starts, mids, ends)
  } else {
    out <- rbind(starts, ends)
  }
  out[order(out$group), , drop = FALSE]
}


#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSegmentFade <- ggplot2::ggproto(
  "GeomSegmentFade",
  ggplot2::GeomSegment,

  # Promote `xend|yend` (the parent `GeomSegment` rule, which means "at
  # least one of xend OR yend") to require BOTH. Our gradient renderer
  # needs both endpoints; a missing one crashed downstream with a
  # cryptic `rbind()` error before this guard.
  required_aes = c("x", "y", "xend", "yend"),

  draw_key = .draw_key_path_fade,

  extra_params = c(
    ggplot2::GeomSegment$extra_params,
    "alpha_fade_to",
    "fade_direction"
  ),

  setup_params = \(self, data, params) {
    .setup_fade_params(self, ggplot2::GeomSegment, data, params)
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
    na.rm = FALSE,
    alpha_fade_to = 0,
    fade_direction = "start"
  ) {
    .check_panel_range(panel_params, "geom_segment_fade")
    data <- ggplot2::remove_missing(
      data,
      na.rm = na.rm,
      c("x", "y", "xend", "yend", "linetype", "linewidth"),
      name = "geom_segment_fade"
    )

    if (nrow(data) == 0L) {
      return(ggplot2::zeroGrob())
    }

    if (.is_uniform_alpha(data, alpha_fade_to)) {
      return(ggplot2::GeomSegment$draw_panel(
        data, panel_params, coord,
        arrow = arrow, arrow.fill = arrow.fill,
        lineend = lineend, linejoin = linejoin,
        na.rm = na.rm
      ))
    }

    # Reshape to path-form (2 rows per segment, one group per row) and
    # delegate to GeomPathFade with alpha_mode = "gradient" to preserve the
    # smooth within-segment gradient rendering of the old Family B pipeline.
    path_data <- .segment_to_path_data(data, fade_direction)
    GeomPathFade$draw_panel(
      path_data,
      panel_params,
      coord,
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = TRUE,
      alpha_fade_to = alpha_fade_to,
      fade_direction = fade_direction,
      alpha_mode = "gradient"
    )
  }
)


#' Line Segments with a Fading Gradient
#'
#' @description
#' `geom_segment_fade()` draws line segments like [ggplot2::geom_segment()]
#' but applies an alpha gradient along each segment so that one or both ends
#' fade to transparent. The gradient direction follows the segment (from
#' `(x, y)` to `(xend, yend)`), so it works at any angle.
#' Rendering requires a graphics device that supports clipping paths
#' (e.g. ragg, cairo, svg).
#'
#' @details
#' ## Non-linear coordinate systems
#'
#' Under non-linear coordinates such as [ggplot2::coord_polar()] and
#' [ggplot2::coord_radial()] the user-supplied endpoints `(x, y)` and
#' `(xend, yend)` are transformed to device space and connected by a
#' straight chord -- exactly as [ggplot2::geom_segment()] does. The fade is
#' then applied along that chord, so both endpoint positions and the fade
#' direction are consistent with the unfaded geom.
#'
#' If you want the line to *follow* a curve implied by the coord system
#' (for example a circle at constant `y` under polar), use
#' [geom_hline_fade()] / [geom_vline_fade()] / [geom_abline_fade()] instead --
#' those geoms subdivide the line in data space so the fade tracks the
#' curve, not a chord.
#'
#' @concept fading segment
#' @concept fading gradient
#'
#' @aesthetics GeomSegmentFade
#'
#' @inheritSection geom_path_fade Self-crossing paths
#'
#' @inheritParams ggplot2::geom_segment
#' @param alpha_fade_to A single finite number between 0 and 1. The alpha
#'   value at the fading end(s). Defaults to `0` (fully transparent).
#' @param fade_direction Which end(s) of each segment fade? A character
#'   vector containing `"start"` (the `(x, y)` end fades), `"end"` (the
#'   `(xend, yend)` end fades), or both `c("start", "end")`. Defaults to
#'   `"start"`.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @seealso [geom_path_fade()] for connected paths with alpha fading,
#'   [ggplot2::geom_segment()] and [ggplot2::geom_curve()]
#'   for the unfaded versions.
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
#' b <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
#' b +
#'   geom_curve_fade(
#'     aes(x = x1, y = y1, xend = x2, yend = y2, colour = "curve"),
#'     data = df
#'   ) +
#'   geom_segment_fade(
#'     aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"),
#'     data = df
#'   )
#'
#' b +
#'   geom_curve_fade(
#'     aes(x = x1, y = y1, xend = x2, yend = y2),
#'     data = df,
#'     curvature = 1,
#'     fade_direction = "start",
#'     arrow = grid::arrow()
#'   )
#'
#' df <- data.frame(x1 = 1, x2 = 9, y1 = 1, y2 = 1)
#' p <- ggplot(df, aes(x)) +
#'   theme_void()
#'
#' # Basic example with default fade_direction
#' p +
#'   geom_segment_fade(
#'     aes(x = x1, y = y1, xend = x2, yend = y2),
#'     fade_direction = "start", # default
#'     linewidth = 10
#'   )
#'
#' # Change fade_direction towards start
#' p +
#'   geom_segment_fade(
#'     aes(x = x1, y = y1, xend = x2, yend = y2),
#'     fade_direction = "end",
#'     linewidth = 10
#'   )
#'
#' # Fade from centre to both sides
#' p +
#'   geom_segment_fade(
#'     aes(x = x1, y = y1, xend = x2, yend = y2),
#'     fade_direction = c("start", "end"),
#'     linewidth = 10
#'   )
#'
geom_segment_fade <- make_constructor(
  GeomSegmentFade,
  stat = "identity",
  position = "identity",
  alpha_fade_to = 0,
  fade_direction = "start"
)
