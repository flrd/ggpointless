# Fading reference lines: horizontal, vertical, and diagonal.
#
# Each ggproto inherits the original Geom*line draw_panel() logic that
# converts intercept/slope parameters into (x, y, xend, yend) data, then
# delegates to GeomSegmentFade$draw_panel() instead of GeomSegment$draw_panel()
# to get the fading gradient.

#' @include geom-segment-fade.R
#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAblineFade <- ggplot2::ggproto(
  "GeomAblineFade",
  ggplot2::GeomAbline,

  draw_key = .draw_key_path_fade,

  extra_params = c(
    ggplot2::GeomAbline$extra_params,
    "alpha_fade_to",
    "fade_direction"
  ),

  setup_params = \(self, data, params) {
    .setup_fade_params(self, ggplot2::GeomAbline, data, params)
  },

  draw_panel = \(
    self,
    data,
    panel_params,
    coord,
    lineend = "butt",
    na.rm = FALSE,
    alpha_fade_to = 0,
    fade_direction = "start"
  ) {
    ranges <- coord$backtransform_range(panel_params)

    # Do NOT extend the ranges (unlike GeomAbline which doubles them to ensure
    # the line reaches the panel edges under viewport clipping).  For the fade
    # geom the segment endpoints must sit at the actual panel boundary so that
    # the linearGradient anchors are within [0, 1] NPC.  Doubling pushes the
    # anchors far outside the panel, so the visible segment only covers a small
    # slice of the gradient and the fade effect is barely perceptible.
    #
    # Coordinate-space alignment: backtransform_range and coord$transform both
    # work in the INTERNAL coordinate space (e.g. negated x under
    # scale_x_reverse).  data$slope and data$intercept are in the ORIGINAL
    # data space.  Apply the scale's x-transformer to convert the line's x
    # intersection values to the internal space before clamping; invert when
    # computing y so the line equation stays in the original space.
    tr    <- .get_scale_transformer(panel_params)
    x_fwd <- tr$fwd
    x_inv <- tr$inv

    # Guard against slope = 0: dividing by zero gives ±Inf which produces NaN
    # when later multiplied back by slope.  For horizontal lines clamp x to the
    # full panel width directly.
    horizontal <- abs(data$slope) < 1e-10
    x_at_y_orig <- sweep(
      outer(ranges$y, data$intercept, FUN = "-"),
      2,
      ifelse(horizontal, 1, data$slope),
      FUN = "/"
    )
    x_at_y <- apply(x_at_y_orig, c(1L, 2L), x_fwd)

    data$x    <- ifelse(horizontal, ranges$x[1], pmax(ranges$x[1], pmin(x_at_y[1, ], x_at_y[2, ])))
    data$xend <- ifelse(horizontal, ranges$x[2], pmin(ranges$x[2], pmax(x_at_y[1, ], x_at_y[2, ])))
    data$y    <- x_inv(data$x)    * data$slope + data$intercept
    data$yend <- x_inv(data$xend) * data$slope + data$intercept

    GeomSegmentFade$draw_panel(
      unique(data),
      panel_params,
      coord,
      lineend = lineend,
      na.rm = na.rm,
      alpha_fade_to = alpha_fade_to,
      fade_direction = fade_direction
    )
  }
)


#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomHlineFade <- ggplot2::ggproto(
  "GeomHlineFade",
  ggplot2::GeomHline,

  draw_key = .draw_key_path_fade,

  extra_params = c(
    ggplot2::GeomHline$extra_params,
    "alpha_fade_to",
    "fade_direction"
  ),

  setup_params = \(self, data, params) {
    .setup_fade_params(self, ggplot2::GeomHline, data, params)
  },

  draw_panel = \(
    self,
    data,
    panel_params,
    coord,
    lineend = "butt",
    na.rm = FALSE,
    alpha_fade_to = 0,
    fade_direction = "start"
  ) {
    ranges <- coord$backtransform_range(panel_params)

    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    data$y    <- data$yintercept
    data$yend <- data$yintercept

    GeomSegmentFade$draw_panel(
      unique(data),
      panel_params,
      coord,
      lineend = lineend,
      na.rm = na.rm,
      alpha_fade_to = alpha_fade_to,
      fade_direction = fade_direction
    )
  }
)


#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomVlineFade <- ggplot2::ggproto(
  "GeomVlineFade",
  ggplot2::GeomVline,

  draw_key = .draw_key_path_fade,

  extra_params = c(
    ggplot2::GeomVline$extra_params,
    "alpha_fade_to",
    "fade_direction"
  ),

  setup_params = \(self, data, params) {
    .setup_fade_params(self, ggplot2::GeomVline, data, params)
  },

  draw_panel = \(
    self,
    data,
    panel_params,
    coord,
    lineend = "butt",
    na.rm = FALSE,
    alpha_fade_to = 0,
    fade_direction = "start"
  ) {
    ranges <- coord$backtransform_range(panel_params)

    data$x    <- data$xintercept
    data$xend <- data$xintercept
    data$y    <- ranges$y[1]
    data$yend <- ranges$y[2]

    GeomSegmentFade$draw_panel(
      unique(data),
      panel_params,
      coord,
      lineend = lineend,
      na.rm = na.rm,
      alpha_fade_to = alpha_fade_to,
      fade_direction = fade_direction
    )
  }
)


#' Reference Lines with a Fading Gradient
#'
#' @description
#' These geoms draw reference lines — horizontal, vertical, or diagonal — with
#' an alpha gradient along the line so that one or both ends fade to
#' transparent.
#'
#' Like their ggplot2 counterparts, these geoms can be used as annotations by
#' passing `slope`/`intercept`, `yintercept`, or `xintercept` as arguments
#' directly. In that case the line is constant across facets.
#' To vary lines across facets, supply your own `data` and `mapping`.
#'
#' @details
#' ## Gradient direction and reversed scales
#'
#' The fade direction is defined in **visual** (panel) space, not in data
#' space. `fade_direction = "start"` always makes the *start* of the line
#' transparent — for horizontal lines this is the visual left edge; for
#' vertical lines, the visual bottom edge — regardless of whether the x or y
#' scale is reversed. This is analogous to other presentational properties
#' such as `hjust` in [ggplot2::geom_text()]: `hjust = 1` means
#' "right-justify" irrespective of axis direction, and
#' `fade_direction = "start"` means "transparent at the left/bottom panel
#' edge" irrespective of axis direction.
#'
#' If you *do* want the gradient to track the data direction — making the
#' low-value end transparent even when it appears on the visual right — pass
#' `fade_direction = "end"` to override the default:
#'
#' ```r
#' # On a reversed x-axis, "end" fades toward larger x values (visual left)
#' p + scale_x_reverse() +
#'   geom_hline_fade(yintercept = 20, fade_direction = "end")
#' ```
#'
#' @concept fading line
#' @concept reference line
#' @concept gradient line
#' @concept annotation
#'
#' @aesthetics GeomSegmentFade
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' @param data A data frame, or other object, to override the plot data.
#' @param alpha_fade_to A single finite number between 0 and 1. The alpha
#'   value at the fading end(s). Defaults to `0` (fully transparent).
#' @param fade_direction Which end(s) of the line fade. A character vector
#'   containing `"start"`, `"end"`, or both `c("start", "end")`. Defaults
#'   to `"start"`. For horizontal lines, `"start"` fades the visual left end;
#'   for vertical lines, `"start"` fades the visual bottom end; for diagonal
#'   lines, `"start"` fades toward the visual left edge of the panel. See
#'   *Gradient direction and reversed scales* for behaviour under
#'   [ggplot2::scale_x_reverse()] / [ggplot2::scale_y_reverse()].
#' @param xintercept,yintercept,slope,intercept Parameters that control the
#'   position of the line. If these are set, `data`, `mapping` and
#'   `show.legend` are overridden.
#' @inheritParams ggplot2::geom_abline
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @seealso [geom_segment_fade()] for fading line segments with explicit
#'   endpoints, [geom_path_fade()] and [geom_line_fade()] for connected paths;
#'   [ggplot2::geom_abline()], [ggplot2::geom_hline()], and [ggplot2::geom_vline()]
#'   for their ggplot2 originals.
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
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Horizontal reference line, fading from the left
#' p + geom_hline_fade(yintercept = 20, linewidth = 1.5)
#'
#' # Vertical line fading at both ends
#' p + geom_vline_fade(xintercept = 3, linewidth = 1.5,
#'                     fade_direction = c("start", "end"))
#'
#' # Diagonal line of best fit, fading from the left
#' coefs <- coef(lm(mpg ~ wt, data = mtcars))
#' p + geom_abline_fade(intercept = coefs[1], slope = coefs[2], linewidth = 1.5)
#'
geom_abline_fade <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  ...,
  slope,
  intercept,
  alpha_fade_to = 0,
  fade_direction = "start",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
) {
  if (is.null(mapping) && missing(slope) && missing(intercept)) {
    slope <- 1
    intercept <- 0
  }

  if (!missing(slope) || !missing(intercept)) {
    if (!is.null(mapping)) {
      cli::cli_warn(
        "{.fn geom_abline_fade}: Ignoring {.arg mapping} because \\
         {.arg slope} and/or {.arg intercept} were provided."
      )
    }
    if (!is.null(data)) {
      cli::cli_warn(
        "{.fn geom_abline_fade}: Ignoring {.arg data} because \\
         {.arg slope} and/or {.arg intercept} were provided."
      )
    }

    if (missing(slope)) {
      slope <- 1
    }
    if (missing(intercept)) {
      intercept <- 0
    }
    n <- max(length(slope), length(intercept))

    data <- data.frame(
      intercept = rep_len(intercept, n),
      slope = rep_len(slope, n)
    )
    mapping <- ggplot2::aes(intercept = intercept, slope = slope)
    show.legend <- FALSE
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAblineFade,
    position = ggplot2::PositionIdentity,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      alpha_fade_to = alpha_fade_to,
      fade_direction = fade_direction,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname geom_abline_fade
#' @export
geom_hline_fade <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  yintercept,
  alpha_fade_to = 0,
  fade_direction = "start",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
) {
  if (!missing(yintercept)) {
    if (!is.null(mapping)) {
      cli::cli_warn(
        "{.fn geom_hline_fade}: Ignoring {.arg mapping} because \\
         {.arg yintercept} was provided."
      )
    }
    if (!is.null(data)) {
      cli::cli_warn(
        "{.fn geom_hline_fade}: Ignoring {.arg data} because \\
         {.arg yintercept} was provided."
      )
    }

    data <- data.frame(yintercept = yintercept)
    mapping <- ggplot2::aes(yintercept = yintercept)
    show.legend <- FALSE
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHlineFade,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      alpha_fade_to = alpha_fade_to,
      fade_direction = fade_direction,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname geom_abline_fade
#' @export
geom_vline_fade <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  xintercept,
  alpha_fade_to = 0,
  fade_direction = "start",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
) {
  if (!missing(xintercept)) {
    if (!is.null(mapping)) {
      cli::cli_warn(
        "{.fn geom_vline_fade}: Ignoring {.arg mapping} because \\
         {.arg xintercept} was provided."
      )
    }
    if (!is.null(data)) {
      cli::cli_warn(
        "{.fn geom_vline_fade}: Ignoring {.arg data} because \\
         {.arg xintercept} was provided."
      )
    }

    data <- data.frame(xintercept = xintercept)
    mapping <- ggplot2::aes(xintercept = xintercept)
    show.legend <- FALSE
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomVlineFade,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      alpha_fade_to = alpha_fade_to,
      fade_direction = fade_direction,
      na.rm = na.rm,
      ...
    )
  )
}
