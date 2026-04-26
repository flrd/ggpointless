#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionRidgeline <- ggplot2::ggproto(
  "PositionRidgeline",
  ggplot2::Position,

  scale = 1,
  min_height = 0,

  setup_params = \(self, data) {
    scale <- self$scale %||% 1
    min_height <- self$min_height %||% 0

    # Accept any finite numeric scalar (integer OR double); `is_scalar_double`
    # would reject `position_ridgeline(scale = 2L)` and any value that arrives
    # as integer from a column or constructor default.
    .check_scalar <- function(x, arg) {
      ok <- is.numeric(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        is.finite(x)
      if (!ok) {
        cli::cli_abort(
          "{.arg {arg}} must be a single finite number, not {.val {x}}."
        )
      }
    }
    .check_scalar(scale, "scale")
    .check_scalar(min_height, "min_height")

    list(scale = scale, min_height = min_height)
  },

  compute_panel = \(data, params, scales) {
    data$ymin <- data$y
    data$ymax <- data$y + params$scale * data$height

    # Points below min_height become NA so GeomRibbon renders a gap
    below <- data$height < params$min_height
    data$ymax[below] <- NA

    data
  }
)


#' @title Position for Ridgeline Plots
#' @description
#' `position_ridgeline()` computes the ribbon bounds (`ymin`, `ymax`) that
#' [geom_ridgeline_fade()] requires. Each ridge sits at its own `y`-baseline;
#' `ymin` is set to `y` and `ymax` to `y + scale * height`. Points whose
#' `height` falls below `min_height` are turned into `NA` gaps.
#'
#' This position is set automatically by [geom_ridgeline_fade()] and should
#' rarely need to be called directly.
#'
#' @param scale Height multiplier applied to `height`. Values > 1 increase
#'   overlap between adjacent ridges. Defaults to `1`.
#' @param min_height Minimum `height` value to draw. Points with
#'   `height < min_height` are dropped, creating gaps in the ridgeline.
#'   Defaults to `0`.
#'
#' @return A [ggplot2::Position] ggproto object.
#' @export
position_ridgeline <- function(scale = 1, min_height = 0) {
  ggplot2::ggproto(
    NULL,
    PositionRidgeline,
    scale = scale,
    min_height = min_height
  )
}
