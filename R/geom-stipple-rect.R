#' @include utils-stipple.R
NULL

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomStippleRect <- ggplot2::ggproto(
  "GeomStippleRect",
  ggplot2::GeomPoint,

  required_aes = c("xmin", "xmax", "ymin", "ymax"),

  draw_panel = function(data, panel_params, coord,
                        dot_spacing = "medium", type = "hex",
                        boundary = "inside", na.rm = FALSE) {
    .check_panel_range(panel_params, "geom_stipple_rect")

    keep_fun <- function(grid, gd, ctx) {
      # Each group is a single rectangle; multiple rows take the bounding box.
      .stipple_points_in_rect(
        grid,
        min(gd$xmin), max(gd$xmax), min(gd$ymin), max(gd$ymax),
        boundary = boundary
      )
    }

    .stipple_grob(
      data, panel_params, coord,
      dot_spacing   = dot_spacing,
      type          = type,
      pos_aes       = c("xmin", "xmax", "ymin", "ymax"),
      keep_fun      = keep_fun,
      dot_radius_pt = .stipple_dot_radius_pt(data)
    )
  }
)


#' Fill a rectangle with a stipple dot grid
#'
#' Renders a dot grid inside each rectangle defined by `xmin`/`xmax`/`ymin`/
#' `ymax` (like [ggplot2::geom_rect()]), sharing the same panel-anchored lattice
#' as the other `geom_stipple_*()` layers. Dot density is constant in physical
#' units -- the grid reflows automatically when the viewer is resized.
#'
#' @param dot_spacing `"fine"`, `"medium"` (default), or `"coarse"` -- physical
#'   spacing between dot centres: 2, 4, or 8 mm. A [grid::unit()] object sets
#'   an explicit size in any unit; a bare numeric is treated as mm.
#' @param type `"hex"` (default) or `"square"` -- grid arrangement.
#' @param boundary `"inside"` (default, strict interior) or `"on"` (include
#'   dots lying exactly on the boundary).
#' @inheritParams ggplot2::geom_point
#'
#' @return A [ggplot2::layer()].
#' @seealso [ggplot2::geom_rect()], [geom_stipple_panel()],
#'   [geom_stipple_path()]
#' @concept stipple
#' @name geom_stipple_rect
#' @export
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(xmin = 1, xmax = 5, ymin = 1, ymax = 4)
#' ggplot(df) +
#'   geom_stipple_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))
geom_stipple_rect <- make_constructor(
  GeomStippleRect,
  stat        = "identity",
  position    = "identity",
  dot_spacing = "medium",
  type        = "hex",
  boundary    = "inside"
)
