#' @include utils-stipple.R
NULL

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomStipplePath <- ggplot2::ggproto(
  "GeomStipplePath",
  ggplot2::GeomPoint,

  # Keep rows with NA x/y instead of dropping them: NA vertices break the line
  # at draw time (.stipple_dist_to_polyline skips segments touching them),
  # mirroring geom_path / geom_line. GeomPoint's handle_na would strip them.
  handle_na = function(self, data, params) {
    data
  },

  # Explicit formals (no `...`): ggplot2 v4 filters draw params via
  # `parameters()`, which would silently drop `...`-routed params.
  draw_panel = function(data, panel_params, coord,
                        dot_spacing = "medium", radius = NULL, type = "hex",
                        na.rm = FALSE) {
    .check_panel_range(panel_params, "geom_stipple_path")
    .stipple_warn_na(data, na.rm, "geom_stipple_path")

    .stipple_grob(
      data, panel_params, coord,
      dot_spacing   = dot_spacing,
      type          = type,
      pos_aes       = c("x", "y"),
      keep_fun      = .stipple_path_keep_fun(.stipple_resolve_radius(radius), type),
      dot_radius_pt = .stipple_dot_radius_pt(data)
    )
  }
)


#' Stipple a path, line, or step function with dots
#'
#' Instead of drawing a continuous stroke, these geoms render a regular grid of
#' dots and display only those within `radius` of the path. At fine `dot_spacing`
#' the result closely resembles [ggplot2::geom_path()] / [ggplot2::geom_line()]
#' / [ggplot2::geom_step()]; as `dot_spacing` increases the discrete, stippled
#' character becomes visible. Dot density is constant in physical units -- the
#' grid reflows automatically when the viewer is resized.
#'
#' `geom_stipple_path()` respects the order of rows in the data (like
#' `geom_path()`); `geom_stipple_line()` orders observations along the
#' independent axis first (like `geom_line()`); `geom_stipple_step()`
#' approximates the stair-step path (like `geom_step()`). `NA` values break the
#' line, exactly as in the originals. See the *Orientation* section.
#'
#' @section Grid geometry:
#' `dot_spacing` is a physical distance in mm, so dot density stays consistent
#' across plots and across axes with very different scales (e.g. a date axis
#' against `log10`). Two arrangements are available via `type`:
#' \describe{
#'   \item{`"hex"` (default)}{60 degree staggered centres -- hexagonal
#'     close-packing.}
#'   \item{`"square"`}{Aligned rows and columns.}
#' }
#' Every `geom_stipple_*()` layer in a plot resolves the same physical spacing
#' against the same panel, so their lattices coincide exactly.
#'
#' @section Orientation:
#' `geom_stipple_line()` is orientation-aware: by default the independent axis
#' is `x`, but this can be switched by setting `orientation = "y"`. See the
#' *Orientation* section of [ggplot2::geom_line()] for more detail.
#'
#' @param dot_spacing `"fine"`, `"medium"` (default), or `"coarse"` -- physical
#'   spacing between dot centres: 2, 4, or 8 mm. A [grid::unit()] object sets
#'   an explicit size in any unit; a bare numeric is treated as mm.
#' @param radius Maximum distance from the path for a dot to be rendered.
#'   Defaults to the grid's *covering radius* -- `dot_spacing / sqrt(3)` for
#'   `type = "hex"`, `dot_spacing / sqrt(2)` for `type = "square"` -- the
#'   smallest value that leaves no gaps while highlighting as few dots as
#'   possible. A [grid::unit()] object sets an explicit distance in any unit; a
#'   bare numeric is treated as mm. Larger values thicken the trace; smaller
#'   values thin it but may introduce gaps.
#' @param type `"hex"` (default) or `"square"` -- grid arrangement.
#' @inheritParams ggplot2::geom_point
#'
#' @return A [ggplot2::layer()].
#' @seealso [ggplot2::geom_path()], [ggplot2::geom_line()],
#'   [ggplot2::geom_step()], [geom_stipple_panel()], [geom_stipple_rect()]
#' @concept stipple
#' @name geom_stipple_path
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot(economics, aes(date, unemploy)) +
#'   geom_stipple_line(dot_spacing = "coarse")
#'
#' # Hex vs square grid
#' df <- data.frame(
#'   x = seq(0, 2 * pi, length.out = 100),
#'   y = sin(seq(0, 2 * pi, length.out = 100))
#' )
#' ggplot(df, aes(x, y)) +
#'   geom_stipple_path(type = "hex", colour = "steelblue") +
#'   geom_stipple_path(
#'     type = "square", colour = "tomato",
#'     position = position_nudge(y = -0.4)
#'   )
geom_stipple_path <- make_constructor(
  GeomStipplePath,
  stat        = "identity",
  position    = "identity",
  dot_spacing = "medium",
  radius      = NULL,
  type        = "hex"
)
