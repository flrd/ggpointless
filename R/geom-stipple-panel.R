#' @include utils-stipple.R
NULL

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomStipplePanel <- ggplot2::ggproto(
  "GeomStipplePanel",
  ggplot2::Geom,

  # No required aesthetics -- the extent is derived from the panel, not data.
  required_aes = character(0L),

  # colour / fill follow the active theme's ink / paper; users can override.
  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(ink),
    fill   = ggplot2::from_theme(paper),
    size   = 1.5,
    alpha  = NA,
    shape  = 19L,
    stroke = 0.5
  ),

  draw_key = ggplot2::draw_key_point,

  draw_panel = function(data, panel_params, coord,
                        dot_spacing = "medium", type = "hex") {
    .check_panel_range(panel_params, "geom_stipple_panel")

    .stipple_grob(
      data, panel_params, coord,
      dot_spacing   = dot_spacing,
      type          = type,
      pos_aes       = c("x", "y"),
      keep_fun      = NULL,
      dot_radius_pt = .stipple_dot_radius_pt(data)
    )
  }
)


#' Fill the panel with a stipple dot grid
#'
#' Renders a regular dot grid across the full panel, creating the illusion of a
#' stippled background. No aesthetic mappings are required: the extent is taken
#' from the panel limits (including any scale expansion). Dot density is
#' constant in physical units -- the grid reflows automatically when the viewer
#' is resized.
#'
#' @section Colour defaults and theming:
#' `colour` and `fill` default to `from_theme(ink)` and `from_theme(paper)`, so
#' the grid inherits the active theme's foreground and background colours.
#' Override per-layer with explicit values, or globally via
#' `theme(geom = element_geom(ink = "steelblue"))`.
#'
#' @param dot_spacing `"fine"`, `"medium"` (default), or `"coarse"` -- physical
#'   spacing between dot centres: 2, 4, or 8 mm. A [grid::unit()] object sets
#'   an explicit size in any unit; a bare numeric is treated as mm.
#' @param type `"hex"` (default) or `"square"` -- grid arrangement.
#' @inheritParams ggplot2::geom_point
#'
#' @return A [ggplot2::layer()].
#' @seealso [geom_stipple_path()], [geom_stipple_rect()]
#' @concept stipple
#' @name geom_stipple_panel
#' @export
#' @examples
#' library(ggplot2)
#'
#' # Stippled background behind a line; colour inherits from the theme ink
#' ggplot(economics, aes(date, unemploy)) +
#'   geom_stipple_panel(dot_spacing = "coarse", alpha = 0.3) +
#'   geom_line(colour = "steelblue")
geom_stipple_panel <- make_constructor(
  GeomStipplePanel,
  stat     = "identity",
  position = "identity",
  inherit.aes = FALSE
)
