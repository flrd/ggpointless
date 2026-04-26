#' Frequency Polygons with Fading Gradient
#'
#' @description
#' `geom_freqpoly_fade()` draws a frequency polygon (like
#' [ggplot2::geom_freqpoly()]) filled with a linear gradient that fades from
#' opaque at the top to transparent at the baseline, using
#' [grid::linearGradient()].
#'
#' @concept frequency polygon
#' @concept fading gradient
#'
#' @section Coordinate systems:
#' `geom_freqpoly_fade()` only supports linear gradients. When used with
#' [ggplot2::coord_polar()] or [ggplot2::coord_radial()], the geom falls back
#' to standard area rendering (equivalent to [ggplot2::geom_area()]), which
#' means no gradient fill is added. The geom emits a warning in this case.
#'
#' @aesthetics GeomAreaFade
#'
#' @inheritParams geom_area_fade
#' @inheritParams ggplot2::geom_freqpoly
#' @param stat Use to override the default connection between
#'   `geom_freqpoly_fade()` and `stat_bin()`.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @seealso [geom_histogram_fade()] for the histogram equivalent,
#'   [geom_area_fade()] for continuous data, [ggplot2::geom_freqpoly()] for the
#'   unfaded version.
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
#' # Basic frequency polygon with fading gradient
#' ggplot(faithful, aes(waiting)) +
#'   geom_freqpoly_fade(
#'     fill = "#3b528b",
#'     colour = "#3b528b",
#'     bins = 20
#'   ) +
#'   theme_minimal()
#'
#' # Rather than stacking histograms, compare frequency polygons
#' ggplot(iris, aes(Sepal.Length, fill = Species, colour = Species)) +
#'   geom_freqpoly_fade(
#'     alpha = 0.8,
#'     position = "identity",
#'     bins = 20
#'   ) +
#'   scale_fill_viridis_d() +
#'   scale_colour_viridis_d() +
#'   theme_minimal()
#'
geom_freqpoly_fade <- make_constructor(
  GeomAreaFade,
  stat = "bin",
  position = "identity",
  alpha_fade_to = 0,
  alpha_scope = "global",
  orientation = NULL
)
