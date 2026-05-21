#' @rdname geom_area_fade
#'
#' @description
#' `geom_freqpoly_fade()` draws a frequency polygon (like
#' [ggplot2::geom_freqpoly()]) filled with the same linear gradient as
#' `geom_area_fade()`.
#'
#' @concept frequency polygon
#' @concept fading gradient
#'
#' @inheritParams geom_area_fade
#' @inheritParams ggplot2::geom_freqpoly
#' @param bins Number of bins. Overridden by `binwidth`. Defaults to 30.
#'   Forwarded to [ggplot2::stat_bin()].
#' @param binwidth Width of each bin in data units. When supplied, takes
#'   precedence over `bins`. Forwarded to [ggplot2::stat_bin()].
#' @param stat Use to override the default connection between
#'   `geom_freqpoly_fade()` and `stat_bin()`.
#'
#' @export
#' @examples
#' # Basic frequency polygon with fading gradient
#' ggplot(faithful, aes(waiting)) +
#'   geom_freqpoly_fade(
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
  binwidth = NULL,
  bins = NULL,
  alpha_fade_to = 0,
  alpha_scope = "global",
  orientation = NA,
  pad = TRUE
)
