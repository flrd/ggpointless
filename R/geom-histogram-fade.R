#' @title Histograms and Frequency Polygons with Fading Gradient
#' @description
#' Visualise the distribution of a single continuous variable with a fading
#' alpha gradient. `geom_histogram_fade()` displays counts with rounded,
#' gradient-filled bars (like [geom_col_fade()] paired with
#' [ggplot2::stat_bin()]). `geom_freqpoly_fade()` displays the same counts as
#' a filled area under the frequency polygon (like [geom_area_fade()] paired
#' with [ggplot2::stat_bin()]).
#'
#' Both functions accept all binning parameters forwarded to
#' [ggplot2::stat_bin()] (`bins`, `binwidth`, `center`, `boundary`, ...).
#'
#' @concept histogram
#' @concept fading gradient
#'
#' @aesthetics GeomColFade
#'
#' @inheritParams geom_col_fade
#' @inheritParams ggplot2::geom_histogram
#' @param stat Use to override the default connection between
#'   `geom_histogram_fade()`/`geom_freqpoly_fade()` and `stat_bin()`.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @seealso [geom_col_fade()] / [geom_bar_fade()] for the bar-chart equivalents,
#'   [geom_area_fade()] for the general area-fade geom,
#'   [ggplot2::geom_histogram()] and [ggplot2::geom_freqpoly()] for the
#'   non-fading originals.
#'
#' @references
#' Murrell, P. (2022). "Vectorised Pattern Fills in R Graphics." Technical
#' Report 2022-01, Department of Statistics, The University of Auckland.
#' Version 1.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/vecpat/vecpat.html}
#'
#' @export
#' @examples
#'
#' # by default each bar has its own alpha scope
#' ggplot(faithful, aes(waiting)) +
#'   geom_histogram_fade(
#'     fill = "#ff005e",
#'     alpha_scope = "bar" # default
#'   ) +
#'   theme_minimal()
#'
#' # Stacked histogram with groups
#' # scale alpha value globally
#' ggplot(iris, aes(Sepal.Length, fill = Species)) +
#'   geom_histogram_fade(
#'     alpha_scope = "global",
#'     radius = unit(5, "pt"),
#'     colour = NA,
#'     bins = 25
#'   ) +
#'   scale_fill_viridis_d() +
#'   theme_minimal()
#'
geom_histogram_fade <- make_constructor(
  GeomColFade,
  stat = "bin",
  position = "stack",
  alpha_fade_to = 0,
  alpha_scope = "bar",
  radius = grid::unit(3, "pt")
)
