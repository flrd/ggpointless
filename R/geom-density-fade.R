#' @title Smoothed Density Estimate with Fading Gradient
#' @description
#' Computes and draws a kernel density estimate -- a smoothed version of the
#' histogram -- with a vertical alpha gradient that fades from opaque at the
#' peak to transparent at the baseline, exactly like [geom_area_fade()].
#'
#' Under the hood this is [GeomAreaFade] paired with [ggplot2::stat_density()],
#' so all smoothing parameters (`bw`, `adjust`, `kernel`, `bounds`, ...) are
#' forwarded to the stat.
#'
#' @concept density plot
#' @concept fading gradient
#'
#' @section Coordinate systems:
#' `geom_density_fade()` only supports linear gradients. When used with
#' [ggplot2::coord_polar()] or [ggplot2::coord_radial()], the geom falls back
#' to standard area rendering (equivalent to [ggplot2::geom_area()]), which
#' means no gradient fill is added. The geom emits a warning in this case.
#'
#' @aesthetics GeomAreaFade
#'
#' @inheritSection ggplot2::geom_density Orientation
#' @inheritSection geom_area_fade Legend key under coord_flip
#'
#' @inheritParams geom_area_fade
#' @inheritParams ggplot2::geom_density
#' @param bw The smoothing bandwidth to be used. If numeric, the standard
#'   deviation of the smoothing kernel. If character, a rule to choose the
#'   bandwidth, as listed in [stats::bw.nrd()].
#' @param adjust A multiplicate bandwidth adjustment. This makes it focused on
#'   giving the kernel bandwidth more or less smoothing.
#' @param kernel Kernel. See [stats::density()] for more details.
#' @param bounds Known lower and upper bounds for the variable.
#'   Default is `c(-Inf, Inf)`.
#' @param stat Use to override the default connection between
#'   `geom_density_fade()` and `stat_density()`.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @seealso [geom_area_fade()] for the general area-fade geom,
#'   [geom_freqpoly_fade()] / [geom_histogram_fade()] for binned variants,
#'   [ggplot2::geom_density()] for the non-fading original.
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
#' # Basic density curve: opaque at the peak, fully transparent at the baseline.
#' ggplot(diamonds, aes(carat)) +
#'   geom_density_fade()
#'
#' # Map the values to y to flip the orientation
#' ggplot(diamonds, aes(y = carat)) +
#'   geom_density_fade()
#'
#' # `alpha_fade_to` controls the alpha at the baseline.
#' # The default `0` is fully transparent; raise it to keep some
#' # opacity at the floor.
#' ggplot(diamonds, aes(carat)) +
#'   geom_density_fade(alpha_fade_to = 0.2)
#'
#' # Multiple groups via `fill`. With the default `alpha_scope = "global"`
#' # the tallest peak in the layer reaches full opacity; shorter peaks fade
#' # in proportion. `xlim()` trims the long tails for clarity.
#' ggplot(diamonds, aes(depth, fill = cut)) +
#'   geom_density_fade() +
#'   xlim(55, 70)
#'
#' # Switch to `alpha_scope = "group"` so every
#' # area hits full opacity independently
#' ggplot(diamonds, aes(depth, fill = cut)) +
#'   geom_density_fade(alpha_scope = "group") +
#'   xlim(55, 70)
#'
#' # You can use position = "fill" to produce a conditional density estimate
#' ggplot(diamonds, aes(carat, after_stat(count), fill = cut)) +
#'   geom_density_fade(position = "fill")
#'
geom_density_fade <- function(
  mapping = NULL,
  data = NULL,
  stat = "density",
  # Match `ggplot2::geom_density()`'s `position = "identity"` so density
  # curves overlay rather than stack. The parent `GeomArea` defaults to
  # `"stack"`, but density curves are conventionally displayed overlaid.
  # Pass `position = "stack"` (or `"fill"`) explicitly for cumulative
  # / conditional density visualisations.
  position = "identity",
  ...,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  bounds = c(-Inf, Inf),
  alpha_fade_to = 0,
  alpha_scope = "global",
  orientation = NA,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAreaFade,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      bounds = bounds,
      alpha_fade_to = alpha_fade_to,
      alpha_scope = alpha_scope,
      orientation = orientation,
      na.rm = na.rm,
      ...
    )
  )
}
