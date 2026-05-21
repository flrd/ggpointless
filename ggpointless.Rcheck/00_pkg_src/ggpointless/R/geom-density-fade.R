#' @title Smoothed Density Estimate with Fading Gradient
#' @description
#' Computes and draws a kernel density estimate — a smoothed version of the
#' histogram — with a vertical alpha gradient that fades from opaque at the
#' peak to transparent at the baseline, exactly like [geom_area_fade()].
#'
#' Under the hood this is [GeomAreaFade] paired with [ggplot2::stat_density()],
#' so all smoothing parameters (`bw`, `adjust`, `kernel`, `bounds`, …) are
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
#' @section Orientation:
#' This geom handles horizontal variants automatically. You can either supply
#' `aes(y = ...)` instead of `aes(x = ...)` and the orientation will be
#' detected, or you can set `orientation = "y"` explicitly.
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
#' ggplot(diamonds, aes(carat)) +
#'   geom_density_fade()
#'
#' # Map the values to y to flip the orientation
#' ggplot(diamonds, aes(y = carat)) +
#'   geom_density_fade()
#'
#' ggplot(diamonds, aes(carat)) +
#'   geom_density_fade(adjust = 1/5)
#' ggplot(diamonds, aes(carat)) +
#'   geom_density_fade(adjust = 5)
#'
#' ggplot(diamonds, aes(depth, colour = cut)) +
#'   geom_density_fade() +
#'   xlim(55, 70)
#' ggplot(diamonds, aes(depth, fill = cut, colour = cut)) +
#'   geom_density_fade(alpha = 0.1) +
#'   xlim(55, 70)
#'
#' # Use `bounds` to adjust computation for known data limits
#' big_diamonds <- diamonds[diamonds$carat >= 1, ]
#' ggplot(big_diamonds, aes(carat)) +
#'   geom_density_fade(color = 'red') +
#'   geom_density_fade(bounds = c(1, Inf), color = 'blue')
#'
#' \donttest{
#' # Stacked density plots: if you want to create a stacked density plot, you
#' # probably want to use the 'count' (density * n) variable instead of the
#' # default density
#'
#' # Loses marginal densities
#' ggplot(diamonds, aes(carat, fill = cut)) +
#'   geom_density_fade(position = "stack")
#' # Preserves marginal densities
#' ggplot(diamonds, aes(carat, after_stat(count), fill = cut)) +
#'   geom_density_fade(position = "stack")
#'
#' # You can use position="fill" to produce a conditional density estimate
#' ggplot(diamonds, aes(carat, after_stat(count), fill = cut)) +
#'   geom_density_fade(position = "fill")
#' }
geom_density_fade <- function(
  mapping = NULL,
  data = NULL,
  stat = "density",
  position = "stack",
  ...,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  bounds = c(-Inf, Inf),
  alpha_fade_to = 0,
  alpha_scope = "global",
  orientation = NULL,
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
