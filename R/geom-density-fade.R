#' @rdname geom_area_fade
#' @description
#' `geom_density_fade()` computes and draws a kernel density estimate --
#' a smoothed version of the histogram -- with the same vertical alpha
#' gradient as `geom_area_fade()`.  Under the hood this is [GeomAreaFade]
#' paired with [ggplot2::stat_density()], so all smoothing parameters
#' (`bw`, `adjust`, `kernel`, `bounds`, ...) are forwarded to the stat.
#'
#' @concept density plot
#' @concept fading gradient
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
#' @export
#' @examples
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
  outline.type = "upper",
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
      outline.type = outline.type,
      na.rm = na.rm,
      ...
    )
  )
}
