#' @rdname geom_ridgeline_fade
#' @include geom-ridgeline-fade.R
#'
#' @description
#' `geom_ridgeline_density_fade()` is a convenience wrapper around
#' `geom_ridgeline_fade()` that uses [ggplot2::stat_density()] to compute a
#' kernel density estimate, mapping the result to `height` automatically via
#' `aes(height = after_stat(density))`. Adjust `scale` manually so adjacent
#' ridges reach the desired overlap.
#'
#' @concept density ridges
#' @concept fading gradient
#'
#' @inheritParams geom_ridgeline_fade
#' @param ... Additional arguments passed to [geom_ridgeline_fade()], including
#'   smoothing parameters (`bw`, `adjust`, `kernel`, `n`, `trim`, `bounds`)
#'   forwarded to [ggplot2::stat_density()].
#'
#' @seealso [geom_ridgeline_fade()] for the lower-level geom. This wrapper
#'   mirrors the design of
#'   \href{https://wilkelab.org/ggridges/reference/geom_density_ridges.html}{\code{ggridges::geom_density_ridges()}}
#'   from the \href{https://wilkelab.org/ggridges/}{\strong{ggridges}} package
#'   by Claus O. Wilke, which is the full-featured original; `ggpointless`
#'   adds the alpha-gradient rendering layer.
#'
#' @export
#' @examples
#' # Average monthly temperatures at Nottingham, 1920-1939
#' dmn <- list(
#'   month.abb,
#'   time(datasets::nottem) |> floor() |> unique()
#' )
#'
#' df_nottem <- datasets::nottem |>
#'   matrix(data = _, 12, dimnames = dmn) |>
#'   as.data.frame() |>
#'   stack() |>
#'   cbind(month = factor(month.abb, levels = month.abb))
#'
#' # Shared base plot reused by the three ridgeline variants below
#' p <- ggplot(df_nottem, aes(x = values, y = month)) +
#'   labs(
#'     x = NULL,
#'     y = NULL,
#'     caption = "Average air temperatures at Nottingham Castle in degrees Fahrenheit (1920-1939)"
#'   ) +
#'   scale_fill_gradient(low = "navy", high = "tomato", guide = "none") +
#'   scale_y_discrete(expand = expansion(add = c(0.2, 1.2)))
#'
#' # Density ridgelines -- convenience wrapper for the stat = "density"
#' p +
#'   geom_ridgeline_density_fade(
#'     aes(fill = after_stat(x)),
#'     outline.type = "none"
#'   )
#'
geom_ridgeline_density_fade <- function(
  mapping = NULL,
  data = NULL,
  stat = "density",
  ...,
  alpha_fade_to = 0,
  alpha_scope = "group",
  scale = NULL,
  min_height = NULL,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {
  # Inject height = after_stat(density) as a default; the user's own height
  # mapping takes precedence if they supply one explicitly.
  mapping <- utils::modifyList(
    ggplot2::aes(height = ggplot2::after_stat(density)),
    mapping %||% ggplot2::aes()
  )
  geom_ridgeline_fade(
    mapping = mapping,
    data = data,
    stat = stat,
    ...,
    alpha_fade_to = alpha_fade_to,
    alpha_scope = alpha_scope,
    scale = scale,
    min_height = min_height,
    na.rm = na.rm,
    orientation = orientation,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}
