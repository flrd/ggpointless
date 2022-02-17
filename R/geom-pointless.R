#' Emphasize some observations with points
#'
#' @description This is a wrapper around `geom_point()` with the one additional argument: `location`.
#' It allows to emphazise some observations, namely the first the last, and the minima and maxima, see examples.
#' This geom is not particularly useful on its own, hence the name, but hopefully in conjunction
#' with `geom_path()` and friends.
#'
#' @import ggplot2
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::layer
#'
#' @param location A character vector specifying which observations to highlight, defaults to `"last"`.
#' @param orientation The orientation of the layer. The default (`NA`) automatically determines
#' the orientation from the aesthetic mapping. In the rare event that this fails it can be given
#' explicitly by setting `orientation` to either `"x"` or `"y"`. See the Orientation section for more detail.
#' @param geom,stat Overwrite the default connection between `geom_pointless()` and `stat_pointless()`.
#'
#' @section Details:
#' The argument `location` allows you to control which observations to highlight - with a point.
#' When `location` is `"last"`, the default, a single point will be plotted at the last observations.
#' Setting `location` to `"first"` adds a point at the first position.
#'
#' @section Orientation:
#' This geom treats each axis differently and, can thus have two orientations.
#' Often the orientation is easy to deduce from a combination of the given mappings
#' and the types of positional scales in use. Thus, ggplot2 will by default try
#' to guess which orientation the layer should have. Under rare circumstances,
#' the orientation is ambiguous and guessing may fail. In that case the orientation
#' can be specified directly using the orientation parameter, which can be either
#' "x" or "y". The value gives the axis that the geom should run along, "x"
#' being the default orientation you would expect for the geom.
#'
#' @section Aesthetics:
#' geom_pointless() understands the following aesthetics (required aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - alpha
#' - color
#' - fill
#' - group
#' - shape
#' - size
#' - stroke
#'
#'
#' @export
#' @examples
#' x <- seq(-pi, pi, length.out = 100)
#' y <- outer(x, 1:5, FUN = function(x, y) sin(x*y))
#'
#' df1 <- data.frame(
#'   var1 = x,
#'   var2 = rowSums(y)
#' )
#'
#'# not terribly useful on its own ...
#'p <- ggplot(df1, aes(x = var1, y = var2))
#'p + geom_pointless()
#'p + geom_pointless(location = "all")
#'
#'# ... but in conjunction with geom_line(), hopefully
#'p <- p + geom_line()
#'p + geom_pointless()
#'p + geom_pointless(location = c("first", "last"))
#'p + geom_pointless(location = c("minimum", "maximum"))
#'p + geom_pointless(location = c("all"))
#'
#'# The layer computes one additional variable, `location`, you can map e.g. to the color aesthetic
#'p + geom_pointless(
#'  aes(color = after_stat(location)),
#'  location = c("all"),
#'  size = 3
#'  )
#'
#' \dontrun{
#' # Example using facets, see https://stackoverflow.com/q/29375169
#' p <- ggplot(economics_long, aes(x = date, y = value)) +
#'   geom_line() +
#'  facet_wrap( ~ variable, ncol = 1, scales = 'free_y')
#'
#' p +
#'   geom_pointless(
#'     aes(color = after_stat(location)),
#'     location = c("minimum", "maximum"),
#'     size = 2
#'   )
#'  }
#'
geom_pointless <- function(mapping = NULL,
                           data = NULL,
                           stat = "pointless",
                           position = "identity",
                           ...,
                           location = "last",
                           na.rm = FALSE,
                           orientation = NA,
                           show.legend = NA,
                           inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      location = location,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}
