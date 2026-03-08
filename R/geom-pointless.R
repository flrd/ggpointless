#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPointless <- ggproto(
  "GeomPointless",
  ggplot2::GeomPoint,
  stat = "pointless"
)

#' @title Emphasize some observations with points
#'
#' @description This is a wrapper around [`ggplot2::geom_point()`] with the one
#' additional argument: `location`. This geom aims to emphasis some observations
#' but is not particularly useful on its own - hence its name - but hopefully
#' in conjunction with `geom_line()` and friends, see examples.
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::layer
#' @param location Position(s) to highlight: `"minimum"`, `"maximum"`,
#' `"first"`, `"last"` (default), or `"all"`.
#'
#' @section Details:
#' The `location` argument allows you to specify which observations
#' should be highlighted. If `location` is `"last"`, the default, a
#' single point will be plotted at the last non-missing observation.
#' The locations are determined in the order in which they appear in
#' the data -- like [ggplot2::geom_path()] does compared to [ggplot2::geom_line()].
#'
#' Points may be plotted on top of one another; if `location` is set
#' to `"all"`, then the order in which points are plotted from top to
#' bottom is: `"first"` > `"last"` > `"minimum"` > `"maximum"`.
#' Otherwise, the order is determined as specified in the `location` argument,
#' which also then applies to the order legend key labels, see
#' `vignette("ggpointless")` for more details.
#'
#' @aesthetics GeomPointless
#'
#' @seealso
#'    [ggplot2::geom_point()]
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#' @export
#' @examples
#' x <- seq(-pi, pi, length.out = 150)
#' y <- outer(x, 1:5, FUN = \(x, y) sin(x * y))
#'
#' df1 <- data.frame(
#'   x = x,
#'   y = rowSums(y)
#' )
#'
#' # not terribly useful on its own ...
#' p <- ggplot(df1, aes(x = x, y = y))
#' p + geom_pointless()
#' p + geom_pointless(location = "all")
#'
#' # ... but in conjunction with geom_line(), hopefully
#' p <- p + geom_line()
#' p + geom_pointless(location = "all")
#' p + geom_pointless(location = c("first", "last"))
#' p + geom_pointless(location = c("minimum", "maximum"))
#'
#' # The layer computes one additional variable, 'location',
#' # that you can map e.g. to colour
#' p + geom_pointless(
#'   aes(colour = after_stat(location)),
#'   location = "all",
#'   size = 3
#' )
#'
#' # Example with missing first and last observations
#' set.seed(42)
#' df2 <- data.frame(x = 1:10, y = c(NA, sample(1:8), NA))
#' ggplot(df2, aes(x, y)) +
#'   geom_line() +
#'   geom_pointless(location = c("first", "last"))
#'
#' # Change the order in which points are drawn when they overlap
#' df3 <- data.frame(x = 1:2, y = 1:2)
#'
#' p <- ggplot(df3, aes(x = x, y = y)) +
#'   geom_path() +
#'   coord_equal()
#'
#' # same as location = 'all'
#' p + geom_pointless(aes(colour = after_stat(location)),
#'   location = c("first", "last", "minimum", "maximum")
#' ) +
#'   labs(subtitle = "same as location = 'all'")
#'
#' # reversed custom order
#' p + geom_pointless(aes(colour = after_stat(location)),
#'   location = c("maximum", "minimum", "last", "first")
#' ) +
#'   labs(subtitle = "custom order")
#'
#' # same as location = 'all' again
#' p + geom_pointless(aes(colour = after_stat(location)),
#'   location = c("maximum", "minimum", "last", "first", "all")
#' ) +
#'   labs(subtitle = "same as location = 'all' again")
#'
#' # Use stat_pointless() with a geom other than "point"
#' set.seed(42)
#' df4 <- data.frame(x = 1:10, y = sample(1:10))
#' ggplot(df4, aes(x, y)) +
#'   geom_line() +
#'   geom_pointless(location = c("maximum", "minimum"), size = 3) +
#'   stat_pointless(
#'     aes(label = after_stat(y)),
#'     location = c("maximum", "minimum"),
#'     geom = "text",
#'     hjust = -1
#'   )
#'
#' # Example using facets
#' # https://stackoverflow.com/q/29375169
#' p <- ggplot(economics_long, aes(x = date, y = value)) +
#'   geom_line() +
#'   facet_wrap(vars(variable), ncol = 1, scales = "free_y")
#'
#' p + geom_pointless(
#'   aes(colour = after_stat(location)),
#'   location = c("minimum", "maximum"),
#'   size = 2
#'   )
#'
geom_pointless <- make_constructor(GeomPointless, stat = "pointless", location = "last")
