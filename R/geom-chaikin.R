#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomChaikin <- ggplot2::ggproto(
  "GeomChaikin",
  ggplot2::GeomPath,
  extra_params = c(
    ggplot2::GeomPath$extra_params,
    "mode",
    "iterations",
    "ratio",
    "closed"
  )
)

#' @title Apply Chaikin's corner cutting algorithm to smooth a path
#'
#' @description
#' Chaikin's corner-cutting algorithm can be used to smooth sharp
#' corners of a path.
#'
#' @details
#' Chaikin's corner cutting algorithm iteratively turns a jagged path into
#' a smooth path.
#'
#' The recursion formula starts from two vertices A and B, which represent
#' a single corner of your path. From this, the algorithm derives two new
#' points: one at the specified ratio when going from point A to point B,
#' and one when going from B to A in the opposite direction.
#' By default, a ratio of 0.25 results in two points: the first at 25% of
#' point A and the other at 75% of point A (or 25% of point B). Those new
#' points form a smoother path. Then the algorithm applies the same rule to
#' each pair of new points. The rule is applied iterations times. The
#' maximum number of iterations is 10, default is 5.
#'
#' @aesthetics GeomChaikin
#'
#' @inheritParams ggplot2::geom_path
#' @param geom,stat Use to override the default connection between
#'   `geom_chaikin()` and `stat_chaikin()`.
#' @param iterations Integer. Number of iterations to apply between `1` and
#'  `10`. When `iterations = 0` the original data is unchanged so essentially
#'   this is the same as calling [`ggplot2::geom_path()`]; however this might
#'   be useful when you want to toggle smoothing on/off programmatically without
#'   removing the layer.
#' @param ratio Numeric. Cutting ratio must be a number between `0` and `1`.
#'   If `ratio > 0.5`, then it will be flipped to `1 - ratio`.
#' @param mode Character. Should the geom draw a closed polygon or an open
#'   path? Must be one of `"open"` (default) or `"closed"`.
#' @param closed `r lifecycle::badge("deprecated")` Use `mode` instead.
#'
#' @references Chaikin, G. An algorithm for high speed curve generation.
#' Computer Graphics and Image Processing 3 (1974), 346–349
#'
#' @seealso The [smoothr](https://strimas.com/smoothr/) package offers tools to
#'  smooth and tidy spatial features
#'
#' @examples
#' set.seed(42)
#' dat <- data.frame(
#'   x = seq.int(10),
#'   y = sample(15:30, 10)
#' )
#'
#' p1 <- ggplot(dat, aes(x, y)) +
#'   geom_line(linetype = "12")
#'
#' p1 +
#'   geom_chaikin()
#'
#' p1 +
#'   geom_chaikin(iterations = 1)
#'
#' triangle <- data.frame(x = c(0, 0, 1), y = c(0, 1, 1))
#' p2 <- ggplot(triangle, aes(x, y)) +
#'   geom_path(linetype = "12") +
#'   coord_equal()
#'
#' # ratio lets you control the cutting amount
#' p2 + geom_chaikin(ratio = .1)
#' p2 + geom_chaikin(ratio = .5)
#'
#' # mode controls whether the result is an open or closed shape
#' p2 + geom_chaikin(mode = "open")   # default
#' p2 + geom_chaikin(mode = "closed")
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#' @export
#' @rdname geom_chaikin
geom_chaikin <- make_constructor(
  GeomChaikin,
  stat = "chaikin",
  mode = "open",
  iterations = 5,
  ratio = 0.25
)
