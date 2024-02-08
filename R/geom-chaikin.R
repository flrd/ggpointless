#' Apply Chaikin's corner cutting algorithm to smooth a path
#'
#' @description
#' Chaikin's corner-cutting algorithm can be used to smooth sharp
#' corners of a path.
#'
#' @section Aesthetics:
#' `geom_chaikin()` understands the following aesthetics (required
#' aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - alpha
#' - color
#' - group
#' - linetype
#' - linewidth
#'
#' @inheritParams ggplot2::geom_path
#' @param geom,stat Use to override the default connection between
#'   \code{geom_chaikin} and \code{stat_chaikin}.
#' @param iterations Integer. Number of iterations to apply. Must be between 0 and 10.
#' @param ratio Numeric. Cutting ratio must be between 0 and 1.
#' @param closed Logical. Specify if result is an open or closed shape.
#' @references Chaikin, G. An algorithm for high speed curve generation.
#' Computer Graphics and Image Processing 3 (1974), 346–349
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
#' The ratio parameter  must be a number between 0 and 1. If ratio > 0.5,
#' then it will be flipped to 1 - ratio, and a message is shown.
#'
#' @export
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
#' # ratio let's you control
#' p2 + geom_chaikin(ratio = .1)
#' p2 + geom_chaikin(ratio = .5)
#'
#' # closed parameter to generate a closed shape - or not
#' p2 + geom_chaikin(iterations = 5, ratio = 0.25, closed = FALSE) # default
#' p2 + geom_chaikin(closed = TRUE)
#'
geom_chaikin <- function(mapping = NULL,
                         data = NULL,
                         stat = "chaikin",
                         position = "identity",
                         ...,
                         iterations = 5,
                         ratio = 0.25,
                         closed = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    geom = GeomPath,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      iterations = iterations,
      ratio = ratio,
      closed = closed,
      na.rm = na.rm,
      ...
    )
  )
}
