#' Apply Chaikin's corner cutting algorithm to smooth a path
#'
#' @description
#' Chaikin's corner-cutting algorithm can be used to smooth sharp
#' corners of a path.
#'
#' @section Aesthetics:
#' geom_chaikin() understands the following aesthetics (required
#' aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - alpha
#' - color
#' - group
#' - linetype
#' - size
#'
#' @inheritParams ggplot2::geom_path
#' @param geom,stat Use to override the default connection between
#'   \code{geom_chaikin} and \code{stat_chaikin}.
#' @param iterations Integer. Number of iterations to apply. Must be between 0 and 10.
#' @param ratio Numeric. Cutting ratio must be between 0 and 1..
#' @param closed Logical. Specify if result is an open or closed shape.
#' @references Chaikin, G. An algorithm for high speed curve generation.
#' Computer Graphics and Image Processing 3 (1974), 346–349
#'
#' @seealso
#'  \code{\link[ggplot2]{geom_line}}: Connect observations (x order);
#'  \code{\link[ggplot2]{geom_path}}: Connect observations;
#'  \code{\link[ggplot2]{geom_polygon}}: Filled paths (polygons);
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
#'   x = rep(seq.int(10), 3),
#'   y = sample(15:30, 10) * rep(seq.int(3), each = 10),
#'   grp = rep(LETTERS[1:3], each = 10)
#' )
#'
#' p1 <- ggplot(dat, aes(x, y, color = grp)) +
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

#' @export
#' @rdname geom_chaikin
stat_chaikin <- function(mapping = NULL,
                         data = NULL,
                         geom = "path",
                         position = "identity",
                         ...,
                         iterations = 5,
                         ratio = 0.25,
                         closed = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatChaikin,
    geom = geom,
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

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatChaikin <- ggproto("StatChaikin", Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales, params,
                           iterations = 5, ratio = 0.25, closed = FALSE) {
    if (is.null(data)) {
      return(data)
    }

    data <- get_chaikin(x = data$x, y = data$y, iterations = iterations, ratio = ratio, closed = closed)
    if (closed) {
      data <- rbind(data, data[1, , drop = FALSE])
    }
    data
  }
)


# helper to test if a number is a whole number
# in the mathematical sense, unlike is.integer
#' @keywords internal
is_integer <- function(x) {
  is.integer(x) || (is.numeric(x) && identical(x %% 1, 0))
}


lerp <- function(a, b, ratio) {
  a + (b - a) * ratio
}

#' @keywords internal
neighbors <- function(x) {
  # credit:
  # https://github.com/Farbfetzen/corner_cutting/blob/main/main.R
  n <- length(x)
  c(rbind(
    c(x[n], x[-n]),
    c(x[-1], x[1])
  ))
}

#' @keywords internal
lerp_neighbors <- function(x, ratio = .25) {
  a <- rep(x, each = 2)
  b <- neighbors(x)
  lerp(a, b, ratio)
}

#' @keywords internal
cut_corners <- function(x, y, ratio, closed = TRUE) {
  new_x <- lerp_neighbors(x, ratio = ratio)
  new_y <- lerp_neighbors(y, ratio = ratio)

  if (!closed) {
    new_x <- new_x[-c(1, length(new_x))]
    new_y <- new_y[-c(1, length(new_y))]
    new_x[c(1, length(new_x))] <- x[c(1, length(x))]
    new_y[c(1, length(new_y))] <- y[c(1, length(y))]
  }

  list(x = new_x, y = new_y)
}

#' @keywords internal
get_chaikin <- function(x, y, iterations = 5, ratio = .25, closed = FALSE) {
  if (iterations == 0) {
    return(data.frame(x = x, y = y))
  }

  if (!is_integer(iterations) || iterations < 0 || iterations > 10) {
    stop("`iterations` should be a whole number between 1 and 10.")
  }

  if (length(x) == 0 || length(y) == 0) {
    stop("`x` and `y` should have a positive length, returning `data`.")
  }

  if (!identical(length(x), length(y))) {
    stop("`x` and `y` must have the same length.")
  }

  if (ratio > 1 || ratio < 0) {
    stop("`ratio` must be a number between 0 and 1.")
  }

  if (ratio > 0.5) {
    ratio <- 1 - ratio
    message("A value of `ratio` > 0.5 will be flipped to `1 - ratio`")
  }

  for (i in seq.int(iterations)) {
    tmp <- list(x = x, y = y)
    new_xy <- cut_corners(tmp[["x"]], tmp[["y"]], ratio = ratio, closed = closed)
    tmp[["x"]] <- new_xy$x
    tmp[["y"]] <- new_xy$y
    x <- tmp[["x"]]
    y <- tmp[["y"]]
  }
  data.frame(x = x, y = y)
}
