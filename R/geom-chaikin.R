#' Apply Chaikin's corner cutting algorithm to smooth a path
#'
#' @description
#' Chaikin's corner-cutting algorithm is used to smooth sharp
#' corners in a path. The algorithm iteratively turns a jagged
#' path into a smooth curve.
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
#' @param iterations Integer between 0 and 15. Number of iterations to apply.
#' @param ratio A logical value indicating whether the spline is an open or a
#'        closed shape.
#' @param closed Logical. Should first and last observation be connected? That is,
#' an open or a closed shape
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
#' For each pair of neighboring points P1 and P2 on the original path, by default,
#' the algorithm finds the 2 new points that are 25% and 75% of the way between
#' P1 and P2. Those new points form a smoother path. The ratio parameter let's
#' you control the distance of the new points from P1 and P2. It must be a number
#' between 0 and 1. If ratio > 0.5, then it will be flipped to 1 - ratio, and a
#' message is shown.
#'
#' If the iterations parameter is 0 or less, a path is drawn based on your input
#' data. The maximum number of iterations is 10, default is 5.
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
#' p <- ggplot(dat, aes(x, y, color = grp)) +
#'   geom_point() +
#'   geom_line(linetype = "12")
#'
#' p +
#'   geom_chaikin()
#'
#' p +
#'   geom_chaikin(iterations = 1)
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

#' @keywords internal
lerp_neighbors <- function(x, ratio = .25) {
  # credit:
  # https://github.com/Farbfetzen/corner_cutting/blob/main/main.R
  #
  # example
  # x:               1, 2, 3, 4
  # left neighbors:  4, 1, 2, 3
  # right neighbors: 2, 3, 4, 1
  #
  # neighbors and x combined in the correct order:
  # x:         1, 1, 2, 2, 3, 3, 4, 4
  # neighbors: 4, 2, 1, 3, 2, 4, 3, 1

  n <- length(x)
  a <- rep(x, each = 2)
  b <- c(rbind(
    c(x[n], x[-n]),
    c(x[-1], x[1])
  ))
  a + (b - a) * ratio
}


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
