#' Draw a catenary curve between fixed points
#'
#' The catenary curve has a U-like shape, superficially similar in appearance
#' to a parabola, which it is not. If a flexible chain or rope is loosely hung
#' between two fixed points, it is a curve called a catenary. The word
#' 'catenary' is derived from the Latin word caténa, which means "chain".
#'
#' @section Aesthetics:
#' geom_catenary() understands the following aesthetics (required
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
#'   \code{geom_catenary} and \code{stat_catenary}.
#' @param chainLength Length of chain between two points.
#'
#' @details
#' Lorem ipsum
#'
#' @export
#' @examples
#' dat <- data.frame(
#'   x = c(0, 1),
#'   y = c(1, 1)
#' )
#'
#' ggplot(dat, aes(x, y)) +
#'   geom_catenary(chainLength = 2)
#'
geom_catenary <- function(mapping = NULL, data = NULL, stat = "catenary",
position = "identity", na.rm = FALSE,
show.legend = NA, inherit.aes = TRUE,
chainLength, ...) {
  layer(
    geom = GeomPath,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      chainLength = chainLength,
      na.rm = na.rm,
      ...)
  )
}

#' @export
#' @rdname geom_catenary
stat_catenary <- function(mapping = NULL, data = NULL,
geom = "path", position = "identity",
na.rm = FALSE, show.legend = NA,
inherit.aes = TRUE, chainLength = chainLength,
...) {
  ggplot2::layer(
    stat = StatCatenary,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      chainLength = chainLength,
      na.rm = na.rm,
      ...)
  )
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatCatenary <- ggproto("StatCatenary", Stat,
                        compute_group = function(data, scales, chainLength) {
                          getCatenaryCurves(data, chainLength = chainLength)
                          },
                        required_aes = c("x", "y")
                        )


# credit: https://github.com/dulnan/catenary-curve/
#' @keywords internal
getCurve <- function(a, p1, p2, offsetX, offsetY, segments = 101) {

  # a: catenary parameter
  # p1: first point
  # p2: second point
  # offsetX: offset x-axis
  # offsetY: offset y-axis
  # segments: number of segments that make the chain

  # initialise data matrix
  mat <- matrix(NA, ncol = 2, nrow = segments)

  # calculate first point
  mat[1, ] <- c(p1$x, a * cosh((p1$x - offsetX) / a) + offsetY)

  # calculate points between first and last
  d <- p2$x - p1$x

  for (i in seq.int(segments - 1)) {
    x <- p1$x + (d * (i + 0.5)) / (segments - 1)
    y <- a * cosh((x - offsetX) / a) + offsetY
    mat[i + 1, ] <- c(x, y)
  }

  # calculate last point
  mat[segments, ] <- c(p2$x, a * cosh((p2$x - offsetX) / a) + offsetY)

  dimnames(mat) <- list(NULL, c("x", "y"))
  return(mat)
}

# calculate catenary parameter a
# solving: sqrt(L^2 - v^2) = 2*a * sinh( h / (2*a) )
#' @keywords internal
getCatenaryParameter <- function(h, v, L, iterationLimit = 9) {

  # h: horizontal distance between P1 and P2
  # v: vertical distance between P1 and P2
  # L: length of the curve from P1 to P2

  epsilon <- 1e-6

  m <- sqrt(L^2 - v^2) / h    # initial estimation for a
  x <- acosh(m) + 1           # initialise x, as the inverse hyperbolic cosine function
  prevx <- -1                 #
  iteration <- 1

  # Iteriere, bis wir einen geeigneten Catenary-Parameter finden oder das Iterationslimit erreichen
  while (abs(x - prevx) > epsilon && iteration < iterationLimit) {
    prevx <- x
    x <- x - (sinh(x) - m * x) / (cosh(x) - m)
    iteration <- iteration + 1
  }

  return(h / (2 * x))
}


#' @keywords internal
getDistanceBetweenPoints <- function(p1, p2) {
  sqrt((p2$x - p1$x)^2 + (p2$y - p1$y)^2)
}


#' @keywords internal
getCatenaryCurve <- function(point1, point2, chainLength, segments = 101, iterationLimit = 15) {

  # flip curves if needed
  isFlipped <- point1$x > point2$x

  p1 <- if(isFlipped) point2 else point1
  p2 <- if(isFlipped) point1 else point2

  distance <- getDistanceBetweenPoints(p1, p2)

  if (distance < chainLength) {

    h <- p2$x - p1$x
    v <- p2$y - p1$y

    a <- getCatenaryParameter(h, v, chainLength, iterationLimit)

    x <- (a * log((chainLength + v) / (chainLength - v)) - h) * 0.5
    y <- a * cosh(x / a)

    offsetX <- p1$x - x
    offsetY <- p1$y - y

    print(sprintf("a: %f", a))
    print(sprintf("offsetX: %f", offsetX))
    print(sprintf("offsetY: %f", offsetY))

    curveData <- getCurve(a, p1, p2, offsetX, offsetY, segments)
    if (isFlipped) {
      curveData <- rev(curveData)
    }
    return(curveData)
  } else {

    message(sprintf("Minimum chain length between '[x: %.f, y: %.f]' and '[x: %.1f, y: %.1f]' is %.2f, drawing a straight line.", p1$x, p1$y, p2$x, p2$y, distance))
    data.frame(
      x = c(p1$x, p2$x),
      y = c(p1$y, p2$y)
      )

  }

}

#' @keywords internal
debme <- function(x) {
  tmp <- embed(x, 2)
  cbind(tmp[,2], tmp[, 1])
}


# generalise getCatenaryCurve to handle 3+ points
#' @keywords internal
getCatenaryCurves <- function(data, chainLength, sorted = TRUE) {

  n_points <- nrow(data)

  if(sorted) {
    data <- data[order(data[, "x"]), ]
  }

  if (identical(n_points, 2L)) {
    point1 <- list(x = data[1, "x"], y = data[1, "y"])
    point2 <- list(x = data[2, "x"], y = data[2, "y"])
    out <- getCatenaryCurve(point1 = point1,
                            point2 = point2,
                            chainLength)
  } else {

    tmp_seq <- seq.int(n_points)
    tmp_idx <- split.default(debme(tmp_seq), tmp_seq[-n_points])
    catenaryCurves <- lapply(tmp_idx, function(i) {

      data_i <- data[i, ]
      point1 <- list(x = data_i[1, "x"], y = data_i[1, "y"])
      point2 <- list(x = data_i[2, "x"], y = data_i[2, "y"])
      catenaryCurve <- getCatenaryCurve(point1 = point1,
                                        point2 = point2,
                                        chainLength)

      catenaryCurve

    })
    out <- unique(do.call(rbind, catenaryCurves))
  }

  as.data.frame(out)

}
