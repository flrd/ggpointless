#' Draw a catenary curve between fixed points
#'
#' `geom_catenary()` draws a catenary curve which has a U-like shape,
#' superficially similar in appearance to a parabola, which it is not.
#'
#' @section Aesthetics:
#' `geom_catenary()` understands the following aesthetics (required
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
#'   \code{geom_catenary} and \code{stat_catenary}.
#' @param chainLength Length of chain between two points.
#'
#' @details
#' If a flexible chain or rope is loosely hung between two fixed
#' points, it is a curve called a catenary. Catenary, from Latin word catēna,
#' means "chain".
#'
#' to do:
#' - require min 2 observations
#'    Warning message:
#'    Computation failed in `stat_catenary()`
#'    Caused by error in `stats::embed()`:
#'    ! wrong embedding dimension
#' - update
#'    - vignette
#' - add unit test
#' - add snapshot tests
#'
#' @export
#' @examples
#' dat <- data.frame(
#'   x = c(0, 1),
#'   y = c(1, 1)
#' )
#'
#' ggplot(dat, aes(x, y)) +
#'   geom_catenary()
#'
#' # multiple chains are supported
#' dat <- data.frame(
#'   x = c(0, 1, 2),
#'   y = c(1, 1, .5)
#' )
#'
#' ggplot(dat, aes(x, y)) +
#'   geom_catenary(chainLength = 1.5)
#'
#' # use the 'chainLength' argument to overwrite the default behaviour,
#' # which calculates the sum of the Euclidean distances between all
#' # x/y coordinates, muliplies it by 2 and returns it as a messsage
#' dat <- data.frame(
#'   x = c(0, 1, 3),
#'   y = c(0, 1, 2)
#' )
#'
#' ggplot(dat, aes(x, y)) +
#'   geom_catenary()
#'
#' # set chain length to 3 between each pair of points
#' ggplot(dat, aes(x, y)) +
#'   geom_catenary(chainLength = 3)
#'
#' # if you pick a chain length that is too short, a straight line is
#' # drawn and a message tells you the value of the minimum chain length
#' ggplot(dat, aes(x, y)) +
#'   geom_catenary(chainLength = 1)
geom_catenary <- function(mapping = NULL, data = NULL, stat = "catenary",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE,
                          chainLength = NULL, ...) {
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
                          inherit.aes = TRUE, chainLength = NULL,
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

                        setup_params = function(data, params) {
                          if(is.null(params$chainLength)) {
                            tmp_mat <- cbind(data$x, data$y)

                            # if no chainLength is provided, set it to
                            # 2 * Euclidean distance
                            tmp_diff <- sum(sqrt(rowSums(diff(tmp_mat)^2)))
                            params$chainLength <- tmp_diff * 2
                            message("Overall chainLength is ", signif(params$chainLength, 3))
                          }
                          return(params)
                        },

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

  x <- seq(p1$x, p2$x, length.out = segments)
  y <- a * cosh((x - offsetX) / a) + offsetY

  mat <- cbind(x, y)
  dimnames(mat) <- list(NULL, c("x", "y"))

  mat
}

# calculate catenary parameter a
# solving: sqrt(L^2 - v^2) = 2*a * sinh( h / (2*a) )
#' @keywords internal
getCatenaryParameter <- function(h, v, L, iterationLimit = 9) {

  # h: horizontal distance between P1 and P2
  # v: vertical distance between P1 and P2
  # L: length of the curve from P1 to P2

  epsilon <- 1e-6

  # catch potential error when two points have same x-value
  if (h == 0) {
    h <- 1
  }

  m <- sqrt(L^2 - v^2) / h    # initial estimation for a
  x <- acosh(m) + 1           # initialise x
  prevx <- -1
  iteration <- 1

  # iteration until a was found or limit was reached
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

    curveData <- getCurve(a, p1, p2, offsetX, offsetY, segments)
    if (isFlipped) {
      curveData <- rev(curveData)
    }

    return(curveData)

    } else {
      message(
        sprintf(
          "Minimum chain length from '(%s, %s)' to '(%s, %s)' is %s, drawing a straight line.",
          signif(p1$x, 1),
          signif(p1$y, 1),
          signif(p2$x, 1),
          signif(p2$y, 1),
          signif(distance, 3)
      )
    )

    data.frame(x = c(p1$x, p2$x), y = c(p1$y, p2$y))

  }

}

#' @keywords internal
debme <- function(x) {
  tmp <- stats::embed(x, 2)
  cbind(tmp[,2], tmp[, 1])
}


# generalise getCatenaryCurve to handle 3+ points
#' @keywords internal
getCatenaryCurves <- function(data, chainLength = NULL, sorted = FALSE) {

  n_points <- nrow(data)

  if(sorted) {
    data <- data[order(data[, "x"]), ]
  }

  if (identical(n_points, 2L)) {

    point1 <- list(x = data[1, "x"], y = data[1, "y"])
    point2 <- list(x = data[2, "x"], y = data[2, "y"])

    out <- getCatenaryCurve(point1, point2, chainLength)
    } else {
    tmp_seq <- seq.int(n_points)
    tmp_idx <- split.default(debme(tmp_seq), tmp_seq[-n_points])

    catenaryCurves <- lapply(tmp_idx, function(i) {

      data_i <- data[i, ]
      point1 <- list(x = data_i[1, "x"], y = data_i[1, "y"])
      point2 <- list(x = data_i[2, "x"], y = data_i[2, "y"])

      catenaryCurve <- getCatenaryCurve(point1, point2, chainLength)
      catenaryCurve

    })

    out <- unique(do.call(rbind, catenaryCurves))

    }

  as.data.frame(out)

}
