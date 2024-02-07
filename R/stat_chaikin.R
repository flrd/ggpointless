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

#' @keywords internal
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
