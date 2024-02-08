#' Draw a catenary curve
#'
#' `geom_catenary()` draws a catenary curve which has a U-like shape,
#'  similar in appearance to a parabola, which it is not.
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
#' @export
#' @examples
#' dat <- data.frame(
#'   x = c(0, 1, 2),
#'   y = c(1, 2, -3)
#' )
#'
#' p <- ggplot(dat, aes(x, y))
#' p + geom_catenary() +
#'   ylim(-4, NA)
#'
#' # use chainLength argument to change default behaviour
#' # if you pick a chain length that is too short, a straight line is
#' # drawn and a message about minimum chain length is shown
#' p + geom_catenary(chainLength = 10) +
#'   ylim(-4, NA)
#'
geom_catenary <- function(mapping = NULL, data = NULL, stat = "catenary",
                          position = "identity", ...,
                          chainLength = NULL, show.legend = NA,
                          inherit.aes = TRUE, na.rm = FALSE) {
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
