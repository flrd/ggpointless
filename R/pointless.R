#' A simple point layer to highlight first and/or last observation only
#'
#' This geom is a wrapper around `geom_point()` with 1 additional argument, `point.position`, used to either
#' show points only at the first observation, last observation, or both, first and last.
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPointless <- ggproto("StatPointless", Stat,
                         setup_data = function(data, params) {
                           GeomPoint$setup_data(data, params)
                         },

                         compute_group = function(data, scales, point.position) {
                           rng(data, point.position = point.position)
                         },

                         required_aes = c("x", "y")

)
#' @export
#' @examples
#' p <- ggplot(economics, aes(date, unemploy)) + geom_line()
#' p + geom_pointless()
#'
#' # Change parameters
#' p + geom_pointless(point.position = "both", colour = "red", size = 2)
geom_pointless <- function(mapping = NULL,
                           data = NULL,
                           stat = "pointless",
                           position = "identity",
                           ...,
                           point.position = "last",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      point.position = point.position,
      na.rm = na.rm,
      ...
    )
  )
}
