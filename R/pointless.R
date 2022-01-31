#' Points to highlight some observations
#'
#' This geom is a wrapper around `geom_point()` with 1 additional argument, `location`, used to either
#' show points only at the first observation, last observation, or both, first and last.
#'
#' @param location A character vector specifying which observations to highlight, defaults to `"last"`.
#'
#' @section Details:
#' The argument `location` allows to control which observations shall be highlighted with a point.
#' When `location` is `"last"`, the default, a single point will be plotted at the last observations.
#' Setting `location` to `"first"` adds a point at the first position.
#'
#'
#' @section Aesthetics:
#' geom_pointless understands the following aesthetics
#' (required aesthetics are in bold):
#'
#' - **x**
#' - **y**
#' - alpha
#' - colour
#' - fill
#' - group
#' - shape
#' - size
#' - stroke
#'
#' Learn more about setting these aesthetics in `vignette("ggplot2-specs")`.
#'
#' @section Computed variables:
#' \describe{
#'   \item{location}{The locations for each row of returned data as characters}
#' }
#'

#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::layer
#'
#' @examples
#' df1 <- data.frame(
#'          x = 1:10,
#'          y = log(1:10)
#'          )
#' p <- ggplot(df1, aes(x, y)) + geom_line()
#' p + geom_pointless()
#'
#' # Change parameters
#' p + geom_pointless(location = c("first", "last"), colour = "red", size = 2)
#'
#' \dontrun{
#' # Example using facets
#' # https://stackoverflow.com/q/29375169
#' library(reshape2)
#' library(ggplot2)
#'
#' me <- melt(economics, id = c("date"))
#' ggplot(data = me, aes(x = date, y = value)) +
#'   geom_line() +
#'   geom_pointless(
#'     aes(colour = after_stat(location)),
#'     location = c("minimum", "maximum"),
#'     size = 2
#'   ) +
#'  facet_wrap( ~ variable, ncol = 1, scales = 'free_y')
#'  }
#' @export
geom_pointless <- function(mapping = NULL,
                           data = NULL,
                           stat = "pointless",
                           position = "identity",
                           ...,
                           location = "last",
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
      location = location,
      na.rm = na.rm,
      ...
    )
  )
}

NULL

#' @rdname geom_pointless
#' @format NULL
#' @usage NULL
#' @export
StatPointless <- ggproto("StatPointless", Stat,

                         setup_params = function(data, params) {
                           GeomLine$setup_params(data, params)
                         },

                         extra_params = c("na.rm", "orientation"),

                         setup_data = function(data, params) {
                           GeomLine$setup_data(data, params)
                         },

                         compute_group = function(data, scales, location) {
                           get_locations(data, location = location)
                         },

                         required_aes = c("x", "y")

)
