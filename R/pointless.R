#' Points to highlight some observations
#'
#' This is a wrapper around `geom_point()` with 1 additional argument: `location`. It allows to highlight
#' some observations, currently these are the first observation, the last one,
#' and observations with minimum or maximum value. This geom is not particularly useful on its own (hence the name),
#' but hopefully in conjunction with `geom_path()` and friends, see examples.
#'
#' @param location A character vector specifying which observations to highlight, defaults to `"last"`.
#' @param orientation The orientation of the layer. The default (`NA`) automatically determines
#' the orientation from the aesthetic mapping. In the rare event that this fails it can be given
#' explicitly by setting `orientation` to either `"x"` or `"y"`. See the Orientation section for more detail.
#'
#' @section Details:
#' The argument `location` allows you to control which observations to highlight - with a point.
#' When `location` is `"last"`, the default, a single point will be plotted at the last observations.
#' Setting `location` to `"first"` adds a point at the first position.
#'
#'
#'
#' @section Computed variables:
#' \describe{
#'   \item{location}{The locations for each row of returned data as characters}
#' }
#'
#' @eval ggplot2:::rd_orientation()
#'
#' @eval wrap_rd_aesthetics("geom", "point", "pointless")
#'
#' @import ggplot2
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::layer
#'
#' @export
#' @examples
#'# example data
#'x <- seq(-2 * pi, 2 * pi, length.out = 400)
#'y <- sin(1/4 * x) + sin(1/2 * x) + sin(x)
#'
#'df1 <- data.frame(
#'  var1 = x,
#'  var2 = y
#')
#'
#'# not terribly useful on its own ...
#'p <- ggplot(df1, aes(x = var1, y = var2))
#'p + geom_pointless()
#'p + geom_pointless(location = "all")
#'p <- p + geom_line()
#'
#'# ... but in conjunction with geom_line(), hopefully
#'p + geom_pointless()
#'p + geom_pointless(location = c("first", "last"))
#'p + geom_pointless(location = c("minimum", "maximum"))
#'p + geom_pointless(location = c("all"))
#'
#'# The layer computes one additional variable, `location`, you can map e.g. to the colour aesthetic
#'p + geom_pointless(
#'  aes(colour = after_stat(location)),
#'  location = c("all"),
#'  size = 3
#'  )
#'
#' \dontrun{
#' # Example using facets, see https://stackoverflow.com/q/29375169
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
geom_pointless <- function(mapping = NULL,
                           data = NULL,
                           stat = "pointless",
                           position = "identity",
                           ...,
                           location = "last",
                           na.rm = FALSE,
                           orientation = NA,
                           show.legend = NA,
                           inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      location = location,
      na.rm = na.rm,
      orientation = orientation,
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

                           # minimum and maximum don't make much sense when data has zero variance
                           if((var(data$y) == 0) & (any(data$location) %in% c("minimum", "maximum", "all"))) {
                             message("There is no variation in your data.")
                           }

                           get_locations(data, location = location)
                         },

                         required_aes = c("x", "y")

)
