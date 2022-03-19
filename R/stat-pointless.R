#' @section Computed variables:
#' \describe{
#'   \item{location}{locations, returned as factor}
#' }
#'
#' @export
#' @rdname geom_pointless
stat_pointless <- function(mapping = NULL,
                           data = NULL,
                           geom = "point",
                           position = "identity",
                           ...,
                           location = "last",
                           na.rm = FALSE,
                           orientation = NA,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatPointless,
    geom = geom,
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

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPointless <- ggproto("StatPointless", Stat,
  setup_params = function(data, params) {
    if (!anyDuplicated(data$group)) {
      message(
        paste(
          "Each group consists of only one observation.",
          "Do you need to adjust the group aesthetic?"
        )
      )
    }
    GeomPath$setup_params(data, params)
  },
  extra_params = c("na.rm", "orientation"),
  setup_data = function(data, params) {
    GeomPath$setup_data(data, params)
  },
  compute_group = function(data, scales, location) {
    get_locations(data, location = location)
  },
  required_aes = c("x", "y")
)
