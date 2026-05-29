#' @include geom-stipple-path.R
NULL

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomStippleLine <- ggplot2::ggproto(
  "GeomStippleLine",
  GeomStipplePath,

  extra_params = c(GeomStipplePath$extra_params, "orientation"),

  setup_params = function(self, data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(
      data,
      params,
      ambiguous = TRUE
    )
    ggplot2::ggproto_parent(GeomStipplePath, self)$setup_params(data, params)
  },

  setup_data = function(self, data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- ggplot2::flip_data(data, params$flipped_aes)
    data <- data[order(data$PANEL, data$group, data$x), ]
    ggplot2::flip_data(data, params$flipped_aes)
  }
)


#' @rdname geom_stipple_path
#'
#' @description
#' `geom_stipple_line()` orders observations along the independent axis before
#' connecting them (like [ggplot2::geom_line()]).
#'
#' @export
#' @examples
#'
#' # A series that runs vertically: orientation = "y"
#' ggplot(economics, aes(unemploy, date)) +
#'   geom_stipple_line(dot_spacing = "coarse", orientation = "y")
geom_stipple_line <- make_constructor(
  GeomStippleLine,
  stat        = "identity",
  position    = "identity",
  dot_spacing = "medium",
  radius      = NULL,
  type        = "hex"
)
