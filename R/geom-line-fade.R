#' @include geom-path-fade.R
#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLineFade <- ggplot2::ggproto(
  "GeomLineFade",
  GeomPathFade,

  extra_params = c(GeomPathFade$extra_params, "orientation"),

  setup_params = \(self, data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(
      data,
      params,
      ambiguous = TRUE
    )
    ggplot2::ggproto_parent(GeomPathFade, self)$setup_params(data, params)
  },

  setup_data = \(self, data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- ggplot2::flip_data(data, params$flipped_aes)
    data <- data[order(data$PANEL, data$group, data$x), ]
    ggplot2::flip_data(data, params$flipped_aes)
  }
)


#' @rdname geom_path_fade
#'
#' @description
#' `geom_line_fade()` is identical to `geom_path_fade()` but sorts
#' observations by x before drawing (like [ggplot2::geom_line()]).
#'
#' @concept fading line
#'
#' @export
#' @examples
#'
#' nile_df <- data.frame(year = time(datasets::Nile), value = c(datasets::Nile))
#' ggplot(nile_df, aes(year, value)) +
#'   geom_line_fade()
#'
#' # NA values split the path into sub-paths -- just like geom_line().
#' # The fade is computed over the concatenated arc length of all visible
#' # pieces, so the alpha just before a gap equals the alpha just after,
#' # as if the path were "pulled apart" at the NA.
#' df <- data.frame(x = c(1, 2, 3, 3, 4, 5), y = c(1, 2, NA, 3, 4, 5))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_line_fade(alpha_mode = "gradient", linewidth = 2)
#'
geom_line_fade <- make_constructor(
  GeomLineFade,
  stat = "identity",
  position = "identity",
  alpha_fade_to = 0,
  fade_direction = "start",
  alpha_mode = "auto"
)
