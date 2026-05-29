#' @include geom-stipple-path.R
NULL

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomStippleStep <- ggplot2::ggproto(
  "GeomStippleStep",
  GeomStipplePath,

  extra_params = c(GeomStipplePath$extra_params, "orientation", "direction"),

  setup_params = function(self, data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(
      data,
      params,
      ambiguous = TRUE
    )
    ggplot2::ggproto_parent(GeomStipplePath, self)$setup_params(data, params)
  },

  draw_panel = function(data, panel_params, coord,
                        dot_spacing = "medium", radius = NULL, type = "hex",
                        direction = "hv", flipped_aes = FALSE, na.rm = FALSE) {
    .check_panel_range(panel_params, "geom_stipple_step")
    .stipple_warn_na(data, na.rm, "geom_stipple_step")

    # Stairstep in the canonical orientation, then flip back, mirroring
    # GeomStep / GeomStepFade: the stipple then approximates the stair path.
    data <- ggplot2::flip_data(data, flipped_aes)
    if (isTRUE(flipped_aes)) {
      direction <- switch(direction, hv = "vh", vh = "hv", direction)
    }
    groups <- split(data, data$group)
    data <- do.call(rbind, lapply(groups, .stairstep, direction = direction))
    rownames(data) <- NULL
    data <- ggplot2::flip_data(data, flipped_aes)

    .stipple_grob(
      data, panel_params, coord,
      dot_spacing   = dot_spacing,
      type          = type,
      pos_aes       = c("x", "y"),
      keep_fun      = .stipple_path_keep_fun(.stipple_resolve_radius(radius), type),
      dot_radius_pt = .stipple_dot_radius_pt(data)
    )
  }
)


#' @rdname geom_stipple_path
#'
#' @description
#' `geom_stipple_step()` approximates a stair-step path (like
#' [ggplot2::geom_step()]); `direction` controls the step shape.
#'
#' @param direction `"hv"` (horizontal then vertical, default), `"vh"`
#'   (vertical then horizontal), or `"mid"` (step half-way between adjacent x
#'   values). `geom_stipple_step()` only.
#'
#' @export
#' @examples
#'
#' # Stair-step stipple
#' recent <- economics[economics$date > as.Date("2013-01-01"), ]
#' ggplot(recent, aes(date, unemploy)) +
#'   geom_stipple_step(dot_spacing = "coarse")
geom_stipple_step <- make_constructor(
  GeomStippleStep,
  stat        = "identity",
  position    = "identity",
  dot_spacing = "medium",
  radius      = NULL,
  type        = "hex",
  direction   = "hv"
)
