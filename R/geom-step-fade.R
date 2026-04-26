# Internal copy of ggplot2:::stairstep to avoid ::: dependency.
# Transforms path data into staircase-step coordinates.
# Source: https://github.com/tidyverse/ggplot2/blob/v4.0.2/R/geom-path.R
#' @noRd
#' @keywords internal
.stairstep <- function(data, direction = "hv") {
  direction <- rlang::arg_match0(direction, c("hv", "vh", "mid"))
  data <- as.data.frame(data)[order(data$x), ]
  n <- nrow(data)

  if (n <= 1L) {
    return(data[0L, , drop = FALSE])
  }

  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2 * n]
    ys <- c(1, rep(2:n, each = 2))
  } else if (direction == "hv") {
    ys <- rep(1:n, each = 2)[-2 * n]
    xs <- c(1, rep(2:n, each = 2))
  } else if (direction == "mid") {
    xs <- rep(1:(n - 1), each = 2)
    ys <- rep(1:n, each = 2)
  }

  if (direction == "mid") {
    gaps <- data$x[-1] - data$x[-n]
    mid_x <- data$x[-n] + gaps / 2
    x <- c(data$x[1], mid_x[xs], data$x[n])
    y <- c(data$y[ys])
    data_attr <- data[
      c(1, xs, n),
      setdiff(names(data), c("x", "y")),
      drop = FALSE
    ]
  } else {
    x <- data$x[xs]
    y <- data$y[ys]
    data_attr <- data[xs, setdiff(names(data), c("x", "y")), drop = FALSE]
  }

  out <- cbind(data.frame(x = x, y = y), data_attr)
  rownames(out) <- NULL
  out
}


#' @include geom-path-fade.R
#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomStepFade <- ggplot2::ggproto(
  "GeomStepFade",
  GeomPathFade,

  extra_params = c(GeomPathFade$extra_params, "orientation", "direction"),

  setup_params = \(self, data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(
      data,
      params,
      ambiguous = TRUE
    )
    ggplot2::ggproto_parent(GeomPathFade, self)$setup_params(data, params)
  },

  draw_panel = \(
    self,
    data,
    panel_params,
    coord,
    arrow = NULL,
    arrow.fill = NULL,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    na.rm = FALSE,
    alpha_fade_to = 0,
    fade_direction = "start",
    alpha_mode = "auto",
    direction = "hv",
    flipped_aes = FALSE
  ) {
    if (.is_uniform_alpha(data, alpha_fade_to)) {
      return(ggplot2::GeomStep$draw_panel(
        data, panel_params, coord,
        direction = direction, na.rm = na.rm
      ))
    }

    data <- ggplot2::flip_data(data, flipped_aes)
    if (isTRUE(flipped_aes)) {
      direction <- switch(direction, hv = "vh", vh = "hv", direction)
    }

    # Apply stairstep transformation per group
    groups <- split(data, data$group)
    data <- do.call(rbind, lapply(groups, .stairstep, direction = direction))
    rownames(data) <- NULL

    data <- ggplot2::flip_data(data, flipped_aes)

    ggplot2::ggproto_parent(GeomPathFade, self)$draw_panel(
      data = data,
      panel_params = panel_params,
      coord = coord,
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm,
      alpha_fade_to = alpha_fade_to,
      fade_direction = fade_direction,
      alpha_mode = alpha_mode
    )
  }
)


#' @rdname geom_path_fade
#'
#' @description
#' `geom_step_fade()` is identical to `geom_path_fade()` but draws a
#' staircase-step path (like [ggplot2::geom_step()]). The `direction` argument
#' controls the step shape: `"hv"` (horizontal then vertical, the default),
#' `"vh"` (vertical then horizontal), or `"mid"` (step at the midpoint).
#'
#' @param direction Direction of the steps. One of `"hv"` (default, horizontal
#'   then vertical), `"vh"` (vertical then horizontal), or `"mid"` (step at
#'   the midpoint between adjacent x values).
#'
#' @concept step function
#'
#' @export
#' @examples
#'
#' # Fading step function
#' set.seed(42)
#' d <- data.frame(
#'   x   = rep(1:10, 2),
#'   y   = c(cumsum(rnorm(10)), cumsum(rnorm(10))),
#'   grp = rep(c("a", "b"), each = 10)
#' )
#'
#' ggplot(d, aes(x, y, colour = grp)) +
#'   geom_step_fade(linewidth = 1, direction = "vh") +
#'   theme_minimal()
#'
geom_step_fade <- make_constructor(
  GeomStepFade,
  stat = "identity",
  position = "identity",
  alpha_fade_to = 0,
  fade_direction = "start",
  alpha_mode = "auto",
  direction = "hv"
)
