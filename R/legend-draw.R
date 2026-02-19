#' Key glyphs for legends
#'
#' Each geom has an associated function that draws the key when
#' the geom needs to be displayed in a legend. These functions are
#' called  draw_key_*(), where * stands for the name of the
#' respective key glyph. The key glyphs of the [geom_lexis()]
#' function draws a short line of 45° with a point at the end,
#' see examples below.
#'
#' @inheritParams ggplot2::draw_key
#' @return A grid grob.
#' @name draw_key_lexis

#' @rdname draw_key_lexis
#' @keywords internal
draw_key_pointless <- function(data, params, size) {
  if (is.null(data$shape)) {
    data$shape <- 19
  } else if (is.character(data$shape)) {
    data$shape <- translate_shape_string(data$shape)
  }
  grid::pointsGrob(
    x = 0.75, y = 0.75,
    pch = data$shape,
    gp = grid::gpar(
      col = alpha(data$colour %||% "black", data$alpha),
      fill = alpha(data$fill %||% "black", data$alpha),
      fontsize = (data$size %||% 1.5) * .pt + (data$stroke %||% 0.5) * .stroke / 2,
      lwd = (data$stroke %||% 0.5) * .stroke / 2
    )
  )
}

#' @rdname draw_key_lexis
#' @keywords internal
draw_key_sabline <- function(data, params, linewidth, size) {
  grid::segmentsGrob(
    x0 = 0.25, y0 = 0.25, x1 = 0.75, y1 = 0.75,
    gp = grid::gpar(
      col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
      lwd = (data$linewidth %||% 0.5) * .pt,
      lty = data$linetype %||% 1,
      lineend = "round"
    )
  )
}

#' @rdname draw_key_lexis
#' @export
#' @examples
#' df1 <- data.frame(x = c(0, 1), xend = c(2, 3), grp = c("A", "B"))
#' ggplot(df1, aes(x, xend = xend, color = grp)) +
#'   geom_lexis()
#'
#' # the glyph can be changed using the `key_glyph` argument
#' ggplot(mtcars, aes(wt, mpg, color = "red")) +
#'   geom_point(key_glyph = "lexis")
draw_key_lexis <- function(data, params, linewidth, size) {

  # test is.null needed if key glyph is used by another geom_*
  # that does not have a point_show param (i.e. all but geom_lexis)
  if (isTRUE(params$point_show) || is.null(params$point_show)) {
    grid::grobTree(
      draw_key_sabline(data, params, size),
      draw_key_pointless(
        transform(data, size = (data$size %||% 2) * 0.65), params
      )
    )
  } else {
    draw_key_sabline(data, params, size)
  }
}

#' @rdname draw_key_area_fade
#' @keywords internal
draw_key_area_fade = function(data, params, size) {
  # We still need to check orientation for the legend icon
  flipped <- params$flipped_aes %||% FALSE
  fill_color <- data$fill %||% "grey20"
  a_start <- data$alpha %||% 1
  a_end   <- params$alpha_fade_to %||% 0
  
  grad_params <- if (flipped) {
    list(
      x1 = 1,
      y1 = 0.5,
      x2 = 0,
      y2 = 0.5
    )
  } else {
    list(
      x1 = 0.5,
      y1 = 1,
      x2 = 0.5,
      y2 = 0
    )
  }
  
  grad <- grid::linearGradient(
    colours = c(
      scales::alpha(fill_color, a_start),
      scales::alpha(fill_color, a_end)
    ),
    x1 = grad_params$x1,
    y1 = grad_params$y1,
    x2 = grad_params$x2,
    y2 = grad_params$y2
  )
  
  grid::rectGrob(gp = grid::gpar(fill = grad, col = NA))
}
