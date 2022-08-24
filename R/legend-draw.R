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
draw_key_sabline <- function(data, params, size) {
  grid::segmentsGrob(
    x0 = 0.25, y0 = 0.25, x1 = 0.75, y1 = 0.75,
    gp = grid::gpar(
      col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
      lwd = (data$size %||% 0.5) * .pt,
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
draw_key_lexis <- function(data, params, size) {

  # test is.null needed if key glayph is used by another geom_
  # that does not have a point_show param (i.e. all but geom_lexis)
  if (isTRUE(params$point_show) | is.null(params$point_show)) {
    grid::grobTree(
      draw_key_sabline(data, params, size),
      draw_key_pointless(
        transform(data, size = (data$size %||% 1) * 3), params
      )
    )
  } else {
    draw_key_sabline(data, params, size)
  }
}
