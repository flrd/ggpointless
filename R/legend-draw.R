#' Key glyphs for legends
#'
#' @inheritParams ggplot2::draw_key
#' @return A grid grob.
#' @name draw_key

#' @rdname draw_key
#' @keywords internal
draw_key_pointless <- function(data, params, size) {

  if (is.null(data$shape)) {
    data$shape <- 19
  } else if (is.character(data$shape)) {
    data$shape <- ggplot2:::translate_shape_string(data$shape)
  }
  grid::pointsGrob(0.85, 0.85, pch = data$shape,
                   gp = grid::gpar(
                     col = alpha(data$colour %||% "black", data$alpha),
                     fill = alpha(data$fill %||% "black", data$alpha),
                     fontsize = (data$size %||% 1.5) * .pt + (data$stroke %||% 0.5) * .stroke / 2,
                     lwd = (data$stroke %||% 0.5) * .stroke / 2
                   )
  )
}

#' @rdname draw_key
#' @export
draw_key_lexis <- function(data, params, size) {

  grid::grobTree(
    grid::segmentsGrob(x0 = 0.15, y0 = 0.15, x1 = 0.85, y1 = 0.85, gp = grid::gpar(
      col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
      lwd = (data$size %||% 0.5) * .pt,
      lty = data$linetype %||% 1,
      lineend = "round"
    ),
    arrow = params$arrow
    ),
    draw_key_pointless(transform(data, size = (data$size %||% 1.5) * 3.5), params)
  )
}



# draw_key_lexis <- function(data, params, size) {
#   grid::gList(
#     grid::segmentsGrob(x0 = 0.15, y0 = 0.15, x1 = 0.85, y1 = 0.85,
#                        gp = grid::gpar(
#                          col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
#                          lwd = (data$size %||% 0.5) * .pt,
#                          lty = data$linetype %||% 1,
#                          lineend = params$lineend %||% "round"
#                        ),
#                        arrow = params$arrow
#     ),
#     draw_key_point(data, size, params)
#   )
# }

# draw_key_lexis <- function(data, params, size) {
#
#   if (is.null(data$shape)) {
#     data$shape <- 19
#   }
#   else if (is.character(data$shape)) {
#     data$shape <- ggplot2:::translate_shape_string(data$shape)
#   }
#
#   grid::grobTree(
#     grid::segmentsGrob(x0 = 0.15, y0 = 0.15, x1 = 0.85, y1 = 0.85, gp = grid::gpar(
#       col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
#       lwd = (data$size %||% 0.5) * .pt,
#       lty = data$linetype %||% 1,
#       lineend = "round"
#     ),
#     arrow = params$arrow
#     ),
#     grid::pointsGrob(x = unit(0.9, "npc"), y = unit(0.9, "npc"), pch = data$shape, size = data$size %||% 1.5, gp = grid::gpar(
#       col = alpha(data$colour %||% "black", data$alpha),
#       fill = alpha(data$fill %||% "black", data$alpha),
#       fontsize = (data$size %||% 1.5) * .pt + (data$stroke %||% 0.5) * .stroke/2,
#       lwd = (data$stroke %||% 0.5) * .stroke/2)
#     )
#   )
# }
