#' Lexis charts
#'
#' The lexis geom is used to plot 45° "lifelines" for each cohort. Lexis diagrams
#' are used by demographers for more than a century and they are named after Wilhelm Lexis.
#' They are a combination of a segment, starting at 0 with a dot at the end.
#'
#' @section Aesthetics:
#' geom_pointless() understands the following aesthetics (required aesthetics are in bold):
#'
#' - **x**
#' - **xend**
#' - alpha
#' - color
#' - fill
#' - group
#' - shape
#' - size
#' - linetype
#' - stroke
#' @inheritParams ggplot2::layer
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
#' @param lineend	Line end style (round, butt, square)
#' @param point.show logical. Should a point be added add the end of each segment? TRUE by default
#' @param point.size the size of the point
#' @param point.colour the colour of the point
#' @param gap_filler logical. Should a line be drawn connecting segments if there are gaps? TRUE by default
#' @inheritParams ggplot2::layer
#' @details
#' This geom draws 45° lines from the start to the end of a lifetime. The geom
#' generates one variable called `'type'` in the layer data, that you can map
#' with [after_scale()] to an aesthetic.
#' See the examples and `vignette("ggpointless")` for more details.
#'
#' @export
#' @examples
#'df1 <- data.frame(
#'  key = c("A", "B", "B", "C", "D", "E"),
#'  start = c(0, 1, 6, 5, 6, 9),
#'  end = c(5, 4, 10, 9, 8, 11)
#')
#'ggplot(df1, aes(x = start, xend = end, color = key)) +
#'  geom_lexis() +
#'  coord_equal()
#'
#'ggplot(df1, aes(x = start, xend = end, color = key)) +
#'  geom_lexis(aes(linetype = after_scale(type))) +
#'  coord_equal()
#'
geom_lexis <- function(mapping = NULL, data = NULL, ...,
                       point.show = TRUE, point.colour = NULL, point.size = NULL, gap_filler = TRUE, lineend = "round",
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomLexis,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      lineend = lineend,
      gap_filler = gap_filler,
      point.show = point.show,
      point.colour = point.colour,
      point.size = point.size,
      ...
    )
  )
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @include legend-draw.R
#' @export
GeomLexis <- ggproto("GeomLexis", Geom,
   required_aes = c("x", "xend"),
   non_missing_aes = c("size", "shape", "point.colour", "point.size", "type"),
   default_aes = aes(
     shape = 19, colour = "black", linetype = "solid", size = 0.3, fill = NA,
     alpha = NA, stroke = 0.5
   ),
   setup_data = function(data, params) {

     if(all(is.na(data$xend))) {
       x_max <- max(data$x, na.rm = TRUE)
       message(paste("All 'xend' values missing, setting those to", x_max))
       data$xend <- x_max
     }

     if(anyNA(data$xend)) {
       xend_max <- max(data$xend, na.rm = TRUE)
       message(paste("Missing 'xend' values set to", xend_max))
       data$xend <- replace(data$xend, is.na(data$xend), xend_max)
       }

     cols_to_keep <- setdiff(names(data), c("x", "y", "xend", "yend"))
     lst <- by(data, data$group, FUN = function(grp) {
       lexis_segment <- get_lexis(grp$x, grp$xend)
       suppressWarnings({
         cbind(lexis_segment, grp[1, cols_to_keep])
       })
     })

     do.call(rbind, lst)
   },

   draw_group = function(data, panel_params, coord,
                         lineend = "round",
                         gap_filler = TRUE,
                         point.show = TRUE, point.colour = NULL, point.size = NULL) {

     points <- tail(data, 1)
     points$colour <- point.colour %||% points$colour
     points$size <- point.size %||% (points$size * 5)
     points <- transform(points, x = xend, y = yend)
     points <- subset(points, select = c(-xend, -yend))

     if(!isTRUE(gap_filler)) {
       data <- subset(data, type != "12")
     }

     if(point.show) {
       grid::gList(
         ggplot2::GeomSegment$draw_panel(data, panel_params, coord),
         ggplot2::GeomPoint$draw_panel(points, panel_params, coord)
       )
     } else {
       grid::gList(
         ggplot2::GeomSegment$draw_panel(data, panel_params, coord)
         )
       }
     },
   draw_key = draw_key_lexis

)
