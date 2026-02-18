#' @title Area with Gradient Fill
#' @description
#' A custom geom that creates area plots where the fill color fades
#' from opaque to transparent.
#' 
#' The geom behaves much like [geom_area()] uses [grid::linearGradient()] to
#' fill the area between `y` and `0`. It supports both vertical and 
#' horizontal orientations (`orientation = "y"`).
#'
#' @aesthetics GeomArea
#' @seealso
#'   [geom_area()] for fully opaque area charts
#' @inheritSection shared_layer_parameters Orientation
#' @inheritParams ggplot2::geom_area
#' @param alpha_fade_to The alpha value the gradient fades to.
#'   Defaults to `0` (fully transparent).
#' @export
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   g = c("a", "a", "a", "b", "b", "b"),
#'   x = c(1, 3, 5, 2, 4, 6),
#'   y = c(2, 5, 1, 3, 6, 7)
#' )
#'
#' a <- ggplot(df, aes(x, y, fill = g)) + 
#'   theme_minimal()
#'
#' # default behaviour
#' a + geom_area_fade()
#'
#' # change alpha value to make area more transparent
#' a + geom_area_fade(alpha = .5)
#'
#' # fade from 75% opacity to 25%
#' a + geom_area_fade(alpha = .75, alpha_fade_to = .25)
#'
#' # reverse fading from bottom to top
#' a + geom_area_fade(alpha = 0, alpha_fade_to = 1)
#'
#' # draw outlines
#' a + geom_area_fade(outline.type = "both")
#'
#' # change orientation
#' a + geom_area_fade(orientation = "y")
#'
#' # To turn off the alignment, the stat can be set to "identity"
#' a + geom_area_fade(stat = "identity")
geom_area_fade <- function(mapping = NULL,
                           data = NULL,
                           stat = "align",
                           position = "stack",
                           ...,
                           alpha_fade_to = NULL,
                           outline.type = NULL,
                           orientation = NA,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  # Internal validation
  if (!is.null(alpha_fade_to) &&
      (alpha_fade_to < 0 || alpha_fade_to > 1)) {
    stop("alpha_fade_to must be between 0 and 1", call. = FALSE)
  }
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAreaFade,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      alpha_fade_to = alpha_fade_to,
      outline.type = outline.type,
      orientation = orientation,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_area_fade
#' @format NULL
#' @usage NULL
#' @export
GeomAreaFade <- ggplot2::ggproto(
  "GeomAreaFade",
  ggplot2::GeomArea,
  
  # define extra_params so alpha_fade_to and outline.type
  # reach draw_group() instead of being stripped away.
  extra_params = c("na.rm", "orientation", "alpha_fade_to", "outline.type"),
  
  # use custom legend key
  draw_key = draw_key_area_fade,
  
  draw_group = function(self,
                        data,
                        panel_params,
                        coord,
                        na.rm = FALSE,
                        flipped_aes = FALSE,
                        alpha_fade_to = NULL,
                        outline.type = "upper") {
    coords <- coord$transform(data, panel_params)
    if (nrow(coords) < 2) return(ggplot2::zeroGrob())
    
    # Because GeomArea$setup_data ran, we have
    # x, y, ymin, and ymax (or flipped versions) here.
    if (flipped_aes) {
      v <- coords$y
      r_min <- coords$xmin
      r_max <- coords$xmax
      x1 <- 1
      y1 <- 0.5
      x2 <- 0
      y2 <- 0.5
    } else {
      v <- coords$x
      r_min <- coords$ymin
      r_max <- coords$ymax
      x1 <- 0.5
      y1 <- 1
      x2 = 0.5
      y2 <- 0
    }
    
    # Polygon path construction
    poly_x <- if (flipped_aes) {
      c(r_max, rev(r_min))
    } else {
      c(v, rev(v))
    }
    poly_y <- if (flipped_aes) {
      c(v, rev(v))
    } else {
      c(r_max, rev(r_min))
    }
    
    fill_color <- coords$fill[1]
    a_start <- coords$alpha[1] %||% 1
    a_end   <- alpha_fade_to %||% 0
    
    grad <- grid::linearGradient(
      colours = c(
        scales::alpha(fill_color, a_start),
        scales::alpha(fill_color, a_end)
      ),
      stops = c(0, 1),
      x1 = x1,
      y1 = y1,
      x2 = x2,
      y2 = y2
    )
    
    fill_grob <- grid::polygonGrob(
      x = poly_x,
      y = poly_y,
      default.units = "native",
      gp = grid::gpar(fill = grad, col = NA)
    )
    
    # outline logic 
    outline_color <- coords$colour
    if (all(is.na(outline_color)))
      outline_color <- coords$fill
    if (is.null(outline.type) ||
        outline.type == "none")
      return(fill_grob)
    
    gp_line <- grid::gpar(
      col = scales::alpha(outline_color, a_start),
      lwd = (coords$linewidth[1] %||% 0.5) * ggplot2::.pt,
      lty = coords$linetype[1] %||% 1
    )
    
    outline_grob <- switch(
      outline.type,
      "full" = grid::pathGrob(
        x = poly_x,
        y = poly_y,
        gp = gp_line,
        default.units = "native"
      ),
      "upper" = grid::polylineGrob(
        x = if (flipped_aes) r_max else v,
        y = if (flipped_aes) v else r_max,
        gp = gp_line,
        default.units = "native"
      ),
      "lower" = grid::polylineGrob(
        x = if (flipped_aes) r_min else v,
        y = if (flipped_aes) v else r_min,
        gp = gp_line,
        default.units = "native"
      ),
      "both" = grid::polylineGrob(
        x = if (flipped_aes) c(r_max, r_min) else c(v, v),
        y = if (flipped_aes) c(v, v) else c(r_max, r_min),
        id.lengths = c(length(v), length(v)),
        gp = gp_line,
        default.units = "native"
      ),
      ggplot2::zeroGrob()
    )
    
    grid::grobTree(fill_grob, outline_grob)
  }
)
