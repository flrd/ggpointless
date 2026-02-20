#' Points that Glow
#'
#' @description
#' geom_point_glow is a version of ([`geom_point()`][ggplot2::geom_point()])
#' that adds a glow (radial gradient) behind each point.
#'
#' @inheritParams ggplot2::geom_point
#' @param glow_colour colour of the glow. If NA (default), it inherits the
#'   colour of the point itself.
#' @param glow_alpha Transparency of the glow. Defaults to 0.5.
#' @param glow_size Numerical value for the glow radius. If NA (default),
#'   it is calculated as two times the point size.
#'
#' @section Aesthetic Inheritance:
#' Since `glow_colour` and `glow_size` default to NA, they dynamically
#' respond to the standard colour and size aesthetics.
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' # Basic usage
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point_glow()
#'
#' # Customizing glow parameters (fixed for all points)
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point_glow(glow_colour = "#333", glow_alpha = 0..25, glow_size = 5) +
#'   theme_minimal()
#'   
#' # use the Geom with another Stat
#' ggplot(head(economics), aes(date, uempmed)) +
#'   geom_line() +
#'   stat_pointless(
#'     geom = "PointGlow",
#'     glow_colour = "red",
#'     glow_size = 10,
#'     location = c("first", "last")
#' )
geom_point_glow <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            ...,
                            glow_colour = NA,
                            glow_alpha = 0.5,
                            glow_size = NA,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  if (getRversion() < "4.1.0") {
    stop("geom_point_glow requires R version 4.1.0 or higher for radial gradients.",
         call. = FALSE)
  }
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointGlow,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      glow_colour = glow_colour,
      glow_alpha = glow_alpha,
      glow_size = glow_size,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_point_glow
#' @format NULL
#' @usage NULL
#' @export
GeomPointGlow <- ggplot2::ggproto(
  "GeomPointGlow",
  ggplot2::GeomPoint,
  
  # Custom legend key that shows the glow
  draw_key = function(data, params, size) {
    # Resolve legend glow colour
    g_col <- if (is.na(params$glow_colour))
      data$colour
    else
      params$glow_colour
    g_alpha <- params$glow_alpha
    
    # Create gradient for the legend box
    grad <- grid::radialGradient(colours = c(ggplot2::alpha(g_col, g_alpha), ggplot2::alpha(g_col, 0)))
    
    grid::gList(
      # The glow (using npc units to stay within the key box)
      grid::circleGrob(
        0.5,
        0.5,
        r = grid::unit(0.35, "npc"),
        gp = grid::gpar(fill = grad, col = NA)
      ),
      # standard Point Core
      ggplot2::draw_key_point(data, params, size)
    )
  },
  
  draw_panel = function(self,
                        data,
                        panel_params,
                        coord,
                        glow_colour = NA,
                        glow_alpha = 0.5,
                        glow_size = NA) {
    coords <- coord$transform(data, panel_params)
    if (nrow(coords) == 0)
      return(grid::nullGrob())
    
    # If glow_colour is NA, use the vector of colours from the data
    g_cols <- if (is.na(glow_colour))
      coords$colour
    else
      glow_colour
    
    # If glow_size is NA, multiply the point sizes by 2
    g_sizes <- if (is.na(glow_size))
      coords$size * 2
    else
      glow_size
    
    # build the Glow Grobs
    glow_grobs <- lapply(seq_len(nrow(coords)), function(i) {
      # Handle potentially vectorized colours/sizes
      current_col <- if (length(g_cols) > 1)
        g_cols[i]
      else
        g_cols
      current_size <- if (length(g_sizes) > 1)
        g_sizes[i]
      else
        g_sizes
      
      grad <- grid::radialGradient(colours = c(
        ggplot2::alpha(current_col, glow_alpha),
        ggplot2::alpha(current_col, 0)
      ))
      
      grid::circleGrob(
        x = coords$x[i],
        y = coords$y[i],
        r = grid::unit(current_size * ggplot2::.pt / 72, "inches"),
        gp = grid::gpar(col = NA, fill = grad)
      )
    })
    
    # create the standard points layer
    points <- ggplot2::GeomPoint$draw_panel(data, panel_params, coord)
    
    # combine glow (bottom) and points (top)
    glow_layer <- grid::gTree(children = do.call(grid::gList, glow_grobs))
    grid::gList(glow_layer, points)
  }
)
