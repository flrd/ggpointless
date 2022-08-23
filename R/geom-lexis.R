#' Display events of different cohorts in form of a lexis charts
#'
#' @description
#' This geom can be used to plot 45° lifelines for a cohort.
#' Lexis diagrams are used by demographers for more than a century
#' and they are named after Wilhelm Lexis. They are a combination
#' of a segment, and a dot.
#'
#' @section Aesthetics:
#' geom_lexis() understands the following aesthetics (required
#' aesthetics are in bold):
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
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param point_show logical. Should a point be shown at the end
#' of each segment? TRUE by default
#' @param lineend line end style (round, butt, square)
#' @param linejoin line join style (round, mitre, bevel)
#' @param point_size the size of a point
#' @param point_colour color of a point
#' @param gap_filler logical. Should gaps be filled?
#' TRUE by default
#'
#' @details
#' This geom draws 45° lines from the start to the end of a 'lifetime'. Besides
#' `y` and `yend` coordinates this geom creates one additional variable called
#' `type` in the layer data. You might want to map to an aesthetic with
#' [ggplot2::after_stat()], see Examples section and `vignette("ggpointless")`
#' for more details.
#'
#' Rows in your data with either missing `x` or `xend` values will be removed
#' (your segments must start and end somewhere).
#'
#' @export
#' @examples
#' df1 <- data.frame(
#'   key = c("A", "B", "B", "C", "D", "E"),
#'   start = c(0, 1, 6, 5, 6, 9),
#'   end = c(5, 4, 10, 9, 8, 11)
#' )
#' p <- ggplot(df1, aes(x = start, xend = end, color = key))
#' p +
#'   geom_lexis()
#' p +
#'   geom_lexis(gap_filler = FALSE)
#' p +
#'   geom_lexis(aes(linetype = after_stat(type)),
#'     point_show = FALSE
#'   )
#'
#' # change point appearance
#' p + geom_lexis(
#'   point_colour = "black",
#'   point_size = 3,
#'   shape = 21,
#'   fill = "white",
#'   stroke = 1
#' )
#'
#' # missing values will be removed
#' df2 <- data.frame(
#'   key = c("A", "B", "B", "C", "D"),
#'   start = c(0, 1, 7, 5, 6),
#'   end = c(5, 4, 13, 9, NA)
#' )
#' ggplot(df2, aes(x = start, xend = end, color = key)) +
#'   geom_lexis()
#'
#' # Ideally, `x` values should be increasing, unlike
#' # in the next example
#' df3 <- data.frame(x = Sys.Date() - 0:2, xend = Sys.Date() + 1:3)
#' ggplot(df3, aes(x = x, xend = xend)) +
#'   geom_lexis()
#'
#' # If `x` is of class Date, `xend` can't be of class `POSIXt` or
#' # `POSIXct`. The error is thrown by the `scales::date_trans` function.
#' \dontrun{
#' ggplot(
#'   data.frame(x = Sys.Date(), xend = Sys.time()),
#'   aes(x = x, xend = xend)
#' ) +
#'   geom_lexis()
#' }
#'
geom_lexis <- function(mapping = NULL,
                       data = NULL,
                       ...,
                       point_show = TRUE,
                       point_colour = NULL,
                       point_size = NULL,
                       gap_filler = TRUE,
                       lineend = "round",
                       linejoin = "round",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = "lexis",
    geom = GeomLexis,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      gap_filler = gap_filler,
      point_show = point_show,
      point_colour = point_colour,
      point_size = point_size,
      na.rm = na.rm,
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
  required_aes = c("x", "y", "xend", "yend"),
  non_missing_aes = c("size", "shape", "point_colour", "point_size", "type"),
  default_aes = aes(
    shape = 19, colour = "black", linetype = "solid", size = 0.3, fill = NA,
    alpha = NA, stroke = 0.5
  ),
  draw_group = function(data, panel_params, coord,
                        lineend = "round", linejoin = "mitre",
                        gap_filler = TRUE,
                        point_show = TRUE,
                        point_colour = NULL,
                        point_size = NULL) {
    if (!is.logical(gap_filler)) {
      stop("'gap_filler' must be a logical value.")
    }

    if (!is.logical(point_show)) {
      stop("'point_show' must be a logical value.")
    }

    points <- tail(data, 1)
    points$colour <- point_colour %||% points$colour
    points$size <- point_size %||% (points$size * 3)
    points <- transform(points, x = xend, y = yend)
    points <- subset(points, select = c(-xend, -yend))

    if (!isTRUE(gap_filler)) {
      data <- subset(data, type != "dotted")
    }

    if (isTRUE(point_show)) {
      grid::gList(
        ggplot2::GeomSegment$draw_panel(
          data = data,
          panel_params = panel_params,
          coord = coord,
          lineend = lineend,
          linejoin = linejoin
        ),
        ggplot2::GeomPoint$draw_panel(
          data = points,
          panel_params = panel_params,
          coord = coord
        )
      )
    } else {
      ggplot2::GeomSegment$draw_panel(
        data = data,
        panel_params = panel_params,
        coord = coord,
        lineend = lineend,
        linejoin = linejoin
      )
    }
  },
  draw_key = draw_key_lexis
)
