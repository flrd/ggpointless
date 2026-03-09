#' @noRd
#' @keywords internal
draw_key_sabline <- function(data, params, size) {
  grid::segmentsGrob(
    x0 = 0.25,
    y0 = 0.25,
    x1 = 0.75,
    y1 = 0.75,
    gp = ggplot2::gg_par(
      col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
      lwd = data$linewidth %||% 0.5,
      lty = data$linetype %||% 1,
      lineend = "round"
    )
  )
}

#' @noRd
#' @keywords internal
draw_key_pointless <- function(data, params, size) {
  if (is.null(data$shape)) {
    data$shape <- 19
  } else if (is.character(data$shape)) {
    data$shape <- ggplot2::translate_shape_string(data$shape)
  }

  grid::pointsGrob(
    x = 0.75,
    y = 0.75,
    pch = data$shape,
    gp = ggplot2::gg_par(
      col = alpha(data$colour %||% "black", data$alpha),
      fill = alpha(data$fill %||% "black", data$alpha),
      pointsize = data$size %||% 1.5,
      stroke = data$stroke %||% 0.5
    )
  )
}

#' @title Key glyphs for legends
#'
#' @description
#' Each geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These functions are called `draw_key_*()`, where
#' `*` stands for the name of the respective key glyph. The key glyphs can be
#' customized for individual geoms by providing a geom with the `key_glyph`
#' argument (see [`layer()`] or examples below.)
#'
#' @return A grid grob.
#' @inheritParams ggplot2::draw_key
#'
#' @examples
#' ggplot(economics_long, aes(date, value01, colour = variable)) +
#'   geom_line(key_glyph = "lexis")
#'
#' @export
#' @keywords internal
draw_key_lexis <- function(data, params, size) {
  # is.null guard: key glyph may be borrowed by geom_*s without point_show
  if (isTRUE(params$point_show) || is.null(params$point_show)) {
    pts_data <- data
    pts_data$size <- (data$size %||% 2) * 0.65
    grid::grobTree(
      draw_key_sabline(data, params, size),
      draw_key_pointless(pts_data, params, size)
    )
  } else {
    draw_key_sabline(data, params, size)
  }
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLexis <- ggproto(
  "GeomLexis",
  Geom,
  required_aes = c("x", "y", "xend", "yend"),
  non_missing_aes = c("size", "shape", "point_colour", "type"),
  extra_params = c(
    "na.rm",
    "point_show",
    "point_colour",
    "gap_filler",
    "lineend",
    "linejoin"
  ),
  default_aes = aes(
    shape = 19,
    colour = "black",
    linetype = "solid",
    linewidth = 0.5,
    size = 1.5,
    fill = NA,
    alpha = NA,
    stroke = 0.5
  ),

  draw_group = \(
    data,
    panel_params,
    coord,
    lineend = "round",
    linejoin = "mitre",
    gap_filler = TRUE,
    point_show = TRUE,
    point_colour = NULL
  ) {
    if (!is.logical(gap_filler)) {
      cli::cli_abort(
        "{.arg gap_filler} must be a logical value, not {.cls {class(gap_filler)}}."
      )
    }

    if (!is.logical(point_show)) {
      cli::cli_abort(
        "{.arg point_show} must be a logical value, not {.cls {class(point_show)}}."
      )
    }

    points <- tail(data, 1)
    points$colour <- point_colour %||% points$colour
    points$x <- points$xend
    points$y <- points$yend
    points <- points[, !names(points) %in% c("xend", "yend"), drop = FALSE]

    if (!isTRUE(gap_filler)) {
      data <- data[data$type != "dotted", , drop = FALSE]
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

#' @title Lexis diagrams
#'
#' @description
#' This geom can be used to plot 45° lifelines for a cohort.
#' Lexis diagrams are named after Wilhelm Lexis and used by demographers
#' for more than a century.
#'
#' @aesthetics GeomLexis
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param lineend line end style (round, butt, square)
#' @param linejoin line join style (round, mitre, bevel)
#' @param point_colour colour of the endpoint point. If `NULL` (default), the
#'   group colour is used.
#' @param point_show logical. Should a point be shown at the end of each
#'   segment? `TRUE` by default.
#' @param gap_filler logical. Should horizontal gap-filler segments be drawn?
#'   `TRUE` by default.
#'
#' @details
#' This geom draws 45° lines from the start to the end of a 'lifetime'. It is
#' a combination of a segment, and a point.
#' Besides `y` and `yend` coordinates this geom creates one additional variable
#' called `type` in the layer data. You might want to map to an aesthetic with
#' [ggplot2::after_stat()], see Examples section and `vignette("ggpointless")`
#' for more details.
#'
#' Rows in your data with either missing `x` or `xend` values will be removed
#' because your segments must start and end somewhere.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
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
#'   size = 3,
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
geom_lexis <- make_constructor(
  GeomLexis,
  stat = "lexis",
  point_show = TRUE,
  point_colour = NULL,
  gap_filler = TRUE,
  lineend = "round",
  linejoin = "round"
)
