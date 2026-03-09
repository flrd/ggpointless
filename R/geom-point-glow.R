#' @noRd
#' @keywords internal
draw_key_point_glow <- function(data, params, size) {
  # Resolve legend glow colour
  # is.null() guard: NULL arrives when the user explicitly passes e.g.

  # glow_colour = NULL; isTRUE(is.na(.)) is safe for NULL and vectors.
  g_col <- if (is.null(params$glow_colour) || isTRUE(is.na(params$glow_colour))) {
    data$colour
  } else {
    params$glow_colour
  }
  g_alpha <- if (is.null(params$glow_alpha) || isTRUE(is.na(params$glow_alpha))) {
    a <- data$alpha %||% 1
    if (is.na(a)) a <- 1
    max(a, 0.5)
  } else {
    max(params$glow_alpha, 0.5)
  }

  # Create gradient for the legend box
  grad <- grid::radialGradient(
    colours = c(
      ggplot2::alpha(g_col, g_alpha),
      ggplot2::alpha(g_col, 0)
    )
  )

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
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPointGlow <- ggplot2::ggproto(
  "GeomPointGlow",
  ggplot2::GeomPoint,

  # Custom legend key that shows the glow
  draw_key = draw_key_point_glow,

  draw_panel = \(
    self,
    data,
    panel_params,
    coord,
    glow_alpha = 0.5,
    glow_colour = NA,
    glow_size = NA
  ) {
    coords <- coord$transform(data, panel_params)
    if (nrow(coords) == 0) {
      return(grid::nullGrob())
    }

    # If glow_colour is NA (or NULL), use the vector of colours from the data.
    # isTRUE(is.na(.)) is safe for NULL and length > 1 vectors.
    g_cols <- if (is.null(glow_colour) || isTRUE(is.na(glow_colour))) {
      coords$colour
    } else {
      glow_colour
    }

    # If glow_alpha is NA (or NULL), inherit from the point's alpha aesthetic
    g_alphas <- if (is.null(glow_alpha) || isTRUE(is.na(glow_alpha))) {
      a <- coords$alpha
      a[is.na(a)] <- 1
      a
    } else {
      glow_alpha
    }

    # If glow_size is NA (or NULL), multiply the point sizes by 3
    g_sizes <- if (is.null(glow_size) || isTRUE(is.na(glow_size))) {
      coords$size * 3
    } else {
      glow_size
    }

    # build the Glow Grobs
    glow_grobs <- lapply(seq_len(nrow(coords)), function(i) {
      # Handle potentially vectorized colours/sizes/alphas
      current_col <- if (length(g_cols) > 1) {
        g_cols[i]
      } else {
        g_cols
      }
      current_size <- if (length(g_sizes) > 1) {
        g_sizes[i]
      } else {
        g_sizes
      }
      current_alpha <- if (length(g_alphas) > 1) {
        g_alphas[i]
      } else {
        g_alphas
      }

      grad <- grid::radialGradient(
        colours = c(
          ggplot2::alpha(current_col, current_alpha),
          ggplot2::alpha(current_col, 0)
        )
      )

      grid::pointsGrob(
        x = coords$x[i],
        y = coords$y[i],
        pch = 21,
        gp = ggplot2::gg_par(
          col = NA,
          fill = grad,
          pointsize = current_size * 3
        )
      )
    })

    # create the standard points layer
    points <- ggplot2::GeomPoint$draw_panel(data, panel_params, coord)

    # combine glow (bottom) and points (top)
    glow_layer <- grid::gTree(children = do.call(grid::gList, glow_grobs))
    grid::gList(glow_layer, points)
  }
)

#' Points that Glow
#'
#' @description
#' geom_point_glow is a version of ([`geom_point()`][ggplot2::geom_point()])
#' that adds a glow (radial gradient) behind each point.
#'
#' @inheritParams ggplot2::geom_point
#' @param glow_alpha Transparency of the glow between 0 (fully transparent)
#'   and 1 (fully opaque). Defaults to `0.5`.
#' @param glow_colour colour of the glow. If `NA` (default), it inherits the
#'   colour of the point itself.
#' @param glow_size Numerical value for the glow radius. If `NA` (default),
#'   it is calculated as three times the point size.
#'
#' @aesthetics GeomPointGlow
#'
#' @seealso
#'    [ggplot2::geom_point()], [grid::radialGradient()]
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @references
#' Murrell, P. (2022). "Vectorised Pattern Fills in R Graphics." Technical
#' Report 2022-01, Department of Statistics, The University of Auckland.
#' Version 1.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/vecpat/vecpat.html}
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
#'   geom_point_glow(glow_colour = "#333", glow_alpha = 0.25, glow_size = 5) +
#'   theme_minimal()
#'
#' # use the Geom with another Stat
#' ggplot(head(economics), aes(date, uempmed)) +
#'   geom_line() +
#'   stat_pointless(
#'     geom = "PointGlow",
#'     glow_colour = "tomato",
#'     glow_size = 10,
#'     location = c("first", "last")
#' )
geom_point_glow <- make_constructor(GeomPointGlow)
