# Deferred grob for device-aware rectangle rendering.
#
# Two tiers (rectangles never need the compositing path — each rect has a
# single fill colour):
#   Tier 1 — linearGradient fill (ragg, cairo, svg, png, ...)
#   Tier 2 — flat semi-transparent (base pdf(), postscript)
#' @noRd
#' @keywords internal
.rect_fade_grob <- function(gradient_glist, flat_glist) {
  grid::gTree(
    gradient_glist = gradient_glist,
    flat_glist = flat_glist,
    cl = "rect_fade_grob"
  )
}

#' @export
makeContent.rect_fade_grob <- function(x) {
  dev_name <- names(grDevices::dev.cur())
  no_gradient <- dev_name %in% c("pdf", "postscript")

  if (no_gradient) {
    cli::cli_inform(
      c(
        "!" = "The current graphics device does not support gradient fills.",
        "i" = "Falling back to a flat semi-transparent fill. Switch to a \\
               device that supports gradients (e.g. {.code ragg::agg_png()}, \\
               {.code svg()}) for the full effect."
      ),
      .frequency = "once",
      .frequency_id = "rect_fade_no_gradient"
    )
    grobs <- x$flat_glist
  } else {
    grobs <- x$gradient_glist
  }

  # Clamp corner radius so it never exceeds half the rect's smaller dimension.
  # Without this, tiny rects degenerate into pill shapes when radius > rect.
  for (i in seq_along(grobs)) {
    g <- grobs[[i]]
    if (!inherits(g, "roundrect")) {
      next
    }
    r_pt <- grid::convertUnit(g$r, "pt", valueOnly = TRUE)
    h_pt <- abs(grid::convertHeight(g$height, "pt", valueOnly = TRUE))
    w_pt <- abs(grid::convertWidth(g$width, "pt", valueOnly = TRUE))
    max_r <- min(h_pt, w_pt) / 2
    if (r_pt > max_r) {
      grobs[[i]]$r <- grid::unit(max_r, "pt")
    }
  }

  grid::setChildren(x, grobs)
}

# Legend key — rounded rect with alpha gradient (vertical or horizontal).
#' @noRd
#' @keywords internal
.draw_key_rect_fade <- function(data, params, size) {
  radius <- params$radius %||% grid::unit(0, "pt")
  if (!grid::is.unit(radius)) {
    radius <- grid::unit(radius, "pt")
  }

  fill_colour <- data$fill %||% "grey35"
  a_start <- data$alpha %||% 1
  a_end <- params$alpha_fade_to %||% 0
  fade_direction <- params$fade_direction %||% "vertical"

  if (identical(fade_direction, "horizontal")) {
    # Left (opaque) → right (transparent)
    grad <- grid::linearGradient(
      colours = c(
        ggplot2::alpha(fill_colour, a_start),
        ggplot2::alpha(fill_colour, a_end)
      ),
      x1 = 0,
      y1 = 0.5,
      x2 = 1,
      y2 = 0.5
    )
  } else {
    # Bottom (transparent) → top (opaque)
    grad <- grid::linearGradient(
      colours = c(
        ggplot2::alpha(fill_colour, a_end),
        ggplot2::alpha(fill_colour, a_start)
      ),
      x1 = 0.5,
      y1 = 0,
      x2 = 0.5,
      y2 = 1
    )
  }

  grid::roundrectGrob(
    r = radius,
    gp = ggplot2::gg_par(
      fill = grad,
      col = data$colour %||% NA
    )
  )
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRectFade <- ggplot2::ggproto(
  "GeomRectFade",
  ggplot2::GeomRect,

  extra_params = c(
    ggplot2::GeomRect$extra_params,
    "alpha_fade_to",
    "fade_direction",
    "radius"
  ),

  draw_key = .draw_key_rect_fade,

  setup_params = \(self, data, params) {
    params <- ggplot2::ggproto_parent(ggplot2::GeomRect, self)$setup_params(
      data,
      params
    )

    params$alpha_fade_to <- params$alpha_fade_to %||% 0

    .check_alpha_fade_to(params$alpha_fade_to)

    params$fade_direction <- rlang::arg_match0(
      params$fade_direction %||% "vertical",
      values = c("vertical", "horizontal")
    )

    if (!is.null(params$radius) && !grid::is.unit(params$radius)) {
      params$radius <- grid::unit(params$radius, "pt")
    }

    params
  },

  draw_panel = \(
    self,
    data,
    panel_params,
    coord,
    lineend = "butt",
    linejoin = "round",
    alpha_fade_to = 0,
    fade_direction = "vertical",
    radius = NULL
  ) {
    radius <- radius %||% grid::unit(0, "pt")
    if (!grid::is.unit(radius)) {
      radius <- grid::unit(radius, "pt")
    }

    if (!coord$is_linear()) {
      cli::cli_inform(
        c(
          "!" = "{.fn geom_rect_fade}: rounded corners require a linear \\
                 coordinate system.",
          "i" = "Falling back to {.fn geom_rect} rendering (no rounding, \\
                 no gradient)."
        ),
        .frequency = "once",
        .frequency_id = "rect_fade_nonlinear"
      )
      return(
        ggplot2::ggproto_parent(ggplot2::GeomRect, self)$draw_panel(
          data,
          panel_params,
          coord,
          lineend = lineend,
          linejoin = linejoin
        )
      )
    }

    if (nrow(data) == 0L) {
      return(ggplot2::zeroGrob())
    }

    coords <- coord$transform(data, panel_params)
    n <- nrow(coords)

    gradient_list <- vector("list", n)
    flat_list <- vector("list", n)

    for (i in seq_len(n)) {
      a_start <- coords$alpha[i]
      if (is.na(a_start)) {
        a_start <- 1
      }

      fill_col <- coords$fill[i]

      # Gradient direction:
      #   "vertical"   — bottom (transparent) → top (opaque)
      #   "horizontal" — left (opaque) → right (transparent)
      if (identical(fade_direction, "horizontal")) {
        grad <- grid::linearGradient(
          colours = c(
            ggplot2::alpha(fill_col, a_start),
            ggplot2::alpha(fill_col, alpha_fade_to)
          ),
          x1 = 0,
          y1 = 0.5,
          x2 = 1,
          y2 = 0.5
        )
      } else {
        grad <- grid::linearGradient(
          colours = c(
            ggplot2::alpha(fill_col, alpha_fade_to),
            ggplot2::alpha(fill_col, a_start)
          ),
          x1 = 0.5,
          y1 = 0,
          x2 = 0.5,
          y2 = 1
        )
      }

      mid_alpha <- (a_start + alpha_fade_to) / 2
      flat_fill <- ggplot2::alpha(fill_col, mid_alpha)

      x_pos <- grid::unit(coords$xmin[i], "native")
      y_pos <- grid::unit(coords$ymax[i], "native")
      w <- grid::unit(coords$xmax[i] - coords$xmin[i], "native")
      h <- grid::unit(coords$ymax[i] - coords$ymin[i], "native")

      gradient_list[[i]] <- grid::roundrectGrob(
        x = x_pos,
        y = y_pos,
        width = w,
        height = h,
        just = c("left", "top"),
        r = radius,
        gp = ggplot2::gg_par(
          col = coords$colour[i],
          fill = grad,
          lwd = coords$linewidth[i],
          lty = coords$linetype[i],
          linejoin = linejoin,
          lineend = lineend
        )
      )

      flat_list[[i]] <- grid::roundrectGrob(
        x = x_pos,
        y = y_pos,
        width = w,
        height = h,
        just = c("left", "top"),
        r = radius,
        gp = ggplot2::gg_par(
          col = coords$colour[i],
          fill = flat_fill,
          lwd = coords$linewidth[i],
          lty = coords$linetype[i],
          linejoin = linejoin,
          lineend = lineend
        )
      )
    }

    .rect_fade_grob(
      do.call(grid::gList, gradient_list),
      do.call(grid::gList, flat_list)
    )
  }
)

#' Rectangles with a Fading Gradient and Rounded Corners
#'
#' @description
#' `geom_rect_fade()` draws axis-aligned rectangles like [ggplot2::geom_rect()]
#' but fills each one with a linear gradient that fades one edge to transparent.
#' The direction is controlled by `fade_direction`. Corners can be rounded via
#' the `radius` argument, enabling rounded rectangles and smooth-cornered visual
#' elements. The default of `0 pt` produces plain rectangles.
#'
#' @concept rounded rectangles
#' @concept rounded corners
#' @concept fading gradient
#' @concept rounded bar charts
#'
#' @aesthetics GeomRectFade
#'
#' @inheritParams ggplot2::geom_rect
#' @param alpha_fade_to A single finite number between 0 and 1. The alpha
#'   value at the fading edge of each rectangle. Defaults to `0`
#'   (fully transparent).
#' @param fade_direction Direction of the alpha gradient. One of:
#'   \describe{
#'     \item{`"vertical"`}{(default) Top edge is opaque (`ymax`), bottom edge
#'       fades to `alpha_fade_to` (`ymin`).}
#'     \item{`"horizontal"`}{Left edge is opaque (`xmin`), right edge fades to
#'       `alpha_fade_to` (`xmax`).}
#'   }
#' @param radius Corner radius passed to [grid::roundrectGrob()]. A
#'   [grid::unit()] object (e.g. `unit(4, "pt")`); a bare number is
#'   interpreted as points. Defaults to `unit(0, "pt")` (sharp corners).
#' @param stat Use to override the default connection between
#'   `geom_rect_fade()` and `stat_identity()`.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @seealso [ggplot2::geom_rect()] for plain rectangles,
#'   [geom_col_fade()] for bar charts with per-bar gradient scaling and
#'   orientation support.
#'
#' @references
#' Murrell, P. (2022). "Vectorised Pattern Fills in R Graphics." Technical
#' Report 2022-01, Department of Statistics, The University of Auckland.
#' Version 1.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/vecpat/vecpat.html}
#'
#' @export
#' @examples
#'
#' # With geom_rect_fade() you can draw arbitrary rectangles
#' # example taken from help(geom_rect)
#' df <- data.frame(
#'   x = rep(c(2, 5, 7, 9, 12), 2),
#'   y = rep(c(1, 2), each = 5),
#'   z = factor(rep(1:5, each = 2)),
#'   w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
#'   )
#'
#' ggplot(df, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1, width = 10)) +
#'   geom_rect_fade(aes(fill = z), colour = "grey50", radius = 15)
#'
#' # Next example taken from ggplot2 book, see:
#' # https://ggplot2-book.org/annotations.html#sec-custom-annotations
#'
#' library(ggplot2)
#' presidential <- subset(presidential, start > economics$date[1])
#'
#' ggplot(economics) +
#'   geom_rect_fade(
#'     aes(xmin = start, xmax = end, fill = party),
#'     ymin = -Inf, ymax = Inf, alpha = 0.2,
#'     data = presidential
#'   ) +
#'   geom_vline_fade(
#'     aes(xintercept = as.numeric(start)),
#'     data = presidential,
#'     colour = "grey50", alpha = 0.5,
#'     fade_direction = "start"
#'   ) +
#'   geom_text(
#'     aes(x = start, y = 2500, label = name),
#'     data = presidential,
#'     size = 3, vjust = 0, hjust = 0, nudge_x = 50
#'   ) +
#'   geom_line(aes(date, unemploy)) +
#'   scale_fill_manual(values = c("blue", "red")) +
#'   xlab("date") +
#'   ylab("unemployment") +
#'   theme_minimal() +
#'   theme(panel.grid = element_blank())
#'
geom_rect_fade <- make_constructor(
  GeomRectFade,
  stat = "identity",
  position = "identity",
  alpha_fade_to = 0,
  fade_direction = "vertical",
  radius = grid::unit(0, "pt")
)
