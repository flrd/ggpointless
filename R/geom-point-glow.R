#' @noRd
#' @keywords internal
draw_key_point_glow <- function(data, params, size) {
  # Resolve legend glow colour
  # is.null() guard: NULL arrives when the user explicitly passes e.g.

  # glow_colour = NULL; isTRUE(is.na(.)) is safe for NULL and vectors.
  g_col <- if (
    is.null(params$glow_colour) || isTRUE(is.na(params$glow_colour))
  ) {
    data$colour
  } else {
    params$glow_colour
  }
  g_alpha <- if (
    is.null(params$glow_alpha) || isTRUE(is.na(params$glow_alpha))
  ) {
    a <- data$alpha %||% 1
    if (is.na(a)) {
      a <- 1
    }
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

  extra_params = c(
    ggplot2::GeomPoint$extra_params,
    "glow_alpha",
    "glow_colour",
    "glow_size"
  ),

  setup_params = function(self, data, params) {
    params <- ggplot2::ggproto_parent(
      ggplot2::GeomPoint,
      self
    )$setup_params(data, params)
    n <- nrow(data)
    params$glow_alpha  <- .check_glow_alpha(params$glow_alpha %||% 0.5, n = n)
    params$glow_size   <- .check_glow_size(params$glow_size %||% NA, n = n)
    params$glow_colour <- .check_glow_colour(params$glow_colour %||% NA, n = n)
    params
  },

  # Stamp vector-length glow_* params onto `data` as `.glow_*` columns so
  # ggplot2's NA-row filter (handle_na, which runs after setup_data) re-aligns
  # them alongside the surviving points. Without this, a vector glow_size on
  # data with NAs silently misaligns in draw_panel.
  # Scalars stay in params and are picked up by draw_panel's fallback paths.
  setup_data = function(self, data, params) {
    data <- ggplot2::ggproto_parent(
      ggplot2::GeomPoint,
      self
    )$setup_data(data, params)
    if (is.numeric(params$glow_size) && length(params$glow_size) > 1L) {
      data$.glow_size <- params$glow_size
    }
    if (is.numeric(params$glow_alpha) && length(params$glow_alpha) > 1L) {
      data$.glow_alpha <- params$glow_alpha
    }
    if (is.character(params$glow_colour) && length(params$glow_colour) > 1L) {
      data$.glow_colour <- params$glow_colour
    }
    data
  },

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

    # Priority for each glow_* value:
    #   1. `.glow_*` column on coords (stamped by setup_data for vector-length
    #      params -- already NA-filter aligned by ggplot2's handle_na).
    #   2. scalar param from draw_panel formals.
    #   3. fallback: inherit from the point's matching aesthetic.
    # isTRUE(is.na(.)) is safe for NULL and length > 1 vectors.
    g_cols <- if (!is.null(coords$.glow_colour)) {
      coords$.glow_colour
    } else if (is.null(glow_colour) || isTRUE(is.na(glow_colour))) {
      coords$colour
    } else {
      glow_colour
    }

    g_alphas <- if (!is.null(coords$.glow_alpha)) {
      coords$.glow_alpha
    } else if (is.null(glow_alpha) || isTRUE(is.na(glow_alpha))) {
      a <- coords$alpha
      a[is.na(a)] <- 1
      a
    } else {
      glow_alpha
    }

    # Default glow: 9x the point's size aesthetic.
    # User-supplied glow_size: taken at face value, in ggplot2 size units
    # (same semantics as `size` in `geom_point()`).
    # gg_par below handles the ggplot2 size -> grid point conversion.
    g_sizes <- if (!is.null(coords$.glow_size)) {
      coords$.glow_size
    } else if (is.null(glow_size) || isTRUE(is.na(glow_size))) {
      coords$size * 9
    } else {
      glow_size
    }

    # Inform once per session when any glow would be hidden under its own point
    # (glow_size <= size means the point grob fully covers the halo, since the
    # base point is drawn on top of the glow layer).  Throttled to "once" so
    # scripted batches don't spam the console.
    if (any(g_sizes <= coords$size, na.rm = TRUE)) {
      cli::cli_inform(
        c(
          "!" = "{.arg glow_size} is smaller than or equal to the point's \\
                 {.arg size} for at least one point; the glow halo will be \\
                 covered by the point itself.",
          "i" = "Use a larger {.arg glow_size} (the default is 9x {.arg size}) \\
                 or a smaller {.arg size} to make the halo visible."
        ),
        .frequency = "once",
        .frequency_id = "geom_point_glow_size_covered"
      )
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
          pointsize = current_size
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
#' `geom_point_glow()` is a version of [ggplot2::geom_point()]
#' that adds a glow (radial gradient) behind each point.
#'
#' @concept glowing points
#' @concept radial gradient
#'
#' @inheritParams ggplot2::geom_point
#' @param glow_alpha Transparency of the glow between 0 (fully transparent)
#'   and 1 (fully opaque). Defaults to `0.5`. Either a scalar or a numeric
#'   vector whose length matches the number of points.
#' @param glow_colour Colour of the glow. If `NA` (default), it inherits the
#'   colour of the point itself. Either a scalar colour or a character
#'   vector whose length matches the number of points.
#' @param glow_size Glow radius in ggplot2 size units (same scale as the
#'   `size` aesthetic in [ggplot2::geom_point()]). If `NA` (default), the
#'   glow is rendered at nine times the point's `size`. Either a scalar
#'   or a numeric vector whose length matches the number of points.
#'
#'   For the halo to be visible, `glow_size` must exceed the point's
#'   `size` -- otherwise the point grob (drawn on top) fully covers the
#'   glow. If this happens the geom emits a one-shot informational
#'   message at draw time pointing you at the fix (enlarge the glow or
#'   shrink the point). See *Examples*.
#'
#' @section Coordinate systems:
#' `geom_point_glow()` works in all coordinate systems. The glow effect
#' remains point-centric and circular in device space, even in non-linear
#' coordinates like [ggplot2::coord_polar()].
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
#' # Tiny dataset on purpose: each glow point becomes its own
#' # `grid::radialGradient` pattern, and the example runner accumulates
#' # patterns from every package example into a single pdf() device.  On
#' # large pdf runs that pattern cache can trigger an upstream R bug at
#' # `dev.off()` -- unrelated to your real plots.
#' df <- head(mtcars, 5)
#'
#' # Basic usage -- the default glow is 9x the point's `size` aesthetic,
#' # so it's always visibly larger than the point itself.
#' ggplot(df, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point_glow()
#'
#' \donttest{
#'   # Customising the glow (fixed values applied to every point)
#'   ggplot(df, aes(wt, mpg, colour = factor(cyl))) +
#'     geom_point_glow(glow_colour = "#333", glow_alpha = 0.25, glow_size = 5) +
#'     theme_minimal()
#'
#'   # Per-point glow: pass a length-N vector for `glow_colour`, `glow_alpha`,
#'   # or `glow_size`.
#'   ggplot(df, aes(wt, mpg)) +
#'     geom_point_glow(glow_colour = rainbow(nrow(df)), glow_size = 5)
#'
#'   # Use the Geom with another Stat to glow only specific observations:
#'   ggplot(head(economics), aes(date, uempmed)) +
#'     geom_line() +
#'     stat_pointless(
#'       geom = "PointGlow",
#'       glow_colour = "tomato",
#'       glow_size = 10,
#'       location = c("first", "last")
#'     )
#' }
geom_point_glow <- make_constructor(GeomPointGlow)
