#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @include geom-area-fade.R
#' @export
GeomRidgelineFade <- ggplot2::ggproto(
  "GeomRidgelineFade",
  ggplot2::GeomRibbon,

  required_aes = c("x", "y", "height"),

  extra_params = c("na.rm", "flipped_aes", "alpha_fade_to", "alpha_scope"),

  draw_key = .draw_key_area_fade,

  setup_params = \(self, data, params) {
    params$flipped_aes <- params$flipped_aes %||% FALSE
    params$alpha_fade_to <- params$alpha_fade_to %||% 0
    params$alpha_scope <- params$alpha_scope %||% "group"

    .check_alpha_fade_to(params$alpha_fade_to)

    params$alpha_scope <- rlang::arg_match0(
      params$alpha_scope,
      values = c("area", "group", "global")
    )

    if (!is.null(params$outline.type)) {
      valid_outline <- c("upper", "lower", "both", "full", "none")
      if (
        !rlang::is_string(params$outline.type) ||
          !params$outline.type %in% valid_outline
      ) {
        cli::cli_abort(
          c(
            "{.arg outline.type} must be one of {.or {.val {valid_outline}}}.",
            "x" = "Got {.val {params$outline.type}} instead."
          )
        )
      }
    }

    params
  },

  # ymin / ymax are computed by PositionRidgeline; only stamp alpha_scope here.
  setup_data = \(self, data, params) {
    data$.alpha_scope <- params$alpha_scope %||% "group"
    data
  },

  # Override draw_panel to sort groups back-to-front and stamp group heights.
  draw_panel = \(
    self,
    data,
    panel_params,
    coord,
    flipped_aes = FALSE,
    alpha_fade_to = 0,
    outline.type = "upper",
    ...
  ) {
    # Split by (group, y) rather than group alone.  If the user maps `group`
    # to something coarser than the y variable (e.g. aes(group = fill) while
    # y has multiple levels), each (group, y) pair becomes its own ridge
    # instead of one ribbon that spans several baselines.  In the normal case
    # where group already uniquely identifies y the result is identical.
    groups <- split(data, interaction(data$group, data$y, drop = TRUE))

    # Draw highest-baseline groups first (back) so lower ones overlap on top.
    o <- order(
      vapply(groups, \(d) d$ymin[1L], numeric(1)),
      decreasing = TRUE
    )
    groups <- groups[o]

    alpha_scope <- data$.alpha_scope[1L] %||% "group"

    # PositionRidgeline has run, so heights are the positioned excursion.
    heights <- abs(data$ymax - data$ymin)
    heights[is.na(heights)] <- 0

    if (identical(alpha_scope, "global")) {
      global_max_height <- max(heights, na.rm = TRUE)
      if (!is.finite(global_max_height) || global_max_height == 0) {
        global_max_height <- 1
      }
      groups <- lapply(groups, \(d) {
        d$global_max_height <- global_max_height
        d
      })
    } else if (identical(alpha_scope, "group")) {
      # "group" scope: scale each ridge relative to the tallest ridge at
      # the same y-baseline (panel-local).
      y_max <- tapply(heights, data$y, max, na.rm = TRUE)
      groups <- lapply(groups, \(d) {
        key <- as.character(d$y[1L])
        val <- y_max[key]
        if (is.finite(val) && val > 0) {
          d$group_max_height <- val
        }
        d
      })
    }
    # "global" and "area" scopes: global_max_height is already in data or
    # is not needed.

    grobs <- lapply(groups, \(group) {
      self$draw_group(
        group,
        panel_params,
        coord,
        flipped_aes = flipped_aes,
        alpha_fade_to = alpha_fade_to,
        outline.type = outline.type
      )
    })

    grid::gTree(children = do.call(grid::gList, grobs))
  },

  draw_group = \(
    self,
    data,
    panel_params,
    coord,
    flipped_aes = FALSE,
    alpha_fade_to = 0,
    outline.type = "upper",
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    na.rm = FALSE
  ) {
    if (nrow(data) < 2L) {
      return(ggplot2::zeroGrob())
    }

    if (inherits(coord, "CoordPolar") || inherits(coord, "CoordRadial")) {
      cli::cli_warn(
        c(
          "!" = "{.fn geom_ridgeline_fade} does not support radial gradients in polar coordinates.",
          "i" = "Falling back to standard ridgeline rendering."
        ),
        .frequency = "once",
        .frequency_id = "geom_ridgeline_fade_polar_unsupported"
      )
      return(
        ggplot2::ggproto_parent(ggplot2::GeomRibbon, self)$draw_group(
          data,
          panel_params,
          coord,
          flipped_aes = flipped_aes,
          outline.type = outline.type,
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre,
          na.rm = na.rm
        )
      )
    }

    # Alpha at the peak; NA → fully opaque
    a_start <- data$alpha[1L]
    if (is.na(a_start)) {
      a_start <- 1
    }

    # Outline handling (same logic as GeomAreaFade)
    if (identical(outline.type, "none")) {
      data$colour <- NA
      outline.type <- "upper"
    } else if (is.na(data$colour[1L])) {
      data$colour <- data$fill[1L]
    }

    # ymin = ridge baseline (constant per ridge, set by PositionRidgeline).
    # ymax = baseline + scale * height; can be above or below baseline.
    baseline <- data$ymin[1L]
    if (!any(!is.na(data$ymax))) {
      return(ggplot2::zeroGrob())
    }
    val_lo <- min(data$ymax, na.rm = TRUE) # most negative extreme
    val_hi <- max(data$ymax, na.rm = TRUE) # most positive extreme

    # Unsigned excursions from baseline (always non-negative).
    excursion_lo <- max(0, baseline - val_lo) # depth below baseline
    excursion_hi <- max(0, val_hi - baseline) # height above baseline
    max_excursion <- max(excursion_lo, excursion_hi)

    if (!is.finite(max_excursion) || max_excursion <= 0) {
      return(ggplot2::zeroGrob())
    }

    # Scale a_start relative to the reference maximum (group or global scope).
    # group_max_height / global_max_height are computed with abs() in draw_panel.
    ref_max <- data$global_max_height[1L] %||% data$group_max_height[1L]
    if (!is.null(ref_max) && is.finite(ref_max) && ref_max > 0) {
      a_start <- alpha_fade_to +
        (a_start - alpha_fade_to) * max_excursion / ref_max
    }

    # Alpha at each extreme, proportional to unsigned excursion.
    alpha_lo <- alpha_fade_to +
      (a_start - alpha_fade_to) * excursion_lo / max_excursion
    alpha_hi <- alpha_fade_to +
      (a_start - alpha_fade_to) * excursion_hi / max_excursion

    fill_col <- data$fill[1L]
    has_multi_fill <- length(unique(data$fill[!is.na(data$fill)])) > 1L

    # Clear alpha so parent doesn't apply it twice.
    data$alpha <- NA

    # Relative position of the baseline within the polygon bounding box:
    # 0 = all-positive (baseline at bottom), 1 = all-negative (baseline at top).
    y_bottom <- min(baseline, val_lo)
    y_top <- max(baseline, val_hi)
    baseline_bbox <- (baseline - y_bottom) / (y_top - y_bottom)

    # Transform trough, baseline, and peak to panel NPC coordinates.
    ref_df <- data.frame(x = data$x[1L], y = c(val_lo, baseline, val_hi))
    ref_npc <- coord$transform(ref_df, panel_params)
    pos_npc <- pmax(0, pmin(1, ref_npc$y))

    # --- Build polygon grob from parent --------------------------------------
    grob <- ggplot2::ggproto_parent(ggplot2::GeomRibbon, self)$draw_group(
      data,
      panel_params,
      coord,
      flipped_aes = flipped_aes,
      outline.type = outline.type,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm
    )

    # Delegate tier construction to shared helper; makeContent() picks the
    # rendering tier at actual draw time based on the output device.
    .build_area_fade_grob(
      poly_grob = grob,
      fill_col = fill_col,
      has_multi_fill = has_multi_fill,
      a_start = a_start,
      alpha_fade_to = alpha_fade_to,
      alpha_lo = alpha_lo,
      alpha_hi = alpha_hi,
      anchor_bbox = baseline_bbox,
      pos_npc = pos_npc
    )
  }
)


#' @title Ridgeline Plots with Fading Linear Gradient
#' @description
#' `geom_ridgeline_fade()` draws ridgeline plots — multiple area shapes
#' stacked at different vertical offsets — with a vertical alpha gradient that
#' fades from opaque at the peaks to transparent at each ridge's baseline.
#'
#' The gradient machinery is shared with [geom_area_fade()]; the difference is
#' that each group's baseline is its own `y` value rather than zero, enabling
#' the characteristic overlapping-ridges layout.
#'
#' @section Coordinate systems:
#' `geom_ridgeline_fade()` only supports linear gradients. When used with
#' [ggplot2::coord_polar()] or [ggplot2::coord_radial()], the geom falls back
#' to standard ridgeline rendering (equivalent to [ggplot2::geom_ribbon()]),
#' which means no gradient fill is added. The geom emits a warning in this
#' case.
#'
#' @concept ridgeline plots
#' @concept fading gradient
#'
#' @aesthetics GeomRidgelineFade
#'
#' @inheritParams ggplot2::geom_ribbon
#' @param alpha_fade_to A single finite number between 0 and 1. The alpha value
#'   at the baseline of each ridge. Defaults to `0` (fully transparent).
#' @param alpha_scope How to scale alpha across ridges.
#'   * `"area"`: every ridge independently uses the full alpha range from
#'     `alpha_fade_to` to full opacity.
#'   * `"group"` (default): alpha is scaled relative to the tallest ridge
#'     *at each y-baseline*. Within the same y-level, the tallest ridge is
#'     fully opaque and shorter ridges appear more transparent; ridges at
#'     different y-levels are independent of each other.
#'   * `"global"`: alpha is scaled relative to the tallest ridge in the
#'     entire panel.
#' @param scale Height multiplier applied to `height`. Values > 1 increase
#'   overlap between adjacent ridges. Defaults to `1`.
#' @param min_height Minimum `height` value to draw. Points with
#'   `height < min_height` are dropped, creating gaps in the ridgeline.
#'   Defaults to `0`.
#' @param ... Additional arguments passed to [ggplot2::layer()], including
#'   `outline.type` (which edges to outline: `"upper"`, `"lower"`, `"both"`,
#'   `"full"`, or `"none"`; default `"upper"`).
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @section Legend key order:
#' Ridges are rendered back-to-front: the ridge with the **highest** y-baseline
#' is drawn first (furthest back) and the ridge with the **lowest** y-baseline
#' is drawn last (on top). When `fill` tracks `y`, the default fill legend
#' lists levels in ascending order — placing the lowest y at the top of the
#' legend — which is the **reverse** of the spatial top-to-bottom reading order
#' (highest y at top of chart, lowest y at bottom).
#'
#' To align the legend with the chart, reverse the legend keys:
#'
#' ```r
#' + guides(fill = guide_legend(reverse = TRUE))
#' ```
#'
#' @seealso [geom_ridgeline_density_fade()] for the convenience density-ridgeline
#'   wrapper, [geom_area_fade()] for area plots with the same gradient effect,
#'   [position_ridgeline()] for the position that computes ridge bounds,
#'   \href{https://wilkelab.org/ggridges/reference/geom_density_ridges.html}{\code{ggridges::geom_density_ridges()}} for the full-featured ridgeline geom that
#'   inspired this family.
#'
#' @references
#' Murrell, P. (2021). "Luminance Masks in R Graphics." Technical Report
#' 2021-04, Department of Statistics, The University of Auckland. Version 1.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/masks/masks.html}
#'
#' Murrell, P. (2022). "Vectorised Pattern Fills in R Graphics." Technical
#' Report 2022-01, Department of Statistics, The University of Auckland.
#' Version 1.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/vecpat/vecpat.html}
#'
#' Murrell, P., Pedersen, T. L., and Skintzos, P. (2023). "Porter-Duff
#' Compositing Operators in R Graphics." Department of Statistics, The
#' University of Auckland. Version 1.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/compositing/compositing.html}
#'
#' Murrell, P. (2023). "Groups, Compositing Operators, and Affine
#' Transformations in R Graphics." Technical Report 2021-02, Department of
#' Statistics, The University of Auckland. Version 3.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html}
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' d <- data.frame(
#'   x = rep(1:5, 3) + c(rep(0, 5), rep(0.3, 5), rep(0.6, 5)),
#'   y = c(rep(0, 5), rep(1, 5), rep(3, 5)),
#'   height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1)
#' )
#'
#' # Basic ridgeline
#' ggplot(d, aes(x, y, height = height, group = y, fill = factor(y))) +
#'   geom_ridgeline_fade() +
#'   scale_fill_viridis_d(direction = -1, guide = "none")
#'
#' # Increase overlap with scale
#' ggplot(d, aes(x, y, height = height, group = y, fill = factor(y))) +
#'   geom_ridgeline_fade(scale = 2) +
#'   scale_fill_viridis_d(direction = -1, guide = "none")
#'
#' # Global alpha scope: shorter ridges appear more transparent
#' ggplot(d, aes(x, y, height = height, group = y, fill = factor(y))) +
#'   geom_ridgeline_fade(alpha_scope = "global") +
#'   scale_fill_viridis_d(direction = -1, guide = "none")
#'
#' # Keep some opacity at the baseline
#' ggplot(d, aes(x, y, height = height, group = y, fill = factor(y))) +
#'   geom_ridgeline_fade(alpha_fade_to = 0.3, scale = 1.5) +
#'   scale_fill_viridis_d(direction = -1, guide = "none")
#'
#' # Aligning legend keys with the chart: ridges are drawn highest-y-first, so
#' # guide_legend(reverse = TRUE) puts the top-of-chart ridge at the top of
#' # the legend.
#' ggplot(d, aes(x, y, height = height, group = y, fill = factor(y))) +
#'   geom_ridgeline_fade() +
#'   scale_fill_viridis_d(direction = -1) +
#'   guides(fill = guide_legend(reverse = TRUE))
#'
#' # Density ridgeline using stat = "density"
#' ggplot(iris, aes(Sepal.Length, y = as.numeric(Species),
#'                  group = Species, fill = Species)) +
#'   geom_ridgeline_fade(
#'     mapping = aes(height = after_stat(density)),
#'     stat = "density",
#'     scale = 3
#'   ) +
#'   scale_fill_viridis_d(option = "C") +
#'   scale_y_continuous(breaks = 1:3, labels = levels(iris$Species)) +
#'   guides(fill = guide_legend(reverse = TRUE)) +
#'   theme_minimal()
#'
geom_ridgeline_fade <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  ...,
  alpha_fade_to = 0,
  alpha_scope = "group",
  scale = 1,
  min_height = 0,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRidgelineFade,
    position = position_ridgeline(scale = scale, min_height = min_height),
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      alpha_fade_to = alpha_fade_to,
      alpha_scope = alpha_scope,
      na.rm = na.rm,
      ...
    )
  )
}


#' Density Ridgeline Plots with Fading Gradient
#'
#' @description
#' `geom_ridgeline_density_fade()` is a convenience wrapper around
#' [geom_ridgeline_fade()] that uses [ggplot2::stat_density()] to compute a
#' kernel density estimate and maps the result to `height` automatically via
#' `aes(height = after_stat(density))`. The concept is inspired by
#' \href{https://wilkelab.org/ggridges/reference/geom_density_ridges.html}{\code{ggridges::geom_density_ridges()}}; unlike that function, no panel-level
#' auto-scaling is performed — adjust `scale` manually so that adjacent ridges
#' reach the desired overlap.
#'
#' @concept density ridges
#' @concept fading gradient
#'
#' @inheritParams geom_ridgeline_fade
#' @param ... Additional arguments passed to [geom_ridgeline_fade()], including
#'   smoothing parameters (`bw`, `adjust`, `kernel`, `n`, `trim`, `bounds`)
#'   forwarded to [ggplot2::stat_density()].
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @seealso [geom_ridgeline_fade()] for the lower-level geom,
#'   \href{https://wilkelab.org/ggridges/reference/geom_density_ridges.html}{\code{ggridges::geom_density_ridges()}}
#'   for the full-featured original this is inspired by.
#'
#' @export
#' @examples
#' # Density ridgelines — convenience wrapper for the stat_density example above
#' ggplot(iris, aes(
#'   x = Sepal.Length,
#'   y = as.integer(Species),
#'   group = Species,
#'   fill = after_stat(x)
#' )
#' ) +
#'   geom_ridgeline_density_fade(scale = 2, alpha_scope = "area") +
#'   scale_fill_viridis_c(option = "C") +
#'   theme_minimal()
geom_ridgeline_density_fade <- function(
  mapping = NULL,
  data = NULL,
  stat = "density",
  ...,
  alpha_fade_to = 0,
  alpha_scope = "group",
  scale = 1,
  min_height = 0,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  # Inject height = after_stat(density) as a default; the user's own height
  # mapping takes precedence if they supply one explicitly.
  mapping <- utils::modifyList(
    ggplot2::aes(height = ggplot2::after_stat(density)),
    mapping %||% ggplot2::aes()
  )
  geom_ridgeline_fade(
    mapping = mapping,
    data = data,
    stat = stat,
    ...,
    alpha_fade_to = alpha_fade_to,
    alpha_scope = alpha_scope,
    scale = scale,
    min_height = min_height,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}
