# Unit bar charts -- bar charts where each bar is a strip of discrete cells,
# one cell per unit of data.
#
# Architecture: Stats (stat-bar-cells.R) output bar-level rows with an integer
# `count` column.  GeomBarCells inherits from GeomBar to get setup_data
# (xmin/xmax/ymin/ymax computation) and orientation support.  Position
# adjustments (stack, dodge, fill, reverse) run on the bar-level rows.  Cell
# expansion happens in draw_panel, AFTER positioning.

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBarCells <- ggplot2::ggproto(
  "GeomBarCells",
  ggplot2::GeomBar,

  extra_params = c("just", "na.rm", "orientation", "radius", "max_cells"),

  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    radius = grid::unit(0, "npc"),
    max_cells = 1e4,
    lineend = "butt",
    linejoin = "mitre"
  ) {
    # NULL radius → sharp corners (same as the default).
    if (is.null(radius)) {
      radius <- grid::unit(0, "npc")
    }
    if (!grid::is.unit(radius)) {
      cli::cli_abort(
        c(
          "{.arg radius} must be a {.fn grid::unit} object.",
          "i" = "Got {.obj_type_friendly {radius}}; did you forget {.fn grid::unit}?"
        ),
        call = NULL
      )
    }

    # Cartesian without fixed ratio lets cells stretch and squash; nudge the
    # user to add coord_equal().  Polar and other non-Cartesian coords are
    # intentional visualisation choices, so skip the warning for them.
    # In ggplot2 v4, coord_equal()/coord_fixed() are plain CoordCartesian with
    # $ratio set, not a CoordFixed subclass — detect via the ratio slot.
    if (inherits(coord, "CoordCartesian") && is.null(coord$ratio)) {
      cli::cli_warn(
        c(
          "{.fn geom_unit_*} works best with a fixed-ratio coordinate.",
          "i" = "Add {.code + coord_equal()} to keep cells square."
        ),
        call = NULL,
        .frequency = "regularly",
        .frequency_id = "ggpointless_unit_bar_coord_equal"
      )
    }

    # Flip to canonical orientation (y = value axis) for cell expansion
    flipped <- isTRUE(data$flipped_aes[1L])
    data <- ggplot2::flip_data(data, flipped)

    # Soft cap on total cells per panel.  Cell expansion allocates one rect
    # per cell; a single bar with y = 1e6 would freeze the graphics device.
    # When exceeded, fall back to plain rect rendering (one rect per bar).
    # Pass `max_cells = Inf` to opt out.
    eps_n <- 1e-9
    raw_h <- data$ymax - data$ymin
    raw_h[!is.finite(raw_h) | raw_h <= eps_n] <- 0
    n_cells_total <- sum(floor(raw_h) + (raw_h %% 1 > eps_n))
    if (is.finite(max_cells) && n_cells_total > max_cells) {
      cli::cli_warn(
        c(
          "Refusing to tile {.val {n_cells_total}} cells (cap: {.val {max_cells}}).",
          "i" = "Falling back to solid bars.",
          "i" = "Pass {.code max_cells = Inf} to override, or rescale {.arg y}."
        ),
        call = NULL,
        .frequency = "regularly",
        .frequency_id = "ggpointless_unit_bar_max_cells"
      )
      data <- ggplot2::flip_data(data, flipped)
      return(ggplot2::GeomRect$draw_panel(
        data,
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin
      ))
    }

    # Expand each bar segment into unit-height cells plus one partial cell at
    # the outer end.  Each full cell is height 1 in data space.
    #  - Positive segments (ymin >= 0): tile upward from ymin, partial at ymax.
    #  - Negative segments (ymax <= 0): tile downward from ymax, partial at ymin.
    # A per-cell boolean `.at_segment_edge_*` is stamped so the inset step below
    # can skip the gap at the two segment boundaries (ymin / ymax of the bar).
    eps <- 1e-9
    cells <- lapply(seq_len(nrow(data)), function(i) {
      row <- data[i, , drop = FALSE]
      raw <- row$ymax - row$ymin
      if (!is.finite(raw) || raw <= eps) {
        return(NULL)
      }

      n_full <- as.integer(floor(raw))
      partial <- raw - n_full
      n_cells <- n_full + (partial > eps)
      if (n_cells <= 0L) {
        return(NULL)
      }

      downward <- row$ymax <= eps
      expanded <- row[rep(1L, n_cells), , drop = FALSE]
      k <- seq_len(n_cells) - 1L
      if (downward) {
        # baseline = ymax (top), partial at ymin (bottom/outer)
        ymax_i <- row$ymax - k
        ymin_i <- pmax(ymax_i - 1, row$ymin)
      } else {
        # baseline = ymin (bottom), partial at ymax (top/outer)
        ymin_i <- row$ymin + k
        ymax_i <- pmin(ymin_i + 1, row$ymax)
      }
      expanded$ymin <- ymin_i
      expanded$ymax <- ymax_i
      expanded$xmin <- row$xmin
      expanded$xmax <- row$xmax
      # Mark which edges are segment boundaries (no gap should be added there).
      expanded$.at_seg_min <- abs(ymin_i - row$ymin) < eps
      expanded$.at_seg_max <- abs(ymax_i - row$ymax) < eps
      expanded
    })

    data <- do.call(rbind, cells)
    if (is.null(data) || nrow(data) == 0L) {
      return(grid::nullGrob())
    }
    rownames(data) <- NULL

    # Constant data-space gap between neighbouring cells.  Inset is applied
    # only at edges that are NOT segment boundaries (so no gap is drawn at
    # the baseline or at the outer end of the bar).  Guard against the
    # pathological case where the inset would exceed cell height by clamping
    # to 40% of the cell height per side.
    gap_half <- 0.025
    cell_h <- data$ymax - data$ymin
    g_eff <- pmin(gap_half, cell_h * 0.4)
    data$ymin <- ifelse(data$.at_seg_min, data$ymin, data$ymin + g_eff)
    data$ymax <- ifelse(data$.at_seg_max, data$ymax, data$ymax - g_eff)
    data$.at_seg_min <- NULL
    data$.at_seg_max <- NULL

    # Flip back to original orientation
    data <- ggplot2::flip_data(data, flipped)

    # Rounded corners require per-cell roundrectGrob rendering in NPC, which
    # only makes sense under a linear coordinate system.  For everything else
    # (including polar), delegate to GeomRect, whose non-linear branch emits
    # polygons that CoordPolar arc-interpolates into proper wedges.
    use_rounding <- !identical(as.numeric(radius), 0) && coord$is_linear()

    if (!use_rounding) {
      return(ggplot2::GeomRect$draw_panel(
        data,
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin
      ))
    }

    coords <- coord$transform(data, panel_params)
    gl <- lapply(seq_len(nrow(coords)), function(i) {
      grid::roundrectGrob(
        x = grid::unit(coords$xmin[i], "npc"),
        y = grid::unit(coords$ymax[i], "npc"),
        width = grid::unit(coords$xmax[i] - coords$xmin[i], "npc"),
        height = grid::unit(coords$ymax[i] - coords$ymin[i], "npc"),
        r = radius,
        just = c("left", "top"),
        gp = ggplot2::gg_par(
          col = coords$colour[i],
          fill = ggplot2::fill_alpha(coords$fill[i], coords$alpha[i]),
          lwd = coords$linewidth[i],
          lty = coords$linetype[i]
        )
      )
    })
    grid::grobTree(children = do.call(grid::gList, gl))
  },

  draw_key = ggplot2::draw_key_rect
)


#' Unit Bar Charts
#'
#' @description
#' Unit bar charts represent data as vertical (or, after [ggplot2::coord_flip()],
#' horizontal) strips of discrete cells, where each cell represents one unit
#' of data. They follow the same `x`/`y` conventions as [ggplot2::geom_bar()],
#' [ggplot2::geom_col()], and [ggplot2::geom_histogram()]:
#'
#' * `geom_unit_bar()` counts observations (one row = one cell), like
#'   [ggplot2::geom_bar()]. Map `x` (or `y` for horizontal bars) to the
#'   grouping variable; `fill` to colour by a second variable.
#' * `geom_unit_col()` uses pre-computed `y` values, like [ggplot2::geom_col()].
#'   Fractional values are supported: `y = 3.7` draws 3 full unit cells
#'   (height 1 in data space) plus a partial cell of height 0.7 at the top.
#' * `geom_unit_histogram()` bins a continuous variable and draws the resulting
#'   counts as cell strips, like [ggplot2::geom_histogram()]. Pass `bins` or
#'   `binwidth` to control the number of bins.
#'
#' Any stat that produces positive `y` (or `ymin`/`ymax`) values works as a
#' drop-in: `geom_unit_bar(stat = "bin")` gives a tiled histogram without a
#' dedicated stat.
#'
#' All position adjustments supported by [ggplot2::geom_bar()] work here:
#' `"stack"` (default), `"dodge"`, `"fill"`,
#' `position_stack(reverse = TRUE)`, etc.
#'
#' Use [ggplot2::coord_equal()] to ensure cells render as squares. Pass a
#' `ratio` to [ggplot2::coord_equal()] to render non-square cells, e.g.
#' `coord_equal(ratio = 2)` for cells twice as tall as wide.
#'
#' @concept unit chart
#' @concept unit bar chart
#' @concept isotype chart
#' @concept bar chart
#'
#' @aesthetics GeomBarCells
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#'   For `geom_unit_bar()`, `x` (or `y` for horizontal bars) is required.
#'   For `geom_unit_col()`, both `x` and `y` are required. Map `fill` to
#'   colour segments.
#' @param data A data frame.
#' @param stat The statistical transformation to use. Override the default to
#'   use a different stat, e.g. `stat = "bin"` for a tiled histogram.
#' @param position A position adjustment to use on the data. Default
#'   `"stack"` stacks bars on top of each other. Use `"dodge"` for
#'   side-by-side bars, `"fill"` for proportional stacking, or
#'   `position_stack(reverse = TRUE)` to reverse the stacking order.
#' @param just Justification of the bar relative to its x position.
#'   `0.5` (default) centres the bar on `x`, `0` aligns the left edge,
#'   `1` aligns the right edge. Same as [ggplot2::geom_bar()].
#' @param radius Corner radius for each cell as a [grid::unit()]. Default
#'   `grid::unit(0, "npc")` gives sharp corners. Only used with linear
#'   coordinates; non-linear coordinates (e.g. [ggplot2::coord_polar()])
#'   fall back to sharp corners.
#' @param max_cells Soft cap on total cells drawn per panel. When the cap is
#'   exceeded, the layer renders as solid bars (one rectangle per bar) and
#'   emits a warning. Default `1e4`; pass `Inf` to disable.
#' @param na.rm If `FALSE` (default), rows with missing or non-positive
#'   `y` are dropped with a warning.
#' @param orientation The orientation of the layer. Default (`NA`) is guessed
#'   from the aesthetic mapping. Set to `"x"` for vertical bars (value on y)
#'   or `"y"` for horizontal bars (value on x). Same as [ggplot2::geom_bar()].
#' @param show.legend logical. Should this layer appear in the legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param ... Other arguments passed to [ggplot2::layer()].
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @note Add [ggplot2::coord_equal()] to ensure cells render as squares.
#'   Use `coord_equal(ratio = r)` for non-square cells.
#'
#' @section Performance:
#' The geom allocates one grid rect per cell, so cost scales with total cell
#' count, not input rows. A plot with `y` values in the millions would emit
#' millions of rects and freeze the graphics device.
#'
#' To guard against this, the layer ships with a soft cap controlled by
#' `max_cells` (default `1e4` per panel). When exceeded, the layer falls back
#' to plain bars (one rectangle per bar, like [ggplot2::geom_col()]) and emits
#' a warning. Pass `max_cells = Inf` to opt out, or rescale `y` so a cell
#' represents a larger unit (e.g. tens instead of ones).
#'
#' Rounded corners (`radius > 0`) add a `roundrectGrob` per cell and are
#' therefore the most expensive path; leave `radius` at its default for large
#' plots. Non-linear coordinates always use plain rects regardless of
#' `radius`.
#'
#' @seealso [ggplot2::geom_bar()], [ggplot2::geom_col()],
#'   [ggplot2::geom_tile()] for the underlying tile rendering.
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' # geom_unit_bar: count observations automatically (like geom_bar)
#' ggplot(mpg, aes(x = class, fill = drv)) +
#'   geom_unit_bar() +
#'   coord_equal()
#'
#' # Horizontal bars via y aesthetic
#' ggplot(mpg, aes(y = class, fill = drv)) +
#'   geom_unit_bar()
#'
#' # Dodged bars
#' ggplot(mpg, aes(x = class, fill = drv)) +
#'   geom_unit_bar(position = "dodge") +
#'   coord_equal()
#'
#' # Reversed stack
#' ggplot(mpg, aes(x = class, fill = drv)) +
#'   geom_unit_bar(position = position_stack(reverse = TRUE)) +
#'   coord_equal()
geom_unit_bar <- make_constructor(
  GeomBarCells,
  stat = "count_cells",
  position = "stack",
  just = 0.5,
  radius = grid::unit(0, "npc"),
  orientation = NA,
  max_cells = 1e4
)


#' @rdname geom_unit_bar
#' @export
#' @examples
#'
#' # geom_unit_col: pre-computed counts in y (like geom_col)
#' ep_data <- data.frame(
#'   episode = factor(
#'     rep(paste0("Ep ", 1:5), each = 2),
#'     levels = paste0("Ep ", 5:1)
#'   ),
#'   gender  = factor(rep(c("Female", "Male"), 5)),
#'   minutes = c(8, 12, 15, 5, 6, 14, 10, 10, 4, 16)
#' )
#'
#' ggplot(ep_data, aes(x = episode, y = minutes, fill = gender)) +
#'   geom_unit_col() +
#'   coord_equal()
#'
#' # Flat cells with rounded corners via coord_equal(ratio)
#' ggplot(ep_data, aes(x = episode, y = minutes, fill = gender)) +
#'   geom_unit_col(radius = grid::unit(3, "pt")) +
#'   coord_equal(ratio = 1/4)
#'
#' # Horizontal bars via orientation = "y" (value on x)
#' ggplot(data.frame(x = 1:5, y = c(2, 4, 3, 5, 1)), aes(x, y)) +
#'   geom_unit_col(orientation = "y") +
#'   coord_equal()
#'
#' # use stat = "bin" to create a histogram
#' ggplot(mpg, aes(x = displ)) +
#'   geom_unit_bar(stat = "bin")
#'
geom_unit_col <- make_constructor(
  GeomBarCells,
  stat = "bar_cells",
  position = "stack",
  just = 0.5,
  radius = grid::unit(0, "npc"),
  orientation = NA,
  max_cells = 1e4
)


#' @rdname geom_unit_bar
#' @export
#' @examples
#'
#' # geom_unit_histogram: tiled histogram for continuous variables
#' ggplot(mpg, aes(x = displ)) +
#'   geom_unit_histogram(bins = 10) +
#'   coord_equal()
#'
#' # Colour by a second variable; stat = "bin" also works directly
#' ggplot(mpg, aes(x = hwy, fill = drv)) +
#'   geom_unit_histogram(bins = 15) +
#'   coord_equal()
geom_unit_histogram <- make_constructor(
  GeomBarCells,
  stat = "bin",
  position = "stack",
  radius = grid::unit(0, "npc"),
  max_cells = 1e4
)
