# Unit bar charts -- bar charts where each bar is a strip of discrete cells,
# one cell per unit of data.
#
# Architecture: Stats (stat-bar-cells.R) output bar-level rows with an integer
# `count` column.  GeomBarCells inherits from GeomBar to get setup_data
# (xmin/xmax/ymin/ymax computation) and orientation support.  Position
# adjustments (stack, dodge, fill, reverse) run on the bar-level rows.  Cell
# expansion happens in draw_panel, AFTER positioning.

#' Key glyph for unit bar charts
#'
#' @description
#' The default legend key for [geom_unit_bar()] / [geom_unit_col()] /
#' [geom_unit_histogram()].  Renders a 2 × 2 grid of small cells with a
#' tiny inter-cell gap, so the legend advertises the unit-cell character
#' of the geom rather than showing a plain solid rectangle.
#'
#' @inheritParams ggplot2::draw_key
#' @return A grid grob.
#' @export
#' @keywords internal
draw_key_unit <- function(data, params, size) {
  fill <- ggplot2::fill_alpha(data$fill %||% "grey30", data$alpha)
  col  <- data$colour %||% NA
  lwd  <- data$linewidth %||% 0.5
  lty  <- data$linetype %||% 1

  # 2x2 grid of cells, each occupying ~40% of the key box, with a ~10%
  # gap between them (relative to the key width/height).  Centres at
  # (0.275, 0.275), (0.725, 0.275), (0.275, 0.725), (0.725, 0.725).
  grid::rectGrob(
    x = grid::unit(rep(c(0.275, 0.725), 2L), "npc"),
    y = grid::unit(rep(c(0.275, 0.725), each = 2L), "npc"),
    width  = grid::unit(0.4, "npc"),
    height = grid::unit(0.4, "npc"),
    gp = ggplot2::gg_par(
      fill = fill,
      col  = col,
      lwd  = lwd,
      lty  = lty
    )
  )
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBarCells <- ggplot2::ggproto(
  "GeomBarCells",
  ggplot2::GeomBar,

  extra_params = c("just", "na.rm", "orientation", "radius", "cell_size", "cell_padding", "cell_count_cap"),

  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    radius = grid::unit(0, "npc"),
    cell_size = 1,
    cell_padding = 0.025,
    cell_count_cap = 1e4,
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

    # `cell_size` controls how many data-units one cell represents.  Must be
    # a positive finite scalar (Inf is meaningless — would draw zero cells).
    if (
      !is.numeric(cell_size) ||
        length(cell_size) != 1L ||
        is.na(cell_size) ||
        !is.finite(cell_size) ||
        cell_size <= 0
    ) {
      cli::cli_warn(
        c(
          "{.arg cell_size} must be a positive finite scalar number.",
          "x" = "Got {.val {cell_size}}.",
          "i" = "Falling back to the default ({.val 1})."
        ),
        call = NULL
      )
      cell_size <- 1
    }

    # `cell_padding` is the inset per side, as a fraction of `cell_size` in
    # data space.  Matches CSS's `padding` semantics:
    #   length 1 — same padding on all four sides
    #   length 2 — c(vertical, horizontal); vertical is the value-axis gap
    #              between stacked cells, horizontal is the category-axis gap
    #              between cells and their bar's outer edges.
    # Each element must be finite, numeric, and in [0, 0.5) — 0.5 or above
    # would collapse cells to zero width/height.
    if (
      !is.numeric(cell_padding) ||
        !length(cell_padding) %in% c(1L, 2L) ||
        anyNA(cell_padding) ||
        !all(is.finite(cell_padding)) ||
        any(cell_padding < 0) ||
        any(cell_padding >= 0.5)
    ) {
      cli::cli_warn(
        c(
          "{.arg cell_padding} must be a finite numeric vector of length 1 \\
           or 2 with each element in {.code [0, 0.5)}.",
          "x" = "Got {.val {cell_padding}}.",
          "i" = "Falling back to the default ({.val 0.025})."
        ),
        call = NULL
      )
      cell_padding <- 0.025
    }
    # Normalise to length 2: c(pad_v, pad_h)
    if (length(cell_padding) == 1L) {
      cell_padding <- c(cell_padding, cell_padding)
    }
    pad_v <- cell_padding[1L]  # vertical (stacking-axis) padding
    pad_h <- cell_padding[2L]  # horizontal (bar-axis) padding

    # `cell_count_cap` must be a positive scalar (or Inf to disable the cap).  Zero,
    # negative, NA, non-numeric, or non-scalar values are nonsensical — warn
    # and fall back to the default rather than fail outright (matches the
    # existing cap-exceeded warning style further down).
    if (
      !is.numeric(cell_count_cap) ||
        length(cell_count_cap) != 1L ||
        is.na(cell_count_cap) ||
        (is.finite(cell_count_cap) && cell_count_cap < 1)
    ) {
      cli::cli_warn(
        c(
          "{.arg cell_count_cap} must be a positive scalar number or {.code Inf}.",
          "x" = "Got {.val {cell_count_cap}}.",
          "i" = "Falling back to the default ({.val 10000})."
        ),
        call = NULL
      )
      cell_count_cap <- 1e4
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
    # per cell; a single bar with y = 1e6 (and default cell_size = 1) would
    # freeze the graphics device.  When exceeded, fall back to plain rect
    # rendering (one rect per bar).  Pass `cell_count_cap = Inf` to opt out, or set
    # `cell_size` so each cell aggregates more units.
    eps_n <- 1e-9
    raw_h <- (data$ymax - data$ymin) / cell_size
    raw_h[!is.finite(raw_h) | raw_h <= eps_n] <- 0
    n_cells_total <- sum(floor(raw_h) + (raw_h %% 1 > eps_n))
    if (is.finite(cell_count_cap) && n_cells_total > cell_count_cap) {
      cli::cli_warn(
        c(
          "Refusing to tile {.val {n_cells_total}} cells (cap: {.val {cell_count_cap}}).",
          "i" = "Falling back to solid bars.",
          "i" = "Set {.arg cell_size} so each cell aggregates more units \\
                 (e.g. {.code cell_size = 1e3}); pair with \\
                 {.code scale_*_continuous(labels = label_cells(cell_size))} \\
                 to relabel the axis in cell counts.",
          "i" = "Or pass {.code cell_count_cap = Inf} to disable the cap entirely."
        ),
        call = NULL,
        .frequency = "regularly",
        .frequency_id = "ggpointless_unit_bar_cell_count_cap"
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

    # Expand each bar segment into cells of height `cell_size` (in data space)
    # plus one partial cell at the outer end if the segment height is not a
    # multiple of `cell_size`.
    #  - Positive segments (ymin >= 0): tile upward from ymin, partial at ymax.
    #  - Negative segments (ymax <= 0): tile downward from ymax, partial at ymin.
    # A per-cell boolean `.at_segment_edge_*` is stamped so the inset step below
    # can skip the gap at the two segment boundaries (ymin / ymax of the bar).
    eps <- 1e-9
    cells <- lapply(seq_len(nrow(data)), function(i) {
      row <- data[i, , drop = FALSE]
      raw_units <- (row$ymax - row$ymin) / cell_size
      if (!is.finite(raw_units) || raw_units <= eps) {
        return(NULL)
      }

      n_full <- as.integer(floor(raw_units))
      partial <- raw_units - n_full
      n_cells <- n_full + (partial > eps)
      if (n_cells <= 0L) {
        return(NULL)
      }

      downward <- row$ymax <= eps
      expanded <- row[rep(1L, n_cells), , drop = FALSE]
      k <- seq_len(n_cells) - 1L
      if (downward) {
        # baseline = ymax (top), partial at ymin (bottom/outer)
        ymax_i <- row$ymax - k * cell_size
        ymin_i <- pmax(ymax_i - cell_size, row$ymin)
      } else {
        # baseline = ymin (bottom), partial at ymax (top/outer)
        ymin_i <- row$ymin + k * cell_size
        ymax_i <- pmin(ymin_i + cell_size, row$ymax)
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

    # Data-space inset applied on both axes; padding is scaled by `cell_size`
    # so absolute magnitudes stay consistent across `cell_size` settings.
    #
    # Vertical (value axis): inset only at edges that are NOT segment
    # boundaries — cells of one segment get gaps between them, but stacked
    # segments touch across their shared boundary (by design).
    #
    # Horizontal (bar axis): inset applied uniformly to every cell, since
    # all cells in a segment share the bar's `xmin` / `xmax`.  This gives
    # each cell a visible margin from the bar's left/right edges, making
    # dense stacks read as a grid of tiles rather than one solid bar.
    #
    # Both axes: cap the inset at 40% of the cell dimension per side to
    # prevent cells from collapsing to negative width/height.
    pad_v_abs <- pad_v * cell_size
    pad_h_abs <- pad_h * cell_size
    cell_h    <- data$ymax - data$ymin
    cell_w    <- data$xmax - data$xmin
    g_eff_v   <- pmin(pad_v_abs, cell_h * 0.4)
    g_eff_h   <- pmin(pad_h_abs, cell_w * 0.4)
    data$ymin <- ifelse(data$.at_seg_min, data$ymin, data$ymin + g_eff_v)
    data$ymax <- ifelse(data$.at_seg_max, data$ymax, data$ymax - g_eff_v)
    data$xmin <- data$xmin + g_eff_h
    data$xmax <- data$xmax - g_eff_h
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

  draw_key = draw_key_unit
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
#' @concept isotype chart
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
#' @param cell_size Number of data-units one cell represents. Default `1`
#'   (one cell per unit, the original "isotype" / pictogram pattern). Set to
#'   a larger value to aggregate units, e.g. `cell_size = 1e6` for one cell
#'   per million. Each cell is then `cell_size` tall in data space, so under
#'   `coord_equal()` you'll likely want
#'   `coord_equal(ratio = cell_size / width)` (default `width` is `0.9`) to
#'   keep cells visually square. The y-axis still shows the original data
#'   values; pair with `scale_y_continuous(labels = label_cells(cell_size))`
#'   if you want it to show cell counts instead.
#' @param cell_padding Inset applied per side of each cell, as a fraction of
#'   `cell_size` in data space. Matches CSS `padding` semantics:
#'   * length 1 (default `0.025`) — same padding on all four sides.
#'   * length 2 — `c(vertical, horizontal)`; `vertical` is the gap
#'     between vertically-stacked cells, `horizontal` is the gap between
#'     each cell and its bar's left/right edge.
#'
#'   Each element must be finite and in `[0, 0.5)`. Set `cell_padding = 0`
#'   for cells that touch (borderless isotype style); increase it for a
#'   waffle-like grid of separated tiles.  Vertical padding is suppressed
#'   at the outer edges of each stacked segment so segments touch across
#'   their shared boundary.
#' @param cell_count_cap Soft cap on total cells drawn per panel. When the cap is
#'   exceeded, the layer renders as solid bars (one rectangle per bar) and
#'   emits a warning. Default `1e4`; pass `Inf` to disable. Setting a larger
#'   `cell_size` is usually the better fix for large `y`.
#' @param lineend Line end style for the cell border when `colour` is set.
#'   One of `"round"`, `"butt"` (default), or `"square"`. Same as
#'   [ggplot2::geom_bar()].
#' @param linejoin Line join style for the cell border. One of `"round"`,
#'   `"mitre"` (default), or `"bevel"`. Same as [ggplot2::geom_bar()].
#' @param na.rm If `FALSE` (default), rows with missing `y` are dropped
#'   with a warning. Non-positive `y` values are kept and produce empty
#'   segments (zero) or downward-tiled cells (negative).
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
#' @section Cell rendering caveats:
#'
#' A few details are easy to overlook when pairing this geom with various
#' coords, positions, and scales:
#'
#' \subsection{Aspect ratio}{
#' Cells are rendered as rectangles of `width × cell_size` in data space.
#' The `width` default is inherited from [ggplot2::geom_bar()] at `0.9`,
#' while `cell_size` defaults to `1` — so under `coord_equal(ratio = 1)`
#' cells render ~11\% taller than wide, not perfectly square. To get
#' square cells, either match `width` to `cell_size`:
#' ```r
#' geom_unit_col(width = 1)
#' ```
#' or override the coord ratio:
#' ```r
#' coord_equal(ratio = cell_size / width)   # default: 1 / 0.9
#' ```
#' }
#'
#' \subsection{`position = "fill"`}{
#' `position_fill()` normalises every stack to height `1`. With the default
#' `cell_size = 1` each stack collapses to a **single** cell — the
#' unit-visualisation semantics disappear. Use `"stack"` (default) or
#' `"dodge"` for unit plots; `"fill"` is only meaningful if you also set
#' `cell_size` to a sub-`1` value.
#' }
#'
#' \subsection{`position = "dodge"`}{
#' `position_dodge()` splits the bar `width` across sub-groups, so each
#' sub-bar shrinks to `width / n_groups`. Under `coord_equal()` the cells
#' in dodged bars therefore become progressively narrower relative to
#' `cell_size`. To restore square cells under dodge, pass a wider
#' `coord_equal()` ratio:
#' ```r
#' coord_equal(ratio = cell_size * n_groups / width)
#' # e.g. n_groups = 3, defaults cell_size = 1 and width = 0.9:
#' coord_equal(ratio = 3 / 0.9)
#' ```
#' Note that setting `width = n_groups` does **not** work — it makes each
#' category's total span wider than the distance between categories, and
#' dodged sub-bars from adjacent categories overlap.
#' }
#'
#' \subsection{Log and other non-linear y scales}{
#' Scale transforms run **before** the stat, so cells tile in the
#' transformed space. `scale_y_log10()` on `y = c(10, 100, 1000)` produces
#' cells at log10 values `1, 2, 3` — the bars do not visually reflect the
#' original counts. Use a linear y-scale if you want one cell per
#' observation, or set `cell_size` to match the transformed units.
#' }
#'
#' \subsection{Gap visibility at small panel sizes}{
#' The inter-cell gap is a fixed `0.025 * cell_size` in data space. On
#' very small panels (or very tall bars) the gap can collapse below 1 px
#' and visually disappear — cells appear fused. Either enlarge the panel
#' or reduce `cell_size` so individual cells take up more pixels.
#' }
#'
#' \subsection{Stacked-segment junction}{
#' The gap applies between cells **within a segment**. The junction
#' between two stacked segments (same bar, different `fill`) has no gap
#' — cells touch across the segment boundary. This is intentional: the
#' stack should read as one continuous bar broken into coloured bands.
#' }
#'
#' @section Performance:
#' The geom allocates one grid rect per cell, so cost scales with total cell
#' count, not input rows. A plot with `y` values in the millions would emit
#' millions of rects and freeze the graphics device.
#'
#' Two parameters control the cell budget:
#' * `cell_size` (semantic) — set this to aggregate multiple data-units into
#'   one cell (e.g. `cell_size = 1e6` makes each cell represent one million,
#'   so `y = 25e6` produces 25 cells instead of 25,000,000).
#' * `cell_count_cap` (defensive) — soft cap on total cells per panel
#'   (default `1e4`). When exceeded, the layer falls back to plain bars
#'   (one rectangle per bar, like [ggplot2::geom_col()]) and emits a warning.
#'   Pass `cell_count_cap = Inf` to disable.
#'
#' Reach for `cell_size` when your data is intrinsically large (populations,
#' currencies); `cell_count_cap` is the seatbelt for accidental large input.
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
#' # Dodged bars — cells shrink to width / n_groups under dodge
#' ggplot(mpg, aes(x = class, fill = drv)) +
#'   geom_unit_bar(position = "dodge") +
#'   coord_equal()
#'
#' # Dodge + square cells: compensate via the coord_equal() ratio so each
#' # sub-bar cell renders as a square.  `mpg$drv` has 3 levels, so pass
#' # ratio = n_groups / width = 3 / 0.9:
#' ggplot(mpg, aes(x = class, fill = drv)) +
#'   geom_unit_bar(position = "dodge") +
#'   coord_equal(ratio = 3 / 0.9)
#'
#' # Reversed stack
#' ggplot(mpg, aes(x = class, fill = drv)) +
#'   geom_unit_bar(position = position_stack(reverse = TRUE)) +
#'   coord_equal()
#'
#' # Asymmetric `cell_padding` under `coord_flip()`.  The length-2 vector
#' # c(vertical, horizontal) is interpreted in the stat's canonical
#' # orientation, so coord_flip() swaps which gap is visually vertical vs.
#' # horizontal.  Here: `c(0.1, 0.005)` gives generous vertical cell-to-cell
#' # gaps (which become horizontal after the flip) and tight cell-to-edge
#' # spacing (which becomes vertical after the flip).
#' ggplot(mpg, aes(x = class, fill = drv)) +
#'   geom_unit_bar(width = 1, cell_padding = c(0.1, 0.005)) +
#'   coord_flip()
#'
#' # Large data + coord_equal(): meet `cell_size` and `label_cells()`
#' # ------------------------------------------------------------------
#' # `coord_equal()` keeps cells visually square by forcing a 1:1 data-space
#' # aspect ratio.  That works on small data (mpg has ~7 categories and
#' # counts up to ~60, so x and y are the same order of magnitude) but
#' # breaks on large data.  `diamonds` has 53,940 rows and the tallest
#' # stack reaches ~2,600, against an x-range (carat) of only ~5.
#' #
#' # 1. What it looks like by default — almost empty:
#' ggplot(diamonds, aes(x = carat, fill = cut)) +
#'   geom_unit_bar() +
#'   coord_equal()
#' # cell_count_cap (default 10,000) fires first and falls back to solid bars,
#' # but even solid bars are ~1px-wide slivers once coord_equal squeezes
#' # a 2,600-tall y-axis alongside a 5-wide x-axis.
#'
#' # 2. Fix the scale mismatch with `cell_size`.  Each cell now represents
#' #    500 observations, so the y-range collapses from ~2,600 to ~5 — now
#' #    comparable to the x-range:
#' ggplot(diamonds, aes(x = carat, fill = cut)) +
#'   geom_unit_bar(cell_size = 500) +
#'   coord_equal()
#'
#' # 3. Relabel the axis in cell counts with `label_cells()` so readers
#' #    can see "2 cells" rather than "1000 diamonds":
#' ggplot(diamonds, aes(x = carat, fill = cut)) +
#'   geom_unit_bar(cell_size = 500) +
#'   scale_y_continuous(labels = label_cells(500)) +
#'   coord_equal() +
#'   labs(y = "Diamonds (1 cell = 500)")
#'
#' # `cell_count_cap` remains the defensive seatbelt: even with `cell_size` set,
#' # it catches pathological inputs (e.g. an extra zero in `cell_size`) so
#' # the graphics device never drowns in rects.
geom_unit_bar <- make_constructor(
  GeomBarCells,
  stat = "count",
  position = "stack",
  just = 0.5,
  radius = grid::unit(0, "npc"),
  orientation = NA,
  cell_size = 1,
  cell_padding = 0.025,
  cell_count_cap = 1e4
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
  stat = "identity",
  position = "stack",
  just = 0.5,
  radius = grid::unit(0, "npc"),
  orientation = NA,
  cell_size = 1,
  cell_padding = 0.025,
  cell_count_cap = 1e4
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
  cell_size = 1,
  cell_padding = 0.025,
  cell_count_cap = 1e4
)


#' Axis labeller for unit-cell charts
#'
#' @description
#' A small helper for use with `scale_*_continuous(labels = ...)` when you've
#' set `cell_size` on a `geom_unit_*()` layer and want the axis to show the
#' *number of cells* rather than the underlying data values.
#'
#' Returns a closure that divides each axis break by `cell_size`. Pair the
#' value passed here with the `cell_size` you passed to the geom.
#'
#' @param cell_size The same `cell_size` value passed to [geom_unit_bar()] /
#'   [geom_unit_col()] / [geom_unit_histogram()]. Must be a positive finite
#'   scalar.
#'
#' @return A function suitable for the `labels` argument of
#'   [ggplot2::scale_y_continuous()] / [ggplot2::scale_x_continuous()].
#'
#' @seealso [geom_unit_bar()] for the geoms that consume `cell_size`.
#'
#' @export
#' @examples
#' library(ggplot2)
#' df <- data.frame(country = c("A", "B", "C"), pop = c(2.4e6, 1.1e6, 3.8e6))
#' ggplot(df, aes(country, pop)) +
#'   geom_unit_col(cell_size = 1e6) +
#'   scale_y_continuous(labels = label_cells(1e6)) +
#'   labs(y = "People (millions; one cell = 1e6)")
label_cells <- function(cell_size = 1) {
  if (
    !is.numeric(cell_size) ||
      length(cell_size) != 1L ||
      is.na(cell_size) ||
      !is.finite(cell_size) ||
      cell_size <= 0
  ) {
    cli::cli_abort(c(
      "{.arg cell_size} must be a positive finite scalar number.",
      "x" = "Got {.val {cell_size}}."
    ))
  }
  function(x) x / cell_size
}
