# Deferred gTree wrapper for unit cells, so that `radius` can be clamped
# against the rendered cell dimensions inside `makeContent()`. At
# `draw_panel` time the panel pt size is not yet known; deferring lets
# `grid::convertHeight()` / `convertWidth()` resolve relative units in
# the panel viewport. The same trick is needed for legend-key cells,
# where the key viewport is also unknown at `draw_key_unit()` time --
# pass `quiet = TRUE` there so the legend doesn't echo a cap message
# the panel cells already (or will) emit.
#' @noRd
#' @keywords internal
.unit_cell_grob <- function(cells_glist, quiet = FALSE) {
  grid::gTree(
    cells_glist = cells_glist,
    quiet = quiet,
    cl = "unit_cell_grob"
  )
}

#' @export
makeContent.unit_cell_grob <- function(x) {
  cells <- .clamp_roundrect_radius(
    x$cells_glist,
    arg = "radius",
    quiet = isTRUE(x$quiet)
  )
  grid::setChildren(x, cells)
}


#' Key glyph for unit bar charts
#'
#' @description
#' The default legend key for [geom_unit_bar()] / [geom_unit_col()] /
#' [geom_unit_histogram()].  Mirrors the geom's orientation so the
#' legend reads as a miniature of the rendered bar:
#'
#' * vertical bars (`flipped_aes = FALSE`, the default) -> two cells
#'   stacked vertically with a single horizontal gap between them, no
#'   vertical gap.
#' * horizontal bars (`flipped_aes = TRUE`, e.g. `orientation = "y"` or
#'   `coord_flip()`) -> two cells placed side by side with a single
#'   vertical gap between them, no horizontal gap.
#'
#' @inheritParams ggplot2::draw_key
#' @return A grid grob.
#' @export
#' @keywords internal
draw_key_unit <- function(data, params, size) {
  fill <- ggplot2::fill_alpha(data$fill %||% "grey30", data$alpha)
  col <- data$colour %||% NA
  lwd <- data$linewidth %||% 0.5
  lty <- data$linetype %||% 1

  flipped <- isTRUE(params$flipped_aes)

  # Two cells. Long axis matches the bar's value axis; the gap appears
  # only along the value axis (so the legend reads as a stack of units).
  #   flipped == FALSE -> cells stacked vertically, gap is horizontal.
  #   flipped == TRUE  -> cells side-by-side,      gap is vertical.
  if (flipped) {
    x <- grid::unit(c(0.275, 0.725), "npc")
    y <- grid::unit(0.5, "npc")
    width <- grid::unit(0.4, "npc")
    height <- grid::unit(0.85, "npc")
  } else {
    x <- grid::unit(0.5, "npc")
    y <- grid::unit(c(0.275, 0.725), "npc")
    width <- grid::unit(0.85, "npc")
    height <- grid::unit(0.4, "npc")
  }

  gp <- ggplot2::gg_par(fill = fill, col = col, lwd = lwd, lty = lty)

  # Mirror the geom's `radius`. Legend cells are tiny (typically ~4 × 8
  # mm), so most user-supplied radii would exceed half the smaller cell
  # dimension and look like circles -- clamp via
  # `.clamp_roundrect_radius()` to keep them readable. Pass `quiet =
  # TRUE` so the legend doesn't surface a cap message: the geom path
  # has already (or will) inform when the panel cells need clamping.
  radius <- .validate_radius(params$radius %||% grid::unit(0, "pt"))
  if (identical(as.numeric(radius), 0)) {
    return(grid::rectGrob(
      x = x,
      y = y,
      width = width,
      height = height,
      gp = gp
    ))
  }

  # `grid::roundrectGrob()` requires scalar x/y/width/height, so build one
  # grob per cell -- exactly two, mirroring the unit-chart "stack of
  # units" idea. One of x/y is length 2 (the cell-stack axis), the other
  # length 1 (the shared axis); iterate over `max(length(x), length(y))`
  # so we get two cells regardless of orientation. Defer the radius
  # clamp via `.unit_cell_grob()`: the legend-key viewport isn't
  # established at draw_key construction time, so `convertHeight/Width`
  # would resolve against the wrong viewport here. `quiet = TRUE` keeps
  # the legend silent -- the panel path is the canonical place for the
  # "max displayable radius" message.
  n_cells <- max(length(x), length(y))
  cells <- lapply(seq_len(n_cells), function(i) {
    grid::roundrectGrob(
      x = if (length(x) >= i) x[i] else x[1L],
      y = if (length(y) >= i) y[i] else y[1L],
      width = width,
      height = height,
      r = radius,
      gp = gp
    )
  })
  .unit_cell_grob(do.call(grid::gList, cells), quiet = TRUE)
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomUnitBar <- ggplot2::ggproto(
  "GeomUnitBar",
  ggplot2::GeomBar,

  extra_params = c(
    "just",
    "na.rm",
    "orientation",
    "radius",
    "cell_size",
    "cell_padding",
    "cell_count_cap"
  ),

  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    radius = grid::unit(0, "pt"),
    cell_size = 1,
    cell_padding = 0.05,
    cell_count_cap = 1e4,
    lineend = "butt",
    linejoin = "mitre"
  ) {
    radius <- .validate_radius(radius)

    # `cell_size` controls how many data-units one cell represents.  Must be
    # a positive finite scalar (Inf is meaningless -- would draw zero cells).
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

    # `cell_padding` is the inset per side, expressed as a fraction of the
    # cell's extent on that axis:
    #   - vertical padding is a fraction of `cell_size` (value-axis units per
    #     cell), so it stays visually consistent across `cell_size` settings.
    #   - horizontal padding is a fraction of the bar's width, so it stays
    #     visually consistent across bar widths.  (Anchoring it to
    #     `cell_size` would collapse bars to slivers when `cell_size` is
    #     large, because `cell_size` lives on the value axis and is unrelated
    #     to bar widths.)
    # Accepted shapes:
    #   - length 1, unnamed         -> same fraction on all sides
    #   - length 2, unnamed         -> c(vertical, horizontal)
    #   - length 1 or 2, named with names from {"vertical", "horizontal"}
    #                               -> position-independent; missing axis falls
    #                                  back to the default (0.05)
    # Each element must be finite, numeric, and in [0, 0.5) -- 0.5 or above
    # would collapse cells to zero width/height.  Numeric/range problems warn
    # and fall back to the default; naming problems hard-error because a
    # typo'd name (e.g., "vert") would otherwise silently use the default
    # for that axis -- harder to debug than a clear error.
    cp_default <- 0.05
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
          "i" = "Falling back to the default ({.val {cp_default}})."
        ),
        call = NULL
      )
      cell_padding <- cp_default
    }
    cp_names <- names(cell_padding)
    if (!is.null(cp_names)) {
      allowed <- c("vertical", "horizontal")
      if (any(cp_names == "")) {
        cli::cli_abort(
          c(
            "{.arg cell_padding} must be either fully named or fully unnamed.",
            "x" = "Got a vector with mixed named and unnamed elements."
          ),
          call = NULL
        )
      }
      bad <- setdiff(cp_names, allowed)
      if (length(bad)) {
        cli::cli_abort(
          c(
            "{.arg cell_padding} has unknown name{?s}: {.val {bad}}.",
            "i" = "Allowed names: {.val {allowed}}."
          ),
          call = NULL
        )
      }
      if (anyDuplicated(cp_names)) {
        cli::cli_abort(
          c(
            "{.arg cell_padding} has duplicated name{?s}: \\
             {.val {cp_names[duplicated(cp_names)]}}."
          ),
          call = NULL
        )
      }
      pad_v <- if ("vertical" %in% cp_names) {
        cell_padding[["vertical"]]
      } else {
        cp_default
      }
      pad_h <- if ("horizontal" %in% cp_names) {
        cell_padding[["horizontal"]]
      } else {
        cp_default
      }
    } else {
      if (length(cell_padding) == 1L) {
        cell_padding <- c(cell_padding, cell_padding)
      }
      pad_v <- cell_padding[1L] # vertical (stacking-axis) padding
      pad_h <- cell_padding[2L] # horizontal (bar-axis) padding
    }

    # `cell_count_cap` must be a positive scalar (or Inf to disable the cap).  Zero,
    # negative, NA, non-numeric, or non-scalar values are nonsensical -- warn
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
    # $ratio set, not a CoordFixed subclass -- detect via the ratio slot.
    if (inherits(coord, "CoordCartesian") && is.null(coord$ratio)) {
      cli::cli_inform(
        c(
          "{.fn geom_unit_*} works best with a fixed-ratio coordinate.",
          "i" = "Add {.code + coord_equal()} to keep cells square."
        ),
        call = NULL,
        # `once` = per R session; `regularly` was too aggressive (file
        # cache, ~8 hours across sessions) for a styling hint.
        .frequency = "once",
        .frequency_id = "ggpointless_unit_bar_coord_equal"
      )
    }

    # Flip to canonical orientation (y = value axis) for cell expansion
    flipped <- isTRUE(data$flipped_aes[1L])
    data <- ggplot2::flip_data(data, flipped)

    # Non-linear value-axis handling. `cell_size` is in data units, but
    # under e.g. `scale_x_log10()` the ymin/ymax we receive are in
    # transformed (panel) units. To preserve cell-count semantics ("1 cell
    # = cell_size observations") we tile in DATA space and forward-
    # transform each cell edge back to panel space at the end. The cells
    # then have non-uniform visual heights -- narrow toward high counts
    # under log10, etc. -- but the count contract is preserved.
    #
    # Locating the value scale in panel_params is a 2x2 XOR of two flips:
    #   * `flipped`        -- data-orientation flip (aes(y = …) / orientation = "y")
    #   * `coord_flipped`  -- panel-render flip swaps panel_params x/y
    # Either flip moves the value scale from panel_params$y to $x; both
    # together cancel out.
    coord_flipped <- inherits(coord, "CoordFlip")
    value_axis <- if (xor(flipped, coord_flipped)) "x" else "y"
    trans <- .get_scale_transformer(panel_params, value_axis)
    # `nonlinear` means "compresses ranges non-uniformly", i.e. a true
    # mathematical transform that affects cell sizes. `date`/`time`/`hms`
    # round-trip numeric through Date/POSIXct (their `inv()` returns a
    # non-numeric object that breaks downstream arithmetic) but do not
    # compress; `reverse` only inverts sign. Treat all four as identity
    # so cells tile in panel space, same as before.
    trivial_transforms <- c("identity", "reverse", "date", "time", "hms")
    nonlinear <- !trans$name %in% trivial_transforms
    d_ymin <- if (nonlinear) trans$inv(data$ymin) else data$ymin
    d_ymax <- if (nonlinear) trans$inv(data$ymax) else data$ymax

    # Soft cap on total cells per panel.  Cell expansion allocates one rect
    # per cell; a single bar with y = 1e6 (and default cell_size = 1) would
    # freeze the graphics device.  When exceeded, fall back to plain rect
    # rendering (one rect per bar).  Pass `cell_count_cap = Inf` to opt out, or set
    # `cell_size` so each cell aggregates more units.
    eps_n <- 1e-9
    raw_h <- (d_ymax - d_ymin) / cell_size
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
        call = NULL
      )
      # Fallback should match what the cells WOULD have rendered.  Cells
      # are inset horizontally by `pad_h * (xmax - xmin)` per side, so the
      # visible cell width is `(1 - 2 * pad_h)` of the slot.  Apply the
      # same shrink to the solid-bar fallback so adjacent bars keep the
      # same visible spacing as in the cell-rendering path -- and so the
      # default (`width = 1`, `cell_padding = 0.05`) renders at the same
      # 0.9 visible width as `geom_col()` without depending on ggplot2's
      # default width slot.
      bar_inset <- (data$xmax - data$xmin) * pad_h
      data$xmin <- data$xmin + bar_inset
      data$xmax <- data$xmax - bar_inset
      data <- ggplot2::flip_data(data, flipped)
      return(ggplot2::GeomRect$draw_panel(
        data,
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin
      ))
    }

    # Expand each bar segment into cells of size `cell_size` (in DATA space)
    # plus one partial cell at the outer end if the segment height is not a
    # multiple of `cell_size`.
    #  - Positive segments (d_ymin >= 0): tile upward from ymin, partial at ymax.
    #  - Negative segments (d_ymax <= 0): tile downward from ymax, partial at ymin.
    # Under a non-linear value scale (`nonlinear == TRUE`), tiling happens
    # in data space and each cell edge is forward-transformed back to
    # panel space at the end.  Vertical padding is also applied in data
    # space (it's a fraction of `cell_size`, a data-space quantity) so it
    # composes correctly under the transform.
    #
    # Implementation: fully vectorised. Previously the expansion ran one
    # `data[i, , drop = FALSE]` slice per row in an `lapply`, which is
    # 14-80x slower than building a single per-cell index vector and
    # slicing once. Equivalence vs the per-row version is checked
    # against the linear and non-linear vdiffr baselines under
    # `tests/testthat/_snaps/geom-unit-bar/`.
    eps <- 1e-9
    raw_units_all <- (d_ymax - d_ymin) / cell_size
    valid <- is.finite(d_ymin) &
      is.finite(d_ymax) &
      is.finite(raw_units_all) &
      raw_units_all > eps

    if (!any(valid)) {
      return(grid::nullGrob())
    }

    raw_units <- raw_units_all[valid]
    n_full <- as.integer(floor(raw_units))
    partial <- raw_units - n_full
    n_cells_per_row <- n_full + as.integer(partial > eps)

    # Defensive: if a row ended up with zero cells (shouldn't happen
    # given `valid` filter, but pathological floating-point inputs
    # could) drop it.
    keep <- n_cells_per_row > 0L
    if (!any(keep)) {
      return(grid::nullGrob())
    }
    valid_rows <- which(valid)[keep]
    n_cells_per_row <- n_cells_per_row[keep]

    # Per-cell expansion index: each source row appears `n_cells_per_row`
    # times in order. `sequence(n_cells_per_row) - 1L` gives the per-cell
    # k counter (0..n_cells - 1) restarting at each new source row.
    rep_idx <- rep.int(valid_rows, n_cells_per_row)
    k <- sequence(n_cells_per_row) - 1L
    expanded <- data[rep_idx, , drop = FALSE]

    d_min_rep <- d_ymin[rep_idx]
    d_max_rep <- d_ymax[rep_idx]
    downward <- (d_ymax <= eps)[rep_idx]

    # Compute per-cell data-space edges. Use boolean-indexed assignment
    # rather than `ifelse()` (faster, no double-evaluation of both
    # branches; matches the rest of the package's vectorisation style).
    n_cells <- length(rep_idx)
    d_min_cells <- numeric(n_cells)
    d_max_cells <- numeric(n_cells)

    up <- !downward
    if (any(up)) {
      d_min_cells[up] <- d_min_rep[up] + k[up] * cell_size
      d_max_cells[up] <- pmin(d_min_cells[up] + cell_size, d_max_rep[up])
    }
    if (any(downward)) {
      d_max_cells[downward] <- d_max_rep[downward] - k[downward] * cell_size
      d_min_cells[downward] <- pmax(
        d_max_cells[downward] - cell_size,
        d_min_rep[downward]
      )
    }

    # Vertical padding.  Two regimes:
    #
    # * Linear value scale (the dominant case): apply padding in DATA
    #   space.  Equal data-units of padding on every side of every
    #   cell.  Partial cells at the bar tip get the SAME absolute
    #   padding as full cells, which makes them visually distinct (less
    #   fill area).  This is the documented "all full cells render at
    #   the same size" contract.
    #
    # * Non-linear value scale (`log10`, `sqrt`, `log1p`, ...): apply
    #   padding in PANEL space, proportional to each cell's panel
    #   extent.  Under log10 the leftmost cell occupies ~half the panel
    #   while the rightmost is a sliver -- so a constant data-space
    #   inset becomes 39 % of the leftmost cell and 5 % of the
    #   rightmost.  Proportional padding restores visual consistency
    #   (every cell gets the same fractional gap regardless of where
    #   it sits on the compressed axis).
    #
    # Both regimes cap padding at 40 % per side to prevent collapse.
    if (nonlinear) {
      p_min <- trans$fwd(d_min_cells)
      p_max <- trans$fwd(d_max_cells)
      cell_h_panel <- p_max - p_min
      g_eff_v <- pmin(pad_v * cell_h_panel, cell_h_panel * 0.4)
      expanded$ymin <- p_min + g_eff_v
      expanded$ymax <- p_max - g_eff_v
    } else {
      pad_v_abs <- pad_v * cell_size
      cell_h_data <- d_max_cells - d_min_cells
      g_eff_v <- pmin(pad_v_abs, cell_h_data * 0.4)
      expanded$ymin <- d_min_cells + g_eff_v
      expanded$ymax <- d_max_cells - g_eff_v
    }

    data <- expanded
    rownames(data) <- NULL

    # Horizontal (bar-axis) inset.  Anchored to the bar's actual width
    # rather than `cell_size` (which lives on the value axis and is
    # unrelated to bar widths -- a large `cell_size` would otherwise
    # collapse bars to a sliver).  Capped at 40 % of the bar width per
    # side so cells can't collapse to negative width.  Vertical padding
    # was applied per-cell in data space above.
    cell_w <- data$xmax - data$xmin
    pad_h_abs <- pad_h * cell_w
    g_eff_h <- pmin(pad_h_abs, cell_w * 0.4)
    data$xmin <- data$xmin + g_eff_h
    data$xmax <- data$xmax - g_eff_h

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
    rr_linejoin <- .roundrect_linejoin(radius, linejoin)
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
          lty = coords$linetype[i],
          linejoin = rr_linejoin
        )
      )
    })
    .unit_cell_grob(do.call(grid::gList, gl))
  },

  draw_key = draw_key_unit
)


#' Unit Bar Charts
#'
#' @description
#' Unit bar charts represent data as discrete cells, where each cell represents
#' one unit of data. They follow the same `x`/`y` conventions as [ggplot2::geom_bar()],
#' [ggplot2::geom_col()], and [ggplot2::geom_histogram()]:
#'
#' * `geom_unit_bar()` counts observations (one row = one cell), like
#'   [ggplot2::geom_bar()]. Map `x` (or `y` for horizontal bars) to the
#'   grouping variable; `fill` to colour by a second variable.
#' * `geom_unit_col()` uses pre-computed `y` values, like [ggplot2::geom_col()].
#'   Fractional values are supported: `y = 3.7` draws 3 full unit cells
#'   (height 1 in data space) plus a partial cell of height 0.7 at the top.
#'
#' For binning continuous data, see [geom_unit_histogram()].
#'
#' All position adjustments supported by [ggplot2::geom_bar()] work here:
#' `"stack"` (default), `"dodge"`, `"fill"`,
#' `position_stack(reverse = TRUE)`, etc. Although  `"fill"` rarely makes sense
#' for these geoms; see the examples below for why.
#'
#' @concept unit chart
#' @concept isotype chart
#'
#' @aesthetics GeomUnitBar
#' @aesthetics StatCount
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
#' @param width Bar width in data units. Default `1`. With the package
#'   defaults (`width = 1`, `cell_size = 1`), `coord_equal()` already
#'   renders cells as squares. For non-default `width` or `cell_size`, see
#'   the `position = "dodge"` subsection below for the general
#'   `coord_equal(ratio = ...)` formula (it also covers the non-dodge
#'   `n_groups = 1` case). Same as [ggplot2::geom_bar()].
#' @param radius Corner radius for each cell as a [grid::unit()]. Default
#'   `grid::unit(0, "pt")` gives sharp corners. Only used with linear
#'   coordinates; non-linear coordinates (e.g. [ggplot2::coord_polar()])
#'   fall back to sharp corners.
#' @param cell_size Number of data units one cell represents. Default `1`
#'   (one cell per unit, the original "isotype" / pictogram pattern). Set
#'   to a larger value to aggregate units, e.g. `cell_size = 1e4` so each
#'   cell stands for one thousand. Each cell is then `cell_size` tall in
#'   data space. With the package defaults (`width = 1`, `cell_size = 1`)
#'   `coord_equal()` already renders cells as squares; for non-default
#'   `cell_size` (or under `position = "dodge"`) the `coord_equal(ratio)`
#'   must scale with `cell_size` — see the `position = "dodge"` subsection
#'   below for the formula. The value axis still shows the original data
#'   values; pair with `scale_*_continuous(labels = label_cells(cell_size))`
#'   to show cell counts instead.
#' @param cell_padding Inset applied per side of each cell, in CSS `padding`
#'   style. On linear value scales the vertical inset is a fraction of
#'   `cell_size` (data space); on non-linear value scales (`log10`,
#'   `sqrt`, ...) it becomes a fraction of each cell's *panel* extent so
#'   the gap looks visually uniform under compression -- see "Log and
#'   other non-linear value scales" below. The horizontal inset is
#'   always a fraction of the bar's `width` (the bar axis is never the
#'   transformed one). Labels are in canonical (vertical-bar)
#'   orientation; under `orientation = "y"` or `coord_flip()` the
#'   on-screen roles swap, but element 1 always pads the value axis and
#'   element 2 always pads the bar axis.
#'   * length 1 (default `0.05`) -- same fraction on all four sides.
#'   * length 2, unnamed -- `c(vertical, horizontal)`; `vertical` is the
#'     inset between vertically-stacked cells, `horizontal` is the inset
#'     between each cell and its bar's left/right edge.
#'   * named (length 1 or 2) -- positional independence; allowed names
#'     are `"vertical"` and `"horizontal"`. A missing axis falls back to
#'     the default `0.05`. So `c(horizontal = 0.2, vertical = 0.1)`,
#'     `c(vertical = 0.1, horizontal = 0.2)`, and `c(0.1, 0.2)` are all
#'     equivalent. Unknown names error rather than silently default --
#'     a typo would otherwise be hard to spot in the rendered plot.
#'
#'   Each element must be finite and in `[0, 0.5)`. Set `cell_padding = 0`
#'   for cells that touch (the borderless isotype style); increase it for
#'   a waffle-like grid of separated cells. The inset is applied uniformly
#'   to every cell, including the cells at the bar's outer edges -- each
#'   cell represents one data unit, so cells must render at identical size
#'   regardless of whether they sit at the floor, in the middle, or at the
#'   top of a bar. As a consequence the bar's outer edges sit slightly
#'   inside the data extent: by `cell_padding * cell_size` vertically and
#'   `cell_padding * width` horizontally on linear scales, and
#'   proportionally less on non-linear value scales (where the inset is
#'   panel-proportional rather than data-proportional).
#' @param cell_count_cap Soft cap on the total number of cells drawn per
#'   panel. A defensive safety net: this geom renders one grob per cell, so
#'   very large `y` values can freeze the graphics device. When the cap is
#'   exceeded, the layer falls back to solid bars (one rectangle per bar)
#'   and emits a warning. Default `1e4`; pass `Inf` to disable. For large
#'   `y` you might want to set a larger `cell_size`, see *Examples*.
#' @param lineend Line end style for the cell border when `colour` is set.
#'   One of `"round"`, `"butt"` (default), or `"square"`. Same as
#'   [ggplot2::geom_bar()].
#' @param linejoin Line join style for the cell border. One of `"round"`,
#'   `"mitre"` (default), or `"bevel"`. Same as [ggplot2::geom_bar()].
#' @param na.rm If `FALSE` (default), rows with missing `y` are dropped
#'   with a warning. Non-positive `y` values are kept: `y = 0` produces an
#'   empty segment, and `y < 0` stacks cells downward from the baseline.
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
#' A few details are easy to overlook. See the *Caveats worth knowing*
#' section of `vignette("ggpointless", package = "ggpointless")` for worked
#' examples and visuals.
#'
#' * `position = "fill"` collapses every stack to a single cell (the unit
#'   semantics disappear). Use `"stack"` or `"dodge"` instead.
#' * `position = "dodge"` shrinks each sub-bar to `width / n_groups`. To
#'   restore square cells, pair with
#'   `coord_equal(ratio = width / (n_groups * cell_size))` for vertical bars,
#'   or the inverse `ratio = n_groups * cell_size / width` for horizontal.
#'   With `preserve = "single"`, `n_groups` is the **max groups per cluster**,
#'   not `nlevels(fill)`.
#' * Non-linear value scales (`log10`, `sqrt`, ...): cells tile in
#'   **data space**, so the "1 cell = `cell_size` observations" contract is
#'   preserved. Cell heights become non-uniform under compression.
#' * Tiny panels: the default 5 % gap can collapse below 1 px and cells
#'   visually fuse. Enlarge the panel or reduce `cell_size`.
#' * Polar coordinates: cells become annular segments. Rounded corners
#'   are dropped under polar (see `radius`).
#'
#' @section Performance:
#'
#' Cost scales with total cell count, not input rows — one grid rect per
#' cell. The defensive `cell_count_cap = 1e4` falls back to plain bars
#' when exceeded; pass `Inf` to disable. For intrinsically large data
#' (populations, currencies, ...), set `cell_size` to aggregate units
#' into single cells. Rounded corners (`radius > 0`) add a `roundrectGrob`
#' per cell and are the most expensive path; leave `radius` at its default
#' for large plots.
#'
#' @seealso [ggplot2::geom_bar()] and [ggplot2::geom_col()] for the regular
#'   (non-unit) counterparts. [geom_unit_histogram()] for binning continuous
#'   data.
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' # Basic example: count observations with geom_unit_bar()
#' p <- ggplot(mtcars, aes(reorder(cyl, cyl, length))) +
#'   labs(y = NULL)
#' p + geom_unit_bar()
#'
#' # Let's make cells look square by adding coord_equal()
#' p <- p + coord_equal()
#' p + geom_unit_bar()
#'
#' # Rounded corners are supported too
#' p + geom_unit_bar(radius = unit(5, "pt"))
#'
#' # When a variable is mapped to fill
#' # aesthetic, bars are stacked by default
#' p + geom_unit_bar(aes(fill = factor(vs)))
#'
#' # But you might want bars to be dodged
#' p +
#'   geom_unit_bar(
#'     aes(fill = factor(vs)),
#'     position = position_dodge(preserve = "single")
#'   ) +
#'   coord_equal(ratio = 1 / 2)
#'
#' # Dodging + facets: getting the coord ratio right.
#' # With `preserve = "single"` every sub-bar is sized to
#' # `width / max_groups_per_cluster` -- the largest number of fill levels
#' # appearing at any *one* x-cluster, NOT the total nlevels(fill). In
#' # `penguins` `fill = species` has three levels, but each island holds
#' # at most two species (Biscoe: Adelie + Gentoo; Dream: Adelie + Chinstrap;
#' # Torgersen: Adelie only), so the effective n_groups is 2.
#' # The square-cell formula for horizontal bars is
#' # `ratio = n_groups * cell_size / width`, hence `ratio = 2 * 1 / 1 = 2`
#' # (not 3, which is what nlevels(fill) would suggest).
#' if (getRversion() >= "4.5.0") {
#'   p2 <- ggplot(datasets::penguins, aes(y = island))
#'   p2 +
#'     geom_unit_bar(
#'       aes(fill = species),
#'       radius = unit(1, "pt"),
#'       position = position_dodge(preserve = "single"),
#'       colour = "#333333",
#'       na.rm = TRUE
#'     ) +
#'     labs(x = NULL, y = NULL) +
#'     facet_wrap(~year, ncol = 1) +
#'     # max 2 species per island -> ratio = 2, not 3
#'     coord_equal(ratio = 2) +
#'     theme(legend.position = "bottom")
#' }
#'
#' # Note: position dodge2 adds extra padding by default, but provides
#' # an option to set this to 0; use the cell_padding argument
#' # instead for full control of vertical and horizontal padding
#' p +
#'   geom_unit_bar(
#'     aes(fill = factor(vs)),
#'     position = position_dodge2(preserve = "single", padding = 0),
#'     cell_padding = c(0.025, 0.1)
#'   ) +
#'   coord_equal(ratio = 1 / 2)
#'
#' # Increase the cell padding (default is 0.05)
#' p + geom_unit_bar(cell_padding = c(
#'   "vertical"   = 0.1,
#'   "horizontal" = 0.05
#'   )
#' )
#'
#' # When you map the categorical to y aesthetic,
#' # the orientation is auto-detected
#' ggplot(mtcars, aes(y = reorder(cyl, cyl, length))) +
#'   geom_unit_bar() +
#'   coord_equal()
#'
#' # `scale_*_binned()` belongs on the *mapped continuous variable*, not on the
#' # count axis. Bin a continuous variable into discrete intervals, then count
#' # observations per bin -- a unit-cell histogram in two lines:
#' ggplot(iris, aes(y = Sepal.Length)) +
#'   # the continuous variable (Sepal.Length) lives on y; binning it ...
#'   geom_unit_bar() +
#'   # ... discretises y into intervals so `stat = "count"` can tally each one.
#'   scale_y_binned()
#' # Using `scale_y_binned()` on the count axis instead would render an empty
#' # plot -- the count axis is already discrete via `stat_count`, so binning
#' # it again has nothing to bin.
#'
geom_unit_bar <- make_constructor(
  GeomUnitBar,
  stat = "count",
  position = "stack",
  just = 0.5,
  radius = grid::unit(0, "pt"),
  orientation = NA,
  width = 1,
  cell_size = 1,
  cell_padding = 0.05,
  cell_count_cap = 1e4
)


#' @rdname geom_unit_bar
#' @export
#' @examples
#'
#' # Plot pre-computed counts with geom_unit_col() (like geom_col() does)
#' # by default 1 cell represents 1 observation
#' df <- data.frame(x = c("A", "B", "C"), y = c(10, 12, 8))
#' ggplot(df, aes(x, y)) + geom_unit_col()
#'
#' # Too many cells might freeze the graphics device. When cell_count_cap
#' # is exceeded, the geom falls back to its ggplot2 sibling with a warning.
#' # For large y, divide at the aes level (e.g. `aes(x, y / 1e3)`) so each
#' # cell represents a meaningful number of observations.
#' df <- data.frame(x = c("A", "B", "C"), y = c(10000, 12000, 8000))
#' ggplot(df, aes(x, y)) + geom_unit_col()
#'
#' # The aes-level division pattern:
#' cs <- 1000
#' ggplot(df, aes(x, y / cs)) +
#'   geom_unit_col() +
#'   labs(caption = sprintf("Each cell represents %d observations", cs)) +
#'   coord_equal()
#'
#' # Flat cells with rounded corners via coord_equal(ratio = ...)
#' ggplot(df, aes(x, y / cs)) +
#'   geom_unit_col(radius = unit(5, "pt")) +
#'   labs(caption = sprintf("Each cell represents %d observations", cs)) +
#'   coord_equal(ratio = 1 / 10)
#'
geom_unit_col <- make_constructor(
  GeomUnitBar,
  stat = "identity",
  position = "stack",
  just = 0.5,
  radius = grid::unit(0, "pt"),
  orientation = NA,
  width = 1,
  cell_size = 1,
  cell_padding = 0.05,
  cell_count_cap = 1e4
)


#' Axis labeller for unit-cell charts
#'
#' @description
#' A thin wrapper around [scales::label_number()] anchored to a
#' `cell_size`: divides each axis break by `cell_size` and formats the
#' result. Use with `scale_*_continuous(labels = ...)` when the
#' corresponding `geom_unit_*()` layer was given a non-default
#' `cell_size` and you want the axis to read in *cell counts* (or in a
#' natural-unit scale like thousands / millions, via `suffix`).
#'
#' Because `label_cells()` is a wrapper, every option that
#' [scales::label_number()] accepts (`accuracy`, `big.mark`,
#' `decimal.mark`, `scale_cut`, `style_positive`, ...) is available via
#' `...`.
#'
#' @param cell_size The same `cell_size` value passed to [geom_unit_bar()] /
#'   [geom_unit_col()] / [geom_unit_histogram()]. Must be a positive
#'   finite scalar. Translated internally to `scale = 1 / cell_size`.
#' @param prefix,suffix Character strings to wrap each label. Default
#'   `""` (no decoration). Useful when `cell_size` matches a natural
#'   unit -- e.g. `cell_size = 1e3` with `suffix = "k"` produces
#'   `"1k"`, `"2k"`, ...; `cell_size = 1e6` with `suffix = "M"`
#'   produces `"1M"`, `"3M"`, ...
#' @param ... Other arguments forwarded to [scales::label_number()] --
#'   e.g. `accuracy = 0.1`, `big.mark = ","`,
#'   `scale_cut = scales::cut_short_scale()` (auto SI prefix).
#'
#' @return A function suitable for the `labels` argument of
#'   [ggplot2::scale_y_continuous()] / [ggplot2::scale_x_continuous()].
#'
#' @seealso [scales::label_number()] for the underlying formatter and the
#'   full list of forwardable options; [geom_unit_bar()] for the geoms
#'   that consume `cell_size`.
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' # cell_size = 1,000 -> axis reads "1k", "2k", ... (one cell = 1,000)
#' df_k <- data.frame(x = c("A", "B", "C"), y = c(4000, 11000, 8000))
#' ggplot(df_k, aes(x, y)) +
#'   geom_unit_col(cell_size = 1e3) +
#'   scale_y_continuous(labels = label_cells(1e3, suffix = "k")) +
#'   labs(
#'     x = NULL,
#'     y = NULL,
#'     caption = "One cell equals 1,000 observations.") +
#'   coord_equal(ratio = 1 / 1e3)
#'
#' # cell_size = 1,000,000 -> axis reads "1M", "3M", ... (one cell = 1,000,000)
#' # Flipped orientation: bars run along x, baselines on y.
#' df_M <- data.frame(x = c("A", "B", "C"), y = c(2.4e6, 1.1e6, 3.8e6))
#' ggplot(df_M, aes(y = x, x = y)) +
#'   geom_unit_col(cell_size = 1e6) +
#'   scale_x_continuous(labels = label_cells(1e6, suffix = "M")) +
#'   labs(
#'     x = NULL,
#'     y = NULL,
#'     caption = "One cell equals 1,000,000 observations.") +
#'   coord_equal(ratio = 1e6)
label_cells <- function(cell_size = 1, prefix = "", suffix = "", ...) {
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
  # prefix / suffix validation: scales::label_number does its own
  # checks but the errors there don't name our argument; raise here
  # so users see "{.arg prefix}" pointing at this signature.
  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix)) {
    cli::cli_abort(c(
      "{.arg prefix} must be a single non-NA string.",
      "x" = "Got {.val {prefix}}."
    ))
  }
  if (!is.character(suffix) || length(suffix) != 1L || is.na(suffix)) {
    cli::cli_abort(c(
      "{.arg suffix} must be a single non-NA string.",
      "x" = "Got {.val {suffix}}."
    ))
  }
  scales::label_number(
    scale = 1 / cell_size,
    prefix = prefix,
    suffix = suffix,
    ...
  )
}
