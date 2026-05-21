# Per-ridge component grob. Holds the fade (fill gradient) grob separately
# from the outline polyline and the opaque polygon shape, so that the
# panel-level container can mask outlines across overlapping ridges
# (back-ridge outlines bleeding through front-ridge transparent
# baselines is the visual artefact this targets).
#
# When rendered standalone (no panel container, e.g. direct test calls),
# `makeContent` just stacks fade + outline -- no cross-ridge masking is
# possible without the sibling shapes.
#' @noRd
#' @keywords internal
.ridge_components_grob <- function(
  fade_grob,
  outline_grob = NULL,
  mask_shape = NULL
) {
  grid::gTree(
    fade_grob = fade_grob,
    outline_grob = outline_grob,
    mask_shape = mask_shape,
    cl = "ridge_components_grob"
  )
}

#' @export
makeContent.ridge_components_grob <- function(x) {
  children <- if (is.null(x$outline_grob)) {
    grid::gList(x$fade_grob)
  } else {
    grid::gList(x$fade_grob, x$outline_grob)
  }
  grid::setChildren(x, children)
}


# Panel-level container for overlapping ridges. Defers the cross-ridge
# outline-masking decision to draw time, so devices without Porter-Duff
# `dest.out` (base `pdf()`, `postscript()`) fall back gracefully to the
# unmasked stacking. Order in `ridges` is back-to-front (the order
# `draw_panel` accumulates them).
#' @noRd
#' @keywords internal
.ridgeline_panel_grob <- function(ridges) {
  grid::gTree(
    ridges = ridges,
    cl = "ridgeline_panel_grob"
  )
}

#' @export
makeContent.ridgeline_panel_grob <- function(x) {
  ridges <- x$ridges
  n <- length(ridges)
  if (n == 0L) {
    return(grid::setChildren(x, grid::gList()))
  }

  dev_caps <- grDevices::dev.capabilities()
  dev_name <- names(grDevices::dev.cur())
  unsafe_dev <- dev_name %in% c("pdf", "postscript")
  can_composite <- !unsafe_dev && .has_compositing_op("dest.out", dev_caps)

  # Inform the user when we'd have masked outlines but the device
  # cannot composite -- only when at least two ridges have outlines
  # (else there's nothing to mask either way). Uses the shared
  # consolidator so faceted plots emit one message per render.
  if (!can_composite) {
    n_outlines <- sum(vapply(
      ridges,
      function(r) !is.null(r$outline_grob),
      logical(1)
    ))
    if (n_outlines >= 2L) {
      .queue_ridgeline_outline_no_mask()
    }
  }

  out <- list()
  for (k in seq_len(n)) {
    r <- ridges[[k]]
    fade <- r$fade_grob
    outline <- r$outline_grob

    # No outline to mask, or front-most ridge (nothing drawn on top), or
    # device cannot do `dest.out` -- stack fade + outline as-is.
    if (is.null(outline) || k == n || !can_composite) {
      if (!is.null(fade)) {
        out[[length(out) + 1L]] <- fade
      }
      if (!is.null(outline)) {
        out[[length(out) + 1L]] <- outline
      }
      next
    }

    # Build the "above-mask": opaque polygon shapes of ridges drawn
    # AFTER this one (drawing order is back-to-front, so later index =
    # in front). dest.out erases the back-ridge outline within those
    # shapes -- only outlines, not fills (those compose normally), and
    # not the panel grid behind the transparent baselines.
    above <- lapply(
      ridges[(k + 1L):n],
      function(rk) rk$mask_shape
    )
    above <- Filter(Negate(is.null), above)
    if (!length(above)) {
      if (!is.null(fade)) {
        out[[length(out) + 1L]] <- fade
      }
      out[[length(out) + 1L]] <- outline
      next
    }
    mask_grob <- grid::gTree(children = do.call(grid::gList, above))
    masked_outline <- grid::groupGrob(
      mask_grob,
      op = "dest.out",
      dst = outline
    )
    if (!is.null(fade)) {
      out[[length(out) + 1L]] <- fade
    }
    out[[length(out) + 1L]] <- masked_outline
  }
  grid::setChildren(x, do.call(grid::gList, out))
}


#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @include geom-area-fade.R
#' @export
GeomRidgelineFade <- ggplot2::ggproto(
  "GeomRidgelineFade",
  ggplot2::GeomRibbon,

  required_aes = c("x", "y", "height"),

  # Inherit `default_aes` from `GeomRibbon` (ggplot2 v4 sets fill /
  # linewidth / linetype via `from_theme()` for theme-driven defaults).
  # We do NOT mirror the parent block here: `col_mix()` is not exported
  # from ggplot2, so an unqualified mirror would break in our namespace.
  # `global_max_height` is a private per-row column managed by
  # `draw_layer` / `draw_panel`; ggplot2 does not strip such columns
  # between draw stages, so it does not need to be declared as an
  # aesthetic (cf. `GeomColFade$.scope_max_abs`, `GeomAreaFade$global_max_abs`).

  extra_params = c(
    "na.rm",
    "flipped_aes",
    "orientation",
    "alpha_fade_to",
    "alpha_scope"
  ),

  draw_key = .draw_key_area_fade,

  setup_params = \(self, data, params) {
    # Orientation detection. ggplot2's `has_flipped_aes` doesn't fit
    # ridgelines: its `group_has_equal` heuristic has the opposite
    # convention (constant-y-per-group → flipped, where ridgelines want
    # constant-y-per-group → canonical). Roll our own:
    #
    #   * Explicit `orientation = "y"` / `"x"` wins.
    #   * Otherwise: the baseline axis is the one CONSTANT within each
    #     group. If x is constant per group, x is the baseline (flipped).
    #     If y is constant per group, y is the baseline (canonical).
    #     If neither or both — keep default canonical (FALSE).
    if (!is.null(params$orientation) && !is.na(params$orientation)) {
      params$flipped_aes <- isTRUE(params$orientation == "y")
    } else if (
      nrow(data) > 0L &&
        all(c("group", "x", "y") %in% names(data))
    ) {
      y_per_grp <- vapply(
        split(data$y, data$group),
        \(v) length(unique(v)),
        integer(1)
      )
      x_per_grp <- vapply(
        split(data$x, data$group),
        \(v) length(unique(v)),
        integer(1)
      )
      x_constant <- all(x_per_grp == 1L)
      y_constant <- all(y_per_grp == 1L)
      params$flipped_aes <- isTRUE(x_constant && !y_constant)
    } else {
      params$flipped_aes <- isTRUE(params$flipped_aes)
    }
    # Vocabulary aligned with `GeomAreaFade` (2026-04-27): the legacy
    # `"area"` is renamed to `"group"` (per ridge / per `data$group`),
    # and the legacy per-y-baseline `"group"` mode is dropped because
    # it was reachable only through unusual `aes(group = ...)` overrides
    # and `"global"` covers that case acceptably.
    params <- .fade_setup_params(
      params,
      scopes = c("group", "global"),
      default_scope = "group"
    )
    .check_outline_type(params$outline.type)
    params
  },

  # Stamp flipped_aes per row so PositionRidgeline (which sees data but
  # not the geom's params) and draw_panel / draw_group can branch on
  # orientation. Data stays in user view throughout -- xmin/xmax are
  # populated when flipped; ymin/ymax when canonical (see
  # PositionRidgeline$compute_panel).
  setup_data = \(self, data, params) {
    data$flipped_aes <- isTRUE(params$flipped_aes)
    data$.alpha_scope <- params$alpha_scope %||% "group"

    # Degenerate-group guard. GeomRibbon needs >=2 unique running-axis
    # values per group to draw a polygon. If every group has exactly 1
    # unique running-axis value the ribbon collapses to a zero-width
    # band -- silent empty plot. The running axis is x by default, y
    # when flipped.
    running_col <- if (isTRUE(params$flipped_aes)) "y" else "x"
    if (
      nrow(data) > 0L &&
        "group" %in% names(data) &&
        running_col %in% names(data)
    ) {
      n_per_group <- stats::ave(
        data[[running_col]],
        data$group,
        FUN = \(v) length(unique(v))
      )
      degenerate <- sum(n_per_group < 2L) == nrow(data)
      # Only emit when EVERY group is degenerate -- otherwise the user
      # has a mix of (e.g.) intentional 2-pt mini-ridges and we shouldn't
      # nag. The full-degenerate case is the one that produces a silent
      # empty plot.
      if (degenerate) {
        cli::cli_warn(
          c(
            "!" = "{.fn geom_ridgeline_fade}: every group has fewer than \\
                   two unique values on the running axis, so each ribbon \\
                   collapses to a zero-width band and the panel will \\
                   render empty.",
            "i" = 'The {.arg group} aesthetic should match the categorical \\
                   (baseline) axis, not the continuous (running) axis. \\
                   With {.code aes(x = continuous, y = categorical)} \\
                   use {.code group = categorical}; with the inverse \\
                   mapping, swap accordingly.'
          ),
          call = NULL,
          .frequency = "regularly",
          .frequency_id = "geom_ridgeline_fade_degenerate_groups"
        )
      }
    }

    data
  },

  # Cross-panel reference for `alpha_scope = "global"`. Same shape as the
  # GeomColFade/GeomAreaFade overrides: draw_panel is per-panel, so
  # computing the max there breaks "global" under faceting (every panel
  # re-normalises to its own tallest ridge). draw_layer sees ALL panels
  # post-position, so we compute the layer-wide ridge height once here
  # and stamp `global_max_height` on every row before the per-panel split.
  draw_layer = \(self, data, params, layout, coord) {
    data <- .fade_stamp_global_max(
      data,
      value_fn = \(d) abs(d$ymax - d$ymin),
      slot = "global_max_height",
      default_scope = "group"
    )
    ggplot2::ggproto_parent(ggplot2::GeomRibbon, self)$draw_layer(
      data,
      params,
      layout,
      coord
    )
  },

  # Override draw_panel to sort groups back-to-front.  Cross-panel "global"
  # is handled in draw_layer above; "group" needs no shared key (each ridge
  # falls through to draw_group which uses its own max_excursion).
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
    .check_panel_range(panel_params, "geom_ridgeline_fade")
    alpha_scope <- data$.alpha_scope[1L] %||% "group"

    # "global": `global_max_height` was stamped per row in draw_layer
    # (cross-panel, post-position). When draw_panel is called directly
    # with un-stamped data (e.g. test helpers) we recompute it here as a
    # per-panel fallback.  Must happen BEFORE the per-group split so the
    # column is copied into each group's data frame.
    # "group" scope: no shared key -- each ridge's draw_group uses its own
    # max_excursion (the unsigned height) as the alpha denominator.
    # Axis-conditional refs: when flipped_aes = TRUE the ribbon's band
    # lives on x (xmin/xmax) instead of y. baseline_col is the categorical
    # baseline axis; band_min_col / band_max_col are PositionRidgeline's
    # output band columns.
    flipped <- isTRUE(data$flipped_aes[1L])
    baseline_col <- if (flipped) "x" else "y"
    band_min_col <- if (flipped) "xmin" else "ymin"
    band_max_col <- if (flipped) "xmax" else "ymax"

    if (
      identical(alpha_scope, "global") &&
        (is.null(data$global_max_height) || all(is.na(data$global_max_height)))
    ) {
      # Heights in DATA space under non-linear value scales so the
      # cross-ridge reference matches the documented "tallest ridge by
      # data height" contract.
      coord_flipped <- inherits(coord, "CoordFlip")
      # The value (band) axis depends on both flipped_aes and CoordFlip.
      # flipped_aes alone puts the band on x; coord_flip rotates panel
      # axes -- their XOR is the visual-band axis.
      value_axis <- if (xor(flipped, coord_flipped)) "x" else "y"
      trans <- .get_scale_transformer(panel_params, value_axis)
      bmax <- data[[band_max_col]]
      bmin <- data[[band_min_col]]
      if (!identical(trans$name, "identity")) {
        heights <- abs(trans$inv(bmax) - trans$inv(bmin))
      } else {
        heights <- abs(bmax - bmin)
      }
      data$global_max_height <- .layer_max_abs(heights)
    }

    # Split by (group, baseline) rather than group alone.  If the user
    # maps `group` to something coarser than the baseline variable (e.g.
    # aes(group = fill) while the baseline has multiple levels), each
    # (group, baseline) pair becomes its own ridge instead of one
    # ribbon that spans several baselines. In the normal case where
    # group already uniquely identifies the baseline the result is
    # identical.
    groups <- split(
      data,
      interaction(data$group, data[[baseline_col]], drop = TRUE)
    )

    # Draw highest-baseline groups first (back) so lower ones overlap on top.
    o <- order(
      vapply(groups, \(d) d[[band_min_col]][1L], numeric(1)),
      decreasing = TRUE
    )
    groups <- groups[o]

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

    # Filter out zero-length / NULL entries (e.g. <2-obs ridges) so the
    # masking loop only sees real ridges.
    grobs <- Filter(
      function(g) {
        inherits(g, "ridge_components_grob")
      },
      grobs
    )
    if (!length(grobs)) {
      return(ggplot2::zeroGrob())
    }
    # Panel container does the cross-ridge outline masking at draw time.
    .ridgeline_panel_grob(grobs)
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
      cli::cli_inform(
        c(
          "i" = "{.fn geom_ridgeline_fade} does not support radial gradients in polar coordinates.",
          "i" = "Falling back to standard ridgeline rendering."
        )
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

    # Alpha at the peak; NA -> fully opaque
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

    # Axis-conditional columns. When flipped_aes = TRUE the ribbon's
    # band is on x: PositionRidgeline emitted xmin/xmax instead of
    # ymin/ymax, and the running axis is y. `running_col` is the row
    # along which the ridge varies; `band_*` columns hold the baseline
    # and peak of the band on the categorical axis.
    band_min_col <- if (isTRUE(flipped_aes)) "xmin" else "ymin"
    band_max_col <- if (isTRUE(flipped_aes)) "xmax" else "ymax"
    running_col <- if (isTRUE(flipped_aes)) "y" else "x"

    # band_min[1L] is the ridge baseline (constant per ridge, set by
    # PositionRidgeline). band_max is baseline + scale * height (can be
    # above or below). Keep panel-space `baseline`, `val_lo`, `val_hi`
    # for NPC positioning of the gradient anchor further down.
    baseline <- data[[band_min_col]][1L]
    if (!any(!is.na(data[[band_max_col]]))) {
      return(ggplot2::zeroGrob())
    }
    val_lo <- min(data[[band_max_col]], na.rm = TRUE)
    val_hi <- max(data[[band_max_col]], na.rm = TRUE)

    # Excursions for alpha ratios live in DATA space so the ratio against
    # `ref_max` (also data space) reflects data magnitude, not log-space
    # height. Under linear scales this is a no-op.
    coord_flipped <- inherits(coord, "CoordFlip")
    value_axis <- if (xor(isTRUE(flipped_aes), coord_flipped)) "x" else "y"
    trans <- .get_scale_transformer(panel_params, value_axis)
    if (!identical(trans$name, "identity")) {
      baseline_data <- trans$inv(baseline)
      val_lo_data <- trans$inv(val_lo)
      val_hi_data <- trans$inv(val_hi)
    } else {
      baseline_data <- baseline
      val_lo_data <- val_lo
      val_hi_data <- val_hi
    }
    excursion_lo <- max(0, baseline_data - val_lo_data) # depth below baseline
    excursion_hi <- max(0, val_hi_data - baseline_data) # height above baseline
    max_excursion <- max(excursion_lo, excursion_hi)

    if (!is.finite(max_excursion) || max_excursion <= 0) {
      return(ggplot2::zeroGrob())
    }

    # Scale a_start relative to the global reference, when set.
    # `global_max_height` is stamped per row in draw_layer for the
    # `"global"` scope (already in data space); for `"group"` it's NULL
    # and a_start stays at its incoming value (each ridge normalises to
    # its own max_excursion below).
    ref_max <- data$global_max_height[1L]
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

    # `coord_flip()` rotates the rendered ridge without touching
    # `flipped_aes`. Detect it so the gradient direction follows the
    # rendered ridge (mirrors the geom_col_fade / geom_area_fade fix).
    flipped_visual <- xor(isTRUE(flipped_aes), inherits(coord, "CoordFlip"))

    # Gradient direction in the polygon bbox; horizontal under flipped_visual.
    if (flipped_visual) {
      gx1 <- 0
      gy1 <- 0.5
      gx2 <- 1
      gy2 <- 0.5
    } else {
      gx1 <- 0.5
      gy1 <- 0
      gx2 <- 0.5
      gy2 <- 1
    }

    # Transform trough, baseline, and peak to panel NPC coordinates.
    # ref_df puts (running_value, band_values) so that coord$transform
    # gets meaningful x and y. Under canonical orientation that's
    # (x = data$x, y = c(val_lo, baseline, val_hi)); under flipped it's
    # (x = c(val_lo, baseline, val_hi), y = data$y) — the band is on x.
    # coord_flip rotates the NPC frame on top of that — flipped_visual
    # tells us which NPC axis the band ended up on.
    if (isTRUE(flipped_aes)) {
      ref_df <- data.frame(x = c(val_lo, baseline, val_hi), y = data$y[1L])
    } else {
      ref_df <- data.frame(x = data$x[1L], y = c(val_lo, baseline, val_hi))
    }
    ref_npc <- coord$transform(ref_df, panel_params)
    pos_npc <- if (flipped_visual) {
      pmax(0, pmin(1, ref_npc$x))
    } else {
      pmax(0, pmin(1, ref_npc$y))
    }

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

    # Separate the polygon (fill shape) from the outline polyline so the
    # panel container can mask outlines across overlapping ridges.
    # GeomRibbon returns:
    #   * outline.type = "full"           -> a single polygonGrob (outline
    #                                        baked into gp$col); no
    #                                        separate polyline child, so
    #                                        we leave the outline alone.
    #   * outline.type in upper/lower/both-> a gTree with polygon +
    #                                        polyline children.
    #   * outline.type = "none"           -> handled earlier (`colour` set
    #                                        to NA above), so the polyline
    #                                        child has no visible stroke
    #                                        even though it's present.
    poly_only <- grob
    outline_polyline <- NULL
    if (inherits(grob, "gTree") && !inherits(grob, "polygon")) {
      kids <- grob$children
      is_poly <- vapply(kids, inherits, logical(1), "polygon")
      is_line <- vapply(kids, inherits, logical(1), "polyline")
      if (any(is_poly) && any(is_line)) {
        # Use setChildren so `$childrenOrder` is rebuilt consistently --
        # mutating `$children` directly leaves stale name refs in
        # `$childrenOrder` that grid's group/drawDetails chases to NULL.
        poly_only <- grid::setChildren(
          grob,
          do.call(grid::gList, kids[is_poly])
        )
        outline_polyline <- kids[is_line][[1L]]
      }
    }

    # Mask source: opaque copies of the polygons, used by dest.out at
    # panel level to erase back-ridge outlines within front-ridge
    # shapes. Build fresh polygonGrobs (avoids dragging name/gp state
    # from the parent grob) and wrap in a fresh gTree if there's more
    # than one.
    collect_polys <- function(g) {
      if (inherits(g, "polygon")) {
        list(grid::polygonGrob(
          x = g$x,
          y = g$y,
          id = g$id,
          id.lengths = g$id.lengths,
          gp = grid::gpar(fill = "black", col = NA)
        ))
      } else if (inherits(g, "gTree")) {
        unlist(lapply(g$children, collect_polys), recursive = FALSE)
      } else {
        list()
      }
    }
    mask_polys <- collect_polys(poly_only)
    mask_shape <- if (length(mask_polys) == 0L) {
      NULL
    } else if (length(mask_polys) == 1L) {
      mask_polys[[1L]]
    } else {
      grid::gTree(children = do.call(grid::gList, mask_polys))
    }

    # Delegate tier construction to shared helper; makeContent() picks the
    # rendering tier at actual draw time based on the output device.
    fade_grob <- .build_area_fade_grob(
      poly_grob = poly_only,
      fill_col = fill_col,
      has_multi_fill = has_multi_fill,
      a_start = a_start,
      alpha_fade_to = alpha_fade_to,
      alpha_lo = alpha_lo,
      alpha_hi = alpha_hi,
      anchor_bbox = baseline_bbox,
      pos_npc = pos_npc,
      gx1 = gx1,
      gy1 = gy1,
      gx2 = gx2,
      gy2 = gy2
    )

    .ridge_components_grob(
      fade_grob = fade_grob,
      outline_grob = outline_polyline,
      mask_shape = mask_shape
    )
  }
)


#' @title Ridgeline Plots with Fading Gradient
#' @description
#' `geom_ridgeline_fade()` draws ridgeline plots: multiple area shapes
#' stacked at different vertical offsets and adds a vertical alpha gradient that
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
#' @inheritSection geom_area_fade alpha_scope = "global" under faceting
#' @inheritSection geom_area_fade Legend key under coord_flip
#'
#' @inheritParams ggplot2::geom_ribbon
#' @param alpha_fade_to A single finite number between 0 and 1. The alpha value
#'   at the baseline of each ridge. Defaults to `0` (fully transparent).
#' @param alpha_scope How to scale alpha across ridges. Vocabulary aligned
#'   with [geom_area_fade()]:
#'   * `"group"` (default): every ridge independently uses the full alpha
#'     range from `alpha_fade_to` to full opacity. Each ridge is its own
#'     reference.
#'   * `"global"`: alpha is scaled relative to the tallest ridge in the
#'     entire layer, **including across facet panels**. Shorter ridges
#'     fade in proportion.
#' @param scale Height multiplier applied to `height`. The default `NULL`
#'   auto-scales the layer so the tallest ridge overlaps its neighbour
#'   by ~50% (to `2 / max(abs(height))`). The auto-resolved value is reported via
#'   [cli::cli_inform()] so you have a starting point if you want to
#'   override.
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
#' lists levels in ascending order -- placing the lowest y at the top of the
#' legend -- which is the **reverse** of the spatial top-to-bottom reading order
#' (highest y at top of chart, lowest y at bottom).
#'
#' To align the legend with the chart, reverse the legend keys:
#'
#' ```r
#' + guides(fill = guide_legend(reverse = TRUE))
#' ```
#'
#' @seealso [geom_ridgeline_density_fade()] for the convenience density-ridgeline
#'   wrapper, [geom_area_fade()] for area plots with the same gradient effect.
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
#' totals <- aggregate(
#'   sales ~ year + month,
#'   data = subset(txhousing, year <= 2004),
#'   FUN = sum,
#'   na.rm = TRUE
#' )
#'
#' p <- ggplot(totals, aes(x = month, y = year, group = year, height = sales))
#' p + geom_ridgeline_fade(outline.type = "none")
#'
#' # increase overlap using the scale parameter
#' p + geom_ridgeline_fade(outline.type = "none", scale = 0.0001)
#'
#' # flip orientation
#' p + aes(y = month, x = year) +
#'   geom_ridgeline_fade()
#'
#' # Map a variable to `fill` to get a 2D gradient
#' # and use stat_chaikin to smooth curves
#' p +
#'   geom_ridgeline_fade(
#'     aes(fill = after_stat(height)),
#'     alpha_scope = "global",
#'     outline.type = "none",
#'     stat = "chaikin"
#'   )
#'
geom_ridgeline_fade <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = NULL,
  ...,
  alpha_fade_to = 0,
  alpha_scope = "group",
  scale = NULL,
  min_height = NULL,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRidgelineFade,
    # `position = NULL` falls through to the package's PositionRidgeline,
    # which converts `(y, height)` into the `(ymin, ymax)` form GeomRibbon
    # expects. Users can override (e.g. `position = "identity"` to take
    # responsibility for `ymin`/`ymax` themselves) — same pattern as the
    # stat-driven siblings (`geom_ridgeline_density_fade()`, etc.).
    position = position %||%
      position_ridgeline(
        scale = scale,
        min_height = min_height
      ),
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      alpha_fade_to = alpha_fade_to,
      alpha_scope = alpha_scope,
      orientation = orientation,
      na.rm = na.rm,
      ...
    )
  )
}

# geom_ridgeline_density_fade() lives in its own file
# (R/geom-ridgeline-density-fade.R) per the one-constructor-per-file
# convention. Its @rdname keeps it on the merged ?geom_ridgeline_fade page.
