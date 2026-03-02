# ---------------------------------------------------------------------------
# Grob-tree helpers
# ---------------------------------------------------------------------------
#
# GeomRibbon$draw_group (the parent) returns one of two structures:
#
#   outline.type = "full":
#     ggname("geom_ribbon", polygonGrob(...))   — polygon grob directly
#
#   all other outline.type values:
#     ggname("geom_ribbon", gTree(
#       children = gList(
#         polygonGrob(...),    # the area fill
#         polylineGrob(...)    # the outline(s)
#       )
#     ))
#
# `ggname()` just sets the `name` attribute; it does NOT add a gTree wrapper.
# Both helpers below recurse into gTree children so they handle both forms.
#
# Why we need to touch the grob tree at all
# ------------------------------------------
# ggplot2 requires that fill colours arrive in `gp$fill` as a GridLinearGradient
# object (class "GridLinearGradient"), NOT wrapped in a list (class
# "GridPatternList"). Assigning `data$fill <- list(gradient)` creates a
# GridPatternList, which the PDF/SVG backend renders as a solid fill.
# Patching `grob$gp$fill` directly after the parent has built the grob is the
# only reliable way to inject a gradient without list-wrapping.
#
# For the compositing path the polygon's gp$fill is left entirely untouched —
# the parent may already have built a HORIZONTAL linearGradient there (when
# fill is mapped to a variable). The vertical alpha schedule is applied as a
# separate compositing step (see .composite_poly_fill below).
# ---------------------------------------------------------------------------

# Fallback path helper: replace gp$fill in every polygon grob in the tree.
# Polyline grobs (outlines) are left unchanged.
#' @noRd
#' @keywords internal
.patch_poly_fill <- function(grob, fill) {
  if (inherits(grob, "gTree")) {
    grob$children <- do.call(
      grid::gList,
      lapply(grob$children, .patch_poly_fill, fill = fill)
    )
  } else if (inherits(grob, "polygon")) {
    grob$gp$fill <- fill
  }
  grob
}

# Primary path helper: wrap every polygon grob in a Porter-Duff "dest.in"
# group, leaving polyline grobs (outlines) at full opacity.
#
# How grid::groupGrob() avoids the viewport+gradient bug
# -------------------------------------------------------
# Naive approach (does NOT work):
#   grid::grobTree(polygon_with_gradient, vp = grid::viewport(mask = mask))
# When a polygon whose gp$fill is a linearGradient is drawn inside a masked
# viewport, the device backend (ragg, Cairo) processes the gradient and the
# mask in the *same* rendering pass. The result: the gradient is silently
# dropped and the polygon is filled with a solid colour.
#
# groupGrob() avoids this by rendering each grob to an *independent offscreen
# buffer* before compositing them:
#   Step 1 — render `dst` (the area polygon) to buffer A. Because this is an
#             isolated render, any linearGradient fill in gp$fill is produced
#             correctly with no mask interference.
#   Step 2 — render `src` (a plain black rectGrob with a vertical alpha
#             gradient) to buffer B.
#   Step 3 — composite: dest.in ≡ result = dst_colour × src_alpha.
#             Only the alpha channel of `src` is used; its colour is irrelevant.
#
# The compositing rule "dest.in" is the Porter-Duff "Destination In Source"
# operator: the destination (area polygon) is made visible only where the
# source (alpha_ref) has non-zero alpha, scaled proportionally. This is
# identical in effect to what ggfx::with_mask() achieves via ImageMagick
# raster compositing, but without any external dependency.
#
# Requires R >= 4.2 (grid::groupGrob added in R 4.2.0) and a device that
# reports "dest.in" in dev.capabilities()[["compositing"]].
#' @noRd
#' @keywords internal
.composite_poly_fill <- function(grob, src) {
  if (inherits(grob, "gTree")) {
    grob$children <- do.call(
      grid::gList,
      lapply(grob$children, .composite_poly_fill, src = src)
    )
  } else if (inherits(grob, "polygon")) {
    grob <- grid::groupGrob(src, op = "dest.in", dst = grob)
  }
  grob
}

#' @noRd
#' @keywords internal
.draw_key_area_fade <- function(data, params, size) {
  flipped <- params$flipped_aes %||% FALSE
  fill_color <- data$fill %||% "grey20"
  a_start <- data$alpha %||% 1
  a_end <- params$alpha_fade_to %||% 0

  # Legend key: opaque at the data-boundary end, transparent at the y = 0 end.
  # For the typical positive-values case this means transparent at bottom.
  # Coordinates are bbox-relative npc: 0 = bottom/left, 1 = top/right.
  if (flipped) {
    x1 <- 1
    y1 <- 0.5
    x2 <- 0
    y2 <- 0.5
  } else {
    x1 <- 0.5
    y1 <- 1
    x2 <- 0.5
    y2 <- 0
  }

  grid::rectGrob(
    gp = grid::gpar(
      fill = grid::linearGradient(
        colours = c(
          ggplot2::alpha(fill_color, a_start),
          ggplot2::alpha(fill_color, a_end)
        ),
        x1 = x1,
        y1 = y1,
        x2 = x2,
        y2 = y2
      ),
      col = NA
    )
  )
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAreaFade <- ggplot2::ggproto(
  "GeomAreaFade",
  ggplot2::GeomArea,

  extra_params = c(ggplot2::GeomArea$extra_params, "alpha_fade_to"),

  draw_key = .draw_key_area_fade,

  # Validation in setup_params() keeps the constructor body clean.

  setup_params = function(self, data, params) {
    params <- ggplot2::ggproto_parent(ggplot2::GeomArea, self)$setup_params(
      data,
      params
    )

    if (
      !rlang::is_scalar_double(params$alpha_fade_to) ||
        !is.finite(params$alpha_fade_to) ||
        params$alpha_fade_to < 0 ||
        params$alpha_fade_to > 1
    ) {
      cli::cli_abort(
        c(
          "{.arg alpha_fade_to} must be a single finite number in {.code [0, 1]}.",
          "x" = "Got {.val {params$alpha_fade_to}} instead."
        )
      )
    }

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

  # draw_panel sees ALL rows before GeomArea splits them by group and calls
  # draw_group once per group.  We stamp global_max_abs here so that every
  # group (e.g. two fill groups with different y-ranges) scales alpha relative
  # to the same global extreme, giving equal |y| equal alpha everywhere.
  # By draw_panel time GeomArea$setup_data has already run, so ymin/ymax exist
  # (ymin = 0, ymax = y, including negative y values).
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    flipped_aes = FALSE,
    alpha_fade_to = 0,
    outline.type = "upper",
    ...
  ) {
    if (flipped_aes) {
      data$global_max_abs <- max(abs(c(data$xmin, data$xmax)), na.rm = TRUE)
    } else {
      data$global_max_abs <- max(abs(c(data$ymin, data$ymax)), na.rm = TRUE)
    }
    ggplot2::ggproto_parent(ggplot2::GeomArea, self)$draw_panel(
      data,
      panel_params,
      coord,
      flipped_aes = flipped_aes,
      alpha_fade_to = alpha_fade_to,
      outline.type = outline.type,
      ...
    )
  },

  draw_group = function(
    self,
    data,
    panel_params,
    coord,
    flipped_aes = FALSE,
    alpha_fade_to = 0,
    outline.type = "upper",
    ...
  ) {
    if (nrow(data) < 2L) {
      cli::cli_warn(
        c(
          "!" = "{.fn geom_area_fade}: dropping group with fewer than 2 observations.",
          "i" = "Consider filtering single-observation groups before plotting."
        )
      )
      return(ggplot2::zeroGrob())
    }

    # Alpha at the data line; NA (no mapping) → fully opaque.
    a_start <- data$alpha[1L]
    if (is.na(a_start)) {
      a_start <- 1
    }

    # Resolve outline colour; handle outline.type = "none" by remapping to
    # "upper" with an invisible colour.  When no colour is specified (NA
    # default) fall back to the fill colour so the upper outline is visible
    # without an explicit colour= argument.
    if (identical(outline.type, "none")) {
      data$colour <- NA
      outline.type <- "upper"
    } else if (is.na(data$colour[1L])) {
      data$colour <- data$fill[1L]
    }

    # Value-axis range of the polygon (setup_data guarantees this always
    # includes 0: ymin = 0, ymax = y; or xmin = 0, xmax = x if flipped).
    if (flipped_aes) {
      val_lo <- min(c(data$xmin, data$xmax), na.rm = TRUE)
      val_hi <- max(c(data$xmin, data$xmax), na.rm = TRUE)
    } else {
      val_lo <- min(c(data$ymin, data$ymax), na.rm = TRUE)
      val_hi <- max(c(data$ymin, data$ymax), na.rm = TRUE)
    }

    # Gradient direction vectors — shared by both rendering paths.
    if (flipped_aes) {
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

    # Global max |y| (stamped by draw_panel across all groups so that equal
    # |y| values get equal alpha regardless of which group they belong to).
    # The %||% fallback fires when this geom is used without the panel hook.
    max_abs <- data$global_max_abs[1L] %||% max(abs(val_lo), abs(val_hi))
    if (!is.finite(max_abs) || max_abs == 0) {
      max_abs <- 1
    }

    # Case flags.
    is_degen <- val_hi <= val_lo
    zero_npc <- if (is_degen) {
      0.5
    } else {
      max(0, min(1, -val_lo / (val_hi - val_lo)))
    }

    # Alpha at each extreme, proportional to |val| / global max.
    alpha_lo <- alpha_fade_to +
      (a_start - alpha_fade_to) * abs(val_lo) / max_abs
    alpha_hi <- alpha_fade_to +
      (a_start - alpha_fade_to) * abs(val_hi) / max_abs

    # Alpha is baked into the gradient or mask; clear it so the parent
    # does not apply it a second time.
    data$alpha <- NA

    # -------------------------------------------------------------------
    # Rendering strategy
    # -------------------------------------------------------------------
    # Primary path — Porter-Duff "dest.in" compositing (R >= 4.2):
    #   1. Call the parent (GeomRibbon$draw_group) to build the polygon grob
    #      normally with data$alpha = NA (so no double alpha application).
    #      When fill is mapped to a variable, ggplot2 already creates a
    #      horizontal linearGradient in gp$fill — we leave that untouched.
    #   2. Build `alpha_ref`: a plain black rectGrob whose linearGradient
    #      alpha channel encodes the vertical fade schedule.  The gradient
    #      stops are anchored at data-space positions (val_lo, 0, val_hi)
    #      converted to panel NPC via coord$transform(), so the baseline
    #      zero is always at the correct visual position regardless of axis
    #      limits or scale reversals.  Colour does not matter for dest.in;
    #      only the alpha channel of the source is used in compositing.
    #   3. .composite_poly_fill() walks the grob tree and wraps each polygon
    #      child with grid::groupGrob(alpha_ref, op = "dest.in", dst = poly).
    #      groupGrob renders each grob to an independent offscreen buffer
    #      before compositing, so the parent's horizontal gradient is
    #      produced correctly without mask interference.
    #      dest.in rule: result = dst_colour × src_alpha.
    #      Polyline (outline) grobs are skipped and remain fully opaque.
    #
    # Fallback path — single-colour vertical gradient (any device / R < 4.2):
    #   Takes data$fill[1] as the sole fill colour and patches gp$fill on
    #   the polygon grob via .patch_poly_fill().  When fill is mapped to a
    #   variable the horizontal colour gradient is lost; a one-time cli
    #   message is emitted to inform the user.
    # -------------------------------------------------------------------

    # ---- Device capability detection ----
    #
    # Two rendering paths depending on what the active device supports:
    #
    #   1. "dest.in" compositing (ragg, cairo_pdf, svg):
    #      Full 2D gradient — horizontal colour gradient from ggplot2 combined
    #      with a vertical alpha fade via Porter-Duff compositing.
    #
    #   2. Fallback — single-colour vertical gradient (all other devices):
    #      Fill collapsed to one colour, linearGradient encodes the vertical
    #      alpha fade.  When fill is mapped to a variable the horizontal
    #      colour gradient is lost; a one-time message informs the user.
    can_composite <-
      "dest.in" %in%
      grDevices::dev.capabilities()[["compositing"]] &&
      exists("groupGrob", envir = asNamespace("grid"), inherits = FALSE)

    if (!can_composite) {
      # ---- Fallback: single-colour vertical gradient ---------------------
      if (length(unique(data$fill[!is.na(data$fill)])) > 1L) {
        cli::cli_inform(
          c(
            "!" = "{.fn geom_area_fade}: the graphics device does not support \\
                   Porter-Duff compositing.",
            "i" = "The {.arg fill} colour gradient is replaced by a single \\
                   colour. Switch to a device that supports compositing \\
                   (e.g. {.code ragg::agg_png()}, {.code svg()}) for the \\
                   combined effect."
          ),
          .frequency = "once",
          .frequency_id = "geom_area_fade_no_composite"
        )
      }

      fill_col <- data$fill[1L]
      col_fade <- ggplot2::alpha(fill_col, alpha_fade_to)

      gradient <- if (is_degen) {
        grid::linearGradient(
          colours = c(col_fade, col_fade),
          x1 = gx1,
          y1 = gy1,
          x2 = gx2,
          y2 = gy2
        )
      } else if (zero_npc < 1e-6 || zero_npc > 1 - 1e-6) {
        grid::linearGradient(
          colours = c(
            ggplot2::alpha(fill_col, alpha_lo),
            ggplot2::alpha(fill_col, alpha_hi)
          ),
          x1 = gx1,
          y1 = gy1,
          x2 = gx2,
          y2 = gy2
        )
      } else {
        grid::linearGradient(
          colours = c(
            ggplot2::alpha(fill_col, alpha_lo),
            col_fade,
            ggplot2::alpha(fill_col, alpha_hi)
          ),
          stops = c(0, zero_npc, 1),
          x1 = gx1,
          y1 = gy1,
          x2 = gx2,
          y2 = gy2
        )
      }

      # Collapse fill to a constant so GeomRibbon does not wastefully create
      # a many-stop horizontal gradient that we would immediately replace.
      data$fill <- fill_col
      data$alpha <- NA

      grob <- ggplot2::ggproto_parent(ggplot2::GeomArea, self)$draw_group(
        data,
        panel_params,
        coord,
        flipped_aes = flipped_aes,
        outline.type = outline.type,
        ...
      )
      return(.patch_poly_fill(grob, gradient))
    }

    # ---- dest.in compositing path -----------------------------------

    # Anchor gradient stops at data-space positions converted to panel NPC.
    # coord$transform() maps (x, y) data coordinates → NPC [0, 1] within the
    # panel viewport.  We probe three sentinel values on the value axis:
    #   pos_npc[1] ≅ val_lo   (most negative data value, or 0 if all-positive)
    #   pos_npc[2] ≅ 0        (the baseline — always fully transparent)
    #   pos_npc[3] ≅ val_hi   (most positive data value, or 0 if all-negative)
    # pmax/pmin clamp to [0, 1] for data that extends beyond the axis limits.
    # Using NPC rather than fixed bbox fractions (0 / 0.5 / 1) is critical:
    # it keeps the baseline anchored at y = 0 regardless of ylim, expansion,
    # or coord_trans() transformations.
    if (flipped_aes) {
      ref_df <- data.frame(x = c(val_lo, 0, val_hi), y = data$y[1L])
      ref_npc <- coord$transform(ref_df, panel_params)
      pos_npc <- pmax(0, pmin(1, ref_npc$x))
    } else {
      ref_df <- data.frame(x = data$x[1L], y = c(val_lo, 0, val_hi))
      ref_npc <- coord$transform(ref_df, panel_params)
      pos_npc <- pmax(0, pmin(1, ref_npc$y))
    }

    # Build the alpha gradient for the reference grob (alpha_ref).
    # Only the alpha channel matters for dest.in compositing; the colour
    # ("black") is irrelevant and is never seen in the final output.
    # Four cases keep the baseline (y = 0) transparent:
    #
    #   is_degen  — val_lo == val_hi (flat/constant data): uniform alpha_fade_to.
    #   all +ve   — polygon lives entirely above zero: two stops, baseline → peak.
    #   all -ve   — polygon lives entirely below zero: two stops, peak → baseline.
    #   mixed     — data crosses zero: three stops with the transparent stop at
    #               the exact NPC position of y = 0 (pos_npc[2]).
    if (is_degen) {
      comp_colours <- ggplot2::alpha(rep("black", 2L), alpha_fade_to)
      comp_stops <- c(0, 1)
    } else if (zero_npc < 1e-6) {
      # All positive: baseline at bottom of panel, peak at top.
      comp_colours <- ggplot2::alpha("black", c(alpha_fade_to, alpha_hi))
      comp_stops <- pos_npc[2:3]
    } else if (zero_npc > 1 - 1e-6) {
      # All negative: peak at bottom of panel, baseline at top.
      comp_colours <- ggplot2::alpha("black", c(alpha_lo, alpha_fade_to))
      comp_stops <- pos_npc[1:2]
    } else {
      # Mixed sign: three-stop gradient spanning the zero crossing.
      comp_colours <- ggplot2::alpha(
        "black",
        c(alpha_lo, alpha_fade_to, alpha_hi)
      )
      comp_stops <- pos_npc
    }

    # linearGradient() requires stops in strictly increasing order.
    # On reversed scales coord$transform() maps high data values to low NPC,
    # so pos_npc[3] < pos_npc[2] < pos_npc[1]. Sorting ensures correctness.
    ord <- order(comp_stops)
    comp_stops <- comp_stops[ord]
    comp_colours <- comp_colours[ord]

    alpha_ref <- grid::rectGrob(
      gp = grid::gpar(
        fill = grid::linearGradient(
          colours = comp_colours,
          stops = comp_stops,
          x1 = gx1,
          y1 = gy1,
          x2 = gx2,
          y2 = gy2
        ),
        col = NA
      )
    )

    grob <- ggplot2::ggproto_parent(ggplot2::GeomArea, self)$draw_group(
      data,
      panel_params,
      coord,
      flipped_aes = flipped_aes,
      outline.type = outline.type,
      ...
    )
    .composite_poly_fill(grob, alpha_ref)
  }
)

#' @title Area with Fading Linear Gradient
#' @description
#' This geom behaves much like [ggplot2::geom_area()] but uses [grid::linearGradient()]
#' to create area plots where the fill colour fades towards the baseline (`y = 0`).
#' The gradient is always anchored at `y = 0`: maximum transparency there,
#' fading to opaque at the data values. Opacity scales with the absolute
#' distance from zero, so equal `|y|` values always receive the same alpha —
#' full opacity is reached only at the extreme with the largest absolute value.
#' This works for positive values, negative values, and groups that cross zero
#' (where a three-stop gradient is used).
#'
#' When `fill` is mapped to a variable (e.g. `aes(fill = pop)`), the geom
#' combines the horizontal colour gradient produced by ggplot2 with the
#' vertical alpha fade, creating a two-dimensional gradient effect. This
#' requires a device that supports Porter-Duff compositing
#' (e.g. [ragg::agg_png()], [grDevices::svg()]). On unsupported devices the
#' geom falls back to a single-colour vertical fade and emits an informational
#' message.
#'
#' @aesthetics GeomAreaFade
#' @seealso
#'    [ggplot2::geom_area()] for fully opaque area charts
#'    [ggfx package](https://ggfx.data-imaginist.com/) for real magic
#' @inheritSection ggplot2::geom_area Orientation
#' @inheritParams ggplot2::geom_area
#' @param alpha_fade_to A single finite number between 0 and 1. The alpha value
#'   at `y = 0` (the baseline). Defaults to `0` (fully transparent).
#' @param outline.type Which edges of the area to draw an outline on. One of
#'   `"upper"` (default), `"lower"`, `"both"` (`"upper"` and `"lower"`),
#'   `"full"` (closed polygon outline), or `"none"`. When no `colour`
#'   aesthetic is set the outline uses the fill colour.
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
#' Murrell, P. (2023). "Groups, Compositing Operators, and Affine
#' Transformations in R Graphics." Technical Report 2021-02, Department of
#' Statistics, The University of Auckland. Version 3.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html}
#' @export
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   g = c("a", "a", "a", "b", "b", "b"),
#'   x = c(1, 3, 5, 2, 4, 6),
#'   y = c(2, 5, 1, 3, 6, 7)
#' )
#'
#' a <- ggplot(df, aes(x, y, fill = g)) +
#'   theme_minimal()
#'
#' # default behaviour: opaque at data line, transparent at y = 0
#' a + geom_area_fade()
#'
#' # change overall opacity at the data line
#' a + geom_area_fade(alpha = .5)
#'
#' # keep some opacity at the baseline
#' a + geom_area_fade(alpha_fade_to = .25)
#'
#' # works with negative values too: gradient fades towards y = 0 from below
#' set.seed(42)
#' df2 <- data.frame(x = seq_len(10), y = rnorm(10))
#' ggplot(df2, aes(x, y)) + geom_area_fade() + theme_minimal()
#'
#' # suppress the default upper outline
#' a + geom_area_fade(outline.type = "none")
#'
#' # draw upper and lower outlines (no left/right edges)
#' a + geom_area_fade(outline.type = "both")
#'
#' # closed outline (all four edges)
#' a + geom_area_fade(outline.type = "full")
#'
#' # horizontal orientation
#' a + geom_area_fade(orientation = "y")
#'
#' # disable stat alignment (useful when x values are already aligned)
#' a + geom_area_fade(aes(colour = g), outline.type = "full", stat = "identity")
#'
geom_area_fade <- function(
  mapping = NULL,
  data = NULL,
  stat = "align",
  position = "stack",
  ...,
  alpha_fade_to = 0,
  orientation = NULL,
  outline.type = "upper",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAreaFade,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      alpha_fade_to = alpha_fade_to,
      orientation = orientation,
      outline.type = outline.type,
      na.rm = na.rm,
      ...
    )
  )
}
