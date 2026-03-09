# Grob-tree helpers
#
# GeomRibbon$draw_group (the parent) returns one of two structures:
#
#   outline.type = "full":
#     ggname("geom_ribbon", polygonGrob(...)) — polygon grob directly
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
#
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
# outline.type = "full" special case
#
# For outline.type values "upper"/"lower"/"both", the parent returns a gTree
# with a polygon (fill) and a separate polyline (outline).  The recursion
# naturally composites only the polygon and leaves the polyline untouched.
#
# For outline.type = "full" the parent returns a single polygonGrob whose
# gp$col IS the outline — there is no separate polyline.  Compositing the
# whole polygon fades the outline together with the fill, making it invisible
# near y = 0.  Fix: detach gp$col before compositing, composite the fill
# alone, then re-add the outline on top at full opacity.
#
# How grid::groupGrob() avoids a viewport+gradient bug
#
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
# Requires R >= 4.2 grid::groupGrob added in R 4.2.0) and a device that
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
    # When outline.type = "full" the outline lives on the polygon itself
    # (gp$col) rather than in a separate polyline grob. Compositing would
    # fade the outline together with the fill, so we detach it first,
    # composite the fill alone, then re-add the outline on top at full
    # opacity.
    outline_col <- grob$gp$col
    has_outline <- !is.null(outline_col) && !identical(outline_col, NA) &&
      !is.na(outline_col)

    if (has_outline) {
      outline_grob <- grob
      outline_grob$gp$fill <- NA
      grob$gp$col <- NA
      grob <- grid::gTree(children = grid::gList(
        grid::groupGrob(src, op = "dest.in", dst = grob),
        outline_grob
      ))
    } else {
      grob <- grid::groupGrob(src, op = "dest.in", dst = grob)
    }
  }
  grob
}

# Custom grob that defers device detection to render time.
#
# Grobs built in draw_group() may be replayed on a different device
# (e.g. dev.copy2pdf(), RStudio "Export > Save as PDF"). By deferring the
# device capability check to makeContent() — which fires at render time —
# we pick the correct rendering tier for the *actual* output device rather
# than the device that was active during plot construction.
#' @noRd
#' @keywords internal
.area_fade_grob <- function(poly_grob, alpha_ref, fallback_gradient,
                            flat_fill, has_multi_fill) {
  grid::gTree(
    poly_grob = poly_grob,
    alpha_ref = alpha_ref,
    fallback_gradient = fallback_gradient,
    flat_fill = flat_fill,
    has_multi_fill = has_multi_fill,
    cl = "area_fade_grob"
  )
}

#' @export
makeContent.area_fade_grob <- function(x) {
  dev_name <- names(grDevices::dev.cur())

  # Three rendering tiers (see draw_group comments for details):
  #   Tier 1 — Porter-Duff "dest.in" compositing (ragg, svg, cairo on-screen)
  #   Tier 2 — single-colour vertical linearGradient (cairo_pdf, png, etc.)
  #   Tier 3 — flat semi-transparent fill (base pdf(), postscript)
  no_gradient <- dev_name %in% c("pdf", "postscript")
  no_composite <- dev_name %in% c("pdf", "cairo_pdf", "postscript")
  can_composite <- !no_composite &&
    exists("groupGrob", envir = asNamespace("grid"), inherits = FALSE) &&
    tryCatch(
      "dest.in" %in% grDevices::dev.capabilities()[["compositing"]],
      error = \(e) FALSE
    )

  if (no_gradient) {
    if (x$has_multi_fill) {
      cli::cli_inform(
        c(
          "!" = "{.fn geom_area_fade}: the graphics device does not support \\
                 gradient fills.",
          "i" = "The {.arg fill} colour gradient is replaced by a single \\
                 colour. Switch to a device that supports compositing \\
                 (e.g. {.code ragg::agg_png()}, {.code svg()}) for the \\
                 full effect."
        ),
        .frequency = "once",
        .frequency_id = "geom_area_fade_no_gradient"
      )
    }
    grob <- .patch_poly_fill(x$poly_grob, x$flat_fill)
  } else if (!can_composite) {
    if (x$has_multi_fill) {
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
    grob <- .patch_poly_fill(x$poly_grob, x$fallback_gradient)
  } else {
    grob <- .composite_poly_fill(x$poly_grob, x$alpha_ref)
  }

  grid::setChildren(x, grid::gList(grob))
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

  extra_params = c(ggplot2::GeomArea$extra_params, "alpha_fade_to", "alpha_scope"),

  draw_key = .draw_key_area_fade,

  # Stamp alpha_scope into the data so draw_panel can read it without

  # relying on ggplot2's param filtering (which uses draw_group formals
  # when draw_panel contains `...`, potentially dropping draw_panel-only
  # params like alpha_scope).
  setup_data = \(self, data, params) {
    data <- ggplot2::ggproto_parent(ggplot2::GeomArea, self)$setup_data(
      data,
      params
    )
    data$.alpha_scope <- params$alpha_scope %||% "global"
    data
  },

  # Validation in setup_params() keeps the constructor body clean.

  setup_params = \(self, data, params) {
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

    params$alpha_scope <- rlang::arg_match0(
      params$alpha_scope,
      values = c("global", "group")
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

  # draw_panel sees ALL rows before GeomArea splits them by group and calls
  # draw_group once per group.  When alpha_scope = "global" we stamp
  # global_max_abs here so that every group scales alpha relative to the same
  # global extreme, giving equal |y| equal alpha everywhere.
  # When alpha_scope = "group" we skip the stamp; draw_group then computes
  # max_abs from its own data so each group uses the full alpha range.
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
    # alpha_scope is stamped into data by setup_data (not passed as a param)
    # because ggplot2's draw_layer filters params by draw_group formals when
    # draw_panel contains `...`, which would drop draw_panel-only params.
    alpha_scope <- data$.alpha_scope[1L] %||% "global"
    if (identical(alpha_scope, "global")) {
      if (flipped_aes) {
        data$global_max_abs <- max(abs(c(data$xmin, data$xmax)), na.rm = TRUE)
      } else {
        data$global_max_abs <- max(abs(c(data$ymin, data$ymax)), na.rm = TRUE)
      }
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

  draw_group = \(
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


    # Pre-compute all three rendering tiers.  The actual device check is
    # deferred to makeContent.area_fade_grob() so that grobs copied to a
    # different device (e.g. dev.copy2pdf(), RStudio "Export > Save as PDF")
    # pick the correct tier for the *output* device, not the device that was
    # active during plot construction.
    #
    # Three tiers:
    #   1. "dest.in" compositing (ragg, svg, cairo on-screen):
    #      Full 2D gradient — ggplot2's horizontal colour gradient combined
    #      with a vertical alpha fade via Porter-Duff compositing.
    #   2. linearGradient fallback (cairo_pdf, png, most other devices):
    #      Fill collapsed to one colour; linearGradient encodes the vertical
    #      alpha fade.
    #   3. Flat-fill fallback (base pdf(), postscript):
    #      No gradient support; plain semi-transparent fill.

    fill_col <- data$fill[1L]
    has_multi_fill <- length(unique(data$fill[!is.na(data$fill)])) > 1L

    # --- Tier 3: flat fill with mid-alpha baked into the colour ------------
    mid_alpha <- (a_start + alpha_fade_to) / 2
    flat_fill <- ggplot2::alpha(fill_col, mid_alpha)

    # --- Tier 2: single-colour vertical gradient ---------------------------
    col_fade <- ggplot2::alpha(fill_col, alpha_fade_to)

    fallback_gradient <- if (is_degen) {
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

    # --- Tier 1: alpha_ref for dest.in compositing -------------------------
    # Anchor gradient stops at data-space positions converted to panel NPC.
    # coord$transform() maps data coordinates → NPC [0, 1] within the panel.
    if (flipped_aes) {
      ref_df <- data.frame(x = c(val_lo, 0, val_hi), y = data$y[1L])
      ref_npc <- coord$transform(ref_df, panel_params)
      pos_npc <- pmax(0, pmin(1, ref_npc$x))
    } else {
      ref_df <- data.frame(x = data$x[1L], y = c(val_lo, 0, val_hi))
      ref_npc <- coord$transform(ref_df, panel_params)
      pos_npc <- pmax(0, pmin(1, ref_npc$y))
    }

    if (is_degen) {
      comp_colours <- ggplot2::alpha(rep("black", 2L), alpha_fade_to)
      comp_stops <- c(0, 1)
    } else if (zero_npc < 1e-6) {
      comp_colours <- ggplot2::alpha("black", c(alpha_fade_to, alpha_hi))
      comp_stops <- pos_npc[2:3]
    } else if (zero_npc > 1 - 1e-6) {
      comp_colours <- ggplot2::alpha("black", c(alpha_lo, alpha_fade_to))
      comp_stops <- pos_npc[1:2]
    } else {
      comp_colours <- ggplot2::alpha(
        "black",
        c(alpha_lo, alpha_fade_to, alpha_hi)
      )
      comp_stops <- pos_npc
    }

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

    # --- Build polygon grob from parent ------------------------------------
    grob <- ggplot2::ggproto_parent(ggplot2::GeomArea, self)$draw_group(
      data,
      panel_params,
      coord,
      flipped_aes = flipped_aes,
      outline.type = outline.type,
      ...
    )

    # Return a deferred grob — makeContent() picks the rendering tier at
    # draw time based on the actual output device.
    .area_fade_grob(
      grob, alpha_ref, fallback_gradient, flat_fill, has_multi_fill
    )
  }
)

#' @title Area Plots with Fading Linear Gradient
#' @description
#' This geom behaves like [ggplot2::geom_area()] but uses [grid::linearGradient()]
#' to create area plots. The gradient is always anchored at `y = 0`: maximum
#' transparency there, fading to opaque at the data values. Opacity scales
#' with the absolute distance from zero, so equal `|y|` values always receive
#' the same alpha — full opacity is reached only at the extreme with the largest
#' absolute value. This works for positive values, negative values, and groups
#' that cross zero (where a three-stop gradient is used).
#'
#' When `fill` is mapped to a variable (e.g. `aes(fill = z)`), the geom
#' combines the horizontal colour gradient produced by ggplot2 with the
#' vertical alpha fade, creating a two-dimensional gradient effect. This
#' requires a device that supports Porter-Duff compositing
#' (e.g. [ragg::agg_png()], [grDevices::svg()]). On unsupported devices the
#' geom falls back to a single-colour vertical fade and emits an informational
#' message.
#'
#' @aesthetics GeomAreaFade
#'
#' @seealso
#'    [ggplot2::geom_area()] for fully opaque area charts,
#'    the [ggfx package](https://ggfx.data-imaginist.com/) for real magic.
#'
#' @inheritSection ggplot2::geom_area Orientation
#'
#' @inheritParams ggplot2::geom_area
#' @param alpha_fade_to A single finite number between 0 and 1. The alpha value
#'   at `y = 0` (the baseline). Defaults to `0` (fully transparent).
#' @param alpha_scope How to scale alpha across groups. `"global"` (default)
#'   computes the maximum absolute y value across **all** groups in the panel so
#'   that equal `|y|` always maps to equal alpha. `"group"` computes the maximum
#'   per group, giving each group the full alpha range independently — useful
#'   with `position = "identity"` when groups have very different amplitudes.
#' @param outline.type Which edges of the area to draw an outline on. One of
#'   `"upper"` (default), `"lower"`, `"both"` (`"upper"` and `"lower"`),
#'   `"full"` (closed polygon outline), or `"none"`. When no `colour`
#'   is specified explicitly the outline inherits the `fill` colour.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
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
#' df1 <- data.frame(
#'   g = c("a", "a", "a", "b", "b", "b"),
#'   x = c(1, 3, 5, 2, 4, 6),
#'   y = c(2, 5, 1, 3, 6, 7)
#' )
#'
#' a <- ggplot(df1, aes(x, y, fill = g)) +
#'   theme_minimal()
#'
#' # default behaviour: opaque at data line, transparent at y = 0
#' # the outline colour remains unaffected
#' a + geom_area_fade()
#'
#' # change overall opacity
#' a + geom_area_fade(alpha = .25)
#'
#' # keep some opacity at the baseline
#' a + geom_area_fade(alpha_fade_to = .25)
#'
#' # suppress the default upper outline
#' a + geom_area_fade(outline.type = "none")
#'
#' # closed outline (all four edges)
#' a + geom_area_fade(outline.type = "full")
#'
#' # horizontal orientation
#' a + geom_area_fade(aes(y, x), orientation = "y")
#'
#' # disable stat alignment (useful when x values are already aligned)
#' a + geom_area_fade(stat = "identity")
#'
#' # draw upper and lower outlines (no left/right edges)
#' a + geom_area_fade(outline.type = "both", stat = "identity")
#'
#' # Use the "alpha_scope" argument to scale the alpha
#' # value of the gradients separately for each group
#' df2 <- data.frame(
#'   g = c("a", "a", "a", "b", "b", "b"),
#'   x = c(1, 3, 5, 2, 4, 6),
#'   y = c(1, 2, 1, 9, 10, 8)
#' )
#' b <- ggplot(df2, aes(x, y, fill = g)) +
#'   theme_minimal()
#'
#' # alpha_scope = "group": each group uses the alpha range independently
#' b + geom_area_fade(
#'   alpha_scope = "group",
#'   position = "identity"
#'   )
#'
#' # compare with the default where small groups appear washed out
#' # next to dominant groups, especially when position = "identity"
#' b + geom_area_fade(
#'   alpha_scope = "global", # default
#'   position = "identity"
#'   )
#'
#' # geom_area_fade works with negative values too:
#' # the gradient fades towards y = 0 from both sides
#' d <- ggplot(df2, aes(x, y - mean(y))) +
#'   theme_minimal()
#' d + geom_area_fade()
#'
#' # overwrite both fill and colour
#' d + geom_area_fade(
#'   fill = "#0833F5",
#'   colour = "#d77e7b",
#'   outline.type = "lower"
#'   )
#'
#' # a 2D-gradient is produced when fill is mapped to a variable
#' # this may not work on all graphic devices, see vignette for details
#' d + geom_area_fade(
#'   aes(fill = y),
#'   colour = "#333333",
#'   outline.type = "both"
#'   )
#'
geom_area_fade <- make_constructor(
  GeomAreaFade,
  stat = "align",
  position = "stack",
  alpha_fade_to = 0,
  alpha_scope = "global",
  orientation = NULL
)
