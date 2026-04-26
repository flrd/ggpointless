# Deferred grob for device-aware bar rendering.
#
# Two tiers (bars never need the compositing path because each bar has a
# single fill colour — there is no within-bar colour gradient to combine):
#   Tier 1 — linearGradient fill (ragg, cairo, svg, png, ...)
#   Tier 2 — flat semi-transparent (base pdf(), postscript)
#' @noRd
#' @keywords internal
.bar_fade_grob <- function(gradient_glist, flat_glist) {
  grid::gTree(
    gradient_glist = gradient_glist,
    flat_glist = flat_glist,
    cl = "bar_fade_grob"
  )
}

#' @export
makeContent.bar_fade_grob <- function(x) {
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
      .frequency_id = "bar_fade_no_gradient"
    )
    grobs <- x$flat_glist
  } else {
    grobs <- x$gradient_glist
  }

  # Clamp corner radius so it never exceeds half the bar's smaller
  # dimension.  Without this, tiny bars degenerate into pill-shaped
  # blobs when the user-supplied radius is larger than the bar itself.
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

# Polar draw path for geom_bar_fade / geom_col_fade.
#
# Each rectangle becomes an arc-interpolated polygon via GeomPolygon.
# A radialGradient attached directly to the polygon would be resolved
# in the polygon's bounding box — each wedge gets its own "pillow"
# highlight, not the intended panel-centered annular fade.
#
# We get panel-centered gradients via viewport clipping paths: for
# each ring we create a viewport whose `clip` is the ring polygon,
# then draw a panel-sized rectGrob with a radialGradient inside
# that viewport. The rectGrob fills the viewport so cx = cy = 0.5
# resolves to the panel centre; the clip path restricts the
# rendered output to the ring shape.
#
# Clipping paths (not compositing groups) are used deliberately:
# an earlier implementation used grid::groupGrob(op = "dest.in"),
# which produced "Unknown group, N" warnings on RStudioGD when many
# sequential compositing groups were in play. Clip paths avoid
# grid's group state machine entirely.
#
# Tier 1 — clipped gradient (devices with clippingPaths + patterns).
# Tier 2 — flat mid-alpha polygons (pdf, postscript, ...).
#' @noRd
#' @keywords internal
.bar_fade_polar_grob <- function(gradient_glist, flat_glist) {
  grid::gTree(
    gradient_glist = gradient_glist,
    flat_glist = flat_glist,
    cl = "bar_fade_polar_grob"
  )
}

#' @export
makeContent.bar_fade_polar_grob <- function(x) {
  dev_name <- names(grDevices::dev.cur())
  can_gradient <- !dev_name %in% c("pdf", "postscript") &&
    tryCatch(
      {
        caps <- grDevices::dev.capabilities()
        isTRUE(caps[["clippingPaths"]]) &&
          "RadialGradient" %in% caps[["patterns"]]
      },
      error = \(e) FALSE
    )

  if (can_gradient) {
    grobs <- x$gradient_glist
  } else {
    cli::cli_inform(
      c(
        "!" = "The current graphics device does not support the clipping \\
               path + radial gradient combination required for polar \\
               bar gradients.",
        "i" = "Falling back to flat semi-transparent fills. Switch to a \\
               device that supports both (e.g. {.code ragg::agg_png()}, \\
               {.code svg()}) for the full effect."
      ),
      .frequency = "once",
      .frequency_id = "bar_fade_polar_no_gradient"
    )
    grobs <- x$flat_glist
  }

  grid::setChildren(x, grobs)
}

#' @noRd
#' @keywords internal
.draw_panel_bar_fade_polar <- function(
  data,
  panel_params,
  coord,
  alpha_fade_to,
  alpha_scope,
  lineend,
  linejoin
) {
  n <- nrow(data)

  # Peak/baseline absolute y-positions used for "group"/"global" scaling.
  # In polar theta = "x", y is the radial axis; in theta = "y", x is the
  # radial axis and y is angular.  We still scale by y magnitude because
  # that mirrors the linear path's semantics and keeps stacked segments
  # that reach higher y more opaque.
  peak_abs <- pmax(abs(data$ymin), abs(data$ymax))
  baseline_abs <- pmin(abs(data$ymin), abs(data$ymax))

  .safe_max <- function(v) {
    mx <- max(v, na.rm = TRUE)
    if (!is.finite(mx) || mx == 0) 1 else mx
  }
  global_max_abs <- .safe_max(peak_abs)
  pos_key <- round(data$x)
  group_max_abs <- vapply(
    split(peak_abs, pos_key),
    .safe_max,
    numeric(1)
  )

  gradient_list <- vector("list", n)
  flat_list <- vector("list", n)

  for (i in seq_len(n)) {
    a_start <- data$alpha[i]
    if (is.na(a_start)) {
      a_start <- 1
    }
    fill_col <- data$fill[i]

    if (identical(alpha_scope, "bar")) {
      a_baseline <- alpha_fade_to
      a_peak <- a_start
    } else if (identical(alpha_scope, "group")) {
      ref <- group_max_abs[as.character(pos_key[i])]
      a_baseline <- alpha_fade_to +
        (a_start - alpha_fade_to) * baseline_abs[i] / ref
      a_peak <- alpha_fade_to +
        (a_start - alpha_fade_to) * peak_abs[i] / ref
    } else {
      a_baseline <- alpha_fade_to +
        (a_start - alpha_fade_to) * baseline_abs[i] / global_max_abs
      a_peak <- alpha_fade_to +
        (a_start - alpha_fade_to) * peak_abs[i] / global_max_abs
    }

    # Build the arc-interpolated polygon via GeomPolygon's non-linear
    # path.  Hand it the 4 rectangle corners with group = 1; it calls
    # coord_munch internally to produce a smooth arc.
    poly_data <- data[rep(i, 4L), , drop = FALSE]
    poly_data$x <- c(data$xmin[i], data$xmax[i], data$xmax[i], data$xmin[i])
    poly_data$y <- c(data$ymax[i], data$ymax[i], data$ymin[i], data$ymin[i])
    poly_data$group <- 1L
    # GeomPolygon uses fill_alpha(fill, alpha); force alpha = 1 so the
    # mask (src) is the sole source of transparency. The per-bar alpha
    # is baked into the gradient colour stops below.
    poly_data$alpha <- 1

    poly_grob <- ggplot2::GeomPolygon$draw_panel(
      poly_data,
      panel_params,
      coord,
      lineend = lineend,
      linejoin = linejoin
    )

    if (!inherits(poly_grob, "polygon")) {
      gradient_list[[i]] <- ggplot2::zeroGrob()
      flat_list[[i]] <- ggplot2::zeroGrob()
      next
    }

    # Compute per-ring NPC radii from the polygon's transformed coords.
    xs <- as.numeric(poly_grob$x)
    ys <- as.numeric(poly_grob$y)
    radii <- sqrt((xs - 0.5)^2 + (ys - 0.5)^2)
    r_in <- min(radii, na.rm = TRUE)
    r_out <- max(radii, na.rm = TRUE)

    mid_alpha <- (a_peak + a_baseline) / 2
    flat_grob <- poly_grob
    flat_grob$gp$fill <- ggplot2::alpha(fill_col, mid_alpha)

    # Degenerate arc (e.g. zero-height segment) — no gradient.
    if (!is.finite(r_in) || !is.finite(r_out) || r_out <= r_in) {
      gradient_list[[i]] <- flat_grob
      flat_list[[i]] <- flat_grob
      next
    }

    # Panel-sized rectGrob with the annular radialGradient. Because the
    # rectGrob fills the viewport, cx = cy = 0.5 resolves to the panel
    # centre. r1 = inner NPC radius, r2 = outer NPC radius → fade from
    # a_baseline (inner) to a_peak (outer).
    gradient_rect <- grid::rectGrob(
      gp = grid::gpar(
        fill = grid::radialGradient(
          colours = c(
            ggplot2::alpha(fill_col, a_baseline),
            ggplot2::alpha(fill_col, a_peak)
          ),
          cx1 = 0.5,
          cy1 = 0.5,
          r1 = r_in,
          cx2 = 0.5,
          cy2 = 0.5,
          r2 = r_out
        ),
        col = NA
      )
    )

    # Clip the gradient to the ring polygon via a viewport whose
    # clip path IS the polygon. No compositing groups involved — this
    # avoids the "Unknown group, N" warning that grid's display-list
    # replay emitted on RStudioGD when many sequential groupGrobs were
    # stacked.
    clip_vp <- grid::viewport(clip = poly_grob)

    gradient_list[[i]] <- grid::gTree(
      children = grid::gList(gradient_rect),
      vp = clip_vp,
      name = paste0("bar_fade_polar_ring_", i)
    )
    flat_list[[i]] <- flat_grob
  }

  .bar_fade_polar_grob(
    do.call(grid::gList, gradient_list),
    do.call(grid::gList, flat_list)
  )
}


# Legend key — rounded rect with vertical alpha gradient.
#' @noRd
#' @keywords internal
.draw_key_col_fade <- function(data, params, size) {
  radius <- params$radius %||% grid::unit(3, "pt")
  if (!grid::is.unit(radius)) {
    radius <- grid::unit(radius, "pt")
  }

  fill_colour <- data$fill %||% "grey35"
  a_start <- data$alpha %||% 1
  a_end <- params$alpha_fade_to %||% 0
  flipped <- params$flipped_aes %||% FALSE

  if (flipped) {
    x1 <- 0
    y1 <- 0.5
    x2 <- 1
    y2 <- 0.5
  } else {
    x1 <- 0.5
    y1 <- 0
    x2 <- 0.5
    y2 <- 1
  }

  grid::roundrectGrob(
    r = radius,
    gp = ggplot2::gg_par(
      fill = grid::linearGradient(
        colours = c(
          ggplot2::alpha(fill_colour, a_end),
          ggplot2::alpha(fill_colour, a_start)
        ),
        x1 = x1,
        y1 = y1,
        x2 = x2,
        y2 = y2
      ),
      col = data$colour %||% NA
    )
  )
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomColFade <- ggplot2::ggproto(
  "GeomColFade",
  ggplot2::GeomBar,

  extra_params = c(
    ggplot2::GeomBar$extra_params,
    "alpha_fade_to",
    "alpha_scope",
    "radius"
  ),

  draw_key = .draw_key_col_fade,

  setup_params = \(self, data, params) {
    params <- ggplot2::ggproto_parent(ggplot2::GeomBar, self)$setup_params(
      data,
      params
    )

    params$alpha_fade_to <- params$alpha_fade_to %||% 0
    params$alpha_scope <- params$alpha_scope %||% "bar"

    .check_alpha_fade_to(params$alpha_fade_to)

    params$alpha_scope <- rlang::arg_match0(
      params$alpha_scope,
      values = c("bar", "group", "global")
    )

    if (!is.null(params$radius) && !grid::is.unit(params$radius)) {
      params$radius <- grid::unit(params$radius, "pt")
    }

    params
  },

  setup_data = \(self, data, params) {
    data <- ggplot2::ggproto_parent(ggplot2::GeomBar, self)$setup_data(
      data,
      params
    )
    data$.alpha_scope <- params$alpha_scope %||% "bar"
    data
  },

  draw_panel = \(
    self,
    data,
    panel_params,
    coord,
    lineend = "butt",
    linejoin = "round",
    alpha_fade_to = 0,
    radius = NULL,
    alpha_scope = "bar"
  ) {
    radius <- radius %||% grid::unit(3, "pt")
    if (!grid::is.unit(radius)) {
      radius <- grid::unit(radius, "pt")
    }

    alpha_scope <- data$.alpha_scope[1L] %||% alpha_scope

    is_polar <- inherits(coord, "CoordPolar") ||
      inherits(coord, "CoordRadial")

    if (!coord$is_linear() && !is_polar) {
      cli::cli_inform(
        c(
          "!" = "{.fn geom_col_fade}: rounded corners require a linear \\
                 coordinate system.",
          "i" = "Falling back to {.fn geom_bar} rendering (no rounding, \\
                 no gradient)."
        ),
        .frequency = "once",
        .frequency_id = "col_fade_nonlinear"
      )
      return(
        ggplot2::ggproto_parent(ggplot2::GeomBar, self)$draw_panel(
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

    if (is_polar) {
      return(.draw_panel_bar_fade_polar(
        data = data,
        panel_params = panel_params,
        coord = coord,
        alpha_fade_to = alpha_fade_to,
        alpha_scope = alpha_scope,
        lineend = lineend,
        linejoin = linejoin
      ))
    }

    if (.is_uniform_alpha(data, alpha_fade_to)) {
      return(ggplot2::ggproto_parent(ggplot2::GeomBar, self)$draw_panel(
        data, panel_params, coord,
        lineend = lineend, linejoin = linejoin
      ))
    }

    flipped <- any(data$flipped_aes %||% FALSE)

    # Negative-bar flag (for gradient direction)
    if (flipped) {
      is_neg <- data$xmax <= 0
    } else {
      is_neg <- data$ymax <= 0
    }

    # Absolute y-positions of each bar's baseline and peak edges.
    # "baseline" = the edge closer to y = 0, "peak" = the edge further away.
    if (flipped) {
      baseline_abs <- ifelse(is_neg, abs(data$xmax), abs(data$xmin))
      peak_abs <- ifelse(is_neg, abs(data$xmin), abs(data$xmax))
    } else {
      baseline_abs <- ifelse(is_neg, abs(data$ymax), abs(data$ymin))
      peak_abs <- ifelse(is_neg, abs(data$ymin), abs(data$ymax))
    }

    # Reference values for "global" and "group" alpha scaling.
    # Using max absolute value-axis position (not segment height) so that
    # stacked segments high up the axis stay opaque.
    .safe_max <- function(v) {
      mx <- max(v, na.rm = TRUE)
      if (!is.finite(mx) || mx == 0) 1 else mx
    }
    global_max_abs <- .safe_max(peak_abs)

    # "group" key: bars sharing the same discrete position-axis coordinate.
    # round() recovers the pre-dodge integer position so that dodged bars
    # within the same category are grouped together.
    pos_key <- if (flipped) round(data$y) else round(data$x)
    group_max_abs <- vapply(
      split(peak_abs, pos_key),
      .safe_max,
      numeric(1)
    )

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

      # Alpha at the baseline (closer to 0) and peak (further from 0) edges.
      # For "bar": full range per bar (baseline = alpha_fade_to, peak = a_start).
      # For "group"/"global": both ends scale by absolute y-position so that
      # stacked segments high up the axis stay opaque.
      if (identical(alpha_scope, "bar")) {
        a_baseline <- alpha_fade_to
        a_peak <- a_start
      } else if (identical(alpha_scope, "group")) {
        grp <- as.character(pos_key[i])
        ref <- group_max_abs[grp]
        a_baseline <- alpha_fade_to +
          (a_start - alpha_fade_to) * baseline_abs[i] / ref
        a_peak <- alpha_fade_to +
          (a_start - alpha_fade_to) * peak_abs[i] / ref
      } else {
        a_baseline <- alpha_fade_to +
          (a_start - alpha_fade_to) * baseline_abs[i] / global_max_abs
        a_peak <- alpha_fade_to +
          (a_start - alpha_fade_to) * peak_abs[i] / global_max_abs
      }

      # Gradient direction in the bar's bounding-box coordinates.
      # Always: colours = c(transparent, opaque), from baseline toward peak.
      if (flipped) {
        if (is_neg[i]) {
          # Negative horizontal: baseline at right, peak at left
          gx1 <- 1
          gy1 <- 0.5
          gx2 <- 0
          gy2 <- 0.5
        } else {
          # Positive horizontal: baseline at left, peak at right
          gx1 <- 0
          gy1 <- 0.5
          gx2 <- 1
          gy2 <- 0.5
        }
      } else {
        if (is_neg[i]) {
          # Negative vertical: baseline at top, peak at bottom
          gx1 <- 0.5
          gy1 <- 1
          gx2 <- 0.5
          gy2 <- 0
        } else {
          # Positive vertical: baseline at bottom, peak at top
          gx1 <- 0.5
          gy1 <- 0
          gx2 <- 0.5
          gy2 <- 1
        }
      }

      grad <- grid::linearGradient(
        colours = c(
          ggplot2::alpha(fill_col, a_baseline),
          ggplot2::alpha(fill_col, a_peak)
        ),
        x1 = gx1,
        y1 = gy1,
        x2 = gx2,
        y2 = gy2
      )

      mid_alpha <- (a_peak + a_baseline) / 2
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

    .bar_fade_grob(
      do.call(grid::gList, gradient_list),
      do.call(grid::gList, flat_list)
    )
  }
)

#' @title Bar Charts with Fading Gradient and Rounded Corners
#' @description
#' `geom_col_fade()` and `geom_bar_fade()` draw bar charts like
#' [ggplot2::geom_col()] / [ggplot2::geom_bar()] but with two visual
#' enhancements:
#'
#'   * **Rounded corners** via [grid::roundrectGrob()], controlled by the
#'     `radius` argument.
#'   * **A vertical alpha gradient** that fades from opaque at the peak of
#'     each bar to transparent at its baseline (the edge closest to `y = 0`).
#'
#' @concept rounded bar charts
#' @concept fading gradient
#'
#' @aesthetics GeomColFade
#'
#' @inheritParams ggplot2::geom_bar
#' @param stat The statistical transformation to use on the data for this
#'   layer, as a string. `geom_col_fade()` uses `"identity"` (no transform),
#'   `geom_bar_fade()` uses `"count"`, and `geom_histogram_fade()` uses
#'   `"bin"`.
#' @param alpha_fade_to A single finite number between 0 and 1. The alpha
#'   value at the baseline of each bar. Defaults to `0` (fully transparent).
#' @param alpha_scope How to scale the alpha gradient across bars and positions.
#'   * `"bar"` (default): every bar gets the full alpha range independently of
#'     positions or groups.
#'   * `"group"`: alpha is scaled relative to the tallest bar *at each
#'     position on the position axis*. For stacked bars this means each
#'     stack independently uses their alpha range; for dodged bars each
#'     bar has its alpha scaled relative to the maximum within that group.
#'     With `position = "fill"` all stacks are normalised to the same
#'     height, so `"group"` and `"global"` produce identical results, see examples.
#'   * `"global"`: for each bar their alpha is scaled relative to the tallest
#'     bar in the entire panel.
#' @param radius Corner radius passed to [grid::roundrectGrob()]. A
#'   [grid::unit()] object (e.g. `unit(4, "pt")`); a bare number is
#'   interpreted as points. Defaults to `unit(3, "pt")`.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @seealso [ggplot2::geom_bar()] and [ggplot2::geom_col()] for fully
#' opaque bar charts,
#'   [geom_histogram_fade()] for the histogram chart equivalent.
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
#' df <- data.frame(
#'   x = c("A", "B", "C", "D", "E"),
#'   y = c(3, 4, -2, -0.5, 1)
#' )
#'
#' ggplot(df, aes(x, y)) +
#'   geom_col_fade() +
#'   theme_minimal()
#'
#' # if you do not want the corners to be round, set the radius to 0
#' # default radius is 3 "pt"
#' ggplot(df, aes(x, y)) +
#'   geom_col_fade(radius = 0) +
#'   theme_minimal()
#'
#' # Global alpha scope: shorter bars appear more transparent
#' ggplot(df, aes(x, y)) +
#'   geom_col_fade(
#'     alpha_scope = "global",
#'     radius = unit(10, "pt")) +
#'   theme_minimal()
#'
#' # Start at 75% opacity and keep some opacity at the baseline
#' ggplot(df, aes(x, y)) +
#'   geom_col_fade(
#'     alpha = 0.75,
#'     alpha_fade_to = 0.25
#'   ) +
#'   theme_minimal()
#'
#' # Horizontal bars are supported
#' ggplot(df, aes(y, x)) +
#'   geom_col_fade() +
#'   theme_minimal()
#'
geom_col_fade <- make_constructor(
  GeomColFade,
  stat = "identity",
  position = "stack",
  alpha_fade_to = 0,
  alpha_scope = "bar",
  radius = grid::unit(3, "pt")
)

#' @rdname geom_col_fade
#' @export
#' @examples
#' library(ggplot2)
#'
#' # multiple groups
#' p <- ggplot(diamonds, aes(color, fill = cut)) +
#'   scale_fill_viridis_d(guide = "none") +
#'   labs(x = NULL, y = NULL) +
#'   theme_minimal()
#'
#' # dodged bar chart - by default each bar has their own alpha scope
#' p + geom_bar_fade(position = "dodge")
#'
#' # when bars are dodged, all bars within the same fill / colour group
#' # can share the same alpha scope
#' p + geom_bar_fade(position = "dodge", alpha_scope = "group")
#'
#' # when you want all bars to share a common scope, set the
#' # alpha scope to be 'global'
#' p + geom_bar_fade(position = "dodge", alpha_scope = "global")
#'
#' # stacked bar chart - by default each bar has their own alpha scope
#' p + geom_bar_fade()
#'
#' # for stacked bar charts each stack uses their alpha range
#' # independently when alpha_scope = "group"
#' p + geom_bar_fade(alpha_scope = "group")
#'
#' # the alpha_scope = "global" option starts at fully opacity at maximum
#' # y-value and scales all other bars accordingly
#' p + geom_bar_fade(alpha_scope = "global")
#'
#' # coord_polar() / coord_radial() — the linear bar fade becomes a
#' # panel-centered radial (annular) fade. Each ring fades from
#' # transparent at its inner radius to opaque at its outer radius.
#' # Rounded corners still require a linear coord system and are
#' # silently dropped under polar.
#' ggplot(mpg, aes(x = factor(1), fill = class)) +
#'   geom_bar_fade(width = 1) +
#'   coord_polar(theta = "y") +
#'   theme_void()
#'
#' # Bars radiating outward from the panel centre
#' ggplot(mpg, aes(x = class, fill = drv)) +
#'   geom_bar_fade() +
#'   coord_radial(theta = "x") +
#'   theme_void()
#'
geom_bar_fade <- make_constructor(
  GeomColFade,
  stat = "count",
  position = "stack",
  alpha_fade_to = 0,
  alpha_scope = "bar",
  radius = grid::unit(3, "pt")
)
