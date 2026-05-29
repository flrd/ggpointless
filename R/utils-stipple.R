# Internal helpers shared by the geom_stipple_* family (panel / path / line /
# step / rect). All dot-prefixed and `@noRd`. The dot lattice is built at
# *render time* inside makeContent from the panel's physical dimensions, so
# dot density stays constant regardless of panel size.
# `.stairstep()` (geom-step-fade.R) is reused by the step geom.

# Resolve a dot_spacing value to a grid::unit object.
#
# Named presets map to physical mm values so dot density stays constant across
# panel sizes. Bare numerics are accepted as mm with a cli::cli_inform; any
# grid::unit object is passed through unchanged.
#' @noRd
#' @keywords internal
.stipple_resolve_dot_spacing <- function(dot_spacing) {
  if (is.character(dot_spacing)) {
    mm <- switch(
      rlang::arg_match0(dot_spacing, c("fine", "medium", "coarse")),
      fine   = 2,
      medium = 4,
      coarse = 8
    )
    return(grid::unit(mm, "mm"))
  }
  if (inherits(dot_spacing, "unit")) {
    return(dot_spacing)
  }
  if (is.numeric(dot_spacing)) {
    cli::cli_inform(
      c(
        "!" = "{.arg dot_spacing} = {dot_spacing} was treated as \\
               {.code unit({dot_spacing}, \"mm\")}.",
        "i" = "See {.help grid::unit} for supported unit types."
      )
    )
    return(grid::unit(dot_spacing, "mm"))
  }
  cli::cli_abort(
    "{.arg dot_spacing} must be a character string, numeric, or {.cls unit} \\
     object, not {.obj_type_friendly dot_spacing}.",
    class = "ggpointless_stipple_dot_spacing"
  )
}


# Resolve a radius value to a grid::unit or NULL.
#
# NULL means "use the default covering radius (dot_spacing / sqrt(3) for hex,
# dot_spacing / sqrt(2) for square)". Bare numerics are treated as mm.
#' @noRd
#' @keywords internal
.stipple_resolve_radius <- function(radius) {
  if (is.null(radius)) return(NULL)
  if (inherits(radius, "unit")) return(radius)
  if (is.numeric(radius)) {
    cli::cli_inform(
      c(
        "!" = "{.arg radius} = {radius} was treated as \\
               {.code unit({radius}, \"mm\")}.",
        "i" = "See {.help grid::unit} for supported unit types."
      )
    )
    return(grid::unit(radius, "mm"))
  }
  cli::cli_abort(
    "{.arg radius} must be numeric, a {.cls unit} object, or NULL, \\
     not {.obj_type_friendly radius}.",
    class = "ggpointless_stipple_radius"
  )
}


# Extract the expanded x/y panel ranges from `panel_params`.
#
# Defensive wrapper over ggplot2's internal structure: handles the pre-3.5.0
# flat layout (`x.range`), the 3.5.0+ nested layout (`x$continuous_range`,
# the expanded range), and polar coordinates (`theta.range` / `r.range`, where
# theta maps to x and r to y). Returns the *expanded* range so the grid fills
# the full visible panel edge to edge.
#' @noRd
#' @keywords internal
.stipple_panel_ranges <- function(panel_params) {
  if (!is.null(panel_params[["x.range"]])) {
    list(x = panel_params[["x.range"]], y = panel_params[["y.range"]])
  } else if (!is.null(panel_params[["x"]][["continuous_range"]])) {
    list(
      x = panel_params[["x"]][["continuous_range"]],
      y = panel_params[["y"]][["continuous_range"]]
    )
  } else if (!is.null(panel_params[["theta.range"]])) {
    list(x = panel_params[["theta.range"]], y = panel_params[["r.range"]])
  } else {
    cli::cli_abort(
      "Cannot determine panel limits from {.arg panel_params}.",
      class = "ggpointless_stipple_panel_ranges"
    )
  }
}


# Generate a regular or hexagonally-staggered dot grid.
#
# `dx` and `dy` are column and row pitches in *data units* (computed by the
# caller from the physical dot spacing and the panel's mm-per-data-unit scale).
# The grid is extended one cell beyond each side and anchored at multiples of
# `dx` / `dy` from 0, so layers sharing the same pitch coincide exactly.
#' @noRd
#' @keywords internal
.stipple_grid <- function(xlim, ylim, dx, dy, type = c("hex", "square")) {
  type <- rlang::arg_match0(type, c("hex", "square"))

  if (!is.finite(dx) || !is.finite(dy) || dx <= 0 || dy <= 0) {
    return(data.frame(x = numeric(0L), y = numeric(0L)))
  }

  xs_base <- seq(
    floor((xlim[1] - dx) / dx) * dx,
    ceiling((xlim[2] + dx) / dx) * dx,
    by = dx
  )
  ys <- seq(
    floor((ylim[1] - dy) / dy) * dy,
    ceiling((ylim[2] + dy) / dy) * dy,
    by = dy
  )

  n_col <- length(xs_base)
  n_row <- length(ys)

  if (type == "square") {
    return(data.frame(
      x = rep(xs_base, times = n_row),
      y = rep(ys, each  = n_col)
    ))
  }

  # Hex: odd absolute rows (anchored at y = 0) are offset by dx/2.
  # The 1e-9 epsilon prevents spurious parity flips at exact half-values.
  abs_rows <- round(ys / dy + 1e-9)
  offsets  <- ifelse(abs_rows %% 2L == 1L, dx / 2, 0)

  data.frame(
    x = unlist(lapply(offsets, function(o) xs_base + o), use.names = FALSE),
    y = rep(ys, each = n_col)
  )
}


# Minimum distance from query points to a polyline.
#
# Segments touching an NA vertex are skipped so NA vertices break the line
# (mirroring geom_path) rather than bridging the gap. Query points with no
# valid segment return Inf (never NA), keeping the downstream filter clean.
#
# Optimisations over the naive per-segment loop:
# 1. Valid segments are extracted and their geometry pre-computed once, outside
#    the loop, so the inner loop body is free of NA checks and scalar arithmetic.
# 2. Distances are accumulated as squared values (`min_d2`); a single `sqrt`
#    runs after the loop rather than once per segment.
# 3. The t-clamping uses in-place assignment (`t[t < 0] <- 0`) rather than
#    `pmax(0, pmin(1, t))`, saving two n_pts-length vector allocations per iter.
#' @noRd
#' @keywords internal
.stipple_dist_to_polyline <- function(px, py, path_x, path_y) {
  n_vert <- length(path_x)

  if (n_vert < 1L) return(rep.int(Inf, length(px)))
  if (n_vert == 1L) {
    if (is.na(path_x[1L]) || is.na(path_y[1L])) return(rep.int(Inf, length(px)))
    return(sqrt((px - path_x[1L])^2 + (py - path_y[1L])^2))
  }

  # Filter to valid (non-NA-touching) segments and pre-compute geometry once.
  ax <- path_x[-n_vert]; ay <- path_y[-n_vert]
  bx <- path_x[-1L];     by <- path_y[-1L]
  ok  <- !is.na(ax) & !is.na(ay) & !is.na(bx) & !is.na(by)
  ax  <- ax[ok];  ay  <- ay[ok];  bx <- bx[ok]; by <- by[ok]
  n_seg <- length(ax)
  if (n_seg == 0L) return(rep.int(Inf, length(px)))

  abx  <- bx - ax
  aby  <- by - ay
  # Absorb degenerate (zero-length) segments: clamping t to [0,1] already maps
  # them to point-to-endpoint distance; using eps prevents divide-by-zero.
  len2 <- pmax(abx * abx + aby * aby, .Machine$double.eps)

  min_d2 <- rep.int(Inf, length(px))

  for (i in seq_len(n_seg)) {
    dpx <- px - ax[i]
    dpy <- py - ay[i]
    t   <- (dpx * abx[i] + dpy * aby[i]) / len2[i]
    t[t < 0] <- 0
    t[t > 1] <- 1
    ex <- dpx - t * abx[i]
    ey <- dpy - t * aby[i]
    min_d2 <- pmin(min_d2, ex * ex + ey * ey)
  }

  sqrt(min_d2)
}


# Logical mask of grid points inside an axis-aligned rectangle.
#' @noRd
#' @keywords internal
.stipple_points_in_rect <- function(grid, xmin, xmax, ymin, ymax,
                                     boundary = c("inside", "on")) {
  boundary <- rlang::arg_match0(boundary, c("inside", "on"))
  if (boundary == "inside") {
    grid$x > xmin & grid$x < xmax & grid$y > ymin & grid$y < ymax
  } else {
    grid$x >= xmin & grid$x <= xmax & grid$y >= ymin & grid$y <= ymax
  }
}


# Warn (once per panel) about NA position rows, matching ggplot2's UX. The rows
# themselves are kept upstream so the line breaks at them.
#' @noRd
#' @keywords internal
.stipple_warn_na <- function(data, na.rm, geom_name) {
  if (isTRUE(na.rm)) return(invisible())
  n <- sum(is.na(data$x) | is.na(data$y))
  if (n > 0L) {
    cli::cli_warn(
      "Removed {n} row{?s} containing missing values or values outside the \\
       scale range ({.fn {geom_name}})."
    )
  }
  invisible()
}


# Build the keep predicate for the path / line / step geoms.
#
# A grid point is kept when its Euclidean distance (in mm, isotropic on screen)
# to the polyline is at most `radius_unit`. Defaults to the lattice covering
# radius (`s_mm / sqrt(3)` hex, `s_mm / sqrt(2)` square) -- the smallest value
# leaving no gaps along any line. Distances are measured in mm so the trace
# width is uniform on screen regardless of axis scale disparity.
#
# Returns a closure with signature function(grid, gd, ctx) -> logical, where
# `ctx` is the render-time context list from makeContent.StippleGTree.
#' @noRd
#' @keywords internal
.stipple_path_keep_fun <- function(radius_unit, type) {
  function(grid, gd, ctx) {
    r_mm <- if (is.null(radius_unit)) {
      ctx$s_mm / if (type == "hex") sqrt(3) else sqrt(2)
    } else {
      as.numeric(grid::convertWidth(radius_unit, "mm"))
    }
    if (nrow(gd) == 0L) return(rep(FALSE, nrow(grid)))
    to_mm_x <- function(v) (v - ctx$ranges$x[1L]) / diff(ctx$ranges$x) * ctx$panel_w_mm
    to_mm_y <- function(v) (v - ctx$ranges$y[1L]) / diff(ctx$ranges$y) * ctx$panel_h_mm
    .stipple_dist_to_polyline(
      to_mm_x(grid$x), to_mm_y(grid$y),
      to_mm_x(gd$x),   to_mm_y(gd$y)
    ) <= r_mm
  }
}


# Conservative effective dot radius in pt, used for edge-margin computation.
#
# Sums size and stroke (both in ggplot2 mm-equivalent units) and converts to pt
# via `.pt = 72.27 / 25.4`. Always an over-estimate, which is the safe direction
# for ensuring full dots appear at the panel boundary.
#' @noRd
#' @keywords internal
.stipple_dot_radius_pt <- function(data) {
  .pt <- 72.27 / 25.4
  size   <- if (!is.null(data[["size"]]))   max(data[["size"]],   na.rm = TRUE) else 1.5
  stroke <- if (!is.null(data[["stroke"]])) max(data[["stroke"]], na.rm = TRUE) else 0.5
  (size + stroke) * .pt / 2
}


# Build a deferred stipple grob that computes its dot grid at render time.
#
# `keep_fun` has signature function(grid, gd, ctx) -> logical, where `ctx` is
# the render-time context list (s_mm, panel_w_mm, panel_h_mm, ranges, dx, dy).
# NULL keep_fun retains all grid points that survive the edge filter (panel
# geom).
#' @noRd
#' @keywords internal
.stipple_grob <- function(data, panel_params, coord,
                          dot_spacing, type, pos_aes, keep_fun,
                          dot_radius_pt) {
  grid::gTree(
    stipple_data     = data,
    panel_params     = panel_params,
    coord            = coord,
    dot_spacing_unit = .stipple_resolve_dot_spacing(dot_spacing),
    type             = type,
    pos_aes          = pos_aes,
    keep_fun         = keep_fun,
    dot_radius_pt    = dot_radius_pt,
    cl               = "StippleGTree"
  )
}


#' @export
makeContent.StippleGTree <- function(x) {
  # Physical panel dimensions -- only knowable at render time.
  panel_w_mm <- as.numeric(grid::convertWidth( grid::unit(1, "npc"), "mm"))
  panel_h_mm <- as.numeric(grid::convertHeight(grid::unit(1, "npc"), "mm"))

  ranges <- .stipple_panel_ranges(x$panel_params)
  s_mm   <- as.numeric(grid::convertWidth(x$dot_spacing_unit, "mm"))

  # Column pitch (dx) and row pitch (dy) in data units, derived from the
  # physical spacing so density stays constant when the viewer is resized.
  dx <- s_mm / panel_w_mm * diff(ranges$x)
  dy <- (if (x$type == "hex") s_mm * sqrt(3) / 2 else s_mm) /
        panel_h_mm * diff(ranges$y)

  grid_pts <- .stipple_grid(ranges$x, ranges$y, dx, dy, x$type)

  if (nrow(grid_pts) == 0L) {
    return(grid::setChildren(x, grid::gList(ggplot2::zeroGrob())))
  }

  # Remove dots whose rendered circles would cross the panel boundary.
  margin_mm <- x$dot_radius_pt * 25.4 / 72.27
  margin_x  <- margin_mm / panel_w_mm * diff(ranges$x)
  margin_y  <- margin_mm / panel_h_mm * diff(ranges$y)
  inside <- grid_pts$x >= ranges$x[1L] + margin_x &
            grid_pts$x <= ranges$x[2L] - margin_x &
            grid_pts$y >= ranges$y[1L] + margin_y &
            grid_pts$y <= ranges$y[2L] - margin_y
  grid_pts <- grid_pts[inside, , drop = FALSE]

  if (nrow(grid_pts) == 0L) {
    return(grid::setChildren(x, grid::gList(ggplot2::zeroGrob())))
  }

  ctx <- list(
    s_mm       = s_mm,
    panel_w_mm = panel_w_mm,
    panel_h_mm = panel_h_mm,
    ranges     = ranges,
    dx         = dx,
    dy         = dy
  )

  data      <- x$stipple_data
  drop_cols <- c(x$pos_aes, "PANEL", "group")
  pieces    <- list()

  for (g in unique(data$group)) {
    gd  <- data[data$group == g, , drop = FALSE]
    pts <- if (is.null(x$keep_fun)) {
      grid_pts
    } else {
      grid_pts[x$keep_fun(grid_pts, gd, ctx), , drop = FALSE]
    }
    if (nrow(pts) == 0L) next

    for (col in setdiff(names(gd), drop_cols)) {
      vals <- unique(gd[[col]])
      if (length(vals) == 1L) pts[[col]] <- vals
    }
    pts$PANEL <- gd$PANEL[1L]
    pts$group <- g
    pieces[[length(pieces) + 1L]] <- pts
  }

  if (length(pieces) == 0L) {
    return(grid::setChildren(x, grid::gList(ggplot2::zeroGrob())))
  }

  dots <- do.call(rbind, pieces)
  rownames(dots) <- NULL

  rendered <- ggplot2::GeomPoint$draw_panel(dots, x$panel_params, x$coord)
  grid::setChildren(x, grid::gList(rendered))
}
