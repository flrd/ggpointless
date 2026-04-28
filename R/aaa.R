#' Base ggproto classes for ggpointless
#'
#' If you are creating a new geom, stat, position, or scale in another package,
#' you'll need to extend from `ggplot2::Geom`, `ggplot2::Stat`,
#' `ggplot2::Position`, or `ggplot2::Scale`.
#'
#' More info at [ggplot2-book.org](https://ggplot2-book.org/)
#'
#' @seealso [ggplot2::ggproto()]
#' @keywords internal
#' @name ggpointless-ggproto
NULL


# Shared numerical constants. Keep definitions here so the intent is named
# once and all call sites tune the same knob.
#
# `.EPS_ZERO` -- treat any scalar (slope, segment length, cross-product,
# normalising factor) whose absolute value is below this as zero. Used by
# path-fade's bisector geometry and abline-fade's horizontal-line branch.
# Not a Newton-solver tolerance (those stay local to stat-catenary.R) and not
# the catenary coordinate dedup tolerance (`.cat_tol`, which is coarser).
#' @noRd
#' @keywords internal
.EPS_ZERO <- 1e-10


# Like `%||%` but also replaces length-1 `NA` values.  Used for theme
# properties that must be concrete (lineend, linetype, linewidth) where `NA`
# would crash grid. `colour` is intentionally NOT a consumer -- `NA` colour is
# valid (transparent) in grid.
#' @noRd
`%|NA|%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) y else x
}


# Validate and normalise `fade_direction`.
#
# Accepts a character vector with values "start" and/or "end".
# Removes duplicates and invalid entries (with a warning), then falls back
# to "start" if nothing valid remains.
#
# @param fade_direction Character vector.
# @return Character vector containing unique valid values.
#' @noRd
#' @keywords internal
.validate_fade_direction <- function(fade_direction) {
  valid <- c("start", "end")
  fade_direction <- unique(fade_direction)

  bad <- setdiff(fade_direction, valid)
  if (length(bad) > 0L) {
    cli::cli_warn(c(
      "!" = "Ignoring invalid {.arg fade_direction} value{?s}: {.val {bad}}.",
      "i" = "Valid options are {.str start} and {.str end}."
    ))
    fade_direction <- intersect(fade_direction, valid)
  }

  if (length(fade_direction) == 0L) {
    fade_direction <- "start"
  }

  fade_direction
}


# Build the gradient colour/stop vectors for a Porter-Duff alpha mask.
#
# Returns a list(colours, stops) suitable for `grid::linearGradient()`.
# The dst grob must be drawn FULLY OPAQUE; all alpha comes from this mask.
# Under dest.in compositing the final alpha at each end is exactly the
# corresponding stop alpha -- direct interpolation, not multiplicative.
#
# @param fade_direction Character vector: "start", "end", or both.
# @param alpha_fade_to  Numeric scalar in [0, 1] -- alpha at the faded end(s).
# @param a_opaque       Numeric scalar in [0, 1] -- alpha at the opaque end
#                       (the aes alpha of the row). Defaults to 1.
# @return Named list with elements `colours` and `stops`.
#' @noRd
#' @keywords internal
.fade_mask_colours <- function(fade_direction, alpha_fade_to, a_opaque = 1) {
  fade_start <- "start" %in% fade_direction
  fade_end   <- "end"   %in% fade_direction
  if (fade_start && fade_end) {
    list(
      colours = ggplot2::alpha("black", c(alpha_fade_to, a_opaque, alpha_fade_to)),
      stops   = c(0, 0.5, 1)
    )
  } else if (fade_start) {
    list(
      colours = ggplot2::alpha("black", c(alpha_fade_to, a_opaque)),
      stops   = c(0, 1)
    )
  } else {
    list(
      colours = ggplot2::alpha("black", c(a_opaque, alpha_fade_to)),
      stops   = c(0, 1)
    )
  }
}


# Retrieve the scale transformer for one axis from panel_params.
#
# Returns a named list with three elements:
#   fwd  -- the forward transform function (e.g. `function(x) -x` for reverse)
#   inv  -- the inverse transform function
#   name -- the transformer name string (e.g. "identity", "reverse", "log-10")
#
# Falls back to identity when panel_params does not expose `get_transformation`
# (should not happen with ggplot2 >= 4.0.0, but kept for safety).
#
# @param panel_params Panel parameters list from `ggplot_build()$layout$panel_params`.
# @param axis `"x"` or `"y"`.
# @return Named list with `fwd`, `inv`, and `name`.
#' @noRd
#' @keywords internal
.get_scale_transformer <- function(panel_params, axis = "x") {
  fn <- panel_params[[axis]]$get_transformation
  if (!is.function(fn)) {
    return(list(fwd = identity, inv = identity, name = "identity"))
  }
  tr <- fn()
  list(fwd = tr$transform, inv = tr$inverse, name = tr$name)
}


# Shared setup_params logic for all fade geoms that carry alpha_fade_to and
# fade_direction.  Calls the parent's setup_params first, then sets defaults
# and validates both fade params.
#
# @param self     The ggproto self object (passed through from the caller).
# @param parent_geom The immediate ggplot2 parent class (e.g. GeomSegment).
# @param data     Layer data (forwarded to parent setup_params).
# @param params   Layer params (forwarded to parent setup_params).
# @return Modified params list.
#' @noRd
#' @keywords internal
.setup_fade_params <- function(self, parent_geom, data, params) {
  params <- ggplot2::ggproto_parent(parent_geom, self)$setup_params(data, params)
  params$alpha_fade_to  <- params$alpha_fade_to  %||% 0
  params$fade_direction <- params$fade_direction %||% "start"
  .check_alpha_fade_to(params$alpha_fade_to)
  params$fade_direction <- .validate_fade_direction(params$fade_direction)
  params
}


# Dispatch for reference-line fade geoms (abline / hline / vline).
#
# Takes segment-form data (x, y, xend, yend) already computed by the caller
# and decides whether to delegate to the normal linear pipeline or to
# subdivide the line in data space first.
#
# * Linear coord -> `GeomSegmentFade$draw_panel()` unchanged (chord / straight
#   segment with fade).
# * Non-linear coord -> subdivide each line in data space into `n_subdivide`
#   equally-spaced vertices, then delegate to `GeomPathFade$draw_panel()` with
#   `alpha_mode = "gradient"` so the fade follows the curve that the coord
#   transform produces (arc for `hline`, ray for `vline`, curve for `abline`).
#
# Guards:
#   * Rows with non-finite x/y/xend/yend are dropped with a throttled
#     warning so silent NaN propagation through `coord$transform` is visible.
#   * Empty data returns `zeroGrob()`.
#   * `n_subdivide` is floored to 3 (< 3 degenerates to a chord, defeating
#     the point) and capped at 512 to keep grob size sane.
#' @noRd
#' @keywords internal
.draw_refline_fade <- function(
  data, panel_params, coord,
  lineend = "butt", na.rm = FALSE,
  alpha_fade_to = 0, fade_direction = "start",
  n_subdivide = 128L
) {
  if (nrow(data) == 0L) return(ggplot2::zeroGrob())

  if (coord$is_linear()) {
    return(GeomSegmentFade$draw_panel(
      data, panel_params, coord,
      lineend = lineend, na.rm = na.rm,
      alpha_fade_to = alpha_fade_to,
      fade_direction = fade_direction
    ))
  }

  n_subdivide <- as.integer(n_subdivide)
  if (!is.finite(n_subdivide) || n_subdivide < 3L) n_subdivide <- 3L
  if (n_subdivide > 512L) n_subdivide <- 512L

  finite <- is.finite(data$x) & is.finite(data$y) &
            is.finite(data$xend) & is.finite(data$yend)
  if (!all(finite)) {
    cli::cli_warn(
      c("!" = "Dropping {sum(!finite)} reference line row{?s} with \\
             non-finite coordinates before polar subdivision."),
      .frequency = "once",
      .frequency_id = "refline_fade_nonfinite"
    )
    data <- data[finite, , drop = FALSE]
    if (nrow(data) == 0L) return(ggplot2::zeroGrob())
  }

  parts <- vector("list", nrow(data))
  for (i in seq_len(nrow(data))) {
    tt <- seq(0, 1, length.out = n_subdivide)
    rep <- data[rep(i, n_subdivide), , drop = FALSE]
    rep$x     <- data$x[i] + tt * (data$xend[i] - data$x[i])
    rep$y     <- data$y[i] + tt * (data$yend[i] - data$y[i])
    rep$xend  <- NULL
    rep$yend  <- NULL
    rep$group <- i
    parts[[i]] <- rep
  }
  dense <- do.call(rbind, parts)
  rownames(dense) <- NULL

  GeomPathFade$draw_panel(
    dense, panel_params, coord,
    arrow = NULL, arrow.fill = NULL,
    lineend = lineend, linejoin = "round",
    na.rm = TRUE,
    alpha_fade_to = alpha_fade_to,
    fade_direction = fade_direction,
    alpha_mode = "gradient"
  )
}


# Check whether every row's effective alpha equals alpha_fade_to -- if so
# the fade has no visual effect and draw_panel can delegate to the parent.
# NA alpha is treated as 1 (the ggplot2 convention for "no alpha mapping").
#
# @param data   Layer data frame containing an `alpha` column.
# @param alpha_fade_to  Numeric scalar in [0, 1].
# @return Logical scalar.
#' @noRd
#' @keywords internal
.is_uniform_alpha <- function(data, alpha_fade_to) {
  a <- data$alpha
  a[is.na(a)] <- 1
  isTRUE(all(a == alpha_fade_to))
}


# Validate `alpha_fade_to` inside setup_params.
#
# Accepts integer and double scalars in [0, 1].  Aborts with a
# cli-formatted message when the value is out of range, non-finite, or
# not scalar numeric.
#
# @param value The value to validate (typically `params$alpha_fade_to`).
# @return The value, invisibly (for use in pipes or assignments).
#' @noRd
#' @keywords internal
.check_alpha_fade_to <- function(value) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      value < 0 ||
      value > 1
  ) {
    cli::cli_abort(c(
      "{.arg alpha_fade_to} must be a single finite number in {.code [0, 1]}.",
      "x" = "Got {.val {value}} instead."
    ))
  }
  invisible(value)
}


# Shared gate for glow_* layer parameters.
#
# Returns NULL when the caller should fall back to the parameter's default
# (input was NULL or a scalar NA), otherwise returns `value` after checking
# that its length is either 1 or `n` (mirroring ggplot2's aesthetic length
# rule: "Aesthetics must be either length 1 or the same as the data").
#
# @param value Raw user-supplied value.
# @param arg   String naming the argument, for error messages.
# @param n     Expected data length for length-n vectors; default 1L keeps
#              the helper usable outside setup_params (e.g. direct validator
#              calls from tests).
#' @noRd
#' @keywords internal
.glow_precheck <- function(value, arg, n = 1L) {
  if (is.null(value) || (length(value) == 1L && is.na(value))) {
    return(NULL)
  }
  if (length(value) != 1L && length(value) != n) {
    cli::cli_abort(c(
      "{.arg {arg}} must be length 1 or the same length as the data ({n}).",
      "x" = "Got length {length(value)}."
    ))
  }
  value
}


# Validate `glow_alpha` for geom_point_glow.  Accepts NA (-> inherit from aes
# alpha), a scalar, or a length-n numeric vector.  Non-numeric or non-finite
# values abort; values outside [0, 1] warn and are clamped (element-wise).
#' @noRd
#' @keywords internal
.check_glow_alpha <- function(value, n = 1L) {
  v <- .glow_precheck(value, "glow_alpha", n)
  if (is.null(v)) return(NA_real_)
  if (!is.numeric(v) || !all(is.finite(v))) {
    cli::cli_abort(c(
      "{.arg glow_alpha} must be finite numeric in {.code [0, 1]} \\
       or {.code NA}.",
      "x" = "Got {.val {value}} instead."
    ))
  }
  out_of_range <- v < 0 | v > 1
  if (any(out_of_range)) {
    bad <- v[which(out_of_range)[1L]]
    cli::cli_warn(c(
      "!" = "{.arg glow_alpha} must be in {.code [0, 1]}; got {.val {bad}}.",
      "i" = "Clamping to the nearest valid value."
    ))
    v <- pmax(0, pmin(1, v))
  }
  v
}


# Validate `glow_size` for geom_point_glow.  Accepts NA (-> 9x point size),
# a scalar, or a length-n non-negative numeric vector.  Non-numeric or
# non-finite values abort; any negative element warns and the whole
# parameter falls back to NA (default) to keep semantics simple.
#' @noRd
#' @keywords internal
.check_glow_size <- function(value, n = 1L) {
  v <- .glow_precheck(value, "glow_size", n)
  if (is.null(v)) return(NA_real_)
  if (!is.numeric(v) || !all(is.finite(v))) {
    cli::cli_abort(c(
      "{.arg glow_size} must be finite non-negative numeric or {.code NA}.",
      "x" = "Got {.val {value}} instead."
    ))
  }
  if (any(v < 0)) {
    bad <- v[which(v < 0)[1L]]
    cli::cli_warn(c(
      "!" = "{.arg glow_size} must be non-negative; got {.val {bad}}.",
      "i" = "Falling back to the default (9x the point size)."
    ))
    v <- NA_real_
  }
  v
}


# Layer-wide `max(abs(values))` with Date/POSIXct coercion and zero/NA
# fallback. Used by every fade geom's `draw_layer` override to compute
# the cross-panel reference for `alpha_scope = "global"`.  Centralised
# here so the same fallback semantics (Date column -> as.numeric, no
# finite values -> 1, max == 0 -> 1) cannot drift between geoms.
#' @noRd
#' @keywords internal
.layer_max_abs <- function(values) {
  v <- suppressWarnings(tryCatch(as.numeric(values), error = \(e) NULL))
  if (!is.numeric(v)) return(1)
  v <- v[is.finite(v)]
  if (length(v) == 0L) return(1)
  mx <- max(abs(v))
  if (mx > 0) mx else 1
}


# Compute the per-row scope reference for `alpha_scope` (bar-fade family).
#
# Given a data frame with at least `xmin`/`xmax`/`ymin`/`ymax` (and
# `group`, `fill`, `colour` for the matching scopes), returns a numeric
# vector the same length as the data, where each entry is the maximum
# `peak_abs` (the larger of `|ymin|` and `|ymax|`, or `|xmin|`/`|xmax|`
# when flipped) within that row's scope group.
#
# Scopes:
#   "global" -- one max for the whole data frame
#   "x"      -- grouped by `round(data$x)` (the discrete position-axis
#              category; round() recovers pre-dodge integer position)
#   "y"      -- grouped by `round(data$y)` (only valid when flipped)
#   "group"  -- grouped by `data$group` (ggplot2's interaction of all
#              discrete aesthetics)
#   "fill"   -- grouped by `as.character(data$fill)` (resolved hex)
#   "colour" -- grouped by `as.character(data$colour)` (resolved hex)
#
# Used by `GeomColFade$draw_layer` to stamp the reference once across
# all panels; also used as a fallback inside both `draw_panel` paths
# (linear + polar) when called directly with un-stamped data.
#' @noRd
#' @keywords internal
.scope_max_abs_vec <- function(data, scope, flipped_aes = FALSE) {
  flipped <- isTRUE(flipped_aes) ||
    isTRUE(any(data$flipped_aes %||% FALSE))
  peak_abs <- if (flipped) {
    pmax(abs(data$xmin), abs(data$xmax))
  } else {
    pmax(abs(data$ymin), abs(data$ymax))
  }
  safe_max <- function(v) {
    mx <- suppressWarnings(max(v, na.rm = TRUE))
    if (!is.finite(mx) || mx == 0) 1 else mx
  }
  switch(
    scope,
    "global" = rep(safe_max(peak_abs), length(peak_abs)),
    "x"      = stats::ave(peak_abs, round(data$x), FUN = safe_max),
    "y"      = stats::ave(peak_abs, round(data$y), FUN = safe_max),
    "group"  = stats::ave(peak_abs, data$group,    FUN = safe_max),
    "fill"   = stats::ave(peak_abs, as.character(data$fill),   FUN = safe_max),
    "colour" = stats::ave(peak_abs, as.character(data$colour), FUN = safe_max),
    rep(safe_max(peak_abs), length(peak_abs))   # unknown scope -> "global"
  )
}


# Drop stat-output rows whose `x`/`y` (and `xend`/`yend`, when present) fall
# outside the coord transform's valid domain.
#
# Stats that legitimately produce output beyond the input data range --
# `stat_fourier()` (Gibbs / harmonic-truncation overshoot) and
# `stat_catenary()` (sag below the endpoints) -- would otherwise crash inside
# `expand_limits_continuous_trans()` when paired with `coord_transform(y =
# "log10")` and similar restricted-domain coord transforms: the transformed
# limits become NaN and `expand_range4()` aborts with a cryptic
# "missing value where TRUE/FALSE needed" error.
#
# The helper is a no-op for any coord that is not a `CoordTransform`, and for
# `coord_transform()` instances whose per-axis transformer has no domain
# restriction (e.g. the default identity on the unused axis).  Filtered rows
# emit a single throttled warning per layer with a pointer to
# `scale_y_log10()` (or similar), which transforms BEFORE the stat runs and
# avoids the issue entirely.
#
# @param data Data frame returned by the stat (post-`compute_layer`).
# @param coord A Coord object (typically `layout$coord`).
# @param stat_name String for the warning message (e.g. `"stat_fourier"`).
# @return Data frame with offending rows removed.
#' @noRd
#' @keywords internal
.crop_to_coord_domain <- function(data, coord, stat_name) {
  if (!inherits(coord, "CoordTransform") || nrow(data) == 0L) {
    return(data)
  }
  trans <- coord$trans
  if (!is.list(trans)) {
    return(data)
  }

  # Each x*/y* column inherits its axis's domain restriction.
  axis_for <- c(x = "x", xend = "x", y = "y", yend = "y")
  cols <- intersect(names(axis_for), names(data))

  is_outside <- function(col) {
    dom <- trans[[axis_for[[col]]]]$domain
    if (length(dom) != 2L || all(is.infinite(dom))) {
      return(logical(nrow(data)))
    }
    v <- data[[col]]
    !is.na(v) & (v < dom[1L] | v > dom[2L])
  }

  bad <- Reduce(`|`, lapply(cols, is_outside), init = logical(nrow(data)))

  if (any(bad)) {
    cli::cli_warn(
      c(
        "!" = "{.fn {stat_name}} produced {sum(bad)} value{?s} outside the \\
               {.fn coord_transform} domain; dropping them to keep the plot \\
               renderable.",
        "i" = "If your data is naturally on a transformed scale, prefer \\
               {.fn scale_y_log10} (or similar) over {.fn coord_transform} -- \\
               scales transform the data before the stat runs, so the stat \\
               operates on already-transformed values."
      ),
      .frequency = "regularly",
      .frequency_id = paste0("crop_coord_domain_", stat_name)
    )
    data <- data[!bad, , drop = FALSE]
    rownames(data) <- NULL
  }

  data
}


# Generic colour-argument validator used by layer params that accept a
# single colour or a length-n vector of colours (geom_point_glow's
# glow_colour, geom_lexis's point_colour, geom_gridline's colour, ...).
#
# Semantics:
#   * NULL or scalar NA  -> returns NULL (caller decides the "inherit"
#     default; cf. NA_character_ for glow, NULL for gridline, etc.).
#   * scalar or length-n -> each element is validated via
#     `farver::decode_colour`, the same path ggplot2 itself uses for
#     colour parsing.  Character names, hex, numeric palette indices and
#     lists unwrappable to character all pass; unknown colour names
#     (e.g. "lorem") abort with a cli error carrying the farver
#     diagnostic as parent.
#   * any other length   -> abort with the length-1-or-n rule message.
#
# @param value  User-supplied value.
# @param arg    Argument name (string), used in error messages.
# @param n      Expected length for length-n vectors; default 1L (scalar).
#' @noRd
#' @keywords internal
.check_colour_arg <- function(value, arg, n = 1L) {
  v <- .glow_precheck(value, arg, n)
  if (is.null(v)) return(NULL)
  tryCatch(
    farver::decode_colour(v),
    error = function(e) {
      cli::cli_abort(
        c(
          "{.arg {arg}} is not a valid colour specification.",
          "x" = conditionMessage(e)
        ),
        parent = e
      )
    }
  )
  v
}


# Thin wrapper around `.check_colour_arg` that preserves the historical
# `NA_character_` return contract for `GeomPointGlow$setup_params`, which
# treats NA as "inherit the point colour" at draw time.
#' @noRd
#' @keywords internal
.check_glow_colour <- function(value, n = 1L) {
  .check_colour_arg(value, "glow_colour", n) %||% NA_character_
}


