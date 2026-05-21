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


# Predicate: does the current graphics device support a Porter-Duff
# compositing operator (e.g. "dest.in", "dest.out")?
#
# Centralises the cap-list check shared across `geom_area_fade()`,
# `geom_curve_fade()`, `geom_segment_fade()`, `geom_path_fade()` and
# `geom_ridgeline_fade()`. The historical implementations also probed
# `exists("groupGrob", envir = asNamespace("grid"))` -- but `grid::groupGrob`
# shipped in R 4.2.0 and `DESCRIPTION` declares `R (>= 4.2.0)`, so the
# `exists()` guard could never be FALSE on a supported installation.
#
# `dev_caps` is optional: pass a cached `grDevices::dev.capabilities()`
# result if you already have one (saves one ~24us call); otherwise the
# helper fetches it. `dev.capabilities()` never raises on any device, so
# no `tryCatch` is needed -- a missing `compositing` slot returns NULL
# and `op %in% NULL` is `logical(0)` which `isTRUE()` treats as FALSE.
#' @noRd
#' @keywords internal
.has_compositing_op <- function(op, dev_caps = NULL) {
  caps <- dev_caps %||% grDevices::dev.capabilities()
  isTRUE(op %in% caps[["compositing"]])
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
  # `coord_flip()` swaps panel_params x/y after scales train, leaving the
  # vacated axis with NULL / empty-name transformers. Treat those as
  # identity so callers don't false-positive on a plain `coord_flip()`.
  if (is.null(tr) || !nzchar(tr$name %||% "")) {
    return(list(fwd = identity, inv = identity, name = "identity"))
  }
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
             non-finite coordinates before polar subdivision.")
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
#              category; round() recovers pre-dodge integer position).
#              `geom_col_fade()` / `geom_bar_fade()` only.
#   "y"      -- grouped by `round(data$y)` (only valid when flipped).
#              `geom_col_fade()` / `geom_bar_fade()` only.
#   "group"  -- grouped by `data$group` (ggplot2's interaction of all
#              discrete aesthetics)
#   "fill"   -- grouped by `as.character(data$fill)` (resolved hex)
#   "colour" -- grouped by `as.character(data$colour)` (resolved hex)
#   "bin"    -- grouped by `data$.bin_id` (pre-dodge bin centre stamped
#              in `GeomHistogramFade$setup_data`). `geom_histogram_fade()`
#              only; column is absent on col/bar/rect/etc.
#
# Used by `GeomColFade$draw_layer` to stamp the reference once across
# all panels; also used as a fallback inside both `draw_panel` paths
# (linear + polar) when called directly with un-stamped data.
#' @noRd
#' @keywords internal
.scope_max_abs_vec <- function(data, scope, flipped_aes = FALSE,
                                trans = NULL) {
  flipped <- isTRUE(flipped_aes) ||
    isTRUE(any(data$flipped_aes %||% FALSE))
  # `data$ymin` / `ymax` arrive in PANEL space (post-coord-transform).
  # Under a non-linear value scale (`scale_y_log10()`, `sqrt`, ...) the
  # max of panel-space values is "max log-value", not "max data value",
  # so the scope normalisation drifts from the documented "relative to
  # the layer's tallest bar in data magnitude" contract. When the
  # caller supplies a non-identity transformer, inverse-transform the
  # value axis to data space before maxing.
  value_min <- if (flipped) data$xmin else data$ymin
  value_max <- if (flipped) data$xmax else data$ymax
  if (!is.null(trans) && !identical(trans$name, "identity")) {
    value_min <- trans$inv(value_min)
    value_max <- trans$inv(value_max)
  }
  peak_abs <- pmax(abs(value_min), abs(value_max))
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
    "bin"    = stats::ave(
      peak_abs,
      # `.bin_id` is stamped by `GeomHistogramFade$setup_data` from the
      # PRE-dodge `data$x` (or `data$y` when flipped). Fall back to
      # post-dodge `x` if the column is somehow missing — this branch is
      # only reachable via `geom_histogram_fade()`, whose scope vocabulary
      # whitelists `"bin"` and where the stamp is always present in
      # production. The fallback is defensive (test harnesses that call
      # `.scope_max_abs_vec()` directly with hand-built data).
      data$.bin_id %||% data$x,
      FUN = safe_max
    ),
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


# Inform the user about aesthetics the user actively mapped to a varying
# variable but that the stat could not carry through.
#
# Stats that re-sample onto a new grid (`stat_fourier()`, `stat_catenary()`,
# `stat_arch()`) only emit `x` and `y` from `compute_group()`; every other
# column is dropped.  ggplot2 itself will warn ("The following aesthetics
# were dropped during statistical transformation"), but its suggestion --
# "did you forget to specify a `group` aesthetic?" -- is misleading here.
# Surface a ggpointless-specific hint that names the dropped columns and
# points at the two real workarounds:
#   * map an `after_stat()` value to colour/shade by the fit itself
#   * overlay the raw data as a separate layer
#
# Only columns that VARY in the input are reported, so the constant
# layer-default columns (fill, colour, linewidth, ... inherited from the
# geom's `default_aes`) do not trigger false positives.
#
# @param input    Data frame passed to `compute_layer()`.
# @param output   Data frame returned by the parent's `compute_layer()`.
# @param stat_name Name of the stat for the message (e.g. `"stat_fourier"`).
#' @noRd
#' @keywords internal
.warn_dropped_extras <- function(input, output, stat_name) {
  if (!is.data.frame(input) || nrow(input) == 0L) {
    return(invisible())
  }
  always_present <- c("x", "y", "group", "PANEL", "flipped_aes")
  candidates <- setdiff(names(input), c(names(output), always_present))
  if (!length(candidates)) {
    return(invisible())
  }
  varying <- candidates[vapply(
    candidates,
    function(col) length(unique(input[[col]])) > 1L,
    logical(1)
  )]
  if (!length(varying)) {
    return(invisible())
  }
  cli::cli_inform(
    c(
      "i" = "{.fn {stat_name}} does not propagate non-position aesthetics: \\
             {.field {varying}} dropped after the fit.",
      "*" = "To shade by the fit itself, map an {.fn after_stat} value \\
             (e.g. {.code aes(fill = after_stat(x))}).",
      "*" = "To overlay the raw data, draw it as a separate layer \\
             (e.g. {.fn geom_point})."
    ),
    .frequency = "regularly",
    .frequency_id = paste0(stat_name, "_dropped_extras")
  )
  invisible()
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


# Validate a `radius` argument for round-rect-style geoms (geom_col_fade,
# geom_rect_fade, geom_unit_*).
#
# Returns the radius unchanged when valid; falls back to `default` with a
# `cli` warning otherwise.  Bare numerics are accepted and coerced to
# points (matching the pattern already used by geom_rect_fade /
# geom_col_fade).  `NULL` returns `default` silently -- it is the documented
# "use the default" sentinel.
#
# Validation rules (everything else falls back):
#   * NULL                                          -> default (silent)
#   * single non-negative finite numeric            -> unit(value, "pt")
#   * single non-negative finite unit               -> unchanged
#   * any other shape (string, list, vector, NA,
#     negative, exotic units that fail convertUnit) -> default (with warning)
#
# @param radius  Raw user value.
# @param default Fallback (a `unit` object).  Defaults to `unit(0, "pt")`.
# @param arg     Argument name in the caller, for messages.
# @return A length-1 unit object.
#' @noRd
#' @keywords internal
.validate_radius <- function(
  radius,
  default = grid::unit(0, "pt"),
  arg = "radius"
) {
  if (is.null(radius)) return(default)

  if (is.numeric(radius) && !grid::is.unit(radius)) {
    if (length(radius) != 1L || !is.finite(radius) || radius < 0) {
      cli::cli_warn(
        c(
          "{.arg {arg}} must be a single non-negative finite number or \\
           {.cls unit}.",
          "x" = "Got {.val {radius}}.",
          "i" = "Falling back to {.code {format(default)}}."
        ),
        call = NULL
      )
      return(default)
    }
    return(grid::unit(radius, "pt"))
  }

  if (!grid::is.unit(radius)) {
    cli::cli_warn(
      c(
        "{.arg {arg}} must be a {.cls unit} object or a single number.",
        "x" = "Got {.obj_type_friendly {radius}}; did you forget \\
               {.fn grid::unit}?",
        "i" = "Falling back to {.code {format(default)}}."
      ),
      call = NULL
    )
    return(default)
  }

  if (length(radius) != 1L) {
    cli::cli_warn(
      c(
        "{.arg {arg}} must be a single value, got length \\
         {.val {length(radius)}}.",
        "i" = "Falling back to {.code {format(default)}}."
      ),
      call = NULL
    )
    return(default)
  }

  # Probe convertibility -- catches NA/NaN, negative units, and exotic
  # unit strings that wouldn't survive convertUnit at draw time.
  pt_val <- tryCatch(
    grid::convertUnit(radius, "pt", valueOnly = TRUE),
    error = function(e) NA_real_
  )
  if (is.na(pt_val) || !is.finite(pt_val) || pt_val < 0) {
    cli::cli_warn(
      c(
        "{.arg {arg}} must be a non-negative finite value, got \\
         {.code {format(radius)}}.",
        "i" = "Falling back to {.code {format(default)}}."
      ),
      call = NULL
    )
    return(default)
  }

  radius
}


# Walk a glist (or list) of grobs and clamp any roundrect's `r` to half
# the smaller rendered dimension.  Intended for use inside `makeContent.*`
# methods where `grid::convertHeight()` / `convertWidth()` resolve
# relative units against the panel viewport.
#
# When at least one roundrect needs clamping, emits a single throttled
# explicit message per call naming the maximum displayable radius and
# the user's input -- both reported in points so the user has a concrete
# upper bound regardless of the unit they supplied.
#
# Non-roundrect children pass through untouched, as do roundrects whose
# r/width/height resolve to NA (e.g. degenerate rects from log-scale -Inf
# inputs).
#
# @param grobs A grid `gList` or list of grobs.
# @param arg   Argument name for the message ("radius").
# @return The (possibly-mutated) grobs.
#' @noRd
#' @keywords internal
.clamp_roundrect_radius <- function(grobs, arg = "radius", quiet = FALSE) {
  reported <- quiet
  for (i in seq_along(grobs)) {
    g <- grobs[[i]]
    if (!inherits(g, "roundrect")) next

    # Fast path: a zero radius has nothing to clamp. Skip the three
    # `convertUnit`/`convertHeight`/`convertWidth` calls (~150 us per
    # grob) -- the default `geom_col_fade()` / `geom_bar_fade()` /
    # `geom_rect_fade()` use `radius = unit(0, "pt")`, so this is the
    # dominant case in real-world plots.
    if (length(g$r) == 1L && as.numeric(g$r) == 0) next

    r_pt <- tryCatch(
      grid::convertUnit(g$r, "pt", valueOnly = TRUE),
      error = function(e) NA_real_
    )
    h_pt <- tryCatch(
      abs(grid::convertHeight(g$height, "pt", valueOnly = TRUE)),
      error = function(e) NA_real_
    )
    w_pt <- tryCatch(
      abs(grid::convertWidth(g$width, "pt", valueOnly = TRUE)),
      error = function(e) NA_real_
    )
    if (!is.finite(r_pt) || !is.finite(h_pt) || !is.finite(w_pt)) next

    max_r <- min(h_pt, w_pt) / 2
    if (r_pt > max_r) {
      # Degenerate (zero-area) grobs are not actually invisible when r > 0:
      # a `roundrectGrob` with h = 0 or w = 0 draws an hourglass/lens
      # because the four quarter-circle corners overlap. Clamp them to
      # r = 0 so they render as a true zero-extent rect, but stay silent
      # (the user can't fix zero-count histogram bins by lowering radius;
      # only mention the cap when a *visible* cell would otherwise show
      # the artefact).
      if (h_pt > 0 && w_pt > 0 && !reported) {
        # `.frequency` defaults to "always" -- the user is actively iterating
        # on a value, so they need feedback every time they pick one above
        # the cap. Within-call duplication (one message per cell) is
        # prevented by the `reported` flag; cross-panel duplication in
        # faceted plots is intentional (the cap is panel-specific).
        cli::cli_inform(c(
          "!" = "{.arg {arg}} of {.val {round(r_pt, 2)}} pt exceeds the \\
                 largest displayable corner radius for the rendered shape.",
          "i" = "Maximum displayable radius is \\
                 {.val {round(max_r, 2)}} pt; falling back to that."
        ))
        reported <- TRUE
      }
      grobs[[i]]$r <- grid::unit(max_r, "pt")
    }
  }
  grobs
}

# Choose a safe linejoin for a `roundrectGrob`.
#
# `roundrectGrob` rendered with `linejoin = "mitre"` (the ggplot2 default
# inherited from `GeomBar` / `GeomRect`) produces visible stub-stroke
# artefacts at the path's closure point when `radius > 0` (visible as
# extra horizontal stubs extending past the bar's edge). Reproducible in
# pure grid: `grid.roundrect(r = unit(5, "pt"), gp = gpar(col = X,
# fill = grad, linejoin = "mitre"))` shows the artefact;
# `linejoin = "round"` is clean.
#
# The rounded path has no actual sharp corners, so "round" is visually
# equivalent to any other join along the path itself; the only place the
# choice matters is the closure point. For square corners (`radius == 0`)
# the user's choice IS visually meaningful, so we preserve it.
#' @noRd
#' @keywords internal
.roundrect_linejoin <- function(radius, linejoin = "mitre") {
  r_pt <- tryCatch(
    grid::convertUnit(radius, "pt", valueOnly = TRUE),
    error = function(e) 0
  )
  if (isTRUE(r_pt > 0)) "round" else linejoin
}

# Shared `setup_params` boilerplate for the fade-geom families.
# Sets defaults for `alpha_fade_to` and `alpha_scope`, validates
# `alpha_fade_to` via `.check_alpha_fade_to()`, and arg_match0's
# `alpha_scope` against the geom-specific vocabulary.
#' @noRd
#' @keywords internal
.fade_setup_params <- function(params, scopes, default_scope = scopes[1L]) {
  params$alpha_fade_to <- params$alpha_fade_to %||% 0
  params$alpha_scope <- params$alpha_scope %||% default_scope
  .check_alpha_fade_to(params$alpha_fade_to)
  params$alpha_scope <- rlang::arg_match0(
    params$alpha_scope,
    values = scopes,
    arg_nm = "alpha_scope"
  )
  params
}

# Validate `outline.type` for fade geoms that share GeomArea / GeomRibbon's
# vocabulary. Two-step check (length, then arg_match0) so the user gets a
# geom-specific error for non-scalar input instead of arg_match0's generic
# "must be a string" message.
#' @noRd
#' @keywords internal
.check_outline_type <- function(outline.type) {
  if (is.null(outline.type)) {
    return(invisible())
  }
  valid <- c("upper", "lower", "both", "full", "none")
  if (length(outline.type) != 1L) {
    cli::cli_abort(c(
      "{.arg outline.type} must be a single string.",
      "x" = "Got a vector of length {length(outline.type)}.",
      "i" = "Allowed values: {.val {valid}}."
    ))
  }
  rlang::arg_match0(
    outline.type,
    values = valid,
    arg_nm = "outline.type"
  )
}

# Cross-panel `alpha_scope = "global"` stamp used in fade geoms'
# `draw_layer` overrides. `value_fn(data)` returns the per-row magnitudes
# from which the layer-wide max is computed; the result is stored on
# `data[[slot]]` for downstream `draw_panel` access.
#' @noRd
#' @keywords internal
.fade_stamp_global_max <- function(
  data,
  value_fn,
  slot,
  default_scope = "global",
  trans = NULL
) {
  if (
    nrow(data) > 0L &&
      identical(data$.alpha_scope[1L] %||% default_scope, "global")
  ) {
    values <- value_fn(data)
    # Under a non-linear value scale the values arrive in panel
    # (transformed) space. Inv-transform to data space before maxing
    # so the layer-wide reference reflects data magnitude, matching
    # the documented `alpha_scope = "global"` contract.
    if (!is.null(trans) && !identical(trans$name, "identity")) {
      values <- trans$inv(values)
    }
    data[[slot]] <- .layer_max_abs(values)
  }
  data
}

# Cross-layer message consolidation
# -----------------------------------------------------------------------
# Several diagnostics (panel-range non-finite, rounded-corner fallback,
# ...) fire once per ggpointless layer, but a single `print(plot)` can
# include many such layers and the user does not want four near-identical
# messages -- they want one message naming all affected geoms.
#
# Strategy: each call buffers `(message_id, geom_name)` into a package
# state env, and the *first* call per id+render walks up the stack to
# find the outermost ggplot2 namespace frame and registers an `on.exit`
# there that flushes the buffer once the render returns. Subsequent
# calls for the same id and same render just append to the buffer.
# When no ggplot2 frame is found (e.g. `.queue_or_emit()` invoked
# directly from a test), the emitter is called immediately for that
# single geom -- preserves the legacy single-geom behaviour.
#' @noRd
#' @keywords internal
.ggpointless_state <- new.env(parent = emptyenv())
.ggpointless_state$pending <- list()


# Locate the outermost frame in the call stack whose function belongs to
# `ggplot2`, `grid`, or `grDevices`. That is the frame whose return
# marks the end of the current render -- it covers all three entry
# paths:
#   * `print.ggplot()` / `ggplotGrob()` (ggplot2 frame)
#   * direct `grid::grid.draw(gtable)` (grid frame; e.g. RStudio's
#     "Export > PDF" rebuilds the gtable and draws it without going
#     back through `print.ggplot`, so without grid we'd see one
#     message per `makeContent` call)
#   * `grDevices::replayPlot()` on a recorded display list
# Returning the first match (lowest stack index) gives the OUTERMOST
# render boundary, so on.exit fires once at the end of the whole render.
#' @noRd
#' @keywords internal
.find_render_frame <- function() {
  n <- sys.nframe()
  if (n <= 1L) return(NULL)
  render_ns <- c("ggplot2", "grid", "grDevices")
  for (i in seq_len(n - 1L)) {
    fn <- tryCatch(sys.function(i), error = function(e) NULL)
    if (is.null(fn)) next
    env <- environment(fn)
    if (!is.environment(env)) next
    top <- topenv(env)
    if (isNamespace(top) && environmentName(top) %in% render_ns) {
      return(sys.frame(i))
    }
  }
  NULL
}


# Buffer a per-layer diagnostic and arrange for a single consolidated
# emission at the end of the current render.
#
# @param id           Stable key identifying the message kind (one buffer
#                     per id per render).
# @param geom_name    Name of the geom triggering the diagnostic.
# @param meta         Named list of extra fields. Fields listed in
#                     `union_keys` are unioned across calls (e.g. `axis`
#                     so that one geom hitting `x` and another hitting
#                     `y` yields a single "x and y" message); all other
#                     fields are kept from the first call.
# @param emit_fn      `function(geoms, meta)` -- builds and emits the
#                     final cli message.
# @param union_keys   Character vector of meta keys to union across
#                     calls. Defaults to `character()`.
#' @noRd
#' @keywords internal
.queue_or_emit <- function(id,
                           geom_name,
                           meta = list(),
                           emit_fn,
                           union_keys = character()) {
  target_frame <- .find_render_frame()
  if (is.null(target_frame)) {
    emit_fn(geom_name, meta)
    return(invisible())
  }

  state <- .ggpointless_state$pending[[id]]
  first_call <- is.null(state)
  if (first_call) {
    state <- list(geoms = character(), meta = meta, emit_fn = emit_fn)
  } else {
    for (key in union_keys) {
      state$meta[[key]] <- unique(c(state$meta[[key]], meta[[key]]))
    }
  }
  state$geoms <- unique(c(state$geoms, geom_name))
  .ggpointless_state$pending[[id]] <- state

  if (first_call) {
    # Build the on.exit expression with the function *value* captured
    # (not a name lookup): the hook runs inside grid / grDevices /
    # ggplot2's namespace frame, where `.flush_pending` is invisible by
    # name. `rlang::call2()` produces `(<function>)(id)` semantics --
    # eval calls the captured function directly. An earlier version
    # used `ggpointless:::.flush_pending`, which works but trips R CMD
    # check's "::: calls to the package's namespace" NOTE.
    expr <- rlang::call2(.flush_pending, id)
    do.call(
      "on.exit",
      list(expr, add = TRUE, after = TRUE),
      envir = target_frame
    )
  }
  invisible()
}


# Flush one buffer. Called by the on.exit hook registered in
# `.queue_or_emit()` -- captured by value into the on.exit expression,
# so the hook fires regardless of which namespace `on.exit` is in.
#' @noRd
#' @keywords internal
.flush_pending <- function(id) {
  state <- .ggpointless_state$pending[[id]]
  .ggpointless_state$pending[[id]] <- NULL
  if (is.null(state) || !length(state$geoms)) return(invisible())
  state$emit_fn(state$geoms, state$meta)
  invisible()
}


# Inform when a panel's x or y range is non-finite -- the canonical
# symptom of `coord_transform()` applied to data outside the transform's
# domain (e.g. `coord_transform(y = "log10")` with a y = 0 data point
# becoming `log10(0) = -Inf`). Under that condition the panel range
# becomes `[-Inf, Inf]`, every finite value normalises to ~0 NPC, and
# all layers visually collapse to the panel edges -- a silent "no
# layers" failure mode that's plain ggplot2's behaviour, not a
# ggpointless bug, but worth surfacing so the user knows what to fix.
#
# Multiple layers in a single render share one consolidated message via
# `.queue_or_emit()` (see comment above).
#' @noRd
#' @keywords internal
.check_panel_range <- function(panel_params, geom_name) {
  bad_y <- !is.null(panel_params$y.range) &&
    any(!is.finite(panel_params$y.range))
  bad_x <- !is.null(panel_params$x.range) &&
    any(!is.finite(panel_params$x.range))
  if (!bad_y && !bad_x) {
    return(invisible())
  }
  axes <- c(if (bad_x) "x", if (bad_y) "y")
  .queue_or_emit(
    id = "panel_range",
    geom_name = geom_name,
    meta = list(axis = axes),
    emit_fn = .emit_panel_range,
    union_keys = "axis"
  )
}


# cli emitter for the panel-range diagnostic. Receives the deduplicated
# vector of geom names and the unioned `axis` meta from `.queue_or_emit()`.
#' @noRd
#' @keywords internal
.emit_panel_range <- function(geoms, meta) {
  axes <- meta$axis
  axis_str <- if (length(axes) >= 2L) "x and y" else axes
  cli::cli_warn(
    c(
      "!" = "{.fn {geoms}}: panel {axis_str}-range is non-finite.",
      "*" = "Usually caused by {.fn coord_transform} applied to data \\
             outside the transform's domain (e.g. {.code y = 0} under \\
             {.code log10}, or negative values under {.code log} / \\
             {.code sqrt}).",
      "*" = "Layers will render at the panel edges and may appear \\
             missing.",
      "*" = "Use {.fn scale_y_log10} (or {.code scale_*_continuous(transform = ...)}) \\
             instead of {.fn coord_transform} -- scale transforms run \\
             before stats and drop bad values cleanly."
    )
  )
}


# Buffer a "rounded corners require a linear coordinate system" notice
# from `geom_col_fade()` / `geom_rect_fade()` and emit a single
# consolidated message at the end of the render (see `.queue_or_emit()`).
#' @noRd
#' @keywords internal
.queue_rounded_corner_fallback <- function(geom_name) {
  .queue_or_emit(
    id = "rounded_corners_nonlinear",
    geom_name = geom_name,
    emit_fn = .emit_rounded_corner_fallback
  )
}


#' @noRd
#' @keywords internal
.emit_rounded_corner_fallback <- function(geoms, meta) {
  cli::cli_inform(
    c(
      "!" = "{.fn {geoms}}: rounded corners require a linear \\
             coordinate system.",
      "i" = "Falling back to flat rendering (no rounding, no gradient)."
    )
  )
}


# Buffer the "device does not support gradient fills" notice from the
# `geom_area_fade()` family (`pdf()` / `postscript()` Tier-3 fallback).
# A single faceted or ridgeline plot can contain dozens of area grobs, so
# without consolidation the user sees dozens of identical messages.
#
# `has_multi_fill` distinguishes the 1D fade (alpha gradient only) from
# the 2D case (fill colour also varies within a group); unioned across
# calls so a render that mixes both falls back to the richer wording.
#' @noRd
#' @keywords internal
.queue_area_fade_no_gradient <- function(has_multi_fill,
                                         geom_name = "geom_area_fade") {
  .queue_or_emit(
    id = "area_fade_no_gradient",
    geom_name = geom_name,
    meta = list(has_multi_fill = has_multi_fill),
    emit_fn = .emit_area_fade_no_gradient,
    union_keys = "has_multi_fill"
  )
}


#' @noRd
#' @keywords internal
.emit_area_fade_no_gradient <- function(geoms, meta) {
  any_multi <- any(meta$has_multi_fill)
  lost <- if (any_multi) {
    "The {.arg fill} colour gradient and the vertical alpha fade \\
     are replaced by a single flat colour."
  } else {
    "The vertical alpha fade is replaced by a flat mid-alpha fill."
  }
  cli::cli_inform(
    c(
      "!" = "{.fn {geoms}}: the graphics device does not support \\
             gradient fills.",
      "i" = paste(
        lost,
        "Switch to a device that supports gradients (e.g. \\
         {.code ragg::agg_png()}, {.code cairo_pdf()}, \\
         {.code svg()}) to keep the fade."
      )
    )
  )
}


# Tier-2 fallback: device supports gradients but not Porter-Duff
# compositing (`cairo_pdf()`). Only the 2D case (multi-fill within one
# group) is degraded, so this is queued only when `has_multi_fill` is
# TRUE at the call site.
#' @noRd
#' @keywords internal
.queue_area_fade_no_composite <- function(geom_name = "geom_area_fade") {
  .queue_or_emit(
    id = "area_fade_no_composite",
    geom_name = geom_name,
    emit_fn = .emit_area_fade_no_composite
  )
}


#' @noRd
#' @keywords internal
.emit_area_fade_no_composite <- function(geoms, meta) {
  cli::cli_inform(
    c(
      "!" = "{.fn {geoms}}: the graphics device does not support \\
             Porter-Duff compositing.",
      "i" = "The {.arg fill} colour gradient is replaced by a single \\
             colour.  Switch to a device that supports compositing \\
             (e.g. {.code ragg::agg_png()}, {.code svg()}) for the \\
             combined effect."
    )
  )
}


# Tier-2 fallback shared by `geom_rect_fade()` / `geom_col_fade()` /
# `geom_bar_fade()` flat (non-polar) rendering under `pdf()` /
# `postscript()`. One grob per panel triggers `makeContent.*_fade_grob`,
# so a faceted render duplicates the message N times without
# consolidation.
#' @noRd
#' @keywords internal
.queue_rect_col_no_gradient <- function(geom_name) {
  .queue_or_emit(
    id = "rect_col_no_gradient",
    geom_name = geom_name,
    emit_fn = .emit_rect_col_no_gradient
  )
}


#' @noRd
#' @keywords internal
.emit_rect_col_no_gradient <- function(geoms, meta) {
  cli::cli_inform(
    c(
      "!" = "{.fn {geoms}}: the graphics device does not support \\
             gradient fills.",
      "i" = "Falling back to a flat semi-transparent fill. Switch to a \\
             device that supports gradients (e.g. {.code ragg::agg_png()}, \\
             {.code svg()}) for the full effect."
    )
  )
}


# Polar fallback shared by `geom_rect_fade()` / `geom_col_fade()` /
# `geom_bar_fade()` under `coord_polar()` / `coord_radial()` when the
# device lacks clipping paths or radial gradient patterns. One grob per
# panel.
#' @noRd
#' @keywords internal
.queue_rect_col_polar_no_clip_pattern <- function(geom_name) {
  .queue_or_emit(
    id = "rect_col_polar_no_clip_pattern",
    geom_name = geom_name,
    emit_fn = .emit_rect_col_polar_no_clip_pattern
  )
}


#' @noRd
#' @keywords internal
.emit_rect_col_polar_no_clip_pattern <- function(geoms, meta) {
  cli::cli_inform(
    c(
      "!" = "{.fn {geoms}}: the graphics device does not support the \\
             clipping path + radial gradient combination required for \\
             polar fills.",
      "i" = "Falling back to flat semi-transparent fills. Switch to a \\
             device that supports both (e.g. {.code ragg::agg_png()}, \\
             {.code svg()}) for the full effect."
    )
  )
}


# `geom_curve_fade()` compositing fallback (pdf, cairo_pdf, postscript,
# or any device without Porter-Duff dest.in). One grob per panel.
#' @noRd
#' @keywords internal
.queue_curve_no_composite <- function() {
  .queue_or_emit(
    id = "curve_no_composite",
    geom_name = "geom_curve_fade",
    emit_fn = .emit_curve_no_composite
  )
}


#' @noRd
#' @keywords internal
.emit_curve_no_composite <- function(geoms, meta) {
  cli::cli_inform(
    c(
      "!" = "{.fn {geoms}}: the graphics device does not support \\
             compositing.",
      "i" = "Falling back to flat semi-transparent curves. Switch to a \\
             device that supports compositing (e.g. {.code ragg::agg_png()}, \\
             {.code svg()}) for the fade effect."
    )
  )
}


# `geom_path_fade()` compositing / clipping-paths fallback. Distinct
# from the curve variant because the per-segment alpha-stepping fallback
# loses linejoin -- the wording flags both losses (compositing AND
# clipping) so the user knows which capability to chase. One grob per
# panel.
#' @noRd
#' @keywords internal
.queue_path_no_composite_clipping <- function() {
  .queue_or_emit(
    id = "path_no_composite_clipping",
    geom_name = "geom_path_fade",
    emit_fn = .emit_path_no_composite_clipping
  )
}


#' @noRd
#' @keywords internal
.emit_path_no_composite_clipping <- function(geoms, meta) {
  cli::cli_inform(
    c(
      "!" = "{.fn {geoms}}: the graphics device does not support \\
             compositing or clipping paths.",
      "i" = "Falling back to per-segment alpha stepping (no linejoin). \\
             Switch to a device that supports compositing (e.g. \\
             {.code ragg::agg_png()}, {.code svg()}) for the fade effect."
    )
  )
}


# `geom_area_fade()` polar / radial fallback. Fires from `draw_group()`
# (not a `makeContent` hook), once per group, and uses the warning
# channel (the message is about an unsupported combination, not a
# routine device limitation). Emitter keeps `cli::cli_warn()` for that
# reason.
#' @noRd
#' @keywords internal
.queue_area_polar_no_gradient <- function() {
  .queue_or_emit(
    id = "area_polar_no_gradient",
    geom_name = "geom_area_fade",
    emit_fn = .emit_area_polar_no_gradient
  )
}


#' @noRd
#' @keywords internal
.emit_area_polar_no_gradient <- function(geoms, meta) {
  cli::cli_warn(
    c(
      "!" = "{.fn {geoms}}: does not support gradient fills under \\
             polar / radial coordinates.",
      "i" = "Falling back to standard area rendering."
    )
  )
}


# Ridgeline outline-mask fallback. The ridgeline family uses Porter-Duff
# `dest.out` to erase back-ridge outlines within front-ridge polygon
# shapes (otherwise they bleed through transparent baselines). When the
# device doesn't support compositing we draw outlines unmasked and emit
# this informational message so the user understands why the bleed-
# through is visible.
#' @noRd
#' @keywords internal
.queue_ridgeline_outline_no_mask <- function(geom_name = "geom_ridgeline_fade") {
  .queue_or_emit(
    id = "ridgeline_outline_no_mask",
    geom_name = geom_name,
    emit_fn = .emit_ridgeline_outline_no_mask
  )
}


#' @noRd
#' @keywords internal
.emit_ridgeline_outline_no_mask <- function(geoms, meta) {
  cli::cli_inform(
    c(
      "!" = "{.fn {geoms}}: the graphics device does not support \\
             Porter-Duff compositing, so back-ridge outlines may bleed \\
             through front-ridge transparent baselines.",
      "i" = "Switch to a device that supports compositing (e.g. \\
             {.code ragg::agg_png()}, {.code svg()}) for masked outlines, \\
             or pass {.code outline.type = \"none\"} to hide them entirely."
    )
  )
}


