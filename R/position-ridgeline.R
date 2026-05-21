#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionRidgeline <- ggplot2::ggproto(
  "PositionRidgeline",
  ggplot2::Position,

  scale = 1,
  min_height = NULL,

  setup_params = \(self, data) {
    scale <- self$scale  # NULL is valid -- triggers auto-scaling in compute_panel
    # `min_height = NULL` (the constructor default) means "user didn't pick a
    # value, fall back to 0 internally". We remember whether the user actively
    # chose a value so compute_panel can inform-on-drop only when the user is
    # on the default and probably doesn't realise negatives are getting clipped.
    min_height_was_default <- is.null(self$min_height)
    min_height <- self$min_height %||% 0

    .check_scalar <- function(x, arg, allow_neg_inf = FALSE) {
      ok <- is.numeric(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        (is.finite(x) || (allow_neg_inf && identical(x, -Inf)))
      if (!ok) {
        cli::cli_abort(
          paste0(
            "{.arg {arg}} must be a single finite number",
            if (allow_neg_inf) " (or {.code -Inf})" else "",
            ", not {.val {x}}."
          )
        )
      }
    }
    if (!is.null(scale)) .check_scalar(scale, "scale")
    .check_scalar(min_height, "min_height", allow_neg_inf = TRUE)

    list(
      scale = scale,
      min_height = min_height,
      min_height_was_default = min_height_was_default
    )
  },

  # Layer-level auto-scale: compute `2 / max(|height|)` ONCE across all
  # panels, so faceted plots use a uniform scale (preserving cross-panel
  # comparability under `alpha_scope = "global"`). Per-panel auto-scaling
  # would defeat that — each panel would normalise independently and the
  # global alpha reference would no longer reflect absolute heights.
  #
  # 2 (rather than 0.9 = "just touching") gives ~50% overlap into the
  # next baseline by default, the canonical "mountain range" ridgeline
  # aesthetic. Inform the user of the auto-resolved value so they have a
  # starting point if they want to override.
  compute_layer = \(self, data, params, layout) {
    if (is.null(params$scale)) {
      h <- abs(data$height)
      h <- h[is.finite(h)]
      max_h <- if (length(h)) max(h) else 0
      params$scale <- if (max_h > 0) 2 / max_h else 1
      cli::cli_inform(c(
        "i" = "Using auto-computed {.code scale = {format(signif(params$scale, 2), scientific = FALSE)}}.",
        "*" = "Pass an explicit {.arg scale} to override."
      ))
    }
    ggplot2::ggproto_parent(ggplot2::Position, self)$compute_layer(
      data,
      params,
      layout
    )
  },

  compute_panel = \(data, params, scales) {
    # A proper ridgeline has ONE baseline (y) per group. When the user
    # forgets to map `y` on `geom_ridgeline_density_fade()`,
    # `stat_density()`'s default `aes(y = after_stat(density))` silently
    # fills y with the per-row density value -- every ridge collapses onto
    # itself in a thin invisible band. The smoking gun: y and height carry
    # the exact same values (both come from `after_stat(density)`).
    y_eq_h <- if (!is.null(data$y) && !is.null(data$height)) {
      data$y == data$height
    } else {
      logical(0)
    }
    if (
      nrow(data) > 1L &&
        any(!is.na(y_eq_h)) &&            # rule out all-NA height (legit edge case)
        isTRUE(all(y_eq_h, na.rm = TRUE))
    ) {
      cli::cli_abort(c(
        "{.fn geom_ridgeline_density_fade} requires the following missing \\
         aesthetics: {.field y}.",
        "i" = "Each ridge needs a baseline value. Map {.code y} to a \\
               categorical or numeric column that identifies each ridge, \\
               e.g. {.code aes(y = factor)}."
      ), call = NULL)
    }

    # `compute_layer` normally resolves `scale = NULL` to a layer-wide value
    # before per-panel dispatch. Direct callers (test helpers, advanced users)
    # may invoke compute_panel without going through compute_layer; resolve
    # per-panel here as a safety net so they get a usable value too.
    scale <- params$scale
    if (is.null(scale)) {
      h <- abs(data$height)
      h <- h[is.finite(h)]
      max_h <- if (length(h)) max(h) else 0
      scale <- if (max_h > 0) 2 / max_h else 1
    }

    # Orientation: setup_data on GeomRidgelineFade stamps flipped_aes per
    # row. When TRUE, the baseline is x and the running axis is y; we
    # emit xmin/xmax so GeomRibbon (under flipped_aes = TRUE) gets the
    # right band column names. When FALSE (canonical), we emit ymin/ymax.
    flipped <- isTRUE(data$flipped_aes[1L])
    if (flipped) {
      data$xmin <- data$x
      data$xmax <- data$x + scale * data$height
    } else {
      data$ymin <- data$y
      data$ymax <- data$y + scale * data$height
    }

    # Points below min_height become NA so GeomRibbon renders a gap.
    # When the user is on the default min_height = 0 AND the dropped rows
    # contain actual negatives (not just zero-count bins), surface a
    # one-shot inform — that combination usually means the user has
    # signed-data they expected to render through the geom's bidirectional
    # gradient path. Pointing them at `min_height = -Inf` gives an explicit
    # opt-in. Density / histogram / freqpoly use cases (heights >= 0) never
    # trigger the inform.
    below <- data$height < params$min_height
    if (flipped) {
      data$xmax[below] <- NA
    } else {
      data$ymax[below] <- NA
    }
    if (
      isTRUE(params$min_height_was_default) &&
        any(below) &&
        any(data$height[below] < 0, na.rm = TRUE)
    ) {
      n_neg <- sum(data$height[below] < 0, na.rm = TRUE)
      cli::cli_warn(c(
        "!" = "Dropped {n_neg} row{?s} with negative {.field height} \\
               (default {.code min_height = 0}).",
        "i" = "Pass {.code min_height = -Inf} to render negative heights \\
               via the bidirectional-gradient path."
      ),
        .frequency = "regularly",
        .frequency_id = "position_ridgeline_default_drop_negatives"
      )
    }

    data
  }
)


# Internal constructor for `PositionRidgeline`. Sets `ymin = y` and
# `ymax = y + scale * height` for each ridge, dropping rows whose
# `height` falls below `min_height` (their `ymax` becomes NA and
# `GeomRibbon` renders a gap there).
#
# Not exported: the ridgeline geom constructors
# (`geom_ridgeline_fade()`, `geom_ridgeline_density_fade()`,
# `geom_ridgeline_histogram_fade()`, `geom_ridgeline_freqpoly_fade()`)
# all forward `scale` and `min_height` to this constructor internally.
# Standalone use against another geom isn't a documented or supported
# entry point.
#
# `scale = NULL` auto-scales the layer to `2 / max(|height|)` so the
# tallest ridge overlaps its neighbour by ~50% (canonical "mountain
# range" aesthetic). The auto-resolved value is reported via
# `cli::cli_inform()`.
#
# `min_height = NULL` is treated as `0` -- a sensible noise floor for
# density / histogram / freqpoly ridgelines (heights >= 0). Pass an
# explicit numeric (or `-Inf` for the bidirectional path) to override.
#' @noRd
#' @keywords internal
position_ridgeline <- function(scale = NULL, min_height = NULL) {
  ggplot2::ggproto(
    NULL,
    PositionRidgeline,
    scale = scale,
    min_height = min_height
  )
}
