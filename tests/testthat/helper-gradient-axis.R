# Helpers for behavioural assertions on the linearGradient axis of fade geoms.
#
# Different fade geoms store the gradient differently:
#   * GeomColFade (`bar_fade_grob`)   — gradient grobs in `$gradient_glist`
#   * GeomAreaFade (`area_fade_grob`) — gradient pattern in `$fallback_gradient`
#     (also used by GeomRidgelineFade, which delegates to area-fade rendering)
#   * GeomRectFade (roundrectGrob)    — gradient inline on `gp$fill`
#
# We walk the panel grob, inspecting every node for a GridLinearGradient
# pattern in any of those locations, returning a matrix with x1/x2/y1/y2
# (NPC). NULL if nothing was found.
.collect_gradient_axes <- function(p) {
  g <- suppressWarnings(suppressMessages(ggplot2::ggplotGrob(p)))
  panel <- g$grobs[[grep("panel", g$layout$name)[1]]]

  axis_row <- function(f) {
    c(x1 = f$x1, x2 = f$x2, y1 = f$y1, y2 = f$y2)
  }

  walk <- function(node, acc = list()) {
    # Direct fill slot (rect_fade roundrects, col_fade gradient_glist members).
    f <- if (is.list(node$gp)) node$gp$fill else NULL
    if (inherits(f, "GridLinearGradient")) {
      acc[[length(acc) + 1L]] <- axis_row(f)
    }
    # area_fade / ridgeline_fade store the resolved gradient on a slot.
    if (inherits(node, "area_fade_grob")) {
      fg <- node$fallback_gradient
      if (inherits(fg, "GridLinearGradient")) {
        acc[[length(acc) + 1L]] <- axis_row(fg)
      }
    }
    # col_fade and rect_fade bundle their rendered gradient grobs in a
    # `$gradient_glist` slot, not as children, so descend into it explicitly.
    if (inherits(node, "bar_fade_grob") || inherits(node, "rect_fade_grob")) {
      for (sub in node$gradient_glist %||% list()) {
        acc <- walk(sub, acc)
      }
    }
    # Ridgeline-fade panel container holds per-ridge components in
    # `$ridges` until draw-time `makeContent` resolves them; descend
    # into the slot so build-time inspection still sees the fade grobs.
    if (inherits(node, "ridgeline_panel_grob")) {
      for (sub in node$ridges %||% list()) {
        acc <- walk(sub, acc)
      }
    }
    if (inherits(node, "ridge_components_grob")) {
      if (!is.null(node$fade_grob))    acc <- walk(node$fade_grob, acc)
      if (!is.null(node$outline_grob)) acc <- walk(node$outline_grob, acc)
    }
    if (inherits(node, "gTree") && length(node$children)) {
      for (ch in node$children) acc <- walk(ch, acc)
    }
    acc
  }

  res <- walk(panel)
  if (length(res) == 0) NULL else do.call(rbind, res)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
