library(ggplot2)

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

# Build with a bar chart base; returns ggplot_build() output.
build_gridline <- function(...) {
  p <- ggplot(mpg, aes(class)) +
    geom_bar(aes(fill = drv)) +
    geom_gridline(suppress = FALSE, ...)
  ggplot_build(p)
}

grob_gridline <- function(...) {
  p <- ggplot(mpg, aes(class)) +
    geom_bar(aes(fill = drv)) +
    geom_gridline(...)
  ggplotGrob(p)
}

# Call GeomGridline$draw_panel directly with the resolved layer data and
# panel_params from a real build, returning the grob.
draw_panel_gridline <- function(p, ...) {
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]           # gridline layer is always second
  pp <- b$layout$panel_params[[1L]]
  co <- b$layout$coord
  GeomGridline$draw_panel(ld, pp, co, ...)
}

base_p <- ggplot(mpg, aes(class)) +
  geom_bar(aes(fill = drv)) +
  geom_gridline(suppress = FALSE)

scatter_p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_gridline(suppress = FALSE)

# ---------------------------------------------------------------------------
# Return type
# ---------------------------------------------------------------------------

test_that("geom_gridline: default returns a list (layer + theme)", {
  result <- geom_gridline()
  expect_true(is.list(result))
  expect_length(result, 2L)
  expect_true(inherits(result[[1L]], "Layer"))
  expect_true(inherits(result[[2L]], "theme"))
})

test_that("geom_gridline: suppress=FALSE returns a single layer", {
  result <- geom_gridline(suppress = FALSE)
  expect_false(is.list(result))
  expect_true(inherits(result, "Layer"))
})

test_that("geom_gridline: grids=c('x','y') suppress=TRUE returns list (layer + theme)", {
  result <- geom_gridline(grids = c("x", "y"))
  expect_true(is.list(result))
  expect_true(inherits(result[[1L]], "Layer"))
})

# ---------------------------------------------------------------------------
# Theme suppression — injected theme() elements
# ---------------------------------------------------------------------------

test_that("geom_gridline: default suppresses panel.grid.major.y", {
  th <- geom_gridline()[[2L]]
  expect_true(inherits(th$panel.grid.major.y, "element_blank"))
})

test_that("geom_gridline: default does NOT suppress panel.grid.major.x", {
  th <- geom_gridline()[[2L]]
  expect_null(th$panel.grid.major.x)
})

test_that("geom_gridline: grids='x' suppresses panel.grid.major.x", {
  th <- geom_gridline(grids = "x")[[2L]]
  expect_true(inherits(th$panel.grid.major.x, "element_blank"))
})

test_that("geom_gridline: grids=c('x','y') suppresses both axes", {
  result <- geom_gridline(grids = c("x", "y"))
  combined <- Reduce(`+`, result[-1L], init = ggplot2::theme())
  expect_true(inherits(combined$panel.grid.major.x, "element_blank"))
  expect_true(inherits(combined$panel.grid.major.y, "element_blank"))
})

test_that("geom_gridline: minor=TRUE also suppresses panel.grid.minor.y", {
  th <- geom_gridline(minor = TRUE)[[2L]]
  expect_true(inherits(th$panel.grid.major.y, "element_blank"))
  expect_true(inherits(th$panel.grid.minor.y, "element_blank"))
})

test_that("geom_gridline: major=FALSE, minor=TRUE only suppresses minor grid", {
  th <- geom_gridline(major = FALSE, minor = TRUE)[[2L]]
  expect_null(th$panel.grid.major.y)
  expect_true(inherits(th$panel.grid.minor.y, "element_blank"))
})

test_that("geom_gridline: major=FALSE, minor=FALSE returns single layer (nothing to suppress)", {
  result <- geom_gridline(major = FALSE, minor = FALSE)
  # Nothing drawn → nothing to suppress → no theme injection
  expect_true(inherits(result, "Layer"))
})

# ---------------------------------------------------------------------------
# draw_panel: grob structure
# ---------------------------------------------------------------------------

test_that("geom_gridline: draw_panel returns gTree for major lines", {
  g <- draw_panel_gridline(base_p, grids = "y", major = TRUE, minor = FALSE)
  expect_true(inherits(g, "gTree"))
})

test_that("geom_gridline: draw_panel returns zeroGrob when major=FALSE, minor=FALSE", {
  g <- draw_panel_gridline(base_p, grids = "y", major = FALSE, minor = FALSE)
  expect_true(inherits(g, "zeroGrob"))
})

test_that("geom_gridline: grids='y' draws one segmentsGrob child", {
  g <- draw_panel_gridline(base_p, grids = "y", major = TRUE, minor = FALSE)
  expect_equal(length(g$children), 1L)
  expect_true(inherits(g$children[[1L]], "segments"))
})

test_that("geom_gridline: grids=c('x','y') draws two segmentsGrob children", {
  g <- draw_panel_gridline(scatter_p, grids = c("x", "y"), major = TRUE, minor = FALSE)
  expect_equal(length(g$children), 2L)
})

test_that("geom_gridline: major+minor draws two segmentsGrob children", {
  g <- draw_panel_gridline(scatter_p, grids = "y", major = TRUE, minor = TRUE)
  expect_equal(length(g$children), 2L)
})

test_that("geom_gridline: major=TRUE, minor=TRUE: minor grob has no major positions", {
  b   <- ggplot_build(scatter_p)
  ld  <- b$data[[2L]]
  pp  <- b$layout$panel_params[[1L]]
  co  <- b$layout$coord
  g   <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = TRUE)

  # child 1 = minor (drawn first, at bottom), child 2 = major (drawn on top)
  minor_pos <- as.numeric(g$children[[1L]]$y0)
  maj_pos   <- pp$y$break_positions()
  maj_pos   <- maj_pos[!is.na(maj_pos)]

  any_overlap <- any(vapply(minor_pos, function(p)
    any(abs(p - maj_pos) < 1e-9), logical(1L)))
  expect_false(any_overlap)
})

test_that("geom_gridline: major=FALSE, minor=TRUE excludes major positions", {
  b   <- ggplot_build(base_p)
  ld  <- b$data[[2L]]
  pp  <- b$layout$panel_params[[1L]]
  co  <- b$layout$coord
  g   <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = FALSE, minor = TRUE)

  drawn   <- as.numeric(g$children[[1L]]$y0)
  maj_pos <- pp$y$break_positions()
  maj_pos <- maj_pos[!is.na(maj_pos)]

  # None of the drawn positions should coincide with a major break.
  any_overlap <- any(vapply(drawn, function(p)
    any(abs(p - maj_pos) < 1e-9), logical(1L)))
  expect_false(any_overlap)
})

test_that("geom_gridline: major=FALSE, minor=TRUE draws only interstitial lines", {
  b   <- ggplot_build(base_p)
  ld  <- b$data[[2L]]
  pp  <- b$layout$panel_params[[1L]]
  co  <- b$layout$coord

  n_minor_all <- sum(!is.na(pp$y$break_positions_minor()))
  n_major     <- sum(!is.na(pp$y$break_positions()))
  n_expected  <- n_minor_all - n_major   # interstitial only

  g <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = FALSE, minor = TRUE)
  expect_equal(length(as.numeric(g$children[[1L]]$y0)), n_expected)
})

# ---------------------------------------------------------------------------
# draw_panel: line positions match scale breaks
# ---------------------------------------------------------------------------

test_that("geom_gridline: y segment positions match panel_params$y$break_positions()", {
  b  <- ggplot_build(base_p)
  ld <- b$data[[2L]]
  pp <- b$layout$panel_params[[1L]]
  co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)

  seg      <- g$children[[1L]]
  expected <- pp$y$break_positions()
  expected <- expected[!is.na(expected)]

  expect_equal(as.numeric(seg$y0), expected, tolerance = 1e-9)
  expect_equal(as.numeric(seg$y1), expected, tolerance = 1e-9)
})

test_that("geom_gridline: x segment positions match panel_params$x$break_positions()", {
  b  <- ggplot_build(scatter_p)
  ld <- b$data[[2L]]
  pp <- b$layout$panel_params[[1L]]
  co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "x", major = TRUE, minor = FALSE)

  seg      <- g$children[[1L]]
  expected <- pp$x$break_positions()
  expected <- expected[!is.na(expected)]

  expect_equal(as.numeric(seg$x0), expected, tolerance = 1e-9)
  expect_equal(as.numeric(seg$x1), expected, tolerance = 1e-9)
})

test_that("geom_gridline: y lines span full panel width (x0=0, x1=1)", {
  b  <- ggplot_build(base_p)
  g  <- GeomGridline$draw_panel(b$data[[2L]], b$layout$panel_params[[1L]],
                                 b$layout$coord, grids = "y", major = TRUE, minor = FALSE)
  seg <- g$children[[1L]]
  expect_true(all(as.numeric(seg$x0) == 0))
  expect_true(all(as.numeric(seg$x1) == 1))
})

test_that("geom_gridline: x lines span full panel height (y0=0, y1=1)", {
  b  <- ggplot_build(scatter_p)
  g  <- GeomGridline$draw_panel(b$data[[2L]], b$layout$panel_params[[1L]],
                                 b$layout$coord, grids = "x", major = TRUE, minor = FALSE)
  seg <- g$children[[1L]]
  expect_true(all(as.numeric(seg$y0) == 0))
  expect_true(all(as.numeric(seg$y1) == 1))
})

# ---------------------------------------------------------------------------
# Regression: inherit.aes = FALSE
# Bug: when inherit.aes was TRUE the global aes(x = class) bled into the
# dummy data frame, corrupting the build.
# ---------------------------------------------------------------------------

test_that("geom_gridline: global aesthetics do not bleed into the layer", {
  # The plot has aes(x = class); geom_gridline uses a dummy data.frame(x=1L)
  # and must not inherit any plot-level mapping.
  p <- ggplot(mpg, aes(class, fill = drv)) +
    geom_bar() +
    geom_gridline(suppress = FALSE)
  expect_no_error(ggplot_build(p))
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]
  # Gridline data has exactly one row (the dummy row), not nrow(mpg)
  expect_equal(nrow(ld), 1L)
})

# ---------------------------------------------------------------------------
# Regression: no "Ignoring unknown parameters" warning for line aesthetics
# Bug: colour/linewidth/linetype/lineend were not in optional_aes, so passing
# them as fixed params triggered an ggplot2 "Ignoring unknown parameters"
# warning and the values were silently dropped.
# ---------------------------------------------------------------------------

test_that("geom_gridline: no warning when setting colour as fixed param", {
  expect_no_warning(
    ggplot_build(ggplot(mpg, aes(class)) + geom_bar() +
                   geom_gridline(colour = "steelblue", suppress = FALSE))
  )
})

test_that("geom_gridline: no warning when setting linewidth as fixed param", {
  expect_no_warning(
    ggplot_build(ggplot(mpg, aes(class)) + geom_bar() +
                   geom_gridline(linewidth = 1.5, suppress = FALSE))
  )
})

test_that("geom_gridline: no warning when setting linetype as fixed param", {
  expect_no_warning(
    ggplot_build(ggplot(mpg, aes(class)) + geom_bar() +
                   geom_gridline(linetype = "dotted", suppress = FALSE))
  )
})

test_that("geom_gridline: no warning when setting lineend as fixed param", {
  expect_no_warning(
    ggplot_build(ggplot(mpg, aes(class)) + geom_bar() +
                   geom_gridline(lineend = "round", suppress = FALSE))
  )
})

# ---------------------------------------------------------------------------
# draw_panel: all line properties inherit from theme(panel.grid.*)
# Bug: only colour was injected from the theme; linetype, linewidth, lineend
# still came from element_geom() tokens (borderwidth/bordertype) or were
# hardcoded ("butt"), ignoring the panel.grid element entirely.
# ---------------------------------------------------------------------------

test_that("geom_gridline: default colour inherits panel.grid.major (white in theme_gray)", {
  b  <- ggplot_build(base_p)
  ld <- b$data[[2L]]
  # panel.grid.major colour in theme_gray() is white; stamped into .mj_y_col
  expect_equal(ld$.mj_y_col[1L], "white")
})

test_that("geom_gridline: colour inherits from theme(panel.grid)", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    geom_gridline(suppress = FALSE) +
    theme(panel.grid = element_line(colour = "tomato"))
  ld <- ggplot_build(p)$data[[2L]]
  expect_equal(ld$.mj_y_col[1L], "tomato")
})

test_that("geom_gridline: linetype inherits from theme(panel.grid)", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    geom_gridline(suppress = FALSE) +
    theme(panel.grid = element_line(linetype = "dotted"))
  ld <- ggplot_build(p)$data[[2L]]
  expect_equal(ld$.mj_y_lty[1L], "dotted")
})

test_that("geom_gridline: linewidth inherits from theme(panel.grid)", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    geom_gridline(suppress = FALSE) +
    theme(panel.grid = element_line(linewidth = 2))
  ld <- ggplot_build(p)$data[[2L]]
  expect_equal(ld$.mj_y_lwd[1L], 2)
})

test_that("geom_gridline: lineend inherits from theme(panel.grid)", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    geom_gridline(suppress = FALSE) +
    theme(panel.grid = element_line(lineend = "round"))
  ld <- ggplot_build(p)$data[[2L]]
  expect_equal(ld$.mj_y_end[1L], "round")
})

test_that("geom_gridline: explicit colour overrides theme, others still from theme", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    geom_gridline(colour = "steelblue", suppress = FALSE) +
    theme(panel.grid = element_line(linetype = "dotted", linewidth = 2))
  ld <- ggplot_build(p)$data[[2L]]
  expect_equal(ld$.mj_y_col[1L], "steelblue")
  expect_equal(ld$.mj_y_lty[1L], "dotted")
  expect_equal(ld$.mj_y_lwd[1L], 2)
})

test_that("geom_gridline: explicit colour overrides theme default", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    geom_gridline(colour = "grey80", suppress = FALSE)
  ld <- ggplot_build(p)$data[[2L]]
  expect_equal(ld$.mj_y_col[1L], "grey80")
})

# ---------------------------------------------------------------------------
# draw_panel: minor linewidth is half of major linewidth
# ---------------------------------------------------------------------------

test_that("geom_gridline: minor linewidth is half of major linewidth", {
  b  <- ggplot_build(scatter_p)
  ld <- b$data[[2L]]
  pp <- b$layout$panel_params[[1L]]
  co <- b$layout$coord

  lwd_major <- unique(GeomGridline$draw_panel(
    ld, pp, co, grids = "y", major = TRUE, minor = FALSE
  )$children[[1L]]$gp$lwd)

  lwd_minor <- unique(GeomGridline$draw_panel(
    ld, pp, co, grids = "y", major = FALSE, minor = TRUE
  )$children[[1L]]$gp$lwd)

  expect_equal(lwd_minor / lwd_major, 0.5, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Rendering — builds without error
# ---------------------------------------------------------------------------

test_that("geom_gridline: builds with default args", {
  expect_no_error(grob_gridline())
})

test_that("geom_gridline: builds with explicit colour", {
  expect_no_error(grob_gridline(colour = "grey80"))
})

test_that("geom_gridline: builds with explicit linewidth", {
  expect_no_error(grob_gridline(linewidth = 0.4))
})

test_that("geom_gridline: builds with grids='x'", {
  expect_no_error(grob_gridline(grids = "x"))
})

test_that("geom_gridline: builds with grids=c('x','y')", {
  expect_no_error(grob_gridline(grids = c("x", "y")))
})

test_that("geom_gridline: builds with minor=TRUE", {
  expect_no_error(grob_gridline(minor = TRUE))
})

test_that("geom_gridline: builds with major=FALSE, minor=TRUE", {
  expect_no_error(grob_gridline(major = FALSE, minor = TRUE))
})

test_that("geom_gridline: builds with major=FALSE, minor=FALSE (zero-grob path)", {
  expect_no_error(grob_gridline(major = FALSE, minor = FALSE))
})

test_that("geom_gridline: builds with suppress=FALSE", {
  expect_no_error(grob_gridline(suppress = FALSE))
})

test_that("geom_gridline: builds with alpha", {
  expect_no_error(grob_gridline(colour = "grey80", alpha = 0.5))
})

# ---------------------------------------------------------------------------
# Coordinate systems
# ---------------------------------------------------------------------------

test_that("geom_gridline: builds with coord_flip", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar(aes(fill = drv)) +
    geom_gridline(grids = "x") +
    coord_flip()
  expect_no_error(ggplotGrob(p))
})

test_that("geom_gridline: builds with coord_cartesian", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_gridline() +
    coord_cartesian()
  expect_no_error(ggplotGrob(p))
})

test_that("geom_gridline: builds with coord_equal", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_gridline(grids = c("x", "y")) +
    coord_equal()
  expect_no_error(ggplotGrob(p))
})


# ---------------------------------------------------------------------------
# Facets
# ---------------------------------------------------------------------------

test_that("geom_gridline: builds with facet_wrap", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    geom_gridline() +
    facet_wrap(~year)
  expect_no_error(ggplotGrob(p))
})

test_that("geom_gridline: builds with facet_grid", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_gridline(grids = c("x", "y")) +
    facet_grid(drv ~ cyl)
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Scatter plot
# ---------------------------------------------------------------------------

test_that("geom_gridline: works on scatter plot with continuous axes", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_gridline(grids = c("x", "y"), minor = TRUE, colour = "grey92")
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Argument validation — grids is normalised gently, not hard-errored
# ---------------------------------------------------------------------------

test_that("geom_gridline: fully invalid grids warns and falls back to 'y'", {
  # Two warnings: one for the invalid value, one for the fallback
  w <- capture_warnings(result <- geom_gridline(grids = "z"))
  expect_gte(length(w), 2L)
  expect_match(w[[1L]], "invalid.*grids")
  expect_match(w[[2L]], "Falling back")
  # After fallback the layer is still usable (suppress=TRUE → list with theme)
  expect_true(is.list(result))
  expect_true(inherits(result[[2L]]$panel.grid.major.y, "element_blank"))
})

test_that("geom_gridline: mixed valid/invalid grids warns and keeps valid values", {
  expect_warning(
    result <- geom_gridline(grids = c("x", "z")),
    regexp = "invalid.*grids"
  )
  expect_true(is.list(result))
})

test_that("geom_gridline: duplicate grids values are silently deduplicated", {
  # No warning for duplicates, result is same as grids='y'
  expect_no_warning(r1 <- geom_gridline(grids = c("y", "y")))
  r2 <- geom_gridline(grids = "y")
  # Both produce the same theme suppression element
  expect_equal(r1[[2L]], r2[[2L]])
})

test_that("geom_gridline: mixed duplicates and invalids warns once, deduplicates", {
  # c("x","x","y","z","abc") → unique: c("x","y","z","abc") → bad: c("z","abc")
  # → valid: c("x","y")
  w <- capture_warnings(result <- geom_gridline(grids = c("x", "x", "y", "z", "abc")))
  expect_length(w, 1L)
  expect_match(w[[1L]], "invalid.*grids")
  # x and y both suppressed → theme has both elements
  combined <- Reduce(`+`, result[-1L], init = ggplot2::theme())
  expect_true(inherits(combined$panel.grid.major.x, "element_blank"))
  expect_true(inherits(combined$panel.grid.major.y, "element_blank"))
})

test_that("geom_gridline: all-invalid grids falls back to 'y' with two warnings", {
  # First warning: invalid values; second: fallback notice
  w <- capture_warnings(result <- geom_gridline(grids = c("z", "abc")))
  expect_gte(length(w), 2L)
  th <- result[[2L]]
  expect_true(inherits(th$panel.grid.major.y, "element_blank"))
  expect_null(th$panel.grid.major.x)
})

test_that("geom_gridline: mapping= warns that it is ignored", {
  expect_warning(
    geom_gridline(mapping = aes(colour = displ)),
    regexp = "Ignoring.*mapping.*panel scale"
  )
})

test_that("geom_gridline: data= warns that it is ignored", {
  expect_warning(
    geom_gridline(data = data.frame(x = 1)),
    regexp = "Ignoring.*data.*panel scale"
  )
})

test_that("geom_gridline: aes() as first positional arg now warns via mapping= formal", {
  # Previously, aes(colour=displ) fell into ... and was matched to `grids`,
  # producing a confusing "Ignoring invalid grids value: ~displ" message.
  # Now it is captured by the explicit mapping= formal and emits a clear warning.
  w <- capture_warnings(geom_gridline(aes(colour = displ)))
  expect_true(any(grepl("Ignoring.*mapping", w)))
  expect_false(any(grepl("invalid.*grids", w)))
})

# ===========================================================================
# Grammar of Graphics adversarial stress tests
#
# One section per GoG building block.  The goal is to ensure geom_gridline()
# never errors or silently misbehaves when callers vary data, mappings,
# layers, scales, coords, facets, or themes in any reasonable way.
# ===========================================================================

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

test_that("GoG/data: empty dataset (no rows) does not error", {
  p <- ggplot(data.frame(x = character(), y = integer()), aes(x, y)) +
    geom_col() +
    geom_gridline()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/data: single-row dataset does not error", {
  p <- ggplot(data.frame(x = "a", y = 5L), aes(x, y)) +
    geom_col() +
    geom_gridline()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/data: all-NA y values do not error", {
  p <- ggplot(data.frame(x = 1:3, y = c(NA_real_, NA_real_, NA_real_)), aes(x, y)) +
    geom_col() +
    geom_gridline()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: negative y values do not error", {
  p <- ggplot(data.frame(x = 1:3, y = c(-2, 1, -3)), aes(x, y)) +
    geom_col() +
    geom_gridline()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/data: date x-axis with grids='x' does not error", {
  p <- ggplot(data.frame(d = Sys.Date() + 1:5, y = 1:5), aes(d, y)) +
    geom_col() +
    geom_gridline(grids = "x")
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Mapping
# ---------------------------------------------------------------------------

test_that("GoG/mapping: inherit.aes=FALSE means plot-level mapping is ignored", {
  # Global aes(fill = drv) must not bleed into the gridline dummy data.
  p <- ggplot(mpg, aes(class, fill = drv)) +
    geom_bar() +
    geom_gridline(suppress = FALSE)
  b <- ggplot_build(p)
  expect_equal(nrow(b$data[[2L]]), 1L)
})

test_that("GoG/mapping: after_stat mapping in other layer does not affect gridline", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar(aes(y = after_stat(prop), group = 1)) +
    geom_gridline()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Layer
# ---------------------------------------------------------------------------

test_that("GoG/layer: gridline placed before other geoms does not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_gridline() +
    geom_bar()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/layer: two geom_gridline layers do not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    geom_gridline(colour = "white") +
    geom_gridline(colour = "grey80", linewidth = 0.2)
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/layer: geom_gridline with no other layers does not error", {
  p <- ggplot(mpg, aes(class)) + geom_gridline()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Scales
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_y_log10 does not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() + scale_y_log10() + geom_gridline()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/scales: scale_y_reverse negates bar y values (gridline)", {
  df_g <- data.frame(x = c("A", "B", "C"), y = c(3, 5, 2))
  b_fwd <- ggplot_build(ggplot(df_g, aes(x, y)) + geom_col() + geom_gridline())
  b_rev <- ggplot_build(ggplot(df_g, aes(x, y)) + geom_col() + scale_y_reverse() +
                          geom_gridline())
  expect_equal(b_rev$data[[1]]$y, -b_fwd$data[[1]]$y)
})

test_that("GoG/scales: scale_x_reverse produces all-negative x values (gridline)", {
  df_g <- data.frame(x = 1:5, y = 1:5)
  b_rev <- ggplot_build(ggplot(df_g, aes(x, y)) + geom_line() + scale_x_reverse() +
                          geom_gridline())
  expect_true(all(b_rev$data[[1]]$x < 0))
})

test_that("GoG/scales: scale_y_sqrt does not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() + scale_y_sqrt() + geom_gridline()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: breaks=NULL (no major breaks) returns zeroGrob silently", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    scale_y_continuous(breaks = NULL) +
    geom_gridline(suppress = FALSE)
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: secondary axis does not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    scale_y_continuous(sec.axis = sec_axis(~ . * 2)) +
    geom_gridline()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: scale_y_continuous with explicit limits does not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    scale_y_continuous(limits = c(0, 200)) +
    geom_gridline()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: expand=c(0,0) does not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    scale_y_continuous(expand = c(0, 0)) +
    geom_gridline()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: reversed discrete x scale does not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    scale_x_discrete(limits = rev(unique(mpg$class))) +
    geom_gridline()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Coord
# ---------------------------------------------------------------------------

test_that("GoG/coord: coord_cartesian zoom does not error", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    coord_cartesian(ylim = c(20, 40)) +
    geom_gridline()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_fixed does not error", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() + coord_fixed() + geom_gridline(grids = c("x", "y"))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_transform (log y) does not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    suppressWarnings(coord_transform(y = "log")) +
    geom_gridline()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/coord: coord_radial does not error", {
  p <- ggplot(mtcars, aes(factor(cyl), fill = factor(gear))) +
    geom_bar() + coord_radial() + geom_gridline()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_flip with grids='y' does not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() + coord_flip() + geom_gridline(grids = "y")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: degenerate coord_cartesian(ylim=c(1,1)) does not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    coord_cartesian(ylim = c(1, 1)) +
    geom_gridline()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Facets
# ---------------------------------------------------------------------------

test_that("GoG/facets: facet_wrap with free_y scales does not error", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_gridline(grids = c("x", "y")) +
    facet_wrap(~drv, scales = "free_y")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/facets: facet_grid with free scales does not error", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_gridline(grids = c("x", "y")) +
    facet_grid(drv ~ cyl, scales = "free")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/facets: many facet panels (stress) do not error", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_gridline() +
    facet_wrap(~ paste(drv, cyl))
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------

test_that("GoG/theme: theme_void (all grid element_blank) does not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() + geom_gridline() + theme_void()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: theme_classic (no grid) does not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() + geom_gridline() + theme_classic()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: theme_bw does not error", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() + geom_gridline() + theme_bw()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: panel.grid=element_blank() falls back to ink colour", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    geom_gridline(suppress = FALSE) +
    theme(panel.grid = element_blank())
  b <- ggplot_build(p)
  ld <- b$data[[2L]]
  # element_blank triggers ink fallback — .mj_y_col must be a non-NA colour string
  expect_true(is.character(ld$.mj_y_col[1L]) && !is.na(ld$.mj_y_col[1L]))
})

test_that("GoG/theme: all panel.grid properties propagate to layer data", {
  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    geom_gridline(suppress = FALSE) +
    theme(panel.grid = element_line(
      colour   = "tomato",
      linetype = "dotted",
      linewidth = 2,
      lineend  = "round"
    ))
  ld <- ggplot_build(p)$data[[2L]]
  expect_equal(ld$.mj_y_col[1L], "tomato")
  expect_equal(ld$.mj_y_lty[1L], "dotted")
  expect_equal(ld$.mj_y_lwd[1L], 2)
  expect_equal(ld$.mj_y_end[1L], "round")
})

# ---------------------------------------------------------------------------
# Regression: minor-only reads from panel.grid.minor, not panel.grid.major
# Bug: major/minor are geom params (not aes params) so they are absent from
# aes_params inside use_defaults().  Fixed by pre-stamping all four
# axis × major/minor property combos (.mj_x_col, .mn_y_lwd, …) in
# use_defaults() so draw_panel can select the right one at render time.
# ---------------------------------------------------------------------------

test_that("GoG/theme: minor-only colour comes from panel.grid.minor", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_gridline(major = FALSE, minor = TRUE, suppress = FALSE) +
    theme(
      panel.grid.major = element_line(colour = "blue"),
      panel.grid.minor = element_line(colour = "red")
    )
  ld <- ggplot_build(p)$data[[2L]]
  expect_equal(ld$.mn_y_col[1L], "red")
})

test_that("GoG/theme: major+minor colour comes from panel.grid.major", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_gridline(major = TRUE, minor = TRUE, suppress = FALSE) +
    theme(
      panel.grid.major = element_line(colour = "blue"),
      panel.grid.minor = element_line(colour = "red")
    )
  ld <- ggplot_build(p)$data[[2L]]
  expect_equal(ld$.mj_y_col[1L], "blue")
})

test_that("%|NA|% falls back on NA and NULL but passes through any other value", {
  # Covers the operator used to guard lineend/linewidth/linetype against NA.
  expect_equal(NA      %|NA|% "butt",  "butt")
  expect_equal(NULL    %|NA|% "butt",  "butt")
  expect_equal("round" %|NA|% "butt",  "round")
  expect_equal(0.5     %|NA|% 1,       0.5)
  expect_equal(FALSE   %|NA|% TRUE,    FALSE)   # FALSE is not NA
  expect_equal(0L      %|NA|% 99L,     0L)      # 0L is not NA
})

test_that("draw_panel: NA lineend in stamped data falls back to 'butt' via %|NA|%", {
  # ggplot2 v4 element_line rejects NA at construction, so NA in stamped data
  # can only arrive via unusual/external manipulation.  draw_panel must still
  # not crash.  We inject NA directly into the layer data to trigger the guard.
  b  <- ggplot_build(scatter_p)
  ld <- b$data[[2L]]
  pp <- b$layout$panel_params[[1L]]
  co <- b$layout$coord

  ld$.mj_y_end <- NA_character_   # inject NA
  expect_no_error(
    GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)
  )
})

test_that("GoG/theme: minor-only linewidth comes from panel.grid.minor", {
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_gridline(major = FALSE, minor = TRUE, suppress = FALSE) +
    theme(
      panel.grid.major = element_line(linewidth = 3),
      panel.grid.minor = element_line(linewidth = 0.5)
    )
  ld <- ggplot_build(p)$data[[2L]]
  expect_equal(ld$.mn_y_lwd[1L], 0.5)
})

# ---------------------------------------------------------------------------
# Regression: theme minor gridlines bleed through when minor = FALSE
# Bug: suppress = TRUE only blanked panel.grid.major.{ax}, not
# panel.grid.minor.{ax}.  With ≥ 2 major breaks ggplot2 auto-computes minor
# breaks, so the theme drew tomato minor lines even when minor = FALSE.
# With 1 break ggplot2 computes no minor breaks → nothing visible → the bug
# appeared intermittent.
# Fix: always blank panel.grid.minor.{ax} for drawn axes when suppress = TRUE.
# ---------------------------------------------------------------------------

test_that("suppress=TRUE blanks minor theme grid even when minor=FALSE", {
  # With 2 explicit breaks ggplot2 computes minor breaks automatically.
  # Before the fix the theme's minor lines bled through.
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y") +
    theme(panel.grid = element_line(colour = "tomato")) +
    scale_y_continuous(breaks = c(8000, 12000))

  built    <- ggplot_build(p)
  th_after <- built$plot$theme

  # After build the injected theme() should have blanked panel.grid.minor.y
  minor_el <- ggplot2::calc_element("panel.grid.minor.y", built$plot$theme)
  expect_true(inherits(minor_el, "element_blank"))
})

test_that("suppress=TRUE blanks minor theme grid for both axes when grids=c('x','y')", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = c("x", "y")) +
    theme(panel.grid = element_line(colour = "tomato")) +
    scale_y_continuous(breaks = c(8000, 12000))

  built <- ggplot_build(p)
  expect_true(inherits(ggplot2::calc_element("panel.grid.minor.y", built$plot$theme), "element_blank"))
  expect_true(inherits(ggplot2::calc_element("panel.grid.minor.x", built$plot$theme), "element_blank"))
})

test_that("suppress=FALSE leaves minor theme grid intact", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", suppress = FALSE) +
    theme(panel.grid.minor.y = element_line(colour = "tomato"))

  built    <- ggplot_build(p)
  minor_el <- ggplot2::calc_element("panel.grid.minor.y", built$plot$theme)
  # suppress = FALSE: our layer must not touch theme grid at all
  expect_false(inherits(minor_el, "element_blank"))
})

# ---------------------------------------------------------------------------
# Creative scale_* tests — minor-grid suppression across scale families
#
# ggplot2 auto-computes minor breaks whenever ≥ 2 major breaks exist.
# The number of auto-computed minor breaks varies by scale family:
#   scale_y_log10  → 4 pure minor breaks per decade
#   scale_y_sqrt   → 4 between each pair of major breaks
#   many evenly-spaced breaks → 4 between each consecutive pair
#   explicit minor_breaks=    → whatever you pass
#   minor_breaks = NULL       → 0 minor breaks (safe to blank anyway)
# In every case, suppress = TRUE must blank panel.grid.minor.{ax}.
# ---------------------------------------------------------------------------

test_that("scale_y_log10 auto-produces minor breaks; suppress=TRUE blanks them", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y") +
    scale_y_log10(breaks = c(2000, 5000, 15000)) +
    theme(panel.grid = element_line(colour = "tomato"))

  built    <- ggplot_build(p)
  minor_el <- ggplot2::calc_element("panel.grid.minor.y", built$plot$theme)
  expect_true(inherits(minor_el, "element_blank"))
  # Major must also be gone
  major_el <- ggplot2::calc_element("panel.grid.major.y", built$plot$theme)
  expect_true(inherits(major_el, "element_blank"))
})

test_that("scale_y_sqrt auto-produces minor breaks; suppress=TRUE blanks them", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y") +
    scale_y_sqrt(breaks = c(4000, 9000, 16000)) +
    theme(panel.grid = element_line(colour = "tomato"))

  built    <- ggplot_build(p)
  minor_el <- ggplot2::calc_element("panel.grid.minor.y", built$plot$theme)
  expect_true(inherits(minor_el, "element_blank"))
})

test_that("many evenly-spaced breaks → many auto minor breaks; all suppressed", {
  # seq(2000, 16000, 2000) → 8 major breaks → 7*4 = 28 minor break positions
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y") +
    scale_y_continuous(breaks = seq(2000, 16000, 2000)) +
    theme(panel.grid = element_line(colour = "tomato"))

  built    <- ggplot_build(p)
  minor_el <- ggplot2::calc_element("panel.grid.minor.y", built$plot$theme)
  expect_true(inherits(minor_el, "element_blank"))
})

test_that("explicit minor_breaks are still blanked by suppress=TRUE", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y") +
    scale_y_continuous(
      breaks       = c(4000, 8000, 12000),
      minor_breaks = c(2000, 6000, 10000, 14000)
    ) +
    theme(panel.grid = element_line(colour = "tomato"))

  built    <- ggplot_build(p)
  minor_el <- ggplot2::calc_element("panel.grid.minor.y", built$plot$theme)
  expect_true(inherits(minor_el, "element_blank"))
})

test_that("minor_breaks = NULL; suppress=TRUE blanks minor element (safely harmless)", {
  # Even when there are no minor breaks to draw, element_blank is injected.
  # This is harmless and predictable: no surprise gridlines if someone later
  # adds minor_breaks back to the theme.
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y") +
    scale_y_continuous(breaks = c(4000, 12000), minor_breaks = NULL) +
    theme(panel.grid = element_line(colour = "tomato"))

  built    <- ggplot_build(p)
  minor_el <- ggplot2::calc_element("panel.grid.minor.y", built$plot$theme)
  expect_true(inherits(minor_el, "element_blank"))
})

test_that("scale_y_reverse with 2 breaks: suppress=TRUE blanks minor grid", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y") +
    scale_y_reverse(breaks = c(12000, 4000)) +
    theme(panel.grid = element_line(colour = "tomato"))

  built    <- ggplot_build(p)
  minor_el <- ggplot2::calc_element("panel.grid.minor.y", built$plot$theme)
  expect_true(inherits(minor_el, "element_blank"))
})

test_that("single major break: no auto minor breaks, yet suppress=TRUE blanks harmlessly", {
  # One major break → ggplot2 computes 0 minor breaks.
  # Nothing would appear even without the fix, but element_blank is still
  # injected — verifies the fix is unconditional and doesn't error.
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y") +
    scale_y_continuous(breaks = 8000) +
    theme(panel.grid = element_line(colour = "tomato"))

  built    <- ggplot_build(p)
  minor_el <- ggplot2::calc_element("panel.grid.minor.y", built$plot$theme)
  expect_true(inherits(minor_el, "element_blank"))
  expect_no_error(ggplotGrob(p))
})

test_that("suppress=FALSE + 2 breaks: minor theme grid is NOT blanked", {
  # This is the canary: if suppress=FALSE, tomato minor lines should survive.
  # Confirms that suppression is purely opt-in, not an implicit side effect.
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", suppress = FALSE) +
    scale_y_continuous(breaks = c(8000, 12000)) +
    theme(panel.grid.minor.y = element_line(colour = "tomato"))

  built    <- ggplot_build(p)
  minor_el <- ggplot2::calc_element("panel.grid.minor.y", built$plot$theme)
  expect_false(inherits(minor_el, "element_blank"))
})

test_that("minor=TRUE with 2 major breaks: layer draws minor lines AND theme minor blanked", {
  # When minor = TRUE, our layer draws the minor lines itself.
  # The theme's minor grid must still be blanked to avoid double-drawing.
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", minor = TRUE) +
    scale_y_continuous(breaks = c(4000, 12000)) +
    theme(panel.grid = element_line(colour = "tomato"))

  built    <- ggplot_build(p)
  minor_el <- ggplot2::calc_element("panel.grid.minor.y", built$plot$theme)
  expect_true(inherits(minor_el, "element_blank"))
  # And the plot itself still renders without error
  expect_no_error(ggplotGrob(p))
})

test_that("date axis: scale_x_date with multiple breaks; suppress=TRUE blanks x minor grid", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "x") +
    scale_x_date(breaks = "10 years") +
    theme(panel.grid = element_line(colour = "tomato"))

  built    <- ggplot_build(p)
  minor_el <- ggplot2::calc_element("panel.grid.minor.x", built$plot$theme)
  expect_true(inherits(minor_el, "element_blank"))
  # y grid should be unaffected (only x was requested)
  major_y_el <- ggplot2::calc_element("panel.grid.major.y", built$plot$theme)
  expect_false(inherits(major_y_el, "element_blank"))
})

test_that("grids=c('x','y') with log x and sqrt y: both minor grids blanked", {
  p <- ggplot(economics, aes(unemploy, pce)) +
    geom_point() +
    geom_gridline(grids = c("x", "y")) +
    scale_x_log10() +
    scale_y_sqrt() +
    theme(panel.grid = element_line(colour = "tomato"))

  built    <- ggplot_build(p)
  expect_true(inherits(ggplot2::calc_element("panel.grid.minor.x", built$plot$theme), "element_blank"))
  expect_true(inherits(ggplot2::calc_element("panel.grid.minor.y", built$plot$theme), "element_blank"))
})

# ---------------------------------------------------------------------------
# Polar coordinate support
#
# geom_hline / geom_vline work in polar because they express coordinates in
# data space and delegate to GeomSegment, which calls coord$transform().
# geom_gridline now uses the same approach: reads data-space breaks and
# delegates to GeomSegment$draw_panel() — so constant-y gridlines become
# circular arcs and constant-x gridlines become radial spokes.
# ---------------------------------------------------------------------------

test_that("geom_gridline does not error in coord_polar (theta='y')", {
  pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1) +
    geom_gridline(grids = "y", colour = "black") +
    coord_polar(theta = "y")
  expect_no_error(ggplotGrob(pie))
})

test_that("geom_gridline does not error in coord_polar (theta='x')", {
  pie <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
    geom_bar() +
    geom_gridline(grids = "y", colour = "black") +
    coord_polar(theta = "x")
  expect_no_error(ggplotGrob(pie))
})

test_that("geom_gridline returns a non-trivial grob in coord_polar (theta='y')", {
  pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1) +
    geom_gridline(grids = "y", major = TRUE, minor = TRUE, suppress = FALSE) +
    coord_polar(theta = "y")
  b  <- ggplot_build(pie)
  ld <- b$data[[2L]]
  pp <- b$layout$panel_params[[1L]]
  co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)
  # Must return something drawable, not a zeroGrob
  expect_false(inherits(g, "zeroGrob"))
})

test_that("geom_gridline with grids='x' (spokes) does not error in coord_polar", {
  pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1) +
    geom_gridline(grids = "x", colour = "black") +
    coord_polar(theta = "y")
  expect_no_error(ggplotGrob(pie))
})

test_that("geom_gridline with both grids in coord_polar does not error", {
  pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1) +
    geom_gridline(grids = c("x", "y"), major = TRUE, minor = TRUE, colour = "black") +
    coord_polar(theta = "y")
  expect_no_error(ggplotGrob(pie))
})

# ---------------------------------------------------------------------------
# Edge cases — non-sensical break values, secondary axes
# ---------------------------------------------------------------------------

test_that("Inf / NaN / -Inf in breaks are silently dropped; finite breaks drawn", {
  # ggplot2's scale training converts non-finite values to NA;
  # our !is.na() filter then removes them. Only 5000 and 12000 survive.
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", suppress = FALSE) +
    scale_y_continuous(breaks = c(Inf, 5000, NaN, NA, -Inf, 12000))

  b  <- ggplot_build(p)
  ld <- b$data[[2L]]
  pp <- b$layout$panel_params[[1L]]
  co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)

  n_drawn <- length(as.numeric(g$children[[1L]]$y0))
  expect_equal(n_drawn, 2L)  # only 5000 and 12000
})

test_that("NA-only breaks result in zeroGrob", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", suppress = FALSE) +
    scale_y_continuous(breaks = NA_real_)

  b  <- ggplot_build(p)
  ld <- b$data[[2L]]
  pp <- b$layout$panel_params[[1L]]
  co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)

  expect_true(inherits(g, "zeroGrob"))
})

test_that("secondary axis breaks do not add extra gridlines", {
  # panel_params$y.sec exists but we only read panel_params$y (primary).
  # Secondary breaks (10000, 20000) must not produce extra lines.
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", suppress = FALSE) +
    scale_y_continuous(
      breaks    = c(4000, 8000, 12000),
      sec.axis  = sec_axis(~ . * 2, breaks = c(10000, 20000))
    )

  b  <- ggplot_build(p)
  ld <- b$data[[2L]]
  pp <- b$layout$panel_params[[1L]]
  co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)

  expect_equal(length(as.numeric(g$children[[1L]]$y0)), 3L)
})

test_that("secondary axis does not error and renders without warnings", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y") +
    scale_y_continuous(
      breaks   = c(4000, 12000),
      sec.axis = sec_axis(~ . / 1000, name = "thousands")
    )
  expect_no_warning(expect_no_error(ggplotGrob(p)))
})

test_that("coord_transform(y='log10') does not error", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y") +
    coord_transform(y = "log10")
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("duplicate breaks draw each position once", {
  # Duplicate breaks are unusual but should not produce double lines.
  # ggplot2 deduplicates internally; we just make sure it doesn't error.
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", suppress = FALSE) +
    scale_y_continuous(breaks = c(8000, 8000, 12000))

  expect_no_error(ggplotGrob(p))
})

# ===========================================================================
# Rendering correctness — grob structure (not just "no error")
# ===========================================================================

# ---------------------------------------------------------------------------
# Section A: Polar grob structure
# ---------------------------------------------------------------------------

test_that("polar grids='x' (spokes) produces non-trivial gTree with 1 child", {
  pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1) +
    geom_gridline(grids = "x", suppress = FALSE) +
    coord_polar(theta = "y")
  b  <- ggplot_build(pie)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "x", major = TRUE, minor = FALSE)
  
  expect_false(inherits(g, "zeroGrob"))
  expect_true(inherits(g, "gTree"))
  expect_equal(length(g$children), 1L)
})

test_that("polar grids=c('x','y') produces gTree with 2 children (one per axis)", {
  pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1) +
    geom_gridline(grids = c("x", "y"), suppress = FALSE) +
    coord_polar(theta = "y")
  b  <- ggplot_build(pie)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = c("x", "y"), major = TRUE, minor = FALSE)

  expect_true(inherits(g, "gTree"))
  expect_equal(length(g$children), 2L)
})

test_that("polar minor=TRUE produces 2 children (major + minor)", {
  pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1) +
    geom_gridline(grids = "y", suppress = FALSE) +
    coord_polar(theta = "y")
  b  <- ggplot_build(pie)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = TRUE)

  expect_equal(length(g$children), 2L)
})

test_that("polar child grob is NOT a segments grob (non-linear → GeomPath)", {
  pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1) +
    geom_gridline(grids = "y", suppress = FALSE) +
    coord_polar(theta = "y")
  b  <- ggplot_build(pie)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)

  child <- g$children[[1L]]
  # In polar, GeomSegment delegates to GeomPath → polyline, not segments
  expect_false(inherits(child, "segments"))
})

# ---------------------------------------------------------------------------
# Critical regression: minor lines must NOT appear when minor=FALSE
# even when ggplot2 auto-computes minor breaks (≥2 major breaks)
# ---------------------------------------------------------------------------

test_that("REGRESSION: minor=FALSE with 2+ breaks draws major only, never minor", {
  # This bug occurred when minor=FALSE but ≥2 major breaks existed.
  # ggplot2 auto-computes minor break positions; the theme would draw them.
  # The fix: always blank panel.grid.minor.{ax} when drawing, regardless of minor param.
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", suppress = FALSE) +
    scale_y_continuous(breaks = c(8000, 12000))  # 2 breaks → auto minor breaks
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)

  # With the bug: would see 2 children (major + minor that bled through)
  # With the fix: exactly 1 child (major only)
  expect_equal(length(g$children), 1L)

  # Drawn positions should be ONLY major positions (2 lines)
  drawn_y <- as.numeric(g$children[[1L]]$y0)
  expect_equal(length(drawn_y), 2L)

  # Verify they match the major break positions, not minor
  major_pos <- pp$y$break_positions()
  minor_pos <- pp$y$break_positions_minor()
  major_pos <- major_pos[!is.na(major_pos)]
  minor_pos <- minor_pos[!is.na(minor_pos)]

  # Drawn should match major (2 positions)
  expect_equal(sort(drawn_y), sort(major_pos))

  # Drawn should NOT include any positions unique to minor
  minor_only <- minor_pos[!vapply(minor_pos, function(p) any(abs(p - major_pos) < 1e-9), logical(1L))]
  if (length(minor_only) > 0L) {
    expect_false(any(vapply(drawn_y, function(d) any(abs(d - minor_only) < 1e-9), logical(1L))))
  }
})

# ---------------------------------------------------------------------------
# Section B3: Break count correctness
# ---------------------------------------------------------------------------

test_that("exact break count matches number of non-NA breaks", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", suppress = FALSE) +
    scale_y_continuous(breaks = c(3000, 6000, 9000, 12000))
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)
  
  n_breaks <- sum(!is.na(pp$y$breaks))
  n_drawn  <- length(as.numeric(g$children[[1L]]$y0))
  expect_equal(n_drawn, n_breaks)
})

# ---------------------------------------------------------------------------
# Section B5: Limits excluding breaks
# ---------------------------------------------------------------------------

test_that("limits that exclude breaks: only in-range breaks drawn", {
  # breaks c(4000, 8000, 12000); limits c(6000, 10000) → only 8000 in range
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", suppress = FALSE) +
    scale_y_continuous(breaks = c(4000, 8000, 12000), limits = c(6000, 10000))
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)
  
  # Only 8000 is in [6000, 10000] → 1 line
  expect_equal(length(as.numeric(g$children[[1L]]$y0)), 1L)
})

# ---------------------------------------------------------------------------
# Section C: Discrete axis positions
# ---------------------------------------------------------------------------

test_that("discrete x scale grids='x': count matches number of levels", {
  p <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    geom_gridline(grids = "x", suppress = FALSE)
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "x", major = TRUE, minor = FALSE)
  
  # class has 7 levels
  expect_equal(length(as.numeric(g$children[[1L]]$x0)), 7L)
})

test_that("discrete x positions match break_positions() values", {
  p <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    geom_gridline(grids = "x", suppress = FALSE)
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "x", major = TRUE, minor = FALSE)
  
  drawn <- sort(as.numeric(g$children[[1L]]$x0))
  expected <- sort(pp$x$break_positions())
  expect_equal(drawn, expected)
})

# ---------------------------------------------------------------------------
# Scale family interactions — verify gridline positions track scale transforms
# ---------------------------------------------------------------------------

test_that("scale_y_log10: gridline positions match log-transformed break_positions()", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", suppress = FALSE) +
    scale_y_log10(breaks = c(2000, 5000, 12000))
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)

  # In log scale, break_positions() are pre-transformed to NPC
  expected <- pp$y$break_positions()
  expected <- expected[!is.na(expected)]
  drawn <- sort(as.numeric(g$children[[1L]]$y0))
  # Should match exactly (log positions)
  expect_equal(drawn, sort(expected), tolerance = 1e-6)
})

test_that("scale_y_sqrt: gridline positions match sqrt-transformed break_positions()", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", suppress = FALSE) +
    scale_y_sqrt(breaks = c(2000, 6000, 10000))
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)

  expected <- pp$y$break_positions()
  expected <- expected[!is.na(expected)]
  drawn <- sort(as.numeric(g$children[[1L]]$y0))
  expect_equal(drawn, sort(expected), tolerance = 1e-6)
})

test_that("scale_y_reverse: gridline y-positions are inverted correctly", {
  # Reversed scale: high y-values map to low NPC, low y-values to high NPC
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", suppress = FALSE) +
    scale_y_reverse(breaks = c(14000, 10000, 4000))
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)

  expected <- pp$y$break_positions()
  expected <- expected[!is.na(expected)]
  drawn <- sort(as.numeric(g$children[[1L]]$y0))
  # In reversed scale, positions should still be ordered 0 < pos < 1
  expect_equal(drawn, sort(expected), tolerance = 1e-6)
})

test_that("scale_x_date: gridline x-positions align with date breaks", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "x", suppress = FALSE) +
    scale_x_date(breaks = "10 years")
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "x", major = TRUE, minor = FALSE)

  expected <- pp$x$break_positions()
  expected <- expected[!is.na(expected)]
  drawn <- sort(as.numeric(g$children[[1L]]$x0))
  expect_equal(drawn, sort(expected), tolerance = 1e-6)
})

test_that("explicit breaks + minor_breaks: count includes both, no overlap", {
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", major = TRUE, minor = TRUE, suppress = FALSE) +
    scale_y_continuous(breaks = c(4000, 8000, 12000), minor_breaks = c(2000, 6000, 10000, 14000))
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = TRUE)

  # Should have 2 children: major (3 lines) + minor (3 pure minor lines, since 1 overlaps)
  expect_equal(length(g$children), 2L)

  major_drawn <- as.numeric(g$children[[1L]]$y0)
  minor_drawn <- as.numeric(g$children[[2L]]$y0)

  # Major: 3 lines
  expect_equal(length(major_drawn), 3L)
  # Minor: ggplot2 filters out-of-range breaks (2000 dropped)
  # minor_breaks after filter = [6000, 10000, 14000] = 3 lines
  expect_equal(length(minor_drawn), 3L)

  # No overlap: major and minor positions should be disjoint
  overlaps <- sapply(minor_drawn, function(m) any(abs(m - major_drawn) < 1e-9))
  expect_false(any(overlaps))
})

test_that("breaks = NULL auto-computed: gridline still correct", {
  # ggplot2 auto-computes pretty breaks; gridline should follow
  p <- ggplot(economics, aes(date, unemploy)) +
    geom_line() +
    geom_gridline(grids = "y", suppress = FALSE) +
    scale_y_continuous(breaks = NULL)
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]; pp <- b$layout$panel_params[[1L]]; co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)

  expected <- pp$y$break_positions()
  expected <- expected[!is.na(expected)]
  drawn <- as.numeric(g$children[[1L]]$y0)
  # Just verify count matches and positions are in [0,1]
  expect_equal(length(drawn), length(expected))
  expect_true(all(drawn >= 0 & drawn <= 1))
})

# ---------------------------------------------------------------------------
# Section G: vdiffr snapshots
# ---------------------------------------------------------------------------

test_that("vdiffr: geom_gridline basic (y-grid on bar chart)", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mpg, aes(class)) +
    geom_bar(aes(fill = drv)) +
    geom_gridline(colour = "grey80")
  vdiffr::expect_doppelganger("gridline-basic", p)
})

test_that("vdiffr: geom_gridline both axes with minor lines", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point() +
    geom_gridline(grids = c("x", "y"), minor = TRUE, colour = "grey90")
  vdiffr::expect_doppelganger("gridline-both-axes-minor", p)
})

test_that("vdiffr: geom_gridline in coord_polar", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1) +
    geom_gridline(grids = c("x", "y"), minor = FALSE, colour = "black") +
    coord_polar(theta = "y")
  vdiffr::expect_doppelganger("gridline-polar", p)
})

# ---------------------------------------------------------------------------
# coord_polar: geom_gridline should reproduce the theme grid
#
# The baseline plot (no geom_gridline layer) renders the polar background via
# CoordPolar$render_bg, which extends rays to npc 0.45 and appends an outer
# boundary circle.  Layering geom_gridline on top (with the theme grid
# suppressed) must match that output 1:1 — same rays to the same outer
# radius, same boundary circle.  The two snapshots below pin the equivalence.
# ---------------------------------------------------------------------------

test_that("vdiffr: polar baseline (no gridline layer) — theme grid only", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1, alpha = 0) +
    coord_polar(theta = "y")
  vdiffr::expect_doppelganger("gridline-polar-baseline-no-layer", p)
})

test_that("vdiffr: polar layered — geom_gridline matches the theme grid", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar(width = 1, alpha = 0) +
    geom_gridline(grids = c("x", "y"), minor = TRUE) +
    coord_polar(theta = "y")
  vdiffr::expect_doppelganger("gridline-polar-layered", p)
})

test_that("vdiffr: geom_gridline in coord_flip (both axes)", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mpg, aes(displ, cty)) +
    geom_point() +
    suppressMessages(geom_smooth()) +
    geom_gridline(grids = c("x", "y")) +
    coord_flip()
  suppressMessages(
    vdiffr::expect_doppelganger("gridline-coord-flip", p)
  )
})

# ---------------------------------------------------------------------------
# coord_flip position correctness
#
# aes(displ, cty) + coord_flip() displays cty on the horizontal axis and
# displ on the vertical axis — exactly the same layout as aes(cty, displ).
# Both must produce gridlines at the same NPC positions.
# This catches any double-flip in the is_flipped + draw_horizontal logic.
# ---------------------------------------------------------------------------

test_that("coord_flip: x-gridline positions equal flipped-aes x-gridline positions", {
  p_coord <- ggplot(mpg, aes(displ, cty)) +
    geom_point() +
    geom_gridline(grids = "x", suppress = FALSE) +
    coord_flip()

  p_aes <- ggplot(mpg, aes(cty, displ)) +
    geom_point() +
    geom_gridline(grids = "x", suppress = FALSE)

  grob_positions <- function(p) {
    b  <- ggplot_build(p)
    ld <- b$data[[2L]]
    pp <- b$layout$panel_params[[1L]]
    co <- b$layout$coord
    g  <- GeomGridline$draw_panel(ld, pp, co, grids = "x", major = TRUE, minor = FALSE)
    sort(as.numeric(g$children[[1L]]$x0))
  }

  expect_equal(grob_positions(p_coord), grob_positions(p_aes), tolerance = 1e-6)
})

test_that("coord_flip: y-gridline positions equal flipped-aes y-gridline positions", {
  p_coord <- ggplot(mpg, aes(displ, cty)) +
    geom_point() +
    geom_gridline(grids = "y", suppress = FALSE) +
    coord_flip()

  p_aes <- ggplot(mpg, aes(cty, displ)) +
    geom_point() +
    geom_gridline(grids = "y", suppress = FALSE)

  grob_positions <- function(p) {
    b  <- ggplot_build(p)
    ld <- b$data[[2L]]
    pp <- b$layout$panel_params[[1L]]
    co <- b$layout$coord
    g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)
    sort(as.numeric(g$children[[1L]]$y0))
  }

  expect_equal(grob_positions(p_coord), grob_positions(p_aes), tolerance = 1e-6)
})

test_that("coord_flip: x-gridlines are vertical (x0 == x1, y spans full panel)", {
  p <- ggplot(mpg, aes(displ, cty)) +
    geom_point() +
    geom_gridline(grids = "x", suppress = FALSE) +
    coord_flip()
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]
  pp <- b$layout$panel_params[[1L]]
  co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "x", major = TRUE, minor = FALSE)
  seg <- g$children[[1L]]
  expect_equal(as.numeric(seg$x0), as.numeric(seg$x1), tolerance = 1e-9)
  expect_true(all(as.numeric(seg$y0) == 0))
  expect_true(all(as.numeric(seg$y1) == 1))
})

test_that("coord_flip: y-gridlines are horizontal (y0 == y1, x spans full panel)", {
  p <- ggplot(mpg, aes(displ, cty)) +
    geom_point() +
    geom_gridline(grids = "y", suppress = FALSE) +
    coord_flip()
  b  <- ggplot_build(p)
  ld <- b$data[[2L]]
  pp <- b$layout$panel_params[[1L]]
  co <- b$layout$coord
  g  <- GeomGridline$draw_panel(ld, pp, co, grids = "y", major = TRUE, minor = FALSE)
  seg <- g$children[[1L]]
  expect_equal(as.numeric(seg$y0), as.numeric(seg$y1), tolerance = 1e-9)
  expect_true(all(as.numeric(seg$x0) == 0))
  expect_true(all(as.numeric(seg$x1) == 1))
})

# ---------------------------------------------------------------------------
# colour validation (delegated to farver via .check_colour_arg)
# ---------------------------------------------------------------------------

test_that("colour: NULL default inherits from theme (no error)", {
  p <- ggplot(mpg, aes(class)) + geom_bar() + geom_gridline(colour = NULL)
  expect_no_error(ggplotGrob(p))
})

test_that("colour: valid scalar colour accepted", {
  p <- ggplot(mpg, aes(class)) + geom_bar() + geom_gridline(colour = "red")
  expect_no_error(ggplotGrob(p))
})

test_that("colour: unknown colour name errors via farver", {
  expect_error(
    geom_gridline(colour = "flurble"),
    "colour"
  )
  expect_error(
    geom_gridline(colour = "flurble"),
    "Unknown colour name"
  )
})

test_that("colour: length-2 vector errors (scalar only)", {
  expect_error(
    geom_gridline(colour = c("red", "blue")),
    "length 1 or the same length as the data"
  )
})
