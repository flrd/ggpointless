library(ggplot2)

# ---------------------------------------------------------------------------
# .validate_radius() -- construction-time validator in R/aaa.R
# ---------------------------------------------------------------------------

test_that(".validate_radius: NULL returns default silently", {
  expect_no_message(
    out <- ggpointless:::.validate_radius(NULL)
  )
  expect_true(grid::is.unit(out))
  expect_equal(as.numeric(out), 0)
})

test_that(".validate_radius: bare positive numeric is coerced to points", {
  out <- expect_no_warning(ggpointless:::.validate_radius(5))
  expect_true(grid::is.unit(out))
  expect_equal(as.numeric(out), 5)
  # Round-trip via convertUnit confirms it was treated as points (1 pt == 1 pt).
  expect_equal(grid::convertUnit(out, "pt", valueOnly = TRUE), 5)
})

test_that(".validate_radius: bare zero is accepted", {
  out <- expect_no_warning(ggpointless:::.validate_radius(0))
  expect_equal(as.numeric(out), 0)
})

test_that(".validate_radius: valid unit passes through unchanged", {
  r <- grid::unit(8, "mm")
  out <- expect_no_warning(ggpointless:::.validate_radius(r))
  expect_identical(out, r)
})

test_that(".validate_radius: bare negative falls back with warning", {
  expect_warning(
    out <- ggpointless:::.validate_radius(-5),
    regexp = "non-negative"
  )
  expect_equal(as.numeric(out), 0)
})

test_that(".validate_radius: bare numeric vector falls back with warning", {
  expect_warning(
    out <- ggpointless:::.validate_radius(c(2, 5)),
    regexp = "single non-negative"
  )
  expect_equal(as.numeric(out), 0)
})

test_that(".validate_radius: bare NA falls back with warning", {
  expect_warning(
    out <- ggpointless:::.validate_radius(NA_real_),
    regexp = "single non-negative"
  )
  expect_equal(as.numeric(out), 0)
})

test_that(".validate_radius: bare Inf falls back with warning", {
  expect_warning(
    out <- ggpointless:::.validate_radius(Inf),
    regexp = "single non-negative"
  )
  expect_equal(as.numeric(out), 0)
})

test_that(".validate_radius: string falls back with warning", {
  expect_warning(
    out <- ggpointless:::.validate_radius("8mm"),
    regexp = "unit.*object|forget.*grid::unit"
  )
  expect_equal(as.numeric(out), 0)
})

test_that(".validate_radius: list falls back with warning", {
  expect_warning(
    out <- ggpointless:::.validate_radius(list(5)),
    regexp = "unit.*object|forget.*grid::unit"
  )
  expect_equal(as.numeric(out), 0)
})

test_that(".validate_radius: unit vector length > 1 falls back with warning", {
  expect_warning(
    out <- ggpointless:::.validate_radius(grid::unit(c(2, 5), "pt")),
    regexp = "single value.*length 2"
  )
  expect_equal(as.numeric(out), 0)
})

test_that(".validate_radius: negative unit falls back with warning", {
  expect_warning(
    out <- ggpointless:::.validate_radius(grid::unit(-3, "pt")),
    regexp = "non-negative"
  )
  expect_equal(as.numeric(out), 0)
})

test_that(".validate_radius: custom default is honoured", {
  custom <- grid::unit(2, "mm")
  expect_warning(
    out <- ggpointless:::.validate_radius(-1, default = custom),
    regexp = "Falling back"
  )
  expect_identical(out, custom)
})

test_that(".validate_radius: arg name appears in the message", {
  expect_warning(
    ggpointless:::.validate_radius(-1, arg = "my_radius"),
    regexp = "my_radius"
  )
})

# ---------------------------------------------------------------------------
# .clamp_roundrect_radius() -- draw-time clamp helper
# ---------------------------------------------------------------------------

test_that(".clamp_roundrect_radius: empty list passes through", {
  expect_identical(ggpointless:::.clamp_roundrect_radius(list()), list())
})

test_that(".clamp_roundrect_radius: non-roundrect grobs pass through", {
  rr <- grid::rectGrob()
  out <- ggpointless:::.clamp_roundrect_radius(grid::gList(rr))
  # gList comparison: same length, same first child class
  expect_equal(length(out), 1L)
  expect_true(inherits(out[[1]], "rect"))
})

test_that(".clamp_roundrect_radius: zero-area roundrects are silently clamped to r = 0", {
  # Regression: zero-count histogram bins under stat_bin produce
  # roundrects with h_pt = 0. A `roundrectGrob` with h = 0 and r > 0 is
  # NOT invisible -- the four quarter-circle corners overlap and render
  # as an hourglass/lens artefact. The clamp must therefore force r = 0
  # for these grobs (so they truly disappear), but stay silent because
  # the user can't fix empty bins by lowering radius.
  skip_if_not_installed("ragg")

  tmp <- tempfile(fileext = ".png")
  ragg::agg_png(tmp, 600, 400)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)

  zero_h <- grid::roundrectGrob(
    width = grid::unit(0.1, "npc"),
    height = grid::unit(0, "npc"),
    r = grid::unit(5, "pt")
  )
  visible <- grid::roundrectGrob(
    width = grid::unit(0.1, "npc"),
    height = grid::unit(0.5, "npc"),
    r = grid::unit(5, "pt")
  )
  glist <- grid::gList(zero_h, visible)

  msgs <- testthat::capture_messages(
    out <- ggpointless:::.clamp_roundrect_radius(glist)
  )
  # No "Maximum displayable radius" message: the user can't fix the
  # zero-area grob and the visible grob's cap is comfortably above 5 pt.
  expect_false(any(grepl("Maximum displayable radius", msgs)))
  # The zero-area grob's radius is now 0 pt (no hourglass artefact);
  # the visible grob keeps its 5 pt request.
  expect_equal(
    as.numeric(grid::convertUnit(out[[1]]$r, "pt", valueOnly = TRUE)),
    0
  )
  expect_equal(
    as.numeric(grid::convertUnit(out[[2]]$r, "pt", valueOnly = TRUE)),
    5
  )
})

test_that(".clamp_roundrect_radius: roundrect under cap is unchanged", {
  # Render a 100x100 pt viewport so width/height are deterministic.
  png_path <- tempfile(fileext = ".png")
  ragg::agg_png(png_path, width = 200, height = 200, scaling = 1)
  grid::pushViewport(grid::viewport(width = grid::unit(100, "pt"),
                                    height = grid::unit(100, "pt")))
  # Order matters: popViewport must happen BEFORE dev.off destroys the
  # whole stack. on.exit runs in registration order with add = TRUE.
  on.exit({
    grid::popViewport()
    if (dev.cur() != 1) dev.off()
  }, add = TRUE)

  rr <- grid::roundrectGrob(width = grid::unit(100, "pt"),
                            height = grid::unit(100, "pt"),
                            r = grid::unit(10, "pt"))
  out <- ggpointless:::.clamp_roundrect_radius(grid::gList(rr))
  expect_equal(as.numeric(grid::convertUnit(out[[1]]$r, "pt", valueOnly = TRUE)), 10)
})

test_that(".clamp_roundrect_radius: roundrect over cap is clamped + message emitted", {
  png_path <- tempfile(fileext = ".png")
  ragg::agg_png(png_path, width = 200, height = 200, scaling = 1)
  grid::pushViewport(grid::viewport(width = grid::unit(100, "pt"),
                                    height = grid::unit(100, "pt")))
  # Order matters: popViewport must happen BEFORE dev.off destroys the
  # whole stack. on.exit runs in registration order with add = TRUE.
  on.exit({
    grid::popViewport()
    if (dev.cur() != 1) dev.off()
  }, add = TRUE)

  rr <- grid::roundrectGrob(width = grid::unit(100, "pt"),
                            height = grid::unit(100, "pt"),
                            r = grid::unit(1000, "pt"))
  expect_message(
    out <- ggpointless:::.clamp_roundrect_radius(grid::gList(rr)),
    regexp = "exceeds the largest displayable corner radius"
  )
  expect_equal(
    as.numeric(grid::convertUnit(out[[1]]$r, "pt", valueOnly = TRUE)),
    50  # half of min(100, 100) = 50
  )
})

# ---------------------------------------------------------------------------
# Integration: the user's surprise case for geom_unit_histogram
# ---------------------------------------------------------------------------

test_that("geom_unit_histogram(radius = unit(1000, 'pt')) clamps + still renders cells", {
  # Before the validator/clamp work, this rendered a single solid panel
  # because each cell's roundrect expanded into a giant blob covering
  # everything.  Now the radius gets clamped at draw time.
  p <- ggplot(iris, aes(Sepal.Length, fill = Species)) +
    geom_unit_histogram(radius = grid::unit(1000, "pt"))
  expect_no_error(suppressWarnings(suppressMessages(ggplotGrob(p))))
})

test_that("geom_unit_histogram(radius = '8mm') falls back with warning, still renders", {
  p <- ggplot(iris, aes(Sepal.Length, fill = Species)) +
    geom_unit_histogram(radius = "8mm")
  expect_warning(
    suppressMessages(ggplotGrob(p)),
    regexp = "unit.*object|forget.*grid::unit"
  )
})

test_that("geom_col_fade(radius = -5) falls back with warning, still renders", {
  df <- data.frame(x = c("A", "B"), y = c(1, 2))
  p <- ggplot(df, aes(x, y)) + geom_col_fade(radius = -5)
  expect_warning(
    suppressMessages(ggplotGrob(p)),
    regexp = "non-negative"
  )
})

test_that("geom_rect_fade(radius = '8mm') falls back with warning, still renders", {
  # geom_rect_fade has its own setup_params + draw_panel + draw_key call sites
  # for `.validate_radius()`; this guards the wiring across all three.
  df <- data.frame(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  p <- ggplot(df) +
    geom_rect_fade(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                   radius = "8mm")
  expect_warning(
    suppressMessages(ggplotGrob(p)),
    regexp = "unit.*object|forget.*grid::unit"
  )
})
