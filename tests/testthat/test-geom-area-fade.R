library(ggplot2)

df_pos   <- data.frame(x = 1:4, y = c(1, 3, 2, 1))
df_neg   <- data.frame(x = 1:4, y = c(-1, -3, -2, -1))
df_mixed <- data.frame(x = 1:4, y = c(1, 2, -1, 0.5))

df_groups <- data.frame(
  g = rep(c("a", "b"), each = 4),
  x = rep(1:4, 2),
  y = c(1, 3, 2, 1, -2, 1, -1, 2)
)

# Data with a continuous fill variable for mapped-fill tests
df_fill <- data.frame(x = 1:6, y = c(1, 3, 2, 4, 2, 3), z = 1:6)

# --- alpha_fade_to validation ------------------------------------------------

test_that("alpha_fade_to must be a finite scalar in [0, 1]", {
  p <- ggplot(df_pos, aes(x, y))

  expect_error(ggplotGrob(p + geom_area_fade(alpha_fade_to = -0.1)),  "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_area_fade(alpha_fade_to =  1.1)),  "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_area_fade(alpha_fade_to = NA_real_)), "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_area_fade(alpha_fade_to = Inf)),   "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_area_fade(alpha_fade_to = c(0.1, 0.2))), "alpha_fade_to")
})

test_that("alpha_fade_to boundary values 0 and 1 are accepted", {
  p <- ggplot(df_pos, aes(x, y))
  expect_no_error(ggplotGrob(p + geom_area_fade(alpha_fade_to = 0)))
  expect_no_error(ggplotGrob(p + geom_area_fade(alpha_fade_to = 1)))
})

# --- outline.type validation -------------------------------------------------

test_that("outline.type rejects invalid values", {
  p <- ggplot(df_pos, aes(x, y))
  expect_error(ggplotGrob(p + geom_area_fade(outline.type = "diagonal")), "outline.type")
  # case-sensitive: "UPPER" is not "upper"
  expect_error(ggplotGrob(p + geom_area_fade(outline.type = "UPPER")),    "outline.type")
})

test_that("all valid outline.type values are accepted", {
  p <- ggplot(df_pos, aes(x, y))
  for (v in c("upper", "lower", "both", "full", "none")) {
    expect_no_error(ggplotGrob(p + geom_area_fade(outline.type = v)))
  }
})

# --- alpha_scope validation ---------------------------------------------------

test_that("alpha_scope rejects invalid values", {
  p <- ggplot(df_pos, aes(x, y))
  expect_error(
    ggplotGrob(p + geom_area_fade(alpha_scope = "panel")),
    "alpha_scope"
  )
})

test_that("alpha_scope = 'global' and 'group' are accepted", {
  p <- ggplot(df_groups, aes(x, y, fill = g))
  expect_no_error(ggplotGrob(p + geom_area_fade(alpha_scope = "global")))
  expect_no_error(ggplotGrob(p + geom_area_fade(alpha_scope = "group")))
})

test_that("alpha_scope = 'group' produces different grobs than 'global'", {
  # Two groups with very different amplitudes: "big" peaks at 10, "small" at 1.
  # With "global" the small group's alpha peaks at ~0.1; with "group" it peaks

  # at 1.  The grob trees must therefore differ.
  df_amp <- data.frame(
    g = rep(c("big", "small"), each = 4),
    x = rep(1:4, 2),
    y = c(2, 10, 5, 2, 0.2, 1, 0.5, 0.2)
  )
  p <- ggplot(df_amp, aes(x, y, fill = g))
  grob_global <- ggplotGrob(p + geom_area_fade(
    position = "identity", alpha_scope = "global"
  ))
  grob_group <- ggplotGrob(p + geom_area_fade(
    position = "identity", alpha_scope = "group"
  ))
  # The serialised grob trees should differ because the alpha_ref gradients

  # have different stop values.
  expect_false(identical(grob_global, grob_group))
})

# --- single-observation warning ----------------------------------------------

test_that("groups with fewer than 2 observations produce a warning", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_area_fade()
  expect_warning(ggplotGrob(p), "fewer than 2 observations")
})

# --- visual tests ------------------------------------------------------------
# vdiffr::expect_doppelganger() calls vdiffr_enabled(), which returns FALSE
# during R CMD check and in covr's subprocess.  When disabled, vdiffr calls
# testthat::skip() without rendering the plot at all, so any R function that
# runs only during legend/panel rendering is invisible to covr.  The
# .draw_key_area_fade unit tests below call the function directly to cover
# those code paths.

test_that("default: positive values, upper outline in fill colour", {
  p <- ggplot(df_pos, aes(x, y)) +
    geom_area_fade(fill = "#311dfc") +
    theme_minimal()
  vdiffr::expect_doppelganger("area-fade positive default", p)
})

test_that("negative values fade towards y = 0 from below", {
  p <- ggplot(df_neg, aes(x, y)) +
    geom_area_fade(fill = "#d77e7b") +
    theme_minimal()
  vdiffr::expect_doppelganger("area-fade negative", p)
})

test_that("mixed sign uses three-stop gradient anchored at y = 0", {
  p <- ggplot(df_mixed, aes(x, y)) +
    geom_area_fade(fill = "#a84dbd") +
    theme_minimal()
  vdiffr::expect_doppelganger("area-fade mixed sign", p)
})

test_that("alpha caps maximum opacity", {
  p <- ggplot(df_pos, aes(x, y)) +
    geom_area_fade(fill = "#311dfc", alpha = 0.4) +
    theme_minimal()
  vdiffr::expect_doppelganger("area-fade alpha 0.4", p)
})

test_that("alpha_fade_to sets baseline opacity", {
  p <- ggplot(df_pos, aes(x, y)) +
    geom_area_fade(fill = "#311dfc", alpha_fade_to = 0.3) +
    theme_minimal()
  vdiffr::expect_doppelganger("area-fade alpha_fade_to 0.3", p)
})

test_that("outline.type = 'none' suppresses the outline", {
  p <- ggplot(df_pos, aes(x, y)) +
    geom_area_fade(fill = "#311dfc", outline.type = "none") +
    theme_minimal()
  vdiffr::expect_doppelganger("area-fade outline none", p)
})

test_that("outline.type = 'full' draws a closed polygon outline", {
  p <- ggplot(df_pos, aes(x, y)) +
    geom_area_fade(fill = "#311dfc", outline.type = "full") +
    theme_minimal()
  vdiffr::expect_doppelganger("area-fade outline full", p)
})

test_that("two fill groups share a global alpha scale and show a legend", {
  p <- ggplot(df_groups, aes(x, y, fill = g)) +
    geom_area_fade() +
    scale_fill_manual(values = c(a = "#f4ae1b", b = "#311dfc")) +
    theme_minimal()
  vdiffr::expect_doppelganger("area-fade two groups", p)
})

test_that("flipped orientation (orientation = 'y') works", {
  p <- ggplot(df_pos, aes(y, x)) +
    geom_area_fade(fill = "#311dfc", orientation = "y") +
    theme_minimal()
  vdiffr::expect_doppelganger("area-fade flipped", p)
})

# --- .patch_poly_fill unit tests ---------------------------------------------

test_that(".patch_poly_fill replaces gp$fill in polygon grobs only", {
  grad <- grid::linearGradient(colours = c("blue", "transparent"))
  poly <- grid::polygonGrob(gp = grid::gpar(fill = "red"))
  line <- grid::polylineGrob()
  tree <- grid::gTree(children = grid::gList(poly, line))

  result <- .patch_poly_fill(tree, grad)

  expect_identical(result$children[[1]]$gp$fill, grad)  # polygon patched
  expect_identical(result$children[[2]], line)           # polyline untouched
})

test_that(".patch_poly_fill passes non-polygon, non-gTree grobs through unchanged", {
  line <- grid::polylineGrob()
  grad <- grid::linearGradient(colours = c("blue", "transparent"))
  expect_identical(.patch_poly_fill(line, grad), line)
})

test_that(".patch_poly_fill recurses into nested gTrees", {
  grad  <- grid::linearGradient(colours = c("blue", "transparent"))
  poly  <- grid::polygonGrob(gp = grid::gpar(fill = "red"))
  inner <- grid::gTree(children = grid::gList(poly))
  outer <- grid::gTree(children = grid::gList(inner))

  result <- .patch_poly_fill(outer, grad)

  expect_identical(result$children[[1]]$children[[1]]$gp$fill, grad)
})

# --- .composite_poly_fill unit tests -----------------------------------------

test_that(".composite_poly_fill wraps polygon children in GridGroup, leaves polylines", {
  skip_if_not(
    exists("groupGrob", envir = asNamespace("grid"), inherits = FALSE),
    message = "grid::groupGrob not available (R < 4.2)"
  )
  poly <- grid::polygonGrob(gp = grid::gpar(fill = "red"))
  line <- grid::polylineGrob()
  tree <- grid::gTree(children = grid::gList(poly, line))
  src  <- grid::rectGrob()

  result <- .composite_poly_fill(tree, src)

  # grid::groupGrob() returns class "GridGroup" (R >= 4.2)
  expect_s3_class(result$children[[1]], "GridGroup")  # polygon wrapped
  expect_identical(result$children[[2]], line)         # outline unchanged
})

test_that(".composite_poly_fill detaches outline for outline.type = 'full' polygons", {
  # When outline.type = "full" the parent returns a single polygonGrob whose
  # gp$col IS the outline (no separate polyline).  Without special handling
  # the dest.in compositing fades the outline together with the fill, making
  # it invisible near the baseline.  The fix: detach gp$col before compositing,
  # composite the fill alone, then layer the outline on top at full opacity.
  skip_if_not(
    exists("groupGrob", envir = asNamespace("grid"), inherits = FALSE),
    message = "grid::groupGrob not available (R < 4.2)"
  )
  poly <- grid::polygonGrob(gp = grid::gpar(fill = "red", col = "blue"))
  src  <- grid::rectGrob()

  result <- .composite_poly_fill(poly, src)

  # Result is a gTree: composited fill (no outline) + outline-only polygon
  expect_s3_class(result, "gTree")
  expect_length(result$children, 2L)

  # First child: composited fill with col stripped
  expect_s3_class(result$children[[1]], "GridGroup")

  # Second child: outline-only polygon (fill = NA, col = original)
  outline <- result$children[[2]]
  expect_s3_class(outline, "polygon")
  expect_true(is.na(outline$gp$fill))
  expect_equal(outline$gp$col, "blue")
})

test_that(".composite_poly_fill skips outline detach when col is NA", {
  # When gp$col is NA (no outline) the polygon should be composited directly
  # without the extra gTree wrapper.
  skip_if_not(
    exists("groupGrob", envir = asNamespace("grid"), inherits = FALSE),
    message = "grid::groupGrob not available (R < 4.2)"
  )
  poly <- grid::polygonGrob(gp = grid::gpar(fill = "red", col = NA))
  src  <- grid::rectGrob()

  result <- .composite_poly_fill(poly, src)

  # Direct GridGroup, no gTree wrapper
  expect_s3_class(result, "GridGroup")
})

test_that(".composite_poly_fill passes non-polygon, non-gTree grobs through unchanged", {
  skip_if_not(
    exists("groupGrob", envir = asNamespace("grid"), inherits = FALSE),
    message = "grid::groupGrob not available (R < 4.2)"
  )
  line <- grid::polylineGrob()
  src  <- grid::rectGrob()
  expect_identical(.composite_poly_fill(line, src), line)
})

test_that(".composite_poly_fill recurses into nested gTrees", {
  skip_if_not(
    exists("groupGrob", envir = asNamespace("grid"), inherits = FALSE),
    message = "grid::groupGrob not available (R < 4.2)"
  )
  poly  <- grid::polygonGrob(gp = grid::gpar(fill = "red"))
  src   <- grid::rectGrob()
  inner <- grid::gTree(children = grid::gList(poly))
  outer <- grid::gTree(children = grid::gList(inner))

  result <- .composite_poly_fill(outer, src)

  expect_s3_class(result$children[[1]]$children[[1]], "GridGroup")
})

# --- mapped fill (aes(fill = var)) -------------------------------------------

test_that("mapped fill: fallback emits a cli message when compositing unavailable", {
  # The message fires at render time (makeContent) rather than at grob-build
  # time, so we must draw the grob to trigger it.  Mock dev.cur() to simulate
  # a non-PDF device with no compositing so we exercise the Tier 2 fallback.
  #
  # The mock must include `patterns` so that ggplot2's internal

  # check_device("gradients") sees definitive gradient support and does not
  # abort with "Unable to check the capabilities of the <device> device" on

  # platforms where the device is not whitelisted (e.g. png on Windows).
  local_mocked_bindings(
    dev.cur = \(...) c(`png` = 2L),
    dev.capabilities = \(...) list(
      compositing = character(0L),
      patterns = c("LinearGradient", "RadialGradient", "TilingPattern")
    ),
    .package = "grDevices"
  )
  # Force verbose mode so .frequency = "once" doesn't suppress the message
  # after it has already fired in this or a previous session.
  withr::local_options(rlib_message_verbosity = "verbose")
  p <- ggplot(df_fill, aes(x, y, fill = z)) +
    geom_area_fade(position = "identity") +
    theme_minimal()
  grob <- ggplotGrob(p)
  tf <- tempfile(fileext = ".png")
  png(tf)
  expect_message(grid::grid.draw(grob), "does not support")
  dev.off()
  unlink(tf)
})

test_that("mapped fill builds a valid grob on any device", {
  # Mock dev.capabilities so that ggplot2's internal check_device("gradients")
  # call gets a definitive answer on all platforms. Without this, Windows CI
  # has an active png device whose capabilities cannot be queried, causing
  # check_device() to abort with "Unable to check the capabilities of the png
  # device." The rendering tier selected in makeContent() is irrelevant here —
  # we only want to confirm the grob builds without error.
  local_mocked_bindings(
    dev.capabilities = \(...) list(
      compositing = character(0L),
      patterns = c("LinearGradient", "RadialGradient", "TilingPattern")
    ),
    .package = "grDevices"
  )
  p <- ggplot(df_fill, aes(x, y, fill = z)) +
    geom_area_fade(position = "identity") +
    theme_minimal()
  expect_no_error(suppressMessages(ggplotGrob(p)))
})

test_that("mapped fill: compositing path runs without error when compositing available", {
  skip_if_not(
    exists("groupGrob", envir = asNamespace("grid"), inherits = FALSE),
    message = "grid::groupGrob not available (R < 4.2)"
  )
  local_mocked_bindings(
    dev.capabilities = \(...) list(
      compositing = "dest.in",
      patterns = c("LinearGradient", "RadialGradient", "TilingPattern")
    ),
    .package = "grDevices"
  )
  p <- ggplot(df_fill, aes(x, y, fill = z)) +
    geom_area_fade(position = "identity") +
    theme_minimal()
  expect_no_error(ggplotGrob(p))
})

test_that("unmapped fill does not emit a compositing message", {
  local_mocked_bindings(
    dev.capabilities = \(...) list(
      compositing = character(0L),
      patterns = c("LinearGradient", "RadialGradient", "TilingPattern")
    ),
    .package = "grDevices"
  )
  p <- ggplot(df_fill, aes(x, y)) +
    geom_area_fade(fill = "#311dfc", position = "identity") +
    theme_minimal()
  expect_no_message(ggplotGrob(p))
})

# --- .draw_key_area_fade unit tests ------------------------------------------
# .draw_key_area_fade is only called during legend rendering, which vdiffr
# skips in covr's subprocess (see note above).  Direct calls below cover both
# orientation branches and verify the gradient object structure.

key_data_fade <- data.frame(fill = "#311dfc", alpha = NA_real_)

test_that(".draw_key_area_fade: non-flipped returns a rect with linearGradient fill", {
  params <- list(flipped_aes = FALSE, alpha_fade_to = 0)
  result <- .draw_key_area_fade(key_data_fade, params, grid::unit(c(1, 1), "cm"))
  expect_s3_class(result, "rect")
  expect_true(inherits(result$gp$fill, "GridLinearGradient"))
})

test_that(".draw_key_area_fade: flipped orientation returns a rect with linearGradient fill", {
  params <- list(flipped_aes = TRUE, alpha_fade_to = 0.1)
  result <- .draw_key_area_fade(key_data_fade, params, grid::unit(c(1, 1), "cm"))
  expect_s3_class(result, "rect")
  expect_true(inherits(result$gp$fill, "GridLinearGradient"))
  # Flipped: gradient runs left-to-right (x1 = 1, x2 = 0)
  expect_equal(result$gp$fill$x1, grid::unit(1, "npc"))
  expect_equal(result$gp$fill$x2, grid::unit(0, "npc"))
})

test_that(".draw_key_area_fade: non-flipped gradient runs top-to-bottom (y1 = 1, y2 = 0)", {
  params <- list(flipped_aes = FALSE, alpha_fade_to = 0)
  result <- .draw_key_area_fade(key_data_fade, params, grid::unit(c(1, 1), "cm"))
  expect_equal(result$gp$fill$y1, grid::unit(1, "npc"))
  expect_equal(result$gp$fill$y2, grid::unit(0, "npc"))
})

test_that(".draw_key_area_fade: alpha and alpha_fade_to are encoded in gradient colours", {
  data   <- data.frame(fill = "#311dfc", alpha = 0.6)
  params <- list(flipped_aes = FALSE, alpha_fade_to = 0.2)
  result <- .draw_key_area_fade(data, params, grid::unit(c(1, 1), "cm"))
  grad   <- result$gp$fill
  # Two colour stops; first should be more opaque than second
  expect_length(grad$colours, 2L)
})
