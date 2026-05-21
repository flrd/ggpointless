library(ggplot2)

# -----------------------------------------------------------------------
# Shared test data
# -----------------------------------------------------------------------

# Three groups at distinct y-offsets, each with a simple triangular ridge
df <- data.frame(
  x      = rep(seq(0, 1, length.out = 5), 3),
  y      = rep(c(1, 2, 3), each = 5),
  height = c(0, 1, 2, 1, 0,   # group 1
             0, 2, 4, 2, 0,   # group 2 (tallest)
             0, 0.5, 1, 0.5, 0), # group 3 (shortest)
  group  = rep(c("a", "b", "c"), each = 5)
)

# Two-group subset for simpler alpha checks
df2 <- data.frame(
  x      = rep(c(0, 1, 2), 2),
  y      = rep(c(1, 2), each = 3),
  height = c(0, 3, 0,   # group 1: peak = 3
             0, 1, 0),  # group 2: peak = 1
  group  = rep(c("a", "b"), each = 3)
)


# -----------------------------------------------------------------------
# PositionRidgeline
# -----------------------------------------------------------------------

test_that("geom_ridgeline_fade always uses PositionRidgeline", {
  layer <- geom_ridgeline_fade()
  expect_s3_class(layer$position, "PositionRidgeline")
})

test_that("scale and min_height are forwarded to PositionRidgeline", {
  layer <- geom_ridgeline_fade(scale = 3, min_height = 0.5)
  expect_equal(layer$position$scale, 3)
  expect_equal(layer$position$min_height, 0.5)
})

test_that("PositionRidgeline$compute_panel sets ymin = y and ymax = y + scale * height", {
  pos    <- position_ridgeline(scale = 1)
  params <- pos$setup_params(df)
  result <- pos$compute_panel(df, params, scales = NULL)
  expect_equal(result$ymin, df$y)
  expect_equal(result$ymax, df$y + df$height)
})

test_that("PositionRidgeline$compute_panel respects scale", {
  pos    <- position_ridgeline(scale = 2)
  params <- pos$setup_params(df)
  result <- pos$compute_panel(df, params, scales = NULL)
  expect_equal(result$ymax, df$y + 2 * df$height)
})

test_that("PositionRidgeline$compute_panel clips heights below min_height to NA", {
  df_clip <- data.frame(x = 1:3, y = 1, height = c(0.5, 1.5, 2.5), group = "a")
  pos     <- position_ridgeline(min_height = 1)
  params  <- pos$setup_params(df_clip)
  result  <- pos$compute_panel(df_clip, params, scales = NULL)
  expect_true(is.na(result$ymax[1]))    # 0.5 < 1 → clipped
  expect_false(is.na(result$ymax[2]))   # 1.5 >= 1 → kept
  expect_false(is.na(result$ymax[3]))   # 2.5 >= 1 → kept
})

test_that("position_ridgeline() validates scale", {
  expect_error(
    ggplotGrob(
      ggplot(df, aes(x, y, height = height, group = group)) +
        geom_ridgeline_fade(scale = Inf)
    ),
    regexp = "scale"
  )
})

test_that("position_ridgeline() validates min_height", {
  expect_error(
    ggplotGrob(
      ggplot(df, aes(x, y, height = height, group = group)) +
        geom_ridgeline_fade(min_height = NA_real_)
    ),
    regexp = "min_height"
  )
})


# -----------------------------------------------------------------------
# Validation: alpha_fade_to
# -----------------------------------------------------------------------

test_that("alpha_fade_to must be a finite scalar in [0, 1]", {
  p <- ggplot(df, aes(x, y, height = height, group = group))
  expect_error(ggplotGrob(p + geom_ridgeline_fade(alpha_fade_to = -0.1)),  "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_ridgeline_fade(alpha_fade_to =  1.1)),  "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_ridgeline_fade(alpha_fade_to = NA_real_)), "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_ridgeline_fade(alpha_fade_to = Inf)),   "alpha_fade_to")
})

test_that("alpha_fade_to boundary values 0 and 1 are accepted", {
  p <- ggplot(df, aes(x, y, height = height, group = group))
  expect_no_error(ggplotGrob(p + geom_ridgeline_fade(alpha_fade_to = 0)))
  expect_no_error(ggplotGrob(p + geom_ridgeline_fade(alpha_fade_to = 1)))
})


# -----------------------------------------------------------------------
# Validation: alpha_scope
# -----------------------------------------------------------------------

test_that("alpha_scope rejects invalid values", {
  p <- ggplot(df, aes(x, y, height = height, group = group))
  expect_error(ggplotGrob(p + geom_ridgeline_fade(alpha_scope = "bar")),   "alpha_scope")
  expect_error(ggplotGrob(p + geom_ridgeline_fade(alpha_scope = "panel")), "alpha_scope")
})

test_that("all valid alpha_scope values are accepted", {
  p <- ggplot(df, aes(x, y, height = height, group = group))
  for (s in c("area", "group", "global")) {
    expect_no_error(ggplotGrob(p + geom_ridgeline_fade(alpha_scope = s)))
  }
})


# -----------------------------------------------------------------------
# Validation: outline.type
# -----------------------------------------------------------------------

test_that("outline.type rejects invalid values", {
  p <- ggplot(df, aes(x, y, height = height, group = group))
  expect_error(ggplotGrob(p + geom_ridgeline_fade(outline.type = "diagonal")), "outline.type")
  expect_error(ggplotGrob(p + geom_ridgeline_fade(outline.type = "UPPER")),    "outline.type")
})

test_that("all valid outline.type values are accepted", {
  p <- ggplot(df, aes(x, y, height = height, group = group))
  for (v in c("upper", "lower", "both", "full", "none")) {
    expect_no_error(ggplotGrob(p + geom_ridgeline_fade(outline.type = v)))
  }
})


# -----------------------------------------------------------------------
# setup_data: ymin / ymax / min_height
# -----------------------------------------------------------------------

test_that("setup_data stamps .alpha_scope into data", {
  params <- list(
    scale = 1, min_height = 0, alpha_scope = "global",
    alpha_fade_to = 0, flipped_aes = FALSE, outline.type = "upper"
  )
  result <- GeomRidgelineFade$setup_data(df, params)
  expect_true(".alpha_scope" %in% names(result))
  expect_true(all(result$.alpha_scope == "global"))
})


# -----------------------------------------------------------------------
# Rendering: basic smoke tests
# -----------------------------------------------------------------------

test_that("basic ridgeline plot builds without error", {
  p <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("single-group ridgeline builds without error", {
  p <- ggplot(df[df$group == "a", ], aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("scale parameter affects plot output", {
  p1 <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(scale = 1)
  p2 <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(scale = 3)
  expect_false(identical(ggplotGrob(p1), ggplotGrob(p2)))
})

test_that("min_height clips low ridges (plots differ from unclipped)", {
  p1 <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(min_height = 0)
  p2 <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(min_height = 1.5)
  expect_false(identical(ggplotGrob(p1), ggplotGrob(p2)))
})

test_that("all three alpha_scope values produce pairwise-different output", {
  mk <- \(scope) {
    ggplotGrob(
      ggplot(df, aes(x, y, height = height, group = group)) +
        geom_ridgeline_fade(alpha_scope = scope)
    )
  }
  g_area   <- mk("area")
  g_group  <- mk("group")
  g_global <- mk("global")
  expect_false(identical(g_area,  g_group))
  expect_false(identical(g_area,  g_global))
  expect_false(identical(g_group, g_global))
})

test_that("group coarser than y: each (group, y) pair becomes its own ridge", {
  # When group is mapped to something that cuts across multiple y-levels,
  # splitting by interaction(group, y) should produce one ridge per
  # (group, y) combination rather than one tangled ribbon per group.
  df_multi <- data.frame(
    x      = rep(c(0, 1, 2), 4),
    y      = rep(c(1, 2), each = 6),
    height = c(0, 1, 0,  0, 2, 0,    # y = 1
               0, 1.5, 0, 0, 0.5, 0), # y = 2
    fill   = rep(c("red", "blue"), times = 6)
  )
  # 2 fill values × 2 y levels = 4 ridges; plot must build without error
  p <- ggplot(df_multi, aes(x, y, height = height, group = fill, fill = fill)) +
    geom_ridgeline_fade()
  expect_no_error(ggplotGrob(p))

  # Output must differ from a plot where group == y (normal case):
  # if the two were identical, the multi-y split would not be working.
  df_normal <- data.frame(
    x      = rep(c(0, 1, 2), 2),
    y      = rep(c(1, 2), each = 3),
    height = c(0, 1, 0,  0, 2, 0),
    group  = rep(c("red", "blue"), each = 3)
  )
  p_normal <- ggplot(df_normal, aes(x, y, height = height, group = group, fill = group)) +
    geom_ridgeline_fade()
  expect_false(identical(ggplotGrob(p), ggplotGrob(p_normal)))
})

# -----------------------------------------------------------------------
# Alpha isolation: ridges must not interact across groups or y-levels
# -----------------------------------------------------------------------

# Helper: build a ridgeline plot, extract the draw_panel grob, and return
# the gradient stop colours for every ribbon polygon.  Uses the fallback
# gradient stored in the "area_fade_grob" tree (Tier 2).
extract_ridge_gradients <- function(p) {
  b     <- ggplot_build(p)
  ldata <- b$data[[1]]
  pp    <- b$layout$panel_params[[1]]
  coord <- b$layout$coord
  gp    <- p$layers[[1]]$geom_params
  tree  <- GeomRidgelineFade$draw_panel(
    ldata, pp, coord,
    alpha_fade_to = gp$alpha_fade_to %||% 0,
    outline.type  = gp$outline.type %||% "upper"
  )
  # tree is a gTree whose children are individual ridge grobs.
  # Each ridge is itself either a gTree or an area_fade_grob containing
  # $fallback_gradient.  Extract them.
  grads <- list()
  for (i in seq_along(tree$children)) {
    child <- tree$children[[i]]
    # area_fade_grob stores fallback_gradient directly
    if (!is.null(child$fallback_gradient)) {
      grads[[length(grads) + 1L]] <- child$fallback_gradient$colours
    }
  }
  grads
}

test_that("alpha_scope = 'area': every ridge is fully independent", {
  # Single ridge at y = 1
  df_solo <- data.frame(
    x = c(0, 1, 2), y = 1, height = c(0, 3, 0), group = "a"
  )
  p_solo <- ggplot(df_solo, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(alpha_scope = "area")

  # Same ridge, but a much taller ridge at y = 2 AND another at y = 1
  df_with <- data.frame(
    x      = rep(c(0, 1, 2), 3),
    y      = c(rep(1, 6), rep(2, 3)),
    height = c(0, 3, 0,    # same ridge at y = 1
               0, 100, 0,  # very tall ridge also at y = 1
               0, 50, 0),  # tall ridge at y = 2
    group  = rep(c("a", "b", "c"), each = 3)
  )
  p_with <- ggplot(df_with, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(alpha_scope = "area")

  grad_solo <- extract_ridge_gradients(p_solo)
  grad_with <- extract_ridge_gradients(p_with)

  expect_length(grad_solo, 1L)
  expect_length(grad_with, 3L)

  # With "area" scope, the ridge (group "a", height = 3) should have the
  # exact same gradient whether it's alone or next to other ridges —
  # no interaction across groups or y-levels.
  expect_true(
    any(vapply(grad_with, \(g) identical(g, grad_solo[[1L]]), logical(1))),
    info = "area scope: ridge gradient must be fully independent"
  )
})

test_that("alpha_scope = 'group': ridges at same y interact, different y do not", {
  # Single ridge at y = 1
  df_solo <- data.frame(
    x = c(0, 1, 2), y = 1, height = c(0, 3, 0), group = "a"
  )
  p_solo <- ggplot(df_solo, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(alpha_scope = "group")

  # Add a tall ridge at a DIFFERENT y-level → should NOT affect group "a"
  df_diff_y <- data.frame(
    x      = rep(c(0, 1, 2), 2),
    y      = rep(c(1, 2), each = 3),
    height = c(0, 3, 0,    # same ridge at y = 1
               0, 100, 0), # very tall ridge at y = 2
    group  = rep(c("a", "b"), each = 3)
  )
  p_diff_y <- ggplot(df_diff_y, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(alpha_scope = "group")

  grad_solo   <- extract_ridge_gradients(p_solo)
  grad_diff_y <- extract_ridge_gradients(p_diff_y)

  # Different y-levels: no interaction → solo gradient matches
  expect_true(
    any(vapply(grad_diff_y, \(g) identical(g, grad_solo[[1L]]), logical(1))),
    info = "group scope: ridge at different y must not affect gradient"
  )

  # Add a tall ridge at the SAME y-level → SHOULD affect group "a"
  df_same_y <- data.frame(
    x      = rep(c(0, 1, 2), 2),
    y      = rep(1, 6),
    height = c(0, 3, 0,    # same ridge at y = 1
               0, 100, 0), # very tall ridge also at y = 1
    group  = rep(c("a", "b"), each = 3)
  )
  p_same_y <- ggplot(df_same_y, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(alpha_scope = "group")

  grad_same_y <- extract_ridge_gradients(p_same_y)

  # Same y-level: group "a" (height 3) now scales against max 100 at y=1.
  # Its peak alpha drops from 1.0 to 3/100 ≈ 0.03 — NOT all gradients
  # can match the solo case any more.
  matches <- vapply(grad_same_y, \(g) identical(g, grad_solo[[1L]]), logical(1))
  expect_false(
    all(matches),
    info = "group scope: taller ridge at same y must change the short ridge's gradient"
  )
})

test_that("alpha_scope = 'global': a short ridge's gradient IS affected by taller ridges", {
  # Counterpart: "global" scope means cross-ridge interaction is expected.
  df_solo <- data.frame(
    x = c(0, 1, 2), y = 1, height = c(0, 3, 0), group = "a"
  )
  p_solo <- ggplot(df_solo, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(alpha_scope = "global")

  df_with_tall <- data.frame(
    x      = rep(c(0, 1, 2), 2),
    y      = rep(c(1, 2), each = 3),
    height = c(0, 3, 0,
               0, 100, 0),
    group  = rep(c("a", "b"), each = 3)
  )
  p_with_tall <- ggplot(df_with_tall, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(alpha_scope = "global")

  grad_solo <- extract_ridge_gradients(p_solo)
  grad_tall <- extract_ridge_gradients(p_with_tall)

  # The short ridge (y = 1) is drawn last (descending back-to-front order
  # puts higher y first), so it is the last gradient.  When alone it gets
  # full alpha (3/3); with the tall ridge it scales to 3/100, so its peak
  # stop must differ.
  short_idx <- length(grad_tall)
  expect_false(
    identical(grad_tall[[short_idx]], grad_solo[[1L]]),
    info = "global scope: adding a taller ridge must change the short ridge's gradient"
  )
})

test_that("alpha_scope = 'group': multi-y split does not bleed alpha across y-levels", {
  # Same group at two y-levels.  With group scope each ridge should be
  # fully independent: the gradient for the short ridge (height = 1)
  # should be the same as if it were drawn alone.
  df_shared_group <- data.frame(
    x      = rep(c(0, 1, 2), 2),
    y      = rep(c(1, 2), each = 3),
    height = c(0, 10, 0,   # same group, tall at y = 1
               0, 1, 0),   # same group, short at y = 2
    group  = "a"
  )
  p_shared <- ggplot(df_shared_group, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(alpha_scope = "group")

  df_short_solo <- data.frame(
    x = c(0, 1, 2), y = 2, height = c(0, 1, 0), group = "a"
  )
  p_solo <- ggplot(df_short_solo, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(alpha_scope = "group")

  grad_shared <- extract_ridge_gradients(p_shared)
  grad_solo   <- extract_ridge_gradients(p_solo)

  expect_length(grad_shared, 2L)
  expect_length(grad_solo, 1L)

  # The short ridge (height = 1) gradient must match the solo case.
  expect_true(
    any(vapply(grad_shared, \(g) identical(g, grad_solo[[1L]]), logical(1))),
    info = "group scope: same group at two y-levels must not bleed alpha"
  )
})


# -----------------------------------------------------------------------
# Negative heights: bidirectional gradient
# -----------------------------------------------------------------------

test_that("negative heights (min_height < 0) render without error", {
  df_neg <- data.frame(
    x = 1:5, y = rep(3, 5),
    height = c(0, 2, -1, 2, 0),
    group = "a"
  )
  p <- ggplot(df_neg, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(min_height = -2)
  expect_no_error(ggplotGrob(p))
})

test_that("mixed heights produce a three-stop bidirectional gradient", {
  df_mix <- data.frame(
    x = 1:5, y = rep(1, 5),
    height = c(0, 3, -2, 3, 0),
    group = "a"
  )
  p <- ggplot(df_mix, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(min_height = -3, alpha_fade_to = 0)

  grads <- extract_ridge_gradients(p)
  expect_length(grads, 1L)
  # Mixed heights → three gradient stops (trough, baseline, peak)
  expect_length(grads[[1L]], 3L)
  # Middle stop (at baseline) must be the most transparent
  alphas <- grDevices::col2rgb(grads[[1L]], alpha = TRUE)[4L, ] / 255
  expect_lt(alphas[2L], alphas[1L])
  expect_lt(alphas[2L], alphas[3L])
})

test_that("all-negative heights produce a two-stop gradient (trough opaque, baseline transparent)", {
  df_allneg <- data.frame(
    x = 1:5, y = rep(5, 5),
    height = c(0, -1, -3, -1, 0),
    group = "a"
  )
  p <- ggplot(df_allneg, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(min_height = -4, alpha_fade_to = 0)
  expect_no_error(ggplotGrob(p))

  grads <- extract_ridge_gradients(p)
  expect_length(grads, 1L)
  # All-negative → two stops (trough → baseline)
  expect_length(grads[[1L]], 2L)
  # Trough (bottom) more opaque than baseline (top)
  alphas <- grDevices::col2rgb(grads[[1L]], alpha = TRUE)[4L, ] / 255
  expect_gt(alphas[1L], alphas[2L])
})

test_that("fill aesthetic is respected", {
  p <- ggplot(df, aes(x, y, height = height, group = group, fill = group)) +
    geom_ridgeline_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("alpha_fade_to = 0.5 produces different output than default 0", {
  p1 <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(alpha_fade_to = 0)
  p2 <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(alpha_fade_to = 0.5)
  expect_false(identical(ggplotGrob(p1), ggplotGrob(p2)))
})


# -----------------------------------------------------------------------
# Snapshot tests
# -----------------------------------------------------------------------

test_that("basic ridgeline_fade renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(df, aes(x, y, height = height, group = group, fill = group)) +
    geom_ridgeline_fade(alpha_scope = "group") +
    scale_fill_viridis_d(guide = "none") +
    theme_minimal()

  vdiffr::expect_doppelganger("ridgeline-fade-basic", p)
})

test_that("ridgeline_fade global scope renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(df, aes(x, y, height = height, group = group, fill = group)) +
    geom_ridgeline_fade(alpha_scope = "global") +
    scale_fill_viridis_d(guide = "none") +
    theme_minimal()

  vdiffr::expect_doppelganger("ridgeline-fade-global", p)
})

test_that("ridgeline_fade with alpha_fade_to = 0.3 renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(df, aes(x, y, height = height, group = group, fill = group)) +
    geom_ridgeline_fade(alpha_fade_to = 0.3) +
    scale_fill_viridis_d(guide = "none") +
    theme_minimal()

  vdiffr::expect_doppelganger("ridgeline-fade-alpha-fade-to", p)
})

test_that("ridgeline_fade area scope renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(df, aes(x, y, height = height, group = group, fill = group)) +
    geom_ridgeline_fade(alpha_scope = "area") +
    scale_fill_viridis_d(guide = "none") +
    theme_minimal()

  vdiffr::expect_doppelganger("ridgeline-fade-area", p)
})

test_that("ridgeline_fade with min_height clipping renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(df, aes(x, y, height = height, group = group, fill = group)) +
    geom_ridgeline_fade(min_height = 0.6, scale = 1.5) +
    scale_fill_viridis_d(guide = "none") +
    theme_minimal()

  vdiffr::expect_doppelganger("ridgeline-fade-min-height", p)
})

test_that("ridgeline_fade with negative heights renders bidirectional gradient", {
  skip_if_not_installed("vdiffr")

  df_neg <- data.frame(
    x      = rep(1:5, 3),
    y      = rep(c(1, 3, 5), each = 5),
    height = c( 0,  2, -1,  2, 0,
                0,  1, -2,  1, 0,
                0,  3,  0,  3, 0),
    group  = rep(c("a", "b", "c"), each = 5)
  )
  p <- ggplot(df_neg, aes(x, y, height = height, group = group, fill = group)) +
    geom_ridgeline_fade(min_height = -3, alpha_scope = "global") +
    scale_fill_viridis_d(guide = "none") +
    theme_minimal()

  vdiffr::expect_doppelganger("ridgeline-fade-negative-heights", p)
})


# ===========================================================================
# Grammar of Graphics adversarial stress tests
# ===========================================================================

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

test_that("GoG/data: empty dataset does not error", {
  p <- ggplot(data.frame(x = numeric(), y = numeric(), height = numeric()),
              aes(x, y, height = height)) +
    geom_ridgeline_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: single group with one point does not error", {
  p <- ggplot(data.frame(x = 1, y = 1, height = 0.5),
              aes(x, y, height = height)) +
    geom_ridgeline_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: all-zero height does not error", {
  df0 <- data.frame(x = 1:5, y = rep(1, 5), height = 0)
  p <- ggplot(df0, aes(x, y, height = height)) + geom_ridgeline_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/data: all-NA height does not error", {
  dfn <- data.frame(x = 1:5, y = rep(1, 5), height = NA_real_)
  p <- ggplot(dfn, aes(x, y, height = height)) + geom_ridgeline_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Mapping
# ---------------------------------------------------------------------------

test_that("GoG/mapping: fill aesthetic mapping does not error", {
  p <- ggplot(df, aes(x, y, height = height, group = group, fill = group)) +
    geom_ridgeline_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/mapping: inherit.aes = FALSE isolates from plot mapping", {
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point() +
    geom_ridgeline_fade(data = df[df$group == "a", ],
                        mapping = aes(x, y, height = height),
                        inherit.aes = FALSE)
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Layer
# ---------------------------------------------------------------------------

test_that("GoG/layer: multiple geom_ridgeline_fade layers do not error", {
  p <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(fill = "red", alpha = 0.3) +
    geom_ridgeline_fade(fill = "blue", alpha = 0.3, alpha_fade_to = 0.5)
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/layer: geom_ridgeline_fade standalone does not error", {
  p <- ggplot(df[df$group == "a", ], aes(x, y, height = height)) +
    geom_ridgeline_fade()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Scales
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_y_reverse negates y values (ridgeline_fade)", {
  b_fwd <- ggplot_build(ggplot(df, aes(x, y, height = height, group = group)) + geom_ridgeline_fade())
  b_rev <- ggplot_build(ggplot(df, aes(x, y, height = height, group = group)) + geom_ridgeline_fade() + scale_y_reverse())
  expect_equal(b_rev$data[[1]]$y, -b_fwd$data[[1]]$y)
})

test_that("GoG/scales: scale_x_reverse negates x values (ridgeline_fade)", {
  b_fwd <- ggplot_build(ggplot(df, aes(x, y, height = height, group = group)) + geom_ridgeline_fade())
  b_rev <- ggplot_build(ggplot(df, aes(x, y, height = height, group = group)) + geom_ridgeline_fade() + scale_x_reverse())
  expect_equal(b_rev$data[[1]]$x, -b_fwd$data[[1]]$x)
})

test_that("GoG/scales: explicit limits do not error", {
  p <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade() + scale_y_continuous(limits = c(-5, 10))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: expand = c(0, 0) does not error", {
  p <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade() + scale_y_continuous(expand = c(0, 0))
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Coord
# ---------------------------------------------------------------------------

test_that("GoG/coord: coord_cartesian zoom does not error", {
  p <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade() + coord_cartesian(ylim = c(0, 5))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_flip does not error", {
  p <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade() + coord_flip()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_fixed does not error", {
  p <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade() + coord_fixed()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_polar does not error", {
  p <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade() + coord_polar()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Facets
# ---------------------------------------------------------------------------

test_that("GoG/facets: facet_wrap with free scales does not error", {
  p <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade() + facet_wrap(~group, scales = "free")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/facets: facet_grid with free scales does not error", {
  p <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade() + facet_grid(~group, scales = "free")
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------

test_that("GoG/theme: theme_void does not error", {
  p <- ggplot(df[df$group == "a", ], aes(x, y, height = height)) +
    geom_ridgeline_fade() + theme_void()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: theme_classic does not error", {
  p <- ggplot(df[df$group == "a", ], aes(x, y, height = height)) +
    geom_ridgeline_fade() + theme_classic()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: theme_bw does not error", {
  p <- ggplot(df[df$group == "a", ], aes(x, y, height = height)) +
    geom_ridgeline_fade() + theme_bw()
  expect_no_error(ggplotGrob(p))
})
