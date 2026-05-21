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

test_that("position_ridgeline() accepts min_height = -Inf as the no-clip sentinel", {
  # Negative heights are dropped under the default min_height = 0 and the
  # geom's bidirectional-gradient code path is unreachable through the
  # public API without -Inf. Validate that -Inf is accepted and renders.
  df_neg <- data.frame(x = 1:5, y = "A", h = c(1, 2, -1, 2, 1))
  p <- ggplot(df_neg, aes(x, y, height = h)) +
    geom_ridgeline_fade(min_height = -Inf)
  b <- suppressWarnings(suppressMessages(ggplot_build(p)))
  expect_equal(nrow(b$data[[1]]), 5L)
  expect_equal(sum(is.na(b$data[[1]]$ymax)), 0L)
})

test_that("position_ridgeline() warns on default-mode negative drop", {
  # When min_height is on its default (NULL → 0 internally) AND negative
  # heights are dropped, surface a one-shot warning pointing at -Inf.
  df_neg <- data.frame(x = 1:5, y = "A", h = c(1, 2, -1, 2, 1))
  p <- ggplot(df_neg, aes(x, y, height = h)) + geom_ridgeline_fade()

  # Throttling may suppress the warning in re-runs; reset the cache.
  rlang::reset_warning_verbosity("position_ridgeline_default_drop_negatives")

  warns <- testthat::capture_warnings(
    suppressMessages(invisible(ggplot_build(p)))
  )
  expect_true(any(grepl("negative", warns, ignore.case = TRUE)))
  expect_true(any(grepl("min_height = -Inf", warns, fixed = TRUE)))
})

test_that("position_ridgeline() does NOT warn when user explicitly sets min_height = 0", {
  # Explicit 0 means user opted into the noise-floor behaviour; respect it.
  df_neg <- data.frame(x = 1:5, y = "A", h = c(1, 2, -1, 2, 1))
  p <- ggplot(df_neg, aes(x, y, height = h)) +
    geom_ridgeline_fade(min_height = 0)
  warns <- testthat::capture_warnings(
    suppressMessages(invisible(ggplot_build(p)))
  )
  expect_false(any(grepl("negative", warns, ignore.case = TRUE)))
})

test_that("position_ridgeline() does NOT warn when no negative heights exist", {
  # Density / histogram / freqpoly use cases (heights >= 0) must stay quiet.
  df_pos <- data.frame(x = 1:5, y = "A", h = c(1, 2, 0, 2, 1))  # zero, not negative
  p <- ggplot(df_pos, aes(x, y, height = h)) + geom_ridgeline_fade()
  warns <- testthat::capture_warnings(
    suppressMessages(invisible(ggplot_build(p)))
  )
  expect_false(any(grepl("negative", warns, ignore.case = TRUE)))
})


# -----------------------------------------------------------------------
# PositionRidgeline$compute_panel y==height guard (edge cases)
# -----------------------------------------------------------------------
# The guard catches the canonical user mistake: calling
# `geom_ridgeline_density_fade()` without mapping `y`, so `stat_density`'s
# default `aes(y = after_stat(density))` makes every row's `y` equal its
# `height`. The guard must (a) fire on that case, (b) stay quiet when
# `height` is mostly NA, (c) stay quiet when `height` is numerically close
# to `y` but not exactly equal.

test_that("guard fires when y == height across every row (smoking-gun case)", {
  # Recreates the original failure mode: forgetting to map `y` on
  # geom_ridgeline_density_fade leaves `y = after_stat(density)`, which is
  # the same value as `height`. The guard aborts at build time.
  pos <- position_ridgeline(scale = 1)
  d <- data.frame(x = 1:5, y = c(0.1, 0.2, 0.3, 0.4, 0.5),
                  height = c(0.1, 0.2, 0.3, 0.4, 0.5))
  expect_error(
    pos$compute_panel(d, list(scale = 1, min_height = 0), NULL),
    "missing.*y"
  )
})

test_that("guard stays quiet when height is all-NA (legit early-pipeline state)", {
  # During some compute_layer passes height can arrive as NA before the
  # stat fills it in; `y == NA` is NA, not TRUE, and the guard must not
  # mis-fire on this transient.
  pos <- position_ridgeline(scale = 1)
  d <- data.frame(x = 1:5, y = c(1, 1, 2, 2, 2),
                  height = NA_real_)
  expect_no_error(
    pos$compute_panel(d, list(scale = 1, min_height = 0), NULL)
  )
})

test_that("guard stays quiet when y is close to height but not exactly equal", {
  # Equality is checked exactly (==), not within tolerance. Real ridgelines
  # whose baseline (y) happens to be near the height values must pass.
  pos <- position_ridgeline(scale = 1)
  d <- data.frame(x = 1:5, y = c(1, 1, 2, 2, 2),
                  height = c(1 + 1e-8, 1 - 1e-8, 2 + 1e-9, 2 - 1e-9, 2.5))
  expect_no_error(
    pos$compute_panel(d, list(scale = 1, min_height = 0), NULL)
  )
})

test_that("guard stays quiet when only some rows have y == height", {
  # A real ridgeline can incidentally have one row where y happens to equal
  # height (e.g. y baseline = 1 and one density value happens to be 1.0
  # after binning). Guard requires ALL rows to match before aborting.
  pos <- position_ridgeline(scale = 1)
  d <- data.frame(x = 1:5, y = c(1, 1, 1, 2, 2),
                  height = c(1, 0.5, 0.3, 1.2, 2))
  expect_no_error(
    pos$compute_panel(d, list(scale = 1, min_height = 0), NULL)
  )
})

test_that("guard stays quiet when y == height holds for a single-row group", {
  # A 1-row panel cannot distinguish "every row matches" from coincidence;
  # the guard explicitly requires nrow > 1 to fire.
  pos <- position_ridgeline(scale = 1)
  d <- data.frame(x = 1, y = 0.4, height = 0.4)
  expect_no_error(
    pos$compute_panel(d, list(scale = 1, min_height = 0), NULL)
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
  for (s in c("group", "global")) {
    expect_no_error(ggplotGrob(p + geom_ridgeline_fade(alpha_scope = s)))
  }
})

test_that("legacy alpha_scope = 'area' is rejected (renamed to 'group' in 0.3.0)", {
  p <- ggplot(df, aes(x, y, height = height, group = group))
  expect_error(ggplotGrob(p + geom_ridgeline_fade(alpha_scope = "area")), "alpha_scope")
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

test_that("the two alpha_scope values produce different output (when ridges differ in height)", {
  # `df` has multiple ridges with non-equal heights, so "global" (scaled
  # against the layer-wide max) and "group" (each ridge to itself) must
  # differ.
  mk <- \(scope) {
    ggplotGrob(
      ggplot(df, aes(x, y, height = height, group = group)) +
        geom_ridgeline_fade(alpha_scope = scope)
    )
  }
  expect_false(identical(mk("group"), mk("global")))
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
  # tree is a `ridgeline_panel_grob` carrying per-ridge components in
  # `$ridges`. Each component's `$fade_grob` is the `area_fade_grob`
  # holding `$fallback_gradient`.
  grads <- list()
  ridges <- tree$ridges %||% list()
  for (i in seq_along(ridges)) {
    fade <- ridges[[i]]$fade_grob
    if (!is.null(fade$fallback_gradient)) {
      grads[[length(grads) + 1L]] <- fade$fallback_gradient$colours
    }
  }
  grads
}

test_that("alpha_scope = 'group': every ridge is fully independent", {
  # Renamed from "area" to "group" in 0.3.0 (matches `geom_area_fade()`).
  # Single ridge at y = 1
  df_solo <- data.frame(
    x = c(0, 1, 2), y = 1, height = c(0, 3, 0), group = "a"
  )
  p_solo <- ggplot(df_solo, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(alpha_scope = "group")

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
    geom_ridgeline_fade(alpha_scope = "group")

  grad_solo <- extract_ridge_gradients(p_solo)
  grad_with <- extract_ridge_gradients(p_with)

  expect_length(grad_solo, 1L)
  expect_length(grad_with, 3L)

  # With "group" scope, the ridge (group "a", height = 3) should have the
  # exact same gradient whether it's alone or next to other ridges —
  # no interaction across groups or y-levels.
  expect_true(
    any(vapply(grad_with, \(g) identical(g, grad_solo[[1L]]), logical(1))),
    info = "group scope: ridge gradient must be fully independent"
  )
})

test_that("facet × density-fade: group keeps each ridge at peak alpha 1; global preserves cross-panel reference", {
  # Regression: faceted ridgeline_density_fade had no behavioural test; the
  # 2026-05-09 manual probe verified (a) "group" makes every ridge peak at
  # alpha 1 regardless of panel, (b) "global" computes the alpha reference
  # across ALL panels (not per panel), so a panel that doesn't contain the
  # tallest ridge has its top alpha < 1.
  set.seed(1)
  df_ridge <- data.frame(
    g     = factor(rep(c("a", "b", "c", "d"), each = 60)),
    panel = rep(c("p1", "p2"), each = 120),
    x     = c(rnorm(60, 0, 0.5), rnorm(60, 1, 0.5),  # p1: tighter peaks (taller density)
              rnorm(60, 2, 1.5), rnorm(60, 3, 1.5))  # p2: wider peaks (shorter density)
  )

  collect_panel_max_alpha <- function(scope) {
    p <- ggplot(df_ridge, aes(x = x, y = g)) +
      geom_ridgeline_density_fade(alpha_scope = scope, outline.type = "none") +
      facet_wrap(~panel)
    tmp <- tempfile(fileext = ".png")
    ragg::agg_png(tmp, 600, 400); on.exit({dev.off(); unlink(tmp)}, add = TRUE)
    g <- suppressWarnings(suppressMessages(ggplotGrob(p)))
    panels <- g$grobs[grep("panel", g$layout$name)]
    out <- list()
    walk <- function(node, pid) {
      if (inherits(node, "area_fade_grob")) {
        fg <- node$fallback_gradient
        if (inherits(fg, "GridLinearGradient")) {
          last <- fg$colours[length(fg$colours)]
          a <- if (nchar(last) == 9) strtoi(substr(last, 8, 9), 16) / 255 else 1
          out[[length(out) + 1L]] <<- list(panel = pid, alpha = a)
        }
      }
      # Ridgeline panel container holds ridges in `$ridges` until
      # makeContent fires at draw time; descend there.
      if (inherits(node, "ridgeline_panel_grob")) {
        for (r in node$ridges %||% list()) walk(r, pid)
      }
      if (inherits(node, "ridge_components_grob")) {
        if (!is.null(node$fade_grob)) walk(node$fade_grob, pid)
      }
      if (inherits(node, "gTree") && length(node$children))
        for (ch in node$children) walk(ch, pid)
    }
    for (i in seq_along(panels)) walk(panels[[i]], i)
    df <- do.call(rbind, lapply(out, as.data.frame))
    tapply(df$alpha, df$panel, max)
  }

  # group scope: every panel's max ridge peaks at alpha 1.0
  group_max <- collect_panel_max_alpha("group")
  expect_equal(unname(as.numeric(group_max)), c(1, 1), tolerance = 1e-6)

  # global scope: only the panel containing the tallest density (p1, with
  # tight peaks) reaches 1.0. The other panel's max < 1.
  global_max <- collect_panel_max_alpha("global")
  expect_equal(max(global_max), 1, tolerance = 1e-6)
  expect_true(min(global_max) < 0.95,
              info = "panel without the tallest density should not reach alpha 1 under global scope")
})

test_that("alpha_scope = 'global' is cross-panel under facet_grid (regression 2026-04-28)", {
  # Reproducer: tall panel max = 10, short panel max = 2. With true global
  # scaling the short-panel ridge should peak at alpha 2/10 = 0.2 (~0x33),
  # not at 1.0. Earlier `geom_ridgeline_fade` recomputed the max per panel
  # in `draw_panel`, which broke this — fixed by adding a `draw_layer`
  # override that stamps `global_max_height` cross-panel.
  df_facet <- data.frame(
    x      = rep(1:5, 2),
    height = c(0, 5, 10, 5, 0,   # Tall ridge (max = 10)
               0, 1,  2, 1, 0),  # Short ridge (max = 2)
    grp    = rep(c("A", "B"), each = 5),
    facet  = rep(c("Tall", "Short"), each = 5)
  )
  p <- ggplot(df_facet, aes(x, y = 0, height = height, group = grp, fill = facet)) +
    geom_ridgeline_fade(alpha_scope = "global", alpha_fade_to = 0) +
    facet_wrap(~facet)

  grads <- extract_ridge_gradients(p)
  # Each gradient is a c(transparent, opaque) pair; the second stop is the peak.
  peak_alphas <- vapply(grads, function(g) {
    last <- g[length(g)]
    strtoi(substr(last, 8L, 9L), base = 16L) / 255
  }, numeric(1))

  # Two ridges total: one peaks at 1.0 (tall, height 10), one at 0.2 (short, height 2).
  expect_equal(sort(peak_alphas), c(0.2, 1.0), tolerance = 0.005)
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

test_that("ridgeline_fade group scope renders correctly", {
  # Renamed from "area" to "group" in 0.3.0.
  skip_if_not_installed("vdiffr")

  p <- ggplot(df, aes(x, y, height = height, group = group, fill = group)) +
    geom_ridgeline_fade(alpha_scope = "group") +
    scale_fill_viridis_d(guide = "none") +
    theme_minimal()

  vdiffr::expect_doppelganger("ridgeline-fade-group", p)
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

test_that("coord_flip: gradient axis rotates from vertical to horizontal", {
  # Behavioural pin for the 2026-05 coord_flip parity fix. Default
  # ridgelines paint a vertical gradient; coord_flip rotates it via
  # `flipped_visual = xor(flipped_aes, CoordFlip)`.
  p_normal <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade()
  p_flip <- p_normal + coord_flip()
  g_normal <- .collect_gradient_axes(p_normal)
  g_flip <- .collect_gradient_axes(p_flip)
  expect_true(!is.null(g_normal) && nrow(g_normal) > 0)
  expect_true(!is.null(g_flip) && nrow(g_flip) > 0)
  # Default: vertical gradient
  expect_true(all(as.numeric(g_normal[, "x1"]) == as.numeric(g_normal[, "x2"])))
  expect_true(all(as.numeric(g_normal[, "y1"]) != as.numeric(g_normal[, "y2"])))
  # Under coord_flip: horizontal gradient
  expect_true(all(as.numeric(g_flip[, "y1"]) == as.numeric(g_flip[, "y2"])))
  expect_true(all(as.numeric(g_flip[, "x1"]) != as.numeric(g_flip[, "x2"])))
})

test_that("coord_flip: vdiffr snapshot pins the rotated rendering", {
  p <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade() + coord_flip()
  vdiffr::expect_doppelganger("ridgeline-fade-coord-flip", p)
})

test_that("vdiffr: alpha_scope = 'global' under scale_y_log10 (data-space heights)", {
  skip_if_not_installed("vdiffr")
  # Regression test for the 2026-05 fix: three ridges of equal data-space
  # height (10) sitting at baselines 1, 10, 100. Under the buggy
  # panel-space heights computation, the upper ridges would alpha-shrink
  # to near-zero. After the fix, heights are computed in data space so
  # the three ridges normalise consistently. Pin the visual.
  df_eq <- data.frame(
    x = rep(seq(0, 1, length.out = 6), 3),
    y = rep(c(1, 10, 100), each = 6),
    height = 10,
    group = rep(c("a", "b", "c"), each = 6)
  )
  p <- ggplot(df_eq, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade(alpha_scope = "global") + scale_y_log10()
  suppressMessages(suppressWarnings(
    vdiffr::expect_doppelganger("ridgeline-fade-global-log10", p)
  ))
})

test_that("GoG/coord: coord_fixed does not error", {
  p <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade() + coord_fixed()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_polar does not error", {
  p <- ggplot(df, aes(x, y, height = height, group = group)) +
    geom_ridgeline_fade() + coord_polar()
  expect_no_error(suppressMessages(ggplotGrob(p)))
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
# Outline masking under overlap
# ---------------------------------------------------------------------------
# When ridges overlap, the back ridges' outlines used to bleed through the
# transparent baseline of front ridges. Each family's draw path now masks
# the back outline within front polygon shapes via Porter-Duff dest.out
# (compositing-capable devices) or falls back to the current unmasked
# rendering otherwise. Pin all four ridgeline geoms.

test_that("vdiffr (outline-mask): smooth ridgeline_fade with overlapping outlines", {
  skip_if_not_installed("vdiffr")
  set.seed(1)
  df_om <- data.frame(
    x      = rep(seq(0, 10, length.out = 25), 4),
    y      = rep(c(1, 2, 3, 4), each = 25),
    height = abs(rnorm(100)) + 0.2,
    grp    = rep(c("a", "b", "c", "d"), each = 25)
  )
  p <- ggplot(df_om,
              aes(x, y, height = height, group = grp, fill = grp)) +
    geom_ridgeline_fade(scale = 2, linewidth = 0.5,
                        outline.type = "upper", alpha_scope = "group")
  vdiffr::expect_doppelganger("outline-mask-ridge", p)
})

test_that("vdiffr (outline-mask): geom_ridgeline_density_fade overlap", {
  skip_if_not_installed("vdiffr")
  df_dens <- data.frame(
    x  = c(rnorm(150,  0, 1.0),
           rnorm(150,  1, 1.0),
           rnorm(150,  2, 1.0),
           rnorm(150,  3, 1.0)),
    grp = rep(c("a", "b", "c", "d"), each = 150)
  )
  p <- ggplot(df_dens, aes(x, y = grp, fill = grp)) +
    geom_ridgeline_density_fade(scale = 4, linewidth = 0.5,
                                outline.type = "upper", alpha_scope = "group")
  suppressMessages(suppressWarnings(
    vdiffr::expect_doppelganger("outline-mask-density", p)
  ))
})

test_that("vdiffr (outline-mask): geom_ridgeline_freqpoly_fade overlap", {
  skip_if_not_installed("vdiffr")
  set.seed(2)
  df_fp <- data.frame(
    x   = c(rnorm(150,  0, 1.0),
            rnorm(150,  1, 1.0),
            rnorm(150,  2, 1.0)),
    grp = rep(c("a", "b", "c"), each = 150)
  )
  p <- ggplot(df_fp, aes(x, y = grp, fill = grp)) +
    geom_ridgeline_freqpoly_fade(bins = 12, scale = 4, linewidth = 0.5,
                                 outline.type = "upper",
                                 alpha_scope = "group")
  suppressMessages(suppressWarnings(
    vdiffr::expect_doppelganger("outline-mask-freqpoly", p)
  ))
})

test_that("vdiffr (outline-mask): geom_ridgeline_histogram_fade overlap", {
  skip_if_not_installed("vdiffr")
  set.seed(3)
  df_hi <- data.frame(
    x   = c(rnorm(150,  0, 1.0),
            rnorm(150,  1, 1.0),
            rnorm(150,  2, 1.0)),
    grp = rep(c("a", "b", "c"), each = 150)
  )
  p <- ggplot(df_hi, aes(x, y = grp, fill = grp)) +
    geom_ridgeline_histogram_fade(bins = 15, scale = 4, linewidth = 0.5,
                                  outline.type = "upper",
                                  alpha_scope = "group")
  suppressMessages(suppressWarnings(
    vdiffr::expect_doppelganger("outline-mask-histogram", p)
  ))
})


# ---------------------------------------------------------------------------
# Snapshot pins for the ridgeline family — canonical orientation
# ---------------------------------------------------------------------------
# These vdiffr snapshots pin the as-shipping behaviour BEFORE the
# flipped_aes refactor, so any regression on the canonical path
# (`aes(x = continuous, y = factor)`) trips immediately.

.snapshot_ridge_data <- function() {
  data.frame(
    x = rep(seq(0, 10, length.out = 12), 3),
    y = rep(c("A", "B", "C"), each = 12),
    height = c(
      sin(seq(0, pi, length.out = 12)),
      cos(seq(0, pi, length.out = 12)) + 1,
      seq(0, 1, length.out = 12)
    )
  )
}

test_that("vdiffr: canonical ridgeline_fade default rendering", {
  skip_if_not_installed("vdiffr")
  d <- .snapshot_ridge_data()
  p <- ggplot(d, aes(x, y, height = height, group = y, fill = y)) +
    geom_ridgeline_fade(outline.type = "none") +
    guides(fill = "none")
  suppressMessages(suppressWarnings(
    vdiffr::expect_doppelganger("canonical-ridgeline-default", p)
  ))
})

test_that("vdiffr: canonical ridgeline_fade alpha_scope='global'", {
  skip_if_not_installed("vdiffr")
  d <- .snapshot_ridge_data()
  p <- ggplot(d, aes(x, y, height = height, group = y, fill = y)) +
    geom_ridgeline_fade(alpha_scope = "global", outline.type = "none") +
    guides(fill = "none")
  suppressMessages(suppressWarnings(
    vdiffr::expect_doppelganger("canonical-ridgeline-global", p)
  ))
})

test_that("vdiffr: canonical ridgeline_fade + coord_flip", {
  skip_if_not_installed("vdiffr")
  d <- .snapshot_ridge_data()
  p <- ggplot(d, aes(x, y, height = height, group = y, fill = y)) +
    geom_ridgeline_fade(outline.type = "none") +
    coord_flip() +
    guides(fill = "none")
  suppressMessages(suppressWarnings(
    vdiffr::expect_doppelganger("canonical-ridgeline-coord-flip", p)
  ))
})

test_that("vdiffr: canonical density_fade", {
  skip_if_not_installed("vdiffr")
  set.seed(1)
  d <- data.frame(
    x = c(rnorm(60, 0), rnorm(60, 2), rnorm(60, 4)),
    y = rep(c("A", "B", "C"), each = 60)
  )
  p <- ggplot(d, aes(x, y, fill = y)) +
    geom_ridgeline_density_fade(outline.type = "none") +
    guides(fill = "none")
  suppressMessages(suppressWarnings(
    vdiffr::expect_doppelganger("canonical-density-default", p)
  ))
})

test_that("vdiffr: canonical freqpoly_fade", {
  skip_if_not_installed("vdiffr")
  set.seed(1)
  d <- data.frame(
    x = c(rnorm(60, 0), rnorm(60, 2), rnorm(60, 4)),
    y = rep(c("A", "B", "C"), each = 60)
  )
  p <- ggplot(d, aes(x, y, fill = y)) +
    geom_ridgeline_freqpoly_fade(bins = 15, scale = 4) +
    guides(fill = "none")
  suppressMessages(suppressWarnings(
    vdiffr::expect_doppelganger("canonical-freqpoly-default", p)
  ))
})

test_that("vdiffr: canonical histogram_fade", {
  skip_if_not_installed("vdiffr")
  set.seed(1)
  d <- data.frame(
    x = c(rnorm(60, 0), rnorm(60, 2), rnorm(60, 4)),
    y = rep(c("A", "B", "C"), each = 60)
  )
  p <- ggplot(d, aes(x, y, fill = y)) +
    geom_ridgeline_histogram_fade(bins = 15, scale = 4) +
    guides(fill = "none")
  suppressMessages(suppressWarnings(
    vdiffr::expect_doppelganger("canonical-histogram-default", p)
  ))
})


# ---------------------------------------------------------------------------
# Snapshot pins for the ridgeline family — FLIPPED orientation
# ---------------------------------------------------------------------------
# Mirrors the canonical pins above, but with x and y swapped. Auto-detected
# via the `x constant per group` rule, OR explicit `orientation = "y"`.

test_that("vdiffr: flipped ridgeline_fade (auto-detected)", {
  skip_if_not_installed("vdiffr")
  d <- .snapshot_ridge_data()
  p <- ggplot(d, aes(y = x, x = y, height = height, group = x, fill = x)) +
    geom_ridgeline_fade(outline.type = "none") +
    guides(fill = "none")
  suppressMessages(suppressWarnings(
    vdiffr::expect_doppelganger("flipped-ridgeline-auto", p)
  ))
})

test_that("vdiffr: flipped ridgeline_fade (explicit orientation = 'y')", {
  skip_if_not_installed("vdiffr")
  d <- .snapshot_ridge_data()
  p <- ggplot(d, aes(y = x, x = y, height = height, group = x, fill = x)) +
    geom_ridgeline_fade(orientation = "y", outline.type = "none") +
    guides(fill = "none")
  suppressMessages(suppressWarnings(
    vdiffr::expect_doppelganger("flipped-ridgeline-explicit", p)
  ))
})

test_that("vdiffr: flipped density_fade", {
  skip_if_not_installed("vdiffr")
  set.seed(1)
  d <- data.frame(
    x = c(rnorm(60, 0), rnorm(60, 2), rnorm(60, 4)),
    y = rep(c("A", "B", "C"), each = 60)
  )
  p <- ggplot(d, aes(y = x, x = y, fill = y)) +
    geom_ridgeline_density_fade(outline.type = "none") +
    guides(fill = "none")
  suppressMessages(suppressWarnings(
    vdiffr::expect_doppelganger("flipped-density-default", p)
  ))
})


# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------



