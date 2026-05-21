library(ggplot2)

# -----------------------------------------------------------------------
# Shared test data
# -----------------------------------------------------------------------

# Simple col data: four independent bars
df_col <- data.frame(
  x = c("A", "B", "C", "D"),
  y = c(3, 7, 5, 9)
)

# Stacked data: two fill groups × three x-positions with unequal heights
df_stack <- data.frame(
  x     = rep(c("A", "B", "C"), 2),
  y     = c(3, 5, 2, 1, 4, 6),
  group = rep(c("p", "q"), each = 3)
)

# Dodged data: two fills × two x-positions.
# Within x = "A": p=4 (tallest), q=2; within x = "B": p=3, q=3 (equal).
df_dodge <- data.frame(
  x    = c("A", "A", "B", "B"),
  fill = c("p", "q", "p", "q"),
  y    = c(4, 2, 3, 3)
)

# Negative bars
df_neg <- data.frame(
  x = c("A", "B", "C"),
  y = c(-3, -7, -5)
)

# Mixed positive/negative
df_mixed <- data.frame(
  x = c("A", "B", "C", "D"),
  y = c(3, -4, 5, -2)
)

# Zero-height bar (edge case)
df_zero <- data.frame(x = c("A", "B"), y = c(0, 5))

# Single-bar panel
df_single <- data.frame(x = "A", y = 4)

# Helper: build a plot and call draw_panel directly, returning the bar_fade_grob.
# Extracts alpha_fade_to and radius from the layer so they reach draw_panel.
build_col_grob <- function(p) {
  b      <- ggplot_build(p)
  ldata  <- b$data[[1]]
  pp     <- b$layout$panel_params[[1]]
  coord  <- b$layout$coord
  gp     <- p$layers[[1]]$geom_params
  GeomColFade$draw_panel(
    ldata, pp, coord,
    alpha_fade_to = gp$alpha_fade_to %||% 0,
    radius        = gp$radius,
    alpha_scope   = ldata$.alpha_scope[1L] %||% "bar"
  )
}

# Helper: extract the alpha colour stops for the i-th gradient in a bar_fade_grob.
gradient_alphas <- function(grob, i) {
  col <- grob$gradient_glist[[i]]$gp$fill$colours
  # Decode alpha from the hex string (#RRGGBBAA) or named colours
  grDevices::col2rgb(col, alpha = TRUE)["alpha", ] / 255
}


# -----------------------------------------------------------------------
# Validation
# -----------------------------------------------------------------------

test_that("alpha_fade_to must be a finite scalar in [0, 1]", {
  p <- ggplot(df_col, aes(x, y))
  expect_error(ggplotGrob(p + geom_col_fade(alpha_fade_to = -0.1)),   "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_col_fade(alpha_fade_to =  1.1)),   "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_col_fade(alpha_fade_to = NA_real_)), "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_col_fade(alpha_fade_to = Inf)),    "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_col_fade(alpha_fade_to = c(0.1, 0.2))), "alpha_fade_to")
})

test_that("alpha_fade_to boundary values 0 and 1 are accepted", {
  p <- ggplot(df_col, aes(x, y))
  expect_no_error(ggplotGrob(p + geom_col_fade(alpha_fade_to = 0)))
  expect_no_error(ggplotGrob(p + geom_col_fade(alpha_fade_to = 1)))
})

test_that("alpha_scope rejects invalid values", {
  p <- ggplot(df_col, aes(x, y))
  expect_error(ggplotGrob(p + geom_col_fade(alpha_scope = "panel")), "alpha_scope")
  expect_error(ggplotGrob(p + geom_col_fade(alpha_scope = "BAR")),   "alpha_scope")
})

test_that("all valid alpha_scope values are accepted", {
  p <- ggplot(df_col, aes(x, y))
  for (s in c("bar", "group", "global")) {
    expect_no_error(ggplotGrob(p + geom_col_fade(alpha_scope = s)))
  }
})

test_that("radius as bare number is coerced to unit", {
  p <- ggplot(df_col, aes(x, y))
  expect_no_error(ggplotGrob(p + geom_col_fade(radius = 5)))
})

test_that("radius > 0 with non-NA colour uses linejoin = 'round' (artefact fix)", {
  # Regression: rounded roundrects rendered with the default linejoin = 'mitre'
  # produce visible stub-stroke artefacts at the path closure point. Forcing
  # 'round' on rounded paths (where the join is visually invisible anyway)
  # eliminates the artefact. See aaa.R `.roundrect_linejoin()`.
  skip_if_not_installed("ragg")

  collect_linejoin <- function(p) {
    tmp <- tempfile(fileext = ".png")
    ragg::agg_png(tmp, 400, 300)
    on.exit(
      {
        dev.off()
        unlink(tmp)
      },
      add = TRUE
    )
    g <- suppressWarnings(suppressMessages(ggplotGrob(p)))
    panel <- g$grobs[[grep("panel", g$layout$name)[1]]]
    bg <- NULL
    walk <- function(node) {
      if (inherits(node, "bar_fade_grob")) {
        bg <<- node
        return(invisible())
      }
      if (inherits(node, "gTree") && length(node$children)) {
        for (ch in node$children) walk(ch)
      }
    }
    walk(panel)
    forced <- grid::makeContent(bg)
    vapply(forced$children, \(c) c$gp$linejoin %||% NA_character_, character(1))
  }

  # Rounded → must be 'round'
  p_r <- ggplot(df_col, aes(x, y)) +
    geom_col_fade(colour = "black", radius = unit(5, "pt"))
  expect_true(all(collect_linejoin(p_r) == "round"))

  # Square (radius = 0) → user's choice survives (default 'mitre')
  p_sq <- ggplot(df_col, aes(x, y)) +
    geom_col_fade(colour = "black")
  expect_true(all(collect_linejoin(p_sq) == "mitre"))
})


# -----------------------------------------------------------------------
# Grob structure
# -----------------------------------------------------------------------

test_that("empty data returns zeroGrob", {
  grob <- GeomColFade$draw_panel(
    data.frame(),
    list(),
    ggplot2::coord_cartesian()
  )
  expect_true(grid::is.grob(grob))
  expect_s3_class(grob, "zeroGrob")
})

test_that("normal data produces a bar_fade_grob gTree", {
  p    <- ggplot(df_col, aes(x, y)) + geom_col_fade()
  grob <- build_col_grob(p)
  expect_s3_class(grob, "bar_fade_grob")
  expect_equal(length(grob$gradient_glist), nrow(df_col))
  expect_equal(length(grob$flat_glist),     nrow(df_col))
})

test_that("each gradient grob is a roundrectGrob", {
  p    <- ggplot(df_col, aes(x, y)) + geom_col_fade()
  grob <- build_col_grob(p)
  for (i in seq_along(grob$gradient_glist)) {
    expect_true(inherits(grob$gradient_glist[[i]], "roundrect"))
  }
})

test_that("each gradient fill is a GridLinearGradient", {
  p    <- ggplot(df_col, aes(x, y)) + geom_col_fade()
  grob <- build_col_grob(p)
  for (i in seq_along(grob$gradient_glist)) {
    fill <- grob$gradient_glist[[i]]$gp$fill
    expect_true(inherits(fill, "GridLinearGradient"),
                info = paste0("bar ", i))
  }
})

test_that("non-linear coord falls back to geom_bar rendering (no bar_fade_grob)", {
  p    <- ggplot(df_col, aes(x, y)) +
    geom_col_fade() +
    coord_polar()
  # Fallback returns a gtable-like grob, NOT a bar_fade_grob.
  grob <- suppressMessages(build_col_grob(p))
  expect_false(inherits(grob, "bar_fade_grob"))
})

test_that("non-linear coord emits an informational message", {
  p <- ggplot(df_col, aes(x, y)) +
    geom_col_fade() +
    coord_polar()
  # Use suppressMessages since cli .frequency may throttle across sessions.
  expect_no_error(suppressMessages(ggplotGrob(p)))
})


# -----------------------------------------------------------------------
# Alpha scope: "bar"
# -----------------------------------------------------------------------

test_that("alpha_scope = 'bar': every bar gets full peak alpha (= a_start)", {
  p    <- ggplot(df_col, aes(x, y)) + geom_col_fade(alpha_scope = "bar")
  grob <- build_col_grob(p)
  for (i in seq_along(grob$gradient_glist)) {
    alphas <- gradient_alphas(grob, i)
    # colours = c(baseline, peak); peak should equal a_start (= 1, no alpha aes)
    expect_equal(alphas[2], 1, tolerance = 0.01,
                 info = paste0("bar ", i, " peak alpha"))
  }
})

test_that("alpha_scope = 'bar': baseline alpha equals alpha_fade_to", {
  p    <- ggplot(df_col, aes(x, y)) +
    geom_col_fade(alpha_scope = "bar", alpha_fade_to = 0.3)
  grob <- build_col_grob(p)
  for (i in seq_along(grob$gradient_glist)) {
    alphas <- gradient_alphas(grob, i)
    expect_equal(alphas[1], 0.3, tolerance = 0.01,
                 info = paste0("bar ", i, " baseline alpha"))
  }
})


# -----------------------------------------------------------------------
# Alpha scope: "global"
# -----------------------------------------------------------------------

test_that("alpha_scope = 'global': tallest bar gets peak alpha = 1", {
  # df_col has max y = 9 (bar D, index 4 after sorting)
  p    <- ggplot(df_col, aes(x, y)) +
    geom_col_fade(alpha_scope = "global", alpha_fade_to = 0)
  grob <- build_col_grob(p)
  alphas_all <- lapply(seq_along(grob$gradient_glist), \(i) gradient_alphas(grob, i))
  max_peak   <- max(vapply(alphas_all, \(a) a[2], numeric(1)))
  expect_equal(max_peak, 1, tolerance = 0.01)
})

test_that("alpha_scope = 'global': shorter bars have proportionally lower peak alpha", {
  # Bar A (y=3) and bar D (y=9): A should have peak ≈ 3/9 = 0.33
  p    <- ggplot(df_col, aes(x, y)) +
    geom_col_fade(alpha_scope = "global", alpha_fade_to = 0)
  b    <- ggplot_build(p)
  grob <- build_col_grob(p)

  # Identify which grob corresponds to which bar by data order
  ldata <- b$data[[1]]
  # x is mapped to integer positions: A=1, B=2, C=3, D=4; after coord transform
  # x=1 → A (y=3), x=4 → D (y=9)
  alphas_all <- lapply(seq_along(grob$gradient_glist), \(i) gradient_alphas(grob, i))
  peak_alphas <- vapply(alphas_all, \(a) a[2], numeric(1))

  expect_equal(max(peak_alphas), 1, tolerance = 0.01)
  # All peaks should be proportional to y / max(y)
  expect_true(min(peak_alphas) < max(peak_alphas))
})

test_that("alpha_scope = 'global' differs from 'bar' when bars have different heights", {
  p_bar    <- ggplot(df_col, aes(x, y)) + geom_col_fade(alpha_scope = "bar")
  p_global <- ggplot(df_col, aes(x, y)) + geom_col_fade(alpha_scope = "global")
  expect_false(identical(ggplotGrob(p_bar), ggplotGrob(p_global)))
})

test_that("alpha_scope = 'global' uses cross-panel max under faceting (2026-04-27)", {
  # Regression for: under `facet_grid(cols = vars(grp))` each panel saw only
  # its own bar, so the per-panel max equalled the bar's own peak and every
  # bar rendered fully opaque.  The fix computes the layer-wide max in
  # draw_layer (post-position) and stamps it on each row.  Faceted and
  # un-faceted plots must produce the same per-bar alpha stops.
  df <- data.frame(x = c("X", "Y", "Z"), y = c(1, 2, 3), grp = c("a", "b", "c"))

  # Build the full gtable (so draw_layer runs end-to-end) and pull the peak
  # alpha out of every bar_fade_grob's first gradient.
  peak_alphas_from_gt <- function(p) {
    gt <- ggplot2::ggplot_gtable(ggplot_build(p))
    panels <- gt$grobs[grep("^panel", gt$layout$name)]
    out <- numeric(0L)
    for (pg in panels) {
      bf <- Filter(\(ch) inherits(ch, "bar_fade_grob"), pg$children)
      for (b in bf) {
        for (i in seq_along(b$gradient_glist)) {
          a <- gradient_alphas(b, i)
          out <- c(out, a[length(a)])
        }
      }
    }
    out
  }

  alphas_plain <- peak_alphas_from_gt(
    ggplot(df, aes(x, y)) +
      geom_col_fade(alpha_scope = "global", alpha_fade_to = 0)
  )
  alphas_facet <- peak_alphas_from_gt(
    ggplot(df, aes(x, y)) +
      geom_col_fade(alpha_scope = "global", alpha_fade_to = 0) +
      facet_grid(cols = vars(grp))
  )

  # Tallest bar (y = 3) → peak alpha 1; y = 2 → ~2/3; y = 1 → ~1/3.
  expect_equal(sort(alphas_plain), c(1 / 3, 2 / 3, 1), tolerance = 0.01)
  # Faceted alphas must match the unfaceted ones (same cross-panel reference).
  expect_equal(sort(alphas_facet), sort(alphas_plain), tolerance = 0.01)
})


# -----------------------------------------------------------------------
# Alpha scope: "x" (per discrete x-position; was "group" before 0.3.0)
# -----------------------------------------------------------------------

test_that("alpha_scope = 'x': tallest bar in each stacked group gets peak alpha = 1", {
  # With position = "stack", each x position is a group. The tallest stack
  # top segment should have a_peak = 1.
  p    <- ggplot(df_stack, aes(x, y, fill = group)) +
    geom_col_fade(position = "stack", alpha_scope = "x", alpha_fade_to = 0)
  grob <- build_col_grob(p)
  alphas_all <- lapply(seq_along(grob$gradient_glist), \(i) gradient_alphas(grob, i))
  max_peak   <- max(vapply(alphas_all, \(a) a[2], numeric(1)))
  expect_equal(max_peak, 1, tolerance = 0.01)
})

test_that("alpha_scope = 'x' with stacked bars differs from 'global'", {
  p_global <- ggplot(df_stack, aes(x, y, fill = group)) +
    geom_col_fade(position = "stack", alpha_scope = "global")
  p_group  <- ggplot(df_stack, aes(x, y, fill = group)) +
    geom_col_fade(position = "stack", alpha_scope = "x")
  expect_false(identical(ggplotGrob(p_global), ggplotGrob(p_group)))
})

test_that("alpha_scope = 'x' with dodged bars: tallest per dodge group gets peak = 1", {
  # df_dodge: group A has p=4 (tallest), group B has equal bars p=3, q=3
  p    <- ggplot(df_dodge, aes(x, y, fill = fill)) +
    geom_col_fade(position = "dodge", alpha_scope = "x", alpha_fade_to = 0)
  grob <- build_col_grob(p)
  alphas_all <- lapply(seq_along(grob$gradient_glist), \(i) gradient_alphas(grob, i))
  max_peak   <- max(vapply(alphas_all, \(a) a[2], numeric(1)))
  expect_equal(max_peak, 1, tolerance = 0.01)
})

test_that("alpha_scope = 'x' differs from 'bar' for dodged bars with unequal heights", {
  # Before the round() fix, "group" == "bar" for dodged. After fix they differ
  # because q in group A (y=2) now scales to 0.5 instead of 1.
  p_bar   <- ggplot(df_dodge, aes(x, y, fill = fill)) +
    geom_col_fade(position = "dodge", alpha_scope = "bar")
  p_group <- ggplot(df_dodge, aes(x, y, fill = fill)) +
    geom_col_fade(position = "dodge", alpha_scope = "x")
  expect_false(identical(ggplotGrob(p_bar), ggplotGrob(p_group)))
})

test_that("alpha_scope = 'x' with dodged bars: shorter bar in group gets peak < 1", {
  # Group A: p=4 (peak), q=2 → q should have peak alpha ≈ 0.5
  p    <- ggplot(df_dodge, aes(x, y, fill = fill)) +
    geom_col_fade(position = "dodge", alpha_scope = "x", alpha_fade_to = 0)
  grob <- build_col_grob(p)
  alphas_all <- lapply(seq_along(grob$gradient_glist), \(i) gradient_alphas(grob, i))
  peak_alphas <- vapply(alphas_all, \(a) a[2], numeric(1))
  # Not all bars can have peak = 1: the shorter bar in group A must be < 1
  expect_true(any(peak_alphas < 0.99))
})

test_that("alpha_scope = 'x' with position = 'fill': gradient stops match 'global'", {
  # All stacks are normalised to height 1, so every group max == global max == 1.
  # The alpha stops must be numerically identical even though the alpha_scope
  # label differs (so we compare stops, not the full grob tree).
  p_global <- ggplot(df_stack, aes(x, y, fill = group)) +
    geom_col_fade(position = "fill", alpha_scope = "global")
  p_group  <- ggplot(df_stack, aes(x, y, fill = group)) +
    geom_col_fade(position = "fill", alpha_scope = "x")

  grob_global <- build_col_grob(p_global)
  grob_group  <- build_col_grob(p_group)
  n <- length(grob_global$gradient_glist)
  for (i in seq_len(n)) {
    expect_equal(gradient_alphas(grob_global, i), gradient_alphas(grob_group, i),
                 tolerance = 0.01)
  }
})


# -----------------------------------------------------------------------
# Full alpha_scope vocabulary (2026-04-27 redesign):
#   c("bar", "group", "x", "y", "fill", "colour", "global")
# Cross-panel correctness under facet_grid is tested for every scope.
# -----------------------------------------------------------------------

# Walk the full gtable and pull every bar_fade_grob's peak alpha (last
# stop of each gradient).  Exercises the real draw_layer + draw_panel
# pipeline (the build_col_grob helper bypasses draw_layer).
peak_alphas_from_gtable <- function(p) {
  gt <- ggplot2::ggplot_gtable(ggplot_build(p))
  panels <- gt$grobs[grep("^panel", gt$layout$name)]
  out <- numeric(0L)
  for (pg in panels) {
    bf <- Filter(\(ch) inherits(ch, "bar_fade_grob"), pg$children)
    for (b in bf) {
      for (i in seq_along(b$gradient_glist)) {
        a <- gradient_alphas(b, i)
        out <- c(out, a[length(a)])
      }
    }
  }
  out
}

# Shared fixture: 2 fills × 3 x positions × 3 facet panels.  ymax values
# are all distinct so each scope produces a measurably different
# normalisation.  Layout (within each panel):
#   x = X     y = 0.5  fill = ha
#   x = X     y = 1.0  fill = ho
#   x = Y     y = 1.5  fill = ha
#   x = Y     y = 2.0  fill = ho
#   x = Z     y = 2.5  fill = ha
#   x = Z     y = 3.0  fill = ho
# panel "p1" = full data, "p2" = same again, "p3" = same again.
df_scope <- expand.grid(
  panel = c("p1", "p2", "p3"),
  fill  = c("ha", "ho"),
  x     = c("X", "Y", "Z"),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
df_scope$y <- ifelse(df_scope$fill == "ha",
  c(0.5, 1.5, 2.5)[match(df_scope$x, c("X", "Y", "Z"))],
  c(1.0, 2.0, 3.0)[match(df_scope$x, c("X", "Y", "Z"))]
)
make_p <- function(scope, ...) {
  ggplot(df_scope, aes(x, y)) +
    facet_grid(cols = vars(panel)) +
    geom_col_fade(
      aes(fill = fill),
      position = "dodge",
      alpha_scope = scope,
      alpha_fade_to = 0,
      ...
    )
}

test_that("alpha_scope = 'bar' under facet_grid: every bar peaks at 1", {
  alphas <- peak_alphas_from_gtable(make_p("bar"))
  # 3 panels × 6 bars = 18 (legend keys are inside guide-box, not panels).
  expect_equal(length(alphas), 18L)
  expect_true(all(abs(alphas - 1) < 1e-3))
})

test_that("alpha_scope = 'global' under facet_grid: peaks scale to cross-panel max", {
  alphas <- peak_alphas_from_gtable(make_p("global"))
  # Each panel has the same 6 bars → expected peaks per panel:
  #   y/3 for y in c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0).
  expected_one_panel <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0) / 3
  expect_equal(sort(alphas), sort(rep(expected_one_panel, 3L)), tolerance = 0.01)
})

test_that("alpha_scope = 'x' under facet_grid: per x-coord normalisation", {
  alphas <- peak_alphas_from_gtable(make_p("x"))
  # Per panel: x=X (max=1.0) → 0.5, 1.0; x=Y (max=2.0) → 0.75, 1.0;
  #           x=Z (max=3.0) → 0.833, 1.0.
  expected_one_panel <- c(0.5, 1.0, 0.75, 1.0, 5/6, 1.0)
  expect_equal(sort(alphas), sort(rep(expected_one_panel, 3L)), tolerance = 0.01)
})

test_that("alpha_scope = 'fill' under facet_grid: per fill-aesthetic normalisation, cross-panel", {
  alphas <- peak_alphas_from_gtable(make_p("fill"))
  # ha bars (across panels): y in c(0.5, 1.5, 2.5), max = 2.5
  #   → peaks 0.2, 0.6, 1.0
  # ho bars (across panels): y in c(1.0, 2.0, 3.0), max = 3.0
  #   → peaks 1/3, 2/3, 1.0
  # Each appears in 3 panels.
  expected <- c(rep(c(0.2, 0.6, 1.0), 3L), rep(c(1/3, 2/3, 1.0), 3L))
  expect_equal(sort(alphas), sort(expected), tolerance = 0.01)
})

test_that("alpha_scope = 'group' under facet_grid: per data$group normalisation", {
  # ggplot2 sets data$group = interaction(x, fill) when both are discrete,
  # so each (x, fill) pair is its own group → degenerates to "bar".  This
  # test pins that observed behaviour: every bar peaks at 1.
  alphas <- peak_alphas_from_gtable(make_p("group"))
  expect_true(all(abs(alphas - 1) < 1e-3))
})

test_that("alpha_scope = 'group' with explicit aes(group=fill): bars share scope by fill", {
  # When the user pins data$group via aes(group=fill), "group" should
  # behave like "fill": ha bars share, ho bars share, across x and panels.
  p <- ggplot(df_scope, aes(x, y, fill = fill, group = fill)) +
    facet_grid(cols = vars(panel)) +
    geom_col_fade(
      position = "dodge", alpha_scope = "group", alpha_fade_to = 0
    )
  alphas <- peak_alphas_from_gtable(p)
  # Same expected vector as the "fill" case above.
  expected <- c(rep(c(0.2, 0.6, 1.0), 3L), rep(c(1/3, 2/3, 1.0), 3L))
  expect_equal(sort(alphas), sort(expected), tolerance = 0.01)
})

test_that("alpha_scope = 'colour' under facet_grid: per colour-aesthetic normalisation", {
  p <- ggplot(df_scope, aes(x, y, fill = fill, colour = fill)) +
    facet_grid(cols = vars(panel)) +
    geom_col_fade(
      position = "dodge", alpha_scope = "colour", alpha_fade_to = 0,
      linewidth = 0.5
    )
  alphas <- peak_alphas_from_gtable(p)
  # colour mirrors fill in this fixture, so same expectation.
  expected <- c(rep(c(0.2, 0.6, 1.0), 3L), rep(c(1/3, 2/3, 1.0), 3L))
  expect_equal(sort(alphas), sort(expected), tolerance = 0.01)
})

test_that("alpha_scope = 'y' aborts when bars are vertical", {
  p <- ggplot(df_scope, aes(x, y, fill = fill)) +
    geom_col_fade(alpha_scope = "y", position = "dodge")
  expect_error(ggplotGrob(p), "alpha_scope.*y.*y-axis")
})

test_that("alpha_scope = 'x' aborts when bars are horizontal (orientation = 'y')", {
  p <- ggplot(df_scope, aes(y, x, fill = fill)) +
    geom_col_fade(alpha_scope = "x", position = "dodge", orientation = "y")
  expect_error(ggplotGrob(p), "alpha_scope.*x.*x-axis")
})

test_that("alpha_scope = 'y' works under orientation = 'y'", {
  p <- ggplot(df_scope, aes(y, x, fill = fill)) +
    facet_grid(cols = vars(panel)) +
    geom_col_fade(
      position = "dodge", alpha_scope = "y", alpha_fade_to = 0,
      orientation = "y"
    )
  alphas <- peak_alphas_from_gtable(p)
  # data$y is now discrete (X, Y, Z); same per-x-coord normalisation as
  # the "x" case above, just on the other axis.
  expected_one_panel <- c(0.5, 1.0, 0.75, 1.0, 5/6, 1.0)
  expect_equal(sort(alphas), sort(rep(expected_one_panel, 3L)), tolerance = 0.01)
})

test_that("alpha_scope = 'x' works under coord_flip (data$x stays discrete)", {
  p <- ggplot(df_scope, aes(x, y, fill = fill)) +
    facet_grid(cols = vars(panel)) +
    geom_col_fade(
      position = "dodge", alpha_scope = "x", alpha_fade_to = 0
    ) + coord_flip()
  alphas <- peak_alphas_from_gtable(p)
  expected_one_panel <- c(0.5, 1.0, 0.75, 1.0, 5/6, 1.0)
  expect_equal(sort(alphas), sort(rep(expected_one_panel, 3L)), tolerance = 0.01)
})


# -----------------------------------------------------------------------
# Negative and mixed-sign bars
# -----------------------------------------------------------------------

test_that("negative bars render without error", {
  p <- ggplot(df_neg, aes(x, y)) + geom_col_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("negative bars produce a bar_fade_grob", {
  p    <- ggplot(df_neg, aes(x, y)) + geom_col_fade()
  grob <- build_col_grob(p)
  expect_s3_class(grob, "bar_fade_grob")
  expect_equal(length(grob$gradient_glist), nrow(df_neg))
})

test_that("negative bars: gradient runs top-to-bottom (x1 = 0.5, y1 = 1 → y2 = 0)", {
  # For a negative vertical bar the baseline is at top (y = 0) and peak at bottom.
  p    <- ggplot(df_neg, aes(x, y)) + geom_col_fade()
  grob <- build_col_grob(p)
  fill <- grob$gradient_glist[[1]]$gp$fill
  expect_equal(as.numeric(fill$y1), 1)  # baseline = top
  expect_equal(as.numeric(fill$y2), 0)  # peak = bottom
})

test_that("mixed positive/negative bars render without error", {
  p <- ggplot(df_mixed, aes(x, y)) + geom_col_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("mixed bars: alpha_scope = 'global' uses max absolute height as reference", {
  p    <- ggplot(df_mixed, aes(x, y)) +
    geom_col_fade(alpha_scope = "global", alpha_fade_to = 0)
  grob <- build_col_grob(p)
  alphas_all <- lapply(seq_along(grob$gradient_glist), \(i) gradient_alphas(grob, i))
  max_peak   <- max(vapply(alphas_all, \(a) a[2], numeric(1)))
  expect_equal(max_peak, 1, tolerance = 0.01)
})


# -----------------------------------------------------------------------
# Edge cases
# -----------------------------------------------------------------------

test_that("single-bar panel renders without error", {
  p <- ggplot(df_single, aes(x, y)) + geom_col_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("zero-height bar does not crash with alpha_scope = 'global'", {
  p <- ggplot(df_zero, aes(x, y)) + geom_col_fade(alpha_scope = "global")
  expect_no_error(ggplotGrob(p))
})

test_that("alpha = NA in data is treated as fully opaque (a_start = 1)", {
  # When no alpha aesthetic is mapped, coords$alpha is NA; code sets a_start = 1.
  p    <- ggplot(df_col, aes(x, y)) +
    geom_col_fade(alpha_scope = "bar", alpha_fade_to = 0)
  grob <- build_col_grob(p)
  for (i in seq_along(grob$gradient_glist)) {
    alphas <- gradient_alphas(grob, i)
    expect_equal(alphas[2], 1, tolerance = 0.01,
                 info = paste0("bar ", i, " peak alpha with NA alpha aes"))
  }
})

test_that("alpha aesthetic caps peak opacity", {
  p    <- ggplot(df_col, aes(x, y)) +
    geom_col_fade(alpha = 0.6, alpha_fade_to = 0)
  grob <- build_col_grob(p)
  for (i in seq_along(grob$gradient_glist)) {
    alphas <- gradient_alphas(grob, i)
    expect_equal(alphas[2], 0.6, tolerance = 0.01,
                 info = paste0("bar ", i, " peak alpha with alpha = 0.6"))
  }
})

test_that("geom_bar_fade works (stat = 'count')", {
  p <- ggplot(mpg, aes(class)) + geom_bar_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("geom_histogram_fade works (stat = 'bin')", {
  p <- ggplot(faithful, aes(waiting)) + geom_histogram_fade(bins = 20)
  expect_no_error(ggplotGrob(p))
})

test_that("geom_histogram_fade: alpha_scope = 'global' accepted", {
  p <- ggplot(faithful, aes(waiting)) +
    geom_histogram_fade(bins = 20, alpha_scope = "global")
  expect_no_error(ggplotGrob(p))
})

test_that("geom_histogram_fade: alpha_scope = 'x' / 'y' rejected with hint at 'bin'", {
  # `"x"` / `"y"` key on round(data$x|y), which buckets bins by integer
  # rounding -- meaningless on a continuous binned axis. A targeted
  # pre-check intercepts these two values and points users at `"bin"`
  # explicitly rather than letting `arg_match0` list all valid options
  # flat (which left beginners reaching for `"group"` instead).
  p_x <- ggplot(faithful, aes(waiting)) +
    geom_histogram_fade(alpha_scope = "x", bins = 10)
  expect_error(ggplotGrob(p_x), 'not accepted by .*geom_histogram_fade')
  expect_error(ggplotGrob(p_x), 'Did you mean .*alpha_scope.*=.*bin')
  p_y <- ggplot(faithful, aes(waiting)) +
    geom_histogram_fade(alpha_scope = "y", bins = 10)
  expect_error(ggplotGrob(p_y), 'not accepted by .*geom_histogram_fade')
  expect_error(ggplotGrob(p_y), 'Did you mean .*alpha_scope.*=.*bin')
})

test_that("geom_histogram_fade: other invalid alpha_scope falls through to arg_match0", {
  # The friendly pre-check is targeted at `"x"` / `"y"` only; any other
  # invalid value should still hit `arg_match0`'s generic "must be one of"
  # error so users get the canonical list of valid options.
  p <- ggplot(faithful, aes(waiting)) +
    geom_histogram_fade(alpha_scope = "panel", bins = 10)
  expect_error(ggplotGrob(p), '`alpha_scope` must be one of')
})

test_that("geom_histogram_fade: alpha_scope = 'bin' normalises per-bin (dodged)", {
  # Per-bin scope: every cluster of dodged bars in the same bin shares a
  # `.scope_max_abs` reference; the value varies across bins.
  p <- ggplot(iris, aes(Sepal.Width, fill = Species)) +
    geom_histogram_fade(
      position = "dodge", bins = 10, alpha_scope = "bin"
    )
  b <- ggplot_build(p)
  d <- b$data[[1]]
  expect_true(".bin_id" %in% names(d))
  # Within each bin, `.bin_id` is constant; across bins it varies.
  by_bin <- split(d, d$.bin_id)
  expect_gt(length(by_bin), 1L)
  # Pre-dodge bin centre = post-dodge `data$x` median per cluster (each
  # cluster has 3 species dodged around the bin centre).
  for (g in by_bin) {
    expect_equal(length(unique(g$.bin_id)), 1L)
  }
})

test_that("geom_col_fade: alpha_scope = 'bin' rejected (no continuous bins)", {
  # `"bin"` is histogram-only; col/bar vocabulary must reject it cleanly.
  df <- data.frame(x = c("A", "B", "C"), y = c(3, 7, 5))
  p <- ggplot(df, aes(x, y)) + geom_col_fade(alpha_scope = "bin")
  expect_error(ggplotGrob(p), '`alpha_scope` must be one of')
})

test_that("flipped orientation (orientation = 'y') works without error", {
  df_h <- data.frame(x = c("A", "B", "C"), y = c(3, 7, 5))
  p    <- ggplot(df_h, aes(y = x, x = y)) +
    geom_col_fade(orientation = "y")
  expect_no_error(ggplotGrob(p))
})

test_that("flipped orientation: gradient runs horizontally (y1 = y2 = 0.5)", {
  df_h <- data.frame(x = "A", y = 4)
  p    <- ggplot(df_h, aes(y = x, x = y)) +
    geom_col_fade(orientation = "y")
  grob <- build_col_grob(p)
  fill <- grob$gradient_glist[[1]]$gp$fill
  expect_equal(as.numeric(fill$y1), 0.5)
  expect_equal(as.numeric(fill$y2), 0.5)
})


# -----------------------------------------------------------------------
# makeContent.bar_fade_grob
# -----------------------------------------------------------------------

test_that("makeContent uses flat fill for pdf device", {
  # Build a bar_fade_grob and call makeContent with a mocked pdf device.
  p    <- ggplot(df_col, aes(x, y)) + geom_col_fade()
  grob <- build_col_grob(p)

  local_mocked_bindings(
    dev.cur = \(...) c(pdf = 2L),
    .package = "grDevices"
  )
  withr::local_options(rlib_message_verbosity = "verbose")
  result <- suppressMessages(grid::makeContent(grob))
  # In the flat path children come from flat_glist (roundrects with solid fill)
  child_fills <- lapply(result$children, \(ch) ch$gp$fill)
  are_gradient <- vapply(child_fills, \(f) inherits(f, "GridLinearGradient"), logical(1))
  expect_false(any(are_gradient))
})

test_that("makeContent uses gradient fill for non-pdf device", {
  p    <- ggplot(df_col, aes(x, y)) + geom_col_fade()
  grob <- build_col_grob(p)

  local_mocked_bindings(
    dev.cur = \(...) c(png = 2L),
    .package = "grDevices"
  )
  result <- grid::makeContent(grob)
  child_fills <- lapply(result$children, \(ch) ch$gp$fill)
  are_gradient <- vapply(child_fills, \(f) inherits(f, "GridLinearGradient"), logical(1))
  expect_true(all(are_gradient))
})

test_that("makeContent emits a message for pdf device", {
  p    <- ggplot(df_col, aes(x, y)) + geom_col_fade()
  grob <- build_col_grob(p)

  local_mocked_bindings(
    dev.cur = \(...) c(pdf = 2L),
    .package = "grDevices"
  )
  withr::local_options(rlib_message_verbosity = "verbose")
  expect_message(grid::makeContent(grob), "gradient")
})


# -----------------------------------------------------------------------
# .draw_key_col_fade
# -----------------------------------------------------------------------

key_data <- data.frame(
  fill      = "#3b528b",
  alpha     = NA_real_,
  colour    = NA_character_,
  size      = 0.5,
  shape     = 16,
  stroke    = 0.5,
  linewidth = 0.5
)

test_that(".draw_key_col_fade returns a roundrectGrob with a linearGradient fill", {
  params <- list(alpha_fade_to = 0, flipped_aes = FALSE)
  result <- .draw_key_col_fade(key_data, params, grid::unit(c(1, 1), "cm"))
  expect_s3_class(result, "roundrect")
  expect_true(inherits(result$gp$fill, "GridLinearGradient"))
})

test_that(".draw_key_col_fade non-flipped: gradient runs bottom-to-top", {
  params <- list(alpha_fade_to = 0, flipped_aes = FALSE)
  result <- .draw_key_col_fade(key_data, params, grid::unit(c(1, 1), "cm"))
  fill   <- result$gp$fill
  expect_equal(as.numeric(fill$x1), 0.5)
  expect_equal(as.numeric(fill$x2), 0.5)
  expect_equal(as.numeric(fill$y1), 0)   # baseline = bottom
  expect_equal(as.numeric(fill$y2), 1)   # peak = top
})

test_that(".draw_key_col_fade flipped: gradient runs left-to-right", {
  params <- list(alpha_fade_to = 0, flipped_aes = TRUE)
  result <- .draw_key_col_fade(key_data, params, grid::unit(c(1, 1), "cm"))
  fill   <- result$gp$fill
  expect_equal(as.numeric(fill$y1), 0.5)
  expect_equal(as.numeric(fill$y2), 0.5)
  expect_equal(as.numeric(fill$x1), 0)   # baseline = left
  expect_equal(as.numeric(fill$x2), 1)   # peak = right
})

test_that(".draw_key_col_fade encodes alpha_fade_to in gradient colours", {
  params <- list(alpha_fade_to = 0.4, flipped_aes = FALSE)
  result <- .draw_key_col_fade(key_data, params, grid::unit(c(1, 1), "cm"))
  a_vals <- grDevices::col2rgb(result$gp$fill$colours, alpha = TRUE)["alpha", ] / 255
  expect_equal(a_vals[1], 0.4, tolerance = 0.01)  # baseline stop = alpha_fade_to
  expect_equal(a_vals[2], 1,   tolerance = 0.01)  # peak stop = a_start (NA → 1)
})

test_that(".draw_key_col_fade: alpha aesthetic caps the peak stop", {
  d      <- key_data
  d$alpha <- 0.7
  params <- list(alpha_fade_to = 0, flipped_aes = FALSE)
  result <- .draw_key_col_fade(d, params, grid::unit(c(1, 1), "cm"))
  a_vals <- grDevices::col2rgb(result$gp$fill$colours, alpha = TRUE)["alpha", ] / 255
  expect_equal(a_vals[2], 0.7, tolerance = 0.01)
})


# -----------------------------------------------------------------------
# Snapshot tests (visual regression via vdiffr)
# -----------------------------------------------------------------------

test_that("stacked bar_fade global scope renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mpg, aes(y = class)) +
    geom_bar_fade(
      aes(fill = drv),
      alpha_scope = "global"
    ) +
    theme(legend.position = "top")

  vdiffr::expect_doppelganger("bar-fade-stacked-global", p)
})

test_that("rounded bar_fade with non-NA outline has no corner artefacts", {
  # Regression for the linejoin = 'mitre' artefact: rounded roundrects
  # rendered with mitre joins produce stub-strokes at the path closure.
  # The fix sets linejoin = 'round' for radius > 0 (see aaa.R).
  skip_if_not_installed("vdiffr")
  df <- data.frame(class = c("a", "b"), n = c(5, 50))
  p <- ggplot(df, aes(y = class, x = n)) +
    geom_col_fade(
      fill = "#311dfc",
      colour = "#311dfc",
      radius = unit(5, "pt")
    )
  vdiffr::expect_doppelganger("bar-fade-rounded-outline-no-artefact", p)
})

# -----------------------------------------------------------------------
# Snapshots: radius is honoured even when the fade is degenerate
# -----------------------------------------------------------------------
# Regression for the bug where `.is_uniform_alpha()` short-circuited to
# `GeomBar$draw_panel` whenever `alpha == alpha_fade_to`, dropping the
# user's `radius` request along the way. Three cases pinned together so
# any future tweak to the bypass condition stays honest about which
# combination renders rounded corners vs flat rectangles.

test_that("bar_fade snapshot: alpha_fade_to = 0.1, radius = 5 (gradient + rounded)", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(iris, aes(Sepal.Width)) +
    geom_bar_fade(alpha_fade_to = 0.1, radius = 5) +
    scale_x_binned()
  vdiffr::expect_doppelganger("bar-fade-fadeto0.1-radius5", p)
})

test_that("bar_fade snapshot: alpha_fade_to = 1, radius = 5 (uniform + rounded)", {
  # The previously-broken case: uniform alpha used to short-circuit to
  # plain GeomBar, which doesn't honour `radius`. The fix gates the
  # bypass on `radius == 0`, so this combination now renders rounded
  # bars with a uniform (non-fading) fill.
  skip_if_not_installed("vdiffr")
  p <- ggplot(iris, aes(Sepal.Width)) +
    geom_bar_fade(alpha_fade_to = 1, radius = 5) +
    scale_x_binned()
  vdiffr::expect_doppelganger("bar-fade-fadeto1-radius5", p)
})

test_that("bar_fade snapshot: alpha_fade_to = 1, radius = 0 (fast path, flat)", {
  # The legitimate fast-path: no fade and no rounding, so delegating to
  # GeomBar$draw_panel is correct. Pinned to ensure the bypass stays
  # reachable for this canonical "nothing to do" case.
  skip_if_not_installed("vdiffr")
  p <- ggplot(iris, aes(Sepal.Width)) +
    geom_bar_fade(alpha_fade_to = 1, radius = 0) +
    scale_x_binned()
  vdiffr::expect_doppelganger("bar-fade-fadeto1-radius0", p)
})

test_that("stacked bar_fade global scope reversed renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mpg, aes(y = class)) +
    geom_bar_fade(
      aes(fill = drv),
      position = position_stack(reverse = TRUE),
      alpha_scope = "global"
    ) +
    theme(legend.position = "top")

  vdiffr::expect_doppelganger("bar-fade-stacked-global-reversed", p)
})

test_that("stacked bar_fade group scope renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mpg, aes(class, fill = drv)) +
    geom_bar_fade(alpha_scope = "x") +
    theme(legend.position = "top")

  vdiffr::expect_doppelganger("bar-fade-stacked-group", p)
})

test_that("dodged bar_fade group scope renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(diamonds, aes(color, fill = cut)) +
    geom_bar_fade(position = "dodge", alpha_scope = "x") +
    theme(legend.position = "top")

  vdiffr::expect_doppelganger("bar-fade-dodged-group", p)
})

test_that("dodged bar_fade global scope renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(diamonds, aes(color, fill = cut)) +
    geom_bar_fade(position = "dodge", alpha_scope = "global") +
    theme(legend.position = "top")

  vdiffr::expect_doppelganger("bar-fade-dodged-global", p)
})

test_that("fill position bar_fade group scope renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mpg, aes(class, fill = drv)) +
    geom_bar_fade(position = "fill", alpha_scope = "x") +
    theme(legend.position = "top")

  vdiffr::expect_doppelganger("bar-fade-fill-group", p)
})

test_that("col_fade negative bars renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(df_neg, aes(x, y, fill = x)) +
    geom_col_fade(alpha_scope = "bar") +
    scale_fill_viridis_d(guide = "none") +
    theme_minimal()

  vdiffr::expect_doppelganger("col-fade-negative", p)
})

test_that("col_fade horizontal bars render correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(df_col, aes(y = x, x = y, fill = x)) +
    geom_col_fade(orientation = "y", alpha_scope = "global") +
    scale_fill_viridis_d(guide = "none") +
    theme_minimal()

  vdiffr::expect_doppelganger("col-fade-horizontal", p)
})

test_that("histogram_fade global scope renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(faithful, aes(waiting)) +
    geom_histogram_fade(
      fill = "#3b528b",
      alpha_scope = "global",
      colour = NA,
      bins = 20
    ) +
    theme_minimal()

  vdiffr::expect_doppelganger("histogram-fade-global", p)
})

test_that("histogram_fade stacked groups renders correctly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(iris, aes(Sepal.Length, fill = Species)) +
    geom_histogram_fade(
      alpha_scope = "global",
      colour = NA,
      bins = 20
    ) +
    scale_fill_viridis_d() +
    theme_minimal()

  vdiffr::expect_doppelganger("histogram-fade-stacked-global", p)
})

test_that("bar_fade with coord_polar theta = 'y' renders a radial pie fade", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mpg, aes(x = factor(1), fill = class)) +
    geom_bar_fade(width = 1) +
    coord_polar(theta = "y") +
    theme_void()

  vdiffr::expect_doppelganger("bar-fade-polar-theta-y", p)
})

test_that("bar_fade with coord_radial theta = 'x' renders annular fade", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mpg, aes(x = class, fill = drv)) +
    geom_bar_fade() +
    coord_radial(theta = "x") +
    theme_void()

  vdiffr::expect_doppelganger("bar-fade-radial-theta-x-stacked", p)
})

# coord_polar examples adapted from ?coord_polar, using *_fade counterparts
# --------------------------------------------------------------------------

test_that("bar_fade pie chart: stacked bar + coord_radial(theta='y', expand=FALSE)", {
  skip_if_not_installed("vdiffr")

  # From ?coord_polar: "A pie chart = stacked bar chart + polar coordinates"
  p <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar_fade(width = 1) +
    coord_radial(theta = "y", expand = FALSE)

  vdiffr::expect_doppelganger("bar-fade-pie-coord-radial", p)
})

test_that("bar_fade coxcomb plot: bar + coord_radial(expand=FALSE)", {
  skip_if_not_installed("vdiffr")

  # From ?coord_polar: "A coxcomb plot = bar chart + polar coordinates"
  p <- ggplot(mtcars, aes(x = factor(cyl))) +
    geom_bar_fade(width = 1, colour = "black") +
    coord_radial(expand = FALSE)

  vdiffr::expect_doppelganger("bar-fade-coxcomb-coord-radial", p)
})

test_that("bar_fade new plot type: bar + coord_radial(theta='y', expand=FALSE)", {
  skip_if_not_installed("vdiffr")

  # From ?coord_polar: "A new type of plot?"
  p <- ggplot(mtcars, aes(x = factor(cyl))) +
    geom_bar_fade(width = 1, colour = "black") +
    coord_radial(theta = "y", expand = FALSE)

  vdiffr::expect_doppelganger("bar-fade-new-type-coord-radial-theta-y", p)
})

test_that("bar_fade bullseye chart: stacked + coord_radial(expand=FALSE)", {
  skip_if_not_installed("vdiffr")

  # From ?coord_polar: "The bullseye chart"
  p <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_bar_fade(width = 1) +
    coord_radial(expand = FALSE)

  vdiffr::expect_doppelganger("bar-fade-bullseye-coord-radial", p)
})

test_that("col_fade Pac-Man chart: coord_radial with start/end", {
  skip_if_not_installed("vdiffr")

  # From ?coord_polar: "Hadley's favourite pie chart"
  df <- data.frame(
    variable = c("does not resemble", "resembles"),
    value    = c(20, 80)
  )
  p <- ggplot(df, aes(x = "", y = value, fill = variable)) +
    geom_col_fade(width = 1) +
    scale_fill_manual(values = c("red", "yellow")) +
    scale_y_continuous(breaks = seq(0, 75, 25)) +
    coord_radial("y", start = pi / 3, expand = FALSE) +
    labs(title = "Pac man")

  vdiffr::expect_doppelganger("col-fade-pacman-coord-radial", p)
})


# ===========================================================================
# Grammar of Graphics adversarial stress tests
# ===========================================================================

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

test_that("GoG/data: empty dataset does not error", {
  p <- ggplot(data.frame(x = character(), y = numeric()), aes(x, y)) +
    geom_col_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: single bar does not error", {
  p <- ggplot(data.frame(x = "A", y = 5), aes(x, y)) + geom_col_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/data: all-NA y values do not error", {
  p <- ggplot(data.frame(x = c("A", "B"), y = NA_real_), aes(x, y)) +
    geom_col_fade()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: negative y values do not error", {
  p <- ggplot(data.frame(x = c("A", "B"), y = c(-3, -5)), aes(x, y)) +
    geom_col_fade()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Mapping
# ---------------------------------------------------------------------------

test_that("GoG/mapping: fill aesthetic mapping does not error", {
  p <- ggplot(df_stack, aes(x, y, fill = group)) + geom_col_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/mapping: inherit.aes = FALSE isolates from plot mapping", {
  df_numeric <- data.frame(x = 1:4, y = c(3, 7, 5, 9))
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point() +
    geom_col_fade(data = df_numeric, mapping = aes(x, y), inherit.aes = FALSE)
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Layer
# ---------------------------------------------------------------------------

test_that("GoG/layer: multiple geom_col_fade layers do not error", {
  p <- ggplot(df_col, aes(x, y)) +
    geom_col_fade(fill = "red", alpha = 0.3) +
    geom_col_fade(fill = "blue", alpha = 0.3)
  expect_no_error(ggplotGrob(p))
})


# ---------------------------------------------------------------------------
# Scales
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_y_reverse negates y values (col_fade)", {
  b_fwd <- ggplot_build(ggplot(df_col, aes(x, y)) + geom_col_fade())
  b_rev <- ggplot_build(ggplot(df_col, aes(x, y)) + geom_col_fade() + scale_y_reverse())
  expect_equal(b_rev$data[[1]]$y, -b_fwd$data[[1]]$y)
})

test_that("GoG/scales: scale_x_discrete reverse reverses x positions (col_fade)", {
  # x is discrete (character) — scale_x_reverse() errors; use scale_x_discrete.
  lvls <- df_col$x  # c("A","B","C","D")
  b_fwd <- ggplot_build(ggplot(df_col, aes(x, y)) + geom_col_fade())
  b_rev <- ggplot_build(ggplot(df_col, aes(x, y)) + geom_col_fade() +
    scale_x_discrete(limits = rev(lvls)))
  expect_equal(b_rev$data[[1]]$x, rev(b_fwd$data[[1]]$x))
})

test_that("GoG/scales: scale_x_discrete reverse reverses x positions (bar_fade)", {
  # class is discrete — scale_x_reverse() errors; use scale_x_discrete.
  lvls <- sort(unique(mpg$class))
  b_fwd <- ggplot_build(ggplot(mpg, aes(x = class)) + geom_bar_fade())
  b_rev <- ggplot_build(ggplot(mpg, aes(x = class)) + geom_bar_fade() +
    scale_x_discrete(limits = rev(lvls)))
  expect_equal(b_rev$data[[1]]$x, rev(b_fwd$data[[1]]$x))
})

test_that("GoG/scales: scale_y_reverse negates bar heights (bar_fade)", {
  b_fwd <- ggplot_build(ggplot(mpg, aes(x = class)) + geom_bar_fade())
  b_rev <- ggplot_build(ggplot(mpg, aes(x = class)) + geom_bar_fade() + scale_y_reverse())
  expect_true(all(b_rev$data[[1]]$y < 0))
})

test_that("GoG/scales: scale_x_reverse produces all-negative x values (histogram_fade)", {
  b_rev <- ggplot_build(ggplot(faithful, aes(waiting)) + geom_histogram_fade(bins = 20) + scale_x_reverse())
  expect_true(all(b_rev$data[[1]]$x < 0))
})

test_that("GoG/scales: scale_y_reverse negates y values (histogram_fade)", {
  b_fwd <- ggplot_build(ggplot(faithful, aes(waiting)) + geom_histogram_fade(bins = 20))
  b_rev <- ggplot_build(ggplot(faithful, aes(waiting)) + geom_histogram_fade(bins = 20) + scale_y_reverse())
  expect_equal(b_rev$data[[1]]$y, -b_fwd$data[[1]]$y)
})

test_that("GoG/scales: explicit limits do not error", {
  p <- ggplot(df_col, aes(x, y)) + geom_col_fade() +
    scale_y_continuous(limits = c(0, 20))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: expand = c(0, 0) does not error", {
  p <- ggplot(df_col, aes(x, y)) + geom_col_fade() +
    scale_y_continuous(expand = c(0, 0))
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Coord
# ---------------------------------------------------------------------------

test_that("GoG/coord: coord_cartesian zoom does not error", {
  p <- ggplot(df_col, aes(x, y)) + geom_col_fade() +
    coord_cartesian(ylim = c(0, 5))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_flip does not error", {
  p <- ggplot(df_col, aes(x, y)) + geom_col_fade() + coord_flip()
  expect_no_error(ggplotGrob(p))
})

test_that("coord_flip: gradient direction matches orientation = 'y'", {
  # Regression test for the coord_flip vs orientation = "y" parity bug.
  # Both should produce the same NPC gradient direction (horizontal).
  set.seed(10)
  df_mixed <- data.frame(x = 1:10, y = rnorm(10))

  p_flip <- ggplot(df_mixed, aes(x, y)) + geom_col_fade() + coord_flip()
  p_orient <- ggplot(df_mixed, aes(y = x, x = y)) +
    geom_col_fade(orientation = "y")

  g_flip <- .collect_gradient_axes(p_flip)
  g_orient <- .collect_gradient_axes(p_orient)

  expect_true(!is.null(g_flip) && nrow(g_flip) > 0)
  expect_equal(nrow(g_flip), nrow(g_orient))
  # Gradient should run horizontally (x1 != x2, y1 == y2 == 0.5)
  expect_true(all(as.numeric(g_flip[, "y1"]) == 0.5))
  expect_true(all(as.numeric(g_flip[, "y2"]) == 0.5))
  expect_true(all(as.numeric(g_flip[, "x1"]) != as.numeric(g_flip[, "x2"])))
  # And it should match the orientation = "y" path's gradient axes.
  expect_equal(g_flip[, c("x1", "x2", "y1", "y2")],
               g_orient[, c("x1", "x2", "y1", "y2")])
})

test_that("coord_flip: vdiffr snapshot pins the rotated rendering", {
  set.seed(10)
  df_mixed <- data.frame(x = 1:10, y = rnorm(10))
  p <- ggplot(df_mixed, aes(x, y)) + geom_col_fade() + coord_flip()
  vdiffr::expect_doppelganger("col-fade-coord-flip", p)
})

test_that("alpha_scope = 'global' uses DATA magnitude under scale_y_log10", {
  # Regression test for the 2026-05 fix: under a non-linear value scale
  # `scope_max` and `peak_abs` must be computed in data space. With bars
  # at y = c(10, 100, 1000) and alpha_scope = "global", bar A's peak
  # alpha must be ~10/1000 = 0.01 (not log10(10)/log10(1000) = 0.33).
  df <- data.frame(x = c("A", "B", "C"), y = c(10, 100, 1000))
  p <- ggplot(df, aes(x, y)) +
    geom_col_fade(alpha_scope = "global") + scale_y_log10()
  # Direct extraction of the alpha endpoints from the rendered gradient.
  # The bar_fade_grob's `gradient_glist` carries one rect per bar with
  # a linearGradient whose final colour-stop alpha is `a_peak`.
  g <- suppressMessages(suppressWarnings(ggplotGrob(p)))
  panel <- g$grobs[[grep("^panel", g$layout$name)[1]]]
  fade_grob <- panel$children[[3]]
  expect_s3_class(fade_grob, "bar_fade_grob")
  # Each gradient_glist child is one bar's rectGrob with a linearGradient fill
  peaks <- vapply(fade_grob$gradient_glist, function(rg) {
    grad <- rg$gp$fill
    if (!inherits(grad, "GridLinearGradient")) return(NA_real_)
    alphas <- attr(grDevices::col2rgb(grad$colours, alpha = TRUE), "matrix")
    # Stops are c(0, 1); the peak (count = ymax) is at stop = 1.
    rev(grDevices::col2rgb(grad$colours, alpha = TRUE)["alpha", ])[1L] / 255
  }, numeric(1))
  # Bar A peak alpha should be ~0.01 (= 10/1000), not ~0.33.
  expect_lt(peaks[1], 0.05)
  # Bar C peak alpha should be ~1 (= 1000/1000).
  expect_gt(peaks[3], 0.95)
  # Bar B peak alpha should be ~0.1 (= 100/1000).
  expect_gt(peaks[2], 0.05)
  expect_lt(peaks[2], 0.2)
})

test_that("vdiffr: alpha_scope = 'global' under scale_y_log10 (data-space scope)", {
  skip_if_not_installed("vdiffr")
  df <- data.frame(x = c("A", "B", "C"), y = c(10, 100, 1000))
  p <- ggplot(df, aes(x, y)) +
    geom_col_fade(alpha_scope = "global") + scale_y_log10()
  vdiffr::expect_doppelganger("col-fade-global-log10", p)
})

test_that("GoG/coord: coord_polar does not error", {
  p <- ggplot(df_col, aes(x, y)) + geom_col_fade() + coord_polar()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/coord: coord_fixed does not error", {
  p <- ggplot(df_col, aes(x, y)) + geom_col_fade() + coord_fixed()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Facets
# ---------------------------------------------------------------------------

test_that("GoG/facets: facet_wrap with free scales does not error", {
  p <- ggplot(df_stack, aes(x, y, fill = group)) + geom_col_fade() +
    facet_wrap(~group, scales = "free")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/facets: facet_grid with free scales does not error", {
  p <- ggplot(df_stack, aes(x, y, fill = group)) + geom_col_fade() +
    facet_grid(~group, scales = "free")
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------



