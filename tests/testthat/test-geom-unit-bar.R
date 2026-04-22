library(ggplot2)

# ---------------------------------------------------------------------------
# Shared test data
# ---------------------------------------------------------------------------

df <- data.frame(
  bar    = factor(rep(c("A", "B"), each = 2), levels = c("A", "B")),
  gender = factor(rep(c("Female", "Male"), 2)),
  n      = c(3, 5, 6, 2)
)

base_p <- ggplot(df, aes(x = bar, y = n, fill = gender)) +
  geom_unit_col()

build_data <- function(p) {
  if (is.list(p)) p <- Reduce(`+`, p, init = ggplot())
  ggplot_build(p)$data[[1]]
}

# ---------------------------------------------------------------------------
# Stat: bar-level output
# ---------------------------------------------------------------------------

test_that("geom_unit_col: one row per segment in build data", {
  d <- build_data(base_p)
  # 2 bars * 2 fill groups = 4 segments

  expect_equal(nrow(d), 4L)
})

test_that("geom_unit_col: count column present and correct", {
  d <- build_data(base_p)
  expect_true("count" %in% names(d))
  expect_equal(sort(d$count), sort(df$n))
})

test_that("geom_unit_col: stacked bars have cumulative ymin/ymax", {
  d <- build_data(base_p)
  # Default position = "stack": segments stack within each bar
  bar_A <- d[d$x == 1, ]
  bar_A <- bar_A[order(bar_A$ymin), ]
  # First segment starts at 0

  expect_equal(bar_A$ymin[1], 0)
  # Second segment starts where first ends
  expect_equal(bar_A$ymin[2], bar_A$ymax[1])
})

test_that("geom_unit_col: total count matches total y per bar", {
  d <- build_data(base_p)
  totals <- tapply(d$count, d$x, sum)
  expect_equal(as.integer(totals), c(8L, 8L))  # A: 3+5, B: 6+2
})

# ---------------------------------------------------------------------------
# NA / non-positive values
# ---------------------------------------------------------------------------

test_that("geom_unit_col: NA values are dropped with a warning from ggplot2", {
  df_na <- rbind(df, data.frame(bar = factor("A"), gender = factor("Female"), n = NA_real_))
  p <- ggplot(df_na, aes(x = bar, y = n, fill = gender)) + geom_unit_col()
  expect_warning(ggplot_build(p), regexp = "non-finite")
})

test_that("geom_unit_col: zero values produce an empty segment (no warning)", {
  df_zero <- rbind(df, data.frame(bar = factor("A"), gender = factor("Female"), n = 0))
  p <- ggplot(df_zero, aes(x = bar, y = n, fill = gender)) + geom_unit_col()
  expect_no_warning(ggplot_build(p))
  expect_no_error(ggplotGrob(p))
})

test_that("geom_unit_col: negative values tile downward from the baseline", {
  df_neg <- data.frame(x = "A", y = -3)
  p <- ggplot(df_neg, aes(x, y)) + geom_unit_col()
  b <- ggplot_build(p)
  # ymin/ymax sit entirely below the baseline and span the full negative range.
  d <- b$data[[1L]]
  expect_equal(d$ymin, -3)
  expect_equal(d$ymax, 0)
  g <- GeomBarCells$draw_panel(
    d, b$layout$panel_params[[1L]], b$layout$coord,
    radius = grid::unit(0, "npc")
  )
  expect_equal(length(as.numeric(g$y)), 3L)
})

test_that("geom_unit_col: fractional y tiles into floor + partial cells", {
  # y = 3.7 → 3 full unit cells (height 1) + 1 partial cell (height 0.7).
  df_frac <- data.frame(x = "A", y = 3.7)
  p <- ggplot(df_frac, aes(x, y)) + geom_unit_col()
  b  <- ggplot_build(p)
  g  <- GeomBarCells$draw_panel(
    b$data[[1L]], b$layout$panel_params[[1L]], b$layout$coord,
    radius = grid::unit(0, "npc")
  )
  # 4 cells total (3 full + 1 partial).
  expect_equal(length(as.numeric(g$y)), 4L)
})

test_that("geom_unit_col: fractional y < 1 renders as a single partial cell", {
  df_frac <- data.frame(x = "A", y = 0.3)
  p <- ggplot(df_frac, aes(x, y)) + geom_unit_col()
  b  <- ggplot_build(p)
  g  <- GeomBarCells$draw_panel(
    b$data[[1L]], b$layout$panel_params[[1L]], b$layout$coord,
    radius = grid::unit(0, "npc")
  )
  # 1 partial cell — no full cells.
  expect_equal(length(as.numeric(g$y)), 1L)
})

test_that("geom_unit_col: orientation = 'y' flips bars to horizontal", {
  df_h <- data.frame(x = 1:5, y = c(2, 4, 3, 5, 1))
  p <- ggplot(df_h, aes(x, y)) + geom_unit_col(orientation = "y") + coord_equal()
  d <- ggplot_build(p)$data[[1L]]
  # Value axis is x: xmin fixed at 0, xmax holds the bar length (d$x after flip).
  expect_true(all(d$flipped_aes))
  expect_equal(d$xmin, rep(0, nrow(d)))
  expect_equal(d$xmax, d$x)
  expect_no_error(ggplotGrob(p))
})

test_that("geom_unit_bar: orientation auto-detected from aes(y =)", {
  p <- ggplot(mpg, aes(y = class)) + geom_unit_bar() + coord_equal()
  d <- ggplot_build(p)$data[[1L]]
  expect_true(all(d$flipped_aes))
  expect_no_error(ggplotGrob(p))
})

test_that("geom_unit_col: max_cells caps the per-panel cell count", {
  df_big <- data.frame(x = "A", y = 1e5)
  p <- ggplot(df_big, aes(x, y)) + geom_unit_col() + coord_equal()
  expect_warning(
    ggplotGrob(p),
    regexp = "Refusing to tile"
  )
  # Opt out: Inf lets it through (no warning).
  p_inf <- ggplot(data.frame(x = "A", y = 50), aes(x, y)) +
    geom_unit_col(max_cells = Inf) + coord_equal()
  expect_no_warning(ggplotGrob(p_inf))
})

test_that("geom_unit_col: radius must be a grid::unit (or NULL)", {
  df_r <- data.frame(x = c("A","B"), y = c(1, 2))
  expect_error(
    ggplotGrob(ggplot(df_r, aes(x, y)) + geom_unit_col(radius = 5)),
    regexp = "must be a .*grid::unit"
  )
  expect_error(
    ggplotGrob(ggplot(df_r, aes(x, y)) + geom_unit_col(radius = "big")),
    regexp = "must be a .*grid::unit"
  )
  # NULL is tolerated and treated as sharp corners.
  expect_no_error(
    ggplotGrob(ggplot(df_r, aes(x, y)) + geom_unit_col(radius = NULL) + coord_equal())
  )
})


# ---------------------------------------------------------------------------
# Rendering
# ---------------------------------------------------------------------------

test_that("geom_unit_col: builds with radius > 0", {
  p <- ggplot(df, aes(x = bar, y = n, fill = gender)) +
    geom_unit_col(radius = grid::unit(2, "pt")) + coord_equal()
  expect_no_error(ggplotGrob(p))
})

test_that("geom_unit_col: single bar builds without error", {
  p <- ggplot(
    data.frame(bar = factor("A"), gender = factor("F"), n = 10),
    aes(x = bar, y = n, fill = gender)
  ) + geom_unit_col() + coord_equal()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Cell geometry at bar level
# ---------------------------------------------------------------------------

test_that("geom_unit_col: xmin < xmax and ymin < ymax for all segments", {
  d <- build_data(base_p)
  expect_true(all(d$xmin < d$xmax))
  expect_true(all(d$ymin < d$ymax))
})

# ---------------------------------------------------------------------------
# Position adjustments
# ---------------------------------------------------------------------------

test_that("geom_unit_col: position stack (default) builds without warning", {
  expect_no_warning(
    ggplotGrob(ggplot(df, aes(x = bar, y = n, fill = gender)) + geom_unit_col())
  )
})

test_that("geom_unit_col: position dodge builds without error", {
  p <- ggplot(df, aes(x = bar, y = n, fill = gender)) +
    geom_unit_col(position = "dodge")
  expect_no_error(ggplotGrob(p))
})

test_that("geom_unit_col: position_stack(reverse = TRUE) reverses order", {
  p <- ggplot(df, aes(x = bar, y = n, fill = gender)) +
    geom_unit_col(position = position_stack(reverse = TRUE))
  d <- build_data(p)
  bar_A <- d[d$x == 1, ]
  bar_A <- bar_A[order(bar_A$ymin), ]
  # With reverse, first group in data should be at bottom
  # (default stack puts last group at bottom)
  d_normal <- build_data(base_p)
  bar_A_normal <- d_normal[d_normal$x == 1, ]
  bar_A_normal <- bar_A_normal[order(bar_A_normal$ymin), ]
  # Bottom segment fill should differ between normal and reversed
  expect_false(identical(bar_A$fill[1], bar_A_normal$fill[1]))
})

test_that("geom_unit_col: position fill normalises to [0, 1]", {
  p <- ggplot(df, aes(x = bar, y = n, fill = gender)) +
    geom_unit_col(position = "fill")
  d <- build_data(p)
  # Each bar should sum to 1 in display units
  for (xval in unique(d$x)) {
    bar <- d[d$x == xval, ]
    expect_equal(max(bar$ymax), 1, tolerance = 1e-9)
  }
})

test_that("geom_unit_bar: position dodge builds without error", {
  p <- ggplot(mpg, aes(x = class, fill = drv)) +
    geom_unit_bar(position = "dodge")
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Orientation (y aesthetic / flipped_aes)
# ---------------------------------------------------------------------------

test_that("geom_unit_bar: y aesthetic produces horizontal bars", {
  p <- ggplot(diamonds, aes(y = color, fill = cut)) + geom_unit_bar()
  d <- build_data(p)
  expect_true(all(d$flipped_aes))
  # Value axis = x: xmin should start at 0
  expect_true(all(d$xmin >= 0 | d$xmax >= 0))
  expect_no_error(ggplotGrob(p))
})

test_that("geom_unit_bar: y aesthetic + dodge builds", {
  p <- ggplot(diamonds, aes(y = color, fill = cut)) +
    geom_unit_bar(position = "dodge")
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Coordinate systems
# ---------------------------------------------------------------------------

test_that("geom_unit_col: coord_equal builds", {
  expect_no_error(ggplotGrob(base_p + coord_equal()))
})

test_that("geom_unit_col: coord_cartesian builds", {
  expect_no_error(ggplotGrob(base_p + coord_cartesian()))
})

# ---------------------------------------------------------------------------
# geom_unit_bar: stat = "count"
# ---------------------------------------------------------------------------

df_bar <- data.frame(
  grp  = factor(rep(c("A", "B"), times = c(5, 8))),
  fill = factor(c(rep("x", 3), rep("y", 2), rep("x", 4), rep("y", 4)))
)

test_that("geom_unit_bar: count per bar matches observation count", {
  p <- ggplot(df_bar, aes(x = grp, fill = fill)) + geom_unit_bar()
  d <- ggplot_build(p)$data[[1]]
  totals <- tapply(d$count, d$x, sum)
  expect_equal(as.integer(totals), c(5L, 8L))
})

test_that("geom_unit_bar: builds without error", {
  p <- ggplot(df_bar, aes(x = grp, fill = fill)) + geom_unit_bar() + coord_equal()
  expect_no_error(ggplotGrob(p))
})

test_that("geom_unit_bar: original aes(x) example works", {
  set.seed(1)
  df2 <- data.frame(x = 1:10, y = sample(20, 10))
  expect_no_error(
    suppressWarnings(ggplot_build(ggplot(df2, aes(x)) + geom_unit_bar(aes(fill = x))))
  )
})

# ===========================================================================
# Grammar of Graphics adversarial stress tests
# ===========================================================================

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

test_that("GoG/data: empty dataset does not error", {
  p <- ggplot(data.frame(x = factor(character()), y = numeric()), aes(x, y)) +
    geom_unit_col()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: single cell does not error", {
  p <- ggplot(data.frame(x = "A", y = 1), aes(x, y)) + geom_unit_col()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/data: all-NA y values do not error", {
  p <- ggplot(data.frame(x = c("A", "B"), y = NA_real_), aes(x, y)) +
    geom_unit_col()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Mapping
# ---------------------------------------------------------------------------

test_that("GoG/mapping: inherit.aes = FALSE isolates from plot mapping", {
  p <- ggplot(mpg, aes(class, hwy, colour = drv)) +
    geom_point() +
    geom_unit_col(data = df, mapping = aes(x = bar, y = n), inherit.aes = FALSE)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Layer
# ---------------------------------------------------------------------------

test_that("GoG/layer: multiple geom_unit_col layers do not error", {
  p <- ggplot(df, aes(x = bar, y = n)) +
    geom_unit_col(fill = "red") +
    geom_unit_col(fill = "blue")
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Scales
# ---------------------------------------------------------------------------

test_that("GoG/scales: explicit y limits do not error", {
  p <- ggplot(df, aes(x = bar, y = n)) + geom_unit_col() +
    scale_y_continuous(limits = c(0, 20))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: scale_y_reverse negates y values (unit_col)", {
  # scale_y_reverse transforms before the stat runs; negatives are now kept
  # and tile downward from the baseline.
  b_fwd <- ggplot_build(ggplot(df, aes(x = bar, y = n)) + geom_unit_col())
  b_rev <- ggplot_build(ggplot(df, aes(x = bar, y = n)) + geom_unit_col() + scale_y_reverse())
  expect_equal(nrow(b_rev$data[[1]]), nrow(b_fwd$data[[1]]))
  expect_true(all(b_rev$data[[1]]$y <= 0))
})

test_that("GoG/scales: scale_x_discrete reverse reverses x positions (unit_col)", {
  # bar is a factor — scale_x_reverse() errors on discrete x; use scale_x_discrete.
  lvls <- levels(df$bar)
  b_fwd <- ggplot_build(ggplot(df, aes(x = bar, y = n)) + geom_unit_col())
  b_rev <- ggplot_build(ggplot(df, aes(x = bar, y = n)) + geom_unit_col() +
    scale_x_discrete(limits = rev(lvls)))
  # 2 bars × same row count; sorted positions must be equal; order must differ.
  expect_equal(sort(b_rev$data[[1]]$x), sort(b_fwd$data[[1]]$x))
  expect_false(identical(b_rev$data[[1]]$x, b_fwd$data[[1]]$x))
})

test_that("GoG/scales: scale_y_reverse negates y values (unit_bar)", {
  # StatCountCells computes counts (positive), then scale_y_reverse negates them.
  # Some class × drv combinations are absent → count = 0 → negated = -0 = 0.
  b_rev <- suppressWarnings(
    ggplot_build(ggplot(mpg, aes(x = class, fill = drv)) + geom_unit_bar() + scale_y_reverse())
  )
  expect_true(all(b_rev$data[[1]]$y <= 0))
  expect_true(any(b_rev$data[[1]]$y < 0))
})

test_that("GoG/scales: scale_x_discrete reverse changes x positions (unit_bar)", {
  # class has unequal numbers of drv groups, so sorted x differs between fwd/rev
  # (different classes are at different positions).  Verify rows are preserved
  # and positions actually changed.
  lvls <- sort(unique(mpg$class))
  b_fwd <- ggplot_build(ggplot(mpg, aes(x = class, fill = drv)) + geom_unit_bar())
  b_rev <- ggplot_build(ggplot(mpg, aes(x = class, fill = drv)) + geom_unit_bar() +
    scale_x_discrete(limits = rev(lvls)))
  expect_equal(nrow(b_rev$data[[1]]), nrow(b_fwd$data[[1]]))
  expect_false(identical(b_rev$data[[1]]$x, b_fwd$data[[1]]$x))
})

test_that("GoG/scales: scale_y_reverse negates y values (unit_histogram)", {
  # StatBin computes bin counts (positive); scale_y_reverse negates them.
  b_rev <- suppressWarnings(
    ggplot_build(ggplot(faithful, aes(waiting)) + geom_unit_histogram(bins = 10) + scale_y_reverse())
  )
  expect_true(all(b_rev$data[[1]]$y < 0))
})

test_that("GoG/scales: scale_x_reverse produces all-negative x values (unit_histogram)", {
  b_rev <- ggplot_build(ggplot(faithful, aes(waiting)) + geom_unit_histogram(bins = 10) + scale_x_reverse())
  expect_true(all(b_rev$data[[1]]$x < 0))
})

test_that("GoG/scales: scale_y_continuous(trans = 'log10') runs stat on transformed y", {
  # Scale transform runs BEFORE the stat, so StatBarCells sees log10(y) values
  # (0.477, 0.699, ...).  All round to 0 or 1; rows that underflow are dropped
  # with a warning.  The geom must cope without erroring.
  p <- base_p + scale_y_continuous(trans = "log10")
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/scales: scale_fill_brewer builds", {
  p <- base_p + scale_fill_brewer(palette = "Set1")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: scale_fill_manual builds", {
  p <- base_p + scale_fill_manual(values = c(Female = "#FF6384", Male = "#36A2EB"))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: sec.axis does not break rendering", {
  p <- base_p + scale_y_continuous(sec.axis = sec_axis(~ . * 10, name = "ten-count"))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: scale_x_discrete(limits = subset) drops a level", {
  # Removing "B" from the scale should yield only bars for "A".
  b <- suppressWarnings(ggplot_build(base_p + scale_x_discrete(limits = "A")))
  expect_true(all(b$data[[1]]$x == 1))
})

test_that("GoG/scales: scale_y_continuous(expand = c(0, 0)) builds with cells flush to edge", {
  p <- base_p + scale_y_continuous(expand = c(0, 0))
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Coord
# ---------------------------------------------------------------------------

test_that("GoG/coord: coord_polar does not error", {
  p <- ggplot(df, aes(x = bar, y = n)) + geom_unit_col() + coord_polar()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/coord: coord_flip swaps axes without error", {
  # coord_flip is semantically different from aes(y = ...): the stat runs in
  # native orientation, then coord_flip swaps in device space.
  p <- ggplot(df, aes(x = bar, y = n, fill = gender)) +
    geom_unit_col() + coord_flip()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/coord: coord_fixed(ratio = 2) builds", {
  # Users explicitly asking for non-square cells via coord_equal's ratio hook.
  p <- base_p + coord_fixed(ratio = 2)
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_cartesian with ylim zoom that cuts cells", {
  # Clipping with ylim should drop no rows from the stat (coord_cartesian zooms
  # after stat) but some cells will fall outside the panel — must not error.
  p <- base_p + coord_cartesian(ylim = c(0, 3))
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/coord: coord_radial(theta = 'x') does not error", {
  # theta = "y" is covered by a vdiffr below; this exercises the other axis.
  p <- ggplot(df, aes(x = bar, y = n, fill = gender)) +
    geom_unit_col() + coord_radial(theta = "x")
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/coord: coord_transform(y = 'log10') does not error", {
  # Stat rounds y to integers in data space BEFORE coord transform.
  # coord_transform then remaps positions; cells compress near the baseline
  # but the geom itself must not break.
  p <- base_p + coord_transform(y = "log10")
  expect_no_error(suppressMessages(suppressWarnings(ggplotGrob(p))))
})

# ---------------------------------------------------------------------------
# Facets
# ---------------------------------------------------------------------------

test_that("GoG/facets: facet_wrap with free scales does not error", {
  p <- ggplot(df, aes(x = bar, y = n, fill = gender)) +
    geom_unit_col() + facet_wrap(~gender, scales = "free")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/facets: facet_grid with free scales does not error", {
  p <- ggplot(df, aes(x = bar, y = n, fill = gender)) +
    geom_unit_col() + facet_grid(~gender, scales = "free")
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------

test_that("GoG/theme: theme_void does not error", {
  p <- ggplot(df, aes(x = bar, y = n)) + geom_unit_col() + theme_void()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: theme_bw does not error", {
  p <- ggplot(df, aes(x = bar, y = n)) + geom_unit_col() + theme_bw()
  expect_no_error(ggplotGrob(p))
})


# ===========================================================================
# Regression: stat param not formally exposed
# ===========================================================================

test_that("geom_unit_bar: stat = 'count' (default) uses StatCountCells", {
  lyr <- geom_unit_bar()
  expect_true(inherits(lyr$stat, "StatCountCells"))
})

test_that("geom_unit_col: stat = 'identity' (default) uses StatBarCells", {
  lyr <- geom_unit_col()
  expect_true(inherits(lyr$stat, "StatBarCells"))
})

test_that("geom_unit_bar: stat = 'bin' is routed correctly, not silently ignored", {
  lyr <- geom_unit_bar(stat = "bin")
  expect_false(inherits(lyr$stat, "StatCountCells"))
  expect_true(inherits(lyr$stat, "StatBin"))
})

test_that("geom_unit_bar: stat as ggproto class object (not string) is accepted", {
  lyr_str <- geom_unit_bar(stat = "bin")
  lyr_cls <- geom_unit_bar(stat = ggplot2::StatBin)
  expect_identical(class(lyr_str$stat), class(lyr_cls$stat))
  expect_true(inherits(lyr_cls$stat, "StatBin"))
})

test_that("geom_unit_col: stat = 'identity' renders (ymin/ymax come from position)", {
  # With the new design draw_panel tiles from ymax - ymin directly, so stats
  # that don't emit a `count` column still work as long as positioning
  # produces valid ymin/ymax.
  p <- ggplot(
    data.frame(x = c("A", "B"), y = c(3, 5)),
    aes(x, y)
  ) + geom_unit_col(stat = "identity")
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_unit_bar: aes(fill = after_stat(count)) maps the computed count", {
  # after_stat(count) reaches the fill aesthetic after StatCountCells runs.
  p <- ggplot(mpg, aes(x = class)) +
    geom_unit_bar(aes(fill = after_stat(count)))
  expect_no_error(ggplotGrob(p))
})

test_that("geom_unit_col: custom stat that emits count works as drop-in", {
  # Contract: any stat emitting a `count` column should render without issue.
  StatCustomCount <- ggplot2::ggproto(
    "StatCustomCount",
    ggplot2::Stat,
    required_aes = c("x", "y"),
    compute_panel = function(data, scales) {
      data$count <- as.integer(round(data$y))
      data
    }
  )
  p <- ggplot(
    data.frame(x = c("A", "B"), y = c(3, 5)),
    aes(x, y)
  ) + geom_unit_col(stat = StatCustomCount)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ===========================================================================
# Rendering correctness -- grob structure for GeomBarCells
# ===========================================================================

# Helper to call GeomBarCells$draw_panel directly
draw_panel_bar_cells <- function(p) {
  b  <- ggplot_build(p)
  ld <- b$data[[1L]]
  pp <- b$layout$panel_params[[1L]]
  co <- b$layout$coord
  layer <- b$plot$layers[[1L]]
  radius <- layer$geom_params$radius %||% grid::unit(0, "npc")
  GeomBarCells$draw_panel(ld, pp, co, radius = radius)
}

# ---------------------------------------------------------------------------
# Section D: Fast-path grob structure (radius == 0)
# ---------------------------------------------------------------------------

test_that("default radius=0 returns rectGrob, not gTree", {
  df_rc <- data.frame(x = c("A", "B"), y = c(3, 5))
  p <- ggplot(df_rc, aes(x, y)) + geom_unit_col()
  g <- draw_panel_bar_cells(p)

  expect_true(inherits(g, "rect"))
  expect_false(inherits(g, "gTree"))
})

test_that("all rectGrob coordinates are finite and in [0,1] NPC range", {
  df_rc <- data.frame(x = c("A", "B"), y = c(3, 5))
  p <- ggplot(df_rc, aes(x, y)) + geom_unit_col()
  g <- draw_panel_bar_cells(p)

  xs <- as.numeric(g$x)
  ys <- as.numeric(g$y)
  ws <- as.numeric(g$width)
  hs <- as.numeric(g$height)

  expect_true(all(is.finite(xs)))
  expect_true(all(xs >= 0 & xs <= 1))
  expect_true(all(is.finite(ys)))
  expect_true(all(ys >= 0 & ys <= 1))
  expect_true(all(ws > 0))
  expect_true(all(hs > 0))
})

# ---------------------------------------------------------------------------
# Section E: Rounded-path grob structure (radius > 0)
# ---------------------------------------------------------------------------

test_that("radius > 0 returns gTree, not rectGrob", {
  df_rc <- data.frame(x = c("A", "B"), y = c(3, 5))
  p <- ggplot(df_rc, aes(x, y)) + geom_unit_col(radius = grid::unit(2, "pt"))
  g <- draw_panel_bar_cells(p)

  expect_true(inherits(g, "gTree"))
  expect_false(inherits(g, "rect"))
})

test_that("rounded gTree children count equals total cell count", {
  df_rc <- data.frame(x = c("A", "B"), y = c(3, 5))
  p <- ggplot(df_rc, aes(x, y)) + geom_unit_col(radius = grid::unit(2, "pt"))
  g <- draw_panel_bar_cells(p)

  expect_equal(length(g$children), 8L)
})

test_that("all rounded gTree children are roundrect grobs", {
  df_rc <- data.frame(x = c("A", "B"), y = c(3, 5))
  p <- ggplot(df_rc, aes(x, y)) + geom_unit_col(radius = grid::unit(2, "pt"))
  g <- draw_panel_bar_cells(p)

  classes <- sapply(g$children, function(ch) inherits(ch, "roundrect"))
  expect_true(all(classes))
})

# ---------------------------------------------------------------------------
# Section F: Cell geometry correctness
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Section G: vdiffr snapshots
# ---------------------------------------------------------------------------

test_that("vdiffr: geom_unit_bar basic (fast path, radius=0)", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mpg, aes(x = class, fill = drv)) +
    geom_unit_bar() +
    coord_equal()
  vdiffr::expect_doppelganger("bar-cells-basic", p)
})

test_that("vdiffr: geom_unit_bar with rounded corners", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mpg, aes(x = class, fill = drv)) +
    geom_unit_bar(radius = grid::unit(3, "pt")) +
    coord_equal()
  vdiffr::expect_doppelganger("bar-cells-rounded", p)
})

# coord_cartesian: vertical and horizontal orientation.
# The horizontal case (aes(y=)) would have caught the "gap lost after flip"
# regression — cells must show visible separation in both orientations.
test_that("vdiffr: geom_unit_bar coord_cartesian vertical", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mpg, aes(x = class, fill = drv)) +
    geom_unit_bar() +
    coord_cartesian()
  vdiffr::expect_doppelganger("bar-cells-cartesian-vertical", p)
})

test_that("vdiffr: geom_unit_bar coord_cartesian horizontal (flipped)", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mpg, aes(y = class, fill = drv)) +
    geom_unit_bar() +
    coord_cartesian()
  vdiffr::expect_doppelganger("bar-cells-cartesian-horizontal", p)
})

test_that("vdiffr: geom_unit_bar coord_radial theta=y (unit pie)", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
    geom_unit_bar(width = 1) +
    coord_radial(theta = "y")
  vdiffr::expect_doppelganger("bar-cells-radial-theta-y", p)
})

test_that("vdiffr: geom_unit_col fractional y (floor + partial)", {
  skip_if_not_installed("vdiffr")
  # Mixed integer and fractional values to show unit cells plus partial tops.
  df_frac <- data.frame(x = c("A", "B", "C"), y = c(3, 3.7, 1.2))
  p <- ggplot(df_frac, aes(x, y, fill = x)) +
    geom_unit_col() +
    coord_equal()
  vdiffr::expect_doppelganger("bar-cells-fractional", p)
})

test_that("vdiffr: geom_unit_bar legend key under coord_radial", {
  skip_if_not_installed("vdiffr")
  # Locks in both the legend key rendering (inherited from GeomBar) and the
  # arc-wedge tiling under coord_radial with default theta = "x".
  p <- ggplot(mpg, aes(x = class, fill = drv)) +
    geom_unit_bar() +
    coord_radial()
  vdiffr::expect_doppelganger("bar-cells-radial-legend", p)
})

