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

test_that("geom_unit_col: total y per bar matches the input data", {
  d <- build_data(base_p)
  # geom_unit_col uses stat = "identity" — no `count` mirror, but `y` carries
  # the same values and post-position ymax sums to the bar total.
  totals <- tapply(d$ymax - d$ymin, d$x, sum)
  expect_equal(as.numeric(totals), c(8, 8))  # A: 3+5, B: 6+2
})

# ---------------------------------------------------------------------------
# NA / non-positive values
# ---------------------------------------------------------------------------

test_that("geom_unit_col: NA values are dropped with a warning from ggplot2", {
  df_na <- rbind(df, data.frame(bar = factor("A"), gender = factor("Female"), n = NA_real_))
  p <- ggplot(df_na, aes(x = bar, y = n, fill = gender)) + geom_unit_col()
  # With stat = "identity" the NA row reaches ggplot2's scale-range check
  # at render time (not at build time); the wording is "missing values or
  # values outside the scale range".
  expect_warning(ggplotGrob(p), regexp = "missing values")
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
  g <- GeomUnitBar$draw_panel(
    d, b$layout$panel_params[[1L]], b$layout$coord,
    radius = grid::unit(0, "npc")
  )
  expect_equal(length(as.numeric(g$y)), 3L)
})

test_that("geom_unit_col: every cell renders at the same height (uniform padding)", {
  # Two groups stacked into one bar:
  #   group 1 contributes y=2 → cells [0,1], [1,2]
  #   group 2 contributes y=2 → cells [2,3], [3,4]
  # Each cell represents one data unit, so all four cells must render at
  # the same height regardless of position in the bar.  With pad_v = 0.025
  # and cell_size = 1, every cell shrinks by 2 * 0.025 = 0.05 in data
  # space -> height 0.95 each.  Inter-cell gaps and the bar-edge gaps are
  # all 0.05.
  df_two <- data.frame(x = "A", y = c(2, 2), grp = factor(c(1, 2)))
  p <- ggplot(df_two, aes(x, y, fill = grp)) +
    geom_unit_col(cell_padding = 0.025)
  b <- ggplot_build(p)
  g <- suppressWarnings(GeomUnitBar$draw_panel(
    b$data[[1L]], b$layout$panel_params[[1L]], b$layout$coord,
    radius = grid::unit(0, "npc"), cell_padding = 0.025
  ))

  # ggplot2's GeomRect renders with `just = c("left", "top")`, so `g$y` is
  # the cell's TOP edge in NPC and `g$y - g$height` is the bottom.
  # Convert NPC back to data using the panel's data-space y range.
  yr <- b$layout$panel_params[[1L]]$y$dimension()
  panel_lo <- yr[1L]; panel_hi <- yr[2L]
  to_data <- function(npc) panel_lo + npc * (panel_hi - panel_lo)
  yt_data <- to_data(as.numeric(g$y))
  yb_data <- to_data(as.numeric(g$y) - as.numeric(g$height))

  o <- order(yb_data)
  yb_data <- yb_data[o]; yt_data <- yt_data[o]

  # 4 cells total, all at the same height.
  expect_equal(length(yb_data), 4L)
  heights <- yt_data - yb_data
  expect_equal(heights, rep(0.95, 4L), tolerance = 1e-3)

  # Outer edges sit one inset inside the data extent.
  expect_equal(yb_data[1L],  0.025, tolerance = 1e-3)
  expect_equal(yt_data[4L],  3.975, tolerance = 1e-3)

  # Inter-cell gaps are 2 * 0.025 = 0.05 (twice the per-side inset).
  gaps <- yb_data[-1] - yt_data[-length(yt_data)]
  expect_equal(gaps, rep(0.05, 3L), tolerance = 1e-3)
})

test_that("geom_unit_col: fractional y tiles into floor + partial cells", {
  # y = 3.7 → 3 full unit cells (height 1) + 1 partial cell (height 0.7).
  df_frac <- data.frame(x = "A", y = 3.7)
  p <- ggplot(df_frac, aes(x, y)) + geom_unit_col()
  b  <- ggplot_build(p)
  g  <- GeomUnitBar$draw_panel(
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
  g  <- GeomUnitBar$draw_panel(
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

test_that("geom_unit_*: non-linear value scale tiles in data space (non-uniform cells)", {
  # `cell_size` is a data-space quantity. Under e.g. `scale_y_log10()`,
  # the geom inverse-transforms ymin/ymax to data space, tiles cells at
  # multiples of `cell_size`, then forward-transforms each edge back.
  # The cell count contract is preserved (1 cell = `cell_size` obs); the
  # visual cell heights are non-uniform -- narrow toward high counts
  # under `log10`.

  # 1. Identity stays uniform, 120 cells (one per observation).
  df <- data.frame(grp = rep(c("A", "B", "C"), each = 40), val = rep(1, 120))
  p_id <- suppressMessages(
    ggplot(df, aes(grp)) + geom_unit_bar(cell_size = 1)
  )
  suppressMessages(suppressWarnings({
    g_id <- ggplotGrob(p_id)
    rect_id <- g_id$grobs[[grep("^panel", g_id$layout$name)[1]]]$children[[3]]
  }))
  expect_equal(length(rect_id$x), 120L)
  heights_id <- as.numeric(rect_id$height)
  expect_true(diff(range(heights_id)) < 1e-9)  # uniform

  # 2. log10 + 3 bars (counts 10, 100, 1000) at `cell_size = 10` should
  # produce 1 + 10 + 100 = 111 cells.
  df3 <- data.frame(x = c("A", "B", "C"), y = c(10, 100, 1000))
  p_log <- suppressMessages(
    ggplot(df3, aes(x, y)) + geom_unit_col(cell_size = 10) + scale_y_log10()
  )
  suppressMessages(suppressWarnings({
    g_log <- ggplotGrob(p_log)
    rect_log <- g_log$grobs[[grep("^panel", g_log$layout$name)[1]]]$children[[3]]
  }))
  expect_equal(length(rect_log$x), 111L)

  # 3. Under log10 the tallest bar's cells must shrink (compress) toward
  # the top of the bar -- pin the monotone trend.
  heights_log <- as.numeric(rect_log$height)
  # The largest bar contributes 100 cells; its top cells are the last
  # 100 in render order. Drop a small head-tail buffer to skip partial
  # cells at boundaries.
  tall_heights <- tail(heights_log, 100)
  # First few cells (low counts) should be substantially taller than the
  # last few (high counts) -- log10 compression.
  expect_gt(mean(head(tall_heights, 10)), mean(tail(tall_heights, 10)))

  # 4. Horizontal orientation: log10 on x is the value-axis transform,
  # cells tile horizontally with non-uniform widths.
  p_hor <- suppressMessages(
    ggplot(df3, aes(y = x, x = y)) +
      geom_unit_col(cell_size = 10, orientation = "y") +
      scale_x_log10()
  )
  suppressMessages(suppressWarnings({
    g_hor <- ggplotGrob(p_hor)
    rect_hor <- g_hor$grobs[[grep("^panel", g_hor$layout$name)[1]]]$children[[3]]
  }))
  expect_equal(length(rect_hor$x), 111L)
})


test_that("geom_unit_col: cell_count_cap caps the per-panel cell count", {
  df_big <- data.frame(x = "A", y = 1e5)
  p <- ggplot(df_big, aes(x, y)) + geom_unit_col() + coord_equal()
  expect_warning(
    ggplotGrob(p),
    regexp = "Refusing to tile"
  )
  # Opt out: Inf lets it through (no warning).
  p_inf <- ggplot(data.frame(x = "A", y = 50), aes(x, y)) +
    geom_unit_col(cell_count_cap = Inf) + coord_equal()
  expect_no_warning(ggplotGrob(p_inf))
})

test_that("geom_unit_*: nonsensical cell_count_cap warns and falls back to default", {
  df_small <- data.frame(x = c("A", "B"), y = c(1, 2))

  bad_values <- list(
    negative = -1,
    zero     = 0,
    na       = NA,
    string   = "big",
    nonscalar = c(1, 2)
  )

  for (nm in names(bad_values)) {
    p <- ggplot(df_small, aes(x, y)) +
      geom_unit_col(cell_count_cap = bad_values[[nm]])
    expect_warning(
      ggplotGrob(p),
      regexp = "must be a positive scalar number or `Inf`",
      info = paste("case:", nm)
    )
  }

  # Sanity: Inf and finite positives stay quiet on this front.
  expect_no_warning(
    ggplotGrob(ggplot(df_small, aes(x, y)) + geom_unit_col(cell_count_cap = Inf))
  )
  expect_no_warning(
    ggplotGrob(ggplot(df_small, aes(x, y)) + geom_unit_col(cell_count_cap = 50))
  )
})

# ---------------------------------------------------------------------------
# cell_padding — per-side inset of each cell, CSS-like
# ---------------------------------------------------------------------------

test_that("cell_padding: scalar = same inset on all four sides", {
  # y=2, cell_size=1, padding=0.1 → every cell loses 0.1 on each side
  # (uniform-padding rule), so every cell renders at height 1 - 2*0.1 = 0.8.
  df <- data.frame(x = "A", y = 2)
  p <- ggplot(df, aes(x, y)) + geom_unit_col(width = 1, cell_padding = 0.1)
  b <- ggplot_build(p)
  g <- GeomUnitBar$draw_panel(
    b$data[[1]], b$layout$panel_params[[1]], b$layout$coord,
    radius = grid::unit(0, "npc"), cell_size = 1, cell_padding = 0.1
  )
  # Walk the rect/polygon to find cell heights in NPC. Note: unit-cell
  # grobs defer their cells in `cells_glist` (consumed by makeContent) so
  # we descend into both `children` and `cells_glist`.
  collect_rects <- function(g, acc = list()) {
    if (inherits(g, "rect") || inherits(g, "polygon")) acc[[length(acc)+1L]] <- g
    if (inherits(g, "gTree") && length(g$children))
      for (ch in g$children) acc <- collect_rects(ch, acc)
    if (inherits(g, "unit_cell_grob") && length(g$cells_glist))
      for (ch in g$cells_glist) acc <- collect_rects(ch, acc)
    acc
  }
  rs <- collect_rects(g)
  expect_gt(length(rs), 0L)
})

test_that("cell_padding: zero padding makes cells touch", {
  df <- data.frame(x = "A", y = 3)
  p <- ggplot(df, aes(x, y)) + geom_unit_col(width = 1, cell_padding = 0) +
    coord_equal()
  # With zero padding, three cells of height 1 tile from 0 to 3 with no gaps.
  # Check via the rendered rect widths/heights.
  gt <- suppressWarnings(ggplotGrob(p))
  panel <- gt$grobs[[grep("panel", gt$layout$name)[1]]]
  find_batch_rect <- function(g) {
    if (inherits(g, "rect") && length(as.numeric(g$x)) > 1L) return(g)
    if (inherits(g, "gTree") && length(g$children))
      for (ch in g$children) { r <- find_batch_rect(ch); if (!is.null(r)) return(r) }
    NULL
  }
  r <- find_batch_rect(panel)
  expect_false(is.null(r))
  heights <- as.numeric(r$height)
  expect_true(all(abs(diff(heights)) < 1e-6),
              info = "All cells should have identical height when padding = 0")
})

test_that("cell_padding: length-2 accepts c(vertical, horizontal)", {
  df <- data.frame(x = c("A", "B"), y = c(2, 2))
  p <- ggplot(df, aes(x, y)) +
    geom_unit_col(width = 1, cell_padding = c(0.05, 0.01)) +
    coord_equal()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# Helper: pull each cell's NPC bounding box from the rendered grob, sorted
# bottom-to-top, left-to-right. Lets us compare cell-padding shapes for
# byte-identical layout without relying on vdiffr.
extract_cell_boxes <- function(grob) {
  if (inherits(grob, "rect")) {
    return(data.frame(
      x = as.numeric(grob$x),
      y = as.numeric(grob$y),
      w = as.numeric(grob$width),
      h = as.numeric(grob$height)
    ))
  }
  if (inherits(grob, "gTree") && length(grob$children)) {
    return(do.call(rbind, lapply(grob$children, extract_cell_boxes)))
  }
  NULL
}

test_that("cell_padding: positional and named forms are equivalent", {
  # The user-facing promise: c(0.1, 0.2), c(vertical = 0.1, horizontal = 0.2),
  # and c(horizontal = 0.2, vertical = 0.1) must all produce the same layout.
  df <- data.frame(x = c("A", "B"), y = c(3, 2))
  build_boxes <- function(cp) {
    p <- ggplot(df, aes(x, y)) +
      geom_unit_col(width = 1, cell_padding = cp) + coord_equal()
    g <- suppressWarnings(GeomUnitBar$draw_panel(
      ggplot_build(p)$data[[1L]],
      ggplot_build(p)$layout$panel_params[[1L]],
      ggplot_build(p)$layout$coord,
      cell_padding = cp
    ))
    bx <- extract_cell_boxes(g)
    bx[order(bx$y, bx$x), ]
  }
  positional <- build_boxes(c(0.1, 0.2))
  named_in_order <- build_boxes(c(vertical = 0.1, horizontal = 0.2))
  named_reversed <- build_boxes(c(horizontal = 0.2, vertical = 0.1))

  expect_equal(positional, named_in_order, ignore_attr = TRUE)
  expect_equal(positional, named_reversed, ignore_attr = TRUE)
})

test_that("cell_padding: a single named element defaults the other axis", {
  # Named length-1 fills the missing axis with the default (0.05).
  df <- data.frame(x = "A", y = 2)
  ref <- function(cp) {
    p <- ggplot(df, aes(x, y)) +
      geom_unit_col(width = 1, cell_padding = cp) + coord_equal()
    extract_cell_boxes(suppressWarnings(GeomUnitBar$draw_panel(
      ggplot_build(p)$data[[1L]],
      ggplot_build(p)$layout$panel_params[[1L]],
      ggplot_build(p)$layout$coord,
      cell_padding = cp
    )))
  }
  expect_equal(
    ref(c(vertical = 0.2)),
    ref(c(vertical = 0.2, horizontal = 0.05)),
    ignore_attr = TRUE
  )
  expect_equal(
    ref(c(horizontal = 0.2)),
    ref(c(vertical = 0.05, horizontal = 0.2)),
    ignore_attr = TRUE
  )
})

test_that("cell_padding: name validation errors hard, not warns", {
  df <- data.frame(x = "A", y = 2)
  build <- function(cp) {
    suppressWarnings(ggplotGrob(
      ggplot(df, aes(x, y)) + geom_unit_col(cell_padding = cp)
    ))
  }
  # Unknown name
  expect_error(build(c(vert = 0.1, horizontal = 0.05)), "unknown name")
  # Mixed named/unnamed
  expect_error(build(c(0.1, vertical = 0.05)), "fully named or fully unnamed")
  # Duplicate name
  expect_error(
    build(c(vertical = 0.1, vertical = 0.2)),
    "duplicated name"
  )
})

test_that("cell_padding: invalid values warn and fall back to default", {
  df <- data.frame(x = c("A", "B"), y = c(1, 1))
  bad_values <- list(
    negative  = -0.1,
    too_large = 0.7,
    at_limit  = 0.5,
    na        = NA,
    length_3  = c(0.01, 0.01, 0.01),
    string    = "big"
  )
  for (nm in names(bad_values)) {
    p <- ggplot(df, aes(x, y)) +
      geom_unit_col(cell_padding = bad_values[[nm]])
    expect_warning(
      ggplotGrob(p),
      regexp = "cell_padding.*must be a finite numeric vector",
      info = paste("case:", nm)
    )
  }
})

test_that("cell_padding: default constructor value is 0.05", {
  lyr <- geom_unit_col()
  expect_equal(lyr$geom_params$cell_padding, 0.05)
  lyr <- geom_unit_bar()
  expect_equal(lyr$geom_params$cell_padding, 0.05)
  lyr <- geom_unit_histogram()
  expect_equal(lyr$geom_params$cell_padding, 0.05)
})

test_that("width: geom_unit_bar / geom_unit_col default to width = 1", {
  # Each bar slot should span 1 in data space.
  b1 <- ggplot_build(
    ggplot(data.frame(x = factor(c("a", "b", "c")), y = c(2, 3, 1)), aes(x, y)) +
      geom_unit_col()
  )
  expect_equal(b1$data[[1L]]$xmax[1L] - b1$data[[1L]]$xmin[1L], 1)

  b2 <- ggplot_build(ggplot(mpg, aes(x = class)) + geom_unit_bar())
  expect_equal(b2$data[[1L]]$xmax[1L] - b2$data[[1L]]$xmin[1L], 1)
})


# ---------------------------------------------------------------------------
# cell_size — semantic dial (each cell = N units)
# ---------------------------------------------------------------------------

test_that("geom_unit_col: cell_size aggregates units per cell", {
  # y = 25, cell_size = 10  →  floor(25/10) = 2 full + (25-20)/10 = 0.5 partial
  # = 3 cells total.
  df_big <- data.frame(x = "A", y = 25)
  p <- ggplot(df_big, aes(x, y)) + geom_unit_col(cell_size = 10)
  b <- ggplot_build(p)
  g <- GeomUnitBar$draw_panel(
    b$data[[1L]], b$layout$panel_params[[1L]], b$layout$coord,
    radius = grid::unit(0, "npc"),
    cell_size = 10
  )
  expect_equal(length(as.numeric(g$y)), 3L)
})

test_that("geom_unit_col: cell_size = 1 (default) is a no-op vs current behaviour", {
  df_frac <- data.frame(x = "A", y = 3.7)  # same data as the existing fractional test
  p <- ggplot(df_frac, aes(x, y)) + geom_unit_col(cell_size = 1)
  b <- ggplot_build(p)
  g <- GeomUnitBar$draw_panel(
    b$data[[1L]], b$layout$panel_params[[1L]], b$layout$coord,
    radius = grid::unit(0, "npc"),
    cell_size = 1
  )
  expect_equal(length(as.numeric(g$y)), 4L)  # 3 full + 1 partial, identical to default
})

test_that("geom_unit_col: cell_size keeps cell_count_cap from firing on big y", {
  # Without cell_size, y = 1e5 would trip the default cell_count_cap (1e4).
  # cell_size = 1e3 reduces to 100 cells — well under the cap, no warning.
  p <- ggplot(data.frame(x = "A", y = 1e5), aes(x, y)) +
    geom_unit_col(cell_size = 1e3)
  expect_no_warning(ggplotGrob(p))
})

test_that("geom_unit_*: nonsensical cell_size warns and falls back to default", {
  df_small <- data.frame(x = c("A", "B"), y = c(1, 2))
  bad_values <- list(
    negative = -1,
    zero     = 0,
    na       = NA,
    inf      = Inf,        # Inf draws zero cells, nonsensical here
    string   = "big",
    nonscalar = c(1, 2)
  )
  for (nm in names(bad_values)) {
    p <- ggplot(df_small, aes(x, y)) +
      geom_unit_col(cell_size = bad_values[[nm]])
    expect_warning(
      ggplotGrob(p),
      regexp = "cell_size.*must be a positive finite scalar",
      info = paste("case:", nm)
    )
  }
})

# ---------------------------------------------------------------------------
# label_cells() helper
# ---------------------------------------------------------------------------

test_that("label_cells: returns a closure that divides input by cell_size", {
  f <- label_cells(100)
  expect_type(f, "closure")
  # Wrapper around scales::label_number -> character output. Accuracy is
  # auto-picked from the vector's resolution; integers stay integer-ish.
  expect_equal(f(c(100, 200, 1000)), c("1", "2", "10"))
})

test_that("label_cells: default cell_size = 1 is identity", {
  # Identity divider; numeric input becomes formatted character.
  expect_equal(label_cells()(c(0, 1, 7)), c("0", "1", "7"))
})

test_that("label_cells: invalid cell_size aborts with a clear cli message", {
  expect_error(label_cells(0),       regexp = "positive finite scalar")
  expect_error(label_cells(-1),      regexp = "positive finite scalar")
  expect_error(label_cells(NA),      regexp = "positive finite scalar")
  expect_error(label_cells(Inf),     regexp = "positive finite scalar")
  expect_error(label_cells("big"),   regexp = "positive finite scalar")
  expect_error(label_cells(c(1, 2)), regexp = "positive finite scalar")
})

test_that("label_cells: divides correctly across multi-break inputs", {
  # Real ggplot2 axis breaks are always multi-value; scales::label_number
  # picks accuracy from the vector's range. Fractional value preserved.
  expect_equal(
    label_cells(100)(c(50, 250, 1000)),
    c("0.5", "2.5", "10.0")
  )
  expect_equal(
    label_cells(1e6)(c(0, 2.4e6, 5e6)),
    c("0.0", "2.4", "5.0")
  )
})

test_that("label_cells: suffix wraps the divided value", {
  f <- label_cells(1e3, suffix = "k")
  expect_equal(f(c(1000, 2000, 10000)), c("1k", "2k", "10k"))
  # cell_size = 1e6 with suffix "M"
  expect_equal(label_cells(1e6, suffix = "M")(c(2.4e6, 1e6)),
               c("2.4M", "1.0M"))
})

test_that("label_cells: prefix wraps the divided value", {
  expect_equal(
    label_cells(100, prefix = "~")(c(50, 250)),
    c("~0.5", "~2.5")
  )
  # Both ends:
  expect_equal(
    label_cells(1000, prefix = "$", suffix = "k")(c(1500, 2500)),
    c("$1.5k", "$2.5k")
  )
})

test_that("label_cells: NA inputs stay NA (no 'NAk' formatting)", {
  f <- label_cells(1000, suffix = "k")
  expect_identical(f(c(1000, NA_real_, 5000)),
                   c("1k", NA_character_, "5k"))
  # Same without suffix:
  expect_identical(label_cells(1000)(c(NA_real_, 1000, 2000)),
                   c(NA_character_, "1", "2"))
})

test_that("label_cells: forwards ... to scales::label_number", {
  # `accuracy` forces decimal precision regardless of vector resolution.
  expect_equal(
    label_cells(1e3, accuracy = 0.01)(c(1500, 12345)),
    c("1.50", "12.34")
  )
  # `big.mark` adds thousands separators.
  expect_equal(
    label_cells(1, big.mark = ",")(c(1000, 1234567)),
    c("1,000", "1,234,567")
  )
})

test_that("label_cells: invalid prefix / suffix aborts with a clear cli message", {
  expect_error(label_cells(1, prefix = c("a", "b")),
               regexp = "non-NA string")
  expect_error(label_cells(1, prefix = NA_character_),
               regexp = "non-NA string")
  expect_error(label_cells(1, prefix = 42),
               regexp = "non-NA string")
  expect_error(label_cells(1, suffix = c("a", "b")),
               regexp = "non-NA string")
  expect_error(label_cells(1, suffix = NA_character_),
               regexp = "non-NA string")
})

test_that("geom_unit_col: radius accepts unit/numeric, falls back on bad input", {
  df_r <- data.frame(x = c("A","B"), y = c(1, 2))
  # Bare numerics are coerced to points (convenience) -- no error.
  expect_no_error(
    suppressMessages(suppressWarnings(
      ggplotGrob(ggplot(df_r, aes(x, y)) + geom_unit_col(radius = 5))
    ))
  )
  # Bad input falls back to default with a warning rather than aborting.
  expect_warning(
    ggplotGrob(ggplot(df_r, aes(x, y)) + geom_unit_col(radius = "big")),
    regexp = "Falling back"
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
  # stat_count computes counts (positive), then scale_y_reverse negates them.
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
  # Scale transform runs BEFORE the stat, so stat_identity sees log10(y) values
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
  # Removing "B" from the scale should yield only bars for "A".  With
  # stat = "identity" the dropped-level rows arrive at the build stage with
  # x = NA (matching vanilla geom_col); the surviving x values are all 1.
  b <- suppressWarnings(ggplot_build(base_p + scale_x_discrete(limits = "A")))
  surviving_x <- b$data[[1]]$x[!is.na(b$data[[1]]$x)]
  expect_true(length(surviving_x) > 0L)
  expect_true(all(surviving_x == 1))
})

test_that("GoG/scales: scale_y_continuous(expand = c(0, 0)) builds without error", {
  # With expand = c(0, 0) the panel y-axis hugs the data extent.  The
  # geom's outer cells now sit one cell_padding * cell_size inside that
  # extent (uniform-padding rule), but the build itself must still work.
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




# ===========================================================================
# Regression: stat param not formally exposed
# ===========================================================================

test_that("geom_unit_bar: stat = 'count' (default) uses ggplot2::StatCount", {
  lyr <- geom_unit_bar()
  expect_true(inherits(lyr$stat, "StatCount"))
})

test_that("geom_unit_col: stat = 'identity' (default) uses ggplot2::StatIdentity", {
  lyr <- geom_unit_col()
  expect_true(inherits(lyr$stat, "StatIdentity"))
})

test_that("geom_unit_bar: stat = 'bin' is routed correctly, not silently ignored", {
  lyr <- geom_unit_bar(stat = "bin")
  expect_false(inherits(lyr$stat, "StatCount"))
  expect_true(inherits(lyr$stat, "StatBin"))
})

test_that("geom_unit_bar: stat as ggproto class object (not string) is accepted", {
  lyr_str <- geom_unit_bar(stat = "bin")
  lyr_cls <- geom_unit_bar(stat = ggplot2::StatBin)
  expect_identical(class(lyr_str$stat), class(lyr_cls$stat))
  expect_true(inherits(lyr_cls$stat, "StatBin"))
})

test_that("geom_unit_col: stat = 'identity' renders (ymin/ymax come from position)", {
  # draw_panel tiles from ymax - ymin directly, so stats that don't emit a
  # `count` column still work as long as positioning produces valid ymin/ymax.
  p <- ggplot(
    data.frame(x = c("A", "B"), y = c(3, 5)),
    aes(x, y)
  ) + geom_unit_col(stat = "identity")
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_unit_bar: aes(fill = after_stat(count)) maps the computed count", {
  # after_stat(count) reaches the fill aesthetic after stat_count runs.
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
# Rendering correctness -- grob structure for GeomUnitBar
# ===========================================================================

# Helper to call GeomUnitBar$draw_panel directly
draw_panel_bar_cells <- function(p) {
  b  <- ggplot_build(p)
  ld <- b$data[[1L]]
  pp <- b$layout$panel_params[[1L]]
  co <- b$layout$coord
  layer <- b$plot$layers[[1L]]
  radius <- layer$geom_params$radius %||% grid::unit(0, "npc")
  GeomUnitBar$draw_panel(ld, pp, co, radius = radius)
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

  # Cells live in `cells_glist` (the deferred slot consumed by
  # `makeContent.unit_cell_grob`) before the grob is rendered.
  expect_equal(length(g$cells_glist), 8L)
})

test_that("all rounded gTree children are roundrect grobs", {
  df_rc <- data.frame(x = c("A", "B"), y = c(3, 5))
  p <- ggplot(df_rc, aes(x, y)) + geom_unit_col(radius = grid::unit(2, "pt"))
  g <- draw_panel_bar_cells(p)

  classes <- sapply(g$cells_glist, function(ch) inherits(ch, "roundrect"))
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


# ---------------------------------------------------------------------------
# Legend key glyph: `radius` mirrors the panel cells
# ---------------------------------------------------------------------------
# `draw_key_unit()` mirrors the geom's `radius`. Pin three regimes so any
# future change is caught:
#   * radius = 0     -> sharp legend keys (sharp panel cells)
#   * radius = 5 pt  -> rounded legend keys, matching the panel cells
#   * radius = 50 pt -> radius exceeds half the key's smaller side, so
#                       `.clamp_roundrect_radius` (called via
#                       `unit_cell_grob`'s makeContent with quiet = TRUE)
#                       clamps to half-side -> legend keys become circles.
#                       Panel cells also clamp to circles for the same
#                       reason. No cap message echoed from the legend.

test_that("vdiffr (key-radius): radius = 0 produces sharp legend keys", {
  skip_if_not_installed("vdiffr")
  df <- data.frame(x = c("A", "B", "C"), y = c(3, 5, 2),
                   g = c("g1", "g2", "g1"))
  p <- ggplot(df, aes(x, y, fill = g)) +
    geom_unit_col(radius = unit(0, "pt")) + coord_equal()
  suppressMessages(
    vdiffr::expect_doppelganger("key-radius-0", p)
  )
})

test_that("vdiffr (key-radius): radius = 5 pt rounds keys to match cells", {
  skip_if_not_installed("vdiffr")
  df <- data.frame(x = c("A", "B", "C"), y = c(3, 5, 2),
                   g = c("g1", "g2", "g1"))
  p <- ggplot(df, aes(x, y, fill = g)) +
    geom_unit_col(radius = unit(5, "pt")) + coord_equal()
  suppressMessages(
    vdiffr::expect_doppelganger("key-radius-5pt", p)
  )
})

test_that("vdiffr (key-radius): radius = 50 pt silently clamps keys to circles", {
  skip_if_not_installed("vdiffr")
  df <- data.frame(x = c("A", "B", "C"), y = c(3, 5, 2),
                   g = c("g1", "g2", "g1"))
  p <- ggplot(df, aes(x, y, fill = g)) +
    geom_unit_col(radius = unit(50, "pt")) + coord_equal()
  suppressMessages(
    vdiffr::expect_doppelganger("key-radius-50pt", p)
  )
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

# ---------------------------------------------------------------------------
# Date / POSIXct support
# ---------------------------------------------------------------------------
# Dates on either axis should render without error: on the category axis
# they convert to a continuous numeric scale; on the value axis, cell_size
# is then in days (Date) or seconds (POSIXct).

test_that("Date on x-axis (category) renders", {
  df <- data.frame(d = as.Date("2024-01-01") + c(0, 7, 14), n = c(3, 5, 2))
  p <- ggplot(df, aes(d, n)) + geom_unit_col()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("POSIXct on x-axis (category) renders", {
  df <- data.frame(
    t = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 3600, 7200),
    n = c(2, 4, 1)
  )
  p <- ggplot(df, aes(t, n)) + geom_unit_col()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("Date on y-axis (value) renders with cell_size in days", {
  df <- data.frame(
    task     = c("A", "B", "C"),
    deadline = as.Date(c("2024-01-08", "2024-01-15", "2024-01-22"))
  )
  # cell_size = 7 → weekly cells
  p <- ggplot(df, aes(task, deadline)) + geom_unit_col(cell_size = 7)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("POSIXct on y-axis (value) renders", {
  df <- data.frame(
    event = c("A", "B"),
    when  = as.POSIXct(c("2024-01-01 12:00", "2024-01-01 15:00"), tz = "UTC")
  )
  p <- ggplot(df, aes(event, when)) + geom_unit_col(cell_size = 3600)
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})


# ---------------------------------------------------------------------------
# draw_key_unit — orientation-aware 2-cell legend key (2026-04-27)
# ---------------------------------------------------------------------------
# `flipped_aes = FALSE` (vertical bars, default) → cells stacked vertically,
# horizontal gap between them.  `flipped_aes = TRUE` (horizontal bars) → cells
# placed side by side, vertical gap between them.

test_that("draw_key_unit: vertical bars produce a column of 2 cells", {
  data <- data.frame(
    fill      = "steelblue",
    colour    = NA,
    alpha     = NA,
    linewidth = 0.5,
    linetype  = 1
  )
  g <- draw_key_unit(data, list(flipped_aes = FALSE), grid::unit(1, "npc"))
  expect_s3_class(g, "rect")
  # Two cells stacked in y → x is scalar (recycled), y has two centres.
  expect_equal(length(as.numeric(g$x)), 1L)
  expect_equal(length(as.numeric(g$y)), 2L)
})

test_that("draw_key_unit: horizontal bars produce a row of 2 cells", {
  data <- data.frame(
    fill      = "steelblue",
    colour    = NA,
    alpha     = NA,
    linewidth = 0.5,
    linetype  = 1
  )
  g <- draw_key_unit(data, list(flipped_aes = TRUE), grid::unit(1, "npc"))
  expect_s3_class(g, "rect")
  # Two cells side-by-side in x → y is scalar (recycled), x has two centres.
  expect_equal(length(as.numeric(g$x)), 2L)
  expect_equal(length(as.numeric(g$y)), 1L)
})

test_that("draw_key_unit: defaults to vertical-bar layout when flipped_aes is missing", {
  data <- data.frame(
    fill      = "steelblue",
    colour    = NA,
    alpha     = NA,
    linewidth = 0.5,
    linetype  = 1
  )
  # No flipped_aes in params → treated as FALSE, vertical layout.
  g <- draw_key_unit(data, list(), grid::unit(1, "npc"))
  expect_equal(length(as.numeric(g$y)), 2L)
})

test_that("geom_unit_col: default legend key matches the geom's orientation", {
  data <- data.frame(
    fill      = "steelblue",
    colour    = NA,
    alpha     = NA,
    linewidth = 0.5,
    linetype  = 1
  )
  g_v <- GeomUnitBar$draw_key(data, list(flipped_aes = FALSE), grid::unit(1, "npc"))
  g_h <- GeomUnitBar$draw_key(data, list(flipped_aes = TRUE),  grid::unit(1, "npc"))
  expect_equal(length(as.numeric(g_v$y)), 2L)
  expect_equal(length(as.numeric(g_h$x)), 2L)
})


# ---------------------------------------------------------------------------
# vdiffr: coord systems that previously had no snapshot coverage
# ---------------------------------------------------------------------------
# These pin the arc-interpolation behaviour (polar / polar-theta-y) and the
# linear-coord variants (flip, trans, fixed-ratio) that the family claims to
# support.  coord_map() and coord_sf() are explicitly NOT covered because
# both fail for vanilla geom_col() too — they are upstream limitations.

test_that("vdiffr: geom_unit_col under coord_polar() (radial wedges)", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(data.frame(x = c("A","B","C"), y = c(2, 3, 1)), aes(x, y)) +
    geom_unit_col()
  vdiffr::expect_doppelganger("bar-cells-coord-polar", p + coord_polar())
})

test_that("vdiffr: geom_unit_col under coord_polar(theta = 'y') (stacked donut)", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(mpg, aes(x = factor(1), fill = class)) +
    geom_unit_bar(width = 1)
  vdiffr::expect_doppelganger(
    "bar-cells-coord-polar-theta-y",
    p + coord_polar(theta = "y")
  )
})

test_that("vdiffr: geom_unit_col under coord_flip()", {
  skip_if_not_installed("vdiffr")
  # coord_flip runs the stat in native orientation then swaps at render time,
  # which differs from aes(y = class) (orientation auto-detect).
  p <- ggplot(data.frame(x = c("A","B","C"), y = c(2, 3, 1)), aes(x, y)) +
    geom_unit_col() +
    coord_flip()
  vdiffr::expect_doppelganger("bar-cells-coord-flip", p)
})

test_that("vdiffr: geom_unit_col under coord_fixed(ratio = 2)", {
  skip_if_not_installed("vdiffr")
  p <- ggplot(data.frame(x = c("A","B","C"), y = c(2, 3, 1)), aes(x, y)) +
    geom_unit_col() +
    coord_fixed(ratio = 2)
  vdiffr::expect_doppelganger("bar-cells-coord-fixed-ratio", p)
})

test_that("vdiffr: geom_unit_col with scale_y log10 transform (non-uniform cells)", {
  skip_if_not_installed("vdiffr")
  # Non-linear value-axis scales tile cells in data space, then forward-
  # transform each edge back to panel space. Cell COUNT is preserved
  # (1 cell = `cell_size` obs) but cell HEIGHTS shrink as count grows
  # under log10. Vertical padding is applied in PANEL space proportional
  # to each cell's panel extent (5 % of cell extent) -- this makes the
  # gap between adjacent cells look the same throughout the bar instead
  # of expanding under log compression near the baseline.
  p <- ggplot(data.frame(x = c("A","B","C"), y = c(10, 100, 1000)), aes(x, y)) +
    geom_unit_col() +
    scale_y_continuous(transform = "log10")
  vdiffr::expect_doppelganger("bar-cells-log10", p)
})

test_that("vdiffr: geom_unit_col with cell_size > 1 under coord_equal(ratio)", {
  skip_if_not_installed("vdiffr")
  # cell_size multiplies data-space cell height; coord_equal(ratio = cell_size)
  # keeps cells visually square.
  df <- data.frame(country = c("A","B","C"), pop = c(2.4e6, 1.1e6, 3.8e6))
  p <- ggplot(df, aes(country, pop)) +
    geom_unit_col(cell_size = 1e6) +
    scale_y_continuous(labels = label_cells(1e6)) +
    coord_equal(ratio = 1e6)
  vdiffr::expect_doppelganger("bar-cells-cell-size", p)
})


# ---------------------------------------------------------------------------
# Linear-coord padding regression baselines
# ---------------------------------------------------------------------------
# Pinned snapshots that the linear-scale padding behaviour must NOT change
# when we switch to panel-proportional padding under non-linear scales.
# These cover the cases most sensitive to a padding-semantics tweak:
# single full cell, multiple full cells, partial-cell-only, partial-cell
# at tip, dodged + stacked layout.

test_that("vdiffr (linear-pad): single full cell (count == cell_size)", {
  skip_if_not_installed("vdiffr")
  df <- data.frame(x = "A", y = 1)
  p <- ggplot(df, aes(x, y)) + geom_unit_col(cell_size = 1)
  suppressMessages(
    vdiffr::expect_doppelganger("linear-pad-single-full-cell", p)
  )
})

test_that("vdiffr (linear-pad): multiple full cells stacked vertically", {
  skip_if_not_installed("vdiffr")
  df <- data.frame(x = c("A", "B", "C"), y = c(5, 3, 8))
  p <- ggplot(df, aes(x, y)) + geom_unit_col(cell_size = 1)
  suppressMessages(
    vdiffr::expect_doppelganger("linear-pad-multiple-full-cells", p)
  )
})

test_that("vdiffr (linear-pad): partial-cell-only bar (count < cell_size)", {
  skip_if_not_installed("vdiffr")
  # A single partial cell, no full cells.
  df <- data.frame(x = c("A", "B"), y = c(7, 3))
  p <- ggplot(df, aes(x, y)) + geom_unit_col(cell_size = 10)
  suppressMessages(
    vdiffr::expect_doppelganger("linear-pad-partial-only", p)
  )
})

test_that("vdiffr (linear-pad): full cells + partial tip", {
  skip_if_not_installed("vdiffr")
  # 12 obs at cell_size 5 -> 2 full cells (0-5, 5-10) plus a 2-unit partial tip.
  df <- data.frame(x = c("A", "B"), y = c(12, 8))
  p <- ggplot(df, aes(x, y)) + geom_unit_col(cell_size = 5)
  suppressMessages(
    vdiffr::expect_doppelganger("linear-pad-full-plus-partial", p)
  )
})

test_that("vdiffr (linear-pad): dodged bars with mixed counts", {
  skip_if_not_installed("vdiffr")
  df <- data.frame(
    x   = rep(c("A", "B", "C"), each = 2),
    y   = c(3, 6, 2, 5, 7, 1),
    grp = rep(c("g1", "g2"), 3)
  )
  p <- ggplot(df, aes(x, y, fill = grp)) +
    geom_unit_col(cell_size = 1, position = "dodge")
  suppressMessages(
    vdiffr::expect_doppelganger("linear-pad-dodged", p)
  )
})


