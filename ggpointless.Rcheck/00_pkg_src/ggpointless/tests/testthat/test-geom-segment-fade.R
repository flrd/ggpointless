# Safety-net tests for geom_segment_fade.
#
# These tests capture the current (pre-Family-A-refactor) behaviour.  Any
# behavioural drift from the upcoming migration to the path_fade architecture
# should show up here — visually via vdiffr and structurally via the unit
# tests.  Tests intentionally pin down API contracts (validation, fallbacks)
# as well as rendering (snapshots).

library(ggplot2)

df_seg <- data.frame(
  x1 = c(1, 3, 5),
  y1 = c(1, 2, 1),
  x2 = c(2, 4, 6),
  y2 = c(3, 1, 3),
  g  = c("a", "b", "c")
)

df_one <- data.frame(x1 = 1, x2 = 9, y1 = 1, y2 = 1)

p_void <- function() ggplot() + theme_void()

# ---------------------------------------------------------------------------
# Parameter validation
# ---------------------------------------------------------------------------

test_that("alpha_fade_to must be a finite scalar in [0, 1]", {
  p <- ggplot(df_one, aes(x = x1, y = y1, xend = x2, yend = y2))
  expect_error(ggplotGrob(p + geom_segment_fade(alpha_fade_to = -0.1)),      "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_segment_fade(alpha_fade_to =  1.1)),      "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_segment_fade(alpha_fade_to = NA_real_)),  "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_segment_fade(alpha_fade_to = Inf)),       "alpha_fade_to")
  expect_error(ggplotGrob(p + geom_segment_fade(alpha_fade_to = c(0, 0.5))), "alpha_fade_to")
})

test_that("alpha_fade_to boundary values 0 and 1 are accepted", {
  p <- ggplot(df_one, aes(x = x1, y = y1, xend = x2, yend = y2))
  expect_no_error(ggplotGrob(p + geom_segment_fade(alpha_fade_to = 0)))
  expect_no_error(ggplotGrob(p + geom_segment_fade(alpha_fade_to = 1)))
})

test_that("fade_direction warns on invalid values and falls back to 'start'", {
  p <- ggplot(df_one, aes(x = x1, y = y1, xend = x2, yend = y2))
  expect_warning(
    ggplotGrob(p + geom_segment_fade(fade_direction = "middle")),
    "fade_direction"
  )
  # Only invalid values → fallback to "start"; must not error overall.
  expect_no_error(suppressWarnings(
    ggplotGrob(p + geom_segment_fade(fade_direction = c("nope", "middle")))
  ))
})

test_that("fade_direction accepts 'start', 'end', and both", {
  p <- ggplot(df_one, aes(x = x1, y = y1, xend = x2, yend = y2))
  for (fd in list("start", "end", c("start", "end"))) {
    expect_no_error(ggplotGrob(p + geom_segment_fade(fade_direction = fd)))
  }
})

# ---------------------------------------------------------------------------
# Edge cases: empty data / NA removal / non-linear coord fallback
# ---------------------------------------------------------------------------

test_that("empty data returns a zeroGrob (no error, no fade grob in tree)", {
  empty <- df_seg[0, ]
  p <- ggplot(empty, aes(x = x1, y = y1, xend = x2, yend = y2)) +
    geom_segment_fade()
  expect_no_error(ggplotGrob(p))
})

test_that("NA in x/y/xend/yend is removed silently when na.rm = TRUE", {
  df <- df_seg
  df$x2[2] <- NA_real_
  p <- ggplot(df, aes(x = x1, y = y1, xend = x2, yend = y2)) +
    geom_segment_fade(na.rm = TRUE)
  expect_no_error(ggplotGrob(p))
})

test_that("NA in x/y/xend/yend warns when na.rm = FALSE (default)", {
  df <- df_seg
  df$x2[2] <- NA_real_
  p <- ggplot(df, aes(x = x1, y = y1, xend = x2, yend = y2)) +
    geom_segment_fade()
  expect_warning(ggplotGrob(p), "missing values")
})

test_that("non-linear coord (coord_polar) renders without error (fades chord)", {
  # As of the polar refactor, segment_fade no longer falls back on non-linear
  # coords — path_fade delegation handles coord$transform directly, so the
  # user-supplied (x,y)→(xend,yend) chord fades in device space. This test
  # just asserts the plot builds; visual correctness is covered by vdiffr.
  p <- ggplot(df_one, aes(x = x1, y = y1, xend = x2, yend = y2)) +
    geom_segment_fade() +
    coord_polar()
  expect_no_error(suppressMessages(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Uniform alpha early-exit  (mirrors the path_fade test at
# test-doppelganger-fade.R:611)
# ---------------------------------------------------------------------------

find_segment_fade_grob <- function(p) {
  result <- NULL
  walk <- function(node) {
    if (is.null(node) || !is.null(result)) return()
    if (inherits(node, "segment_fade_grob")) {
      result <<- node
      return()
    }
    for (ch in c(node$children, node$grobs)) walk(ch)
  }
  walk(ggplotGrob(p))
  result
}

test_that("alpha == alpha_fade_to takes the early exit (no segment_fade_grob in tree)", {
  # When the effective alpha is uniform, draw_panel delegates to GeomSegment.
  # Post-refactor this contract may be re-expressed (no segment_fade_grob
  # class may exist), but the visual result must still match geom_segment —
  # captured by the vdiffr snapshot further down.
  for (fd in list("start", "end", c("start", "end"))) {
    p <- ggplot(df_one, aes(x = x1, y = y1, xend = x2, yend = y2)) +
      geom_segment_fade(
        linewidth = 10, alpha = 0.5,
        alpha_fade_to = 0.5, fade_direction = fd
      )
    expect_no_error(ggplotGrob(p))
    expect_null(find_segment_fade_grob(p))
  }
})

# ---------------------------------------------------------------------------
# Visual regression — vdiffr snapshots
#
# These capture the current rendering behaviour so the Family A refactor
# can be validated pixel-by-pixel.  Any intended visual change must be
# reviewed and re-accepted explicitly via vdiffr::manage_cases().
# ---------------------------------------------------------------------------

test_that("segment_fade visual: fade_direction = 'start' (default)", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "segment-fade-start",
    p_void() +
      geom_segment_fade(
        aes(x = x1, y = y1, xend = x2, yend = y2),
        data = df_one,
        fade_direction = "start",
        linewidth = 10,
        colour = "#e63946"
      )
  )
})

test_that("segment_fade visual: fade_direction = 'end'", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "segment-fade-end",
    p_void() +
      geom_segment_fade(
        aes(x = x1, y = y1, xend = x2, yend = y2),
        data = df_one,
        fade_direction = "end",
        linewidth = 10,
        colour = "#e63946"
      )
  )
})

test_that("segment_fade visual: fade_direction = c('start', 'end')", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "segment-fade-both",
    p_void() +
      geom_segment_fade(
        aes(x = x1, y = y1, xend = x2, yend = y2),
        data = df_one,
        fade_direction = c("start", "end"),
        linewidth = 10,
        colour = "#e63946"
      )
  )
})

test_that("segment_fade visual: alpha_fade_to = 0.5 (partial fade)", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "segment-fade-alpha-half",
    p_void() +
      geom_segment_fade(
        aes(x = x1, y = y1, xend = x2, yend = y2),
        data = df_one,
        alpha_fade_to = 0.5,
        linewidth = 10,
        colour = "#e63946"
      )
  )
})

test_that("segment_fade visual: multi-row with mapped colour", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "segment-fade-multi-row",
    ggplot(df_seg, aes(x = x1, y = y1, xend = x2, yend = y2, colour = g)) +
      geom_segment_fade(linewidth = 4) +
      theme_minimal()
  )
})

test_that("segment_fade visual: with arrow and fade_direction = 'end'", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "segment-fade-arrow-end",
    p_void() +
      geom_segment_fade(
        aes(x = x1, y = y1, xend = x2, yend = y2),
        data = df_one,
        fade_direction = "end",
        arrow = arrow(length = unit(0.1, "npc")),
        linewidth = 4,
        colour = "#457b9d"
      )
  )
})

test_that("segment_fade visual: closed arrow with arrow.fill override", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "segment-fade-arrow-fill",
    p_void() +
      geom_segment_fade(
        aes(x = x1, y = y1, xend = x2, yend = y2),
        data = df_one,
        fade_direction = "start",
        arrow = arrow(type = "closed", length = unit(0.08, "npc")),
        arrow.fill = "tomato",
        linewidth = 2,
        colour = "#1d3557"
      )
  )
})

test_that("segment_fade visual: lineend variants", {
  skip_if_not_installed("vdiffr")
  for (le in c("butt", "round", "square")) {
    vdiffr::expect_doppelganger(
      paste0("segment-fade-lineend-", le),
      p_void() +
        geom_segment_fade(
          aes(x = x1, y = y1, xend = x2, yend = y2),
          data = df_one,
          fade_direction = "start",
          lineend = le,
          linewidth = 12,
          colour = "#2a9d8f"
        )
    )
  }
})

test_that("segment_fade visual: uniform alpha matches geom_segment reference", {
  # When alpha == alpha_fade_to the fade degenerates to a flat stroke; the
  # output must be visually indistinguishable from plain geom_segment.
  # This is the visual partner to the "early exit" unit test above.
  skip_if_not_installed("vdiffr")
  base <- p_void() +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 2))

  vdiffr::expect_doppelganger(
    "segment-fade-uniform-reference",
    base + geom_segment(
      aes(x = x1, y = y1, xend = x2, yend = y2),
      data = df_one,
      linewidth = 10, colour = "#e63946", alpha = 0.5
    )
  )
  vdiffr::expect_doppelganger(
    "segment-fade-uniform-matches-geom-segment",
    base + geom_segment_fade(
      aes(x = x1, y = y1, xend = x2, yend = y2),
      data = df_one,
      linewidth = 10, colour = "#e63946", alpha = 0.5,
      alpha_fade_to = 0.5
    )
  )
})
