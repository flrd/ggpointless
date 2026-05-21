library(ggplot2)

dat <- data.frame(x = c(0, 1, 2), y = c(1, 1, 1))

test_that("geom_catenary works", {
  p <- ggplot(dat, aes(x, y)) +
    geom_catenary()
  vdiffr::expect_doppelganger("geom_catenary_default", p)
})

test_that("geom_catenary has a default value for chainLength", {
  p <- ggplot(dat, aes(x, y))
  vdiffr::expect_doppelganger("chainLength default", p + geom_catenary())
})


test_that("user can set a value for chain_length", {
  p <- ggplot(dat, aes(x, y))
  vdiffr::expect_doppelganger("chainLength = 2", p + geom_catenary(chain_length = 4))
})

test_that("straight line is drawn if chain_length is too short", {
  p <- ggplot(dat, aes(x, y))
  vdiffr::expect_doppelganger("chainLength = 3", p + geom_catenary(chain_length = 3))
})

test_that("stat_catenary also works", {
  p <- ggplot(dat[c(1, 2),], aes(x, y))
  vdiffr::expect_doppelganger("stat_catenary", p + stat_catenary())
})

test_that("stat_catenary also works for reversed data", {
  p <- ggplot(dat[c(2, 1),], aes(x, y))
  vdiffr::expect_doppelganger("stat_catenary-rev", p + stat_catenary())
})

# --- chain_length (new non-deprecated name) ----------------------------------

test_that("chain_length controls sag depth", {
  p <- ggplot(dat, aes(x, y)) +
    geom_catenary(chain_length = 4)
  vdiffr::expect_doppelganger("catenary chain_length 4", p)
})

test_that("chain_length shorter than segment distance draws a straight line", {
  p <- ggplot(dat[1:2, ], aes(x, y)) +
    geom_catenary(chain_length = 0.5)   # shorter than dist = 1
  expect_warning(ggplotGrob(p), "shorter than the distance")
})

test_that("chainLength is now defunct and errors", {
  expect_error(
    ggplot(dat[1:2, ], aes(x, y)) + geom_catenary(chainLength = 3),
    "defunct"
  )
})

# --- sag parameter -----------------------------------------------------------

test_that("sag controls vertical drop below lowest endpoint", {
  p <- ggplot(dat, aes(x, y)) +
    geom_catenary(sag = 1)
  vdiffr::expect_doppelganger("catenary sag 1", p)
})

test_that("sag is recycled across segments", {
  df4 <- data.frame(x = 1:4, y = c(1, 1, 0, 2))
  p <- ggplot(df4, aes(x, y)) +
    geom_catenary(sag = c(0.5, NA, NA))
  expect_no_error(ggplotGrob(p))
})

test_that("negative chain_length is rejected", {
  p <- ggplot(dat[1:2, ], aes(x, y)) +
    geom_catenary(chain_length = -1)
  expect_error(ggplotGrob(p), "non-negative")
})

# --- geom_arch / stat_arch ---------------------------------------------------

df_arch <- data.frame(x = seq_len(4), y = c(1, 1, 0, 2))

test_that("geom_arch renders without error", {
  p <- ggplot(df_arch, aes(x, y)) + geom_arch()
  expect_no_error(ggplotGrob(p))
})

test_that("geom_arch default visual", {
  p <- ggplot(df_arch, aes(x, y)) +
    geom_arch() +
    geom_point(size = 3)
  vdiffr::expect_doppelganger("arch default", p)
})

test_that("geom_arch with arch_height", {
  p <- ggplot(df_arch, aes(x, y)) +
    geom_arch(arch_height = 0.5) +
    geom_point(size = 3)
  vdiffr::expect_doppelganger("arch height 0.5", p)
})

test_that("geom_arch with arch_length", {
  p <- ggplot(df_arch, aes(x, y)) +
    geom_arch(arch_length = 5) +
    geom_point(size = 3)
  vdiffr::expect_doppelganger("arch length 5", p)
})

test_that("arch_height and arch_length together: arch_height wins (message may be throttled)", {
  # cli messages with .frequency = "regularly" are throttled across sessions;
  # test for no-error rather than capturing the message.
  p <- ggplot(df_arch[1:2, ], aes(x, y)) +
    geom_arch(arch_height = 0.5, arch_length = 5)
  expect_no_error(suppressMessages(ggplotGrob(p)))
})

test_that("negative arch_height is rejected", {
  p <- ggplot(df_arch[1:2, ], aes(x, y)) +
    geom_arch(arch_height = -1)
  expect_error(ggplotGrob(p), "non-negative")
})

test_that("stat_arch renders without error", {
  p <- ggplot(df_arch[1:2, ], aes(x, y)) + stat_arch()
  expect_no_error(ggplotGrob(p))
})

test_that("geom_arch / stat_arch default to the arch key glyph", {
  # Regression: GeomCatenary's draw_key (a hanging catenary) was leaking into
  # the arch legend because make_constructor() does not forward `key_glyph` to
  # layer(). Fixed by giving GeomArch its own draw_key.
  # body() comparison is brittle under covr (instrumentation injects tracking
  # calls into every function body); skip when covr is the test driver.
  testthat::skip_if(nzchar(Sys.getenv("R_COVR")), "covr instrumentation alters body()")
  for (lyr in list(geom_arch(), stat_arch(geom = "arch"))) {
    expect_s3_class(lyr$geom, "GeomArch")
    fn <- environment(lyr$geom$draw_key)$f
    expect_identical(body(fn), body(draw_key_arch))
  }
})

# --- geom_catenary constructor branch: both chainLength and chain_length ------

test_that("geom_catenary: chainLength always errors, even when chain_length also supplied", {
  expect_error(
    ggplot(dat[1:2, ], aes(x, y)) + geom_catenary(chainLength = 3, chain_length = 5),
    "defunct"
  )
})

# --- solve_a_from_len unit tests ---------------------------------------------

test_that("solve_a_from_len: returns Inf when L < |dy| (impossible geometry)", {
  # L must exceed |dy| for a catenary to exist; discriminant = L^2 - dy^2 < 0 -> Inf
  expect_equal(solve_a_from_len(dx = 1, dy = 5, L = 3), Inf)
})

test_that("solve_a_from_len: returns a finite positive value for valid inputs", {
  result <- solve_a_from_len(dx = 1, dy = 0, L = 2)
  expect_true(is.finite(result))
  expect_gt(result, 0)
})

# --- solve_a_from_lowest_sag unit tests --------------------------------------

test_that("solve_a_from_lowest_sag: returns Inf for non-positive sag", {
  expect_equal(solve_a_from_lowest_sag(dx = 1, dy = 0, S =  0), Inf)
  expect_equal(solve_a_from_lowest_sag(dx = 1, dy = 0, S = -1), Inf)
})

test_that("solve_a_from_lowest_sag: returns finite value and expands bracket when sag is small", {
  # dx >> sag forces the bracketing loop to expand a_up before converging
  result <- solve_a_from_lowest_sag(dx = 10, dy = 0, S = 0.1)
  expect_true(is.finite(result))
  expect_gt(result, 0)
})

# --- compute_catenary_group unit tests ---------------------------------------

.ccg <- function(data, ...) {
  compute_catenary_group(data, n = 5L, chain_length = NULL, sag = NULL,
                         gravity = 1, len_name = "chain_length",
                         sag_name = "sag", ...)
}

test_that("compute_catenary_group: single point returns empty data frame", {
  result <- .ccg(data.frame(x = 1, y = 1))
  expect_equal(nrow(result), 0L)
})

test_that("compute_catenary_group: near-duplicate points trigger warning and are removed", {
  data <- data.frame(x = c(0, 1e-20, 1), y = c(0, 0, 0))
  expect_warning(
    result <- .ccg(data),
    "duplicate"
  )
  # After dedup 2 points remain -> 1 segment -> 5-point output
  expect_equal(nrow(result), 5L)
})

test_that("compute_catenary_group: all-duplicate points returns empty after dedup", {
  data <- data.frame(x = c(0, 1e-20), y = c(0, 0))
  expect_warning(
    result <- .ccg(data),
    "duplicate"
  )
  expect_equal(nrow(result), 0L)
})

test_that("compute_catenary_group: non-numeric chain_length raises an error", {
  data <- data.frame(x = c(0, 1), y = c(0, 0))
  expect_error(
    compute_catenary_group(data, n = 5L, chain_length = "bad", sag = NULL,
                           gravity = 1, len_name = "chain_length", sag_name = "sag"),
    "numeric"
  )
})

test_that("compute_catenary_group: vertical segment (dx = 0) returns interpolated y", {
  data   <- data.frame(x = c(0, 0), y = c(0, 1))
  result <- .ccg(data)
  expect_equal(nrow(result), 5L)
  expect_true(all(result$x == 0))
})

test_that("compute_catenary_group: coincident segment (L2 < tol, L1 >= tol) returns repeated point", {
  # Build a point-pair whose L1 distance exactly equals .cat_tol (passes dedup)
  # but whose L2 distance is tol/sqrt(2) < tol (treated as coincident in segment loop).
  tol  <- sqrt(.Machine$double.eps)
  data <- data.frame(x = c(0, tol / 2, 1), y = c(0, tol / 2, 0))
  result <- .ccg(data)
  # Segment 1 (coincident) -> 5 repeated (0, 0); segment 2 (normal) -> 5 points
  expect_equal(nrow(result), 10L)
  expect_true(all(result$x[1:5] == 0))
})

test_that("compute_catenary_group: sag = 0 returns a straight line (safety net)", {
  data   <- data.frame(x = c(0, 1), y = c(0, 0))
  result <- compute_catenary_group(data, n = 5L, chain_length = NULL, sag = 0,
                                   gravity = 1, len_name = "chain_length", sag_name = "sag")
  # solve_a_from_lowest_sag(S=0) -> Inf -> safety net -> get_linear()
  expect_equal(nrow(result), 5L)
  expect_true(all(result$y == 0))
})

test_that("compute_catenary_group: non-monotonic x is sorted before computing", {
  # Regression test: head(mtcars, 3) has wt = c(2.620, 2.875, 2.320) — the

  # third point has a smaller x than the second.  Without x-sorting, segments
  # overlap in x and GeomLine's x-sort interleaves points from both segments,
  # producing visible zigzag artefacts.
  data <- data.frame(x = c(2.620, 2.875, 2.320), y = c(16.46, 17.02, 18.61))
  result <- .ccg(data)

  # After sorting by x the order becomes (2.320, 2.620, 2.875), giving two
  # segments whose x ranges do not overlap.
  expect_equal(nrow(result), 10L)  # 2 segments × 5 points each

  # x must be monotonically non-decreasing across the entire output
  expect_true(all(diff(result$x) >= 0))
})

test_that("compute_catenary_group: chain_length == straight_dist gives a straight line (tiny sinh)", {
  # When chain_length exactly equals the Euclidean distance, the solver returns
  # a near-infinite alpha and sinh(dx / (2*alpha)) underflows below 1e-9.
  data   <- data.frame(x = c(0, 1), y = c(0, 0))
  result <- compute_catenary_group(data, n = 5L, chain_length = 1, sag = NULL,
                                   gravity = 1, len_name = "chain_length", sag_name = "sag")
  expect_equal(nrow(result), 5L)
  expect_true(all(result$y == 0))
})

# --- too many values for segment parameters -----------------------------------

test_that("compute_catenary_group warns when sag has more values than segments", {
  data <- data.frame(x = 1:4, y = c(1, 1, 0, 2))  # 3 segments
  expect_warning(
    compute_catenary_group(
      data, n = 10L, chain_length = NULL, sag = c(1, 2, 3, 4),
      gravity = 1, len_name = "chain_length", sag_name = "sag"
    ),
    "4 values provided.*sag.*3 segments"
  )
})

test_that("compute_catenary_group warns when chain_length has more values than segments", {
  data <- data.frame(x = 1:4, y = c(1, 1, 0, 2))  # 3 segments
  expect_warning(
    compute_catenary_group(
      data, n = 10L, chain_length = c(5, 5, 5, 5), sag = NULL,
      gravity = 1, len_name = "chain_length", sag_name = "chain_length"
    ),
    "4 values provided.*chain_length.*3 segments"
  )
})

test_that("compute_catenary_group warns when arch_height has more values than segments", {
  data <- data.frame(x = 1:3, y = c(0, 1, 0))  # 2 segments
  expect_warning(
    compute_catenary_group(
      data, n = 10L, chain_length = NULL, sag = c(1, 2, 3),
      gravity = -1, len_name = "arch_length", sag_name = "arch_height"
    ),
    "3 values provided.*arch_height.*2 segments"
  )
})

test_that("compute_catenary_group does not warn when values match or are fewer than segments", {
  data <- data.frame(x = 1:4, y = c(1, 1, 0, 2))  # 3 segments
  # Exact match — no warning
  expect_no_warning(
    compute_catenary_group(
      data, n = 10L, chain_length = NULL, sag = c(1, 2, 3),
      gravity = 1, len_name = "chain_length", sag_name = "sag"
    )
  )
  # Fewer (recycled) — no warning
  expect_no_warning(
    compute_catenary_group(
      data, n = 10L, chain_length = NULL, sag = 1,
      gravity = 1, len_name = "chain_length", sag_name = "sag"
    )
  )
})


# ===========================================================================
# Grammar of Graphics adversarial stress tests
# ===========================================================================

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

test_that("GoG/data: empty dataset does not error", {
  p <- ggplot(data.frame(x = numeric(), y = numeric()), aes(x, y)) +
    geom_catenary()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: single point (no segment) does not error", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_catenary()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: coincident points (zero-length segment) do not error", {
  p <- ggplot(data.frame(x = c(1, 1), y = c(1, 1)), aes(x, y)) +
    geom_catenary()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: all-NA y values do not error", {
  p <- ggplot(data.frame(x = 1:3, y = NA_real_), aes(x, y)) +
    geom_catenary()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: negative y values do not error", {
  p <- ggplot(data.frame(x = c(0, 1, 2), y = c(-5, -3, -5)), aes(x, y)) +
    geom_catenary()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Mapping
# ---------------------------------------------------------------------------

test_that("GoG/mapping: colour aesthetic mapping does not error", {
  df <- data.frame(x = c(0, 1, 2, 3), y = c(1, 1, 2, 2),
                   g = c("a", "a", "b", "b"))
  p <- ggplot(df, aes(x, y, colour = g)) + geom_catenary()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/mapping: inherit.aes = FALSE isolates from plot mapping", {
  df <- data.frame(x = c(0, 1, 2), y = c(1, 1, 1))
  p <- ggplot(data.frame(a = 1:5, b = 1:5, c = letters[1:5]),
              aes(a, b, colour = c)) +
    geom_point() +
    geom_catenary(data = df, mapping = aes(x, y), inherit.aes = FALSE)
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Layer
# ---------------------------------------------------------------------------

test_that("GoG/layer: multiple geom_catenary layers do not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(1, 1, 1))
  p <- ggplot(df, aes(x, y)) +
    geom_catenary(colour = "red") +
    geom_catenary(sag = 0.5, colour = "blue")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/layer: geom_arch works alongside geom_catenary", {
  df <- data.frame(x = c(0, 1, 2), y = c(1, 1, 1))
  p <- ggplot(df, aes(x, y)) +
    geom_catenary() + geom_arch()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Scales
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_y_reverse turns catenary into a visual arch", {
  # scale_y_reverse() flips the coordinate system: a hanging catenary (U)
  # should visually become an arch (∩) — exactly as an inverted catenary is
  # a structural arch.  The anchor y values arrive at the stat pre-negated
  # (y=-1); flipping gravity compensates, so the arch peak is computed at a
  # value MORE POSITIVE than the anchor y (-1), which the reversed axis then
  # displays above the anchor labels.
  df <- data.frame(x = c(1, 5), y = c(1, 1))
  p_fwd <- ggplot(df, aes(x, y)) + geom_catenary()
  p_rev <- ggplot(df, aes(x, y)) + geom_catenary() + scale_y_reverse()
  y_fwd <- ggplot_build(p_fwd)$data[[1]]$y
  y_rev <- ggplot_build(p_rev)$data[[1]]$y
  # Forward: catenary sags BELOW anchors (min y < anchor y = 1)
  expect_true(min(y_fwd) < 1)
  # Reversed: arch peaks ABOVE anchors in visual space.
  # Anchor built-data y = -1; arch peak is at max(y_rev) > -1.
  anchor_y_rev <- -1  # scale_y_reverse negates original y = 1
  expect_true(max(y_rev) > anchor_y_rev)
})

test_that("GoG/scales: scale_y_reverse turns arch into a visual catenary", {
  # Symmetrically to the catenary test: an arch (∩) becomes a catenary (U)
  # when the y-axis is reversed.
  df <- data.frame(x = c(1, 5), y = c(1, 1))
  p_fwd <- ggplot(df, aes(x, y)) + geom_arch()
  p_rev <- ggplot(df, aes(x, y)) + geom_arch() + scale_y_reverse()
  y_fwd <- ggplot_build(p_fwd)$data[[1]]$y
  y_rev <- ggplot_build(p_rev)$data[[1]]$y
  expect_true(max(y_fwd) > 1)   # forward arch peaks above anchors
  expect_true(min(y_rev) < -1)  # reversed sags below anchors
})

test_that("GoG/scales: scale_x_reverse produces all-negative x values (catenary)", {
  df <- data.frame(x = c(1, 5), y = c(1, 1))
  b_fwd <- ggplot_build(ggplot(df, aes(x, y)) + geom_catenary())
  b_rev <- ggplot_build(ggplot(df, aes(x, y)) + geom_catenary() + scale_x_reverse())
  # Same set of |x| values, just negated (possibly reordered by stat)
  expect_equal(sort(b_rev$data[[1]]$x), sort(-b_fwd$data[[1]]$x))
})

test_that("GoG/scales: scale_y_sqrt does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(1, 1, 1))
  p <- ggplot(df, aes(x, y)) + geom_catenary() + scale_y_sqrt()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: explicit limits do not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(1, 1, 1))
  p <- ggplot(df, aes(x, y)) + geom_catenary() +
    scale_y_continuous(limits = c(-5, 5))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: expand = c(0, 0) does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(1, 1, 1))
  p <- ggplot(df, aes(x, y)) + geom_catenary() +
    scale_y_continuous(expand = c(0, 0))
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Coord
# ---------------------------------------------------------------------------

test_that("GoG/coord: coord_cartesian zoom does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(1, 1, 1))
  p <- ggplot(df, aes(x, y)) + geom_catenary() +
    coord_cartesian(ylim = c(-1, 2))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_fixed does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(1, 1, 1))
  p <- ggplot(df, aes(x, y)) + geom_catenary() + coord_fixed()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_flip does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(1, 1, 1))
  p <- ggplot(df, aes(x, y)) + geom_catenary() + coord_flip()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_polar does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(1, 1, 1))
  p <- ggplot(df, aes(x, y)) + geom_catenary() + coord_polar()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Facets
# ---------------------------------------------------------------------------

test_that("GoG/facets: facet_wrap with free scales does not error", {
  df <- data.frame(x = rep(c(0, 1, 2), 2), y = rep(c(1, 1, 1), 2),
                   g = rep(c("a", "b"), each = 3))
  p <- ggplot(df, aes(x, y)) + geom_catenary() +
    facet_wrap(~g, scales = "free")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/facets: facet_grid with free scales does not error", {
  df <- data.frame(x = rep(c(0, 1, 2), 2), y = rep(c(1, 1, 1), 2),
                   g = rep(c("a", "b"), each = 3))
  p <- ggplot(df, aes(x, y)) + geom_catenary() +
    facet_grid(~g, scales = "free")
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------




# ---------------------------------------------------------------------------
# coord_transform restricted-domain regression (issue 2026-04-26)
# ---------------------------------------------------------------------------
#
# Catenaries sag below their endpoints, so even strictly positive endpoints
# produce y < 0 in the reconstructed curve.  Combined with `coord_transform(y =
# "log10")` those negatives transform to NaN and used to crash
# `expand_range4()`.  `.crop_to_coord_domain()` (R/aaa.R) drops them with a
# helpful warning before limit expansion runs.

df_pos_cat <- data.frame(x = c(1, 5), y = c(2, 2))

test_that("coord_transform(y='log10') no longer crashes on catenary sag", {
  p <- ggplot(df_pos_cat, aes(x, y)) +
    geom_catenary() +
    coord_transform(y = "log10")
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("coord_transform(y='log10') drops catenary sag rows", {
  # Behavioural check: cli's regularly-throttled warnings are flaky under
  # expect_warning across tests in the same session, so compare row counts.
  build_plain <- ggplot_build(ggplot(df_pos_cat, aes(x, y)) + geom_catenary())
  build_clip  <- suppressWarnings(ggplot_build(
    ggplot(df_pos_cat, aes(x, y)) +
      geom_catenary() +
      coord_transform(y = "log10")
  ))
  n_negative <- sum(build_plain$data[[1]]$y < 0)
  expect_gt(n_negative, 0L)  # sanity: catenary really does dip below 0 here
  expect_equal(
    nrow(build_clip$data[[1]]),
    nrow(build_plain$data[[1]]) - n_negative
  )
})

test_that("coord_transform crop also wired into stat_arch (no-op for safe input)", {
  # Arches rise above endpoints, so log10 is safe — the crop should run but
  # produce no warning.
  df_arch_pos <- data.frame(x = c(1, 5), y = c(2, 3))
  p <- ggplot(df_arch_pos, aes(x, y)) +
    geom_arch() +
    coord_transform(y = "log10")
  expect_no_warning(ggplotGrob(p))
})

test_that("crop is a no-op on default CoordCartesian", {
  p <- ggplot(df_pos_cat, aes(x, y)) + geom_catenary()
  expect_no_warning(ggplotGrob(p))
})
