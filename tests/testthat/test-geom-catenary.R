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


test_that("user can set a value for chainLength", {
  p <- ggplot(dat, aes(x, y))
  vdiffr::expect_doppelganger("chainLength = 2", p + geom_catenary(chainLength = 4))
})

test_that("straight line is drawn if chainLength is too short", {
  p <- ggplot(dat, aes(x, y))
  vdiffr::expect_doppelganger("chainLength = 3", p + geom_catenary(chainLength = 3))
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

test_that("chainLength deprecation warning is emitted", {
  expect_warning(
    ggplot(dat[1:2, ], aes(x, y)) + geom_catenary(chainLength = 3),
    "deprecated"
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

test_that("arch_height and arch_length together: arch_height wins with message", {
  p <- ggplot(df_arch[1:2, ], aes(x, y)) +
    geom_arch(arch_height = 0.5, arch_length = 5)
  expect_message(ggplotGrob(p), "arch_height")
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

# --- geom_catenary constructor branch: both chainLength and chain_length ------

test_that("geom_catenary: chain_length wins over chainLength when both supplied (no error)", {
  # cli_inform at lines 89-94 fires at layer construction. The message is
  # frequency-limited ("once per 8h"), so we test for no error rather than
  # capturing the message, which may be throttled in repeated test runs.
  expect_no_error(
    suppressWarnings(suppressMessages(
      ggplot(dat[1:2, ], aes(x, y)) + geom_catenary(chainLength = 3, chain_length = 5)
    ))
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
