df1 <- data.frame(x = c(1, 4, 4, 3, 2), y = c(1, 1, 1.5, .5, 3))

p1 <- ggplot(df1, aes(x, y)) +
  geom_polygon(fill = NA, linetype = "12", color = "#777777") +
  stat_chaikin(closed = TRUE, geom = "point", iterations = 1) +
  stat_chaikin(closed = TRUE, iterations = 1)

test_that("user can change point apearance", {
  vdiffr::expect_doppelganger("cut corners 1 time", p1)
})

# --- stat_chaikin deprecation branch: both closed and mode supplied ----------

test_that("stat_chaikin: mode wins over closed when both are provided (no error)", {
  # cli_inform at lines 20-24 fires at layer construction. The message is
  # frequency-limited ("once per 8h"), so we test for no error rather than
  # capturing the message, which may be throttled in repeated test runs.
  expect_no_error(
    suppressWarnings(suppressMessages(
      ggplot(df1, aes(x, y)) + stat_chaikin(closed = TRUE, mode = "open")
    ))
  )
})

# --- get_chaikin unit tests --------------------------------------------------

test_that("get_chaikin: iterations = 0 returns the input unchanged", {
  result <- get_chaikin(c(0, 1, 2), c(0, 1, 0), iterations = 0L)
  expect_equal(result$x, c(0, 1, 2))
  expect_equal(result$y, c(0, 1, 0))
})

test_that("get_chaikin: zero-length x or y raises an error", {
  expect_error(get_chaikin(numeric(0), numeric(0)), "positive length")
})

test_that("get_chaikin: zero-length input with iterations = 0 also raises an error", {
  # Length validation now precedes the iterations == 0 early return so that
  # invalid inputs are always caught, regardless of iterations.
  expect_error(get_chaikin(numeric(0), numeric(0), iterations = 0L), "positive length")
})

test_that("get_chaikin: mismatched x/y lengths raise an error", {
  expect_error(get_chaikin(1:3, 1:2), "same length")
})

test_that("get_chaikin: mismatched lengths with iterations = 0 also raise an error", {
  expect_error(get_chaikin(1:3, 1:2, iterations = 0L), "same length")
})

# --- ratio validation --------------------------------------------------------

test_that("ratio = NA_real_ raises a clean error (not a cryptic base-R condition)", {
  # Previously, !is.numeric(NA_real_) was FALSE, then NA_real_ < 0 evaluated
  # to NA, and if(NA) threw "missing value where TRUE/FALSE needed".
  p <- ggplot(df1, aes(x, y)) + stat_chaikin(ratio = NA_real_)
  expect_error(ggplotGrob(p), "ratio")
})

test_that("ratio = Inf raises an error", {
  p <- ggplot(df1, aes(x, y)) + stat_chaikin(ratio = Inf)
  expect_error(ggplotGrob(p), "ratio")
})

# --- non-finite values in x / y ---------------------------------------------

test_that("get_chaikin: non-finite values are removed with a cli warning", {
  # Use 4 points so that 3 remain after the Inf is dropped — enough to smooth.
  expect_warning(
    result <- get_chaikin(c(0, Inf, 1, 2), c(0, 0, 0, 0), iterations = 1),
    "non-finite"
  )
  expect_true(all(is.finite(result$x)))
  expect_true(all(is.finite(result$y)))
})

test_that("get_chaikin: all-non-finite input returns empty data frame with a warning", {
  expect_warning(
    result <- get_chaikin(c(Inf, NaN), c(0, 0), iterations = 1),
    "non-finite"
  )
  expect_equal(nrow(result), 0L)
})

# --- minimum-points guard ----------------------------------------------------

test_that("get_chaikin: fewer than 3 points warns and returns input unchanged", {
  # n = 2, open path — previously a silent no-op (no warning, no smoothing)
  expect_warning(
    result <- get_chaikin(c(0, 1), c(0, 0), iterations = 1, closed = FALSE),
    "at least 3"
  )
  expect_equal(result$x, c(0, 1))
  expect_equal(result$y, c(0, 0))
})

test_that("get_chaikin: single point warns and returns input unchanged", {
  # n = 1 previously emitted a base-R recycling warning from cut_corners().
  expect_warning(
    result <- get_chaikin(c(0), c(0), iterations = 1, closed = FALSE),
    "at least 3"
  )
  expect_equal(result$x, 0)
  expect_equal(result$y, 0)
})

test_that("get_chaikin: fewer than 3 points warns for closed paths too", {
  # n = 2, closed — previously produced 4 degenerate duplicated points silently.
  expect_warning(
    result <- get_chaikin(c(0, 1), c(0, 0), iterations = 1, closed = TRUE),
    "at least 3"
  )
  expect_equal(result$x, c(0, 1))
})
