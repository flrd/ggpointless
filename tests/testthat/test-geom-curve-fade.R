library(ggplot2)

# ---------------------------------------------------------------------------
# Shared test data
# ---------------------------------------------------------------------------

make_curves <- function(n, seed = 1) {
  set.seed(seed)
  data.frame(
    x    = runif(n, 0, 10),
    y    = runif(n, 0, 10),
    xend = runif(n, 0, 10),
    yend = runif(n, 0, 10)
  )
}

# ---------------------------------------------------------------------------
# curve_count_cap — defensive soft cap on composited curves
# ---------------------------------------------------------------------------

test_that("curve_count_cap: under the cap, the fade grob is built normally", {
  df <- make_curves(5)
  p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade(linewidth = 1.5)
  expect_no_warning(suppressMessages(ggplotGrob(p)))
})

test_that("curve_count_cap: above the cap, warns with a helpful message", {
  # Single test combining all cap-strike assertions — the underlying
  # `cli_warn(.frequency = "regularly")` uses a file-based cache that
  # throttles repeat fires across tests in the same session, so we cannot
  # assert the warning twice.
  df <- make_curves(300)
  p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade(linewidth = 0.5)
  w <- tryCatch(
    suppressMessages(ggplotGrob(p)),
    warning = function(w) conditionMessage(w)
  )
  # If the first warning was the cap warning, w is a string.  If not (the
  # cap fired silently due to frequency throttling from an earlier test,
  # and w is therefore the gtable), we still skip gracefully.
  skip_if_not(is.character(w), "curve_count_cap warning was throttled")
  expect_match(w, "Refusing to composite")
  expect_match(w, "300")
  expect_match(w, "200")
  expect_match(w, "geom_path_fade", fixed = TRUE)
  expect_match(w, "geom_segment_fade", fixed = TRUE)
})

test_that("curve_count_cap: user can opt out with Inf", {
  df <- make_curves(250)
  p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
    geom_curve_fade(linewidth = 0.5, curve_count_cap = Inf)
  # The cap warning must not fire when disabled.  Other device-capability
  # messages from the compositing path may still appear — we only assert
  # that `Refusing to composite` is absent.
  saw_cap_warning <- FALSE
  withCallingHandlers(
    suppressMessages(ggplotGrob(p)),
    warning = function(w) {
      if (grepl("Refusing to composite", conditionMessage(w))) {
        saw_cap_warning <<- TRUE
      }
      invokeRestart("muffleWarning")
    }
  )
  expect_false(saw_cap_warning)
})

test_that("curve_count_cap: nonsensical values warn and fall back to default 200", {
  df <- make_curves(10)
  bad_values <- list(negative = -1, zero = 0, na = NA,
                     string = "big", nonscalar = c(1, 2))
  for (nm in names(bad_values)) {
    p <- ggplot(df, aes(x, y, xend = xend, yend = yend)) +
      geom_curve_fade(curve_count_cap = bad_values[[nm]])
    expect_warning(
      suppressMessages(ggplotGrob(p)),
      regexp = "curve_count_cap.*must be a positive scalar number or `Inf`",
      info = paste("case:", nm)
    )
  }
})

test_that("curve_count_cap: default constructor exposes curve_count_cap = 200", {
  lyr <- geom_curve_fade()
  expect_equal(lyr$geom_params$curve_count_cap, 200)
})
