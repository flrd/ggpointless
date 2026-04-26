# Doppelgänger drop-in replacement tests
#
# Every ggpointless doppelgänger geom should be a true drop-in replacement for
# its ggplot2 equivalent. This suite is a direct transcription of the official
# ggplot2 documentation examples, with only the geom name swapped.
#
# If an example from `?geom_bar` or `?geom_col` is NOT in this file, it is
# a gap in coverage — please add it.
#
# Examples sourced from: ?ggplot2::geom_bar, ?ggplot2::geom_col

library(ggplot2)

# ===========================================================================
# geom_unit_bar drop-in for geom_bar
# From: ?ggplot2::geom_bar (all documented examples, in order)
# ===========================================================================

test_that("geom_unit_bar [doc ex 1]: basic count", {
  skip_if_not_installed("vdiffr")
  # geom_bar: g + geom_bar()
  g <- ggplot(mpg, aes(class))
  vdiffr::expect_doppelganger("bar-cells-basic", g + geom_unit_bar())
})

test_that("geom_unit_bar [doc ex 2]: weighted counts", {
  # geom_bar: g + geom_bar(aes(weight = displ))
  g <- ggplot(mpg, aes(class))
  expect_no_error(ggplotGrob(g + geom_unit_bar(aes(weight = displ))))
})

test_that("geom_unit_bar [doc ex 3]: y aesthetic flips orientation", {
  # geom_bar: ggplot(mpg) + geom_bar(aes(y = class))
  expect_no_error(ggplotGrob(ggplot(mpg) + geom_unit_bar(aes(y = class))))
})

test_that("geom_unit_bar [doc ex 4]: fill aesthetic (auto-stacked)", {
  skip_if_not_installed("vdiffr")
  # geom_bar: g + geom_bar(aes(fill = drv))
  g <- ggplot(mpg, aes(class))
  vdiffr::expect_doppelganger("bar-cells-fill", g + geom_unit_bar(aes(fill = drv)))
})

test_that("geom_unit_bar [doc ex 5]: y + position_stack(reverse=TRUE)", {
  # geom_bar: ggplot(mpg, aes(y = class)) +
  #             geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) +
  #             theme(legend.position = "top")
  p <- ggplot(mpg, aes(y = class)) +
    geom_unit_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) +
    theme(legend.position = "top")
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("geom_unit_bar [doc ex 6]: continuous x data", {
  # geom_bar: ggplot(df, aes(x)) + geom_bar()
  df <- data.frame(x = rep(c(2.9, 3.1, 4.5), c(5, 10, 4)))
  expect_no_error(ggplotGrob(ggplot(df, aes(x)) + geom_unit_bar()))
})

# ===========================================================================
# geom_unit_col drop-in for geom_col
# From: ?ggplot2::geom_col (all documented examples, in order)
# ===========================================================================

test_that("geom_unit_col [doc ex 1]: basic pre-computed heights", {
  skip_if_not_installed("vdiffr")
  # geom_col: ggplot(df, aes(trt, outcome)) + geom_col()
  df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
  vdiffr::expect_doppelganger("col-cells-basic", ggplot(df, aes(trt, outcome)) + geom_unit_col())
})

test_that("geom_unit_col [doc ex 2]: date axis with just=0.5 (centred)", {
  # geom_col: ggplot(df, aes(x, y)) + geom_col(just = 0.5)
  df <- data.frame(x = as.Date(c("2020-01-01", "2020-02-01")), y = 1:2)
  expect_no_error(ggplotGrob(ggplot(df, aes(x, y)) + geom_unit_col(just = 0.5)))
})

test_that("geom_unit_col [doc ex 3]: date axis with just=1 (left-aligned)", {
  # geom_col: ggplot(df, aes(x, y)) + geom_col(just = 1)
  df <- data.frame(x = as.Date(c("2020-01-01", "2020-02-01")), y = 1:2)
  expect_no_error(ggplotGrob(ggplot(df, aes(x, y)) + geom_unit_col(just = 1)))
})
