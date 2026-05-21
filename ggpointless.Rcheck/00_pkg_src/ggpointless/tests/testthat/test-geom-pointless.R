test_that("geom_pointless accepts only 'first', 'last', 'minimum', 'maximum', and 'all'", {
  df1 <- data.frame(x = 1:3, y = 1:3)
  p <- ggplot(df1, aes(x, y)) +
    geom_pointless(location = c("foo", "bar"))
  expect_warning(print(p))
})

test_that("location = 'all' is equal to c('first', 'last', 'minimum', 'maximum')", {
  set.seed(42)
  df2 <- data.frame(x = 1:10, y = sample(1:10))
  p1 <- ggplot(df2, aes(x, y)) +
    geom_pointless(location = c("all"))
  p2 <- ggplot(df2, aes(x, y)) +
    geom_pointless(location = c("first", "last", "minimum", "maximum"))
  expect_equal(layer_data(p1), layer_data(p2))

  df3 <- data.frame(
    var1 = 1:2,
    var2 = 1:2
  )
  p <- ggplot(df3, aes(x = var1, y = var2))
  p1 <- p + geom_pointless(aes(color = after_stat(location)),
    location = c("first", "last", "minimum", "maximum")
  )
  p2 <- p + geom_pointless(aes(color = after_stat(location)),
    location = c("maximum", "minimum", "last", "first", "all")
  )
  expect_equal(layer_data(p1), layer_data(p2))
})

test_that("geom_pointless works in both directions", {
  df3 <- data.frame(
    x = c(1, 2, 3),
    y = c(1, 2, 1)
  )

  p <- ggplot(df3, aes(x, y)) +
    geom_line() +
    geom_pointless(location = "all")
  x <- layer_data(p)
  expect_false(x$flipped_aes[1])

  p <- ggplot(df3, aes(y, x)) +
    geom_line(orientation = "y") +
    geom_pointless(location = "all", orientation = "y")
  y <- layer_data(p)
  expect_true(y$flipped_aes[1])

  x$flipped_aes <- NULL
  y$flipped_aes <- NULL
  expect_identical(x, ggplot2::flip_data(y, TRUE)[names(x)])
})

test_that("readme example works", {
  cols <- c("#f4ae1b", "#d77e7b", "#a84dbd", "#311dfc")
  x <- seq(-pi, pi, length.out = 500)
  y <- outer(x, 1:5, function(x, y) sin(x * y))

  df1 <- data.frame(
    var1 = x,
    var2 = rowSums(y)
  )

  p <- ggplot(df1, aes(x = var1, y = var2)) +
    geom_line() +
    geom_pointless(aes(color = after_stat(location)),
      location = "all",
      size = 3
    ) +
    scale_color_manual(values = cols) +
    theme_minimal()
  vdiffr::expect_doppelganger("readme geom_pointless example", p)
})


# ===========================================================================
# Grammar of Graphics adversarial stress tests
# ===========================================================================

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

test_that("GoG/data: empty dataset does not error", {
  p <- ggplot(data.frame(x = numeric(), y = numeric()), aes(x, y)) +
    geom_line() + geom_pointless()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: single-row dataset does not error", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_pointless(location = "all")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/data: all-NA y values do not error", {
  p <- ggplot(data.frame(x = 1:3, y = NA_real_), aes(x, y)) +
    geom_pointless()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Mapping
# ---------------------------------------------------------------------------

test_that("GoG/mapping: after_stat(location) colour mapping works", {
  p <- ggplot(data.frame(x = 1:5, y = c(3, 1, 5, 2, 4)), aes(x, y)) +
    geom_pointless(aes(colour = after_stat(location)), location = "all")
  expect_no_error(ggplotGrob(p))
})

test_that("after_stat(location) carries composite labels on collisions", {
  # y = c(5, 3, 1, 3, 5): idx 1 is first + maximum, idx 5 is last + maximum,
  # idx 3 is minimum. Three rows, three distinct composite labels.
  df <- data.frame(x = 1:5, y = c(5, 3, 1, 3, 5))
  p <- ggplot(df, aes(x, y)) +
    geom_line() +
    geom_pointless(
      aes(colour = after_stat(location)),
      location = "all",
      size = 3
    ) +
    theme_minimal()
  vdiffr::expect_doppelganger("pointless composite location labels", p)
})

test_that("GoG/mapping: inherit.aes = FALSE isolates from plot mapping", {
  p <- ggplot(data.frame(x = 1:5, y = 1:5, g = letters[1:5]),
              aes(x, y, colour = g)) +
    geom_line() +
    geom_pointless(data = data.frame(x = 1:3, y = 1:3),
                   inherit.aes = FALSE, mapping = aes(x, y))
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Layer
# ---------------------------------------------------------------------------

test_that("GoG/layer: multiple geom_pointless layers do not error", {
  p <- ggplot(data.frame(x = 1:5, y = c(3, 1, 5, 2, 4)), aes(x, y)) +
    geom_line() +
    geom_pointless(location = "first", colour = "red") +
    geom_pointless(location = "last", colour = "blue")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/layer: geom_pointless before geom_line does not error", {
  p <- ggplot(data.frame(x = 1:5, y = 1:5), aes(x, y)) +
    geom_pointless() + geom_line()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Scales
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_y_log10 does not error", {
  p <- ggplot(data.frame(x = 1:5, y = c(1, 10, 100, 10, 1)), aes(x, y)) +
    geom_line() + geom_pointless(location = "all") + scale_y_log10()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: scale_y_reverse negates y values (pointless)", {
  df_p <- data.frame(x = 1:5, y = 1:5)
  b_fwd <- ggplot_build(ggplot(df_p, aes(x, y)) + geom_line() + geom_pointless())
  b_rev <- ggplot_build(ggplot(df_p, aes(x, y)) + geom_line() + geom_pointless() + scale_y_reverse())
  expect_equal(b_rev$data[[2]]$y, -b_fwd$data[[2]]$y)
})

test_that("GoG/scales: scale_x_reverse negates x values (pointless)", {
  df_p <- data.frame(x = 1:5, y = 1:5)
  b_fwd <- ggplot_build(ggplot(df_p, aes(x, y)) + geom_line() + geom_pointless())
  b_rev <- ggplot_build(ggplot(df_p, aes(x, y)) + geom_line() + geom_pointless() + scale_x_reverse())
  expect_equal(b_rev$data[[2]]$x, -b_fwd$data[[2]]$x)
})

test_that("GoG/scales: scale_y_reverse keeps minimum/maximum labels on the correct data points", {
  # scale_y_reverse() negates y before the stat runs.  Without compensation,
  # location='minimum' would pick the data MAXIMUM (whose negated value is the
  # smallest) and vice-versa.  After the fix, the labels must refer to the
  # original data extremes regardless of axis direction.
  df_p <- data.frame(x = 1:5, y = c(3, 1, 4, 1, 5))
  b_fwd <- ggplot_build(
    ggplot(df_p, aes(x, y)) + geom_line() + geom_pointless(location = "all")
  )
  b_rev <- ggplot_build(
    ggplot(df_p, aes(x, y)) +
      geom_line() + geom_pointless(location = "all") + scale_y_reverse()
  )
  locs_fwd <- b_fwd$data[[2]][, c("x", "y", "location")]
  locs_rev <- b_rev$data[[2]][, c("x", "y", "location")]

  # Composite labels (e.g. "last, maximum" at x = 5) can carry multiple
  # tags per row; grepl on the label string picks up both singleton and
  # composite occurrences.
  has <- function(df, tag) df[grepl(tag, df$location, fixed = TRUE), ]

  # minimum: always at x = 2 and x = 4 (y = 1 in forward, y = -1 in reversed)
  fwd_min <- has(locs_fwd, "minimum")
  rev_min <- has(locs_rev, "minimum")
  expect_equal(sort(fwd_min$x), c(2L, 4L))
  expect_equal(sort(rev_min$x), c(2L, 4L))   # same data points, just negated y
  expect_equal(rev_min$y, -fwd_min$y)

  # maximum: always at x = 5 (y = 5 in forward, y = -5 in reversed);
  # on this dataset x = 5 is also the last point, so the label is the
  # composite "last, maximum".
  fwd_max <- has(locs_fwd, "maximum")
  rev_max <- has(locs_rev, "maximum")
  expect_equal(fwd_max$x, 5L)
  expect_equal(rev_max$x, 5L)
  expect_equal(rev_max$y, -fwd_max$y)
})

test_that("GoG/scales: explicit limits do not error", {
  p <- ggplot(data.frame(x = 1:5, y = 1:5), aes(x, y)) +
    geom_line() + geom_pointless() +
    scale_y_continuous(limits = c(-10, 10))
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Coord
# ---------------------------------------------------------------------------

test_that("GoG/coord: coord_cartesian zoom does not error", {
  p <- ggplot(data.frame(x = 1:5, y = c(3, 1, 5, 2, 4)), aes(x, y)) +
    geom_line() + geom_pointless(location = "all") +
    coord_cartesian(ylim = c(2, 4))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_flip does not error", {
  p <- ggplot(data.frame(x = 1:5, y = 1:5), aes(x, y)) +
    geom_line() + geom_pointless() + coord_flip()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_polar does not error", {
  p <- ggplot(data.frame(x = 1:5, y = 1:5), aes(x, y)) +
    geom_line() + geom_pointless() + coord_polar()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Facets
# ---------------------------------------------------------------------------

test_that("GoG/facets: facet_wrap with free scales does not error", {
  df <- data.frame(x = rep(1:5, 2), y = c(1:5, 5:1), g = rep(c("a", "b"), each = 5))
  p <- ggplot(df, aes(x, y)) +
    geom_line() + geom_pointless(location = "all") +
    facet_wrap(~g, scales = "free")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/facets: facet_grid does not error", {
  df <- data.frame(x = rep(1:5, 2), y = c(1:5, 5:1), g = rep(c("a", "b"), each = 5))
  p <- ggplot(df, aes(x, y)) +
    geom_line() + geom_pointless() +
    facet_grid(~g)
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------

test_that("GoG/theme: theme_void does not error", {
  p <- ggplot(data.frame(x = 1:5, y = 1:5), aes(x, y)) +
    geom_line() + geom_pointless() + theme_void()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: theme_classic does not error", {
  p <- ggplot(data.frame(x = 1:5, y = 1:5), aes(x, y)) +
    geom_line() + geom_pointless() + theme_classic()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: theme_bw does not error", {
  p <- ggplot(data.frame(x = 1:5, y = 1:5), aes(x, y)) +
    geom_line() + geom_pointless() + theme_bw()
  expect_no_error(ggplotGrob(p))
})
