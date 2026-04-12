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

test_that("GoG/scales: scale_y_reverse does not error", {
  p <- ggplot(data.frame(x = 1:5, y = 1:5), aes(x, y)) +
    geom_line() + geom_pointless() + scale_y_reverse()
  expect_no_error(ggplotGrob(p))
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
