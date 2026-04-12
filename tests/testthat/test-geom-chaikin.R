cols <- c("#f4ae1b", "#d77e7b", "#a84dbd", "#311dfc")
lst <- list(
  data = list(
    closed_square = data.frame(x = c(0, 0, 1, 1), y = c(2, 3, 3, 2)),
    whale = data.frame(x = c(.5, 4, 4, 3.5, 2), y = c(.5, 1, 1.5, .5, 3)),
    open_triangle = data.frame(x = c(3, 3, 5), y = c(2, 3, 3)),
    closed_triangle = data.frame(x = c(3.5, 5, 5), y = c(0, 0, 1.5))
  ),
  color = cols,
  mode = c("closed", "closed", "open", "closed")
)

p1 <- ggplot(mapping = aes(x, y)) +
  lapply(lst$data, \(i) {
    geom_polygon(data = i, fill = NA, linetype = "12", color = "#777777")
  }) +
  Map(f = \(data, color, mode) {
    geom_chaikin(data = data, color = color, mode = mode)
  }, data = lst$data, color = lst$color, mode = lst$mode) +
  geom_point(data = data.frame(x = 1.5, y = 1.5)) +
  coord_equal()


test_that("geom_chaikin readme example works", {
  vdiffr::expect_doppelganger("readme geom_chaikin example", p1)
})


# ===========================================================================
# Grammar of Graphics adversarial stress tests
# ===========================================================================

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

test_that("GoG/data: empty dataset does not error", {
  p <- ggplot(data.frame(x = numeric(), y = numeric()), aes(x, y)) +
    geom_chaikin()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: single point does not error", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_chaikin()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

test_that("GoG/data: two points (minimal path) does not error", {
  p <- ggplot(data.frame(x = c(0, 1), y = c(0, 1)), aes(x, y)) +
    geom_chaikin()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/data: all-NA y values do not error", {
  p <- ggplot(data.frame(x = 1:3, y = NA_real_), aes(x, y)) +
    geom_chaikin()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Mapping
# ---------------------------------------------------------------------------

test_that("GoG/mapping: colour aesthetic mapping does not error", {
  df <- data.frame(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0),
                   g = c("a", "a", "b", "b"))
  p <- ggplot(df, aes(x, y, colour = g)) + geom_chaikin()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/mapping: inherit.aes = FALSE isolates from plot mapping", {
  df <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0))
  p <- ggplot(data.frame(a = 1:5, b = 1:5, c = letters[1:5]),
              aes(a, b, colour = c)) +
    geom_point() +
    geom_chaikin(data = df, mapping = aes(x, y), inherit.aes = FALSE)
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Layer
# ---------------------------------------------------------------------------

test_that("GoG/layer: multiple geom_chaikin layers do not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0))
  p <- ggplot(df, aes(x, y)) +
    geom_chaikin(colour = "red") +
    geom_chaikin(iterations = 5, colour = "blue")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/layer: geom_chaikin as standalone layer does not error", {
  df <- data.frame(x = c(0, 1, 2, 3), y = c(0, 1, 1, 0))
  p <- ggplot(df, aes(x, y)) + geom_chaikin()
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Scales
# ---------------------------------------------------------------------------

test_that("GoG/scales: scale_y_reverse does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0))
  p <- ggplot(df, aes(x, y)) + geom_chaikin() + scale_y_reverse()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: scale_y_sqrt does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0))
  p <- ggplot(df, aes(x, y)) + geom_chaikin() + scale_y_sqrt()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/scales: explicit limits do not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0))
  p <- ggplot(df, aes(x, y)) + geom_chaikin() +
    scale_y_continuous(limits = c(-5, 5))
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Coord
# ---------------------------------------------------------------------------

test_that("GoG/coord: coord_cartesian zoom does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0))
  p <- ggplot(df, aes(x, y)) + geom_chaikin() +
    coord_cartesian(ylim = c(0, 0.5))
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_fixed does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0))
  p <- ggplot(df, aes(x, y)) + geom_chaikin() + coord_fixed()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_flip does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0))
  p <- ggplot(df, aes(x, y)) + geom_chaikin() + coord_flip()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/coord: coord_polar does not error", {
  df <- data.frame(x = c(0, 1, 2, 3), y = c(0, 1, 1, 0))
  p <- ggplot(df, aes(x, y)) + geom_chaikin() + coord_polar()
  expect_no_error(suppressWarnings(ggplotGrob(p)))
})

# ---------------------------------------------------------------------------
# Facets
# ---------------------------------------------------------------------------

test_that("GoG/facets: facet_wrap with free scales does not error", {
  df <- data.frame(x = rep(c(0, 1, 2), 2), y = c(0, 1, 0, 0, 2, 0),
                   g = rep(c("a", "b"), each = 3))
  p <- ggplot(df, aes(x, y)) + geom_chaikin() +
    facet_wrap(~g, scales = "free")
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/facets: facet_grid does not error", {
  df <- data.frame(x = rep(c(0, 1, 2), 2), y = c(0, 1, 0, 0, 2, 0),
                   g = rep(c("a", "b"), each = 3))
  p <- ggplot(df, aes(x, y)) + geom_chaikin() +
    facet_grid(~g)
  expect_no_error(ggplotGrob(p))
})

# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------

test_that("GoG/theme: theme_void does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0))
  p <- ggplot(df, aes(x, y)) + geom_chaikin() + theme_void()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: theme_classic does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0))
  p <- ggplot(df, aes(x, y)) + geom_chaikin() + theme_classic()
  expect_no_error(ggplotGrob(p))
})

test_that("GoG/theme: theme_bw does not error", {
  df <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0))
  p <- ggplot(df, aes(x, y)) + geom_chaikin() + theme_bw()
  expect_no_error(ggplotGrob(p))
})
