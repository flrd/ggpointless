df1 <- data.frame(x = c(0, 1), xend = c(3, 3))
df2 <- data.frame(x = c(0, 1), xend = c(3, NA))
df3 <- data.frame(
  key = c("A", "B", "B", "C", "D"),
  start = c(0, 1, 6, 5, 6),
  end = c(5, 4, 10, 8, 10)
)
cols <- c("#f4ae1b", "#d77e7b", "#a84dbd", "#311dfc")

p1 <- ggplot(df1, aes(x = x, xend = xend)) +
  geom_lexis()
p2 <- ggplot(df2, aes(x = x, xend = xend)) +
  geom_lexis()

test_that("NA in xend are filled with max(x)", {
  expect_equal(layer_data(p1), layer_data(p2))
})

test_that("readme example works", {
  p <- ggplot(df3, aes(x = start, xend = end, color = key)) +
    geom_lexis(aes(linetype = after_scale(type)), point_size = 3)
  p <- p +
    coord_equal() +
    scale_x_continuous(breaks = c(df3$start, df3$end)) +
    scale_color_manual(values = cols) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  vdiffr::expect_doppelganger("readme geom_lexis example", p)
})

test_that("horizontal lines can be hidden", {
  p <- ggplot(df3, aes(x = start, xend = end, color = key)) +
    geom_lexis(gap_filler = FALSE)
  vdiffr::expect_doppelganger("no horizontal segments", p)
})

test_that("points can have different shape than 19", {
  p <- ggplot(df3, aes(x = start, xend = end, color = key)) +
    geom_lexis(point_size = 3, shape = 21, fill = "#000000", stroke = 2)
  vdiffr::expect_doppelganger("different point shape", p)
})
