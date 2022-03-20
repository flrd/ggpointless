df1 <- data.frame(x = c(0, 1), xend = c(3, 3))
df2 <- data.frame(x = c(0, 1), xend = c(3, NA))

p1 <- ggplot(df1, aes(x = x, xend = xend)) +
  geom_lexis()
p2 <- ggplot(df2, aes(x = x, xend = xend)) +
  geom_lexis()

test_that("NA in xend are filled with max(x)", {
  expect_equal(layer_data(p1), layer_data(p2))
})

test_that("readme example works", {
  df1 <- data.frame(
    key = c("A", "B", "B", "C", "D"),
    start = c(0, 1, 6, 5, 6),
    end = c(5, 4, 10, 8, 10)
  )
  cols <- c("#f4ae1b", "#d77e7b", "#a84dbd", "#311dfc")

  p <- ggplot(df1, aes(x = start, xend = end, color = key)) +
    geom_lexis(aes(linetype = after_scale(type)), point.size = 3)
  p <- p +
    coord_equal() +
    scale_x_continuous(breaks = c(df1$start, df1$end)) +
    scale_color_manual(values = cols) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  vdiffr::expect_doppelganger("readme geom_lexis example", p)
})
