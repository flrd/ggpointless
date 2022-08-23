df1 <- data.frame(x = 1, xend = 2, y = 3)
df2 <- data.frame(
  key = c("A", "B", "B", "C", "D"),
  start = c(0, 1, 7, 5, 6),
  end = c(5, 4, 13, 9, 7)
)

p2 <- ggplot(df2, aes(x = start, xend = end, color = key)) +
  stat_lexis(
    point_colour = "black",
    point_size = 3,
    shape = 23,
    fill = "white"
  )

test_that("user can change point apearance", {
  vdiffr::expect_doppelganger("points can be squares", p2)
})
