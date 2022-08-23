df1 <- data.frame(x = c(1, 4, 4, 3, 2), y = c(1, 1, 1.5, .5, 3))

p1 <- ggplot(df1, aes(x, y)) +
  geom_polygon(fill = NA, linetype = "12", color = "#777777") +
  stat_chaikin(closed = TRUE, geom = "point", iterations = 1) +
  stat_chaikin(closed = TRUE, iterations = 1)

test_that("user can change point apearance", {
  vdiffr::expect_doppelganger("cut corners 1 time", p1)
})
