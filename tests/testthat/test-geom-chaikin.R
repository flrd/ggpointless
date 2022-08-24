cols <- c("#f4ae1b", "#d77e7b", "#a84dbd", "#311dfc")
lst <- list(
  data = list(
    closed_square = data.frame(x = c(0, 0, 1, 1), y = c(2, 3, 3, 2)),
    whale = data.frame(x = c(.5, 4, 4, 3.5, 2), y = c(.5, 1, 1.5, .5, 3)),
    open_triangle = data.frame(x = c(3, 3, 5), y = c(2, 3, 3)),
    closed_triangle = data.frame(x = c(3.5, 5, 5), y = c(0, 0, 1.5))
  ),
  color = cols,
  closed = c(TRUE, TRUE, FALSE, TRUE)
)

p1 <- ggplot(mapping = aes(x, y)) +
  lapply(lst$data, function(i) {
    geom_polygon(data = i, fill = NA, linetype = "12", color = "#777777")
  }) +
  Map(f = function(data, color, closed) {
    geom_chaikin(data = data, color = color, closed = closed)
  }, data = lst$data, color = lst$color, closed = lst$closed) +
  geom_point(data = data.frame(x = 1.5, y = 1.5)) +
  coord_equal()


test_that("geom_chaikin readme example works", {
  vdiffr::expect_doppelganger("readme geom_chaikin example", p1)
})
