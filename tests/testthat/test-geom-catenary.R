dat <- data.frame(x = c(0, 1), y = c(1, 1))

test_that("geom_catenary works", {
  p <- ggplot(dat, aes(x, y)) +
    geom_catenary()
  vdiffr::expect_doppelganger("lorem", p)
})

test_that("geom_catenary has a default value for chainLength", {
  p <- ggplot(dat, aes(x, y))
  vdiffr::expect_doppelganger("chainLength default", p + geom_catenary())
})


test_that("user can set a value for chainLength", {
  p <- ggplot(dat, aes(x, y))
  vdiffr::expect_doppelganger("chainLength = 2", p + geom_catenary(chainLength = 2))
})

