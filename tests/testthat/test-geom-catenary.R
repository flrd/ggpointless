dat <- data.frame(x = c(0, 1, 2), y = c(1, 1, 1))

test_that("geom_catenary works", {
  p <- ggplot(dat, aes(x, y)) +
    geom_catenary()
  vdiffr::expect_doppelganger("geom_catenary_default", p)
})

test_that("geom_catenary has a default value for chainLength", {
  p <- ggplot(dat, aes(x, y))
  vdiffr::expect_doppelganger("chainLength default", p + geom_catenary())
})


test_that("user can set a value for chainLength", {
  p <- ggplot(dat, aes(x, y))
  vdiffr::expect_doppelganger("chainLength = 2", p + geom_catenary(chainLength = 4))
})

test_that("straight line is drawn if chainLength is too short", {
  p <- ggplot(dat, aes(x, y))
  vdiffr::expect_doppelganger("chainLength = 3", p + geom_catenary(chainLength = 3))
})

test_that("stat_catenary also works", {
  p <- ggplot(dat[c(1, 2),], aes(x, y))
  vdiffr::expect_doppelganger("stat_catenary", p + stat_catenary())
})

test_that("stat_catenary also works for reversed data", {
  p <- ggplot(dat[c(2, 1),], aes(x, y))
  vdiffr::expect_doppelganger("stat_catenary-rev", p + stat_catenary())
})
