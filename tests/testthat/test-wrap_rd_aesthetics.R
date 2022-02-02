test_that("multiplication works", {
  expect_equal(
    wrap_rd_aesthetics("geom", "point"),
    ggplot2:::rd_aesthetics("geom", "point"))
})
