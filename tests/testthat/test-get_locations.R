test_that("get_locations(..., c('first', 'last')) returns first and last rows", {

  # generate data
  tmp <- seq(5, -1, length.out = 100) * pi
  spiral <- data.frame(x = round(sin(tmp) * 1:100, 4),
                       y = round(cos(tmp) * 1:100), 4)

  expect_equal(
    get_locations(spiral, location = c("all")),
    structure(list(x = c(0, -7.9847, 0, 0), y = c(-1, 84, -100, -100
    ), X4 = c(4, 4, 4, 4), location = structure(c(4L, 1L, 2L, 3L),
    .Label = c("maximum", "minimum", "last", "first"),
    class = "factor")), class = "data.frame", row.names = c("1", "84", "100", "100.1"))
  )

  expect_snapshot(spiral[1,, drop = FALSE])
})

test_that("emtpy data argument, or non-data.frame gives error", {
  expect_error(get_locations("a string", location = "all"))
  expect_error(get_locations(NULL, location = "all"))
  expect_error(get_locations(matrix(1:4, nrow = 2), location = "all"))
  expect_error(get_locations(NA, location = "all"))
})
