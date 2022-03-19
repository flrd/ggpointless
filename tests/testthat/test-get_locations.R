test_that("get_locations(..., c('all')) returns 4 rows", {

  # generate data
  tmp <- seq(5, -1, length.out = 100) * pi
  spiral <- data.frame(
    x = round(sin(tmp) * 1:100, 4),
    y = round(cos(tmp) * 1:100, 4)
  )

  expect_equal(
    get_locations(spiral, location = c("all")),
    structure(list(x = c(0, -7.9847, 0, 0), y = c(-1, 83.6196, -100,
    -100), location = structure(c(1L, 4L, 3L, 2L), .Label = c("first",
    "last", "minimum", "maximum"), class = "factor")), class = "data.frame",
    row.names = c("1", "84", "100", "100.1"))
  )
})

test_that("emtpy data argument, or non-data.frame gives error", {
  expect_error(get_locations("a string", location = "all"))
  expect_error(get_locations(NULL, location = "all"))
  expect_error(get_locations(matrix(1:4, nrow = 2), location = "all"))
  expect_error(get_locations(NA, location = "all"))
})
