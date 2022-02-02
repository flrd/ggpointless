test_that("get_locations(..., c('first', 'last')) returns first and last rows", {

  # generate data
  tmp <- seq(5, -1, length.out = 100) * pi
  spiral <- data.frame(x = sin(tmp) * 1:100,
                       y = cos(tmp) * 1:100)

  expect_equal(
    get_locations(spiral, location = c("first", "last")),
    structure(list(x = c(6.12303176911189e-16, -1.22460635382238e-14
    ), y = c(-1, -100), location = c("First", "Last")), class = "data.frame",
    row.names = c(1L, 100L))
  )

  expect_snapshot(spiral[1,, drop = FALSE])
})

test_that("emtpy data argument, or non-data.frame gives error", {
  expect_error(get_locations("a string", location = "all"))
})


