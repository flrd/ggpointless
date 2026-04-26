test_that("get_locations(..., c('all')) returns composite label on collision", {

  # generate data
  tmp <- seq(5, -1, length.out = 100) * pi
  spiral <- data.frame(
    x = round(sin(tmp) * 1:100, 4),
    y = round(cos(tmp) * 1:100, 4)
  )

  # minimum coincides with last (both at row 100, y = -100): the colliding
  # point is emitted once with a composite label "last, minimum".
  res <- get_locations(spiral, location = c("all"))
  expect_equal(nrow(res), 3L)
  expect_equal(
    as.character(res$location),
    c("first", "last, minimum", "maximum")
  )
  expect_equal(
    levels(res$location),
    c("first", "last, minimum", "maximum")
  )
})

test_that("emtpy data argument, or non-data.frame gives error", {
  expect_error(get_locations("a string", location = "all"))
  expect_error(get_locations(NULL, location = "all"))
  expect_error(get_locations(matrix(1:4, nrow = 2), location = "all"))
  expect_error(get_locations(NA, location = "all"))
})
