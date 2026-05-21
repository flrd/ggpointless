test_that("neighbors works", {
  # example
  # x:               1, 2, 3, 4
  # left neighbors:  4, 1, 2, 3
  # right neighbors: 2, 3, 4, 1
  #
  # neighbors combined in correct order:
  # neighbors: 4, 2, 1, 3, 2, 4, 3, 1

  expect_equal(
    neighbors(seq_len(4)),
    c(4, 2, 1, 3, 2, 4, 3, 1)
  )
})
