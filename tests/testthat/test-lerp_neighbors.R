test_that("lerp_neighbors works", {
  expect_equal(
    c(1.75, 1.25, 1.75, 2.25, 2.75, 3.25, 3.75, 3.25),
    lerp_neighbors(x = seq_len(4), ratio = .25)
  )
})
