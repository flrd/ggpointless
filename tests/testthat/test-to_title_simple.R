test_that("multiplication works", {
  expect_equal(to_title_simple(c("Foo", "bar")), c("Foo", "Bar"))
  expect_equal(to_title_simple(c("foo", "Bar")), c("Foo", "Bar"))
  expect_equal(to_title_simple(c("foo", "bar")), c("Foo", "Bar"))
})
