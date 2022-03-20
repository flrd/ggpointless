shapes <- c(
  "square open",
  "circle open",
  "triangle open",
  "plus",
  "cross",
  "diamond open",
  "triangle down open",
  "square cross",
  "asterisk",
  "diamond plus",
  "circle plus",
  "star",
  "square plus",
  "circle cross",
  "square triangle",
  "triangle square",
  "square",
  "circle small",
  "triangle",
  "diamond",
  "circle",
  "bullet",
  "circle filled",
  "square filled",
  "diamond filled",
  "triangle filled",
  "triangle down filled"
)
test_that("shapes are translated correctly", {
  expect_equal(translate_shape_string(shapes), c(0, 1:14, 14:25))
})

test_that("I get an error for shapes that do not exists", {
  expect_error(translate_shape_string(c("foo" = "bar")))
})

test_that("Shape names must be unambiguous:", {
  expect_error(translate_shape_string(rep("trian", 2)))
})
