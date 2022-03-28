# Setting of legend key glyphs has to be tested visually

test_that("alternative key glyphs work", {
  df1 <- data.frame(x = 1:3, xend = 4:6, z = letters[1:3])

  # specify key glyph by name
  vdiffr::expect_doppelganger(
    "pointrange and lexis key glyphs",
    ggplot(df1, aes(x = x, y = xend)) +
      geom_line(aes(color = "line"),
        key_glyph = "pointrange"
      ) +
      geom_point(aes(fill = z),
        pch = 21,
        size = 2,
        key_glyph = "lexis"
      )
  )

  # specify key glyph by function
  vdiffr::expect_doppelganger(
    "lexis key glyphs",
    ggplot(df1, aes(x = x, xend = xend)) +
      geom_lexis(aes(fill = z),
        pch = 21,
        point_size = 3,
        stroke = 1
      )
  )
})

test_that("lexis key depends on point_show setting", {
  df2 <- data.frame(x = 0, xend = 1)

  vdiffr::expect_doppelganger(
    "point_show = TRUE",
    ggplot(df2, aes(x = x, xend = xend, color = "red")) + geom_lexis(point_show = TRUE)
  )

  vdiffr::expect_doppelganger(
    "point_show = FALSE",
    ggplot(df2, aes(x = x, xend = xend, color = "red")) + geom_lexis(point_show = FALSE)
  )
})
