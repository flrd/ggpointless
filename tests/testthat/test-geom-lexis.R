df1 <- data.frame(x = c(0, 1), xend = c(3, 3))
df2 <- data.frame(x = c(0, 1), xend = c(3, NA))
df3 <- data.frame(
  key = c("A", "B", "B", "C", "D"),
  x = c(0, 1, 6, 5, 6),
  xend = c(5, 4, 10, 8, 10)
)
cols <- c("#f4ae1b", "#d77e7b", "#a84dbd", "#311dfc")

p1 <- ggplot(df1, aes(x = x, xend = xend)) +
  geom_lexis()
p2 <- ggplot(df2, aes(x = x, xend = xend)) +
  geom_lexis()

test_that("readme example works", {
  p <- ggplot(df3, aes(x = x, xend = xend, color = key)) +
    geom_lexis(aes(linetype = after_stat(type)), size = 3)
  p <- p +
    coord_equal() +
    scale_x_continuous(breaks = c(df3$x, df3$xend)) +
    scale_color_manual(values = cols) +
    scale_linetype_identity() +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())

  vdiffr::expect_doppelganger("readme geom_lexis example", p)
})

test_that("horizontal lines can be hidden", {
  p <- ggplot(df3, aes(x = x, xend = xend, color = key)) +
    geom_lexis(gap_filler = FALSE)
  vdiffr::expect_doppelganger("no horizontal segments", p)
})

test_that("points can have different shape than 19", {
  p <- ggplot(df3, aes(x = x, xend = xend, color = key)) +
    geom_lexis(size = 3, shape = 21, fill = "#000000", stroke = 2)
  vdiffr::expect_doppelganger("different point shape", p)
})

# --- draw_key_pointless / draw_key_lexis unit tests --------------------------
# The visual tests above all use vdiffr::expect_doppelganger().  vdiffr calls
# vdiffr_enabled(), which returns FALSE during R CMD check and in covr's
# subprocess; the tests are skipped without rendering.  draw_key_pointless and
# draw_key_lexis are therefore never reached by covr through the visual tests.
# The direct calls below cover the untested branches.

# Minimal key data for draw_key_pointless
kd_pl <- data.frame(colour = "black", fill = NA_character_,
                    size = 1, alpha = NA_real_, stroke = 0.5)

test_that("draw_key_pointless: NULL shape defaults to pch 19", {
  # No 'shape' column → data$shape is NULL
  result <- draw_key_pointless(kd_pl, list(), grid::unit(c(1, 1), "cm"))
  expect_s3_class(result, "points")
  expect_equal(result$pch, 19L)
})

test_that("draw_key_pointless: character shape is translated via translate_shape_string", {
  data_chr <- kd_pl
  data_chr$shape <- "circle"
  result <- draw_key_pointless(data_chr, list(), grid::unit(c(1, 1), "cm"))
  expect_s3_class(result, "points")
  expect_true(is.numeric(result$pch))
})

# --- draw_key_lexis ----------------------------------------------------------

kd_lx <- data.frame(colour = "red", fill = NA_character_, alpha = NA_real_,
                    linetype = 1L, linewidth = 0.5, size = 1, stroke = 0.5)

test_that("character x/xend gives a clear warning, not a misleading geometry error", {
  df_bad <- data.frame(
    key = c("A", "B", "B", "C", "D"),
    x = c(0, 1, 6, 5, "b"),
    xend = c(5, 4, 10, 8, "c")
  )
  p <- ggplot(df_bad, aes(x = x, xend = xend, colour = key)) +
    geom_lexis()
  expect_warning(
    ggplot_build(p),
    "continuous variable"
  )
})

test_that("draw_key_lexis: point_show = FALSE returns the segment grob only (no grobTree)", {
  params <- list(point_show = FALSE)
  result <- draw_key_lexis(kd_lx, params, grid::unit(c(1, 1), "cm"))
  expect_false(inherits(result, "gTree"))
  expect_s3_class(result, "segments")
})
