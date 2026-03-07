## Generate man/figures/fourier_square_wave.gif
## Run from the package root: Rscript inst/scripts/gen_fourier_gif.R

library(ggplot2)
library(ggpointless)
library(gganimate)
library(magick)

# 501-point square wave on [0, 2π]
n_obs <- 501L
x_sw  <- seq(0, 2 * pi, length.out = n_obs)
df_sw <- data.frame(x = x_sw, y = ifelse(x_sw < pi, 1, -1))

# Harmonic sequence — odd harmonics reveal the square-wave structure.
# stat_fourier always returns max(500, 2 * n_obs) = 1002 rows per state,
# so every state shares the same x grid, which gganimate needs for smooth
# tweening between frames.
max_k     <- floor(n_obs / 2L)   # = 250 (Nyquist limit)
harmonics <- c(1L, 3L, 5L, 7L, 9L, 11L, 15L, 21L, 31L, 51L, max_k)

dfs <- lapply(harmonics, function(n_h) {
  built   <- ggplot_build(
    ggplot(df_sw, aes(x, y)) + stat_fourier(n_harmonics = n_h)
  )
  d       <- built$data[[1]]
  d$label <- factor(sprintf("n = %d", n_h),
                    levels = sprintf("n = %d", harmonics))
  d
})
df_anim <- do.call(rbind, dfs)

# Perfect square wave for reference: two horizontal + one vertical segment
df_ref <- data.frame(
  x    = c(0,  pi, pi),
  xend = c(pi, pi,  2 * pi),
  y    = c(1,   1,  -1),
  yend = c(1,  -1,  -1)
)

p_anim <- ggplot(df_anim, aes(x, y)) +
  geom_segment(
    data        = df_ref,
    aes(x = x, y = y, xend = xend, yend = yend),
    colour      = "#333333",
    linetype    = "dotted",
    linewidth   = 0.55,
    inherit.aes = FALSE
  ) +
  geom_line(colour = "#311dfc", linewidth = 0.8) +
  scale_x_continuous(
    breaks = c(0, pi / 2, pi, 3 * pi / 2, 2 * pi),
    labels = c("0", expression(pi / 2), expression(pi),
               expression(3 * pi / 2), expression(2 * pi))
  ) +
  ylim(-1.5, 1.5) +
  labs(
    title    = "Fourier series: square wave approximation",
    subtitle = "Harmonics included: {closest_state}",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal() +
  theme(panel.grid = element_line(linetype = "dotted")) +
  transition_states(label, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

anim <- animate(
  p_anim,
  nframes   = 220,
  fps       = 20,
  end_pause = 20,
  width     = 600,
  height    = 350,
  renderer  = magick_renderer()
)

out <- file.path("man", "figures", "fourier_square_wave.gif")
magick::image_write(anim, out)
message("Written: ", out)
