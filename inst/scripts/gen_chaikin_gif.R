## Generate man/figures/chaikin_iterations.gif
## Run from the package root: Rscript inst/scripts/gen_chaikin_gif.R

library(ggplot2)
library(ggpointless)
library(gganimate)
library(magick)

# --- 1. Five-pointed star control polygon ------------------------------------
# 5 outer + 5 inner vertices, interleaved, giving 10 sharp corners.
n_pts   <- 5L
outer_r <- 1
inner_r <- 0.38

ang_out <- seq(pi / 2, pi / 2 + 2 * pi, length.out = n_pts + 1L)[seq_len(n_pts)]
ang_in  <- ang_out + pi / n_pts

pts <- data.frame(
  x = c(rbind(outer_r * cos(ang_out), inner_r * cos(ang_in))),
  y = c(rbind(outer_r * sin(ang_out), inner_r * sin(ang_in)))
)

# --- 2. Reparameterise to a fixed arc-length grid ----------------------------
# Ensures every animation state has the same number of rows so that gganimate
# can tween smoothly between consecutive iteration counts.
reparameterize <- function(x, y, n_grid = 300L) {
  x   <- c(x, x[1L]); y <- c(y, y[1L])               # close the loop
  arc <- c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2)))
  s   <- seq(0, arc[length(arc)], length.out = n_grid + 1L)[seq_len(n_grid)]
  data.frame(x = approx(arc, x, s)$y, y = approx(arc, y, s)$y)
}

# --- 3. One data frame per iteration level -----------------------------------
iter_seq <- c(0, 1, 2, 3, 5, 10)

dfs <- lapply(iter_seq, \(k) {
  xy    <- ggpointless:::get_chaikin(pts$x, pts$y, iterations = k, closed = TRUE)
  d     <- reparameterize(xy$x, xy$y)
  d$label <- factor(
    sprintf("iterations = %d", k),
    levels = sprintf("iterations = %d", iter_seq)
  )
  d
})
df_anim <- do.call(rbind, dfs)

# Original star as dotted reference (closed)
df_ref <- rbind(pts, pts[1L, ])

# --- 4. Animated plot --------------------------------------------------------
p_anim <- ggplot(df_anim, aes(x, y)) +
  geom_polygon(
    data      = df_ref,
    fill      = NA,
    colour    = "#333333",
    linetype  = "dotted",
    linewidth = 0.55
  ) +
  geom_polygon(
    fill      = "#311dfc",
    alpha     = 0.20,
    colour    = "#311dfc",
    linewidth = 0.8
  ) +
  coord_equal(clip = "off") +
  labs(
    title    = "Chaikin's corner-cutting algorithm",
    subtitle = "{closest_state}",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid  = element_line(linetype = "dotted"),
    axis.text   = element_blank(),
    axis.ticks  = element_blank()
  ) +
  transition_states(label, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

anim <- animate(
  p_anim,
  nframes   = 220,
  fps       = 20,
  end_pause = 20,
  width     = 500,
  height    = 500,
  renderer  = magick_renderer()
)

out <- file.path("man", "figures", "chaikin_iterations.gif")
magick::image_write(anim, out)
message("Written: ", out)
