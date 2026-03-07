# Extracted from test-geom-area-fade.R:267

# prequel ----------------------------------------------------------------------
library(ggplot2)
df_pos   <- data.frame(x = 1:4, y = c(1, 3, 2, 1))
df_neg   <- data.frame(x = 1:4, y = c(-1, -3, -2, -1))
df_mixed <- data.frame(x = 1:4, y = c(1, 2, -1, 0.5))
df_groups <- data.frame(
  g = rep(c("a", "b"), each = 4),
  x = rep(1:4, 2),
  y = c(1, 3, 2, 1, -2, 1, -1, 2)
)
df_fill <- data.frame(x = 1:6, y = c(1, 3, 2, 4, 2, 3), z = 1:6)

# test -------------------------------------------------------------------------
local_mocked_bindings(
    dev.capabilities = function(...) list(compositing = character(0L)),
    .package = "grDevices"
  )
rlang::reset_message_verbosity("geom_area_fade_no_composite")
p <- ggplot(df_fill, aes(x, y, fill = z)) +
    geom_area_fade(position = "identity") +
    theme_minimal()
expect_message(ggplotGrob(p), "does not support")
