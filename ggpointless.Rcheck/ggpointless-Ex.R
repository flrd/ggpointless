pkgname <- "ggpointless"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ggpointless')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("draw_key_lexis")
### * draw_key_lexis

flush(stderr()); flush(stdout())

### Name: draw_key_lexis
### Title: Key glyphs for legends
### Aliases: draw_key_lexis
### Keywords: internal

### ** Examples

ggplot(economics_long, aes(date, value01, colour = variable)) +
  geom_line(key_glyph = "lexis")




cleanEx()
nameEx("geom_abline_fade")
### * geom_abline_fade

flush(stderr()); flush(stdout())

### Name: geom_abline_fade
### Title: Reference Lines with a Fading Gradient
### Aliases: geom_abline_fade geom_hline_fade geom_vline_fade

### ** Examples

library(ggplot2)

p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

# Horizontal reference line, fading from the left
p + geom_hline_fade(yintercept = 20, linewidth = 1.5)

# Vertical line fading at both ends
p + geom_vline_fade(xintercept = 3, linewidth = 1.5,
                    fade_direction = c("start", "end"))

# Diagonal line of best fit, fading from the left
coefs <- coef(lm(mpg ~ wt, data = mtcars))
p + geom_abline_fade(intercept = coefs[1], slope = coefs[2], linewidth = 1.5)




cleanEx()
nameEx("geom_area_fade")
### * geom_area_fade

flush(stderr()); flush(stdout())

### Name: geom_area_fade
### Title: Area Plots with Fading Linear Gradient
### Aliases: geom_area_fade

### ** Examples

library(ggplot2)
df1 <- data.frame(
  g = c("a", "a", "a", "b", "b", "b"),
  x = c(1, 3, 5, 2, 4, 6),
  y = c(2, 5, 1, 3, 6, 7)
)

a <- ggplot(df1, aes(x, y, fill = g)) +
  theme_minimal()

# default behaviour: opaque at data line, transparent at y = 0
# the outline colour remains unaffected
a + geom_area_fade()

# change overall opacity
a + geom_area_fade(alpha = .25)

# keep some opacity at the baseline
a + geom_area_fade(alpha_fade_to = .25)

# suppress the default upper outline
a + geom_area_fade(outline.type = "none")

# closed outline (all four edges)
a + geom_area_fade(outline.type = "full")

# horizontal orientation
a + geom_area_fade(aes(y, x), orientation = "y")

# disable stat alignment (useful when x values are already aligned)
a + geom_area_fade(stat = "identity")

# draw upper and lower outlines (no left/right edges)
a + geom_area_fade(outline.type = "both", stat = "identity")

# Use the "alpha_scope" argument to scale the alpha
# value of the gradients separately for each group
df2 <- data.frame(
  g = c("a", "a", "a", "b", "b", "b"),
  x = c(1, 3, 5, 2, 4, 6),
  y = c(1, 2, 1, 9, 10, 8)
)
b <- ggplot(df2, aes(x, y, fill = g)) +
  theme_minimal()

# alpha_scope = "group": each group uses the alpha range independently
b + geom_area_fade(
  alpha_scope = "group",
  position = "identity"
  )

# compare with the default where small groups appear washed out
# next to dominant groups, especially when position = "identity"
b + geom_area_fade(
  alpha_scope = "global", # default
  position = "identity"
  )

# geom_area_fade works with negative values too:
# the gradient fades towards y = 0 from both sides
d <- ggplot(df2, aes(x, y - mean(y))) +
  theme_minimal()
d + geom_area_fade()

# overwrite both fill and colour
d + geom_area_fade(
  fill = "#0833F5",
  colour = "#d77e7b",
  outline.type = "lower"
  )

# a 2D-gradient is produced when fill is mapped to a variable
# this may not work on all graphic devices, see vignette for details
d + geom_area_fade(
  aes(fill = y),
  colour = "#333333",
  outline.type = "both"
  )




cleanEx()
nameEx("geom_catenary")
### * geom_catenary

flush(stderr()); flush(stdout())

### Name: geom_catenary
### Title: Catenary Curves and Arches
### Aliases: geom_catenary geom_arch stat_catenary stat_arch

### ** Examples

library(ggplot2)

df <- data.frame(x = seq_len(4), y = c(1, 1, 0, 2))

# basic usage
p <- ggplot(df, aes(x, y)) + ylim(-3, NA) + geom_point(size = 3)
p + geom_catenary()

# Catenary with sag = 2, considered from lowest point of each segment
# recycled, if only a one value is provided
p + geom_catenary(sag = 2)
p + geom_catenary(sag = c(2, 1, 1))

# if sag and chain_length are provided for same segment(s), sag wins
p + geom_catenary(sag = c(2, 1, NA), chain_length = 10)

# Arch with height = 2, considered from highest point of each segment
p + geom_arch(arch_height = c(2, 1, 1))

# Rice house, see https://en.wikipedia.org/wiki/Rice_House,_Eltham
rice_house <- data.frame(x = c(0, 1.5, 2.5, 3.5, 5), y = c(0, 1, 1, 1, 0))
ggplot(rice_house, aes(x, y)) +
  geom_arch(arch_height = .15, lwd = 2) +
  geom_segment(aes(xend = x, yend = 0)) +
  geom_hline(yintercept = 0, colour = "forestgreen", linewidth = 3) +
  coord_equal()



cleanEx()
nameEx("geom_chaikin")
### * geom_chaikin

flush(stderr()); flush(stdout())

### Name: geom_chaikin
### Title: Apply Chaikin's corner cutting algorithm to smooth a path
### Aliases: geom_chaikin stat_chaikin

### ** Examples

set.seed(42)
dat <- data.frame(
  x = seq.int(10),
  y = sample(15:30, 10)
)

p1 <- ggplot(dat, aes(x, y)) +
  geom_line(linetype = "12")

p1 +
  geom_chaikin()

p1 +
  geom_chaikin(iterations = 1)

triangle <- data.frame(x = c(0, 0, 1), y = c(0, 1, 1))
p2 <- ggplot(triangle, aes(x, y)) +
  geom_path(linetype = "12") +
  coord_equal()

# ratio lets you control the cutting amount
p2 + geom_chaikin(ratio = .1)
p2 + geom_chaikin(ratio = .5)

# mode controls whether the result is an open or closed shape
p2 + geom_chaikin(mode = "open")   # default
p2 + geom_chaikin(mode = "closed")




cleanEx()
nameEx("geom_col_fade")
### * geom_col_fade

flush(stderr()); flush(stdout())

### Name: geom_col_fade
### Title: Bar Charts with Fading Gradient and Rounded Corners
### Aliases: geom_col_fade geom_bar_fade

### ** Examples

library(ggplot2)

df <- data.frame(
  x = c("A", "B", "C", "D", "E"),
  y = c(3, 4, -2, -0.5, 1)
)

ggplot(df, aes(x, y)) +
  geom_col_fade() +
  theme_minimal()

# if you do not want the corners to be round, set the radius to 0
# default radius is 3 "pt"
ggplot(df, aes(x, y)) +
  geom_col_fade(radius = 0) +
  theme_minimal()

# Global alpha scope: shorter bars appear more transparent
ggplot(df, aes(x, y)) +
  geom_col_fade(
    alpha_scope = "global",
    radius = unit(10, "pt")) +
  theme_minimal()

# Start at 75% opacity and keep some opacity at the baseline
ggplot(df, aes(x, y)) +
  geom_col_fade(
    alpha = 0.75,
    alpha_fade_to = 0.25
  ) +
  theme_minimal()

# Horizontal bars are supported
ggplot(df, aes(y, x)) +
  geom_col_fade() +
  theme_minimal()

library(ggplot2)

# multiple groups
p <- ggplot(diamonds, aes(color, fill = cut)) +
  scale_fill_viridis_d(guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal()

# dodged bar chart - by default each bar has their own alpha scope
p + geom_bar_fade(position = "dodge")

# when bars are dodged, all bars within the same fill / colour group
# can share the same alpha scope
p + geom_bar_fade(position = "dodge", alpha_scope = "group")

# when you want all bars to share a common scope, set the
# alpha scope to be 'global'
p + geom_bar_fade(position = "dodge", alpha_scope = "global")

# stacked bar chart - by default each bar has their own alpha scope
p + geom_bar_fade()

# for stacked bar charts each stack uses their alpha range
# independently when alpha_scope = "group"
p + geom_bar_fade(alpha_scope = "group")

# the alpha_scope = "global" option starts at fully opacity at maximum
# y-value and scales all other bars accordingly
p + geom_bar_fade(alpha_scope = "global")

# coord_polar() / coord_radial() — the linear bar fade becomes a
# panel-centered radial (annular) fade. Each ring fades from
# transparent at its inner radius to opaque at its outer radius.
# Rounded corners still require a linear coord system and are
# silently dropped under polar.
ggplot(mpg, aes(x = factor(1), fill = class)) +
  geom_bar_fade(width = 1) +
  coord_polar(theta = "y") +
  theme_void()

# Bars radiating outward from the panel centre
ggplot(mpg, aes(x = class, fill = drv)) +
  geom_bar_fade() +
  coord_radial(theta = "x") +
  theme_void()




cleanEx()
nameEx("geom_density_fade")
### * geom_density_fade

flush(stderr()); flush(stdout())

### Name: geom_density_fade
### Title: Smoothed Density Estimate with Fading Gradient
### Aliases: geom_density_fade

### ** Examples

library(ggplot2)

ggplot(diamonds, aes(carat)) +
  geom_density_fade()

# Map the values to y to flip the orientation
ggplot(diamonds, aes(y = carat)) +
  geom_density_fade()

ggplot(diamonds, aes(carat)) +
  geom_density_fade(adjust = 1/5)
ggplot(diamonds, aes(carat)) +
  geom_density_fade(adjust = 5)

ggplot(diamonds, aes(depth, colour = cut)) +
  geom_density_fade() +
  xlim(55, 70)
ggplot(diamonds, aes(depth, fill = cut, colour = cut)) +
  geom_density_fade(alpha = 0.1) +
  xlim(55, 70)

# Use `bounds` to adjust computation for known data limits
big_diamonds <- diamonds[diamonds$carat >= 1, ]
ggplot(big_diamonds, aes(carat)) +
  geom_density_fade(color = 'red') +
  geom_density_fade(bounds = c(1, Inf), color = 'blue')




cleanEx()
nameEx("geom_fourier")
### * geom_fourier

flush(stderr()); flush(stdout())

### Name: geom_fourier
### Title: Fourier Series Smoothing
### Aliases: geom_fourier stat_fourier

### ** Examples

library(ggplot2)

n <- 50
df1 <- data.frame(
  x = seq(0, 1, length.out = n),
  y = sin(seq(0, 2 * pi, length.out = n)) + rnorm(n, sd = 0.2)
)

# Basic usage – Interpolating fit (all harmonics)
p <- ggplot(df1, aes(x, y)) +
  geom_point(alpha = 0.5)
p + geom_fourier()

# Use 1 harmonic only
p + geom_fourier(n_harmonics = 1)

# De-trending a linearly drifting signal
set.seed(2)
x <- seq(0, 4 * pi, length.out = n)
df2 <- data.frame(
  x = x,
  y = sin(x) + x * 0.3 + rnorm(n, sd = 0.15)
)

ggplot(df2, aes(x, y))  +
geom_point(alpha = 0.35) +
  geom_fourier(aes(colour = "detrend = NULL"), n_harmonics = 3) +
  geom_fourier(aes(colour = "detrend = \"lm\""), n_harmonics = 3,
               detrend = "lm")

# Multiple groups
set.seed(3)
x <- seq(0, 2 * pi, length.out = n/2)
df3 <- rbind(
  data.frame(x = x,
             y = sin(x) + rnorm(n/2, sd = 0.2),
             grp = "sine"),
  data.frame(x = x,
             y = cos(x) + rnorm(n/2, sd = 0.2),
             grp = "cosine")
)

ggplot(df3, aes(x, y, colour = grp)) +
  geom_point(alpha = 0.5) +
  geom_fourier()

# when the data is not uniformly-spaced, the Fourier
# curve will not hit every data point exactly
ggplot(head(economics, 25), aes(date, unemploy)) +
  geom_fourier() +
  geom_point()  +
  geom_curve_fade(
    data = data.frame(
      x    = as.Date("1967-10-01"),
      xend = as.Date("1968-01-01"),
      y    = 2750,
      yend = 2850
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(),
    colour = "tomato"
    )

# ... in extreme cases a warning is emitted
df4 <- data.frame(
  x = c(1:10, 19:20),
  y = sin(seq_len(12))
)

ggplot(df4, aes(x, y)) +
  geom_point() +
  geom_fourier()




cleanEx()
nameEx("geom_freqpoly_fade")
### * geom_freqpoly_fade

flush(stderr()); flush(stdout())

### Name: geom_freqpoly_fade
### Title: Frequency Polygons with Fading Gradient
### Aliases: geom_freqpoly_fade

### ** Examples

library(ggplot2)

# Basic frequency polygon with fading gradient
ggplot(faithful, aes(waiting)) +
  geom_freqpoly_fade(
    fill = "#3b528b",
    colour = "#3b528b",
    bins = 20
  ) +
  theme_minimal()

# Rather than stacking histograms, compare frequency polygons
ggplot(iris, aes(Sepal.Length, fill = Species, colour = Species)) +
  geom_freqpoly_fade(
    alpha = 0.8,
    position = "identity",
    bins = 20
  ) +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  theme_minimal()




cleanEx()
nameEx("geom_gridline")
### * geom_gridline

flush(stderr()); flush(stdout())

### Name: geom_gridline
### Title: Lines Drawn on Top of Other Layers
### Aliases: geom_gridline

### ** Examples

library(ggplot2)

# Basic example - geom_gridline() is just another layer
# plotted in the order you add them to your ggplot
p <- ggplot(mpg, aes(class)) +
  geom_bar()
p + geom_gridline()

# Horizontal bars: flip axes, draw gridlines atop x-grid
ggplot(mpg, aes(y = class)) +
  geom_bar() +
  geom_gridline(grids = "x")

# Line properties are inherited from theme
# their positions from the scale
p +
  geom_gridline() +
  scale_y_continuous(breaks = c(10, 20)) +
  theme_gray(paper = "cornsilk", ink = "navy")

# When you explicitly set properties in geom_gridline
# they will overwrite theme properties
p +
  geom_gridline(colour = "tomato", major = FALSE, minor = TRUE) +
  scale_y_continuous(breaks = c(10, 20)) +
  theme_gray(paper = "cornsilk", ink = "navy")

# polar coordinates are supported too
ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
  geom_bar(width = 1) +
  geom_gridline(grids = c("x", "y"), minor = TRUE) +
  coord_polar(theta = "y")




cleanEx()
nameEx("geom_histogram_fade")
### * geom_histogram_fade

flush(stderr()); flush(stdout())

### Name: geom_histogram_fade
### Title: Histograms and Frequency Polygons with Fading Gradient
### Aliases: geom_histogram_fade

### ** Examples


# by default each bar has its own alpha scope
ggplot(faithful, aes(waiting)) +
  geom_histogram_fade(
    fill = "#ff005e",
    alpha_scope = "bar" # default
  ) +
  theme_minimal()

# Stacked histogram with groups
# scale alpha value globally
ggplot(iris, aes(Sepal.Length, fill = Species)) +
  geom_histogram_fade(
    alpha_scope = "global",
    radius = unit(5, "pt"),
    colour = NA,
    bins = 25
  ) +
  scale_fill_viridis_d() +
  theme_minimal()




cleanEx()
nameEx("geom_lexis")
### * geom_lexis

flush(stderr()); flush(stdout())

### Name: geom_lexis
### Title: Lexis diagrams
### Aliases: geom_lexis stat_lexis

### ** Examples

df1 <- data.frame(
  key = c("A", "B", "B", "C", "D", "E"),
  start = c(0, 1, 6, 5, 6, 9),
  end = c(5, 4, 10, 9, 8, 11)
)
p <- ggplot(df1, aes(x = start, xend = end, color = key))
p +
  geom_lexis()
p +
  geom_lexis(gap_filler = FALSE)
p +
  geom_lexis(aes(linetype = after_stat(type)),
    point_show = FALSE
  )

# change point appearance
p + geom_lexis(
  point_colour = "black",
  size = 3,
  shape = 21,
  fill = "white",
  stroke = 1
)

# missing values will be removed
df2 <- data.frame(
  key = c("A", "B", "B", "C", "D"),
  start = c(0, 1, 7, 5, 6),
  end = c(5, 4, 13, 9, NA)
)
ggplot(df2, aes(x = start, xend = end, color = key)) +
  geom_lexis()

# Ideally, `x` values should be increasing, unlike
# in the next example
df3 <- data.frame(x = Sys.Date() - 0:2, xend = Sys.Date() + 1:3)
ggplot(df3, aes(x = x, xend = xend)) +
  geom_lexis()




cleanEx()
nameEx("geom_path_fade")
### * geom_path_fade

flush(stderr()); flush(stdout())

### Name: geom_path_fade
### Title: Paths and Lines with a Fading Gradient
### Aliases: geom_path_fade geom_line_fade geom_step_fade

### ** Examples

library(ggplot2)

# Path that doubles back — fade follows the drawing order
theta <- seq(1.3, -1.3, length.out = 101)
df_ichthys <- data.frame(
  x = theta^2,
  y = 0.5 * theta * (theta^2 - 1)
)

p <- ggplot(df_ichthys, aes(x, y)) +
  geom_pointless(
    location = c("first", "last"),
    aes(colour = after_stat(location)),
    size = 4
  ) +
  coord_fixed() +
  theme_minimal()

p + geom_path_fade(
    linewidth = 1.5,
    fade_direction = "start" # default
  )

p + geom_path_fade(
    linewidth = 1.5,
    fade_direction = c("start", "end")
  )

# With few thick segments the default `"auto"` picks `"gradient"` for
# you, because at n <= 50 the smoother within-segment fade matters more
# than the (negligible) extra compute time.
df_thick <- data.frame(
  x = c(0, 1, 1.5, 1, 0),
  y = c(0, 0.5, 1, 1.5, 1)
)

p <- ggplot(df_thick, aes(x, y)) +
  coord_equal() +
  theme_minimal()

# auto → gradient (n = 5, well below the 50-vertex threshold)
p + geom_path_fade(
  linewidth = 8,
  colour = "#e63946"
  )

# Force `"step"` to see the per-segment stepping for comparison.
p + geom_path_fade(
  linewidth = 8,
  colour = "#e63946",
  alpha_mode = "step"
  )

# Explicit `"gradient"`, in this example, does the same thing
# `"auto"` picked above; for large n (> 200) this gets slow with
# not much gain visually.
p + geom_path_fade(
  linewidth = 8,
  colour = "#e63946",
  alpha_mode = "gradient"
  )

# using stat_function
ggplot() +
  stat_function(
    alpha = 0.5,
    fun = dnorm,
    n = 100,
    xlim = c(-4, 4),
    geom = "area_fade",
    outline.type = "none" # remove solid outline
  ) +
  # add fading outline instead
  stat_function(
    fun = dnorm, n = 100,
    xlim = c(-4, 4),
    geom = "path_fade",
    fade_direction = c("start", "end")
  )


ggplot(economics, aes(date, unemploy)) + geom_line_fade()

# NA values split the path into sub-paths — just like geom_line().
# The fade is computed over the concatenated arc length of all visible
# pieces, so the alpha just before a gap equals the alpha just after,
# as if the path were "pulled apart" at the NA.
df <- data.frame(x = 1:5, y = c(1, 2, NA, 4, 5))

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_line_fade(alpha_mode = "gradient", linewidth = 2)


# Fading step function
set.seed(42)
d <- data.frame(
  x   = rep(1:10, 2),
  y   = c(cumsum(rnorm(10)), cumsum(rnorm(10))),
  grp = rep(c("a", "b"), each = 10)
)

ggplot(d, aes(x, y, colour = grp)) +
  geom_step_fade(linewidth = 1, direction = "vh") +
  theme_minimal()




cleanEx()
nameEx("geom_point_glow")
### * geom_point_glow

flush(stderr()); flush(stdout())

### Name: geom_point_glow
### Title: Points that Glow
### Aliases: geom_point_glow

### ** Examples

library(ggplot2)

# Basic usage — the default glow is 9× the point's `size` aesthetic,
# so it's always visibly larger than the point itself.
ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point_glow()

# Customising the glow (fixed values, applied to every point)
ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point_glow(glow_colour = "#333", glow_alpha = 0.25, glow_size = 5) +
  theme_minimal()

# Pitfall: glow_size is in the same units as `size`, and the default
# point `size` is 1.5. If glow_size <= 1.5 the halo is covered by the
# point itself — the gradient is drawn but invisible underneath.
ggplot(mtcars, aes(wt, mpg)) +
  geom_point_glow(glow_size = 1)   # ← glow < point size, no halo shows

# Either shrink the point or grow the glow so the halo extends past it:
ggplot(mtcars, aes(wt, mpg)) +
  geom_point_glow(size = 0.5, glow_size = 1)   # shrink the point, or
ggplot(mtcars, aes(wt, mpg)) +
  geom_point_glow(glow_size = 4)               # grow the glow

# Per-point glow (scalar or length matching nrow(data)): the vector is
# aligned alongside the data, so any NA rows dropped by ggplot2 pull
# their glow value with them.
ggplot(mtcars, aes(wt, mpg)) +
  geom_point_glow(glow_colour = rainbow(nrow(mtcars)), glow_size = 5)

# use the Geom with another Stat
ggplot(head(economics), aes(date, uempmed)) +
  geom_line() +
  stat_pointless(
    geom = "PointGlow",
    glow_colour = "tomato",
    glow_size = 10,
    location = c("first", "last")
)



cleanEx()
nameEx("geom_pointless")
### * geom_pointless

flush(stderr()); flush(stdout())

### Name: geom_pointless
### Title: Emphasize some observations with points
### Aliases: geom_pointless stat_pointless

### ** Examples

x <- seq(-pi, pi, length.out = 150)
y <- outer(x, 1:5, FUN = \(x, y) sin(x * y))

df1 <- data.frame(
  x = x,
  y = rowSums(y)
)

# not terribly useful on its own ...
p <- ggplot(df1, aes(x = x, y = y))
p + geom_pointless()
p + geom_pointless(location = "all")

# ... but in conjunction with geom_line(), hopefully
p <- p + geom_line()
p + geom_pointless(location = "all")
p + geom_pointless(location = c("first", "last"))
p + geom_pointless(location = c("minimum", "maximum"))

# The layer computes one additional variable, 'location',
# that you can map e.g. to colour
p + geom_pointless(
  aes(colour = after_stat(location)),
  location = "all",
  size = 3
)

# Example with missing first and last observations
set.seed(42)
df2 <- data.frame(x = 1:10, y = c(NA, sample(1:8), NA))
ggplot(df2, aes(x, y)) +
  geom_line() +
  geom_pointless(location = c("first", "last"))

# Change the order in which points are drawn when they overlap
df3 <- data.frame(x = 1:2, y = 1:2)

p <- ggplot(df3, aes(x = x, y = y)) +
  geom_path() +
  coord_equal()

# same as location = 'all'
p + geom_pointless(aes(colour = after_stat(location)),
  location = c("first", "last", "minimum", "maximum")
) +
  labs(subtitle = "same as location = 'all'")

# reversed custom order
p + geom_pointless(aes(colour = after_stat(location)),
  location = c("maximum", "minimum", "last", "first")
) +
  labs(subtitle = "custom order")

# same as location = 'all' again
p + geom_pointless(aes(colour = after_stat(location)),
  location = c("maximum", "minimum", "last", "first", "all")
) +
  labs(subtitle = "same as location = 'all' again")

# Use stat_pointless() with a geom other than "point"
set.seed(42)
df4 <- data.frame(x = 1:10, y = sample(1:10))
ggplot(df4, aes(x, y)) +
  geom_line() +
  geom_pointless(location = c("maximum", "minimum"), size = 3) +
  stat_pointless(
    aes(label = after_stat(y)),
    location = c("maximum", "minimum"),
    geom = "text",
    hjust = -1
  )

# Example using facets
# https://stackoverflow.com/q/29375169
p <- ggplot(economics_long, aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(vars(variable), ncol = 1, scales = "free_y")

p + geom_pointless(
  aes(colour = after_stat(location)),
  location = c("minimum", "maximum"),
  size = 2
  )




cleanEx()
nameEx("geom_rect_fade")
### * geom_rect_fade

flush(stderr()); flush(stdout())

### Name: geom_rect_fade
### Title: Rectangles with a Fading Gradient and Rounded Corners
### Aliases: geom_rect_fade

### ** Examples


# With geom_rect_fade() you can draw arbitrary rectangles
ggplot(head(economics, 25), aes(date, unemploy)) +
  geom_rect_fade(
    data = data.frame(
      xmin = as.Date("1968-07-01"),
      xmax = as.Date("1969-07-01"),
      ymin = -Inf, ymax = 2800
    ),
    inherit.aes = FALSE,
    alpha = 0,
    alpha_fade_to = 0.3,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  ) +
  stat_fourier(geom = "line_fade", fade_direction = "start", alpha_fade_to = 0.2) +
  geom_point(size = 3, alpha = 0.2) +
  theme_minimal()




cleanEx()
nameEx("geom_ridgeline_density_fade")
### * geom_ridgeline_density_fade

flush(stderr()); flush(stdout())

### Name: geom_ridgeline_density_fade
### Title: Density Ridgeline Plots with Fading Gradient
### Aliases: geom_ridgeline_density_fade

### ** Examples

# Density ridgelines — convenience wrapper for the stat_density example above
ggplot(iris, aes(
  x = Sepal.Length,
  y = as.integer(Species),
  group = Species,
  fill = after_stat(x)
)
) +
  geom_ridgeline_density_fade(scale = 2, alpha_scope = "area") +
  scale_fill_viridis_c(option = "C") +
  theme_minimal()



cleanEx()
nameEx("geom_ridgeline_fade")
### * geom_ridgeline_fade

flush(stderr()); flush(stdout())

### Name: geom_ridgeline_fade
### Title: Ridgeline Plots with Fading Linear Gradient
### Aliases: geom_ridgeline_fade

### ** Examples

library(ggplot2)

d <- data.frame(
  x = rep(1:5, 3) + c(rep(0, 5), rep(0.3, 5), rep(0.6, 5)),
  y = c(rep(0, 5), rep(1, 5), rep(3, 5)),
  height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1)
)

# Basic ridgeline
ggplot(d, aes(x, y, height = height, group = y, fill = factor(y))) +
  geom_ridgeline_fade() +
  scale_fill_viridis_d(direction = -1, guide = "none")

# Increase overlap with scale
ggplot(d, aes(x, y, height = height, group = y, fill = factor(y))) +
  geom_ridgeline_fade(scale = 2) +
  scale_fill_viridis_d(direction = -1, guide = "none")

# Global alpha scope: shorter ridges appear more transparent
ggplot(d, aes(x, y, height = height, group = y, fill = factor(y))) +
  geom_ridgeline_fade(alpha_scope = "global") +
  scale_fill_viridis_d(direction = -1, guide = "none")

# Keep some opacity at the baseline
ggplot(d, aes(x, y, height = height, group = y, fill = factor(y))) +
  geom_ridgeline_fade(alpha_fade_to = 0.3, scale = 1.5) +
  scale_fill_viridis_d(direction = -1, guide = "none")

# Aligning legend keys with the chart: ridges are drawn highest-y-first, so
# guide_legend(reverse = TRUE) puts the top-of-chart ridge at the top of
# the legend.
ggplot(d, aes(x, y, height = height, group = y, fill = factor(y))) +
  geom_ridgeline_fade() +
  scale_fill_viridis_d(direction = -1) +
  guides(fill = guide_legend(reverse = TRUE))

# Density ridgeline using stat = "density"
ggplot(iris, aes(Sepal.Length, y = as.numeric(Species),
                 group = Species, fill = Species)) +
  geom_ridgeline_fade(
    mapping = aes(height = after_stat(density)),
    stat = "density",
    scale = 3
  ) +
  scale_fill_viridis_d(option = "C") +
  scale_y_continuous(breaks = 1:3, labels = levels(iris$Species)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal()




cleanEx()
nameEx("geom_segment_fade")
### * geom_segment_fade

flush(stderr()); flush(stdout())

### Name: geom_segment_fade
### Title: Line Segments with a Fading Gradient
### Aliases: geom_segment_fade geom_curve_fade

### ** Examples

library(ggplot2)

b <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
b +
  geom_curve_fade(
    aes(x = x1, y = y1, xend = x2, yend = y2, colour = "curve"),
    data = df
  ) +
  geom_segment_fade(
    aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"),
    data = df
  )

b +
  geom_curve_fade(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    data = df,
    curvature = 1,
    fade_direction = "start",
    arrow = grid::arrow()
  )

df <- data.frame(x1 = 1, x2 = 9, y1 = 1, y2 = 1)
p <- ggplot(df, aes(x)) +
  theme_void()

# basic example with default fade_direction
p +
  geom_segment_fade(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    fade_direction = "start", # default
    linewidth = 10
  )

# change fade_direction towards start
p +
  geom_segment_fade(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    fade_direction = "end",
    linewidth = 10
  )

# fade from center to both sides
p +
  geom_segment_fade(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    fade_direction = c("start", "end"),
    linewidth = 10
  )




cleanEx()
nameEx("geom_unit_bar")
### * geom_unit_bar

flush(stderr()); flush(stdout())

### Name: geom_unit_bar
### Title: Unit Bar Charts
### Aliases: geom_unit_bar geom_unit_col geom_unit_histogram

### ** Examples

library(ggplot2)

# geom_unit_bar: count observations automatically (like geom_bar)
ggplot(mpg, aes(x = class, fill = drv)) +
  geom_unit_bar() +
  coord_equal()

# Horizontal bars via y aesthetic
ggplot(mpg, aes(y = class, fill = drv)) +
  geom_unit_bar()

# Dodged bars — cells shrink to width / n_groups under dodge
ggplot(mpg, aes(x = class, fill = drv)) +
  geom_unit_bar(position = "dodge") +
  coord_equal()

# Dodge + square cells: compensate via the coord_equal() ratio so each
# sub-bar cell renders as a square.  `mpg$drv` has 3 levels, so pass
# ratio = n_groups / width = 3 / 0.9:
ggplot(mpg, aes(x = class, fill = drv)) +
  geom_unit_bar(position = "dodge") +
  coord_equal(ratio = 3 / 0.9)

# Reversed stack
ggplot(mpg, aes(x = class, fill = drv)) +
  geom_unit_bar(position = position_stack(reverse = TRUE)) +
  coord_equal()

# Asymmetric `cell_padding` under `coord_flip()`.  The length-2 vector
# c(vertical, horizontal) is interpreted in the stat's canonical
# orientation, so coord_flip() swaps which gap is visually vertical vs.
# horizontal.  Here: `c(0.1, 0.005)` gives generous vertical cell-to-cell
# gaps (which become horizontal after the flip) and tight cell-to-edge
# spacing (which becomes vertical after the flip).
ggplot(mpg, aes(x = class, fill = drv)) +
  geom_unit_bar(width = 1, cell_padding = c(0.1, 0.005)) +
  coord_flip()

# Large data + coord_equal(): meet `cell_size` and `label_cells()`
# ------------------------------------------------------------------
# `coord_equal()` keeps cells visually square by forcing a 1:1 data-space
# aspect ratio.  That works on small data (mpg has ~7 categories and
# counts up to ~60, so x and y are the same order of magnitude) but
# breaks on large data.  `diamonds` has 53,940 rows and the tallest
# stack reaches ~2,600, against an x-range (carat) of only ~5.
#
# 1. What it looks like by default — almost empty:
ggplot(diamonds, aes(x = carat, fill = cut)) +
  geom_unit_bar() +
  coord_equal()
# cell_count_cap (default 10,000) fires first and falls back to solid bars,
# but even solid bars are ~1px-wide slivers once coord_equal squeezes
# a 2,600-tall y-axis alongside a 5-wide x-axis.

# 2. Fix the scale mismatch with `cell_size`.  Each cell now represents
#    500 observations, so the y-range collapses from ~2,600 to ~5 — now
#    comparable to the x-range:
ggplot(diamonds, aes(x = carat, fill = cut)) +
  geom_unit_bar(cell_size = 500) +
  coord_equal()

# 3. Relabel the axis in cell counts with `label_cells()` so readers
#    can see "2 cells" rather than "1000 diamonds":
ggplot(diamonds, aes(x = carat, fill = cut)) +
  geom_unit_bar(cell_size = 500) +
  scale_y_continuous(labels = label_cells(500)) +
  coord_equal() +
  labs(y = "Diamonds (1 cell = 500)")

# `cell_count_cap` remains the defensive seatbelt: even with `cell_size` set,
# it catches pathological inputs (e.g. an extra zero in `cell_size`) so
# the graphics device never drowns in rects.

# geom_unit_col: pre-computed counts in y (like geom_col)
ep_data <- data.frame(
  episode = factor(
    rep(paste0("Ep ", 1:5), each = 2),
    levels = paste0("Ep ", 5:1)
  ),
  gender  = factor(rep(c("Female", "Male"), 5)),
  minutes = c(8, 12, 15, 5, 6, 14, 10, 10, 4, 16)
)

ggplot(ep_data, aes(x = episode, y = minutes, fill = gender)) +
  geom_unit_col() +
  coord_equal()

# Flat cells with rounded corners via coord_equal(ratio)
ggplot(ep_data, aes(x = episode, y = minutes, fill = gender)) +
  geom_unit_col(radius = grid::unit(3, "pt")) +
  coord_equal(ratio = 1/4)

# Horizontal bars via orientation = "y" (value on x)
ggplot(data.frame(x = 1:5, y = c(2, 4, 3, 5, 1)), aes(x, y)) +
  geom_unit_col(orientation = "y") +
  coord_equal()

# use stat = "bin" to create a histogram
ggplot(mpg, aes(x = displ)) +
  geom_unit_bar(stat = "bin")


# geom_unit_histogram: tiled histogram for continuous variables
ggplot(mpg, aes(x = displ)) +
  geom_unit_histogram(bins = 10) +
  coord_equal()

# Colour by a second variable; stat = "bin" also works directly
ggplot(mpg, aes(x = hwy, fill = drv)) +
  geom_unit_histogram(bins = 15) +
  coord_equal()



cleanEx()
nameEx("label_cells")
### * label_cells

flush(stderr()); flush(stdout())

### Name: label_cells
### Title: Axis labeller for unit-cell charts
### Aliases: label_cells

### ** Examples

library(ggplot2)
df <- data.frame(country = c("A", "B", "C"), pop = c(2.4e6, 1.1e6, 3.8e6))
ggplot(df, aes(country, pop)) +
  geom_unit_col(cell_size = 1e6) +
  scale_y_continuous(labels = label_cells(1e6)) +
  labs(y = "People (millions; one cell = 1e6)")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
