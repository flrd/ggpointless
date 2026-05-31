# Examples

This article walks through every layer function in `ggpointless`. The
package groups its layers into two broad categories:

- **Data transformations** — geoms backed by a stat that fits or
  transforms data.
- **Visual effects** — drop-in replacements for `ggplot2` layers with
  extra visual flair: alpha gradients, glows, fading reference lines.
  These do not add statistical meaning; they re-render the same data but
  give you some additional features to change the appearance of your
  data.

> **Note on devices**: Most R graphics devices ship with gradient
> support (ragg, Cairo-backed PNG on Linux/macOS, SVG). The notable
> exception is
> [`grDevices::pdf()`](https://rdrr.io/r/grDevices/pdf.html), on which
> the fade family falls back to flat semi-transparent fills.

## Setup

``` r

library(ggpointless)
#> Loading required package: ggplot2

# Brand palette -- kept in sync with the README
cols <- c("#311dfc", "#a84dbd", "#d77e7b", "#f4ae1b")
theme_set(
  theme_minimal() +
    theme(
      legend.position           = "bottom",
      geom                      = element_geom(fill = cols[1]),
      palette.fill.discrete     = cols,
      palette.colour.discrete   = cols,
      palette.fill.continuous   = cols,
      palette.colour.continuous = cols
    )
)
```

## Statistical transformations

## geom_arch() and geom_catenary()

A **catenary** is the curve formed by a flexible chain or cable hanging
freely between two fixed supports. It follows this equation:

\\y = a \cosh\\\left(\frac{x}{a}\right)\\

[`geom_catenary()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
draws a hanging-chain catenary curve between successive points;
[`geom_arch()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
draws a catenary arch, i.e. the inverted curve.

### geom_arch

The shape is controlled by `arch_length` or `arch_height` (vertical rise
above the highest endpoint of each segment). By default the arc length
is twice the Euclidean distance calculated for each segment.

``` r

ggplot(data.frame(x = 1:2, y = c(0, 0)), aes(x, y)) +
  geom_arch(arch_height = 0.6)
```

![Single arched curve rising between two points on a flat
baseline.](ggpointless_files/figure-html/arch-basic-1.png)

[`stat_arch()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
exposes the underlying computation. Here it shows the effect of
different `arch_height` values between the same two endpoints:

``` r

ggplot(data.frame(x = c(0, 2), y = c(0, 0)), aes(x, y)) +
  lapply(c(0.5, 1.5, 3), \(ah) {
    stat_arch(arch_height = ah, aes(colour = paste0("arch_height = ", ah)))
  }) +
  labs(colour = NULL)
```

![Three arches of increasing height drawn between the same two
endpoints, distinguished by
colour.](ggpointless_files/figure-html/stat-arch-compare-1.png)

A real-world application is the **[Rice
House](https://en.wikipedia.org/wiki/Rice_House,_Eltham)** in Eltham,
whose distinctive roofline consists of shallow catenary arched spans
rising above vertical columns. The sketch below reconstructs its
simplified cross-section:

``` r

rice_house <- data.frame(
  x = c(0, 1.5, 2.5, 3.5, 5),
  y = c(0, 1, 1, 1, 0)
)

ggplot(rice_house, aes(x, y)) +
  geom_arch(arch_height = 0.15, linewidth = 2, colour = "#333333") +
  geom_segment(aes(xend = x, yend = 0), colour = "#333333") +
  geom_hline(yintercept = 0, colour = "#4a7c59", linewidth = 3) +
  coord_equal() +
  theme_void() +
  labs(caption = "Rice House, Eltham (simplified catenary cross-section)")
```

![Simplified cross-section of the Rice House roof showing three shallow
catenary arches resting on vertical columns above a green ground
line.](ggpointless_files/figure-html/rice-house-1.png)

### geom_catenary

[`geom_catenary()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
draws the hanging-chain form between successive points.

``` r

set.seed(1)
ggplot(data.frame(x = 1:6, y = sample(6)), aes(x, y)) +
  geom_catenary() +
  geom_point(size = 3, colour = "#333333")
```

![Hanging-chain catenary curves connecting six scattered points, with
the points marked in dark
grey.](ggpointless_files/figure-html/catenary-basic-1.png)

By default the chain length is twice the Euclidean distance per segment.
Pass `chain_length` to set an explicit arc length — longer values make
the chain sag more. If the value is shorter than the straight-line
distance a warning is issued and a straight line is drawn instead.

``` r

ggplot(data.frame(x = c(0, 1), y = c(1, 1)), aes(x, y)) +
  lapply(c(1.5, 1.75, 2), \(cl) {
    geom_catenary(chain_length = cl)
  }) +
  ylim(0, 1.05)
```

![Three catenary curves between the same two points, sagging
progressively more as the chain length
increases.](ggpointless_files/figure-html/catenary-chain-length-1.png)

`sag` sets the vertical drop below the lowest endpoint of each segment,
giving you direct control over how much the chain droops:

``` r

df_sag <- data.frame(x = c(0, 2, 4, 6), y = c(1, 1, 1, 1))

ggplot(df_sag, aes(x, y)) +
  geom_catenary(sag = c(0.1, 0.5, 1.2)) +
  geom_point(size = 3, colour = "#333333")
```

![Three catenary curves linking four points at the same height, each
with a different vertical
sag.](ggpointless_files/figure-html/catenary-sag-1.png)

`chain_length` and `sag` can be mixed per segment by placing `NA` in the
position where the other argument should win. When both are non-`NA` for
the same segment, `sag` takes precedence.

``` r

df_sag <- data.frame(x = c(0, 2, 4, 6), y = c(1, 1, 1, 1))

ggplot(df_sag, aes(x, y)) +
  geom_catenary(
    chain_length = c(2, NA, 3),
    sag = c(0.1, 0.5, NA)
  ) +
  geom_point(size = 3, colour = "#333333") +
  ylim(c(-.25, 1))
#> Both `sag` and `chain_length` supplied for 1 segment; using `sag`.
#> This message is displayed once every 8 hours.
```

![Three catenary curves between four equally-spaced points, mixing
chain-length and sag arguments per
segment.](ggpointless_files/figure-html/catenary-sag-vs-chain-length-1.png)

## geom_chaikin

Chaikin’s corner-cutting algorithm smooths a polygonal path by
iteratively replacing each corner with two new points placed at a given
ratio along the adjacent edges. With `ratio = 0.25` (the default) and
`iterations = 5` the path converges to a smooth B-spline approximation.

``` r

set.seed(42)
dat <- data.frame(x = seq.int(10), y = sample(15:30, 10))

ggplot(dat, aes(x, y)) +
  geom_line(linetype = "dashed", colour = "#333333") +
  geom_chaikin(colour = cols[1])
```

![A jagged dashed line of ten random points with a smoothed Chaikin
curve following the same
trajectory.](ggpointless_files/figure-html/chaikin-basic-1.png)

`ratio` controls how aggressively corners are cut. At `ratio = 0.5` the
new points coincide with the edge midpoints, producing the maximum
rounding possible in a single pass.

``` r

triangle <- data.frame(x = c(0, 0.5, 1), y = c(0, 1, 0))

ggplot(triangle, aes(x, y)) +
  geom_polygon(fill = NA, colour = "grey70", linetype = "dashed") +
  lapply(c(0.1, 0.25, 0.5), \(r) {
    geom_chaikin(ratio = r, mode = "closed", aes(colour = paste0("ratio = ", r)))
  }) +
  labs(colour = NULL) +
  coord_equal()
```

![A dashed triangle outline with three Chaikin-smoothed closed curves
inside, showing how larger ratios round the corners more
aggressively.](ggpointless_files/figure-html/chaikin-ratio-1.png)

### Effect of iterations

Each iteration halves the sharpness of every corner. The animation below
steps through iterations = 0, 1, 2, 3, 5, and 10, applied to a
five-pointed star.

![Animation of a five-pointed star being progressively rounded by
successive Chaikin iterations.](figures/chaikin_iterations.gif)

The source script that generates the animation is
`inst/scripts/gen_chaikin_gif.R`.

## geom_fourier

[`geom_fourier()`](https://flrd.github.io/ggpointless/dev/reference/geom_fourier.md)
fits a Fourier series (via
[fft()](https://search.r-project.org/R/refmans/stats/html/fft.html)) to
the supplied `x`/`y` observations and renders the reconstructed smooth
curve. By default all harmonics up to the Nyquist limit are used, giving
an exact interpolating fit.

![Animation of a Fourier reconstruction of a square wave, accumulating
harmonics one at a time.](figures/fourier_square_wave.gif)

> The source script that generates the animation is at
> `inst/scripts/gen_fourier_gif.R`.

Reducing `n_harmonics` progressively smooths the result. Fewer harmonics
produce a smoother, low-frequency summary; retaining all harmonics
reproduces the original signal exactly (up to interpolation artefacts).

``` r

set.seed(42)
n <- 150
df_f <- data.frame(
  x = seq(0, 2 * pi, length.out = n),
  y = sin(seq(0, 2 * pi, length.out = n)) +
    0.4 * sin(3 * seq(0, 2 * pi, length.out = n)) +
    rnorm(n, sd = 0.25)
)

ggplot(df_f, aes(x, y)) +
  geom_point(alpha = 0.25) +
  lapply(c(1, 3), \(nh) {
    geom_fourier(aes(colour = paste0("n_harmonics = ", nh)), n_harmonics = nh)
  }) +
  labs(colour = NULL)
```

![Scattered noisy points fitted by two Fourier reconstructions, one
using a single harmonic and a smoother one using three
harmonics.](ggpointless_files/figure-html/fourier-harmonics-1.png)

### Detrending

A linear drift in the data will dominate the low-frequency coefficients,
preventing the Fourier series from capturing the periodic structure. Set
`detrend = "lm"` (or `"loess"`) to subtract the trend before the
transform; it is added back afterwards so the output remains on the
original scale.

``` r

set.seed(3)
x_d <- seq(0, 4 * pi, length.out = 100)
df_d <- data.frame(
  x = x_d,
  y = sin(x_d) + x_d * 0.4 + rnorm(100, sd = 0.2)
)

ggplot(df_d, aes(x, y)) +
  geom_point(alpha = 0.35) +
  geom_fourier(aes(colour = "detrend = NULL"),
    n_harmonics = 3
  ) +
  geom_fourier(
    aes(colour = "detrend = \"lm\""),
    n_harmonics = 3,
    detrend = "lm") +
  labs(
    x = NULL, y = NULL
  )
```

![Upward-drifting noisy points compared with two Fourier fits, one
without detrending that misses the periodic structure and one with
linear detrending that captures the oscillation atop the rising
trend.](ggpointless_files/figure-html/fourier-detrend-1.png)

## geom_gridline

[`geom_gridline()`](https://flrd.github.io/ggpointless/dev/reference/geom_gridline.md)
draws panel grid lines as a regular layer, so they can sit *on top* of
other layers instead of disappearing behind them. Line styling falls
through from `theme(panel.grid.*)` for any property you don’t explicitly
override. To remove the underlying theme grid (so the layer’s lines
don’t stack with `ggplot2`’s own grid) add
`+ theme(panel.grid = element_blank())`.

``` r

ggplot(mpg, aes(class)) +
  geom_bar() +
  geom_gridline()
```

![Bar chart of vehicle counts by class with horizontal gridlines drawn
on top of the bars.](ggpointless_files/figure-html/gridline-basic-1.png)

### Picking axes

Use `grids = "x"`, `"y"`, or `c("x", "y")` to choose which axis to draw,
and `lines = c('major', 'minor')` to draw minor (grid-)lines too.
Positions come from the trained scale — no manual breaks needed.

``` r

ggplot(mpg, aes(y = class)) +
  geom_bar() +
  geom_gridline(grids = "x", lines = c("major", "minor")) +
  scale_x_log10()
```

![Horizontal bar chart of vehicle counts on a log-scaled x-axis with
major and minor vertical gridlines drawn on
top.](ggpointless_files/figure-html/gridline-axes-1.png)

But you can of course add breaks whenever you want and
[`geom_gridline()`](https://flrd.github.io/ggpointless/dev/reference/geom_gridline.md)
picks these values up automatically.

``` r

ggplot(mpg, aes(y = class)) +
  geom_bar() +
  geom_gridline(
    grids = "x",
    linewidth = 2,
    colour = cols[4]
    ) +
  scale_x_log10(breaks = c(5, 10, 20))
```

![Horizontal bar chart on a log-scaled x-axis with thick gold
custom-positioned gridlines at the breaks 5, 10 and
20.](ggpointless_files/figure-html/gridline-axes-log10-1.png)

### Polar coordinates

[`geom_gridline()`](https://flrd.github.io/ggpointless/dev/reference/geom_gridline.md)
works under
[`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
and
[`coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
too.

``` r

ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
  geom_bar(width = 1) +
  geom_gridline(grids = c("x", "y"), lines = c("major", "minor")) +
  coord_polar(theta = "y") +
  theme_void()
```

![Stacked polar bar chart of cylinder counts with radial and angular
gridlines overlaid.](ggpointless_files/figure-html/gridline-polar-1.png)

## geom_lexis

A **Lexis diagram** displays lifelines — the duration of individual
subjects from start to end — as 45° lines.

[`geom_lexis()`](https://flrd.github.io/ggpointless/dev/reference/geom_lexis.md)
draws a lifeline for each observation from its `x` (start) to its `xend`
(end). The `y` and `yend` aesthetics are calculated by
[`stat_lexis()`](https://flrd.github.io/ggpointless/dev/reference/geom_lexis.md)
and represent the cumulative duration.

### US Presidential Terms

Here is a lifeline diagram showing the terms of US presidents from
Eisenhower to Trump II, with data augmented from ggplot2’s
`presidential` dataset:

``` r

# Augment presidential data with Biden and Trump II
presidential_update <- data.frame(
  name = c("Biden", "Trump"),
  start = as.Date(c("2021-01-20", "2025-01-20")),
  end = c(as.Date("2025-01-20"), Sys.Date()),
  party = c("Democratic", "Republican")
)

df_lexis <- rbind(ggplot2::presidential, presidential_update)

# Combine names with first names for readability
first_names <- c(
  Eisenhower = "Dwight",
  Kennedy = "John",
  Johnson = "Lyndon",
  Nixon = "Richard",
  Ford = "Gerald",
  Carter = "Jimmy",
  Reagan = "Ronald",
  Bush = "George H. W.",
  Clinton = "Bill",
  Bush = "George W.",
  Obama = "Barack",
  Trump = "Donald",
  Biden = "Joe",
  Trump = "Donald"
)

df_lexis$name <- paste(first_names, df_lexis$name)

p <- ggplot(df_lexis, aes(x = start, xend = end, group = name, colour = party)) +
  scale_colour_manual(
    values = c("Democratic" = "#0044c9", "Republican" = "#DE0100")
  ) +
  scale_linetype_identity() +
  scale_y_continuous(
    breaks = c(2, 4, 6, 8) * 365.25,
    labels = \(d) floor(d / 365.25)
  ) +
  labs(
    title = "Terms of US Presidents: Eisenhower to Trump II",
    caption = sprintf("As of %s.", Sys.Date()),
    x = NULL,
    y = "Years in office"
  ) +
  coord_fixed()

p + geom_lexis(aes(linetype = after_stat(type)))
```

![Lexis diagram showing US presidential terms from Eisenhower to Trump
II as 45-degree lifelines, each coloured by party affiliation
(Democratic blue, Republican
red).](ggpointless_files/figure-html/lexis-presidential-1.png)

When there is a gap between two terms of the same president (as is the
case with Trump’s two non-consecutive terms), a horizontal dotted
segment bridges the gap. Set `gap_filler = FALSE` to hide these
gap-fillers and show only the lifelines themselves.

``` r

df_lexis_subset <- subset(df_lexis, grepl("Trump|Biden|Obama", name))

p + geom_lexis(aes(linetype = after_stat(type)), gap_filler = FALSE)
```

![The same presidential Lexis diagram without gap-filling segments,
showing Trump's two separate terms as two disconnected
lifelines.](ggpointless_files/figure-html/lexis-gap-1.png)

### Styling with after_stat(type)

The computed variable `type` takes the value `"solid"` for 45° lifelines
and `"dotted"` for horizontal gap-fillers. Map it to `linetype` to make
the distinction explicit. You can also style the endpoint dot
independently of the lifeline colour with `point_colour`:

``` r

df_demo <- data.frame(
  key  = c("A", "B", "B", "C", "D"),
  x    = c(0, 1, 6, 5, 6),
  xend = c(5, 4, 10, 8, 10)
)

ggplot(df_demo, aes(x = x, xend = xend, group = key, colour = key)) +
  stat_lexis(
    aes(linetype = after_stat(type)),
    point_colour = "#333333",
    shape        = 21,
    fill         = "white",
    size         = 2.5,
    stroke       = 0.8
  ) +
  scale_linetype_identity() +
  coord_equal()
```

![Lexis diagram styled with solid 45-degree lifelines, dotted horizontal
gap-fillers, and white-filled circular endpoint markers with a dark
outline.](ggpointless_files/figure-html/lexis-type-1.png)

## geom_pointless

[`geom_pointless()`](https://flrd.github.io/ggpointless/dev/reference/geom_pointless.md)
highlights selected observations — first, last, minimum, maximum, or all
of them — by default with points. Its name reflects that it is not
particularly useful on its own, but in conjunction with
[`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
and friends, it adds useful context at a glance.

``` r

x <- seq(-pi, pi, length.out = 100)
df1 <- data.frame(var1 = x, var2 = rowSums(outer(x, 1:5, \(x, y) sin(x * y))))

p <- ggplot(df1, aes(x = var1, y = var2)) +
  geom_line()

p + geom_pointless(location = "all", size = 3)
```

![Oscillating line plot with the first, last, minimum, and maximum
observations highlighted as large
points.](ggpointless_files/figure-html/pointless-basic-1.png)

Map the computed variable `location` to `colour` to distinguish the four
roles:

``` r

p +
  geom_pointless(
    aes(colour = after_stat(location)),
    location = "all",
    size = 3
  ) +
  theme(legend.position = "bottom")
```

![Same oscillating line with the first, last, minimum, and maximum
points each shown in a different colour and a legend identifying
them.](ggpointless_files/figure-html/pointless-colour-1.png)

### Order and orientation

Locations are determined in data order. This matters when e.g. the path
crosses itself:

``` r

x <- seq(5, -1, length.out = 1000) * pi
spiral <- data.frame(var1 = sin(x) * 1:1000, var2 = cos(x) * 1:1000)

p_spi <- ggplot(spiral) +
  geom_path() +
  coord_equal(xlim = c(-1000, 1000), ylim = c(-1000, 1000))

p_spi +
  aes(x = var1, y = var2) +
  geom_pointless(aes(colour = after_stat(location)), location = "all", size = 3) +
  labs(subtitle = "orientation = 'x'")

p_spi +
  aes(y = var1, x = var2) +
  geom_pointless(aes(colour = after_stat(location)), location = "all", size = 3) +
  labs(subtitle = "orientation = 'y'")
```

![Two spiral paths side by side, each with first, last, minimum, and
maximum landmarks coloured differently to show how orientation x versus
y changes which extremes are
picked.](ggpointless_files/figure-html/pointless-spiral-1.png)![Two
spiral paths side by side, each with first, last, minimum, and maximum
landmarks coloured differently to show how orientation x versus y
changes which extremes are
picked.](ggpointless_files/figure-html/pointless-spiral-2.png)

When `location = "all"`, points are drawn bottom to top in the order:
`"maximum"` \< `"minimum"` \< `"last"` \< `"first"`. Passing an explicit
vector lets you override this:

``` r

df2 <- data.frame(x = 1:2, y = 1:2)
p2 <- ggplot(df2, aes(x, y)) +
  geom_path() +
  coord_equal()

p2 + geom_pointless(aes(colour = after_stat(location)),
  location = c("first", "last", "minimum", "maximum"), size = 4
) +
  labs(subtitle = "first on top")

p2 + geom_pointless(aes(colour = after_stat(location)),
  location = c("maximum", "minimum", "last", "first"), size = 4
) +
  labs(subtitle = "maximum on top")
```

![Two short diagonal lines side by side with the same four landmark
points stacked in opposite orders, showing how the location vector
controls drawing
order.](ggpointless_files/figure-html/pointless-order-1.png)![Two short
diagonal lines side by side with the same four landmark points stacked
in opposite orders, showing how the location vector controls drawing
order.](ggpointless_files/figure-html/pointless-order-2.png)

[`geom_pointless()`](https://flrd.github.io/ggpointless/dev/reference/geom_pointless.md)
respects `ggplot2`’s group structure and works naturally with facets:

``` r

ggplot(
  subset(economics_long, variable %in% c("psavert", "unemploy")),
  aes(x = date, y = value)
) +
  geom_line(colour = "#333333") +
  geom_pointless(
    aes(colour = after_stat(location)),
    location = c("minimum", "maximum"),
    size = 3
  ) +
  stat_pointless(
    geom = "text",
    aes(label = after_stat(y)),
    location = c("minimum", "maximum"),
    hjust = -.55) +
  facet_wrap(vars(variable), ncol = 1, scales = "free_y") +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, colour = NULL)
```

![Two stacked time-series facets of US personal savings rate and
unemployment, with minima and maxima of each series marked by coloured
points and labelled with their
y-values.](ggpointless_files/figure-html/pointless-facets-1.png)

Here it adds horizontal reference lines at the minimum and maximum:

``` r

set.seed(42)
df3 <- data.frame(x = 1:10, y = sample(10))

ggplot(df3, aes(x, y)) +
  geom_line() +
  stat_pointless(
    aes(yintercept = y, colour = after_stat(location)),
    location = c("minimum", "maximum"),
    geom = "hline"
  ) +
  theme(legend.position = "bottom")
```

![Line plot of ten random points with coloured horizontal reference
lines drawn at the minimum and maximum
y-values.](ggpointless_files/figure-html/pointless-hline-1.png)

## Visual effects

## geom_area_fade

[`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md)
fills each group with a linear gradient that fades from the fill colour
to transparent at the baseline at `y = 0`.

``` r

ggplot(economics, aes(date, unemploy)) +
  geom_area_fade()
```

![Area chart of US unemployment over time, filled with a vertical
gradient that fades from solid at the data line down to transparent at
the baseline.](ggpointless_files/figure-html/area-fade-economics-1.png)

Use `alpha` to set the starting opacity and `alpha_fade_to` to control
at which alpha value the gradient ends.

``` r

ggplot(economics, aes(date, unemploy)) +
  geom_area_fade(alpha = 0.5, alpha_fade_to = 0.05)
```

![Unemployment area chart with a softer alpha range, semi-transparent at
the top and nearly invisible at the
baseline.](ggpointless_files/figure-html/area-fade-alpha-1.png)

The direction can be reversed — transparent at the top, opaque at the
baseline — by swapping their values. The outline colour is unaffected by
the alpha logic.

``` r

ggplot(economics, aes(date, unemploy)) +
  geom_area_fade(alpha = 0, alpha_fade_to = 1)
```

![Unemployment area chart with the fade direction reversed, transparent
at the top of the data line and opaque at the
baseline.](ggpointless_files/figure-html/geom-area-fade-reverse-1.png)

When `fill` is mapped to a variable inside
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html), `ggplot2`
builds a **horizontal** colour gradient across each group.
[`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md)
overlays its vertical alpha schedule on top, giving a true 2D gradient:
colour varies left-to-right while opacity fades from the data line down
to the baseline.

``` r

set.seed(42)
ggplot(economics, aes(date, unemploy)) +
  geom_area_fade(aes(fill = pop), outline.type = "none") +
  guides(fill = "none")
```

![Unemployment area chart with a two-dimensional gradient: colour varies
horizontally across the chart while opacity fades vertically from the
data line to the
baseline.](ggpointless_files/figure-html/area-fade-2d-1.png)

### Multiple groups

When your data contains multiple groups, those are stacked
(`position = "stack"`) and aligned (`stat = "align"`) – just like
[`geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html).
By default, the alpha fade scales to the global maximum across *all*
groups (`alpha_scope = "global"`), so equal `|y|` always maps to equal
opacity.

``` r

df1 <- data.frame(
  g = c("a", "a", "a", "b", "b", "b"),
  x = c(1, 3, 5, 2, 4, 6),
  y = c(2, 5, 1, 3, 6, 7)
)

ggplot(df1, aes(x, y, fill = g)) +
  geom_area_fade()
```

![Two stacked faded areas in different fill colours, where equal
y-magnitudes map to equal opacity across both
groups.](ggpointless_files/figure-html/geom-area-fade-multiple-groups-basic-1.png)

When groups have very different amplitudes, or when you switch from the
default `position = "stack"` to `stat = "identity"`, this can make
smaller groups nearly invisible next to dominant groups.

``` r

df_alpha_scope <- data.frame(
  g = c("a", "a", "a", "b", "b", "b"),
  x = c(1, 3, 5, 2, 4, 6),
  y = c(1, 2, 1, 9, 10, 8)
)
p <- ggplot(df_alpha_scope, aes(x, y, fill = g))
p + geom_area_fade(
  alpha_scope = "global", # default
  position = "identity"
)
```

![Two overlapping faded areas with very different amplitudes under
global alpha scope, where the smaller group looks nearly invisible next
to the larger
one.](ggpointless_files/figure-html/geom-area-fade-global-1.png)

Setting `alpha_scope = "group"` lets the algorithm calculate the alpha
range for each group separately.

``` r

p <- ggplot(df_alpha_scope, aes(x, y, fill = g))

# alpha_scope = "group": each group uses the alpha range independently
p + geom_area_fade(
  alpha_scope = "group",
  position = "identity"
  )
```

![Same two overlapping faded areas under group alpha scope, where each
group fades within its own range so both reach full opacity at their
peaks.](ggpointless_files/figure-html/geom-area-fade-group-1.png)

## geom_freqpoly_fade

[`geom_freqpoly_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md)
draws the area under a frequency polygon — the filled region between the
binned line and the baseline — using the same fading gradient as
[`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md).
It is paired with
[`stat_bin()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html),
so all binning parameters (`bins`, `binwidth`, `center`, `boundary`, …)
are forwarded.

``` r

ggplot(mpg, aes(hwy, fill = drv, colour = drv)) +
  geom_freqpoly_fade(position = "identity", alpha = 0.7, bins = 20) +
  scale_fill_manual(
    values = cols[1:3],
    labels = c("4" = "four-wheel", "f" = "front-wheel", "r" = "rear-wheel")
  ) +
  scale_colour_manual(
    values = cols[1:3],
    labels = c("4" = "four-wheel", "f" = "front-wheel", "r" = "rear-wheel")
  ) +
  labs(x = "Highway MPG", y = NULL, fill = NULL, colour = NULL)
```

![Overlapping faded frequency polygons of highway MPG split by
drivetrain, each filled with a vertical fade from the polygon line down
to the baseline.](ggpointless_files/figure-html/freqpoly-fade-mpg-1.png)

## geom_density_fade

[`geom_density_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md)
pairs `GeomAreaFade` with
[`stat_density()`](https://ggplot2.tidyverse.org/reference/geom_density.html)
to compute and draw a kernel density estimate with a fading gradient.
All smoothing parameters (`bw`, `adjust`, `kernel`, `bounds`, …) are
forwarded to the stat.

``` r

p <- ggplot(iris, aes(Sepal.Length, fill = Species))  +
  labs(y = NULL, fill = NULL)

p + geom_density_fade()
```

![Three overlapping faded kernel density estimates of iris sepal length,
one curve per species, each filled with a vertical alpha
gradient.](ggpointless_files/figure-html/density-fade-iris-1.png)

## geom_ridgeline_density_fade

A ridgeline plot, popularised by the cover of Joy Division’s album
[Unknown Pleasures](https://en.wikipedia.org/wiki/Unknown_Pleasures),
stacks multiple lines vertically along the y-dimension.

All ridgeline geoms auto-scales the layer so the tallest ridge overlaps
its neighbour by ~50%. The auto-resolved value is reported via
[`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html) so
you have a starting point if you want to override.

Credit to the [`ggridges`
package](https://wilkelab.org/ggridges/index.html).

[`geom_ridgeline_density_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md)
is a wrapper for
[`geom_ridgeline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md)
where the `stat` parameter uses `"density"` by default to compute kernel
density estimates on the fly.

``` r

# ridgeline plots are drawn along the y-dimension
p <- p + aes(y = Species)

p + geom_ridgeline_density_fade(
  outline.type = "none",
  alpha_scope = "global"
)
#> ℹ Using auto-computed `scale = 1.6`.
#> • Pass an explicit `scale` to override.
```

![Ridgeline density plot of iris sepal length, with one faded density
curve per species stacked
vertically.](ggpointless_files/figure-html/density-ridges-fade-iris-1.png)
\## geom_ridgeline_freqpoly_fade

``` r

p + geom_ridgeline_freqpoly_fade()
#> ℹ Using auto-computed `scale = 0.12`.
#> • Pass an explicit `scale` to override.
```

![Ridgeline freqpoly plot of iris sepal length, with one faded freqpoly
curve per species stacked
vertically.](ggpointless_files/figure-html/freqpoly-ridges-fade-iris-1.png)

## geom_ridgeline_histogram_fade

``` r

p + geom_ridgeline_histogram_fade()
#> ℹ Using auto-computed `scale = 0.12`.
#> • Pass an explicit `scale` to override.
```

![Ridgeline histogram plot of iris sepal length, with one faded freqpoly
curve per species stacked
vertically.](ggpointless_files/figure-html/histogram-ridges-fade-iris-1.png)

## geom_ridgeline_fade

[`geom_ridgeline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md)
draws overlapping ridge shapes — each group’s area sits at its own
vertical baseline — with the same fading gradient as
[`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md).
In contrast to
[`geom_ridgeline_density_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md),
[`geom_ridgeline_freqpoly_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md)
and
[`geom_ridgeline_histogram_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md)
the underlying `stat` does not compute the `height` but you provide it
yourself.

``` r

total_sales <- aggregate(
  sales ~ year + month,
  data = subset(txhousing, year <= 2014),
  FUN = sum,
  na.rm = TRUE
)

ggplot(total_sales, aes(x = month, y = year, group = year, height = sales)) +
  geom_ridgeline_fade(
    aes(fill = after_stat(y)),
    alpha = 0.5,
    # stat = "chaikin" smooths the monthly polyline
    stat = "chaikin",
    outline.type = "none"
  ) +
  scale_x_continuous(breaks = 1:12, labels = month.abb, expand = 0) +
  guides(fill = "none") +
  labs(x = NULL, y = NULL) +
  theme(panel.grid = element_blank())
#> ℹ Using auto-computed `scale = 0.000055`.
#> • Pass an explicit `scale` to override.
```

![Ridgeline plot of Texas housing sales: one curve per year from 2000 at
the bottom to 2014 at the top, height encoding monthly sales totals,
fill graded by year and fading to transparent at each row's
baseline.](ggpointless_files/figure-html/geom-ridgeline-fade-1.png)

## geom_point_glow

[`geom_point_glow()`](https://flrd.github.io/ggpointless/dev/reference/geom_point_glow.md)
is a drop-in replacement for
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
that adds a radial gradient glow behind each point using
[`grid::radialGradient()`](https://rdrr.io/r/grid/patterns.html). By
default, alpha, colour and size inherit their values from
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

``` r

# Basic usage
ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
   geom_point_glow()
```

![Scatter plot of car weight against fuel economy with points coloured
by cylinder count, each point surrounded by a soft radial
glow.](ggpointless_files/figure-html/point-glow-basic-1.png)

You can control the alpha, colour, and size of the gradient with these
arguments:

- `glow_alpha`
- `glow_colour`
- `glow_size`

``` r

# Customizing glow parameters (fixed for all points)
ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point_glow(
    glow_alpha = 0.25,
    glow_colour = cols[1],
    glow_size = 15
    )
```

![Same scatter plot of car weight against fuel economy, but every point
has a larger uniform blue glow regardless of its
colour.](ggpointless_files/figure-html/point-glow-overwrite-params-1.png)

### Big Dipper

The constellation [Big
Dipper](https://en.wikipedia.org/wiki/Big_Dipper), drawn with star
positions in right ascension and declination.

![Stylised night-sky rendering of the Big Dipper constellation against a
dark blue gradient background, with each labelled star drawn as a
glowing star-shape sized by its
magnitude.](ggpointless_files/figure-html/big-dipper-1.png)

``` r

# source: https://de.wikipedia.org/wiki/Gro%C3%9Fer_B%C3%A4r#Sterne
# colours, coordinates all approximations of course
big_dipper <- data.frame(
  star = c(
    "Megrez",
    "Dubhe",
    "Merak",
    "Phecda",
    "Megrez",
    "Alioth",
    "Mizar",
    "Alkaid"
  ),
  ra_h = c(12.257, 11.062, 11.031, 11.897, 12.257, 12.900, 13.399, 13.792),
  dec_d = c(57.03, 61.75, 56.38, 53.70, 57.03, 55.96, 54.93, 49.31),
  mag = c(3.32, 1.81, 2.34, 2.41, 3.32, 1.77, 2.23, 1.86),
  colour = c("#CFDDFF", "#FFDBBF", "#C7D9FF", "#C8D9FF", "#CFDDFF", "#C7D9FF", "#CBDBFF", "#BAD0FF")
)

big_dipper$x <- -big_dipper$ra_h
big_dipper$y <- big_dipper$dec_d

# Linear size mapping: brighter (lower mag) → larger point
mag_to_size <- \(m) pmax(0.7, (5.5 - m) * 1.0)

ggplot(big_dipper, aes(x = x, y = y)) +
  geom_path(colour = "#F5F5F5", linewidth = 0.6, alpha = .6) +
  geom_point_glow(
    data = big_dipper[-1L, ], # don't plot Megrez a second time
    aes(x = -ra_h, y = dec_d, size = mag_to_size(mag), colour = colour,
        alpha = mag),
    shape = 8,
    glow_alpha = 0.75
  ) +
  scale_alpha_continuous(range = c(1, 0.4), guide = "none") +
  scale_size_identity() +
  scale_colour_identity() +
  geom_text(
    aes(x = -ra_h, y = dec_d, label = star),
    colour = "#bbccdd",
    vjust = -1.5,
    size = 2.5,
    check_overlap = TRUE
  ) +
  scale_x_continuous(
    breaks = seq(-14, -8, by = 1),
    labels = \(x) paste0(abs(x), "h")
  ) +
  scale_y_continuous(labels = \(x) paste0(x, "°")) +
  labs(title = "Big Dipper", x = NULL, y = NULL) +
  coord_cartesian(clip = 'off') +
  theme(
    panel.background = element_rect(fill = grid::linearGradient(colours = c("#1D2180", "#081849")), colour = NA),
    plot.background = element_rect(fill = grid::linearGradient(colours = c("#1D2180", "#081849")), colour = NA),
    panel.grid = element_blank(),
    text = element_text(colour = "#344B73"),
    plot.title = element_text(size = 18, face = "bold")
  )
```

## geom_segment_fade

[`geom_segment_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_segment_fade.md)
and
[`geom_curve_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_segment_fade.md)
are the fading counterparts of
[`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
and
[`geom_curve()`](https://ggplot2.tidyverse.org/reference/geom_segment.html).
The alpha gradient runs along each segment or curve from `(x, y)` to
`(xend, yend)`; `fade_direction` chooses which end fades.

``` r

df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_curve_fade(
    aes(x = x1, y = y1, xend = x2, yend = y2, colour = "curve"),
    arrow = arrow(),
    data = df
  ) +
  geom_segment_fade(
    aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"),
    data = df
  )
```

![Mtcars scatter plot with two fading arrows, one straight segment and
one curve, pointing between the same pair of cars to highlight a
comparison.](ggpointless_files/figure-html/geom-segment-fade-arrows-1.png)

Under non-linear coordinates
(e.g. [`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)),
the user-supplied endpoints are transformed to device space and the fade
is applied along the straight chord between them — the same behaviour as
[`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
itself. For a fade that **follows** a curve implied by the coord, use
[`geom_hline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md)
/
[`geom_vline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md)
/
[`geom_abline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md),
which subdivide the line in data space before rendering.

## geom_path_fade

[`geom_path_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_path_fade.md),
[`geom_line_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_path_fade.md),
and
[`geom_step_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_path_fade.md)
are the fading counterparts of
[`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html),
[`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html),
and
[`geom_step()`](https://ggplot2.tidyverse.org/reference/geom_path.html).
The fade runs along the path’s arc length, so one or both ends can fade
to transparent independently of the path’s direction.

``` r

ggplot(economics, aes(date, unemploy)) +
  geom_line_fade(linewidth = 1)
```

![Time-series line of US unemployment that fades from transparent at the
earliest dates to fully opaque at the most
recent.](ggpointless_files/figure-html/geom-line-fade-economics-1.png)

[`geom_step_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_path_fade.md)
follows the step geometry, faded along its visible length:

``` r

set.seed(7)
ggplot(data.frame(x = 1:12, y = cumsum(rnorm(12))), aes(x, y)) +
  geom_step_fade(linewidth = 1.5, direction = "hv")
```

![A step line of a cumulative random walk that fades along its arc
length from one end to the
other.](ggpointless_files/figure-html/geom-step-fade-1.png)

`alpha_mode` trades smoothness for performance:

- `"gradient"` — one continuous `linearGradient` mask along the whole
  path. Smoothest, slowest.
- `"step"` — per-segment alpha stepping. Fastest; discrete steps are
  visible for short paths.
- `"auto"` (default) — picks `"gradient"` for n ≤ 50, `"step"` above.

For self-crossing paths the alpha values compound at the crossing
pixels; see the **Self-crossing paths** section in
[`?geom_path_fade`](https://flrd.github.io/ggpointless/dev/reference/geom_path_fade.md).

## geom_abline_fade

[`geom_abline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md),
[`geom_hline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md),
and
[`geom_vline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md)
are the fading counterparts of `ggplot2`’s reference-line geoms. The
line is rendered with an alpha gradient so it fades from solid at one
end to fully transparent at the other — or to both ends — under user
control via `fade_direction`.

``` r

p <- ggplot() +
  geom_abline_fade(
    colour = "#a84dbd",
    slope = 1,
    intercept = 0,
    linewidth = 1,
    fade_direction = "start" # default
  ) +
  geom_hline_fade(
    colour = "#d77e7b",
    yintercept = 1,
    linewidth = 1,
    fade_direction = "end"
  ) +
  geom_vline_fade(
    colour = "#f4ae1b",
    xintercept = 1,
    linewidth = 1,
    fade_direction = c("start", "end")
  ) +
  xlim(-2, 2) +
  ylim(-2, 2)

p
```

![Three fading reference lines on a square panel: a purple diagonal that
fades at the start, a pink horizontal at y = 1 that fades at the end,
and a yellow vertical at x = 1 that fades from both
ends.](ggpointless_files/figure-html/geom-abline-fade-1.png)

Under non-linear coordinates
(e.g. [`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)),
each reference line is subdivided in data space before rendering so the
fade follows the curve that the coord transform produces, not a chord
between endpoints.

``` r

p + coord_polar()
```

![The same three fading reference lines rendered under polar
coordinates, where each line follows the curve produced by the coord
transform with its fade tracking the curved
path.](ggpointless_files/figure-html/geom-abline-fade-non-linear-coord-1.png)

## geom_rect_fade

The
[`geom_rect_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_rect_fade.md)
function draws a rectangle with a linear colour gradient, allowing you
to subtly highlight elements. In the following example, a
[`geom_segment_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_segment_fade.md)
is also added to serve as a border. Like all `geom_*_fade()` functions,
this one is primarily intended to enhance the visual appeal for the
viewer.

``` r

df <-data.frame(
  x = seq_len(15),
  y = c(90, 93, 88, 91, 86, 85, 89, 84, 96, 72, 68, 66, 62, 59, 46)
)

ggplot(df, aes(x, y)) +
  geom_rect_fade(
    data = data.frame(xmin = 10, xmax = 15, ymin = 0, ymax = 100),
    mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = cols[1],
    alpha = 0,
    alpha_fade_to = 0.1,
    inherit.aes = FALSE
  ) +
  geom_segment_fade(
    data = data.frame(x = 10, xend = 10, y = 100, yend = 0),
    aes(x = x, xend = xend, y = y, yend = yend),
    colour = cols[1],
    alpha = 0.75,
    alpha_fade_to = 0,
    inherit.aes = FALSE
  ) +
  geom_fourier(colour = cols[3]) +
  geom_point(size = 4, colour = cols[3], alpha = 0.25) +
  scale_x_continuous(NULL, breaks = 10, labels = "Breakpoint?") +
  scale_y_continuous(NULL, limits = c(0, NA), labels = \(x) paste(x, "%")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
    theme(
    axis.text.x = element_text(
      size = 10,
      colour = ggplot2::alpha(cols[1], 0.75),
      face = "bold"
    )
  )
```

![Time series of declining percentages overlaid with a Fourier-smoothed
curve and a soft rectangular highlight after x = 10, marked by a fading
vertical border line.](ggpointless_files/figure-html/rect-fade-1.png)

### Pattern fills

Instead of a solid fade,
[`geom_rect_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_rect_fade.md)
can fill each rectangle with a texture that fades along with the alpha
gradient. The
[`hatch()`](https://flrd.github.io/ggpointless/dev/reference/hatch.md)
helper builds line hatching at any angle, recoloured to the `fill`
aesthetic:

``` r

df <- data.frame(xmin = 1, xmax = 9, ymin = 0, ymax = 6)

ggplot(df) +
  geom_rect_fade(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = cols[1], alpha = 0.9, alpha_fade_to = 0,
    pattern = hatch()
  ) +
  theme_minimal()
```

![A rectangle filled with diagonal stripe hatching that fades from
opaque at the top to transparent at the
bottom.](ggpointless_files/figure-html/rect-fade-hatch-1.png)

[`hatch()`](https://flrd.github.io/ggpointless/dev/reference/hatch.md)
covers the common CAD-style fills from one helper: `hatch(angle)`
rotates the lines, and `style = "crossed"` overlays a second family for
a crosshatch or grid. So `hatch(style = "crossed")` is a diagonal
crosshatch, `hatch(90)` a vertical stripe, and
`hatch(0, style = "crossed")` a square grid. `colour`, `linewidth`,
`linetype`, and `spacing` (a physical distance, default `unit(2, "mm")`)
are all optional:

``` r

ggplot(df) +
  geom_rect_fade(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = cols[3], alpha = 0.9, alpha_fade_to = 0,
    pattern = hatch(angle = 30, style = "crossed", spacing = unit(3, "mm"))
  ) +
  theme_minimal()
```

![A rectangle filled with a steel-blue crosshatch at 30 degrees, fading
toward the
baseline.](ggpointless_files/figure-html/rect-fade-hatch-variants-1.png)

For the five common presets you can skip the constructor and pass a
string — `pattern = "crossed"` is shorthand for
`pattern = hatch(style = "crossed")`, the same way `position = "dodge"`
stands in for
[`position_dodge()`](https://ggplot2.tidyverse.org/reference/position_dodge.html).
The vocabulary is `"stripe"` (the diagonal default), `"crossed"`,
`"vertical"`, `"horizontal"`, and `"grid"` (vertical + horizontal).
Reach for
[`hatch()`](https://flrd.github.io/ggpointless/dev/reference/hatch.md)
itself when you want to tune `angle` or `spacing`.

``` r

ggplot(df) +
  geom_rect_fade(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = cols[4], alpha = 0.9, alpha_fade_to = 0,
    pattern = "grid"
  ) +
  theme_minimal()
```

![A rectangle filled with a grid of vertical and horizontal lines that
fades toward the
baseline.](ggpointless_files/figure-html/rect-fade-hatch-string-1.png)

The hatch vocabulary is borrowed, with thanks, from the
[`ggpattern`](https://trevorldavis.com/R/ggpattern/) package by Trevor
L. Davis; for a far richer set of patterns use `ggpattern` directly.

#### Build your own pattern

`pattern` also accepts a `grid` grob, which is drawn once and
**clipped** to each rectangle. This is the reliable way to get
*continuous* custom hatching:
[`grid::pattern()`](https://rdrr.io/r/grid/patterns.html) tiling clips
every tile at its own bounds, so diagonal or wavy lines come out as
disconnected dashes. Drawing a single grob and clipping it keeps the
lines continuous.

The key is to build the grob in `"npc"` units, so it spans the rectangle
(`x` and `y` run from 0 to 1 across width and height). Here is a
wavy-line “pattern”: a stack of sine curves stepped up the height,
overshooting the `[0, 1]` range so clipping trims them cleanly. Leave
the stroke colour unset —
[`geom_rect_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_rect_fade.md)
recolours it to the `fill` aesthetic for you.

``` r

wave_hatch <- function(spacing = 0.14, frequency = 6, amplitude = 0.04,
                       linewidth = 1.5) {
  t <- seq(0, 1, length.out = 250)
  y0 <- seq(-0.2, 1.2, by = spacing) # overshoot; clipping trims the ends
  lines <- lapply(y0, function(y) {
    grid::linesGrob(
      x = grid::unit(t, "npc"),
      y = grid::unit(y + amplitude * sin(2 * pi * frequency * t), "npc"),
      gp = grid::gpar(lwd = linewidth)
    )
  })
  grid::gTree(children = do.call(grid::gList, lines))
}

ggplot(df) +
  geom_rect_fade(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = cols[3], alpha = 0.9, alpha_fade_to = 0,
    pattern = wave_hatch()
  ) +
  theme_minimal()
```

![A rectangle filled with continuous wavy horizontal lines that fade
from opaque at the top to transparent at the
bottom.](ggpointless_files/figure-html/rect-fade-wave-1.png)

For self-contained *motifs* (dots, small shapes) rather than continuous
lines, a tiled [`grid::pattern()`](https://rdrr.io/r/grid/patterns.html)
works well and is simpler — tiling only breaks down for lines that need
to cross tile boundaries:

``` r

dots <- grid::pattern(
  grid::circleGrob(r = grid::unit(1, "mm"), gp = grid::gpar(fill = "black")),
  width = grid::unit(5, "mm"), height = grid::unit(5, "mm"), extend = "repeat"
)

ggplot(df) +
  geom_rect_fade(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = cols[2], alpha = 0.9, alpha_fade_to = 0,
    pattern = dots
  ) +
  theme_minimal()
```

![A rectangle filled with a regular grid of dots that fades from opaque
at the top to transparent at the
bottom.](ggpointless_files/figure-html/rect-fade-dots-1.png)

Pattern fills require a graphics device with Porter-Duff compositing
(for example
[`ragg::agg_png()`](https://ragg.r-lib.org/reference/agg_png.html) or
[`grDevices::svg()`](https://rdrr.io/r/grDevices/cairo.html)); on
devices without it the geom falls back to a flat semi-transparent fill.

## geom_col_fade

[`geom_col_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_col_fade.md)
is the fading counterpart of
[`geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.html):
each bar is filled with a vertical alpha gradient, opaque at the tip and
transparent at the baseline.
[`geom_bar_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_col_fade.md)
is the `stat = "count"` alias. Bars render with sharp corners by default
but rounded bar charts are supported via `radius` argument. The
`alpha_scope` argument controls how the gradient’s normalisation
reference is chosen:

- `"bar"` — every bar uses its own range; every peak hits full opacity.
- `"global"` — every bar normalises to the tallest bar **in the entire
  layer**, including across facets.
- `"x"` / `"y"` — every bar normalises to the tallest bar at the same
  discrete x (or y) position; useful for comparing within a stack or
  dodge cluster. `"x"` is for vertical bars (`aes(x = ...)`), `"y"` for
  horizontal bars (`aes(y = ...)`). The scope follows the **mapping**,
  not the coord:
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
  alone does **not** toggle this — to use `"y"` you must map your
  category to `y`.
- `"group"` — every bar normalises to the tallest bar in its `ggplot2`
  group (the interaction of all discrete aesthetics). Most useful with
  explicit `aes(group = …)`; with the typical
  `aes(x = factor, fill = factor)` it degenerates to `"bar"` because
  every (x, fill) combination is its own group.
- `"fill"` / `"colour"` — every bar normalises to the tallest bar with
  the same fill (or colour). Bars sharing a fill share an alpha range
  across x and across facets.

### Comparing alpha_scope modes

A small toy dataset, rendered once per mode.

`"bar"` — every bar fades within its own height range, so each peak hits
full opacity:

``` r

df <- data.frame(
  x = rep(c("A", "B"), each = 3),
  y = c(1, 2, 3, 2, 4, 1),
  grp = rep(c("X", "Y", "Z"), 2)
)

# by default each bar fades from fully opaque
# at top to full transparency at baseline
ggplot(df, aes(x, y, fill = grp)) +
  geom_col_fade(
    position = "dodge",
    alpha_scope = "bar"
  )
```

![Dodged faded column chart of made up data for demonstration
only.](ggpointless_files/figure-html/col-fade-dodge-1.png)

`"global"` — the tallest bar in the whole layer (`y = 4`) is the only
one at full opacity. Every other bar fades in proportion to its own
height:

``` r

ggplot(df, aes(x, y, fill = grp)) +
  geom_col_fade(
    position = "dodge",
    alpha_scope = "global"
  )
```

![Same chart under alpha_scope global, where only the tallest bar
reaches full opacity and every other bar fades in
proportion.](ggpointless_files/figure-html/col-fade-scope-global-1.png)

`"x"` — within each x category, the taller of the two dodged bars hits
full opacity; the shorter one fades to its (own height / max at this x)
ratio:

``` r

ggplot(df, aes(x, y, fill = grp)) +
  geom_col_fade(
    position = "dodge",
    alpha_scope = "x"
  )
```

![Same chart under alpha_scope x, where within each x category the
taller dodged bar reaches full opacity and the shorter one fades
relative to it.](ggpointless_files/figure-html/col-fade-scope-x-1.png)

`"fill"` — bars sharing the same fill share an alpha range across x
positions *and* across panels:

``` r

ggplot(df, aes(x, y, fill = grp)) +
  geom_col_fade(
    position = "dodge",
    alpha_scope = "fill"
  )
```

![Same tchart under alpha_scope fill, where each fill colour shares one
opacity range across all x positions and
panels.](ggpointless_files/figure-html/col-fade-scope-fill-1.png)

The `"y"` mode is the mirror of `"x"` for horizontal bars — reach for it
by mapping your category to `y` (`aes(y = ...)`), not by flipping the
coord.
[`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
alone leaves `flipped_aes` unchanged, so the orientation guard would
still reject `"y"` in that case. `"colour"` is the mirror of `"fill"`
when you map the `colour` aesthetic discretely.

``` r

ggplot(df, aes(y, x, fill = grp)) +
  geom_col_fade(
    position = "dodge",
    alpha_scope = "y"
  )
```

![Same chart under alpha_scope y and flipped orientation, where within
each x category the taller dodged bar reaches full opacity and the
shorter one fades relative to
it.](ggpointless_files/figure-html/col-fade-scope-y-1.png)

## geom_histogram_fade

[`geom_histogram_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_histogram_fade.md)
is the fading counterpart of
[`geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).
It shares the same parameters and underlying mechanics like
[`geom_bar_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_col_fade.md)
and
[`geom_col_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_col_fade.md)
rounded, but uses
[`stat_bin()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).

``` r

ggplot(iris, aes(Sepal.Width, fill = Species)) +
  geom_histogram_fade(
    alpha_scope = "fill",
    alpha_fade_to = 0.25,
    bins = 15,
    position = "dodge"
    ) +
  labs(x = "Sepal Width", y = "Count", fill = NULL)
```

![Stacked faded histogram of sepal width coloured by species, where
taller bars are also more opaque so bar height is reflected both in size
and in
transparency.](ggpointless_files/figure-html/histogram-fade-diamonds-1.png)

## geom_unit_bar

The `geom_unit_*()` family draws bar charts where each bar is a strip of
discrete cells. By default, every cell represents one unit of the value
axis (or `cell_size` units when aggregating). Inspired by isotype /
pictogram visualisations and the
[`waffle`](https://github.com/hrbrmstr/waffle) package, and built on the
standard `ggplot2` layering and coord systems so it composes cleanly
with themes, facets, coords.

Three constructors, mirroring the `ggplot2` vocabulary:

- [`geom_unit_bar()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_bar.md)
  counts observations (like
  [`geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.html)).
- [`geom_unit_col()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_bar.md)
  uses pre-computed `y` (like
  [`geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.html)).
- [`geom_unit_histogram()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_histogram.md)
  bins a continuous variable (like
  [`geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)).

### Basic usage

Six examples on the same `p`, building from a bare bar chart to a dodged
and rounded version with a transformed value axis. The
`coord_equal(ratio = ...)` formula for square cells under
`position = "dodge"` is `ratio = width / (n_groups * cell_size)` (see
the dodge caveat below for the horizontal-orientation form). With
defaults `width = 1`, `cell_size = 1`, and `mtcars$vs` having two
levels, that means `ratio = 1/2`.

``` r

p <- ggplot(mtcars, aes(carb))

# Bare bar chart: each cell = 1 observation
p + geom_unit_bar()
#> `geom_unit_*()` works best with a fixed-ratio coordinate.
#> ℹ Add `+ coord_equal()` to keep cells square.
#> This message is displayed once per session.
```

![Pictogram-style bar chart of carburettor counts in mtcars where each
bar is built from stacked unit
cells.](ggpointless_files/figure-html/unit-bar-basic-1.png)

``` r

# Square cells with rounded corners
p +
  geom_unit_bar(radius = unit(3, "pt")) +
  coord_equal()
```

![Same unit bar chart of carburettor counts but with square cells and
gently rounded
corners.](ggpointless_files/figure-html/unit-bar-square-1.png)

``` r

# Stacked cells coloured by `vs`. With one stack per `carb` value and
# default width, `coord_equal()` already renders square cells.
p +
  geom_unit_bar(radius = unit(3, "pt")) +
  coord_equal() +
  aes(fill = factor(vs))
```

![Unit bar chart of carburettor counts with each bar stacked into two
coloured segments by engine
vs-shape.](ggpointless_files/figure-html/unit-bar-stacked-1.png)

``` r

# Dodged cells: each sub-bar shrinks to `width / n_groups`. With
# `vs` (n_groups = 2) and default `width = cell_size = 1`, the
# square-cell ratio is `ratio = 1 / 2`.
p +
  geom_unit_bar(
    radius = unit(3, "pt"),
    position = position_dodge(preserve = "single")
  ) +
  coord_equal(ratio = 1 / 2) +
  aes(fill = factor(vs))
```

![Unit bar chart with bars dodged side-by-side by engine vs-shape, each
dodged sub-bar rendered as a column of square
cells.](ggpointless_files/figure-html/unit-bar-dodged-1.png)

``` r

# Add an outline and let cells render slightly wider than tall
# (visual aspect 2:1). The factor of 2 in the denominator is purely
# aesthetic — square cells under dodge would be `ratio = 1 / 2`;
# `ratio = 1 / (2 * 2)` stretches cells horizontally to give the
# outlined edges more room to breathe.
p +
  geom_unit_bar(
    radius = unit(3, "pt"),
    position = position_dodge(preserve = "single"),
    colour = "#333333"
  ) +
  coord_equal(ratio = 1 / (2 * 2)) +
  aes(fill = factor(vs))
```

![Dodged unit bar chart with rounded outlined cells rendered slightly
wider than tall for visual breathing
room.](ggpointless_files/figure-html/unit-bar-outline-1.png)

``` r

# Same chart under `scale_y_sqrt()`: cells tile in transformed space
# (heights shrink as y grows). See the "Scale transforms run before
# the stat" caveat below.
p +
  geom_unit_bar(
    radius = unit(3, "pt"),
    position = position_dodge(preserve = "single"),
    colour = "#333333"
  ) +
  coord_equal(ratio = 1 / 2) +
  aes(fill = factor(vs)) +
  scale_y_sqrt()
```

![Dodged outlined unit bar chart drawn on a square-root y-scale, so
cells higher up the bars become progressively
shorter.](ggpointless_files/figure-html/unit-bar-sqrt-1.png)

    #> ! `radius` of 3 points exceeds the largest displayable corner radius for one or
    #>   more rendered shapes, which is 1.99 points.
    #> ℹ Falling back to each shape's maximum displayable radius.
    #> This message is displayed once every 8 hours.

When counts are pre-computed, switch to
[`geom_unit_col()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_bar.md)
— the `coord_equal(ratio = ...)` lever works identically. The example
below mimics a familiar real-world shape: three fill levels in the
layer, but each row contains at most **two** of them.
`position_dodge(preserve = "single")` sizes every sub-bar to
`width / max_groups_per_cluster`, so the effective `n_groups` is **2**,
not `nlevels(fill) = 3`. The square-cell formula for horizontal bars
(`ratio = n_groups * cell_size / width`) therefore gives `ratio = 2`,
not 3 — a subtle case worth keeping in mind when deciding which `ratio`
value to pass.

``` r

df <- data.frame(
  variable = factor(c("A", "A", "B", "B", "C"), levels = c("C", "B", "A")),
  fill     = factor(c("X", "Y", "X", "Z", "X")),
  count    = c(3, 5, 4, 2, 6)
)

ggplot(df, aes(count, variable, fill = fill)) +
  geom_unit_col(
    radius   = unit(1, "pt"),
    position = position_dodge(preserve = "single"),
    colour   = "#333333"
  ) +
  labs(x = NULL, y = NULL) +
  coord_equal(ratio = 2) +   # max 2 groups per variable so ratio = 2, not 3
  theme(legend.position = "bottom")
```

![Horizontal unit column chart with three categories on the y-axis and
dodged outlined cells in three fill colours, where each row contains at
most two of the three fill
levels.](ggpointless_files/figure-html/unit-col-precomputed-1.png)

### geom_unit_histogram

A tribute to [The Pudding’s editorial
style](https://pudding.cool/2017/09/this-american-life/), using
[`geom_unit_histogram()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_histogram.md)
with a **continuous fill** mapping. Each cell is one observed eruption;
cells are coloured by where they fall along the bin axis.

``` r

bw <- diff(range(faithful$eruptions)) / 30

ggplot(faithful, aes(eruptions)) +
  geom_unit_histogram(
    aes(fill = after_stat(x)),
    radius = unit(1, "pt")
  ) +
  guides(fill = "none") +
  scale_x_continuous(
    breaks = c(2, 3, 4, 5),
    labels = paste(2:5, "min"),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  coord_equal(ratio = 1/3 * bw, clip = "off") +
  labs(
    title = "Old Faithful runs in two rhythms",
    x = NULL, y = NULL
  ) +
  theme_minimal()
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![Unit histogram of Old Faithful eruption durations showing two clear
peaks of short and long bursts, with each cell coloured along a
continuous palette by bin position and annotated cluster
labels.](ggpointless_files/figure-html/faithful-pudding-1.png)
