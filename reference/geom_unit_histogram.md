# Unit Chart Histograms

A unit-cell version of
[`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html):
bins a continuous variable and draws each bin as a stack of unit cells.
With `cell_size = 1` (the default) one cell is one observation, so the
bar's height reads as a literal count.

Like
[`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
this is a thin convenience wrapper around `stat = "bin"`. Pass `bins` or
`binwidth` to control the binning; everything else (rendering, position,
padding) follows
[`geom_unit_bar()`](https://flrd.github.io/ggpointless/reference/geom_unit_bar.md)
– see there for `cell_size`, `cell_padding`, `cell_count_cap`, `radius`,
the rendering caveats, and the performance notes.

## Usage

``` r
geom_unit_histogram(
  mapping = NULL,
  data = NULL,
  stat = "bin",
  position = "stack",
  ...,
  binwidth = NULL,
  bins = NULL,
  orientation = NA,
  radius = grid::unit(0, "pt"),
  cell_size = 1,
  cell_padding = 0.05,
  cell_count_cap = 10000,
  lineend = "butt",
  linejoin = "mitre",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  For `geom_unit_histogram()`, `x` (or `y` for horizontal histograms) is
  required and supplies the continuous variable to be binned. Map `fill`
  to colour cell segments.

- data:

  A data frame.

- stat:

  The statistical transformation to use. Override the default to use a
  different stat, e.g. `stat = "bin"` for a tiled histogram.

- position:

  A position adjustment to use on the data. Default `"stack"` stacks
  bars on top of each other. Use `"dodge"` for side-by-side bars,
  `"fill"` for proportional stacking, or
  `position_stack(reverse = TRUE)` to reverse the stacking order.

- ...:

  Other arguments passed to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- binwidth:

  Width of each bin in data units. When supplied, takes precedence over
  `bins`. Forwarded to
  [`ggplot2::stat_bin()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).

- bins:

  Number of bins. Overridden by `binwidth`. Defaults to 30. Forwarded to
  [`ggplot2::stat_bin()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).

- orientation:

  The orientation of the layer. Default (`NA`) is guessed from the
  aesthetic mapping. Set to `"x"` for vertical bars (value on y) or
  `"y"` for horizontal bars (value on x). Same as
  [`ggplot2::geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.html).

- radius:

  Corner radius for each cell as a
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html). Default
  `grid::unit(0, "pt")` gives sharp corners. Only used with linear
  coordinates; non-linear coordinates (e.g.
  [`ggplot2::coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html))
  fall back to sharp corners.

- cell_size:

  Number of data units one cell represents. Default `1` (one cell per
  unit, the original "isotype" / pictogram pattern). Set to a larger
  value to aggregate units, e.g. `cell_size = 1e4` so each cell stands
  for one thousand. Each cell is then `cell_size` tall in data space.
  With the package defaults (`width = 1`, `cell_size = 1`)
  [`coord_equal()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html)
  already renders cells as squares; for non-default `cell_size` (or
  under `position = "dodge"`) the `coord_equal(ratio)` must scale with
  `cell_size` — see the `position = "dodge"` subsection below for the
  formula. The value axis still shows the original data values; pair
  with `scale_*_continuous(labels = label_cells(cell_size))` to show
  cell counts instead.

- cell_padding:

  Inset applied per side of each cell, in CSS `padding` style. On linear
  value scales the vertical inset is a fraction of `cell_size` (data
  space); on non-linear value scales (`log10`, `sqrt`, ...) it becomes a
  fraction of each cell's *panel* extent so the gap looks visually
  uniform under compression – see "Log and other non-linear value
  scales" below. The horizontal inset is always a fraction of the bar's
  `width` (the bar axis is never the transformed one). Labels are in
  canonical (vertical-bar) orientation; under `orientation = "y"` or
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
  the on-screen roles swap, but element 1 always pads the value axis and
  element 2 always pads the bar axis.

  - length 1 (default `0.05`) – same fraction on all four sides.

  - length 2, unnamed – `c(vertical, horizontal)`; `vertical` is the
    inset between vertically-stacked cells, `horizontal` is the inset
    between each cell and its bar's left/right edge.

  - named (length 1 or 2) – positional independence; allowed names are
    `"vertical"` and `"horizontal"`. A missing axis falls back to the
    default `0.05`. So `c(horizontal = 0.2, vertical = 0.1)`,
    `c(vertical = 0.1, horizontal = 0.2)`, and `c(0.1, 0.2)` are all
    equivalent. Unknown names error rather than silently default – a
    typo would otherwise be hard to spot in the rendered plot.

  Each element must be finite and in `[0, 0.5)`. Set `cell_padding = 0`
  for cells that touch (the borderless isotype style); increase it for a
  waffle-like grid of separated cells. The inset is applied uniformly to
  every cell, including the cells at the bar's outer edges – each cell
  represents one data unit, so cells must render at identical size
  regardless of whether they sit at the floor, in the middle, or at the
  top of a bar. As a consequence the bar's outer edges sit slightly
  inside the data extent: by `cell_padding * cell_size` vertically and
  `cell_padding * width` horizontally on linear scales, and
  proportionally less on non-linear value scales (where the inset is
  panel-proportional rather than data-proportional).

- cell_count_cap:

  Soft cap on the total number of cells drawn per panel. A defensive
  safety net: this geom renders one grob per cell, so very large `y`
  values can freeze the graphics device. When the cap is exceeded, the
  layer falls back to solid bars (one rectangle per bar) and emits a
  warning. Default `1e4`; pass `Inf` to disable. For large `y` you might
  want to set a larger `cell_size`, see *Examples*.

- lineend:

  Line end style for the cell border when `colour` is set. One of
  `"round"`, `"butt"` (default), or `"square"`. Same as
  [`ggplot2::geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.html).

- linejoin:

  Line join style for the cell border. One of `"round"`, `"mitre"`
  (default), or `"bevel"`. Same as
  [`ggplot2::geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.html).

- na.rm:

  If `FALSE` (default), rows with missing `y` are dropped with a
  warning. Non-positive `y` values are kept: `y = 0` produces an empty
  segment, and `y < 0` stacks cells downward from the baseline.

- show.legend:

  logical. Should this layer appear in the legends?

- inherit.aes:

  If `FALSE`, overrides the default aesthetics.

## Value

A
[`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
object that can be added to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## See also

[`geom_unit_bar()`](https://flrd.github.io/ggpointless/reference/geom_unit_bar.md)
for the discrete counterparts and the shared rendering options.
[`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
for the regular (non-unit) histogram.

## Aesthetics

[`geom_unit_bar()`](https://flrd.github.io/ggpointless/reference/geom_unit_bar.md)
understands the following aesthetics. Required aesthetics are displayed
in bold and defaults are displayed for optional aesthetics:

|  |  |  |
|----|----|----|
| • | **[`x`](https://ggplot2.tidyverse.org/reference/aes_position.html)** |  |
| • | **[`y`](https://ggplot2.tidyverse.org/reference/aes_position.html)** |  |
| • | [`alpha`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html) | → `NA` |
| • | [`colour`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | [`fill`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | [`group`](https://ggplot2.tidyverse.org/reference/aes_group_order.html) | → inferred |
| • | [`linetype`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | [`linewidth`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | `width` | → `0.9` |

[`stat_bin()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
understands the following aesthetics. Required aesthetics are displayed
in bold and defaults are displayed for optional aesthetics:

|  |  |  |
|----|----|----|
| • | **[`x`](https://ggplot2.tidyverse.org/reference/aes_position.html) *or* [`y`](https://ggplot2.tidyverse.org/reference/aes_position.html)** |  |
| • | [`group`](https://ggplot2.tidyverse.org/reference/aes_group_order.html) | → inferred |
| • | `weight` | → `1` |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.html).

## Examples

``` r
library(ggplot2)

# Bin a continuous variable; one cell per observation.
# Basic example
p <- ggplot(iris, aes(Sepal.Length, fill = Species))
p + geom_unit_histogram(bins = 40)


# Square cells:
# With `cell_size = 1` (the default), the bin width in data units is
# the `coord_equal()` ratio that makes cells render as squares
bins <- 30
bw <- diff(range(iris$Sepal.Length)) / bins
cs <- 1 # cell_size's default for demonstration here only

# Use bw (bin width) and cs (cell_size) here in coord_equal()
p <- p + coord_equal(ratio = bw / cs)
p + geom_unit_histogram()
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.


# Rounded corners are supported too
p + geom_unit_histogram(radius = unit(3, "pt"))
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.


# Horizontal orientation, and ratio: 1/2
ggplot(iris, aes(y = Sepal.Length, fill = Species)) +
  geom_unit_histogram() +
  coord_equal(ratio = (1/bw) / 2 ) +
  theme(legend.position = "bottom")
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.


# When bin counts are large, set `cell_size` so each cell aggregates
# multiple observations. Pair with `label_cells()` to relabel the axis.
set.seed(1)
big <- data.frame(l = rnorm(1e5))
bw <- diff(range(big$l)) / 30
cs <- 1000

ggplot(big, aes(l)) +
  geom_unit_histogram(cell_size = cs) +
  scale_y_continuous(
    breaks = seq(0, 10, 2) * cs,
    labels = label_cells(cs, suffix = "k")
  ) +
  coord_equal(ratio = bw / cs) +
  labs(
    x = NULL,
    y = NULL,
    caption = "One cell equals 1.000 observations."
  )
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

```
