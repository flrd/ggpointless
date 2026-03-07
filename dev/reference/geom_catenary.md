# Catenary Curves and Arches

`geom_catenary()` draws a catenary curve (hanging chain) between
successive points. `geom_arch()` draws an inverted catenary curve and is
hence intended for people living on the southern hemisphere.

The shape follows the catenary equation: \\\\ y = a\\ \cosh \\
\\\bigl(\frac{x - h}{a}\bigr) + v\\.

## Usage

``` r
geom_catenary(
  mapping = NULL,
  data = NULL,
  stat = "catenary",
  position = "identity",
  ...,
  chain_length = NULL,
  sag = NULL,
  chainLength = lifecycle::deprecated(),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_arch(
  mapping = NULL,
  data = NULL,
  stat = "arch",
  position = "identity",
  ...,
  arrow = NULL,
  arrow.fill = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_catenary(
  mapping = NULL,
  data = NULL,
  geom = "catenary",
  position = "identity",
  ...,
  chain_length = NULL,
  sag = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_arch(
  mapping = NULL,
  data = NULL,
  geom = "line",
  position = "identity",
  ...,
  arch_length = NULL,
  arch_height = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- chain_length:

  Numeric vector of physical chain lengths. Recycled to the number of
  segments. If `NULL` and `sag` is also `NULL`, defaults to twice the
  Euclidean distance per segment. Can be mixed with `sag` by placing
  `NA` in the appropriate positions.

- sag:

  Numeric vector giving the vertical drop of the curve below the
  **lowest endpoint** of each segment. Takes precedence over
  `chain_length` when both are supplied for the same segment.

- chainLength:

  **\[deprecated\]** Use `chain_length` instead.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- arrow:

  Arrow specification, as created by
  [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html).

- arrow.fill:

  fill colour to use for the arrow head (if closed). `NULL` means use
  `colour` aesthetic.

- lineend:

  Line end style (round, butt, square).

- linejoin:

  Line join style (round, mitre, bevel).

- linemitre:

  Line mitre limit (number greater than 1).

- geom, stat:

  Override the default connection between `geom_catenary()` and
  `stat_catenary()`, or between `geom_arch()` and `stat_arch()`.

- arch_length:

  Numeric vector of arch lengths. Recycled to the number of segments. If
  `NULL` and `arch_height` is also `NULL`, defaults to twice the
  Euclidean distance per segment. Can be mixed with `arch_height` by
  placing `NA` in the appropriate positions.

- arch_height:

  Numeric vector giving the vertical rise of the arch above the
  **highest endpoint** of each segment. Takes precedence over
  `arch_length` when both are supplied for the same segment.

## Value

A
[`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
object that can be added to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## See also

[`geom_fourier()`](https://flrd.github.io/ggpointless/dev/reference/geom_fourier.md)
for fitting smooth curves to data via Fourier series,
[`geom_chaikin()`](https://flrd.github.io/ggpointless/dev/reference/geom_chaikin.md)
for smoothing paths via corner cutting. The catenary equation is
described at <https://en.wikipedia.org/wiki/Catenary>.

## Aesthetics

`geom_catenary()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                                     |                                                                       |
|-----|-------------------------------------------------------------------------------------|-----------------------------------------------------------------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/reference/aes_position.html)**                |                                                                       |
| •   | **[`y`](https://ggplot2.tidyverse.org/reference/aes_position.html)**                |                                                                       |
| •   | [`alpha`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)       | → `NA`                                                                |
| •   | [`colour`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)      | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| •   | [`group`](https://ggplot2.tidyverse.org/reference/aes_group_order.html)             | → inferred                                                            |
| •   | [`linetype`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)  | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| •   | [`linewidth`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.html).

## Examples

``` r
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
#> Both `sag` and `chain_length` supplied for 2 segments; using `sag`.
#> This message is displayed once every 8 hours.


# Arch with height = 2, considered from highest point of each segment
p + geom_arch(arch_height = c(2, 1, 1))


# Rice house, see https://en.wikipedia.org/wiki/Rice_House,_Eltham
rice_house <- data.frame(x = c(0, 1.5, 2.5, 3.5, 5), y = c(0, 1, 1, 1, 0))
ggplot(rice_house, aes(x, y)) +
  geom_arch(arch_height = .15, lwd = 2) +
  geom_segment(aes(xend = x, yend = 0)) +
  geom_hline(yintercept = 0, colour = "forestgreen", linewidth = 3) +
  coord_equal()
```
