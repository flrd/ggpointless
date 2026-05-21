# Emphasise some observations with points

This is a wrapper around
[`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
with one additional argument: `location`. This geom aims to emphasise
some observations, and is not particularly useful on its own — hence its
name — but it shines in conjunction with
[`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
and friends; see *Examples*.

## Usage

``` r
geom_pointless(
  mapping = NULL,
  data = NULL,
  stat = "pointless",
  position = "identity",
  ...,
  location = "last",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_pointless(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  ...,
  location = "last",
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

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

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

- location:

  Position(s) to highlight: `"minimum"`, `"maximum"`, `"first"`,
  `"last"` (default), or `"all"`.

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

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
    give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer
    geom](https://ggplot2.tidyverse.org/reference/layer_geoms.html)
    documentation.

## Value

A
[`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
object that can be added to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Details

The `location` argument allows you to specify which observations should
be highlighted. If `location` is `"last"`, the default, a single point
will be plotted at the last non-missing observation. The locations are
determined in the order in which they appear in the data – like
[`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
does compared to
[`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html).

When a single observation matches multiple `location` criteria – for
example, the last point is also the maximum – it is emitted once with a
composite label joined by `", "` (e.g. `"last, maximum"`). The row order
in the output – which drives draw order and legend order – follows the
order given in `location`; for `"all"` the canonical order is `"first"`,
`"last"`, `"minimum"`, `"maximum"`. See `vignette("ggpointless")` for
more details.

## See also

[`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)

## Aesthetics

`geom_pointless()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|  |  |  |
|----|----|----|
| • | **[`x`](https://ggplot2.tidyverse.org/reference/aes_position.html)** |  |
| • | **[`y`](https://ggplot2.tidyverse.org/reference/aes_position.html)** |  |
| • | [`alpha`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html) | → `NA` |
| • | [`colour`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | [`fill`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | [`group`](https://ggplot2.tidyverse.org/reference/aes_group_order.html) | → inferred |
| • | [`shape`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | [`size`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | `stroke` | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.html).

## Examples

``` r
library(ggplot2)
x <- seq(-pi, pi, length.out = 150)
y <- outer(x, 1:5, FUN = \(x, y) sin(x * y))

df1 <- data.frame(
  x = x,
  y = rowSums(y)
)

# Not terribly useful on its own ...
p <- ggplot(df1, aes(x = x, y = y))
p + geom_pointless()

p + geom_pointless(location = "all")


# ... but in conjunction with geom_line(), hopefully
p <- p + geom_line()
p + geom_pointless(location = "all")

p + geom_pointless(location = c("first", "last"))

p + geom_pointless(location = c("minimum", "maximum"))


# The layer computes one additional variable, 'location',
# that you can map to aesthetics. Pair `colour` with `shape` for redundant
# encoding -- the four roles stay distinguishable in greyscale and under
# colour-vision deficiencies.
p + geom_pointless(
  aes(colour = after_stat(location), shape = after_stat(location)),
  location = "all",
  size = 3
)


# Example with missing first and last observations
set.seed(42)
df2 <- data.frame(x = 1:10, y = c(NA, sample(1:8), NA))
ggplot(df2, aes(x, y)) +
  geom_line() +
  geom_pointless(location = c("first", "last"))
#> Warning: Removed 2 rows containing non-finite outside the scale range
#> (`stat_pointless()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_line()`).


# Change the order in which points are drawn when they overlap
df3 <- data.frame(x = 1:2, y = 1:2)

p <- ggplot(df3, aes(x = x, y = y)) +
  geom_path() +
  coord_equal()

# Same as location = 'all'
p + geom_pointless(aes(colour = after_stat(location)),
  location = c("first", "last", "minimum", "maximum")
) +
  labs(subtitle = "same as location = 'all'")


# Reversed custom order
p + geom_pointless(aes(colour = after_stat(location)),
  location = c("maximum", "minimum", "last", "first")
) +
  labs(subtitle = "custom order")


# Same as location = 'all' again
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

```
