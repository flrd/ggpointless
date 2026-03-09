# Lexis diagrams

This geom can be used to plot 45° lifelines for a cohort. Lexis diagrams
are named after Wilhelm Lexis and used by demographers for more than a
century.

## Usage

``` r
geom_lexis(
  mapping = NULL,
  data = NULL,
  stat = "lexis",
  position = "identity",
  ...,
  point_show = TRUE,
  point_colour = NULL,
  gap_filler = TRUE,
  lineend = "round",
  linejoin = "round",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_lexis(
  mapping = NULL,
  data = NULL,
  geom = "lexis",
  position = "identity",
  ...,
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

- point_show:

  logical. Should a point be shown at the end of each segment? `TRUE` by
  default.

- point_colour:

  colour of the endpoint point. If `NULL` (default), the group colour is
  used.

- gap_filler:

  logical. Should horizontal gap-filler segments be drawn? `TRUE` by
  default.

- lineend:

  line end style (round, butt, square)

- linejoin:

  line join style (round, mitre, bevel)

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

This geom draws 45° lines from the start to the end of a 'lifetime'. It
is a combination of a segment, and a point. Besides `y` and `yend`
coordinates this geom creates one additional variable called `type` in
the layer data. You might want to map to an aesthetic with
[`ggplot2::after_stat()`](https://ggplot2.tidyverse.org/reference/aes_eval.html),
see Examples section and
[`vignette("ggpointless")`](https://flrd.github.io/ggpointless/articles/ggpointless.md)
for more details.

Rows in your data with either missing `x` or `xend` values will be
removed because your segments must start and end somewhere.

## Aesthetics

`geom_lexis()` understands the following aesthetics. Required aesthetics
are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                                     |             |
|-----|-------------------------------------------------------------------------------------|-------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/reference/aes_position.html)**                |             |
| •   | **[`y`](https://ggplot2.tidyverse.org/reference/aes_position.html)**                |             |
| •   | **[`xend`](https://ggplot2.tidyverse.org/reference/aes_position.html)**             |             |
| •   | **[`yend`](https://ggplot2.tidyverse.org/reference/aes_position.html)**             |             |
| •   | [`alpha`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)       | → `NA`      |
| •   | [`colour`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)      | → `"black"` |
| •   | [`fill`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)        | → `NA`      |
| •   | [`group`](https://ggplot2.tidyverse.org/reference/aes_group_order.html)             | → inferred  |
| •   | [`linetype`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)  | → `"solid"` |
| •   | [`linewidth`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → `0.5`     |
| •   | [`shape`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)     | → `19`      |
| •   | [`size`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)      | → `1.5`     |
| •   | `stroke`                                                                            | → `0.5`     |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.html).

## Examples

``` r
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
#> Warning: Removed 1 row containing non-finite outside the scale range (`stat_lexis()`).


# Ideally, `x` values should be increasing, unlike
# in the next example
df3 <- data.frame(x = Sys.Date() - 0:2, xend = Sys.Date() + 1:3)
ggplot(df3, aes(x = x, xend = xend)) +
  geom_lexis()


# Mixing Date and POSIXct in x/xend is not supported: scales cannot
# transform across the two types and will throw an error.
# \donttest{
ggplot(
  data.frame(x = Sys.Date(), xend = Sys.time()),
  aes(x = x, xend = xend)
) +
  geom_lexis()

# }
```
