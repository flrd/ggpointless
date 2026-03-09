# Apply Chaikin's corner cutting algorithm to smooth a path

Chaikin's corner-cutting algorithm can be used to smooth sharp corners
of a path.

## Usage

``` r
geom_chaikin(
  mapping = NULL,
  data = NULL,
  stat = "chaikin",
  position = "identity",
  ...,
  mode = "open",
  iterations = 5,
  ratio = 0.25,
  arrow = NULL,
  arrow.fill = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_chaikin(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  ...,
  mode = "open",
  iterations = 5,
  ratio = 0.25,
  closed = lifecycle::deprecated(),
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

- mode:

  Character. Should the geom draw a closed polygon or an open path? Must
  be one of `"open"` (default) or `"closed"`.

- iterations:

  Integer. Number of iterations to apply between `1` and `10`. When
  `iterations = 0` the original data is unchanged so essentially this is
  the same as calling
  [`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html);
  however this might be useful when you want to toggle smoothing on/off
  programmatically without removing the layer.

- ratio:

  Numeric. Cutting ratio must be a number between `0` and `1`. If
  `ratio > 0.5`, then it will be flipped to `1 - ratio`.

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

- geom, stat:

  Use to override the default connection between `geom_chaikin()` and
  `stat_chaikin()`.

- closed:

  **\[deprecated\]** Use `mode` instead.

## Value

A
[`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
object that can be added to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Details

Chaikin's corner cutting algorithm iteratively turns a jagged path into
a smooth path.

The recursion formula starts from two vertices A and B, which represent
a single corner of your path. From this, the algorithm derives two new
points: one at the specified ratio when going from point A to point B,
and one when going from B to A in the opposite direction. By default, a
ratio of 0.25 results in two points: the first at 25% of point A and the
other at 75% of point A (or 25% of point B). Those new points form a
smoother path. Then the algorithm applies the same rule to each pair of
new points. The rule is applied iterations times. The maximum number of
iterations is 10, default is 5.

## References

Chaikin, G. An algorithm for high speed curve generation. Computer
Graphics and Image Processing 3 (1974), 346–349

## See also

The [smoothr](https://strimas.com/smoothr/) package offers tools to
smooth and tidy spatial features

## Aesthetics

`geom_chaikin()` understands the following aesthetics. Required
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

```
