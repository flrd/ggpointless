# Stipple a path, line, or step function with dots

Instead of drawing a continuous stroke, these geoms render a regular
grid of dots and display only those within `radius` of the path. At fine
`dot_spacing` the result closely resembles
[`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
/
[`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
/
[`ggplot2::geom_step()`](https://ggplot2.tidyverse.org/reference/geom_path.html);
as `dot_spacing` increases the discrete, stippled character becomes
visible. Dot density is constant in physical units – the grid reflows
automatically when the viewer is resized.

`geom_stipple_line()` orders observations along the independent axis
before connecting them (like
[`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)).

`geom_stipple_step()` approximates a stair-step path (like
[`ggplot2::geom_step()`](https://ggplot2.tidyverse.org/reference/geom_path.html));
`direction` controls the step shape.

## Usage

``` r
geom_stipple_path(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  dot_spacing = "medium",
  radius = NULL,
  type = "hex",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_stipple_line(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  dot_spacing = "medium",
  radius = NULL,
  type = "hex",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_stipple_step(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  dot_spacing = "medium",
  radius = NULL,
  type = "hex",
  direction = "hv",
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

- dot_spacing:

  `"fine"`, `"medium"` (default), or `"coarse"` – physical spacing
  between dot centres: 2, 4, or 8 mm. A
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object sets an
  explicit size in any unit; a bare numeric is treated as mm.

- radius:

  Maximum distance from the path for a dot to be rendered. Defaults to
  the grid's *covering radius* – `dot_spacing / sqrt(3)` for
  `type = "hex"`, `dot_spacing / sqrt(2)` for `type = "square"` – the
  smallest value that leaves no gaps while highlighting as few dots as
  possible. A [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object
  sets an explicit distance in any unit; a bare numeric is treated
  as mm. Larger values thicken the trace; smaller values thin it but may
  introduce gaps.

- type:

  `"hex"` (default) or `"square"` – grid arrangement.

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

- direction:

  `"hv"` (horizontal then vertical, default), `"vh"` (vertical then
  horizontal), or `"mid"` (step half-way between adjacent x values).
  `geom_stipple_step()` only.

## Value

A
[`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

## Details

`geom_stipple_path()` respects the order of rows in the data (like
[`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html));
`geom_stipple_line()` orders observations along the independent axis
first (like
[`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html));
`geom_stipple_step()` approximates the stair-step path (like
[`geom_step()`](https://ggplot2.tidyverse.org/reference/geom_path.html)).
`NA` values break the line, exactly as in the originals. See the
*Orientation* section.

## Grid geometry

`dot_spacing` is a physical distance in mm, so dot density stays
consistent across plots and across axes with very different scales (e.g.
a date axis against `log10`). Two arrangements are available via `type`:

- `"hex"` (default):

  60 degree staggered centres – hexagonal close-packing.

- `"square"`:

  Aligned rows and columns.

Every `geom_stipple_*()` layer in a plot resolves the same physical
spacing against the same panel, so their lattices coincide exactly.

## Orientation

`geom_stipple_line()` is orientation-aware: by default the independent
axis is `x`, but this can be switched by setting `orientation = "y"`.
See the *Orientation* section of
[`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
for more detail.

## See also

[`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html),
[`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html),
[`ggplot2::geom_step()`](https://ggplot2.tidyverse.org/reference/geom_path.html),
[`geom_stipple_panel()`](https://flrd.github.io/ggpointless/dev/reference/geom_stipple_panel.md),
[`geom_stipple_rect()`](https://flrd.github.io/ggpointless/dev/reference/geom_stipple_rect.md)

## Examples

``` r
library(ggplot2)

ggplot(economics, aes(date, unemploy)) +
  geom_stipple_line(dot_spacing = "coarse")


# Hex vs square grid
df <- data.frame(
  x = seq(0, 2 * pi, length.out = 100),
  y = sin(seq(0, 2 * pi, length.out = 100))
)
ggplot(df, aes(x, y)) +
  geom_stipple_path(type = "hex", colour = "steelblue") +
  geom_stipple_path(
    type = "square", colour = "tomato",
    position = position_nudge(y = -0.4)
  )


# A series that runs vertically: orientation = "y"
ggplot(economics, aes(unemploy, date)) +
  geom_stipple_line(dot_spacing = "coarse", orientation = "y")


# Stair-step stipple
recent <- economics[economics$date > as.Date("2013-01-01"), ]
ggplot(recent, aes(date, unemploy)) +
  geom_stipple_step(dot_spacing = "coarse")
```
