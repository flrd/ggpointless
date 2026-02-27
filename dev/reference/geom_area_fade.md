# Area with Fading Linear Gradient

This geom behaves much like
[`ggplot2::geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)
but uses
[`grid::linearGradient()`](https://rdrr.io/r/grid/patterns.html) to
create area plots where the fill colour fades from opaque to
transparent.

## Usage

``` r
geom_area_fade(
  mapping = NULL,
  data = NULL,
  stat = "align",
  position = "stack",
  ...,
  alpha_fade_to = 0,
  outline.type = "upper",
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

- alpha_fade_to:

  A single finite number between 0 and 1. The alpha value the gradient
  fades *to*. Defaults to 0 (fully transparent).

- outline.type:

  Which edges of the area to draw an outline on. One of `"none"`
  (default), `"upper"`, `"lower"`, `"both"` (`"upper"` and `"lower"`),
  or `"full"` (closed polygon outline).

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

## Orientation

This geom treats each axis differently and, thus, can thus have two
orientations. Often the orientation is easy to deduce from a combination
of the given mappings and the types of positional scales in use. Thus,
ggplot2 will by default try to guess which orientation the layer should
have. Under rare circumstances, the orientation is ambiguous and
guessing may fail. In that case the orientation can be specified
directly using the `orientation` parameter, which can be either `"x"` or
`"y"`. The value gives the axis that the geom should run along, `"x"`
being the default orientation you would expect for the geom.

## See also

[`ggplot2::geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)
for fully opaque area charts

## Aesthetics

`geom_area_fade()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|     |                                                                                     |                                                                       |
|-----|-------------------------------------------------------------------------------------|-----------------------------------------------------------------------|
| •   | **[`x`](https://ggplot2.tidyverse.org/reference/aes_position.html)**                |                                                                       |
| •   | **[`y`](https://ggplot2.tidyverse.org/reference/aes_position.html)**                |                                                                       |
| •   | [`alpha`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)       | → `NA`                                                                |
| •   | [`colour`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)      | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| •   | [`fill`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)        | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| •   | [`group`](https://ggplot2.tidyverse.org/reference/aes_group_order.html)             | → inferred                                                            |
| •   | [`linetype`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html)  | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| •   | [`linewidth`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.html).

## Examples

``` r
library(ggplot2)
df <- data.frame(
  g = c("a", "a", "a", "b", "b", "b"),
  x = c(1, 3, 5, 2, 4, 6),
  y = c(2, 5, 1, 3, 6, 7)
)

a <- ggplot(df, aes(x, y, fill = g)) +
  theme_minimal()

# default behaviour
a + geom_area_fade()


# change overall opacity
a + geom_area_fade(alpha = .5)


# fade from 75% to 25% opacity
a + geom_area_fade(alpha = .75, alpha_fade_to = .25)


# reverse direction: transparent at top, opaque at baseline
a + geom_area_fade(alpha = 0, alpha_fade_to = 1)


# draw upper and lower outlines only (no left/right edges)
a + geom_area_fade(outline.type = "both")


# closed outline (all four edges)
a + geom_area_fade(outline.type = "full")


# horizontal orientation
a + geom_area_fade(orientation = "y")


# disable stat alignment (useful when x values are already aligned)
a + geom_area_fade(aes(colour = g), outline.type = "full", stat = "identity")

```
