# Area Plots with Fading Linear Gradient

This geom behaves like
[`ggplot2::geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)
but uses
[`grid::linearGradient()`](https://rdrr.io/r/grid/patterns.html) to
create area plots. The gradient is always anchored at `y = 0`: maximum
transparency there, fading to opaque at the data values. Opacity scales
with the absolute distance from zero, so equal `|y|` values always
receive the same alpha — full opacity is reached only at the extreme
with the largest absolute value. This works for positive values,
negative values, and groups that cross zero (where a three-stop gradient
is used).

When `fill` is mapped to a variable (e.g. `aes(fill = z)`), the geom
combines the horizontal colour gradient produced by ggplot2 with the
vertical alpha fade, creating a two-dimensional gradient effect. This
requires a device that supports Porter-Duff compositing (e.g.
[`ragg::agg_png()`](https://ragg.r-lib.org/reference/agg_png.html),
[`grDevices::svg()`](https://rdrr.io/r/grDevices/cairo.html)). On
unsupported devices the geom falls back to a single-colour vertical fade
and emits an informational message.

## Usage

``` r
geom_area_fade(
  mapping = NULL,
  data = NULL,
  stat = "align",
  position = "stack",
  ...,
  alpha_fade_to = 0,
  alpha_scope = "global",
  orientation = NULL,
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

  A single finite number between 0 and 1. The alpha value at `y = 0`
  (the baseline). Defaults to `0` (fully transparent).

- alpha_scope:

  How to scale alpha across groups. `"global"` (default) computes the
  maximum absolute y value across **all** groups in the panel so that
  equal `|y|` always maps to equal alpha. `"group"` computes the maximum
  per group, giving each group the full alpha range independently —
  useful with `position = "identity"` when groups have very different
  amplitudes.

- orientation:

  The orientation of the layer. The default (`NA`) automatically
  determines the orientation from the aesthetic mapping. In the rare
  event that this fails it can be given explicitly by setting
  `orientation` to either `"x"` or `"y"`. See the *Orientation* section
  for more detail.

- outline.type:

  Which edges of the area to draw an outline on. One of `"upper"`
  (default), `"lower"`, `"both"` (`"upper"` and `"lower"`), `"full"`
  (closed polygon outline), or `"none"`. When no `colour` is specified
  explicitly the outline inherits the `fill` colour.

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

## Value

A
[`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
object that can be added to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

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

## References

Murrell, P. (2021). "Luminance Masks in R Graphics." Technical Report
2021-04, Department of Statistics, The University of Auckland.
Version 1.
<https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/masks/masks.html>

Murrell, P. (2022). "Vectorised Pattern Fills in R Graphics." Technical
Report 2022-01, Department of Statistics, The University of Auckland.
Version 1.
<https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/vecpat/vecpat.html>

Murrell, P., Pedersen, T. L., and Skintzos, P. (2023). "Porter-Duff
Compositing Operators in R Graphics." Department of Statistics, The
University of Auckland. Version 1.
<https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/compositing/compositing.html>

Murrell, P. (2023). "Groups, Compositing Operators, and Affine
Transformations in R Graphics." Technical Report 2021-02, Department of
Statistics, The University of Auckland. Version 3.
<https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html>

## See also

[`ggplot2::geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)
for fully opaque area charts, the [ggfx
package](https://ggfx.data-imaginist.com/) for real magic.

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

```
