# Line Segments with a Fading Gradient

`geom_segment_fade()` draws line segments like
[`ggplot2::geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
but applies an alpha gradient along each segment so that one or both
ends fade to transparent. The gradient direction follows the segment
(from `(x, y)` to `(xend, yend)`), so it works at any angle. Rendering
requires a graphics device that supports clipping paths (e.g. ragg,
cairo, svg).

## Usage

``` r
geom_segment_fade(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  alpha_fade_to = 0,
  fade_direction = "start",
  arrow = NULL,
  arrow.fill = NULL,
  lineend = "butt",
  linejoin = "round",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_curve_fade(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  curvature = 0.5,
  angle = 90,
  ncp = 5,
  shape = 0.5,
  alpha_fade_to = 0,
  fade_direction = "start",
  curve_count_cap = 200,
  arrow = NULL,
  arrow.fill = NULL,
  lineend = "butt",
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

  A single finite number between 0 and 1. The alpha value at the fading
  end(s). Defaults to `0` (fully transparent).

- fade_direction:

  Which end(s) of each segment fade? A character vector containing
  `"start"` (the `(x, y)` end fades), `"end"` (the `(xend, yend)` end
  fades), or both `c("start", "end")`. Defaults to `"start"`.

- arrow:

  specification for arrow heads, as created by
  [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html).

- arrow.fill:

  fill colour to use for the arrow head (if closed). `NULL` means use
  `colour` aesthetic.

- lineend:

  Line end style (round, butt, square).

- linejoin:

  Line join style (round, mitre, bevel).

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

- curvature:

  A numeric value giving the amount of curvature. Negative values
  produce left-hand curves, positive values produce right-hand curves,
  and zero produces a straight line.

- angle:

  A numeric value between 0 and 180, giving an amount to skew the
  control points of the curve. Values less than 90 skew the curve
  towards the start point and values greater than 90 skew the curve
  towards the end point.

- ncp:

  The number of control points used to draw the curve. More control
  points creates a smoother curve.

- shape:

  A numeric in `[-1, 1]` controlling the curve's shape, passed to
  [`grid::curveGrob()`](https://rdrr.io/r/grid/grid.curve.html). Applied
  by the fade rendering on all `ggplot2` versions; on the
  plain-[`geom_curve()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
  fallback paths (non-linear coords, over the `curve_count_cap`, or
  uniform alpha) it requires `ggplot2` \>= 4.1.0 and is otherwise
  ignored.

- curve_count_cap:

  Soft cap on the number of curves `geom_curve_fade()` will composite
  per panel. One `groupGrob(op = "dest.in")` is built per curve, so
  render cost scales roughly linearly at ~30 ms / curve. Past the cap
  the layer falls back to plain
  [`ggplot2::geom_curve()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
  (no fade) and emits a warning. Default `200` (~6 s worst case). Pass
  `Inf` to disable. For bulk fading, use
  [`geom_path_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_path_fade.md)
  or `geom_segment_fade()` instead.

## Value

A
[`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
object that can be added to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Details

### Non-linear coordinate systems

Under non-linear coordinates such as
[`ggplot2::coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
and
[`ggplot2::coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
the user-supplied endpoints `(x, y)` and `(xend, yend)` are transformed
to device space and connected by a straight chord – exactly as
[`ggplot2::geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
does. The fade is then applied along that chord, so both endpoint
positions and the fade direction are consistent with the unfaded geom.

If you want the line to *follow* a curve implied by the coord system
(for example a circle at constant `y` under polar), use
[`geom_hline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md)
/
[`geom_vline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md)
/
[`geom_abline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md)
instead – those geoms subdivide the line in data space so the fade
tracks the curve, not a chord.

## Self-crossing paths

When a faded path crosses itself, the pixels at the crossing are
rasterised once per overlapping segment, and the per-segment alpha
values compound. Where the two strands carry different alphas (one near
the faded end, one near the opaque end), the crossing appears noticeably
darker than either strand alone.

This behaviour is inherent to how semi-transparent strokes are
alpha-blended at the device level, not specific to
[`geom_path_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_path_fade.md)
– the same effect appears with `ggplot2::geom_path(alpha = 0.5)`. There
is no general workaround at the rendering layer; if a clean intersection
matters for your plot, the practical options are:

- Raise `alpha_fade_to` so the strands at both ends are closer in
  opacity (smaller delta -\> less visible darkening).

- Use a fully opaque stroke (no fade) for paths known to self-cross.

- Restructure the data so the crossing is split across separate layers.

Applies equally to `geom_segment_fade()` and `geom_curve_fade()` when
two segments / curves overlap at the same pixel.

## References

Murrell, P., Pedersen, T. L., and Skintzos, P. (2023). "Porter-Duff
Compositing Operators in R Graphics." Department of Statistics, The
University of Auckland. Version 1.
<https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/compositing/compositing.html>

Murrell, P. (2023). "Groups, Compositing Operators, and Affine
Transformations in R Graphics." Technical Report 2021-02, Department of
Statistics, The University of Auckland. Version 3.
<https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/groups/groups.html>

## See also

[`geom_path_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_path_fade.md)
for connected paths with alpha fading,
[`ggplot2::geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
and
[`ggplot2::geom_curve()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
for the unfaded versions.

## Aesthetics

`geom_segment_fade()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|  |  |  |
|----|----|----|
| • | **[`x`](https://ggplot2.tidyverse.org/reference/aes_position.html)** |  |
| • | **[`y`](https://ggplot2.tidyverse.org/reference/aes_position.html)** |  |
| • | **[`xend`](https://ggplot2.tidyverse.org/reference/aes_position.html)** |  |
| • | **[`yend`](https://ggplot2.tidyverse.org/reference/aes_position.html)** |  |
| • | [`alpha`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html) | → `NA` |
| • | [`colour`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | [`group`](https://ggplot2.tidyverse.org/reference/aes_group_order.html) | → inferred |
| • | [`linetype`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | [`linewidth`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.html).

## Examples

``` r
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

# Basic example with default fade_direction
p +
  geom_segment_fade(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    fade_direction = "start", # default
    linewidth = 10
  )


# Change fade_direction towards start
p +
  geom_segment_fade(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    fade_direction = "end",
    linewidth = 10
  )


# Fade from centre to both sides
p +
  geom_segment_fade(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    fade_direction = c("start", "end"),
    linewidth = 10
  )

```
