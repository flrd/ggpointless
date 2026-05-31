# Rectangles with a Fading Gradient and Rounded Corners

`geom_rect_fade()` draws axis-aligned rectangles and fills each one with
a linear gradient that fades one edge to transparent. The direction is
controlled by `fade_direction`. Corners can be rounded via the `radius`
argument, enabling rounded rectangles and smooth-cornered visual
elements. The default of `0 pt` produces plain rectangles.

## Usage

``` r
geom_rect_fade(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  alpha_fade_to = 0,
  fade_direction = "vertical",
  radius = grid::unit(0, "pt"),
  pattern = NULL,
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

  Use to override the default connection between `geom_rect_fade()` and
  [`stat_identity()`](https://ggplot2.tidyverse.org/reference/stat_identity.html).

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
  edge of each rectangle. Defaults to `0` (fully transparent).

- fade_direction:

  Direction of the alpha gradient. One of:

  `"vertical"`

  :   (default) Top edge is opaque (`ymax`), bottom edge fades to
      `alpha_fade_to` (`ymin`).

  `"horizontal"`

  :   Left edge is opaque (`xmin`), right edge fades to `alpha_fade_to`
      (`xmax`).

- radius:

  Corner radius passed to
  [`grid::roundrectGrob()`](https://rdrr.io/r/grid/grid.roundrect.html).
  A [`grid::unit()`](https://rdrr.io/r/grid/unit.html) object (e.g.
  `unit(4, "pt")`); a bare number is interpreted as points. Defaults to
  `unit(0, "pt")` (sharp corners).

- pattern:

  An optional textured fill, applied *underneath* the alpha fade. One
  of:

  `"stripe"`

  :   The built-in preset: continuous diagonal hatching (see *Patterns*
      below).

  a [`grid::grob()`](https://rdrr.io/r/grid/grid.grob.html)

  :   Drawn once and clipped to each rectangle. Best for continuous
      custom hatching (e.g. wavy lines built in `"npc"` units), which
      [`grid::pattern()`](https://rdrr.io/r/grid/patterns.html) tiling
      cannot render without breaking lines into dashes. See the vignette
      for a worked "wave" example.

  a [`grid::pattern()`](https://rdrr.io/r/grid/patterns.html) object

  :   A tiled pattern – best for self-contained motifs (dots, shapes).
      Tiling renders continuous diagonal/wavy lines as dashes, so prefer
      a grob for those.

  In every case the texture's stroke colour (`gp$col`) is automatically
  recoloured to match the `fill` aesthetic. The alpha fade is applied on
  top via Porter-Duff `"dest.in"` compositing; devices without
  compositing support fall back to a flat semi-transparent fill.
  Patterns are not supported under polar coordinates. `NULL` (default)
  uses the plain gradient fill.

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

## Value

A
[`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
object that can be added to a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Polar coordinates

Under
[`ggplot2::coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
/
[`ggplot2::coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
each rectangle is bent into an annular segment. A radial alpha gradient
– transparent at the inner radius, opaque at the outer – is rendered
when the fade direction aligns with the radial axis:

- `theta = "x"` (default) + `fade_direction = "vertical"`: `ymin`/`ymax`
  map to inner/outer radius and fade radially.

- `theta = "y"` + `fade_direction = "horizontal"`: `xmin`/`xmax` map to
  inner/outer radius and fade radially.

Any other combination (for example `theta = "x"` with
`fade_direction = "horizontal"`) would require an angular / conic
gradient, which `grid` does not yet expose. Such plots fall back to
plain
[`ggplot2::geom_rect()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)
rendering and emit a one-time warning. Rounded corners (`radius`) are
ignored in polar coordinates since arcs do not carry corner geometry.

## Patterns

One built-in preset is available: `pattern = "stripe"` draws continuous
diagonal hatching, recoloured to the `fill` aesthetic and faded with the
alpha gradient. The `"stripe"` name is borrowed, with thanks, from the
ggpattern package by Trevor L. Davis
(<https://trevorldavis.com/R/ggpattern/>); the implementation here is an
independent grid one.

Unlike [`grid::pattern()`](https://rdrr.io/r/grid/patterns.html) tiling
– which renders diagonal or wavy lines as disconnected dashes, because
each tile is clipped at its own bounds – the `"stripe"` preset draws
real parallel lines clipped to the rectangle, so the diagonals stay
continuous at a true visual angle.

For your own *continuous* hatching (e.g. wavy lines), build a
[`grid::grob()`](https://rdrr.io/r/grid/grid.grob.html) in `"npc"` units
and pass it as `pattern`: it is drawn once and clipped to each
rectangle, preserving line continuity. For self-contained *motifs*
(dots, small shapes) pass a
[`grid::pattern()`](https://rdrr.io/r/grid/patterns.html) object, which
tiles cleanly. In both cases the stroke colour is recoloured to the
`fill` aesthetic. The article `vignette("ggpointless")` walks through
building a wavy-line grob and injecting it. For a far richer set of
patterns and controls, use ggpattern directly.

## Legend key under coord_flip

The legend key glyph always shows the canonical (data-axis) fade
direction – vertical for the default orientation, horizontal under
`orientation = "y"`. Under
[`ggplot2::coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
the rendered geom rotates correctly but the legend key does *not*:
ggplot2's legend builder is coord-independent by design (`draw_key` has
no access to the coord). For a legend key that matches a horizontal
layout, prefer `aes(y = ...)` with auto-detected `orientation = "y"`
over `aes(x = ...) + coord_flip()`.

## References

Murrell, P. (2022). "Vectorised Pattern Fills in R Graphics." Technical
Report 2022-01, Department of Statistics, The University of Auckland.
Version 1.
<https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/vecpat/vecpat.html>

## See also

[`ggplot2::geom_rect()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)
for plain rectangles,
[`geom_col_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_col_fade.md)
for bar charts with per-bar gradient scaling and orientation support.
The ggpattern package (<https://trevorldavis.com/R/ggpattern/>) for
comprehensive pattern fills.

## Aesthetics

`geom_rect_fade()` understands the following aesthetics. Required
aesthetics are displayed in bold and defaults are displayed for optional
aesthetics:

|  |  |  |
|----|----|----|
| • | **[`x`](https://ggplot2.tidyverse.org/reference/aes_position.html) *or* `width` *or* [`xmin`](https://ggplot2.tidyverse.org/reference/aes_position.html) *or* [`xmax`](https://ggplot2.tidyverse.org/reference/aes_position.html)** |  |
| • | **[`y`](https://ggplot2.tidyverse.org/reference/aes_position.html) *or* `height` *or* [`ymin`](https://ggplot2.tidyverse.org/reference/aes_position.html) *or* [`ymax`](https://ggplot2.tidyverse.org/reference/aes_position.html)** |  |
| • | [`alpha`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html) | → `NA` |
| • | [`colour`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | [`fill`](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | [`group`](https://ggplot2.tidyverse.org/reference/aes_group_order.html) | → inferred |
| • | [`linetype`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |
| • | [`linewidth`](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) | → via [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) |

Learn more about setting these aesthetics in
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.html).

## Examples

``` r
library(ggplot2)

# With geom_rect_fade() you can draw arbitrary rectangles
ggplot(head(economics, 25), aes(date, unemploy)) +
  geom_rect_fade(
    data = data.frame(
      xmin = as.Date("1968-07-01"),
      xmax = as.Date("1969-07-01"),
      ymin = -Inf, ymax = 2800
    ),
    inherit.aes = FALSE,
    alpha = 0,
    alpha_fade_to = 0.3,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  ) +
  stat_fourier(geom = "line_fade", fade_direction = "start", alpha_fade_to = 0.2) +
  geom_point(size = 3, alpha = 0.2) +
  theme_minimal()

```
