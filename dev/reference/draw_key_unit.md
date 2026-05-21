# Key glyph for unit bar charts

The default legend key for
[`geom_unit_bar()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_bar.md)
/
[`geom_unit_col()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_bar.md)
/
[`geom_unit_histogram()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_histogram.md).
Mirrors the geom's orientation so the legend reads as a miniature of the
rendered bar:

- vertical bars (`flipped_aes = FALSE`, the default) -\> two cells
  stacked vertically with a single horizontal gap between them, no
  vertical gap.

- horizontal bars (`flipped_aes = TRUE`, e.g. `orientation = "y"` or
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html))
  -\> two cells placed side by side with a single vertical gap between
  them, no horizontal gap.

## Usage

``` r
draw_key_unit(data, params, size)
```

## Arguments

- data:

  A single row data frame containing the scaled aesthetics to display in
  this key

- params:

  A list of additional parameters supplied to the geom.

- size:

  Width and height of key in mm.

## Value

A grid grob.
