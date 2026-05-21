# Key glyphs for legends

Each geom has an associated function that draws the key when the geom
needs to be displayed in a legend. These functions are called
`draw_key_*()`, where `*` stands for the name of the respective key
glyph. The key glyphs can be customised for individual geoms by
providing a geom with the `key_glyph` argument (see
[`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) or
examples below).

## Usage

``` r
draw_key_fourier(data, params, size)
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
