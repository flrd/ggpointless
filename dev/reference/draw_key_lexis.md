# Legend key glyph for `geom_lexis`

Draws a short 45° line with an optional point at the upper-right end,
matching the visual appearance of a Lexis diagram segment. Can also be
used as a `key_glyph` in other geoms via `key_glyph = "lexis"`.

## Usage

``` r
draw_key_lexis(data, params, size)
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

## Examples

``` r
df <- data.frame(x = c(0, 1), xend = c(3, 4), grp = c("A", "B"))

# default key glyph used automatically by geom_lexis
ggplot2::ggplot(df, aes(x = x, xend = xend, color = grp)) +
  geom_lexis()


# borrow the glyph for another geom
ggplot2::ggplot(df, aes(x, xend, colour = grp)) +
  ggplot2::geom_line(key_glyph = "lexis")
#> `geom_line()`: Each group consists of only one observation.
#> ℹ Do you need to adjust the group aesthetic?
```
