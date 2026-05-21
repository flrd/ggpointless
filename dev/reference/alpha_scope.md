# Alpha scope: how the fade gradient is normalised across rows

Several fade geoms accept an `alpha_scope` argument that controls how
the alpha gradient is *normalised* — that is, which subset of the
rendered shapes the most-opaque end of the gradient is calibrated to.
The vocabulary overlaps but is not identical across the geom families,
because what counts as a meaningful "reference" differs between bars,
areas, and ridgelines. This page is the consolidated reference.

### What `alpha_scope` does

Each fade geom interpolates the rendered alpha between two extremes:
`alpha_fade_to` (the faded end, default fully transparent) and the row's
aesthetic `alpha` value (the opaque end, default fully opaque). The peak
of that interpolation is positioned somewhere on the data, and
`alpha_scope` chooses where:

- Per-row scopes anchor the peak at the row's own extreme (each shape
  gets the full alpha range to itself).

- Group-relative scopes anchor the peak at the maximum within a discrete
  subset (rows in the same subset share an alpha range; smaller rows
  appear proportionally fainter).

- Layer-wide scopes anchor the peak at the maximum across the whole
  layer (or all panels under faceting).

### Vocabulary by geom family

The accepted values and defaults are family-specific:

- **Area family**
  ([`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md),
  [`geom_density_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md),
  [`geom_freqpoly_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md)):

  Default `"global"`. Allowed: `"global"`, `"group"`. `"global"` scales
  every group to the layer-wide maximum `|y|`, so equal `|y|` always
  maps to equal alpha. `"group"` lets each group use the full alpha
  range independently.

- **Bar family**
  ([`geom_col_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_col_fade.md),
  [`geom_bar_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_col_fade.md)):

  Default `"bar"`. Allowed: `"bar"`, `"group"`, `"x"`, `"y"`, `"fill"`,
  `"colour"`, `"global"`. `"bar"` gives every bar its own range (every
  peak hits full opacity); `"x"` / `"y"` normalise within a
  position-axis cluster (useful for stacks and dodges); `"fill"` /
  `"colour"` normalise within a colour class; `"global"` normalises
  layer-wide; `"group"` falls back to `ggplot2`'s `data$group`.

- **Histogram family**
  ([`geom_histogram_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_histogram_fade.md)):

  Default `"bar"`. Allowed: `"bar"`, `"group"`, `"bin"`, `"fill"`,
  `"colour"`, `"global"`. The shared bar-family scopes carry over, but
  `"x"` / `"y"` are **not** accepted: they key on `round(data$x|y)`
  which is meaningless on a continuous binned axis. Use `"bin"` instead
  — it normalises within each bin (every cluster of dodged bars in one
  bin shares an alpha range), recovering the per-cluster intent that
  `"x"` / `"y"` give on
  [`geom_col_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_col_fade.md)
  /
  [`geom_bar_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_col_fade.md).

- **Ridgeline family**
  ([`geom_ridgeline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md),
  [`geom_ridgeline_density_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md)):

  Default `"group"`. Allowed: `"group"`, `"global"`. `"group"` lets each
  ridge use the full alpha range independently. `"global"` scales
  relative to the tallest ridge in the entire layer (incl. across
  facets), so shorter ridges fade in proportion.

### Why the same name means different things

`"global"` always means *the layer-wide maximum* — but the *maximum of
what* depends on the geom: `|y|` for areas, `|y|` (or `|x|` under
`orientation = "y"`) for bars and histograms, and the tallest ridge
`height` for ridgelines. `"group"` always means *normalise per `ggplot2`
group* (`data$group`, the interaction of all discrete aesthetics). For
ridgelines this is effectively "per ridge" because each `data$group`
corresponds to one ridge. The defaults above are chosen so that the most
common usage of each family produces sensible output without explicit
`alpha_scope`.

## See also

[`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md),
[`geom_col_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_col_fade.md),
[`geom_ridgeline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md)
for the geom-side documentation that drives the actual rendering.
