# Changelog

## ggpointless (development version)

## ggpointless 0.2.0

CRAN release: 2026-03-09

### New features

- New
  [`geom_fourier()`](https://flrd.github.io/ggpointless/dev/reference/geom_fourier.md)
  and
  [`stat_fourier()`](https://flrd.github.io/ggpointless/dev/reference/geom_fourier.md)
  fit a truncated Fourier series (via
  [`stats::fft()`](https://rdrr.io/r/stats/fft.html)) to `x`/`y` data
  and render the reconstructed curve. Supports optional detrending
  (`"lm"` or `"loess"`) and harmonic selection via `n_harmonics`
  ([\#7](https://github.com/flrd/ggpointless/issues/7)).

- New
  [`geom_arch()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  and
  [`stat_arch()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  draw inverted catenary curves (arches) between successive points,
  complementing the existing
  [`geom_catenary()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  ([\#4](https://github.com/flrd/ggpointless/issues/4)).

- New
  [`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md)
  draws area charts where the fill colour fades from opaque to
  transparent using
  [`grid::linearGradient()`](https://rdrr.io/r/grid/patterns.html). The
  fade target alpha is controlled via `alpha_fade_to`
  ([\#3](https://github.com/flrd/ggpointless/issues/3)).

- New
  [`geom_point_glow()`](https://flrd.github.io/ggpointless/dev/reference/geom_point_glow.md)
  draws points with a radial gradient glow behind each point using
  [`grid::radialGradient()`](https://rdrr.io/r/grid/patterns.html). The
  glow alpha, colour, and size can be customised via `glow_alpha`,
  `glow_colour`, and `glow_size`
  ([\#6](https://github.com/flrd/ggpointless/issues/6)).

### Breaking changes

- The bundled (but outdated) datasets `co2_ml`, `covid_vac`, and
  `female_leaders` have been removed from the package. These datasets
  can be obtained from their  
  original sources: [Mauna Loa
  CO₂](https://gml.noaa.gov/ccgg/trends/data.html), [CDC vaccination
  data](https://covid.cdc.gov/covid-data-tracker/#rates-by-vaccine-status),
  and [Wikipedia female
  leaders](https://en.wikipedia.org/w/index.php?title=List_of_elected_and_appointed_female_heads_of_state_and_government&oldid=1078024588),
  respectively. The `vignette("examples")` that showcased these datasets
  has been removed alongside them.

### Improvements

- The package now requires R \>= 4.2.0 and ggplot2 \>= 4.0.0. Several
  geoms take (mostly internal) advantage of new ggplot2 features such as
  [`make_constructor()`](https://ggplot2.tidyverse.org/reference/make_constructor.html),
  and [`gg_par()`](https://ggplot2.tidyverse.org/reference/gg_par.html).

- Messages and errors across the package have been migrated to the `cli`
  and `rlang` packages, giving consistent, hyperlink-aware output.

- [`geom_catenary()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  gained a vectorized `chain_length` argument and deprecated
  `chainLength` instead
  ([\#4](https://github.com/flrd/ggpointless/issues/4)).

- [`stat_catenary()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  no longer wrongfully removes data points when the upper limit in
  [`ylim()`](https://ggplot2.tidyverse.org/reference/lims.html) is set
  to the maximum y-value of the dataset
  ([\#1](https://github.com/flrd/ggpointless/issues/1)).

## ggpointless 0.1.0

CRAN release: 2024-02-08

- New
  [`geom_catenary()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  and
  [`stat_catenary()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  let you draw a hanging chain.
- [`geom_lexis()`](https://flrd.github.io/ggpointless/dev/reference/geom_lexis.md)
  supports `linewidth` argument now, which was released in `ggplot2`
  v3.4.0.
- [`geom_lexis()`](https://flrd.github.io/ggpointless/dev/reference/geom_lexis.md)
  deprecates `point_size` argument in favour of `size`.

## ggpointless 0.0.3

CRAN release: 2022-08-25

- New
  [`geom_chaikin()`](https://flrd.github.io/ggpointless/dev/reference/geom_chaikin.md)
  and
  [`stat_chaikin()`](https://flrd.github.io/ggpointless/dev/reference/geom_chaikin.md)
  apply Chaikin’s corner cutting algorithm to ragged paths.

## ggpointless 0.0.2

CRAN release: 2022-06-08

- New
  [`geom_lexis()`](https://flrd.github.io/ggpointless/dev/reference/geom_lexis.md)
  and
  [`stat_lexis()`](https://flrd.github.io/ggpointless/dev/reference/geom_lexis.md)
  draw lexis graphs.
- New `female_leaders` dataset available.

## ggpointless 0.0.1

CRAN release: 2022-03-08

- New
  [`geom_pointless()`](https://flrd.github.io/ggpointless/dev/reference/geom_pointless.md)
  and
  [`stat_pointless()`](https://flrd.github.io/ggpointless/dev/reference/geom_pointless.md)
  emphasise some observations.
- New data sets on `covid_vac` and `co2_ml` added.
