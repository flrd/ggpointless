# ggpointless 0.2.0

## New features

* New `geom_fourier()` and `stat_fourier()` fit a truncated Fourier series
  (via `stats::fft()`) to `x`/`y` data and render the reconstructed curve.
  Supports optional detrending (`"lm"` or `"loess"`) and harmonic selection
  via `n_harmonics` (#7).

* New `geom_arch()` and `stat_arch()` draw inverted catenary curves (arches)
  between successive points, complementing the existing `geom_catenary()` (#4).

* New `geom_area_fade()` draws area charts where the fill colour fades from
  opaque to transparent using `grid::linearGradient()`. The fade target alpha
  is controlled via `alpha_fade_to` (#3).

* New `geom_point_glow()` draws points with a radial gradient glow behind
  each point using `grid::radialGradient()`. The glow alpha, colour, and size
  can be customised via `glow_alpha`, `glow_colour`, and `glow_size` (#6).

## Breaking changes

* The bundled (but outdated) datasets `co2_ml`, `covid_vac`, and `female_leaders` have been
  removed from the package. These datasets can be obtained from their  
  original sources: [Mauna Loa CO~2~](https://gml.noaa.gov/ccgg/trends/data.html),
  [CDC vaccination data](https://covid.cdc.gov/covid-data-tracker/#rates-by-vaccine-status),
  and [Wikipedia female leaders](https://en.wikipedia.org/w/index.php?title=List_of_elected_and_appointed_female_heads_of_state_and_government&oldid=1078024588),
  respectively. The `vignette("examples")` that showcased these datasets has
  been removed alongside them.

## Improvements

* The package now requires R >= 4.2.0 and ggplot2 >= 4.0.0. Several geoms 
  take (mostly internal) advantage of new ggplot2 features such as
  `make_constructor()`, and `gg_par()`.

* Messages and errors across the package have been migrated to the `cli` and
  `rlang` packages, giving consistent, hyperlink-aware output.

* `geom_catenary()` gained a vectorized `chain_length` argument and 
   deprecated `chainLength` instead (#4).

* `stat_catenary()` no longer wrongfully removes data points when the upper
  limit in `ylim()` is set to the maximum y-value of the dataset (#1).

# ggpointless 0.1.0
* New `geom_catenary()` and `stat_catenary()` let you draw a hanging chain. 
* `geom_lexis()` supports `linewidth` argument now, which was released in
`ggplot2` v3.4.0.
* `geom_lexis()` deprecates `point_size` argument in favour of `size`.

# ggpointless 0.0.3
* New `geom_chaikin()` and `stat_chaikin()` apply Chaikin's corner cutting
algorithm to ragged paths.

# ggpointless 0.0.2
* New `geom_lexis()` and `stat_lexis()` draw lexis graphs.
* New `female_leaders` dataset available.

# ggpointless 0.0.1
* New `geom_pointless()` and `stat_pointless()` emphasise some observations.
* New data sets on `covid_vac` and `co2_ml` added.
