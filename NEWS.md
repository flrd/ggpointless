# ggpointless (development version)

## New features

* **New** `geom_stipple_panel()`, `geom_stipple_path()`, `geom_stipple_line()`,
  `geom_stipple_step()`, and `geom_stipple_rect()` render lines, paths, steps,
  and filled regions as a regular grid of dots (a "stippled" look) instead of a
  solid stroke. `spacing` (`"fine"`/`"medium"`/`"coarse"`) sets the dot density
  as a per-axis fraction of the panel, so density stays consistent across plots
  and across axes with different scales; every `geom_stipple_*()` layer shares
  the same lattice, so their dots coincide. The keep `radius` defaults to the
  lattice's covering radius, the smallest value that traces the line without
  gaps. `geom_stipple_line()` is orientation-aware (`orientation = "y"`),
  `geom_stipple_step()` supports `direction`, and `NA` values break the line
  like `geom_line()` / `geom_step()`.

# ggpointless 0.3.0

## New features

* **New** `geom_unit_bar()`, `geom_unit_col()`, and `geom_unit_histogram()`
  draw isotype / pictogram bar charts, where each bar is a stack of discrete
  unit cells (one cell = one observation by default). `geom_unit_bar()` counts
  observations like `geom_bar()`; `geom_unit_col()` uses pre-computed `y` like
  `geom_col()`; `geom_unit_histogram()` bins a continuous variable like
  `geom_histogram()`. Fractional `y` values produce a partial cell at the
  outer edge. All three work with `coord_equal()`, `coord_polar()`,
  `coord_radial()`, `coord_flip()`, and faceting. #15

* **New** `label_cells(cell_size)` helper pairs with `cell_size` to relabel
  the value axis in cell counts instead of raw units: pass it to
  `scale_y_continuous(labels = ...)` so a tick at `y = 200` displays as `2`
  when `cell_size = 100`.
  
* **New** `geom_gridline()` draws (panel)grid lines as a layer on top of other
  geoms. Reads positions directly from the trained scales (no manual breaks
  needed), inherits line styling from
  `theme(panel.grid.major.*)` / `panel.grid.minor.*` for any property not
  explicitly overridden, and supports `coord_polar()` / `coord_radial()`. Use
  `grids = "x" / "y" / c("x", "y")` to pick axes; `minor = TRUE` to draw
  minor lines too. To remove the underlying theme grid, compose
  with `+ theme(panel.grid = element_blank())`. #12
  
* **New** `geom_path_fade()`, `geom_line_fade()`, and `geom_step_fade()` draw
  paths, lines, and step functions with a linear alpha gradient along their
  length, so one or both ends fade to transparent. The `fade_direction` argument
  controls which end(s) fade (`"end"`, `"start"`, or `c("start", "end")`);
  `alpha_fade_to` sets the target alpha.

* **New** `geom_segment_fade()` draws individual line segments like
  `geom_segment()` but fades each segment along its own direction — the
  gradient follows the segment from `(x, y)` to `(xend, yend)`, so it works at
  any angle. Accepts the same `fade_direction` and `alpha_fade_to` arguments as
  `geom_path_fade()`. #11

* **New** `geom_curve_fade()` draws Bézier curves like `geom_curve()` but with
  an alpha gradient along the curve direction. The fade follows from start to
  end point, so curves fade at any angle. Uses the same `fade_direction` and
  `alpha_fade_to` arguments as `geom_segment_fade()`, with Porter-Duff
  compositing for smooth gradients. Falls back to semi-transparent curves on
  unsupported devices. #11

* **New** `geom_abline_fade()`, `geom_hline_fade()`, and `geom_vline_fade()`
  draw reference lines (diagonal, horizontal, vertical) with an alpha gradient
  along the line direction. They mirror the `ggplot2` annotation pattern: pass
  `slope`/`intercept`, `yintercept`, or `xintercept` directly for constant
  lines, or supply `data` and `mapping` for facet-varying lines. Under
  non-linear coordinate systems (`coord_polar()`, `coord_radial()`) the fade
  follows the curve that the coord transform produces — `geom_hline_fade()`
  fades around a circle, `geom_vline_fade()` fades along a ray, and
  `geom_abline_fade()` fades along the resulting arc.

* **New** `geom_rect_fade()` draws arbitrary rectangles with a
  linear alpha gradient that fades one edge to transparent. The `fade_direction`
  argument controls the gradient direction: `"vertical"` (default) fades from
  opaque at the top to transparent at the bottom; `"horizontal"` fades from
  opaque at the left to transparent at the right. Supports a `radius` argument
  for rounded corners too. #13

* **New** `geom_col_fade()`, `geom_bar_fade()`, and `geom_histogram_fade()` 
  draw bar charts with a vertical alpha gradient that fades from opaque at
  the peak to transparent at the baseline. Additionally these geoms support a
  `radius` to draw bar charts with rounded corners 
  The rendering tier is chosen at draw time: gradient fill on capable devices, flat
  semi-transparent fill on `pdf()`/`postscript()`. #14

* **New** `geom_freqpoly_fade()` draws a filled frequency polygon — the area under
  the `geom_freqpoly()` line — with the same fading gradient as
  `geom_area_fade()`. Paired with `stat_bin()`, so all binning parameters
  (`bins`, `binwidth`, `center`, `boundary`, …) are forwarded.

* **New** `geom_density_fade()` draws a kernel density estimate with a fading
  gradient, using `geom_area_fade()` paired with `stat_density()`. Accepts all
  smoothing parameters (`bw`, `adjust`, `kernel`, `bounds`, …).

* **New** `geom_ridgeline_fade()`, `geom_ridgeline_density_fade()`,
  `geom_ridgeline_histogram_fade()`, and `geom_ridgeline_freqpoly_fade()` draw
  ridgeline plots — overlapping ridge shapes at different vertical offsets — with
  a vertical alpha gradient that fades from opaque at each ridge's peak to
  transparent at the baseline. The `alpha_scope` argument controls how alpha is
  scaled across ridges. `geom_ridgeline_fade()` takes explicit `(x, y, height)`;
  the three other forms compute `height` from a stat (smooth density, stepped
  histogram, polyline frequency polygon) so users only supply `(x, y)`. All four
  handle negative heights (dips below the baseline) with a bidirectional gradient.
  The `scale` argument defaults to `NULL`, which auto-scales the layer so
  the tallest ridge overlaps its neighbour by ~50% (the canonical "mountain
  range" ridgeline look). The auto-resolved value is reported via a
  `cli::cli_inform()` so you have a starting point if you want to override. #19

* **New** `PositionRidgeline` (callable via `position = "ridgeline"`) is the
  position adjustment that converts a `(y, height)` mapping into the
  ribbon-style `(ymin, ymax)` form `GeomRibbon` expects, so each ridge sits
  on its own y-baseline. Used as the default position by
  `geom_ridgeline_fade()`, but also composes with vanilla
  [ggplot2::geom_ribbon()] for users who want the layout without the fade.
  
## Breaking changes

* `geom_point_glow(glow_size = X)` now interprets `X` at face value in
  `ggplot2` size units, matching the `size` aesthetic of
  [ggplot2::geom_point()]. Previously `X` was wrongfully multiplied by 3
  before rendering, so existing user-supplied values render roughly
  three times smaller than in 0.2.0. The default (`glow_size = NA`) is
  unchanged and still renders at nine times the point's `size`.

* Removed `geom_lexis(point_size = ...)`, deprecated since 0.1.0 in favour
  of `size`. Code still passing `point_size` will now error with
  `unused argument`; rename the argument to `size`.

## Bug fixes and improvements

* `stat_chaikin()` no longer renders nothing without a message when the
  smoothed data has missing values in different rows of `(x, y)` than in a
  numeric extra aesthetic (e.g. `aes(fill = z)` where `z` has its own NAs).
  Previously each numeric column was de-NA'd independently, the resulting
  per-column outputs had different lengths, the assignment errored, and
  `ggplot2`'s stat machinery silently swallowed the error — leaving an empty
  layer. The stat now pre-filters complete cases across all numeric columns
  once and warns about dropped rows in the standard `ggplot2` way (suppress
  via `na.rm = TRUE`). #18

* `stat_fourier()` / `stat_catenary()` / `stat_arch()` no longer crash
  `ggplot2`'s limit expansion when paired with `coord_transform(y = "log10")`
  (and other restricted-domain transforms such as `"log"`, `"sqrt"`).
  The stats now drop those rows with a single helpful warning that points users
  at `scale_y_log10()`, which transforms before the stat runs and avoids the 
  issue entirely.
  
* `stat_pointless()` / `geom_pointless()`: when a single observation matches
  multiple `location` criteria — e.g. the last point is also the maximum —
  `after_stat(location)` now carries a composite label (e.g. `"last, maximum"`)
  instead of silently dropping the secondary labels. Previously only the first
  matching label in iteration order was kept. Row order still follows the order
  given in `location` (canonical `"first"`, `"last"`, `"minimum"`, `"maximum"`
  for `"all"`).
  
* `geom_area_fade()`: now renders alpha gradient when `+ coord_flip()` is
  applied. Previously the geom did not render any fill without a message or
  warning shown to users. #17

* `geom_area_fade()`: on devices without gradient support (base `pdf()`,
  `postscript()`) the informational message now fires for solid-fill plots too
  — users were previously silent about the lost vertical fade unless `fill`
  was mapped to a variable. Wording is tailored to what was actually lost
  (colour gradient vs. vertical fade). Devices that support gradients but not
  compositing still stay silent for solid-fill plots (tier 2 renders the
  vertical fade faithfully).

* `geom_area_fade()`: the `global_max_abs` scan that drives `alpha_scope =
  "global"` now handles `Date` and `POSIXct` value axes. Previously both
  failed `is.numeric()` and silently fell back to `global_max = 1`; plots
  built through the normal scale pipeline were unaffected (values are
  already numeric by draw time), but direct `draw_panel()` calls with raw
  Date / POSIXct ymax are now robust.

* `geom_area_fade()`: the legend key (`.draw_key_area_fade()`) now validates
  `alpha_fade_to` via the shared `.check_alpha_fade_to()` helper, matching
  the validation used by `setup_params()`. A guide that constructs a key
  outside the normal setup pipeline now aborts on out-of-range values
  instead of silently producing an invalid gradient.

* `geom_area_fade()`: fixed alpha overflow for stacked areas with
  `alpha_scope = "global"` (the default). The reference max was computed
  from the pre-stacking `y` values, so the top ribbon's alpha exceeded 1
  and was silently clamped to fully opaque, defeating the fade. The
  reference is now taken from post-position-adjustment data, so equal
  rendered `|y|` maps to equal opacity as documented. No effect on
  `position = "identity"` or `alpha_scope = "group"`.

*  `geom_area_fade()`: now accepts both integer and floating-point `alpha_fade_to`
  values (e.g. `0L`, `0.5`). Previously integer input was rejected.

* `geom_area_fade()`: fixed `has_outline` check to handle `coord_polar()` without
  crashing when outline colour is a vector.

* `geom_area_fade()`: fixed duplicate `comp_stops` that could arise when values
  were clipped to `[0, 1]` bounds (e.g. when `val_hi` is far outside the panel).
  Identical stops are now de-duplicated before gradient construction.

* `geom_area_fade()` under `coord_flip()` now rotates its alpha gradient to
  follow the rendered visual axis, instead of staying vertical and visually
  disappearing against horizontal bars. The gradient is now anchored to the
  rendered geometry via `flipped_visual = xor(flipped_aes, CoordFlip)`, so
  `coord_flip()` and `orientation = "y"` produce equivalent output.

* New custom legend key glyphs for `geom_fourier()` (sine wave),
  `geom_catenary()` (hanging curve), and `geom_arch()` (arch curve) provide
  custom visual cues in the legend.

* All `draw_key_*()` functions now consistently use `ggplot2::gg_par()` instead
  of `grid::gpar()` for proper theme resolution in `ggplot2` v4.0+.

# ggpointless 0.2.0

## New features

* **New** `geom_fourier()` and `stat_fourier()` fit a truncated Fourier series
  (via `stats::fft()`) to `x`/`y` data and render the reconstructed curve.
  Supports optional detrending (`"lm"` or `"loess"`) and harmonic selection
  via `n_harmonics` (#7).

* **New** `geom_arch()` and `stat_arch()` draw inverted catenary curves (arches)
  between successive points, complementing the existing `geom_catenary()` (#4).

* **New** `geom_area_fade()` draws area charts where the fill colour fades from
  opaque to transparent using `grid::linearGradient()`. The fade target alpha
  is controlled via `alpha_fade_to` (#3).

* **New** `geom_point_glow()` draws points with a radial gradient glow behind
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

* The package now requires R >= 4.2.0 and `ggplot2` >= 4.0.0. Several geoms 
  take (mostly internal) advantage of new `ggplot2` features such as
  `make_constructor()`, and `gg_par()`.

* Messages and errors across the package have been migrated to the `cli` and
  `rlang` packages, giving consistent, hyperlink-aware output.

* `geom_catenary()` gained a vectorized `chain_length` argument and 
   deprecated `chainLength` instead (#4).

* `stat_catenary()` no longer wrongfully removes data points when the upper
  limit in `ylim()` is set to the maximum y-value of the dataset (#1).

# ggpointless 0.1.0
* **New** `geom_catenary()` and `stat_catenary()` let you draw a hanging chain. 
* `geom_lexis()` supports `linewidth` argument now, which was released in
`ggplot2` v3.4.0.
* `geom_lexis()` deprecates `point_size` argument in favour of `size`.

# ggpointless 0.0.3
* **New** `geom_chaikin()` and `stat_chaikin()` apply Chaikin's corner cutting
algorithm to ragged paths.

# ggpointless 0.0.2
* **New** `geom_lexis()` and `stat_lexis()` draw lexis graphs.
* **New** `female_leaders` dataset available.

# ggpointless 0.0.1
* **New** `geom_pointless()` and `stat_pointless()` emphasise some observations.
* **New** data sets on `covid_vac` and `co2_ml` added.
