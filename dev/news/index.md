# Changelog

## ggpointless (development version)

## ggpointless 0.3.0

### New features

- **New**
  [`geom_unit_bar()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_bar.md),
  [`geom_unit_col()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_bar.md),
  and
  [`geom_unit_histogram()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_histogram.md)
  draw isotype / pictogram bar charts, where each bar is a stack of
  discrete unit cells (one cell = one observation by default).
  [`geom_unit_bar()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_bar.md)
  counts observations like
  [`geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.html);
  [`geom_unit_col()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_bar.md)
  uses pre-computed `y` like
  [`geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.html);
  [`geom_unit_histogram()`](https://flrd.github.io/ggpointless/dev/reference/geom_unit_histogram.md)
  bins a continuous variable like
  [`geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html).
  Fractional `y` values produce a partial cell at the outer edge. All
  three work with
  [`coord_equal()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html),
  [`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html),
  [`coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html),
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html),
  and faceting. [\#15](https://github.com/flrd/ggpointless/issues/15)

- **New** `label_cells(cell_size)` helper pairs with `cell_size` to
  relabel the value axis in cell counts instead of raw units: pass it to
  `scale_y_continuous(labels = ...)` so a tick at `y = 200` displays as
  `2` when `cell_size = 100`.

- **New**
  [`geom_gridline()`](https://flrd.github.io/ggpointless/dev/reference/geom_gridline.md)
  draws (panel)grid lines as a layer on top of other geoms. Reads
  positions directly from the trained scales (no manual breaks needed),
  inherits line styling from `theme(panel.grid.major.*)` /
  `panel.grid.minor.*` for any property not explicitly overridden, and
  supports
  [`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
  /
  [`coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html).
  Use `grids = "x" / "y" / c("x", "y")` to pick axes; `minor = TRUE` to
  draw minor lines too. To remove the underlying theme grid, compose
  with `+ theme(panel.grid = element_blank())`.
  [\#12](https://github.com/flrd/ggpointless/issues/12)

- **New**
  [`geom_path_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_path_fade.md),
  [`geom_line_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_path_fade.md),
  and
  [`geom_step_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_path_fade.md)
  draw paths, lines, and step functions with a linear alpha gradient
  along their length, so one or both ends fade to transparent. The
  `fade_direction` argument controls which end(s) fade (`"end"`,
  `"start"`, or `c("start", "end")`); `alpha_fade_to` sets the target
  alpha.

- **New**
  [`geom_segment_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_segment_fade.md)
  draws individual line segments like
  [`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
  but fades each segment along its own direction — the gradient follows
  the segment from `(x, y)` to `(xend, yend)`, so it works at any angle.
  Accepts the same `fade_direction` and `alpha_fade_to` arguments as
  [`geom_path_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_path_fade.md).
  [\#11](https://github.com/flrd/ggpointless/issues/11)

- **New**
  [`geom_curve_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_segment_fade.md)
  draws Bézier curves like
  [`geom_curve()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
  but with an alpha gradient along the curve direction. The fade follows
  from start to end point, so curves fade at any angle. Uses the same
  `fade_direction` and `alpha_fade_to` arguments as
  [`geom_segment_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_segment_fade.md),
  with Porter-Duff compositing for smooth gradients. Falls back to
  semi-transparent curves on unsupported devices.
  [\#11](https://github.com/flrd/ggpointless/issues/11)

- **New**
  [`geom_abline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md),
  [`geom_hline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md),
  and
  [`geom_vline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md)
  draw reference lines (diagonal, horizontal, vertical) with an alpha
  gradient along the line direction. They mirror the `ggplot2`
  annotation pattern: pass `slope`/`intercept`, `yintercept`, or
  `xintercept` directly for constant lines, or supply `data` and
  `mapping` for facet-varying lines. Under non-linear coordinate systems
  ([`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html),
  [`coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html))
  the fade follows the curve that the coord transform produces —
  [`geom_hline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md)
  fades around a circle,
  [`geom_vline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md)
  fades along a ray, and
  [`geom_abline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_abline_fade.md)
  fades along the resulting arc.

- **New**
  [`geom_rect_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_rect_fade.md)
  draws arbitrary rectangles with a linear alpha gradient that fades one
  edge to transparent. The `fade_direction` argument controls the
  gradient direction: `"vertical"` (default) fades from opaque at the
  top to transparent at the bottom; `"horizontal"` fades from opaque at
  the left to transparent at the right. Supports a `radius` argument for
  rounded corners too.
  [\#13](https://github.com/flrd/ggpointless/issues/13)

- **New**
  [`geom_col_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_col_fade.md),
  [`geom_bar_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_col_fade.md),
  and
  [`geom_histogram_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_histogram_fade.md)
  draw bar charts with a vertical alpha gradient that fades from opaque
  at the peak to transparent at the baseline. Additionally these geoms
  support a `radius` to draw bar charts with rounded corners The
  rendering tier is chosen at draw time: gradient fill on capable
  devices, flat semi-transparent fill on
  [`pdf()`](https://rdrr.io/r/grDevices/pdf.html)/[`postscript()`](https://rdrr.io/r/grDevices/postscript.html).
  [\#14](https://github.com/flrd/ggpointless/issues/14)

- **New**
  [`geom_freqpoly_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md)
  draws a filled frequency polygon — the area under the
  [`geom_freqpoly()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)
  line — with the same fading gradient as
  [`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md).
  Paired with
  [`stat_bin()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html),
  so all binning parameters (`bins`, `binwidth`, `center`, `boundary`,
  …) are forwarded.

- **New**
  [`geom_density_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md)
  draws a kernel density estimate with a fading gradient, using
  [`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md)
  paired with
  [`stat_density()`](https://ggplot2.tidyverse.org/reference/geom_density.html).
  Accepts all smoothing parameters (`bw`, `adjust`, `kernel`, `bounds`,
  …).

- **New**
  [`geom_ridgeline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md),
  [`geom_ridgeline_density_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md),
  [`geom_ridgeline_histogram_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md),
  and
  [`geom_ridgeline_freqpoly_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md)
  draw ridgeline plots — overlapping ridge shapes at different vertical
  offsets — with a vertical alpha gradient that fades from opaque at
  each ridge’s peak to transparent at the baseline. The `alpha_scope`
  argument controls how alpha is scaled across ridges.
  [`geom_ridgeline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md)
  takes explicit `(x, y, height)`; the three other forms compute
  `height` from a stat (smooth density, stepped histogram, polyline
  frequency polygon) so users only supply `(x, y)`. All four handle
  negative heights (dips below the baseline) with a bidirectional
  gradient. The `scale` argument defaults to `NULL`, which auto-scales
  the layer so the tallest ridge overlaps its neighbour by ~50% (the
  canonical “mountain range” ridgeline look). The auto-resolved value is
  reported via a
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)
  so you have a starting point if you want to override.
  [\#19](https://github.com/flrd/ggpointless/issues/19)

- **New** `PositionRidgeline` (callable via `position = "ridgeline"`) is
  the position adjustment that converts a `(y, height)` mapping into the
  ribbon-style `(ymin, ymax)` form `GeomRibbon` expects, so each ridge
  sits on its own y-baseline. Used as the default position by
  [`geom_ridgeline_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_ridgeline_fade.md),
  but also composes with vanilla \[ggplot2::geom_ribbon()\] for users
  who want the layout without the fade.

### Breaking changes

- `geom_point_glow(glow_size = X)` now interprets `X` at face value in
  `ggplot2` size units, matching the `size` aesthetic of
  \[ggplot2::geom_point()\]. Previously `X` was wrongfully multiplied by
  3 before rendering, so existing user-supplied values render roughly
  three times smaller than in 0.2.0. The default (`glow_size = NA`) is
  unchanged and still renders at nine times the point’s `size`.

- Removed `geom_lexis(point_size = ...)`, deprecated since 0.1.0 in
  favour of `size`. Code still passing `point_size` will now error with
  `unused argument`; rename the argument to `size`.

### Bug fixes and improvements

- [`stat_chaikin()`](https://flrd.github.io/ggpointless/dev/reference/geom_chaikin.md)
  no longer renders nothing without a message when the smoothed data has
  missing values in different rows of `(x, y)` than in a numeric extra
  aesthetic (e.g. `aes(fill = z)` where `z` has its own NAs). Previously
  each numeric column was de-NA’d independently, the resulting
  per-column outputs had different lengths, the assignment errored, and
  `ggplot2`’s stat machinery silently swallowed the error — leaving an
  empty layer. The stat now pre-filters complete cases across all
  numeric columns once and warns about dropped rows in the standard
  `ggplot2` way (suppress via `na.rm = TRUE`).
  [\#18](https://github.com/flrd/ggpointless/issues/18)

- [`stat_fourier()`](https://flrd.github.io/ggpointless/dev/reference/geom_fourier.md)
  /
  [`stat_catenary()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  /
  [`stat_arch()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  no longer crash `ggplot2`’s limit expansion when paired with
  `coord_transform(y = "log10")` (and other restricted-domain transforms
  such as `"log"`, `"sqrt"`). The stats now drop those rows with a
  single helpful warning that points users at
  [`scale_y_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html),
  which transforms before the stat runs and avoids the issue entirely.

- [`stat_pointless()`](https://flrd.github.io/ggpointless/dev/reference/geom_pointless.md)
  /
  [`geom_pointless()`](https://flrd.github.io/ggpointless/dev/reference/geom_pointless.md):
  when a single observation matches multiple `location` criteria —
  e.g. the last point is also the maximum — `after_stat(location)` now
  carries a composite label (e.g. `"last, maximum"`) instead of silently
  dropping the secondary labels. Previously only the first matching
  label in iteration order was kept. Row order still follows the order
  given in `location` (canonical `"first"`, `"last"`, `"minimum"`,
  `"maximum"` for `"all"`).

- [`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md):
  now renders alpha gradient when `+ coord_flip()` is applied.
  Previously the geom did not render any fill without a message or
  warning shown to users.
  [\#17](https://github.com/flrd/ggpointless/issues/17)

- [`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md):
  on devices without gradient support (base
  [`pdf()`](https://rdrr.io/r/grDevices/pdf.html),
  [`postscript()`](https://rdrr.io/r/grDevices/postscript.html)) the
  informational message now fires for solid-fill plots too — users were
  previously silent about the lost vertical fade unless `fill` was
  mapped to a variable. Wording is tailored to what was actually lost
  (colour gradient vs. vertical fade). Devices that support gradients
  but not compositing still stay silent for solid-fill plots (tier 2
  renders the vertical fade faithfully).

- [`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md):
  the `global_max_abs` scan that drives `alpha_scope = "global"` now
  handles `Date` and `POSIXct` value axes. Previously both failed
  [`is.numeric()`](https://rdrr.io/r/base/numeric.html) and silently
  fell back to `global_max = 1`; plots built through the normal scale
  pipeline were unaffected (values are already numeric by draw time),
  but direct `draw_panel()` calls with raw Date / POSIXct ymax are now
  robust.

- [`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md):
  the legend key (`.draw_key_area_fade()`) now validates `alpha_fade_to`
  via the shared `.check_alpha_fade_to()` helper, matching the
  validation used by `setup_params()`. A guide that constructs a key
  outside the normal setup pipeline now aborts on out-of-range values
  instead of silently producing an invalid gradient.

- [`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md):
  fixed alpha overflow for stacked areas with `alpha_scope = "global"`
  (the default). The reference max was computed from the pre-stacking
  `y` values, so the top ribbon’s alpha exceeded 1 and was silently
  clamped to fully opaque, defeating the fade. The reference is now
  taken from post-position-adjustment data, so equal rendered `|y|` maps
  to equal opacity as documented. No effect on `position = "identity"`
  or `alpha_scope = "group"`.

- [`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md):
  now accepts both integer and floating-point `alpha_fade_to` values
  (e.g. `0L`, `0.5`). Previously integer input was rejected.

- [`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md):
  fixed `has_outline` check to handle
  [`coord_polar()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
  without crashing when outline colour is a vector.

- [`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md):
  fixed duplicate `comp_stops` that could arise when values were clipped
  to `[0, 1]` bounds (e.g. when `val_hi` is far outside the panel).
  Identical stops are now de-duplicated before gradient construction.

- [`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md)
  under
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
  now rotates its alpha gradient to follow the rendered visual axis,
  instead of staying vertical and visually disappearing against
  horizontal bars. The gradient is now anchored to the rendered geometry
  via `flipped_visual = xor(flipped_aes, CoordFlip)`, so
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
  and `orientation = "y"` produce equivalent output.

- New custom legend key glyphs for
  [`geom_fourier()`](https://flrd.github.io/ggpointless/dev/reference/geom_fourier.md)
  (sine wave),
  [`geom_catenary()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  (hanging curve), and
  [`geom_arch()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  (arch curve) provide custom visual cues in the legend.

- All `draw_key_*()` functions now consistently use
  [`ggplot2::gg_par()`](https://ggplot2.tidyverse.org/reference/gg_par.html)
  instead of [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html) for
  proper theme resolution in `ggplot2` v4.0+.

## ggpointless 0.2.0

CRAN release: 2026-03-09

### New features

- **New**
  [`geom_fourier()`](https://flrd.github.io/ggpointless/dev/reference/geom_fourier.md)
  and
  [`stat_fourier()`](https://flrd.github.io/ggpointless/dev/reference/geom_fourier.md)
  fit a truncated Fourier series (via
  [`stats::fft()`](https://rdrr.io/r/stats/fft.html)) to `x`/`y` data
  and render the reconstructed curve. Supports optional detrending
  (`"lm"` or `"loess"`) and harmonic selection via `n_harmonics`
  ([\#7](https://github.com/flrd/ggpointless/issues/7)).

- **New**
  [`geom_arch()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  and
  [`stat_arch()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  draw inverted catenary curves (arches) between successive points,
  complementing the existing
  [`geom_catenary()`](https://flrd.github.io/ggpointless/dev/reference/geom_catenary.md)
  ([\#4](https://github.com/flrd/ggpointless/issues/4)).

- **New**
  [`geom_area_fade()`](https://flrd.github.io/ggpointless/dev/reference/geom_area_fade.md)
  draws area charts where the fill colour fades from opaque to
  transparent using
  [`grid::linearGradient()`](https://rdrr.io/r/grid/patterns.html). The
  fade target alpha is controlled via `alpha_fade_to`
  ([\#3](https://github.com/flrd/ggpointless/issues/3)).

- **New**
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

- The package now requires R \>= 4.2.0 and `ggplot2` \>= 4.0.0. Several
  geoms take (mostly internal) advantage of new `ggplot2` features such
  as
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

- **New**
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

- **New**
  [`geom_chaikin()`](https://flrd.github.io/ggpointless/dev/reference/geom_chaikin.md)
  and
  [`stat_chaikin()`](https://flrd.github.io/ggpointless/dev/reference/geom_chaikin.md)
  apply Chaikin’s corner cutting algorithm to ragged paths.

## ggpointless 0.0.2

CRAN release: 2022-06-08

- **New**
  [`geom_lexis()`](https://flrd.github.io/ggpointless/dev/reference/geom_lexis.md)
  and
  [`stat_lexis()`](https://flrd.github.io/ggpointless/dev/reference/geom_lexis.md)
  draw lexis graphs.
- **New** `female_leaders` dataset available.

## ggpointless 0.0.1

CRAN release: 2022-03-08

- **New**
  [`geom_pointless()`](https://flrd.github.io/ggpointless/dev/reference/geom_pointless.md)
  and
  [`stat_pointless()`](https://flrd.github.io/ggpointless/dev/reference/geom_pointless.md)
  emphasise some observations.
- **New** data sets on `covid_vac` and `co2_ml` added.
