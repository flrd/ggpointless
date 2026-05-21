# ggpointless (development version)

## Breaking changes

* The exported Position class `PositionRidgeLine` has been renamed to
  `PositionRidgeline` (consistent with ggplot2's naming convention:
  `PositionStack`, `PositionDodge`). User-facing code should use
  `position_ridgeline()` — unaffected — but direct references to the
  ggproto class must be updated.

* `geom_point_glow(glow_size = X)` now interprets `X` at face value in
  ggplot2 size units, matching the `size` aesthetic of
  [ggplot2::geom_point()]. Previously `X` was silently multiplied by 3
  before rendering, so existing user-supplied values render roughly
  three times smaller than in 0.2.0. The default (`glow_size = NA`) is
  unchanged and still renders at nine times the point's `size`.

## New features

* **New** `geom_unit_bar()`, `geom_unit_col()`, and `geom_unit_histogram()`
  draw isotype / pictogram bar charts, where each bar is a stack of discrete
  unit cells (one cell = one observation by default). `geom_unit_bar()` counts
  observations like `geom_bar()`; `geom_unit_col()` uses pre-computed `y` like
  `geom_col()`; `geom_unit_histogram()` bins a continuous variable like
  `geom_histogram()`. Fractional `y` values produce a partial cell at the
  outer edge. All three work with `coord_equal()`, `coord_polar()`,
  `coord_radial()`, `coord_flip()`, and faceting.

* **New** `cell_size` parameter on the `geom_unit_*()` family controls how
  many data units one cell represents. Default `1`. Setting `cell_size = 1e6`
  (for example) aggregates one million units into each cell, so `y = 25e6`
  produces 25 cells instead of 25 million rects. Essential for large counts.

* **New** `label_cells(cell_size)` helper pairs with `cell_size` to relabel
  the value axis in cell counts instead of raw units: pass it to
  `scale_y_continuous(labels = ...)` so a tick at `y = 2e6` displays as `2`
  when `cell_size = 1e6`.

* **New** `cell_count_cap` parameter on the `geom_unit_*()` family is a defensive
  soft cap on the total cell count per panel (default `1e4`). When exceeded,
  the layer falls back to plain solid bars and emits a warning pointing the
  user at `cell_size`. Pass `cell_count_cap = Inf` to disable. Prevents the
  graphics device from hanging on accidentally large inputs.

* **New** `draw_key_unit()` is the default legend key for the
  `geom_unit_*()` family. Renders a 2 × 2 grid of small cells so the
  legend advertises the unit-cell character of the geom instead of a
  plain solid rectangle.

* **New** `geom_bar_fade()` and `geom_col_fade()` now support `coord_polar()`
  and `coord_radial()`. Each ring or wedge receives an annular radial alpha
  gradient — transparent at the inner edge, opaque at the outer rim — using
  viewport clipping paths. All three `alpha_scope` modes (`"bar"`, `"group"`,
  `"global"`) work in polar coordinates. Falls back to a solid mid-alpha
  polygon on devices that do not support clipping paths or radial gradients.

* **New** `geom_path_fade()`, `geom_line_fade()`, and `geom_step_fade()` draw
  paths, lines, and step functions with a linear alpha gradient along their
  length, so one or both ends fade to transparent. `geom_line_fade()` sorts
  observations by x before drawing (like `geom_line()`); `geom_step_fade()`
  draws staircase-step paths (like `geom_step()`) with a `direction` argument
  (`"hv"`, `"vh"`, or `"mid"`). The `fade_direction` argument controls which
  end(s) fade (`"end"`, `"start"`, or `c("start", "end")`); `alpha_fade_to`
  sets the target alpha. The `alpha_mode` argument controls rendering: `"step"`
  (default, fast) uses discrete per-segment alpha steps; `"gradient"` uses
  per-segment Porter-Duff compositing for smooth continuous fades. Both require
  a device that supports it (e.g. `ragg::agg_png()`, `svg()`); on unsupported
  devices step mode falls back to semi-transparent lines with an informational
  message.

* **New** `geom_segment_fade()` draws individual line segments like
  `geom_segment()` but fades each segment along its own direction — the
  gradient follows the segment from `(x, y)` to `(xend, yend)`, so it works at
  any angle. Accepts the same `fade_direction` and `alpha_fade_to` arguments as
  `geom_path_fade()`. Falls back to unfaded `geom_segment()` rendering on
  non-linear coordinate systems and to semi-transparent segments on devices
  without compositing support.

* **New** `geom_curve_fade()` draws Bézier curves like `geom_curve()` but with
  an alpha gradient along the curve direction. The fade follows from start to
  end point, so curves fade at any angle. Uses the same `fade_direction` and
  `alpha_fade_to` arguments as `geom_segment_fade()`, with Porter-Duff
  compositing for smooth gradients. Falls back to semi-transparent curves on
  unsupported devices.

* **New** `geom_abline_fade()`, `geom_hline_fade()`, and `geom_vline_fade()`
  draw reference lines (diagonal, horizontal, vertical) with an alpha gradient
  along the line direction. They mirror the ggplot2 annotation pattern: pass
  `slope`/`intercept`, `yintercept`, or `xintercept` directly for constant
  lines, or supply `data` and `mapping` for facet-varying lines. Under
  non-linear coordinate systems (`coord_polar()`, `coord_radial()`) the fade
  follows the curve that the coord transform produces — `geom_hline_fade()`
  fades around a circle, `geom_vline_fade()` fades along a ray, and
  `geom_abline_fade()` fades along the resulting arc.

* `geom_segment_fade()` now fades under non-linear coordinate systems. The
  user-supplied endpoints are connected by a chord in device space (matching
  `geom_segment()`) and the fade is applied along that chord. Use the
  reference-line geoms above if you want the fade to follow a curve instead.

* **New** `geom_rect_fade()` draws rectangles (like `geom_rect()`) with a
  linear alpha gradient that fades one edge to transparent. The `fade_direction`
  argument controls the gradient direction: `"vertical"` (default) fades from
  opaque at the top to transparent at the bottom; `"horizontal"` fades from
  opaque at the left to transparent at the right. Supports a `radius` argument
  for rounded corners (default `unit(0, "pt")`, i.e. square). `alpha_fade_to`
  controls the target alpha at the fading edge. Under `coord_polar()` /
  `coord_radial()` the rectangle becomes an annular segment; a radial fade is
  rendered when the fade direction aligns with the radial axis
  (`fade_direction = "vertical"` with `theta = "x"`, or
  `fade_direction = "horizontal"` with `theta = "y"`). Angular (theta-aligned)
  fades are not yet supported — `grid` has no conic-gradient primitive — and
  fall back to a flat `geom_rect()` render with a one-time warning.

* **New** `geom_col_fade()`, `geom_bar_fade()`, and `geom_histogram_fade()` 
  draw bar charts with a vertical alpha gradient that fades from opaque at
  the peak to transparent at the baseline. Additionally these geoms support a
  `radius` to draw bars with rounded corners.
  The `alpha_scope` argument for bar-fade geoms (`geom_col_fade()`,
  `geom_bar_fade()`, `geom_histogram_fade()`) controls how alpha is scaled
  across bars:
    - `"bar"` (default): every bar gets the full alpha range independently.
    - `"group"`: alpha is scaled per position (stack); each stack independently
      uses the full alpha range. Most useful with `position = "stack"`.
    - `"global"`: alpha is scaled relative to the tallest bar in the entire
      panel. For both `"group"` and `"global"`, alpha at each edge of a bar
      segment is based on its absolute axis position, so stacked segments high
      up the axis stay opaque.
  The rendering tier is chosen at draw time: gradient fill on capable devices, flat
  semi-transparent fill on `pdf()`/`postscript()`.

* **New** `geom_freqpoly_fade()` draws a filled frequency polygon — the area under
  the `geom_freqpoly()` line — with the same fading gradient as
  `geom_area_fade()`. Paired with `stat_bin()`, so all binning parameters
  (`bins`, `binwidth`, `center`, `boundary`, …) are forwarded.

* **New** `geom_density_fade()` draws a kernel density estimate with a fading
  gradient, using `geom_area_fade()` paired with `stat_density()`. Accepts all
  smoothing parameters (`bw`, `adjust`, `kernel`, `bounds`, …).

* **New** `geom_ridgeline_fade()` and `geom_ridgeline_density_fade()` draw ridgeline
  plots — overlapping ridge shapes at different vertical offsets — with a vertical
  alpha gradient that fades from opaque at each ridge's peak to transparent at the
  baseline. The `alpha_scope` argument controls how alpha is scaled across ridges.
  `geom_ridgeline_density_fade()` is a convenience wrapper that computes kernel density
  estimates automatically. Both handle negative heights (dips below the baseline) with
  a bidirectional gradient. Inspired by the
  [ggridges](https://wilkelab.org/ggridges/) package.

## Bug fixes and improvements

* `geom_line_fade()` / `geom_path_fade()`: single-segment paths (two
  observations) are now always rendered as a smooth gradient, even when
  `alpha_mode = "step"` (the default). Previously a two-point line collapsed
  to a single uniform mid-opaque stroke because step mode assigns each
  segment its endpoint-mean alpha — with only one segment, no fade was
  visible. Multi-segment paths keep step's cheaper rendering.

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

* `geom_gridline()` now matches `coord_polar(theta = "y")`'s theme grid: rays
  extend to the outer boundary and the boundary circle is drawn. Previously
  rays stopped short of the outer edge and no boundary circle was emitted, so
  layering `geom_gridline()` on top of a plot with the theme grid suppressed
  produced a visibly different result. `coord_radial()` already rendered
  correctly and is unaffected.

* `stat_pointless()` / `geom_pointless()`: when a single observation matches
  multiple `location` criteria — e.g. the last point is also the maximum —
  `after_stat(location)` now carries a composite label (e.g. `"last, maximum"`)
  instead of silently dropping the secondary labels. Previously only the first
  matching label in iteration order was kept. Row order still follows the order
  given in `location` (canonical `"first"`, `"last"`, `"minimum"`, `"maximum"`
  for `"all"`).

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

* New custom legend key glyphs for `geom_fourier()` (sine wave),
  `geom_catenary()` (hanging curve), and `geom_arch()` (arch curve) provide
  custom visual cues in the legend.

* All `draw_key_*()` functions now consistently use `ggplot2::gg_par()` instead
  of `grid::gpar()` for proper theme resolution in ggplot2 v4.0+.

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
