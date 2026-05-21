## Release summary

This is a feature release (0.3.0) with new geometries/stats:
the `geom_unit_*` family (isotype / pictogram bars), the
`geom_*_fade` family (path, line, step, segment, curve, abline,
hline, vline, col, bar, histogram, density, freqpoly, ridgeline,
rect), `geom_gridline()` for drawing panel grid lines as a layer,
and supporting helpers (`label_cells()`, `draw_key_unit()`,
`draw_key_lexis()`).

## Breaking changes (since 0.2.0)

* `geom_point_glow(glow_size = X)` now interprets `X` at face value in
  ggplot2 size units. Previously `X` was silently multiplied by 3.
  The default (`glow_size = NA`) still renders at nine times the
  point's `size`.

## Notes for the reviewer

* The base `pdf()` and `postscript()` devices have an upstream R
  heap-corruption bug at `dev.off()` once enough clipping-viewport or
  gradient-pattern operations accumulate (reproducible with pure
  `grid` on R 4.6.0 — no ggpointless code involved). To keep
  examples / vignettes safe, every fade geom that draws via clipping
  or gradients now detects the pdf/postscript device in its
  `makeContent` and routes through a flat per-segment fallback
  instead. Other devices (`ragg`, `cairo`, `svg`, `png`, …) keep the
  full smooth-gradient rendering. `geom_point_glow` keeps radial
  gradients (it has no flat alternative); its examples are
  intentionally tiny and most are wrapped in `\donttest{}`.

* DESCRIPTION migrated from `RoxygenNote: 7.3.3` to
  `Config/roxygen2/version: 8.0.0`; all `man/*.Rd` files were
  regenerated. Diffs are purely cosmetic (consistent link styling,
  indented bullet handling).

* The CRAN incoming-feasibility check on win-builder flags five words
  in DESCRIPTION as possibly misspelled: "Chaikin" / "Chaikin's"
  (eponymous corner-cutting algorithm), "catenary" (mathematical
  term), "geoms" (ggplot2 vocabulary), and "isotype" (the data-
  visualisation tradition originated by Otto Neurath), "ridgelines".
  All are spelled correctly.

## R CMD check results

Local: 0 errors | 0 warnings | 0 notes
win-builder R-devel: 0 errors | 0 warnings | 1 NOTE
  (possibly misspelled words — see Notes for the reviewer above)

## Test environments

* local Ubuntu 24.04, R 4.6.0 (2026-05-21), ggplot2 4.0.3 — Status: OK
* win-builder R-devel (2026-05-19 r90065 ucrt, re-run 2026-05-21) — Status: 1 NOTE (spelling, see above)
* win-builder R-release (R 4.6.0, 2026-04-24 ucrt, re-run 2026-05-21) — Status: 1 NOTE (spelling, see above)
* macOS builder (sonoma-arm64, macOS Tahoe 26.2, R 4.6.0 Patched 2026-04-24 r89963) — Status: OK (re-run 2026-05-21)

## Reverse dependencies

There are no reverse dependencies.
