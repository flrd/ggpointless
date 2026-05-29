# CRAN watch: the geom_curve_fade() `shape` runtime guard ---------------------
#
# `GeomCurve` only accepts a `shape` draw parameter from ggplot2 4.1.0 (PR
# #6523). While 4.1.0 is not on CRAN, ggpointless keeps `Depends: ggplot2
# (>= 4.0.0)` and forwards `shape` to the parent only when the installed
# ggplot2 supports it (see `.geom_curve_parent_draw()`).
#
# This test watches CRAN and FAILS once ggplot2 >= 4.1.0 ships, so the guard
# and the relaxed pin don't silently outlive their purpose. It is network-only:
# skipped on CRAN and when offline, so it never affects a CRAN check.

test_that("drop the geom_curve_fade `shape` guard once ggplot2 >= 4.1.0 is on CRAN", {
  skip_on_cran()
  skip_if_offline()

  db <- tryCatch(
    utils::available.packages(repos = "https://cloud.r-project.org"),
    error = function(e) NULL
  )
  skip_if(
    is.null(db) || !"ggplot2" %in% rownames(db),
    "Could not read the ggplot2 version from CRAN."
  )

  cran_ver <- package_version(db["ggplot2", "Version"])

  if (cran_ver >= package_version("4.1.0")) {
    fail(paste0(
      "ggplot2 ", cran_ver, " is on CRAN (>= 4.1.0). The geom_curve_fade() ",
      "`shape` runtime guard is now obsolete -- clean up:\n",
      "  1. R/geom-curve-fade.R: delete `.geom_curve_parent_draw()` and call ",
      "`ggplot2::GeomCurve$draw_panel(..., shape = shape)` directly in all ",
      "three fallback branches.\n",
      "  2. DESCRIPTION: bump `Depends:` to `ggplot2 (>= 4.1.0)`.\n",
      "  3. R/geom-curve-fade.R: drop the explicit `@param shape` and let ",
      "`@inheritParams ggplot2::geom_curve` carry it; add `xspline` to ",
      "inst/WORDLIST.\n",
      "  4. Remove this test."
    ))
  } else {
    succeed()
  }
})
