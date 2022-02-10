
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggpointless

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/flrd/ggpointless/branch/main/graph/badge.svg)](https://app.codecov.io/gh/flrd/ggpointless?branch=main)
[![R-CMD-check](https://github.com/flrd/ggpointless/workflows/R-CMD-check/badge.svg)](https://github.com/flrd/ggpointless/actions)
<!-- badges: end -->

The package provides a simple point layer to emphazise some observations
in your data.

## Installation

You can install the development version of ggpointless from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("flrd/ggpointless")
```

## Usage

There are two functions in the `ggpointless` package:
`geom_pointless()`, which is powered by `stat_pointless()`. Both
functions add a layer to a `ggplot` object; a point layer by default.

``` r
# dummy data
x <- seq(-pi, pi, length.out = 100)
y <- outer(x, 1:5, FUN = \(x, y) sin(x*y)) |> 
  rowSums()

df1 <- data.frame(
  var1 = x,
  var2 = y
)

# plot
ggplot(df1, aes(x = var1, y = var2)) +
  geom_line() +
  geom_pointless(
    aes(colour = after_stat(location)),
    location = "all",
    size = 2.5) +
  theme_void()
```

<img src="man/figures/README-hello_world-1.png" width="90%" style="display: block; margin: auto;" />

## Motivation

Laziness. I found myself trying to recreate this fantastic plot from
Gregor Aisch on [Carbon dioxide concentration over
time](https://blog.datawrapper.de/weekly-chart-carbon-dioxide/).

More examples and details can be found in the package’s [vignette]().

## Related work

The package [`ggpmisc`](https://exts.ggplot2.tidyverse.org/ggpmisc.html)
provides the stat `stat_peaks`, which calls the `peaks()` function from
the [`splus2r`](https://github.com/spkaluzny/splus2r) package. With
these functions / stats you are able to find and hence highlight *local*
minima and maxima too, besides global extrema.
