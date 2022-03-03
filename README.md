
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggpointless <a href="https://flrd.github.io/ggpointless/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/flrd/ggpointless/branch/main/graph/badge.svg)](https://app.codecov.io/gh/flrd/ggpointless?branch=main)
[![R-CMD-check](https://github.com/flrd/ggpointless/workflows/R-CMD-check/badge.svg)](https://github.com/flrd/ggpointless/actions)
<!-- badges: end -->

Add a minimal accent to your plots. `ggpointless` is an extension of the
[`ggplot2`](https://ggplot2.tidyverse.org/) library making it easy to
add a simple point layer to highlight first, last, minima or maxima
observations with the goal to provide some additional context. Or just
some visual sugar.

## Installation

You can install the development version of `ggpointless` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("flrd/ggpointless")
```

## Usage

There are two functions in this small package: `geom_pointless()`, which
is powered by `stat_pointless()`. `geom_pointless()` behaves like
`geom_point()` does with the addition of a `location` argument. You can
set it to `"first"`, `"last"` (default), `"minimum"`, `"maximum"`, and
`"all"`, where `"all"` is just shorthand to select `"first"`, `"last"`,
`"minimum"` and `"maximum"`.

See the `vignette("ggpointless")` for more details.

``` r
x <- seq(-pi, pi, length.out = 100)
y <- outer(x, 1:5, function(x, y) sin(x*y))

df1 <- data.frame(
  var1 = x,
  var2 = rowSums(y)
)

ggplot(df1, aes(x = var1, y = var2)) +
  geom_line() +
  geom_pointless(aes(color = after_stat(location)),
                 location = "all",
                 size = 3) +
  scale_color_manual(values = c('#f4ae1b', '#d77e7b', '#a84dbd', '#311dfc')) +
  theme_minimal()
```

<img src="man/figures/README-hello_world-1.png" width="100%" style="display: block; margin: auto;" />

## Data

The `ggpointless` package contains two data sets:

1.  `co2_ml` : [CO<sub>2</sub> records taken at Mauna Loa,
    Hawaii](https://gml.noaa.gov/ccgg/trends/data.html)
2.  `covid_vac` : [COVID-19 Cases and Deaths by Vaccination
    Status](https://covid.cdc.gov/covid-data-tracker/#rates-by-vaccine-status)

See the `vignette("examples")` for possible use cases.

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/flrd/ggpointless/blob/master/conduct.md). By
participating in this project you agree to abide by its terms.
