
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggpointless <a href="https://flrd.github.io/ggpointless/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggpointless)](https://CRAN.R-project.org/package=ggpointless)
[![R-CMD-check](https://github.com/flrd/ggpointless/workflows/R-CMD-check/badge.svg)](https://github.com/flrd/ggpointless/actions)
[![Codecov test
coverage](https://codecov.io/gh/flrd/ggpointless/branch/main/graph/badge.svg)](https://app.codecov.io/gh/flrd/ggpointless?branch=main)
<!-- [![Downloads](http://cranlogs.r-pkg.org/badges/ggpointless)](http://www.r-pkg.org/pkg/ggpointless) -->

<!-- badges: end -->

`ggpointless` is an extension of the
[`ggplot2`](https://ggplot2.tidyverse.org/) library providing additional
layers. The following functions are implemented in this small package:

-   `geom_pointless()` / `stat_pointless()`: functions that are making
    it easy to add minimal emphasis to your plots by means of a point
    layer.

-   `geom_lexis()` / `stat_lexis()`: a layer to plot a 45° ‘lifeline’ of
    an event

## Installation

``` r
install.packages("ggpointless")
# Or install the development version
# install.packages("devtools")
devtools::install_github("flrd/ggpointless")
```

## Using ggpointless

Once you have installed the package, simply attach it by calling:

``` r
library(ggpointless)
```

The main functions in this package are `geom_pointless()` and
`geom_lexis()`. They work like you are used to from other `geom_*`
functions.

### geom_pointless

Using the functions `geom_pointless()`, which is a constructor for
`stat_pointless()`, you can highlight the first, or last observations,
sample minimum and maximum with the goal to provide some additional
context. Or just some visual sugar. `geom_pointless()` behaves like
`geom_point()` does with the addition of a `location` argument. You can
set it to `"first"`, `"last"` (default), `"minimum"`, `"maximum"`, and
`"all"`, where `"all"` is just shorthand to select `"first"`, `"last"`,
`"minimum"` and `"maximum"`.

``` r
cols <- c('#f4ae1b', '#d77e7b', '#a84dbd', '#311dfc')
theme_set(theme_minimal())

x <- seq(-pi, pi, length.out = 500)
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
  scale_color_manual(values = cols)
```

<img src="man/figures/README-hello-world-1.png" width="100%" style="display: block; margin: auto;" />

### geom_lexis

`geom_lexis()` is a combination of a segment and a point layer. Given a
start value and an end value, this geom draws a 45° line which indicates
the duration of an event. Required are `x` and `xend` aesthetics, `y`
and `yend` coordinates will be calculated for you.

``` r
df2 <- data.frame(
  key = c("A", "B", "B", "C", "D"),
  x = c(0, 1, 6, 5, 6),
  xend = c(5, 4, 10, 8, 10)
)

ggplot(df2, aes(x = x, xend = xend, color = key)) +
  geom_lexis(aes(linetype = after_stat(type)), size = .5, point_size = 3) +
  coord_equal() +
  scale_x_continuous(breaks = c(df2$x, df2$xend)) +
  scale_color_manual(values = cols) +
  scale_linetype_identity() +
  theme(panel.grid.minor = element_blank())
```

<img src="man/figures/README-geom-lexis-1.png" width="100%" style="display: block; margin: auto;" />

See the
[`vignette("ggpointless")`](https://flrd.github.io/ggpointless/articles/ggpointless.html)
for more details.

## developement version

### geom_chaikin

Chaikin’s corner cutting algorithm in action! Credit to [Farbfetzen /
corner_cutting](https://github.com/Farbfetzen/corner_cutting)

``` r
df3 <- data.frame(x = c(0, 4, 4, 3, 2.5),
                  y = c(0, 0, 2, -1, 4))

ggplot(df3, aes(x, y)) + 
  geom_polygon(fill = NA, linetype = "13", color = '#f4ae1b') + 
  geom_chaikin(color = '#f4ae1b', closed = TRUE) +
  coord_equal()
```

<img src="man/figures/README-geom-chaikin-1.png" width="100%" style="display: block; margin: auto;" />

## Data

The `ggpointless` package contains the following data sets:

1.  `co2_ml` : [CO<sub>2</sub> records taken at Mauna
    Loa](https://gml.noaa.gov/ccgg/trends/data.html)
2.  `covid_vac` : [COVID-19 Cases and Deaths by Vaccination
    Status](https://covid.cdc.gov/covid-data-tracker/#rates-by-vaccine-status)
3.  `female_leaders` : [Elected and appointed female heads of state and
    government](https://en.wikipedia.org/w/index.php?title=List_of_elected_and_appointed_female_heads_of_state_and_government&oldid=1078024588)

See the
[`vignette("examples")`](https://flrd.github.io/ggpointless/articles/examples.html)
for possible use cases.

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/flrd/ggpointless/blob/master/conduct.md). By
participating in this project you agree to abide by its terms.
