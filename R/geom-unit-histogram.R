#' @include geom-unit-bar.R
NULL

#' Unit Chart Histograms
#'
#' @description
#' A unit-cell version of [ggplot2::geom_histogram()]: bins a continuous
#' variable and draws each bin as a stack of unit cells. With `cell_size = 1`
#' (the default) one cell is one observation, so the bar's height reads as a
#' literal count.
#'
#' Like [ggplot2::geom_histogram()] this is a thin convenience wrapper around
#' `stat = "bin"`. Pass `bins` or `binwidth` to control the binning; everything
#' else (rendering, position, padding) follows [geom_unit_bar()] -- see there
#' for `cell_size`, `cell_padding`, `cell_count_cap`, `radius`, the rendering
#' caveats, and the performance notes.
#'
#' @concept unit chart
#' @concept isotype chart
#'
#' @aesthetics GeomUnitBar
#' @aesthetics StatBin
#'
#' @inheritParams geom_unit_bar
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. For
#'   `geom_unit_histogram()`, `x` (or `y` for horizontal histograms) is
#'   required and supplies the continuous variable to be binned. Map `fill`
#'   to colour cell segments.
#' @param bins Number of bins. Overridden by `binwidth`. Defaults to 30.
#'   Forwarded to [ggplot2::stat_bin()].
#' @param binwidth Width of each bin in data units. When supplied, takes
#'   precedence over `bins`. Forwarded to [ggplot2::stat_bin()].
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @seealso [geom_unit_bar()] for the discrete counterparts and the shared
#'   rendering options. [ggplot2::geom_histogram()] for the regular
#'   (non-unit) histogram.
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' # Bin a continuous variable; one cell per observation.
#' # Basic example
#' p <- ggplot(iris, aes(Sepal.Length, fill = Species))
#' p + geom_unit_histogram(bins = 40)
#'
#' # Square cells:
#' # With `cell_size = 1` (the default), the bin width in data units is
#' # the `coord_equal()` ratio that makes cells render as squares
#' bins <- 30
#' bw <- diff(range(iris$Sepal.Length)) / bins
#' cs <- 1 # cell_size's default for demonstration here only
#'
#' # Use bw (bin width) and cs (cell_size) here in coord_equal()
#' p <- p + coord_equal(ratio = bw / cs)
#' p + geom_unit_histogram()
#'
#' # Rounded corners are supported too
#' p + geom_unit_histogram(radius = unit(3, "pt"))
#'
#' # Horizontal orientation, and ratio: 1/2
#' ggplot(iris, aes(y = Sepal.Length, fill = Species)) +
#'   geom_unit_histogram() +
#'   coord_equal(ratio = (1/bw) / 2 ) +
#'   theme(legend.position = "bottom")
#'
#' # When bin counts are large, set `cell_size` so each cell aggregates
#' # multiple observations. Pair with `label_cells()` to relabel the axis.
#' set.seed(1)
#' big <- data.frame(l = rnorm(1e5))
#' bw <- diff(range(big$l)) / 30
#' cs <- 1000
#'
#' ggplot(big, aes(l)) +
#'   geom_unit_histogram(cell_size = cs) +
#'   scale_y_continuous(
#'     breaks = seq(0, 10, 2) * cs,
#'     labels = label_cells(cs, suffix = "k")
#'   ) +
#'   coord_equal(ratio = bw / cs) +
#'   labs(
#'     x = NULL,
#'     y = NULL,
#'     caption = "One cell equals 1.000 observations."
#'   )
#'
geom_unit_histogram <- make_constructor(
  GeomUnitBar,
  stat = "bin",
  position = "stack",
  binwidth = NULL,
  bins = NULL,
  orientation = NA,
  radius = grid::unit(0, "pt"),
  cell_size = 1,
  cell_padding = 0.05,
  cell_count_cap = 1e4
)
