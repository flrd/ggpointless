# Per-(group, y-baseline) binning stat for ridgeline histogram / freqpoly.
#
# StatBin's setup_params hard-rejects (x AND y) because both are positional;
# for ridgelines we WANT y as the categorical baseline and x as the variable
# to bin. We bypass the parent's check by stripping y from the data we pass
# to setup_params, then in compute_group strip y locally, delegate to
# StatBin's binning logic on x only, and re-attach y as the baseline.

#' @noRd
#' @keywords internal
StatBinRidges <- ggplot2::ggproto(
  "StatBinRidges",
  ggplot2::StatBin,

  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(
    weight = 1,
    height = ggplot2::after_stat(count)
  ),

  setup_params = function(self, data, params) {
    data_x <- data
    data_x$y <- NULL
    ggplot2::StatBin$setup_params(data_x, params)
  },

  compute_group = function(
    self,
    data,
    scales,
    ...,
    binwidth = NULL,
    bins = NULL,
    center = NULL,
    boundary = NULL,
    closed = c("right", "left"),
    pad = FALSE
  ) {
    y_baseline <- data$y[1L]
    data_x_only <- data
    data_x_only$y <- NULL

    binned <- ggplot2::ggproto_parent(ggplot2::StatBin, self)$compute_group(
      data_x_only,
      scales,
      ...,
      binwidth = binwidth,
      bins = bins,
      center = center,
      boundary = boundary,
      closed = closed,
      pad = pad
    )

    binned$y <- y_baseline
    binned$height <- binned$count
    binned
  }
)
