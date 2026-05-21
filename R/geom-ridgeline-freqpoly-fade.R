#' @rdname geom_ridgeline_fade
#' @include geom-ridgeline-fade.R
#'
#' @description
#' `geom_ridgeline_freqpoly_fade()` and `geom_ridgeline_histogram_fade()`
#' are the ridgeline counterparts of [geom_freqpoly_fade()] and
#' [geom_histogram_fade()]. Both bin the `x` aesthetic per categorical
#' `y` baseline; they differ only in how consecutive bins connect:
#' freqpoly draws linear connections between bin midpoints (polyline);
#' histogram draws stepped flat-top rectangles with vertical jumps at
#' the bin edges.
#'
#' @concept fading gradient
#' @concept ridgeline
#'
#' @param bins Number of bins. Overridden by `binwidth`. Defaults to 30.
#'   Forwarded to [ggplot2::stat_bin()].
#' @param binwidth Width of each bin in data units. When supplied, takes
#'   precedence over `bins`. Forwarded to [ggplot2::stat_bin()].
#' @param center,boundary,closed,pad Forwarded to [ggplot2::stat_bin()].
#'   `pad = TRUE` (the default for ridgeline forms) prepends/appends a
#'   zero-count bin at each end so the ribbon touches the baseline cleanly.
#'
#' @export
#' @examples
#' # freqpoly uses stat = "bin"
#' p +
#'   geom_ridgeline_freqpoly_fade(
#'     aes(fill = after_stat(x)),
#'     alpha_scope = "global",
#'     bins = 40
#'   )
geom_ridgeline_freqpoly_fade <- function(
  mapping = NULL,
  data = NULL,
  position = NULL,
  ...,
  bins = 30,
  binwidth = NULL,
  center = NULL,
  boundary = NULL,
  closed = c("right", "left"),
  pad = TRUE,
  alpha_fade_to = 0,
  alpha_scope = "group",
  scale = NULL,
  min_height = NULL,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {
  closed <- match.arg(closed)
  ggplot2::layer(
    data = data,
    mapping = mapping,
    geom = GeomRidgelineFade,
    stat = StatBinRidges, # locked: not user-overridable by design
    position = position %||%
      position_ridgeline(
        scale = scale,
        min_height = min_height
      ),
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      bins = bins,
      binwidth = binwidth,
      center = center,
      boundary = boundary,
      closed = closed,
      pad = pad,
      alpha_fade_to = alpha_fade_to,
      alpha_scope = alpha_scope,
      orientation = orientation,
      na.rm = na.rm,
      ...
    )
  )
}
