# For the stepped/histogram look, we want each bin to render as a
# flat-top rectangle: the ribbon walks horizontally across the bin and
# jumps vertically at bin edges. To get that with `GeomRidgelineFade`
# (which connects consecutive (x, height) points linearly), we expand
# each bin row into TWO rows at xmin and xmax with the same height. The
# resulting polyline naturally walks stepped.

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @include geom-ridgeline-fade.R
#' @include stat-bin-ridges.R
#' @export
GeomRidgelineHistogramFade <- ggplot2::ggproto(
  "GeomRidgelineHistogramFade",
  GeomRidgelineFade,

  setup_data = function(self, data, params) {
    data <- ggplot2::ggproto_parent(GeomRidgelineFade, self)$setup_data(
      data,
      params
    )
    if (nrow(data) == 0L || is.null(data$xmin) || is.null(data$xmax)) {
      return(data)
    }
    expanded <- do.call(
      rbind,
      lapply(split(data, data$group, drop = TRUE), function(g) {
        out <- g[rep(seq_len(nrow(g)), each = 2L), , drop = FALSE]
        out$x <- as.numeric(rbind(g$xmin, g$xmax))
        out
      })
    )
    rownames(expanded) <- NULL
    expanded
  }
)

#' @rdname geom_ridgeline_fade
#'
#' @concept fading gradient
#' @concept ridgeline
#' @concept histogram
#'
#' @export
#' @examples
#' # ridgeline histogram uses stat = "bin" too
#' p +
#'   geom_ridgeline_histogram_fade(
#'     aes(fill = after_stat(x)),
#'     alpha_scope = "global",
#'     bins = 40
#'   )
geom_ridgeline_histogram_fade <- function(
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
    geom = GeomRidgelineHistogramFade,
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
