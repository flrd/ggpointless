#' @include geom-col-fade.R
NULL

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomHistogramFade <- ggplot2::ggproto(
  "GeomHistogramFade",
  GeomColFade,

  # Histogram-specific scope vocabulary: drops `"x"` / `"y"` (which
  # `.scope_max_abs_vec()` keys on `round(data$x|y)` -- meaningless for a
  # continuous binned axis) and adds `"bin"` (per-bin normalisation, where
  # each cluster of dodged bars in the same bin shares an alpha range).
  setup_params = \(self, data, params) {
    params <- ggplot2::ggproto_parent(ggplot2::GeomBar, self)$setup_params(
      data,
      params
    )
    # Catch the common "carried over from `geom_col_fade()`" mistake before
    # `arg_match0` runs: users who reach for `"x"` / `"y"` on a histogram
    # almost always want per-bin normalisation. Point them at `"bin"`
    # explicitly rather than letting `arg_match0` list every option flat.
    if (isTRUE(params$alpha_scope %in% c("x", "y"))) {
      cli::cli_abort(c(
        '{.arg alpha_scope} = {.val {params$alpha_scope}} is not accepted by \\
         {.fn geom_histogram_fade} because the binned axis is continuous, not \\
         discrete.',
        "i" = 'Did you mean {.arg alpha_scope} = {.val bin}? It normalises \\
               bars within each bin -- analogous to \\
               {.code geom_col_fade(alpha_scope = "x")} on a discrete x-axis.'
      ))
    }
    params <- .fade_setup_params(
      params,
      scopes = c("bar", "group", "bin", "fill", "colour", "global"),
      default_scope = "bar"
    )
    params$radius <- .validate_radius(params$radius)
    params
  },

  # `position_dodge()` mutates `data$x` (or `data$y` under
  # `orientation = "y"`) to offset bars within a bin, destroying any
  # post-position reference to "which bin am I in". `setup_data` runs
  # PRE-position, so we capture the bin centre here in a private column
  # that the position adjustment leaves untouched.
  # `.scope_max_abs_vec()` uses this column when `alpha_scope = "bin"`.
  setup_data = \(self, data, params) {
    data <- ggplot2::ggproto_parent(GeomColFade, self)$setup_data(
      data,
      params
    )
    flipped <- isTRUE(params$flipped_aes)
    data$.bin_id <- if (flipped) data$y else data$x
    data
  }
)

#' @title Histograms with Fading Gradient
#' @description
#' Visualise the distribution of a single continuous variable as a histogram
#' with a fading alpha gradient. Counts are drawn with rounded,
#' gradient-filled bars (like [geom_col_fade()] paired with
#' [ggplot2::stat_bin()]). Accepts all binning parameters forwarded to
#' [ggplot2::stat_bin()] (`bins`, `binwidth`, `center`, `boundary`, ...).
#'
#' @concept histogram
#' @concept fading gradient
#'
#' @section Aesthetics:
#' `geom_histogram_fade()` understands the same aesthetics as
#' [geom_col_fade()] (it is `GeomHistogramFade`, a subclass of `GeomColFade`,
#' paired with [ggplot2::stat_bin()]). See `?geom_col_fade` for the full
#' aesthetics table.
#'
#' @inheritSection ggplot2::geom_histogram Orientation
#'
#' @inheritParams geom_col_fade
#' @inheritParams ggplot2::geom_histogram
#' @param alpha_scope How to choose the per-bar reference height that the
#'   gradient normalises against. The histogram family's vocabulary differs
#'   from [geom_col_fade()] / [geom_bar_fade()] because `x` is continuous
#'   (a binned variable) rather than a discrete category:
#'   * `"bar"` (default), `"group"`, `"fill"`, `"colour"`, `"global"` -- same
#'     meaning as in [geom_col_fade()].
#'   * `"bin"` -- every bar in the same bin shares an alpha range. Useful
#'     under `position = "dodge"` for highlighting the tallest group within
#'     each bin (e.g. "which species dominates each Sepal.Width bin").
#'
#'   The `"x"` / `"y"` scopes accepted by [geom_col_fade()] are **not**
#'   available here -- on a continuous binned axis they would key on
#'   `round(data$x)`, which buckets bins by integer rounding rather than
#'   by bin identity. Use `"bin"` for the per-bin meaning.
#' @param bins Number of bins. Overridden by `binwidth`. Defaults to 30.
#'   Forwarded to [ggplot2::stat_bin()].
#' @param binwidth Width of each bin in data units. When supplied, takes
#'   precedence over `bins`. Forwarded to [ggplot2::stat_bin()].
#' @param stat Use to override the default connection between
#'   `geom_histogram_fade()`/`geom_freqpoly_fade()` and `stat_bin()`.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @seealso [geom_col_fade()] / [geom_bar_fade()] for the bar-chart equivalents,
#'   [geom_area_fade()] for the general area-fade geom,
#'   [ggplot2::geom_histogram()] and [ggplot2::geom_freqpoly()] for the
#'   non-fading originals.
#'
#' @references
#' Murrell, P. (2022). "Vectorised Pattern Fills in R Graphics." Technical
#' Report 2022-01, Department of Statistics, The University of Auckland.
#' Version 1.
#' \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/vecpat/vecpat.html}
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' # By default each bar has its own alpha scope
#' p <- ggplot(faithful, aes(waiting))
#' p + geom_histogram_fade()
#'
#' # when all bars shall share the same alpha scope,
#' # set alpha_scope = "global"
#' p +
#'   geom_histogram_fade(
#'     alpha_scope = "global",
#'     alpha = 0.75,
#'     alpha_fade_to = 0.1,
#'     radius = unit(3, "pt"),
#'     colour = "#333333"
#'   ) +
#'   theme_minimal()
#'
#' # Stacked histogram with groups
#' ggplot(iris, aes(Sepal.Length, fill = Species)) +
#'   geom_histogram_fade(alpha_fade_to = 0.25) +
#'   theme_minimal()
#'
#' # Stacked histogram with groups and global alpha scope
#' ggplot(iris, aes(Sepal.Length, fill = Species)) +
#'   geom_histogram_fade(
#'     alpha_fade_to = 0.25,
#'     alpha_scope = "global"
#'   )
#'
#' # Per-fill scope under position = "dodge": each fill cluster has its own
#' # alpha range, so the tallest sub-bar in every bin reaches full opacity.
#' ggplot(iris, aes(Sepal.Width, fill = Species)) +
#'   geom_histogram_fade(
#'     position = "dodge",
#'     bins = 10,
#'     alpha_scope = "fill"
#'   )
#'
geom_histogram_fade <- make_constructor(
  GeomHistogramFade,
  stat = "bin",
  position = "stack",
  binwidth = NULL,
  bins = NULL,
  alpha_fade_to = 0,
  alpha_scope = "bar",
  orientation = NA,
  radius = grid::unit(0, "pt")
)
