#' @title Key glyphs for legends
#'
#' @description
#' Each geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These functions are called `draw_key_*()`, where
#' `*` stands for the name of the respective key glyph. The key glyphs can be
#' customised for individual geoms by providing a geom with the `key_glyph`
#' argument (see [`layer()`] or examples below).
#'
#' @return A grid grob.
#' @inheritParams ggplot2::draw_key
#'
#' @export
#' @keywords internal
draw_key_fourier <- function(data, params, size) {
  x <- seq(0.1, 0.9, length.out = 30)
  # Sine wave: one full period, centered vertically at 0.5 NPC
  y <- 0.5 + 0.35 * sin(2 * pi * (x - 0.1) / 0.8)

  grid::linesGrob(
    x,
    y,
    gp = ggplot2::gg_par(
      col = alpha(data$colour %||% "black", data$alpha),
      lwd = data$linewidth %||% 0.5,
      lty = data$linetype %||% 1,
      lineend = "round"
    )
  )
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFourier <- ggproto(
  "GeomFourier",
  ggplot2::GeomLine,
  draw_key = draw_key_fourier
)

#' @title Fourier Series Smoothing
#'
#' @description
#' `geom_fourier()` and `stat_fourier()` fit a truncated Fourier (discrete
#' Fourier transform, DFT) series to the supplied `x`/`y` observations and render
#' the reconstructed smooth curve. The data are first aggregated at duplicate
#' `x` positions, interpolated to a uniform grid, optionally de-trended,
#' transformed via [stats::fft()], and then reconstructed from the requested
#' number of harmonics.
#'
#' @concept curve smoothing
#' @concept discrete fourier series
#'
#' @section Period convention:
#' The DFT treats the input as one period of an infinitely repeating signal.
#' The correct period for \eqn{N} uniformly-spaced samples with spacing
#' \eqn{\Delta x} is \eqn{P = N \cdot \Delta x}, not \eqn{x_{max} -
#' x_{min}}.  Using the latter (a closed interval) implicitly maps the last
#' sample to \eqn{t = 1}, which coincides with \eqn{t = 0} of the next
#' period, causing a boundary discontinuity and Gibbs-phenomenon ringing
#' whenever the first and last `y` values differ. This implementation uses
#' the half-open period.
#'
#' @section Detrending:
#' Before the FFT is applied the data can be de-trended so that slow,
#' non-periodic trends do not dominate the low-frequency coefficients:
#' \describe{
#'   \item{`NULL` (default)}{No de-trending; the raw signal is transformed.}
#'   \item{`"lm"`}{Subtract a global ordinary-least-squares linear fit.}
#'   \item{`"loess"`}{Subtract a LOESS smooth. Falls
#'     back to `"lm"` with a message if the group is too small for LOESS
#'     (fewer than 4 observations).}
#' }
#' The trend is added back before the final curve is returned, so the output
#' is always on the original y-scale.
#'
#' @section Nyquist limit:
#' The maximum number of harmonics recoverable from \eqn{N} observations is
#' \eqn{\lfloor N/2 \rfloor}. Requesting more triggers a message and the
#' limit is used instead.
#'
#' @section Irregular spacing:
#' The input data is linearly interpolated onto a uniform grid before the FFT.
#' If the original x-spacing is highly irregular (e.g. monthly time series data),
#' the interpolation may introduce artefacts in sparse regions. A message is
#' emitted when the coefficient of variation of the x-spacing exceeds `0.5`.
#'
#' @inheritParams ggplot2::geom_line
#' @param n_harmonics Integer or NULL. Number of Fourier harmonics to
#'   retain.  NULL (default) uses all harmonics up to the Nyquist limit,
#'   giving an interpolating fit.  Smaller values produce smoother curves.
#' @param detrend Character string or `NULL`. De-trending method applied
#'   before the FFT; one of `"lm"`, `"loess"`, or `NULL` (default). See
#'   the *Detrending* section for details.
#'
#' @aesthetics GeomFourier
#' @param geom,stat Override the default connection between `geom_fourier()`
#'   and `stat_fourier()`.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @seealso
#'   [stats::fft()] for the underlying Fast Fourier Transform,
#'   [lm()] and [loess()] for the optional detrending fits,
#'   [geom_catenary()] and [geom_chaikin()] for other curve-fitting geoms.
#'
#' @examples
#' library(ggplot2)
#'
#' n <- 50
#' df1 <- data.frame(
#'   x = seq(0, 1, length.out = n),
#'   y = sin(seq(0, 2 * pi, length.out = n)) + rnorm(n, sd = 0.2)
#' )
#'
#' # Basic usage - Interpolating fit (all harmonics)
#' p <- ggplot(df1, aes(x, y)) +
#'   geom_point(alpha = 0.5)
#' p + geom_fourier()
#'
#' # Use 1 harmonic only
#' p + geom_fourier(n_harmonics = 1)
#'
#' # De-trending a linearly drifting signal
#' set.seed(2)
#' x <- seq(0, 4 * pi, length.out = n)
#' df2 <- data.frame(
#'   x = x,
#'   y = sin(x) + x * 0.3 + rnorm(n, sd = 0.15)
#' )
#'
#' ggplot(df2, aes(x, y))  +
#' geom_point(alpha = 0.35) +
#'   geom_fourier(aes(colour = "detrend = NULL"), n_harmonics = 3) +
#'   geom_fourier(aes(colour = "detrend = \"lm\""), n_harmonics = 3,
#'                detrend = "lm")
#'
#' # Multiple groups
#' set.seed(3)
#' x <- seq(0, 2 * pi, length.out = n/2)
#' df3 <- rbind(
#'   data.frame(x = x, y = sin(x) + rnorm(n / 2, sd = 0.2), grp = "sine"),
#'   data.frame(x = x, y = cos(x) + rnorm(n / 2, sd = 0.2), grp = "cosine")
#' )
#'
#' ggplot(df3, aes(x, y, colour = grp)) +
#'   geom_point(alpha = 0.5) +
#'   geom_fourier()
#'
#' # When the data is not uniformly-spaced, the Fourier
#' # curve will not hit every data point exactly
#' ggplot(head(economics, 25), aes(date, unemploy)) +
#'   geom_fourier() +
#'   geom_point()  +
#'   geom_curve_fade(
#'     data = data.frame(
#'       x    = as.Date("1967-10-01"),
#'       xend = as.Date("1968-01-01"),
#'       y    = 2750,
#'       yend = 2850
#'     ),
#'     aes(x = x, xend = xend, y = y, yend = yend),
#'     arrow = arrow(),
#'     colour = "tomato"
#'     )
#'
#' # ... in extreme cases a warning is emitted
#' df4 <- data.frame(
#'   x = c(1:10, 19:20),
#'   y = sin(seq_len(12))
#' )
#'
#' ggplot(df4, aes(x, y)) +
#'   geom_point() +
#'   geom_fourier()
#'
#' @export
geom_fourier <- make_constructor(
  GeomFourier,
  stat = "fourier",
  n_harmonics = NULL,
  detrend = NULL
)
