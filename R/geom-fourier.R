#' @title Fourier Series Smoothing
#'
#' @description
#' geom_fourier() and stat_fourier() fit a truncated Fourier (discrete
#' Fourier transform, DFT) series to the supplied x`/`y data and render
#' the reconstructed smooth curve.  The data are first aggregated at duplicate
#' x positions, interpolated to a uniform grid, optionally de-trended,
#' transformed via [stats::fft()], and then reconstructed from the requested
#' number of harmonics.
#'
#' @section Period convention:
#' The DFT treats the input as one period of an infinitely repeating signal.
#' The correct period for \eqn{N} uniformly-spaced samples with spacing
#' \eqn{\Delta x} is \eqn{P = N \cdot \Delta x}, not \eqn{x_{max} -
#' x_{min}}.  Using the latter (a closed interval) implicitly maps the last
#' sample to \eqn{t = 1}, which coincides with \eqn{t = 0} of the next
#' period, causing a boundary discontinuity and Gibbs-phenomenon ringing
#' whenever the first and last y values differ.  This implementation always
#' uses the correct half-open period.
#'
#' @section Detrending:
#' Before the FFT is applied the data can be de-trended so that slow,
#' non-periodic trends do not dominate the low-frequency coefficients:
#' \describe{
#'   \item{`NULL` (default)}{No de-trending; the raw signal is transformed.}
#'   \item{`"lm"`}{Subtract a global ordinary-least-squares linear fit.}
#'   \item{`"loess"`}{Subtract a LOESS smooth (requires \pkg{stats}).  Falls
#'     back to "lm" with a message if the group is too small for LOESS
#'     (fewer than 4 observations).}
#' }
#' The trend is added back before the final curve is returned, so the output
#' is always on the original y-scale.
#'
#' @section Nyquist limit:
#' The maximum number of harmonics recoverable from \eqn{N} observations is
#' \eqn{\lfloor N/2 \rfloor}.  Requesting more triggers a message and the
#' limit is used instead.
#'
#' @section Irregular spacing:
#' The input data is linearly interpolated onto a uniform grid before the FFT.
#' If the original x-spacing is highly irregular (e.g. most points clustered
#' in a narrow region), the interpolation may introduce artefacts in sparse
#' regions.  A message is emitted when the coefficient of variation of the
#' x-spacing exceeds 0.5.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' @param data The data to be displayed in this layer.
#' @param position    Position adjustment; default "identity".
#' @param na.rm       If TRUE, missing values and non-finite values are
#'   silently removed per group.  If FALSE (default), a message is emitted
#'   and the group is skipped.
#' @param show.legend Logical; whether to include this layer in the legend.
#' @param inherit.aes If FALSE, override the default aesthetics.
#' @param n_harmonics Integer or NULL.  Number of Fourier harmonics to
#'   retain.  NULL (default) uses all harmonics up to the Nyquist limit,
#'   giving an interpolating fit.  Smaller values produce smoother curves.
#' @param detrend Character string or `NULL`. De-trending method applied
#'   before the FFT; one of "lm", "loess", or NULL (default). See
#'   the *Detrending* section for details.
#' @param geom The geometric object used to render the layer; defaults
#'   to "line" for stat_fourier().
#' @param ... Additional arguments passed to [ggplot2::layer()].
#'
#' @return A ggplot2 layer object.
#'
#' @examples
#' library(ggplot2)
#'
#' set.seed(1)
#' df <- data.frame(
#'   x = seq(0, 1, length.out = 60)[-60],          # half-open grid
#'   y = sin(seq(0, 2 * pi, length.out = 60)[-60]) + rnorm(59, sd = 0.2)
#' )
#'
#' # Basic usage – Interpolating fit (all harmonics)
#' ggplot(df, aes(x, y)) +
#'   geom_point(alpha = 0.4) +
#'   geom_fourier()
#'
#' # Use 5 harmonics
#' ggplot(df, aes(x, y)) +
#'   geom_point(alpha = 0.4) +
#'   geom_fourier(n_harmonics = 5)
#'
#' # – De-trending a linearly drifting signal
#' set.seed(2)
#' x <- seq(0, 4 * pi, length.out = 80)
#' df2 <- data.frame(
#'   x = x,
#'   y = sin(x) + x * 0.3 + rnorm(80, sd = 0.15)
#' )
#'
#' ggplot(df2, aes(x, y)) +
#'   geom_point(alpha = 0.4) +
#'   geom_fourier(detrend = "lm", colour = "seagreen")
#'
#' # – Multiple groups –––––––––––––––––––––––––––––
#' set.seed(3)
#' x <- seq(0, 2 * pi, length.out = 50)
#' df3 <- rbind(
#'   data.frame(x = x,
#'              y = sin(x) + rnorm(50, sd = 0.2),
#'              grp = "sine"),
#'   data.frame(x = x,
#'              y = cos(x) + rnorm(50, sd = 0.2),
#'              grp = "cosine")
#' )
#'
#' ggplot(df3, aes(x, y, colour = grp)) +
#'   geom_point(alpha = 0.4) +
#'   geom_fourier(n_harmonics = 3)
#'
#' @rdname geom_fourier
#' @export
geom_fourier <- function(mapping = NULL,
                         data = NULL,
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         n_harmonics = NULL,
                         detrend = NULL,
                         ...) {
  ggplot2::layer(
    geom = ggplot2::GeomLine,
    stat = StatFourier,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      n_harmonics = n_harmonics,
      detrend = detrend,
      ...
    )
  )
}
