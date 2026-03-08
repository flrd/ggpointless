#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomCatenary <- ggplot2::ggproto(
  "GeomCatenary",
  ggplot2::GeomLine,
  stat = "catenary"
)

#' @title Catenary Curves and Arches
#'
#' @description
#' `geom_catenary()` draws a catenary curve (hanging chain) between
#' successive points. `geom_arch()` draws an inverted catenary curve and
#' is hence intended for people living on the southern hemisphere.
#'
#' The shape follows the catenary equation:
#' \eqn{\ y = a\ \cosh \ \!\bigl(\frac{x - h}{a}\bigr) + v}.
#'
#' @param chain_length Numeric vector of physical chain lengths. Recycled to
#'   the number of segments. If `NULL` and `sag` is also `NULL`, defaults to
#'   twice the Euclidean distance per segment. Can be mixed with `sag` by
#'   placing `NA` in the appropriate positions.
#' @param arch_length Numeric vector of arch lengths. Recycled to the number
#'   of segments. If `NULL` and `arch_height` is also `NULL`, defaults to
#'   twice the Euclidean distance per segment. Can be mixed with
#'   `arch_height` by placing `NA` in the appropriate positions.
#' @param sag Numeric vector giving the vertical drop of the curve below
#'   the **lowest endpoint** of each segment. Takes precedence over
#'   `chain_length` when both are supplied for the same segment.
#' @param arch_height Numeric vector giving the vertical rise of the arch
#'   above the **highest endpoint** of each segment. Takes precedence
#'   over `arch_length` when both are supplied for the same segment.
#' @param chainLength `r lifecycle::badge("deprecated")` Use
#'   `chain_length` instead.
#' @inheritParams ggplot2::geom_path
#'
#' @aesthetics GeomCatenary
#' @param geom,stat Override the default connection between `geom_catenary()`
#'   and `stat_catenary()`, or between `geom_arch()` and `stat_arch()`.
#'
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'
#' @seealso
#'   [geom_fourier()] for fitting smooth curves to data via Fourier series,
#'   [geom_chaikin()] for smoothing paths via corner cutting.
#'   The catenary equation is described at
#'   <https://en.wikipedia.org/wiki/Catenary>.
#'
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(x = seq_len(4), y = c(1, 1, 0, 2))
#'
#' # basic usage
#' p <- ggplot(df, aes(x, y)) + ylim(-3, NA) + geom_point(size = 3)
#' p + geom_catenary()
#'
#' # Catenary with sag = 2, considered from lowest point of each segment
#' # recycled, if only a one value is provided
#' p + geom_catenary(sag = 2)
#' p + geom_catenary(sag = c(2, 1, 1))
#'
#' # if sag and chain_length are provided for same segment(s), sag wins
#' p + geom_catenary(sag = c(2, 1, NA), chain_length = 10)
#'
#' # Arch with height = 2, considered from highest point of each segment
#' p + geom_arch(arch_height = c(2, 1, 1))
#'
#' # Rice house, see https://en.wikipedia.org/wiki/Rice_House,_Eltham
#' rice_house <- data.frame(x = c(0, 1.5, 2.5, 3.5, 5), y = c(0, 1, 1, 1, 0))
#' ggplot(rice_house, aes(x, y)) +
#'   geom_arch(arch_height = .15, lwd = 2) +
#'   geom_segment(aes(xend = x, yend = 0)) +
#'   geom_hline(yintercept = 0, colour = "forestgreen", linewidth = 3) +
#'   coord_equal()
#' @export
geom_catenary <- function(
  mapping = NULL,
  data = NULL,
  stat = "catenary",
  position = "identity",
  ...,
  chain_length = NULL,
  sag = NULL,
  chainLength = lifecycle::deprecated(),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (lifecycle::is_present(chainLength)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "ggpointless::geom_catenary(chainLength)",
      "ggpointless::geom_catenary(chain_length)"
    )
    if (!is.null(chain_length)) {
      cli::cli_inform(
        "Note that {.arg chain_length} wins over deprecated {.arg chainLength}. \\
        Use {.arg chain_length}.",
        .frequency = "regularly",
        .frequency_id = "catenary_chain_length_wins"
      )
    }
    chain_length <- chain_length %||% chainLength
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCatenary,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      chain_length = chain_length,
      sag = sag,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_catenary
#' @export
geom_arch <- make_constructor(
  GeomCatenary,
  stat = "arch",
  arch_length = NULL,
  arch_height = NULL
)
