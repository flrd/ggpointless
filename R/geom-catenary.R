#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomCatenary <- ggplot2::ggproto("GeomCatenary", ggplot2::GeomLine, stat = "catenary")

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomArch <- ggplot2::ggproto("GeomArch", ggplot2::GeomLine, stat = "arch")

#' @title Catenary Curves and Arches
#'
#' @description
#' `geom_catenary()` draws a catenary curve (hanging chain) between
#' successive points. `geom_arch()` is made for people living on the
#' southern hemisphere as it draws an inverted catenary curve.
#'
#' The shape follows the catenary equation
#' \eqn{y = a \cosh\!\bigl(\frac{x - h}{a}\bigr) + v}.
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
#'
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(x = 0:2, y = c(1, 0, 1))
#'
#' # Catenary with sag = 2, considered from lowest point of each segment
#' ggplot(df, aes(x, y)) +
#'   geom_catenary(sag = 2) +
#'   geom_point()
#'
#' # Arch with height = 2, considered from highest point of each segment
#' ggplot(df, aes(x, y)) +
#'   geom_arch(arch_height = c(2, 1))
#'
#' # stat_arch() paired with a different geom
#' ggplot(df, aes(x, y)) +
#'   stat_arch(arch_height = 2, geom = "point_glow", colour = "tomato")
#'
#' # Rice house, https://en.wikipedia.org/wiki/Rice_House,_Eltham
#' rice_house <- data.frame(x = c(0, 2, 3, 4, 6), y = c(0, 1, 1, 1, 0))
#' ggplot(rice_houses, aes(x, y)) +
#'   geom_arch(arch_height = .2, lwd = 2) +
#'   geom_segment(aes(xend = x, yend = 0)) +
#'   geom_hline(yintercept = 0) +
#'   coord_equal()
#' @export
geom_catenary <- function(mapping = NULL,
                          data = NULL,
                          stat = "catenary",
                          position = "identity",
                          ...,
                          chain_length = NULL,
                          sag = NULL,
                          chainLength = deprecated(),
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  if (lifecycle::is_present(chainLength)) {
    lifecycle::deprecate_warn("0.2.0",
                              "geom_catenary(chainLength)",
                              "geom_catenary(chain_length)")
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
geom_arch <- function(mapping = NULL,
                      data = NULL,
                      stat = "arch",
                      position = "identity",
                      ...,
                      arch_length = NULL,
                      arch_height = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArch,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arch_length = arch_length,
      arch_height = arch_height,
      na.rm = na.rm,
      ...
    )
  )
}
