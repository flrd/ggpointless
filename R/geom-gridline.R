# geom_gridline() — panel grid lines drawn above other layers.
#
# Inspired by Observable Plot's Grid mark:
# https://observablehq.com/plot/marks/grid
#
# Positions are read directly from panel_params (ViewScale$break_positions()),
# so they always match the trained scale — no manual break specification
# needed.  By default the layer also injects theme() calls to suppress the
# underlying theme grid so the two don't double-up.
#
# Validate and normalise the `grids` argument.
# Removes duplicates and unrecognised values (with a warning), then falls back
# to "y" if nothing valid remains.  Mirrors the pattern of
# .validate_fade_direction() in aaa.R.
#
#' @noRd
#' @keywords internal
.validate_grids <- function(grids) {
  valid <- c("x", "y")
  grids <- unique(grids)

  bad <- setdiff(grids, valid)
  if (length(bad) > 0L) {
    cli::cli_warn(c(
      "!" = "Ignoring invalid {.arg grids} value{?s}: {.val {bad}}.",
      "i" = "Valid options are {.val \"x\"} and {.val \"y\"}."
    ))
    grids <- intersect(grids, valid)
  }

  if (length(grids) == 0L) {
    cli::cli_warn(c(
      "!" = "No valid {.arg grids} values remain after filtering.",
      "i" = 'Falling back to {.val y}.'
    ))
    grids <- "y"
  }

  grids
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomGridline <- ggplot2::ggproto(
  "GeomGridline",
  ggplot2::Geom,

  required_aes = character(0),

  # colour, linewidth, linetype, lineend are in optional_aes so that:
  #  a) explicit overrides from the constructor are recognised without an
  #     "Ignoring unknown parameters" warning, and
  #  b) their absence from data is detectable in use_defaults() so we can
  #     inject values from the theme's panel.grid element(s).
  # alpha lives in default_aes so it always has a concrete NA sentinel.
  optional_aes = c("colour", "linewidth", "linetype", "lineend"),

  default_aes = ggplot2::aes(alpha = NA),

  # Stamp `major` and `minor` onto data so use_defaults() can read them.
  # `major`/`minor` are geom params (not aes params), so they are absent from
  # the `params` argument that use_defaults() receives (which is aes_params).
  # Stamping via setup_data() — which runs before use_defaults() — is the
  # canonical workaround.
  setup_data = function(data, params) {
    data$.major <- isTRUE(params$major)
    data$.minor <- isTRUE(params$minor)
    data
  },

  # Read all line properties from theme(panel.grid.major / panel.grid.minor)
  # for any property the user has not explicitly overridden.
  # use_defaults() is called from compute_geom_2() with the full plot theme
  # (plot@theme), so this is the correct hook — unlike draw_layer/draw_panel,
  # the theme object is available here.
  use_defaults = function(
    self,
    data,
    params = list(),
    modifiers = ggplot2::aes(),
    default_aes = NULL,
    theme = NULL,
    ...
  ) {
    data <- ggplot2::ggproto_parent(ggplot2::Geom, self)$use_defaults(
      data,
      params,
      modifiers,
      default_aes,
      theme = theme,
      ...
    )

    user_set <- names(params)

    # Prefer the more specific element based on what will actually be drawn.
    # Read from data (stamped in setup_data) because major/minor are geom
    # params and are not present in the aes_params received here.
    el_name <- if (isTRUE(data$.minor[1L]) && !isTRUE(data$.major[1L])) {
      "panel.grid.minor"
    } else {
      "panel.grid.major"
    }

    grid_el <- if (!is.null(theme)) {
      tryCatch(ggplot2::calc_element(el_name, theme), error = function(e) NULL)
    } else {
      NULL
    }

    if (!is.null(grid_el) && !inherits(grid_el, "element_blank")) {
      if (!"colour" %in% user_set) {
        data$colour <- grid_el$colour %||% "black"
      }
      if (!"linewidth" %in% user_set) {
        data$linewidth <- grid_el$linewidth %||% 0.5
      }
      if (!"linetype" %in% user_set) {
        data$linetype <- grid_el$linetype %||% 1L
      }
      if (!"lineend" %in% user_set) data$lineend <- grid_el$lineend %||% "butt"
    } else {
      # Grid element is blank or theme unavailable — apply sensible fallbacks.
      ink <- if (!is.null(theme)) {
        ggplot2::calc_element("geom", theme)$ink %||% "black"
      } else {
        "black"
      }
      if (!"colour" %in% user_set) {
        data$colour <- data$colour %||% ink
      }
      if (!"linewidth" %in% user_set) {
        data$linewidth <- data$linewidth %||% 0.5
      }
      if (!"linetype" %in% user_set) {
        data$linetype <- data$linetype %||% 1L
      }
      if (!"lineend" %in% user_set) data$lineend <- data$lineend %||% "butt"
    }

    data
  },

  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    grids = "y",
    major = TRUE,
    minor = FALSE
  ) {
    grids <- .validate_grids(grids)

    col <- ggplot2::alpha(data$colour[1L], data$alpha[1L])
    lwd <- data$linewidth[1L]
    lty <- data$linetype[1L]
    lend <- data$lineend[1L] %||% "butt"

    # Data-space range for the orthogonal axis — used to span each gridline
    # from edge to edge.  Works in Cartesian and polar alike.
    ranges <- coord$backtransform_range(panel_params)
    grobs <- list()

    for (ax in grids) {
      bks <- .gridline_data_breaks(ax, panel_params, coord, major, minor)
      if (is.null(bks)) {
        next
      }

      if (length(bks$major) > 0L) {
        grobs <- c(
          grobs,
          list(
            .gridline_seg_grob(
              ax,
              bks$major,
              ranges,
              col,
              lwd,
              lty,
              lend,
              panel_params,
              coord
            )
          )
        )
      }
      if (length(bks$minor) > 0L) {
        grobs <- c(
          grobs,
          list(
            .gridline_seg_grob(
              ax,
              bks$minor,
              ranges,
              col,
              lwd * 0.5,
              lty,
              lend,
              panel_params,
              coord
            )
          )
        )
      }
    }

    if (length(grobs) == 0L) {
      return(ggplot2::zeroGrob())
    }
    grid::grobTree(children = do.call(grid::gList, grobs))
  },

  draw_key = ggplot2::draw_key_blank
)


# Return a list(major = <numeric>, minor = <numeric>) of break positions in
# *data space* for axis `ax`.  Returns NULL when the coord type is not
# supported (neither Cartesian nor polar).
#
# geom_hline / geom_vline work by setting coordinates in data space and
# delegating to GeomSegment, which calls coord$transform() internally.
# We do the same: read data-space breaks here, draw via GeomSegment below.
.gridline_data_breaks <- function(ax, panel_params, coord, major, minor) {
  vs <- panel_params[[ax]] # ViewScale present in Cartesian, NULL in polar

  if (!is.null(vs) && is.function(vs$break_positions)) {
    # Standard Cartesian: ViewScale carries data-space breaks.
    # Discrete scales store character labels with a `pos` integer attribute;
    # continuous scales are plain numeric.
    .b <- function(x) {
      p <- attr(x, "pos")
      as.numeric(if (!is.null(p)) p else x)
    }
    maj <- if (major) .b(vs$breaks) else numeric(0L)
    maj_dedup <- .b(vs$breaks) # always needed to strip minor overlaps
    min <- if (minor) .b(vs$minor_breaks) else numeric(0L)
  } else if (inherits(coord, "CoordPolar")) {
    # Polar: panel_params uses theta.major/r.major etc. (data-space values).
    # Which axis maps to theta vs r depends on coord$theta ("x" or "y").
    if (ax == coord$theta) {
      maj <- if (major) as.numeric(panel_params$theta.major) else numeric(0L)
      maj_dedup <- as.numeric(panel_params$theta.major)
      min <- if (minor) as.numeric(panel_params$theta.minor) else numeric(0L)
    } else {
      maj <- if (major) as.numeric(panel_params$r.major) else numeric(0L)
      maj_dedup <- as.numeric(panel_params$r.major)
      min <- if (minor) as.numeric(panel_params$r.minor) else numeric(0L)
    }
  } else {
    return(NULL) # Unsupported coord — skip silently
  }

  maj <- maj[!is.na(maj)]
  maj_dedup <- maj_dedup[!is.na(maj_dedup)]
  min <- min[!is.na(min)]

  # minor_breaks() is a superset of breaks() — strip overlapping positions so
  # thin minor lines never sit on top of the major ones.  Use maj_dedup (all
  # major positions) even when major = FALSE so nothing bleeds through.
  if (length(min) > 0L && length(maj_dedup) > 0L) {
    is_maj <- vapply(
      min,
      function(p) any(abs(p - maj_dedup) < 1e-9),
      logical(1L)
    )
    min <- min[!is_maj]
  }

  list(major = maj, minor = min)
}


# Build a minimal segment data frame (data-space coords) and render it via
# GeomSegment$draw_panel so that coord$transform() handles Cartesian *and*
# polar (where constant-y segments become circular arcs via GeomPath).
.gridline_seg_grob <- function(
  ax,
  breaks,
  ranges,
  col,
  lwd,
  lty,
  lend,
  panel_params,
  coord
) {
  n <- length(breaks)
  if (ax == "y") {
    df <- data.frame(
      x = ranges$x[1L],
      xend = ranges$x[2L],
      y = breaks,
      yend = breaks,
      colour = col,
      linewidth = lwd,
      linetype = lty,
      alpha = NA_real_,
      PANEL = 1L,
      group = seq_len(n)
    )
  } else {
    df <- data.frame(
      x = breaks,
      xend = breaks,
      y = ranges$y[1L],
      yend = ranges$y[2L],
      colour = col,
      linewidth = lwd,
      linetype = lty,
      alpha = NA_real_,
      PANEL = 1L,
      group = seq_len(n)
    )
  }
  ggplot2::GeomSegment$draw_panel(df, panel_params, coord, lineend = lend)
}


#' Grid Lines Drawn on Top of Other Layers
#'
#' @description
#' `geom_gridline()` draws (panel grid) lines as a regular ggplot2 layer, so
#' they appear *above* the other geoms in your plot instead of behind them.
#' This is useful when bars, columns, or filled areas would otherwise
#' obscure the grid.
#'
#' Inspired by Observable Plot's Grid mark:
#' <https://observablehq.com/plot/marks/grid#grid-mark>.
#'
#' The grid-line positions are read directly from the trained scale (via
#' `panel_params`), so they always match the axis ticks automatically.
#' By default the layer also suppresses the equivalent theme grid so the
#' lines don't double up.
#'
#' @section Colour:
#' By default `geom_gridline()` inherits the colour of
#' `theme(panel.grid.major)` (or `panel.grid.minor` when only minor lines are
#' requested), so the on-top lines look exactly like the background grid would.
#' Pass an explicit `colour` to override — e.g. `colour = "#FFFFFF"`. The geom
#' creates a "bleed-through" effect where lines cut through e.g. bar fills
#' without being visible against the panel background.
#'
#' @param grids Character vector specifying which "grid" lines to draw:
#'   `"x"`, `"y"` (default), or `c("x", "y")` for both.
#' @param major Draw major grid lines? Default `TRUE`.
#' @param minor Draw minor grid lines? Default `FALSE`.
#' @param colour,linewidth,linetype,lineend Line aesthetics. Default `NULL`
#'   inherits each property from `theme(panel.grid.major)` (or
#'   `panel.grid.minor` when only minor lines are drawn). Pass explicit values
#'   to override individual properties.
#' @param alpha Opacity in `[0, 1]`. Default `NA` (fully opaque).
#' @param suppress Suppress the equivalent theme panel grid so lines do not
#'   double up? Default `TRUE`. When `TRUE`, both the major *and* minor theme
#'   grid elements are blanked for the drawn axes — this prevents ggplot2's
#'   auto-computed minor breaks from showing through when `minor = FALSE`.
#'   Set to `FALSE` to keep the theme grid entirely.
#' @param na.rm If `FALSE` (default) missing values are silently dropped.
#' @param show.legend Logical. Should this layer appear in the legends?
#'   Default `FALSE` (grid lines rarely need a legend entry).
#' @param inherit.aes If `FALSE`, overrides the default aesthetics.
#' @param ... Other arguments passed to [ggplot2::layer()].
#'
#' @return When `suppress = TRUE` (default), a list of a [ggplot2::layer()]
#'   and one or more [ggplot2::theme()] calls that blank the corresponding
#'   theme grid elements. Otherwise a single layer.
#'
#' @seealso [ggplot2::geom_hline()], [ggplot2::geom_vline()] for fixed
#'   reference lines; [ggplot2::theme()] for controlling the underlying
#'   panel grid.
#'
#' @concept grid
#' @concept gridline
#' @concept reference lines
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' # Basic example - geom_gridline() is just another layer
#' # plotted in the order you add them to your ggplot
#' p <- ggplot(mpg, aes(class)) +
#'   geom_bar()
#' p + geom_gridline()
#'
#' # Horizontal bars: flip axes, draw gridlines atop x-grid
#' ggplot(mpg, aes(y = class)) +
#'   geom_bar() +
#'   geom_gridline(grids = "x")
#'
#' # Gridline properties are inherited from theme
#' p +
#'   geom_gridline(grids = c("x", "y"), minor = TRUE) +
#'   theme(panel.grid = element_line(colour = "tomato", linetype = "dashed")) +
#'   scale_y_continuous(breaks = c(10, 20))
#'
#' # Grid properties can be overwritten when you set them
#' # explicitly in geom_gridlines()
#' p +
#'   geom_gridline(grids = c("x", "y"), colour = "black", minor = TRUE) +
#'   theme(panel.grid = element_line(colour = "tomato")) +
#'   scale_y_continuous(breaks = c(10, 20))
#'
#' # polar coordinates
#' ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
#'   geom_bar(width = 1) +
#'   geom_gridline(grids = c("x", "y"), minor = TRUE) +
#'   coord_polar(theta = "y")
#'
geom_gridline <- function(
  grids = "y",
  major = TRUE,
  minor = FALSE,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  lineend = NULL,
  alpha = NA,
  suppress = TRUE,
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = FALSE,
  ...
) {
  grids <- .validate_grids(grids)

  # Fixed-aesthetic overrides: pass non-NULL values as params so they
  # flow through aes_params into use_defaults() where we check names(params)
  # to know which properties the user has explicitly fixed.
  overrides <- Filter(
    Negate(is.null),
    list(
      colour = colour,
      linewidth = linewidth,
      linetype = linetype,
      lineend = lineend
    )
  )

  lyr <- ggplot2::layer(
    data = data.frame(x = 1L),
    mapping = NULL,
    stat = ggplot2::StatIdentity,
    geom = GeomGridline,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(
      list(
        grids = grids,
        major = major,
        minor = minor,
        alpha = alpha,
        na.rm = na.rm
      ),
      overrides,
      rlang::list2(...)
    )
  )

  if (!suppress) {
    return(lyr)
  }

  # Suppress the theme grid for the axes / levels this layer covers so the
  # on-top lines don't double up with the background ones.
  # Minor grid is always suppressed for drawn axes: ggplot2 auto-computes
  # minor breaks whenever ≥2 major breaks exist, and the theme would draw
  # those even when minor = FALSE.  Without this, theme minor lines bleed
  # through behind the layer's lines.
  th_args <- list()
  for (ax in grids) {
    if (major) {
      th_args[[paste0("panel.grid.major.", ax)]] <- ggplot2::element_blank()
    }
    # Always suppress the theme's minor grid for axes where we draw something.
    # With ≥ 2 major breaks ggplot2 auto-computes minor breaks and the theme
    # draws them even when minor = FALSE, causing bleed-through.
    # When major = FALSE and minor = FALSE nothing is drawn at all, so we
    # leave the theme untouched (th_args stays empty → single layer returned).
    if (major || minor) {
      th_args[[paste0("panel.grid.minor.", ax)]] <- ggplot2::element_blank()
    }
  }

  if (length(th_args) == 0L) {
    return(lyr)
  }
  list(lyr, do.call(ggplot2::theme, th_args))
}
