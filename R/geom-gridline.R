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
# `%|NA|%` (NA-safe fallback operator) lives in aaa.R.
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

  # Read all line properties from theme(panel.grid.major.x/y / panel.grid.minor.x/y)
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

    # Determine "ink" fallback from theme or default to black
    ink <- if (!is.null(theme)) {
      ggplot2::calc_element("geom", theme)$ink %||% "black"
    } else {
      "black"
    }

    # Helper to resolve a specific grid element's properties
    .resolve_grid <- function(el_name) {
      el <- if (!is.null(theme)) {
        tryCatch(ggplot2::calc_element(el_name, theme), error = function(e) {
          NULL
        })
      } else {
        NULL
      }
      if (is.null(el) || inherits(el, "element_blank")) {
        return(NULL)
      }
      list(
        colour = el$colour,
        linewidth = el$linewidth,
        linetype = el$linetype,
        lineend = el$lineend
      )
    }

    # Resolve all 4 possible axis-specific elements.
    # calc_element handles the fallback hierarchy (e.g. major.x -> major -> line).
    grid_maj <- .resolve_grid("panel.grid.major")
    grid_min <- .resolve_grid("panel.grid.minor")
    m_x <- .resolve_grid("panel.grid.major.x")
    m_y <- .resolve_grid("panel.grid.major.y")
    n_x <- .resolve_grid("panel.grid.minor.x")
    n_y <- .resolve_grid("panel.grid.minor.y")

    # Major X  (colour uses %||%: NA is valid / transparent)
    data$.mj_x_col <- if ("colour" %in% user_set) {
      data$colour
    } else {
      m_x$colour %||% grid_maj$colour %||% ink
    }
    data$.mj_x_lwd <- if ("linewidth" %in% user_set) {
      data$linewidth
    } else {
      m_x$linewidth %|NA|% grid_maj$linewidth %|NA|% 0.5
    }
    data$.mj_x_lty <- if ("linetype" %in% user_set) {
      data$linetype
    } else {
      m_x$linetype %|NA|% grid_maj$linetype %|NA|% 1L
    }
    data$.mj_x_end <- if ("lineend" %in% user_set) {
      data$lineend
    } else {
      m_x$lineend %|NA|% grid_maj$lineend %|NA|% "butt"
    }

    # Major Y
    data$.mj_y_col <- if ("colour" %in% user_set) {
      data$colour
    } else {
      m_y$colour %||% grid_maj$colour %||% ink
    }
    data$.mj_y_lwd <- if ("linewidth" %in% user_set) {
      data$linewidth
    } else {
      m_y$linewidth %|NA|% grid_maj$linewidth %|NA|% 0.5
    }
    data$.mj_y_lty <- if ("linetype" %in% user_set) {
      data$linetype
    } else {
      m_y$linetype %|NA|% grid_maj$linetype %|NA|% 1L
    }
    data$.mj_y_end <- if ("lineend" %in% user_set) {
      data$lineend
    } else {
      m_y$lineend %|NA|% grid_maj$lineend %|NA|% "butt"
    }

    # Minor X
    data$.mn_x_col <- if ("colour" %in% user_set) {
      data$colour
    } else {
      n_x$colour %||% grid_min$colour %||% data$.mj_x_col
    }
    data$.mn_x_lwd <- if ("linewidth" %in% user_set) {
      data$linewidth
    } else {
      n_x$linewidth %|NA|% grid_min$linewidth %|NA|% (data$.mj_x_lwd * 0.5)
    }
    data$.mn_x_lty <- if ("linetype" %in% user_set) {
      data$linetype
    } else {
      n_x$linetype %|NA|% grid_min$linetype %|NA|% data$.mj_x_lty
    }
    data$.mn_x_end <- if ("lineend" %in% user_set) {
      data$lineend
    } else {
      n_x$lineend %|NA|% grid_min$lineend %|NA|% data$.mj_x_end
    }

    # Minor Y
    data$.mn_y_col <- if ("colour" %in% user_set) {
      data$colour
    } else {
      n_y$colour %||% grid_min$colour %||% data$.mj_y_col
    }
    data$.mn_y_lwd <- if ("linewidth" %in% user_set) {
      data$linewidth
    } else {
      n_y$linewidth %|NA|% grid_min$linewidth %|NA|% (data$.mj_y_lwd * 0.5)
    }
    data$.mn_y_lty <- if ("linetype" %in% user_set) {
      data$linetype
    } else {
      n_y$linetype %|NA|% grid_min$linetype %|NA|% data$.mj_y_lty
    }
    data$.mn_y_end <- if ("lineend" %in% user_set) {
      data$lineend
    } else {
      n_y$lineend %|NA|% grid_min$lineend %|NA|% data$.mj_y_end
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

    # Detect orientation.  In coord_flip(), the x-scale is vertical and the
    # y-scale is horizontal.
    is_flipped <- inherits(coord, "CoordFlip")

    # Data-space range for the orthogonal axis — used to span each gridline
    # from edge to edge.  Works in Cartesian and polar alike.
    ranges <- coord$backtransform_range(panel_params)
    grobs <- list()

    # Pre-compute breaks once per axis — result is independent of major/minor type.
    bks_by_ax <- list()
    for (ax in c("x", "y")) {
      if (!ax %in% grids) {
        next
      }
      bks_by_ax[[ax]] <- .gridline_data_breaks(
        ax,
        panel_params,
        coord,
        major,
        minor
      )
    }

    # coord_polar (not coord_radial) renders rays out to npc 0.45 via a
    # `0.45 * sin(theta)` constant, and appends 0.45 to rfine to draw an
    # outer boundary circle (see ggplot2::CoordPolar$render_bg).  r_rescale
    # maps r.range to the donut c(0, 0.4), so the data-space value that
    # maps to npc 0.45 sits at r_range[1] + 1.125 * diff(r_range).  Extend
    # the r-axis span so our rays reach the same outer edge, and append
    # that synthetic position as an extra major break so the boundary
    # circle is drawn too — matching what the theme suppresses.
    if (inherits(coord, "CoordPolar") && !inherits(coord, "CoordRadial")) {
      r_axis <- if (identical(coord$theta, "y")) "x" else "y"
      r_rng <- ranges[[r_axis]]
      outer_extent <- r_rng[1L] + 1.125 * (r_rng[2L] - r_rng[1L])
      ranges[[r_axis]][2L] <- outer_extent
      if (major && r_axis %in% grids && !is.null(bks_by_ax[[r_axis]])) {
        bks_by_ax[[r_axis]]$major <- c(
          bks_by_ax[[r_axis]]$major,
          outer_extent
        )
      }
    }

    # Rendering convention:
    # 1. Major lines are drawn atop minor lines.
    # 2. Y-aesthetic lines are drawn atop X-aesthetic lines.
    # Bottom-to-top order: Minor X -> Minor Y -> Major X -> Major Y.
    for (type in c("minor", "major")) {
      # Skip if this type is not requested
      if (identical(type, "major") && !major) {
        next
      }
      if (identical(type, "minor") && !minor) {
        next
      }

      # Ensure X is processed before Y so Y is on top
      for (ax in c("x", "y")) {
        if (!ax %in% grids) {
          next
        }

        bks <- bks_by_ax[[ax]]
        if (is.null(bks)) {
          next
        }

        vals <- bks[[type]]
        if (length(vals) == 0L) {
          next
        }

        # Pick axis- and type-specific aesthetics stamped in use_defaults()
        prefix <- if (identical(type, "major")) ".mj_" else ".mn_"
        col <- ggplot2::alpha(
          data[[paste0(prefix, ax, "_col")]][1L],
          data$alpha[1L]
        )
        lwd <- data[[paste0(prefix, ax, "_lwd")]][1L]
        lty <- data[[paste0(prefix, ax, "_lty")]][1L]
        lend <- data[[paste0(prefix, ax, "_end")]][1L] %|NA|% "butt"

        # Determine if lines for this aesthetic should be horizontal or vertical.
        # Normal: X = vertical, Y = horizontal
        # Flipped: X = horizontal, Y = vertical
        draw_horizontal <- if (is_flipped) (ax == "x") else (ax == "y")

        grobs <- c(
          grobs,
          list(
            .gridline_seg_grob(
              draw_horizontal,
              vals,
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
# supported or breaks cannot be found.
#
# geom_hline / geom_vline work by setting coordinates in data space and
# delegating to GeomSegment, which calls coord$transform() internally.
# We do the same: read data-space breaks here, draw via GeomSegment below.
.gridline_data_breaks <- function(ax, panel_params, coord, major, minor) {
  # Helper to resolve positions (handles discrete 'pos' attribute)
  .b <- function(x) {
    p <- attr(x, "pos")
    as.numeric(if (!is.null(p)) p else x)
  }

  vs <- panel_params[[ax]]

  # Tier 1: Modern ViewScale (Cartesian, Sf, Flip, Fixed, Trans, Quickmap)
  if (!is.null(vs) && is.function(vs$break_positions)) {
    maj <- if (major) .b(vs$breaks) else numeric(0L)
    maj_dedup <- .b(vs$breaks)
    min <- if (minor) .b(vs$minor_breaks) else numeric(0L)
  } else if (inherits(coord, "CoordPolar") || inherits(coord, "CoordRadial")) {
    # Tier 2: Polar / Radial
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
  } else if (!is.null(panel_params[[paste0(ax, ".major")]])) {
    # Tier 3: Legacy / Map fallback (x.major, y.major style)
    maj <- if (major) {
      as.numeric(panel_params[[paste0(ax, ".major")]])
    } else {
      numeric(0L)
    }
    maj_dedup <- as.numeric(panel_params[[paste0(ax, ".major")]])
    min <- if (minor) {
      as.numeric(panel_params[[paste0(ax, ".minor")]])
    } else {
      numeric(0L)
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
      function(p) any(abs(p - maj_dedup) < 1e-9, na.rm = TRUE),
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
  horizontal,
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
  # Use back-transformed ranges for the orthogonal axis to ensure the lines
  # span the entire panel exactly, matching the default grid.
  if (horizontal) {
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
  # Only pass essential aesthetics to avoid row-count mismatch warnings
  # in coord_polar/coord_munch (Issue: 750 rows to replace 150 rows).
  aes_cols <- c(
    "x",
    "y",
    "xend",
    "yend",
    "colour",
    "linewidth",
    "linetype",
    "alpha",
    "PANEL",
    "group"
  )
  ggplot2::GeomSegment$draw_panel(
    df[, aes_cols],
    panel_params,
    coord,
    lineend = lend
  )
}


#' Lines Drawn on Top of Other Layers
#'
#' @description
#'
#' `geom_gridline()` draws horizontal and vertical lines where grid lines are
#' as a regular ggplot2 layer, so they appear *above* bar charts or any other
#' geom in your plot.
#'
#' The line positions are read directly from the trained scale (via
#' `panel_params`), and the line properties are read from the theme; so
#' `geom_gridline()` always match the grid line positions and properties
#' automatically; but you can overwrite these of course. All built-in ggplot2
#' coordinate systems are supported, including [ggplot2::coord_flip()],
#' [ggplot2::coord_fixed()], [ggplot2::coord_polar()],
#' [ggplot2::coord_radial()], and [ggplot2::coord_sf()].
#' By default the layer also suppresses the equivalent theme grid so the
#' lines don't double up.
#'
#' This was inspired by Observable Plot's Grid mark:
#' <https://observablehq.com/plot/marks/grid#grid-mark>.
#'
#' @section Line properties:
#' By default `geom_gridline()` inherits properties from
#' `theme(panel.grid.major)` (or `panel.grid.minor` when only minor lines are
#' requested), so the on-top lines look exactly like the background grid would.
#' Pass an explicit `colour` to override, see examples.
#'
#' @section Rendering order:
#' `geom_gridline()` follows a specific Z-order convention to ensure
#' maximum visibility:
#' \enumerate{
#'   \item Major grid lines are always drawn **on top** of minor grid lines.
#'   \item Y-aesthetic grid lines are drawn **on top** of X-aesthetic grid
#'         lines.
#' }
#' This means the final drawing sequence (from bottom to top) is: Minor X,
#' Minor Y, Major X, Major Y.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#'   Usually `NULL`: `geom_gridline()` reads break positions from the trained
#'   panel scales rather than from the layer's data, so no mapping is
#'   required.  Pass one only if you want to override a specific aesthetic
#'   (e.g. `aes(colour = ...)`) without setting it via `theme()`.
#' @param data Optional data frame.  Same caveat as `mapping`: not needed in
#'   typical use because the geom derives its positions from the panel
#'   scales, but can be supplied for custom break sources.
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
#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#'   When `suppress = TRUE` (default), a list of a [ggplot2::layer()]
#'   and one or more [ggplot2::theme()] calls that blank the corresponding
#'   theme grid elements. Otherwise a single layer.
#'
#' @seealso [ggplot2::geom_hline()], [ggplot2::geom_vline()] for fixed
#'   reference lines; [ggplot2::theme()] for controlling the underlying
#'   panel grid.
#'
#' @concept gridline
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
#' # Line properties are inherited from theme
#' # their positions from the scale
#' p +
#'   geom_gridline() +
#'   scale_y_continuous(breaks = c(10, 20)) +
#'   theme_gray(paper = "cornsilk", ink = "navy")
#'
#' # When you explicitly set properties in geom_gridline
#' # they will overwrite theme properties
#' p +
#'   geom_gridline(colour = "tomato", major = FALSE, minor = TRUE) +
#'   scale_y_continuous(breaks = c(10, 20)) +
#'   theme_gray(paper = "cornsilk", ink = "navy")
#'
#' # polar coordinates are supported too
#' ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
#'   geom_bar(width = 1) +
#'   geom_gridline(grids = c("x", "y"), minor = TRUE) +
#'   coord_polar(theta = "y")
#'
geom_gridline <- function(
  mapping = NULL,
  data = NULL,
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
  if (!is.null(mapping)) {
    cli::cli_warn(
      "{.fn geom_gridline}: Ignoring {.arg mapping} because gridline positions are read from the panel scale, not from data."
    )
  }
  if (!is.null(data)) {
    cli::cli_warn(
      "{.fn geom_gridline}: Ignoring {.arg data} because gridline positions are read from the panel scale, not from data."
    )
  }

  grids <- .validate_grids(grids)

  # Validate the (scalar) colour override up front so an invalid colour
  # spec fails immediately with a clean cli error, not deep inside grid
  # at draw time.  NULL is honoured as "inherit from theme".
  colour <- .check_colour_arg(colour, "colour", n = 1L)

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

  dots <- rlang::list2(...)
  if (
    !is.null(dots$stat) &&
      !identical(dots$stat, "identity") &&
      !identical(dots$stat, ggplot2::StatIdentity)
  ) {
    cli::cli_warn(
      "{.fn geom_gridline} only supports {.val identity} stat. Ignoring {.arg stat}."
    )
  }

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
      dots
    )
  )

  if (!suppress) {
    return(lyr)
  }

  # Suppress the theme grid for the axes / levels this layer covers so the
  # on-top lines don't double up with the background ones.
  # Minor grid is always suppressed for drawn axes: ggplot2 auto-computes
  # minor breaks whenever >=2 major breaks exist, and the theme would draw
  # those even when minor = FALSE.  Without this, theme minor lines bleed
  # through behind the layer's lines.
  th_args <- list()
  for (ax in grids) {
    if (major) {
      th_args[[paste0("panel.grid.major.", ax)]] <- ggplot2::element_blank()
    }
    # Always suppress the theme's minor grid for axes where we draw something.
    # With >= 2 major breaks ggplot2 auto-computes minor breaks and the theme
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
