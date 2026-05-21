#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPointless <- ggproto(
  "StatPointless",
  Stat,
  setup_params = \(data, params) {
    # Validate location parameter
    valid_locations <- c("first", "last", "minimum", "maximum", "all")
    invalid <- setdiff(params$location, valid_locations)
    if (length(invalid) > 0L) {
      cli::cli_warn(c(
        "{cli::qty(invalid)}Ignoring invalid {.arg location} value{?s}: {.val {invalid}}.",
        "i" = "Must be one or more of: {.val {valid_locations}}."
      ))
      params$location <- intersect(params$location, valid_locations)
      if (length(params$location) == 0L) {
        params$location <- "last"
      }
    }

    # Note: previously emitted a "single-observation groups" warning here.
    # Removed: a lone point that simultaneously matches first/last/min/max
    # is a perfectly valid use case (and the composite-label feature was
    # designed for it), so the warning was actively misleading on minimal
    # example data.

    params
  },

  setup_data = \(data, params) {
    if (nrow(data) == 0L) {
      return(data.frame(x = numeric(0), y = numeric(0)))
    }
    data
  },

  extra_params = c("na.rm", "location"),

  compute_group = \(data, scales, location = "last") {
    # scale_y_reverse() negates y before the stat runs, so the numerical
    # minimum becomes the data maximum and vice-versa.  Temporarily un-negate
    # to find the correct extremes, then restore the transformed y values so
    # the points render at the right positions on the reversed axis.
    if (!is.null(scales$y) && isTRUE(scales$y$trans$name == "reverse") &&
        any(c("minimum", "maximum", "all") %in% location)) {
      data_orig <- data
      data_orig$y <- -data$y
      result <- get_locations(data_orig, location = location)
      result$y <- -result$y
      return(result)
    }
    get_locations(data, location = location)
  },

  required_aes = c("x", "y")
)

#' @return A [ggplot2::layer()] object that can be added to a [ggplot2::ggplot()].
#' @export
#' @rdname geom_pointless
stat_pointless <- make_constructor(StatPointless, geom = "point")

#' @keywords internal
#' @noRd
get_locations <- function(data, location = "last") {
  # Expand "all" to canonical order (this order determines z-order)
  if ("all" %in% location) {
    locations <- c("first", "last", "minimum", "maximum")
  } else {
    locations <- location
  }

  y_values <- data$y
  non_na_idx <- which(!is.na(y_values))

  # Handle all-NA case
  if (length(non_na_idx) == 0L) {
    cli::cli_warn(
      "All {.field y} values are {.val NA}; no points to highlight."
    )
    data$location <- factor(
      character(0L),
      levels = c("first", "last", "minimum", "maximum")
    )
    return(data[0L, , drop = FALSE])
  }

  # Pre-compute indices for each location type
  y_non_na <- y_values[non_na_idx]
  y_min <- min(y_non_na)
  y_max <- max(y_non_na)

  location_map <- list(
    first = non_na_idx[1L],
    last = non_na_idx[length(non_na_idx)],
    minimum = non_na_idx[y_non_na == y_min],
    maximum = non_na_idx[y_non_na == y_max]
  )

  # Collect every location label that applies to each observation, so a
  # point that matches multiple criteria (e.g. the last point that is also
  # the maximum) is emitted once with a composite label like "last, maximum".
  # Row order follows the order of `locations`, which drives draw order and
  # legend order.
  result_rows <- integer(0L)
  labels_per_row <- list()

  for (loc in locations) {
    for (idx in location_map[[loc]]) {
      pos <- match(idx, result_rows)
      if (is.na(pos)) {
        result_rows <- c(result_rows, idx)
        labels_per_row <- c(labels_per_row, list(loc))
      } else {
        labels_per_row[[pos]] <- c(labels_per_row[[pos]], loc)
      }
    }
  }

  result_locations <- vapply(
    labels_per_row,
    \(x) paste(x, collapse = ", "),
    character(1L)
  )

  # Build result data frame
  result <- data[result_rows, , drop = FALSE]
  result$location <- factor(result_locations, levels = unique(result_locations))
  rownames(result) <- NULL

  result
}
