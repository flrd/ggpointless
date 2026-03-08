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

    # Check for single-observation groups
    if (nrow(data) > 0L && !anyDuplicated(data$group)) {
      cli::cli_warn(
        "Each group consists of only one observation. \\
         Do you need to adjust the {.field group} aesthetic?"
      )
    }

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
    get_locations(data, location = location)
  },

  required_aes = c("x", "y")
)

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

  # Build result by iterating through requested locations
  # This preserves user-specified order and handles duplicates correctly
  result_rows <- integer(0L)
  result_locations <- character(0L)

  for (loc in locations) {
    indices <- location_map[[loc]]
    # Only add indices we haven't seen yet (prevents overplotting)
    new_indices <- setdiff(indices, result_rows)
    if (length(new_indices) > 0L) {
      result_rows <- c(result_rows, new_indices)
      result_locations <- c(result_locations, rep(loc, length(new_indices)))
    }
  }

  # Build result data frame
  result <- data[result_rows, , drop = FALSE]
  result$location <- factor(result_locations, levels = locations)
  rownames(result) <- NULL

  result
}
