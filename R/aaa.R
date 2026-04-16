#' Base ggproto classes for ggpointless
#'
#' If you are creating a new geom, stat, position, or scale in another package,
#' you'll need to extend from `ggplot2::Geom`, `ggplot2::Stat`,
#' `ggplot2::Position`, or `ggplot2::Scale`.
#'
#' More info at [ggplot2-book.org](https://ggplot2-book.org/)
#'
#' @seealso [ggplot2::ggproto()]
#' @keywords internal
#' @name ggpointless-ggproto
NULL



# Validate and normalise `fade_direction`.
#
# Accepts a character vector with values "start" and/or "end".
# Removes duplicates and invalid entries (with a warning), then falls back
# to "start" if nothing valid remains.
#
# @param fade_direction Character vector.
# @return Character vector containing unique valid values.
#' @noRd
#' @keywords internal
.validate_fade_direction <- function(fade_direction) {
  valid <- c("start", "end")
  fade_direction <- unique(fade_direction)

  bad <- setdiff(fade_direction, valid)
  if (length(bad) > 0L) {
    cli::cli_warn(c(
      "!" = "Ignoring invalid {.arg fade_direction} value{?s}: {.val {bad}}.",
      "i" = "Valid options are {.str start} and {.str end}."
    ))
    fade_direction <- intersect(fade_direction, valid)
  }

  if (length(fade_direction) == 0L) {
    fade_direction <- "start"
  }

  fade_direction
}


# Validate `alpha_fade_to` inside setup_params.
#
# Accepts integer and double scalars in [0, 1].  Aborts with a
# cli-formatted message when the value is out of range, non-finite, or
# not scalar numeric.
#
# @param value The value to validate (typically `params$alpha_fade_to`).
# @return The value, invisibly (for use in pipes or assignments).
#' @noRd
#' @keywords internal
.check_alpha_fade_to <- function(value) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      value < 0 ||
      value > 1
  ) {
    cli::cli_abort(c(
      "{.arg alpha_fade_to} must be a single finite number in {.code [0, 1]}.",
      "x" = "Got {.val {value}} instead."
    ))
  }
  invisible(value)
}
