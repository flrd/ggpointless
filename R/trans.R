#' Unit of time transformations
#'
#' Transform a numeric vector assumed to represent either the number of
#' seconds, or the number of days into another unit of time. Supported are
#' second, minute, hour, day, week, year, decade, and century, see examples.
#'
#'
#' @param from a character string, unit of time to convert `from` to `to`,
#' `"day"` by default, see examples
#' @param to a character string, unit of time to convert `to` from `from`,
#' `"decade"` by default, see examples
#' @param ...	Additional arguments passed on to scales::trans_new()
#' @return A new transformation object
#' @seealso \code{\link[scales]{trans_new}}
#'    \code{\link[base]{as.Date}}
#'    \code{\link[base]{DateTimeClasses}}
#' @export
#' @examples
#' # Date class is internally represented as the number
#' # of days since some origin, e.g. 1970-01-01
#' df1 <- data.frame(
#'   x = as.Date("2021-01-01"),
#'   xend = as.Date("2022-01-01")
#' )
#' p <- ggplot(df1, aes(x, xend = xend)) +
#'   geom_lexis()
#' p
#'
#' # wrap a transformer object into `function()` to be
#' # used in scale_*_continuous
#'
#' day_year_trans <- function() {
#'   time_to_time_trans(from = "day", to = "year")
#' }
#'
#' p + scale_y_continuous(trans = day_year_trans())
#' p + scale_y_continuous(trans = "day_year")
#'
#' # DateTime class on the other hand is internally represented as
#' # the number of seconds since some origin
#' df2 <- data.frame(
#'   x = as.POSIXct("2021-01-01"),
#'   xend = as.POSIXct("2022-01-01")
#' )
#'
#' p <- ggplot(df2, aes(x, xend = xend)) +
#'   geom_lexis()
#' p
#'
#' sec_year_trans <- function() {
#'   time_to_time_trans(from = "second", to = "year")
#' }
#'
#' p + scale_y_continuous(trans = "sec_year")
time_to_time_trans <- function(from = c(
                                 "day",
                                 "second"
                               ),
                               to = c(
                                 "century",
                                 "decade",
                                 "year",
                                 "month",
                                 "week",
                                 "day",
                                 "hour",
                                 "minute",
                                 "second"
                               ), ...) {
  force(from)
  force(to)

  from <- match.arg(from)
  to <- match.arg(to)

  unit_of_time <- list(
    second = 1,
    minute = 60,
    hour = 60 * 60,
    day = 60 * 60 * 24,
    week = 60 * 60 * 24 * 7,
    month = 60 * 60 * 24 * 365.25 / 12,
    year = 60 * 60 * 24 * 365.25,
    decade = 60 * 60 * 24 * 365.25 * 10,
    century = 60 * 60 * 24 * 365.25 * 100
  )

  if (from == "day") {
    trans <- function(x) x / 86400 * unit_of_time[[to]]
    inv <- function(x) x * 86400 / unit_of_time[[to]]
  } else {
    trans <- function(x) x * unit_of_time[[to]]
    inv <- function(x) x / unit_of_time[[to]]
  }
  scales::trans_new(
    name = sprintf("%s_to_%s", from, to),
    transform = trans,
    inverse = inv,
    breaks = scales::breaks_extended(),
    format = scales::number_format(),
    ...
  )
}
