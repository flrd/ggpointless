#' Unit of time transformations
#'
#' Transform a numeric vector assumed to represent either the number of
#' seconds, or the number of days into seconds, minutes, hours, days,
#' week, year, or decade.
#'
#' @param from a character string, unit of time to convert `from` to `to`,
#' `"day"` by default, see examples
#' @param to a character string, unit of time to convert `to` from `from`,
#' `"decade"` by default, see examples
#' @examples
#' df1 <- data.frame(x = as.Date("2021-01-01"), xend = as.Date("2022-01-01"))
#' p <- ggplot(df1, aes(x, xend = xend)) +
#'   geom_lexis()
#' p + scale_y_continuous("days")
#' p + scale_y_continuous("weeks", trans = day_week_trans())
#' @export
time_to_time_trans <- function(from = c(
                                 "day",
                                 "second"
                               ),
                               to = c(
                                 "decade",
                                 "year",
                                 "month",
                                 "week",
                                 "day",
                                 "hour",
                                 "minute",
                                 "second"
                               )) {
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
    decade = 60 * 60 * 24 * 365.25 * 10
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
    format = scales::number_format()
  )
}

#' @export
#' @rdname time_to_time_trans
day_decade_trans <- function() {
  time_to_time_trans(from = "day", to = "decade")
}

#' @export
#' @rdname time_to_time_trans
day_year_trans <- function() {
  time_to_time_trans("day", "year")
}

#' @export
#' @rdname time_to_time_trans
day_month_trans <- function() {
  time_to_time_trans("day", "month")
}

#' @export
#' @rdname time_to_time_trans
day_week_trans <- function() {
  time_to_time_trans("day", "week")
}

#' @export
#' @rdname time_to_time_trans
day_hour_trans <- function() {
  time_to_time_trans("day", "hour")
}

#' @export
#' @rdname time_to_time_trans
day_minute_trans <- function() {
  time_to_time_trans("day", "minute")
}

#' @export
#' @rdname time_to_time_trans
day_minute_trans <- function() {
  time_to_time_trans("day", "second")
}


#' @export
#' @rdname time_to_time_trans
second_decade_trans <- function() {
  time_to_time_trans(from = "second", to = "decade")
}

#' @export
#' @rdname time_to_time_trans
second_year_trans <- function() {
  time_to_time_trans("second", "year")
}

#' @export
#' @rdname time_to_time_trans
second_month_trans <- function() {
  time_to_time_trans("second", "month")
}

#' @export
#' @rdname time_to_time_trans
second_week_trans <- function() {
  time_to_time_trans("second", "week")
}

#' @export
#' @rdname time_to_time_trans
second_day_trans <- function() {
  time_to_time_trans("second", "day")
}

#' @export
#' @rdname time_to_time_trans
second_hour_trans <- function() {
  time_to_time_trans("second", "hour")
}

#' @export
#' @rdname time_to_time_trans
second_minute_trans <- function() {
  time_to_time_trans("second", "minute")
}
