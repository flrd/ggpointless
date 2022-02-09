#' Subset input data based on locations
#'
#' @description
#' Given a data frame, this functions returns a subset of the input. It returns a data frame
#' with either "first" row, "last" row(s) and/or the row(s) that contain mimima or maxima
#'
#' @param data A `data.frame`
#' @param location A character string specifying which rows to return:
#'  "first", "last", "minimum", "maximum" or "all", default is "last"
#' @return A data.frame as subset of the input data
#'
#' @examples
#' \dontrun{
#' get_locations(iris, c("first", "last"))
#' }
get_locations <- function(data = NULL, location = c("first", "last", "minimum", "maximum", "all")) {

  if (is.null(data) | !is.data.frame(data) | nrow(data) == 0) {
    stop("Please provide a valid data frame.")
  }

  locations <- match.arg(location, several.ok = TRUE)

  if("all" %in% location) {
    locations <- c("first", "last", "minimum", "maximum")
  }

  # might move this to compute_panel ?
  if(nrow(data) == 1) {
    message("Data contains a single row only. Returning data.")
    return(data)
  }

  # get row indices
  first_row <- 1
  n_rows <- nrow(data)

  lst <- list(
    first = first_row,
    last = n_rows,
    minimum = which(data$y == min(data$y, na.rm = TRUE)),
    maximum = which(data$y == max(data$y, na.rm = TRUE))
  )

  # filter only desired locations
  lst <- lst[locations]

  # create 2 cols data frame of row indices and locations
  tmp <- utils::stack(lst)

  # make sure that first is plotted on top of last, minimum, maximum, in that order
  # hence, order by row index, i.e. "values", then by reversed location level
  tmp <- tmp[order(tmp[["values"]], -as.integer(tmp[["ind"]])),, drop = FALSE]

  # return subset of input data
  return(
    cbind(
      data[tmp[["values"]],, drop = FALSE],
      location = tmp[["ind"]]
    )
  )
}


# create adecade from number ----------------------------------------------
#'Given a year (A.D.), get the decade
#'
#'@param year A numeric vector
#'@return A string of the same length as year
#'
#'@examples
#'\dontrun{
#'decades(c(2019:2021))
#'}

decades <- function(year) {
  stopifnot(is.numeric(year) | !is.null(year))

  years_AD <- pmax(0, year)

  if(!identical(year, years_AD)) {
    message("Year must be larger than 0, returning \"00's\".")
  }

  decade <- years_AD %/% 10 * 10

  # don't format NA values, as they'd turn into "NA's"
  idx <- is.na(decade)
  out <- replace(decade, !idx, sprintf("%02.0f's", decade[!idx]))
  out
}

