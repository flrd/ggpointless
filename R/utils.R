#' Subset input data based on locations
#'
#' @description
#' Given a data frame, this functions returns a subset of the input. It returns a data frame
#' with either "first" row, "last" row and/or the row(s) that contain minima or maxima
#'
#' @param data A `data.frame`
#' @param location A character string specifying which rows to return:
#'  "first", "last" (default), "minimum", "maximum" or "all"
#' @return A data.frame
#'
#' @keywords internal
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

  # extract only desired locations
  lst <- lst[locations]

  # creates two column data frame of row indices and locations
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


# create a decade from a number -------------------------------------------
#'Given a year, get the decade
#'
#'@param years A numeric vector
#'@param anno_domini logical; should only "years of the Lord" be considered?
#'@return A string of the same length as year
#'
#'@keywords internal
#'@examples
#' \dontrun{
#'get_decades(c(2019:2021))
#'get_decades(c(-2019:-2021))
#'get_decades(c(-723, 111, 2022), anno_domini = TRUE)
#'}

get_decades <- function(years, anno_domini = FALSE) {
  stopifnot(is.numeric(years) | !is.null(years))

  if(anno_domini) {

    tmp <- pmax(0, years)

    if(!isTRUE(all.equal(years, tmp))) {
      message("All years must be AD, returning 0. Consider to use `anno_domini = FALSE`")
    }
    decade <- tmp %/% 10 * 10
  } else {
    tmp <- abs(years)
    decade <- sign(years) * tmp %/% 10 * 10
  }

  # don't format NA values, as they'd turn into "NA's"
  idx <- is.na(decade)
  out <- replace(decade, !idx, sprintf("%02.0f's", decade[!idx]))
  out
}

