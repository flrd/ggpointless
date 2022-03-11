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
get_locations <- function(data = NULL, location = c("first", "last", "minimum", "maximum", "all")) {

  if (is.null(data) | !is.data.frame(data) | nrow(data) == 0) {
    stop("Please provide a valid data frame.")
  }

  locations <- match.arg(location, several.ok = TRUE)

  if("all" %in% location) {
    locations <- c("first", "last", "minimum", "maximum")
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


# x else y ----------------------------------------------------------------
#'If not x, return y
#'
#'@keywords internal
`%||%` <- function(x, y) {
  if(is.null(x)) y else x
}


# get 'lifelines' ---------------------------------------------------------
#'Given a year, get the decade
#'
#'@param x A vector of mode numeric
#'@param xend A vector of mode numeric
#'@return A data.frame
#'
#'@keywords internal
get_lexis <- function(x, xend) {

  if(is.character(x) || is.character(x)) {
    stop("`x` and `xend` must be continous.")
  }

  # if(typeof(x) != typeof(xend)) {
  #   stop("`x` and `xend` must be of the same type.")
  # }

  if(anyNA(x)) {
    stop("`x` should not contain missing values.")
  }

  if(any(xend < 0, na.rm = TRUE)) {
    stop("All values in `xend` must be greater than zero.")
  }

  if(any(x > xend, na.rm = TRUE)) {
    stop("`xend` must be greater than `x`")
  }

  # get all x-postions
  tmp_x <- sort(c(x, xend))

  # get the y-positions
  # unclass because cumsum won't work if we work with difftime objects
  tmp_y <- cumsum(unclass(xend - x))
  tmp_y <- sort(c(0, tmp_y[-length(tmp_y)], tmp_y))

  # collect xy-coordinates
  out <- data.frame(
    x = tmp_x[-length(tmp_x)],
    xend = tmp_x[-1],
    y = tmp_y[-length(tmp_y)],
    yend = tmp_y[-1]
  )

  # check y and yend positions are the same, if so, assign
  # dotted linetype to this segment, else solid
  # out[["lty"]] <- c("solid", "dotted")[(out[["yend"]] - out[["y"]] == 0) + 1]
  out[["type"]] <- ifelse(out[["yend"]] - out[["y"]] == 0, "constant", "diagonal")
  return(out)
}


