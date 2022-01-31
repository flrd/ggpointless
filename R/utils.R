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
#' get_locations(iris, c("first", "last"))
get_locations <- function(data, location = c("first", "last", "minimum", "maximum", "all")) {

  location <- match.arg(location, several.ok = TRUE)

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop("Please provide a valid data frame.")
  }

  if(nrow(data) == 1) {
    message("Data contains a single row only. Returning data.")
    return(
      cbind(
        data,
        location = "First & Last"
      )
    )
  }

  if("all" %in% location) {
    location <- c("first", "last", "minimum", "maximum")
  }

  # do I need to filter missing values or is this done somewhere else maybe
  # y_complete <- na.omit(data$y)

  lst <- list(
    first = 1L,
    last = nrow(data),
    minimum = which(data$y == min(data$y)),
    maximum = which(data$y == max(data$y))
  )

  # filter for desired locations
  lst <- lst[location]

  # create a two column data frame which contains row indices and respective location type
  tmp <- utils::stack(lst)
  tmp <- tmp[order(tmp[["values"]]), ]

  # subset data and return it
  return(
    cbind(
      data[tmp[["values"]],, drop = FALSE],
      location = to_title_simple(tmp[["ind"]])
    )
  )

  # handle edge cases
  # 1 row
  # 2 rows
  # max = min = start = end
  # etc.
}



# helper to capitalize first letter of a string ---------------------------

#' Capitalize a character vector
#'
#' Helper to get from "here" to "Here"
#'
#' @param x A character vector
#' @return A character vector of the same length as x
#'
#' @examples
#' to_title_simple(c("foo", "bar"))
to_title_simple <- function(x) {
  paste0(
    toupper(substring(x, first = 1, last = 1)),
    substring(x, first = 2)
    )
}


# correct wrongly typed params with warning -------------------------------

#' Is any value capitalized?
#'
#' Helper to find any capitalized string.
#'
#' @param string A character vector
#' @return A logical vector of length one
#'
#' @examples
#' is_any_capitalized(c("Foo", "bar"))
is_any_capitalized <- function(string) {
  any(grepl("^[A-Z]", substring(string, 1, 1)))
}



