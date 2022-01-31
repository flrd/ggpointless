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

  location <- match.arg(location, several.ok = TRUE)

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop("Please provide a valid data frame.")
  }

  # might move this to compute_panel
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

  # get row indices of first, last
  first_row <- 1
  n_rows <- nrow(data)

  lst <- list(
    first = first_row,
    last = n_rows
  )

  if("minimum" %in% location) {
    lst[["minimum"]] <- setdiff(which(data$y == min(data$y)), c(first_row, n_rows))
  }

  if("maxima" %in% location) {
    lst[["maxima"]] <- setdiff(which(data$y == max(data$y)), c(first_row, n_rows))
  }

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
#' \dontrun{
#' to_title_simple(c("foo", "bar"))
#' }
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
#' \dontrun{
#' is_any_capitalized(c("Foo", "bar"))
#' }
is_any_capitalized <- function(string) {
  any(grepl("^[A-Z]", substring(string, 1, 1)))
}



# wrap aesthetic description from geom_point() ----------------------------

wrap_rd_aesthetics <-
  function(type = "geom",
           name = "pointless",
           output_name = NULL) {

    if(is.null(output_name)) {
      output_name <- name
    }

    obj <- switch(
      type,
      geom = ggplot2:::check_subclass(name, "Geom", env = globalenv()),
      stat = ggplot2:::check_subclass(name, "Stat", env = globalenv())
    )
    aes <- ggplot2:::rd_aesthetics_item(obj)
    c(
      "@section Aesthetics:",
      paste0(
        "\\code{",
        type,
        "_",
        output_name,
        "()} ",
        "understands the following aesthetics (required aesthetics are in bold):"
      ),
      "\\itemize{",
      paste0("  \\item ", aes),
      "}",
      "Learn more about setting these aesthetics in \\code{vignette(\"ggplot2-specs\")}."
    )
  }
