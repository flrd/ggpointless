#' Return first, last or first and last row(s) of a data frame
#'
#' This function takes a data frame and returns a subset based on the argument
#'
#' @param data A data frame
#' @param point.position A character string specifying which observations to return; ether "first", "last", or "both"
#' @return A data.frame as subset of the input data
#'
#' @examples
#' rng(economics, "both")
rng <- function(data = NULL, point.position = c("first", "last", "both")) {
    if (is.null(data)) {
      stop("Please provide a data frame.")
    }

    if (nrow(data) %in% c(1, 2)) {
      message("Maybe use a larger data frame to see an effect.")
    }

    point.position = match.arg(point.position)

    tmp <- switch(
      point.position,
      first = 1,
      last = nrow(data),
      both = c(1, nrow(data))
    )

    return(data[tmp, , drop = FALSE])

  }
