`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Subset input data based on locations
#'
#' @description
#' Given a data frame, this functions returns a subset. Returns a
#' data frame with either "first" row, "last" row and/or the row(s)
#' that contain minima or maxima
#'
#' @param data A `data.frame`
#' @param location A character string specifying which rows to return:
#'  "first", "last" (default), "minimum", "maximum" or "all"
#' @return A data.frame
#'
#' @keywords internal
get_locations <- function(data = NULL,
                          location = c(
                            "first",
                            "last",
                            "minimum",
                            "maximum",
                            "all"
                          )) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    stop("Please provide a valid data frame.")
  }

  locations <- match.arg(location, several.ok = TRUE)

  if ("all" %in% location) {
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

  # make sure that first is plotted on top of last, minimum, maximum,
  # in that order hence, order by row index, i.e. "values",
  # then by reversed location level
  tmp <- tmp[order(tmp[["values"]], -as.integer(tmp[["ind"]])), , drop = FALSE]

  # return subset of input data
  return(
    cbind(
      data[tmp[["values"]], , drop = FALSE],
      location = tmp[["ind"]]
    )
  )
}


# create a decade from a number -------------------------------------------
#' Given a year, get the decade
#'
#' @param years A numeric vector
#' @param anno_domini logical; should only "years of the Lord" be considered?
#' @return A string of the same length as year
#'
#' @keywords internal

get_decades <- function(years, anno_domini = FALSE) {
  stopifnot(is.numeric(years) | !is.null(years))

  if (anno_domini) {
    tmp <- pmax(0, years)

    if (!isTRUE(all.equal(years, tmp))) {
      message(paste(
        "All years must be AD, returning 0.",
        "Consider to use `anno_domini = FALSE`"
      ))
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


# get 'lifelines' ---------------------------------------------------------
#' Given a year, get the decade
#'
#' @param x A vector of mode numeric
#' @param xend A vector of mode numeric
#' @return A data.frame
#'
#' @keywords internal
get_lexis <- function(x, xend) {
  if (is.character(x) || is.character(xend)) {
    stop("`x` and `xend` must be continuous.")
  }

  if (!(mode(c(x, xend)) == "numeric")) {
    stop("`x` and `xend` must be continuous.")
  }

  if (anyNA(x)) {
    stop("`x` must not contain missing values.")
  }

  if (any(x > xend, na.rm = TRUE)) {
    stop("For each observation, `xend` must be greater than `x`")
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
  # Note: we need to assign 'real' linetypes here otherwise we'd
  # run into an error if we want to use the "type" column from the
  # layer data and map it to an aesthetic
  out[["type"]] <- c("solid", "11")[(out[["yend"]] - out[["y"]] == 0) + 1]
  return(out)
}


# translate shape strings - 'borrowed' from ggplot2 -----------------------
#' Given a year, get the decade
#'
#' @param shape_string character
#' @return A data.frame
#'
#' @keywords internal
translate_shape_string <- function(shape_string) {
  if (nchar(shape_string[1]) <= 1) {
    return(shape_string)
  }
  pch_table <- c(
    `square open` = 0,
    `circle open` = 1,
    `triangle open` = 2,
    plus = 3,
    cross = 4,
    `diamond open` = 5,
    `triangle down open` = 6,
    `square cross` = 7,
    asterisk = 8,
    `diamond plus` = 9,
    `circle plus` = 10,
    star = 11,
    `square plus` = 12,
    `circle cross` = 13,
    `square triangle` = 14,
    `triangle square` = 14,
    square = 15,
    `circle small` = 16,
    triangle = 17,
    diamond = 18,
    circle = 19,
    bullet = 20,
    `circle filled` = 21,
    `square filled` = 22,
    `diamond filled` = 23,
    `triangle filled` = 24,
    `triangle down filled` = 25
  )
  shape_match <- charmatch(shape_string, names(pch_table))
  invalid_strings <- is.na(shape_match)
  nonunique_strings <- shape_match == 0
  if (any(invalid_strings)) {
    bad_string <- unique(shape_string[invalid_strings])
    n_bad <- length(bad_string)
    collapsed_names <- sprintf("\n* '%s'", bad_string[1:min(
      5,
      n_bad
    )])
    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad -
        5, ifelse(n_bad > 6, "s", ""))
    } else {
      ""
    }
    stop(sprintf(
      "Can't find shape name: %s %s",
      collapsed_names, more_problems
    ))
  }
  if (any(nonunique_strings)) {
    bad_string <- unique(shape_string[nonunique_strings])
    n_bad <- length(bad_string)
    n_matches <- vapply(
      bad_string[seq_len(min(5, n_bad))],
      function(shape_string) {
        sum(grepl(paste0(
          "^",
          shape_string
        ), names(pch_table)))
      }, integer(1)
    )
    collapsed_names <- sprintf(
      "\n* '%s' partially matches %d shape names",
      bad_string[1:min(5, n_bad)], n_matches
    )
    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad -
        5, ifelse(n_bad > 6, "s", ""))
    } else {
      ""
    }
    stop(sprintf(
      "Shape names must be unambiguous: %s %s",
      collapsed_names, more_problems
    ))
  }
  unname(pch_table[shape_match])
}
