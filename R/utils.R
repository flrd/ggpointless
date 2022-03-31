`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
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

# translate shape strings - 'borrowed' from ggplot2 -----------------------
#' Given a number, get the corresponding point shape
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
