#' @section Computed variables:
#' \describe{
#'   \item{location}{locations, returned as factor}
#' }
#'
#' @export
#' @rdname geom_pointless
stat_pointless <- function(mapping = NULL,
                           data = NULL,
                           geom = "point",
                           position = "identity",
                           ...,
                           location = "last",
                           na.rm = FALSE,
                           orientation = NA,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatPointless,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      location = location,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPointless <- ggproto("StatPointless", Stat,
  setup_params = function(data, params) {
    if (!anyDuplicated(data$group)) {
      message(
        paste(
          "Each group consists of only one observation.",
          "Do you need to adjust the group aesthetic?"
        )
      )
    }
    GeomPath$setup_params(data, params)
  },
  extra_params = c("na.rm", "orientation"),
  setup_data = function(data, params) {
    GeomPath$setup_data(data, params)
  },
  compute_group = function(data, scales, location) {
    get_locations(data, location = location)
  },
  required_aes = c("x", "y")
)

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
