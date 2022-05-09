#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatLexis <- ggproto("StatLexis", Stat,
  required_aes = c("x", "xend"),
  default_aes = aes(y = after_stat(y), yend = after_stat(yend)),
  setup_params = function(data, params) {

    has_y <- !(is.null(data$y) && is.null(params$y))
    has_yend <- !(is.null(data$yend) && is.null(params$yend))
    if (has_y || has_yend) {
      message("stat_lexis() calculates y and yend aesthetics for you.")
    }
    params
  },

  compute_group = function(data, scales) {
    get_lexis(data$x, data$xend)
  }
)

#' @export
#' @rdname geom_lexis
#' @format NULL
#' @usage NULL
stat_lexis <- function(mapping = NULL, data = NULL,
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatLexis,
    geom = "lexis",
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}


#' Given a start, and end get the 'age' of an event
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

  if (mode(c(x, xend)) != "numeric") {
    stop("`x` and `xend` must be continuous.")
  }

  if (any(x > xend, na.rm = TRUE)) {
    stop(paste(
      "For each row in your data, `xend` must",
      "be greater than `x`"
    ))
  }

  # get all x-positions
  tmp_x <- sort(c(x, xend))

  # get the y-positions
  # unclass because cumsum doesn't work with difftime objects
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
  out[["type"]] <- ifelse(out[["yend"]] - out[["y"]] == 0, "dotted", "solid")
  return(out)
}
