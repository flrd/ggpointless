#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatLexis <- ggproto(
  "StatLexis",
  Stat,
  required_aes = c("x", "xend"),
  default_aes = aes(y = after_stat(y), yend = after_stat(yend)),
  setup_params = \(data, params) {
    if (!is.null(data$y) || !is.null(data$yend)) {
      cli::cli_inform(
        "{.fn stat_lexis} calculates {.field y} and {.field yend} for you."
      )
    }
    params
  },
  compute_group = \(data, scales) {
    if (isTRUE(scales$x$is_discrete())) {
      cli::cli_abort(
        c(
          "{.arg x} and {.arg xend} must be continuous variables.",
          "i" = "A discrete (character or factor) value was detected. \\
                 Check that all values in {.arg x} and {.arg xend} are numeric."
        )
      )
    }
    get_lexis(data$x, data$xend)
  }
)

#' @export
#' @rdname geom_lexis
stat_lexis <- make_constructor(StatLexis, geom = "lexis")

#' @keywords internal
get_lexis <- function(x, xend) {
  if (mode(c(x, xend)) != "numeric") {
    cli::cli_abort("{.arg x} and {.arg xend} must be continuous.")
  }

  if (any(x > xend, na.rm = TRUE)) {
    cli::cli_abort(
      "Each {.arg xend} must be greater than or equal to its {.arg x}."
    )
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
