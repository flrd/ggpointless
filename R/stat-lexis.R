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
    # stat-computed columns bypass scales$transform_df (which runs before the
    # stat), so reversing scales never touch them.  Both reversals need manual
    # compensation.
    #
    # x-reverse: the scale has already negated data$x and data$xend.
    # Un-negate, run get_lexis on the original values (so the algorithm assigns
    # y correctly: small original time → small y), then re-negate the output x
    # values so they live in the reversed transformed space ggplot2 expects.
    # This makes segments tilt upper-LEFT (correct) instead of upper-RIGHT.
    x_is_reversed <- !is.null(scales$x) && isTRUE(scales$x$trans$name == "reverse")
    if (x_is_reversed) {
      out      <- get_lexis(-data$x, -data$xend)
      out$x    <- -out$x
      out$xend <- -out$xend
    } else {
      out <- get_lexis(data$x, data$xend)
    }
    # y-reverse: same bypass issue for stat-computed y/yend.
    if (!is.null(scales$y) && isTRUE(scales$y$trans$name == "reverse")) {
      out$y    <- -out$y
      out$yend <- -out$yend
    }
    out
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

  # Swap pairs where x > xend (e.g. a dataset where start > end) so cumsum
  # always sees non-negative durations.  The scale_x_reverse() case is handled
  # before get_lexis() is called (in compute_group), so x/xend arrive here
  # in their original, un-negated form.
  swap <- !is.na(x) & !is.na(xend) & (x > xend)
  if (any(swap)) {
    tmp       <- x[swap]
    x[swap]   <- xend[swap]
    xend[swap] <- tmp
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
