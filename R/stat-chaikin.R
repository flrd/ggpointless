#' @export
#' @rdname geom_chaikin
stat_chaikin <- function(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  ...,
  mode = "open",
  iterations = 5,
  ratio = 0.25,
  closed = lifecycle::deprecated(),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (lifecycle::is_present(closed)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "stat_chaikin(closed)",
      "stat_chaikin(mode)"
    )
    if (missing(mode)) {
      mode <- if (isTRUE(closed)) "closed" else "open"
    } else {
      cli::cli_inform(
        "{.arg mode} wins over deprecated {.arg closed}. Use {.arg mode}.",
        .frequency = "regularly",
        .frequency_id = "chaikin_mode_wins"
      )
    }
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatChaikin,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      mode = mode,
      iterations = iterations,
      ratio = ratio,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpointless-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatChaikin <- ggproto(
  "StatChaikin",
  Stat,
  required_aes = c("x", "y"),
  extra_params = c("na.rm", "mode", "iterations", "ratio", "closed"),

  setup_params = \(data, params) {
    # Handle deprecated `closed` (arrives via ... from geom_chaikin(closed = ...))
    if (!is.null(params$closed) && lifecycle::is_present(params$closed)) {
      lifecycle::deprecate_warn(
        "0.2.0",
        "geom_chaikin(closed)",
        "geom_chaikin(mode)"
      )
      if (!is.null(params$mode)) {
        # mode was explicitly set by the user — mode wins
        cli::cli_inform(
          "{.arg mode} wins over deprecated {.arg closed}. Use {.arg mode}.",
          .frequency = "regularly",
          .frequency_id = "chaikin_mode_wins"
        )
      } else {
        params$mode <- if (isTRUE(params$closed)) "closed" else "open"
      }
      params$closed <- NULL
    }

    # Resolve NULL (= not explicitly set) to the default
    params$mode <- params$mode %||% "open"

    # Validate mode
    params$mode <- rlang::arg_match0(params$mode, values = c("open", "closed"))

    # Validate iterations
    if (
      !rlang::is_integerish(params$iterations, n = 1L, finite = TRUE) ||
        params$iterations < 0L ||
        params$iterations > 10L
    ) {
      cli::cli_abort(
        "{.arg iterations} must be a whole number between 0 and 10, \\
         not {.val {params$iterations}}."
      )
    }
    params$iterations <- as.integer(params$iterations)

    # Validate ratio — must be a single finite number in [0, 1].
    # Note: !is.numeric catches logical NA; !is.finite catches NA_real_, NaN,
    # Inf, and -Inf, so the range comparisons are always safe afterwards.
    if (
      !is.numeric(params$ratio) ||
        length(params$ratio) != 1L ||
        !is.finite(params$ratio) ||
        params$ratio < 0 ||
        params$ratio > 1
    ) {
      cli::cli_abort(
        "{.arg ratio} must be a finite number in [0, 1], \\
         not {.val {params$ratio}}."
      )
    }
    if (params$ratio > 0.5) {
      cli::cli_warn(
        "{.arg ratio} = {params$ratio} is converted to its complement \\
         {1 - params$ratio}: for closed paths the shape is identical; \\
         for open paths the curve near the endpoints may differ slightly."
      )
      params$ratio <- 1 - params$ratio
    }

    params
  },

  compute_group = \(
    data,
    scales,
    mode = "open",
    iterations = 5L,
    ratio = 0.25
  ) {
    closed <- mode == "closed"
    data <- get_chaikin(
      x = data$x,
      y = data$y,
      iterations = iterations,
      ratio = ratio,
      closed = closed
    )
    if (closed) {
      data <- rbind(data, data[1L, , drop = FALSE])
    }
    data
  }
)

#' @keywords internal
lerp <- function(a, b, ratio) {
  a + (b - a) * ratio
}

#' @keywords internal
neighbors <- function(x) {
  # credit:
  # https://github.com/Farbfetzen/corner_cutting/blob/main/main.R
  n <- length(x)
  c(rbind(
    c(x[n], x[-n]),
    c(x[-1], x[1])
  ))
}

#' @keywords internal
lerp_neighbors <- function(x, ratio = .25) {
  a <- rep(x, each = 2)
  b <- neighbors(x)
  lerp(a, b, ratio)
}

#' @keywords internal
cut_corners <- function(x, y, ratio, closed = TRUE) {
  new_x <- lerp_neighbors(x, ratio = ratio)
  new_y <- lerp_neighbors(y, ratio = ratio)

  if (!closed) {
    new_x <- new_x[-c(1, length(new_x))]
    new_y <- new_y[-c(1, length(new_y))]
    new_x[c(1, length(new_x))] <- x[c(1, length(x))]
    new_y[c(1, length(new_y))] <- y[c(1, length(y))]
  }

  list(x = new_x, y = new_y)
}

#' @keywords internal
get_chaikin <- function(x, y, iterations = 5, ratio = .25, closed = FALSE) {
  # 1. Validate lengths first — before the early-return for iterations = 0 —
  #    so that callers always receive clean cli errors for malformed inputs.
  if (length(x) == 0L || length(y) == 0L) {
    cli::cli_abort("{.arg x} and {.arg y} must have a positive length.")
  }

  if (!identical(length(x), length(y))) {
    cli::cli_abort("{.arg x} and {.arg y} must have the same length.")
  }

  if (iterations == 0L) {
    return(data.frame(x = x, y = y))
  }

  # 2. Non-finite values (Inf, NaN, NA) would produce NaN throughout the lerp
  #    arithmetic via Inf - Inf = NaN.  Remove them with a warning so the
  #    remaining path is well-defined.
  bad <- !is.finite(x) | !is.finite(y)
  if (any(bad)) {
    cli::cli_warn(
      "{sum(bad)} non-finite value{?s} in {.arg x}/{.arg y} removed before \\
       corner-cutting."
    )
    x <- x[!bad]
    y <- y[!bad]
    if (length(x) == 0L) {
      return(data.frame(x = numeric(0L), y = numeric(0L)))
    }
  }

  # 3. Fewer than 3 points means there are no interior corners to cut
  #    (open path) or no non-degenerate polygon to smooth (closed path).
  #    Returning the input unchanged avoids a base-R warning from the empty-
  #    vector assignment that would otherwise occur in cut_corners().
  if (length(x) < 3L) {
    cli::cli_warn(
      "Corner-cutting requires at least 3 points; returning input unchanged."
    )
    return(data.frame(x = x, y = y))
  }

  for (i in seq.int(iterations)) {
    xy <- cut_corners(x, y, ratio = ratio, closed = closed)
    x <- xy$x
    y <- xy$y
  }
  data.frame(x = x, y = y)
}
