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
    lifecycle::deprecate_stop(
      "0.3.0",
      "stat_chaikin(closed)",
      "stat_chaikin(mode)"
    )
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
      lifecycle::deprecate_stop(
        "0.3.0",
        "geom_chaikin(closed)",
        "geom_chaikin(mode)"
      )
    }

    # Resolve NULLs (= not explicitly set) to defaults.
    # These can be NULL when stat = "chaikin" is used from a geom that
    # doesn't define these params (e.g. geom_ridgeline_fade(stat = "chaikin")).
    params$mode       <- params$mode %||% "open"
    params$iterations <- params$iterations %||% 5L
    params$ratio      <- params$ratio %||% 0.25

    # Validate mode
    params$mode <- rlang::arg_match0(params$mode, values = c("open", "closed"), arg_nm = "mode")

    # Validate iterations
    if (
      !rlang::is_integerish(params$iterations, n = 1L, finite = TRUE) ||
        params$iterations < 0L ||
        params$iterations > 10L
    ) {
      cli::cli_abort(c(
        "{.arg iterations} must be a whole number between 0 and 10, \\
         not {.val {params$iterations}}.",
        "i" = "Each iteration roughly doubles the vertex count, so values \\
               above 10 explode the output (1 input -> ~1024+ rendered \\
               vertices) without visible improvement."
      ))
    }
    params$iterations <- as.integer(params$iterations)

    # Validate ratio -- must be a single finite number in [0, 1].
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
      cli::cli_warn(c(
        "!" = "{.arg ratio} = {params$ratio} is outside the conventional \\
               Chaikin range {.code [0, 0.5]}; flipping to its complement \\
               {.val {1 - params$ratio}}.",
        "i" = "For closed paths the resulting vertex set is the same (with a \\
               cyclic shift); for open paths the curve differs from the one \\
               implied by {.code ratio = {params$ratio}}.",
        "i" = "For stronger smoothing, increase {.arg iterations} instead."
      ))
      params$ratio <- 1 - params$ratio
    }

    params
  },

  compute_group = \(
    self,
    data,
    scales,
    mode = "open",
    iterations = 5L,
    ratio = 0.25,
    na.rm = FALSE
  ) {
    closed <- mode == "closed"

    # Pre-filter rows with NA in x, y, OR any numeric extra column we'll
    # smooth. Without this each get_chaikin() call would strip NAs
    # independently and produce different output lengths -- the per-column
    # assignment `result[[col]] <- smoothed$y` would then error with
    # "replacement has X rows, data has Y", and ggplot2's stat machinery
    # silently swallows the error, leaving the layer empty. Filtering once
    # here keeps every smoothed column aligned and surfaces the dropped
    # rows via a single ggplot2-style warning when `na.rm = FALSE`.
    extra_cols <- setdiff(names(data), c("x", "y"))
    smooth_cols <- c("x", "y", extra_cols[vapply(
      data[extra_cols], is.numeric, logical(1)
    )])
    keep <- Reduce(`&`, lapply(smooth_cols, \(c) is.finite(data[[c]])))
    if (!isTRUE(na.rm) && any(!keep)) {
      cli::cli_warn(
        "Removed {sum(!keep)} row{?s} containing missing values \\
         ({.fn stat_chaikin})."
      )
    }
    data <- data[keep, , drop = FALSE]

    if (nrow(data) == 0L) {
      return(data.frame(x = numeric(0L), y = numeric(0L)))
    }

    result <- get_chaikin(
      x = data$x,
      y = data$y,
      iterations = iterations,
      ratio = ratio,
      closed = closed
    )

    # When extra numeric columns are present (e.g. `height` from
    # geom_ridgeline_fade), apply the same corner-cutting so they
    # are interpolated at the new x-positions. Pre-filtering above
    # guarantees each call returns the same number of rows.
    for (col in extra_cols) {
      if (is.numeric(data[[col]])) {
        smoothed <- get_chaikin(
          x = data$x,
          y = data[[col]],
          iterations = iterations,
          ratio = ratio,
          closed = closed
        )
        result[[col]] <- smoothed$y
      } else {
        # Constant discrete columns (group, fill, etc.) -- replicate first value
        result[[col]] <- data[[col]][1L]
      }
    }

    if (closed) {
      result <- rbind(result, result[1L, , drop = FALSE])
    }
    result
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
  # 1. Validate lengths first -- before the early-return for iterations = 0 --
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
