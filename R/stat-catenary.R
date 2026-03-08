# Tolerance constant
# sqrt(.Machine$double.eps) ~ 1.5e-8; practical for floating-point-fuzzed coords
.cat_tol <- sqrt(.Machine$double.eps)

#' Solve catenary parameter `a` from chain/arch length
#' @noRd
solve_a_from_len <- function(dx, dy, L) {
  abs_dx <- abs(dx)
  discriminant <- L^2 - dy^2
  # Guard: length must exceed the vertical gap
  if (discriminant <= 0) {
    return(Inf)
  }

  tgt <- sqrt(discriminant) / abs_dx

  # Initial guess for Newton's method on sinh(A)/A = tgt
  A <- if (tgt < 3) {
    sqrt(6 * (tgt - 1))
  } else {
    log_2t <- log(2 * tgt)
    log_2t + log(log_2t)
  }
  A <- max(A, 1e-12)

  for (j in seq_len(25L)) {
    sinh_A <- sinh(A)
    cosh_A <- cosh(A)
    val <- sinh_A / A - tgt
    deriv <- (cosh_A * A - sinh_A) / A^2

    if (abs(deriv) < 1e-12) {
      break
    }
    step <- val / deriv
    A <- A - step
    A <- max(A, 1e-12)
    if (abs(val) < 1e-9) {
      break
    }
  }

  abs_dx / (2 * A)
}

#' Solve catenary parameter `a` from sag (drop below lowest endpoint)
#' @noRd
solve_a_from_lowest_sag <- function(dx, dy, S) {
  if (S <= 0) {
    return(Inf)
  }

  abs_dx <- abs(dx)
  abs_dy <- abs(dy)

  H_low <- S
  H_high <- S + abs_dy

  f <- function(a) {
    a * acosh(H_low / a + 1) + a * acosh(H_high / a + 1) - abs_dx
  }

  max_bracket_iter <- 50L

  a_up <- max(abs_dx, 1)
  for (i in seq_len(max_bracket_iter)) {
    f_up <- f(a_up)
    if (is.nan(f_up)) {
      return(Inf)
    }
    if (f_up >= 0) {
      break
    }
    a_up <- a_up * 10
  }

  a_low <- a_up / 10
  for (i in seq_len(max_bracket_iter)) {
    f_low <- f(a_low)
    if (is.nan(f_low)) {
      return(Inf)
    }
    if (f_low <= 0) {
      break
    }
    a_low <- a_low / 10
  }

  if (is.nan(f(a_low)) || is.nan(f(a_up))) {
    return(Inf)
  }
  if (f(a_low) * f(a_up) > 0) {
    return(Inf)
  }

  tryCatch(
    stats::uniroot(
      f,
      lower = a_low,
      upper = a_up,
      tol = 1e-10
    )$root,
    error = \(e) {
      Inf
    }
  )
}


# Shared computation engine (internal)

#' Compute catenary/arch curves for one group
#'
#' @param data Data frame with `x` and `y`.
#' @param n Number of interpolation points per segment.
#' @param chain_length,sag User-supplied shape parameters.
#' @param gravity `1` for hanging chain, `-1` for arch.
#' @param len_name,sag_name User-facing parameter names for messages.
#' @noRd
compute_catenary_group <- function(
  data,
  n = 100L,
  chain_length,
  sag,
  gravity,
  len_name,
  sag_name
) {
  empty <- data.frame(x = numeric(0), y = numeric(0))

  if (nrow(data) < 2L) {
    return(empty)
  }

  # 1. Sort by x so segments always go left-to-right (prevents

  #    overlapping x ranges that cause rendering artefacts).
  data <- data[order(data$x), ]

  # 2. Deduplication

  dx_raw <- diff(data$x)
  dy_raw <- diff(data$y)
  is_dup <- c(FALSE, (abs(dx_raw) + abs(dy_raw)) < .cat_tol)

  if (any(is_dup)) {
    cli::cli_warn(
      "Removed {sum(is_dup)} near-duplicate point{?s} from input data."
    )
    data <- data[!is_dup, ]
  }

  if (nrow(data) < 2L) {
    return(empty)
  }

  n_segs <- nrow(data) - 1L

  # 3. Segment geometry
  starts <- data[-nrow(data), ]
  ends <- data[-1L, ]
  dx_all <- ends$x - starts$x
  dy_all <- ends$y - starts$y
  dists <- sqrt(dx_all^2 + dy_all^2)

  # 4. Parameter processing & validation
  process_param <- function(val, name) {
    if (is.null(val)) {
      return(rep(NA_real_, n_segs))
    }
    if (!is.numeric(val)) {
      cli::cli_abort("{.arg {name}} must be numeric, not {.cls {class(val)}}.")
    }
    if (length(val) > n_segs) {
      cli::cli_warn(
        "{length(val)} value{?s} provided for {.arg {name}} but there \\
         {?is/are} only {n_segs} segment{?s}. Only the first {n_segs} \\
         {?value was/values were} used; the rest was ignored."
      )
    }
    val <- rep_len(val, n_segs)
    bad <- !is.na(val) & val < 0
    if (any(bad)) {
      cli::cli_abort(
        "{.arg {name}} must be non-negative (got {val[which(bad)[1]]})."
      )
    }
    val
  }

  chain_length_vec <- process_param(chain_length, len_name)
  sag_vec <- process_param(sag, sag_name)

  conflict <- !is.na(chain_length_vec) & !is.na(sag_vec)
  if (any(conflict)) {
    cli::cli_inform(
      "Both {.arg {sag_name}} and {.arg {len_name}} supplied for \\
       {sum(conflict)} segment{?s}; using {.arg {sag_name}}.",
      .frequency = "regularly",
      .frequency_id = "catenary_param_conflict"
    )
  }

  # 5. Per-segment curve computation
  results <- lapply(seq_len(n_segs), function(i) {
    x1 <- starts$x[i]
    y1 <- starts$y[i]
    x2 <- ends$x[i]
    y2 <- ends$y[i]
    dx <- dx_all[i]
    dy <- dy_all[i]
    straight_dist <- dists[i]

    # Coincident points
    if (straight_dist < .cat_tol) {
      return(data.frame(x = rep(x1, n), y = rep(y1, n)))
    }

    x_seq <- seq(x1, x2, length.out = n)

    get_linear <- function() {
      data.frame(x = x_seq, y = y1 + (dy / dx) * (x_seq - x1))
    }

    # Vertical segment
    if (abs(dx) < .cat_tol) {
      return(data.frame(x = rep(x1, n), y = seq(y1, y2, length.out = n)))
    }

    # Effective (gravity-flipped) coordinates
    eff_y1 <- y1 * gravity
    eff_y2 <- y2 * gravity
    eff_dy <- eff_y2 - eff_y1

    # Solver selection
    current_alpha <- NULL

    # Priority 1: sag / arch_height
    if (!is.na(sag_vec[i])) {
      current_alpha <- solve_a_from_lowest_sag(dx, eff_dy, sag_vec[i])
    } else if (!is.na(chain_length_vec[i])) {
      # Priority 2: chain_length / arch_length
      if (chain_length_vec[i] >= straight_dist) {
        current_alpha <- solve_a_from_len(dx, eff_dy, chain_length_vec[i])
      } else {
        cli::cli_warn(
          "The {.arg {len_name}} ({round(chain_length_vec[i], 2)}) is shorter \\
           than the distance ({round(straight_dist, 2)}) between \\
           ({round(x1, 2)}, {round(y1, 2)}) and ({round(x2, 2)}, {round(y2, 2)}). \\
           Drawing a straight line."
        )
        return(get_linear())
      }
    } else {
      # Priority 3: default = 2x euclidean distance
      current_alpha <- solve_a_from_len(dx, eff_dy, straight_dist * 2)
    }

    # Safety net (catches NULL, Inf, NaN)
    if (
      is.null(current_alpha) ||
        !is.finite(current_alpha) ||
        current_alpha < .cat_tol
    ) {
      return(get_linear())
    }

    # Catenary evaluation
    sinh_val <- sinh(dx / (2 * current_alpha))
    if (abs(sinh_val) < 1e-9) {
      return(get_linear())
    }

    val <- eff_dy / (2 * current_alpha * sinh_val)
    h <- (x1 + x2) / 2 - current_alpha * asinh(val)
    v <- eff_y1 - current_alpha * cosh((x1 - h) / current_alpha)

    y_cat <- current_alpha * cosh((x_seq - h) / current_alpha) + v

    # Overflow guard
    if (any(!is.finite(y_cat))) {
      return(get_linear())
    }

    # Flip back for arch; snap endpoints to the exact user-specified coordinates
    # to prevent floating-point drift from clipping them at hard scale limits
    # (e.g. ylim(NA, 2) would drop a computed y of 2 + 2e-16).
    y_out <- y_cat * gravity
    y_out[1L] <- y1
    y_out[length(y_out)] <- y2
    data.frame(x = x_seq, y = y_out)
  })

  do.call(rbind, results)
}

#' @rdname ggpointless-ggproto
#' @usage NULL
#' @format NULL
#' @export
StatCatenary <- ggproto(
  "StatCatenary",
  Stat,
  required_aes = c("x", "y"),

  extra_params = c("na.rm", "chain_length", "chainLength"),

  setup_params = \(data, params) {
    has_chainLength <- !is.null(params$chainLength)
    has_chain_length <- !is.null(params$chain_length)

    if (has_chainLength && has_chain_length) {
      cli::cli_inform(
        "Note that {.arg chain_length} wins over deprecated {.arg chainLength}. \\
        Use {.arg chain_length}.",
        .frequency = "regularly",
        .frequency_id = "catenary_chain_length_wins"
      )
      # drop chainLength
      params$chainLength <- NULL
    }

    params
  },

  compute_group = \(data, scales, chain_length = NULL, sag = NULL) {
    compute_catenary_group(
      data,
      chain_length = chain_length,
      sag = sag,
      gravity = 1,
      len_name = "chain_length",
      sag_name = "sag"
    )
  }
)

#' @rdname ggpointless-ggproto
#' @usage NULL
#' @format NULL
#' @export
StatArch <- ggproto(
  "StatArch",
  Stat,
  required_aes = c("x", "y"),

  compute_group = \(
    data,
    scales,
    arch_length = NULL,
    arch_height = NULL
  ) {
    compute_catenary_group(
      data,
      chain_length = arch_length,
      sag = arch_height,
      gravity = -1,
      len_name = "arch_length",
      sag_name = "arch_height"
    )
  }
)

#' @rdname geom_catenary
#' @export
stat_catenary <- make_constructor(StatCatenary, geom = "catenary")

#' @rdname geom_catenary
#' @export
stat_arch <- make_constructor(StatArch, geom = "line")
