#' @rdname geom_fourier
#' @format NULL
#' @usage NULL
#' @export
StatFourier <- ggplot2::ggproto(
  "StatFourier",
  ggplot2::Stat,
  
  required_aes = c("x", "y"),
  
  extra_params = c("na.rm", "n_harmonics", "detrend"),
  
  default_aes = ggplot2::aes(
    colour = "black",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),
  
  # Parameter validation (runs once, not per group)
  setup_params = function(data, params) {
    # Validate detrend
    if (!is.null(params$detrend)) {
      params$detrend <- match.arg(params$detrend, choices = c("lm", "loess"))
    }
    # Validate n_harmonics
    if (!is.null(params$n_harmonics)) {
      params$n_harmonics <- suppressWarnings(as.integer(params$n_harmonics))
      if (is.na(params$n_harmonics) || params$n_harmonics < 1L) {
        message("stat_fourier: `n_harmonics` must be a positive integer. ",
                "Using 1.")
        params$n_harmonics <- 1L
      }
    }

    params
    
  },
  
  # NA handling, duplicate aggregation, and all maths live here so that each
  # group is processed independently.
  compute_group = function(data,
                           scales,
                           n_harmonics = NULL,
                           detrend     = NULL,
                           na.rm       = FALSE,
                           ...) {
    # 1. Handle missing and non-finite values per group
    is_bad <- !is.finite(data$x) | !is.finite(data$y)
    if (any(is_bad)) {
      if (na.rm) {
        data <- data[!is_bad, ]
      } else {
        message(
          "stat_fourier: Group contains missing or non-finite values. ",
          "Set `na.rm = TRUE` to drop them silently."
        )
        return(data.frame(x = numeric(0L), y = numeric(0L)))
      }
    }
    
    if (nrow(data) == 0L) {
      return(data.frame(x = numeric(0L), y = numeric(0L)))
    }
    
    # 2. Sort and aggregate duplicate x values (mean y)
    # stats::approx() returns NA for tied x values, so we collapse them first.
    data <- data[order(data$x), ]
    if (anyDuplicated(data$x)) {
      data <- stats::aggregate(y ~ x, data = data[, c("x", "y")], FUN = mean)
      data <- data[order(data$x), ]
    }
    
    n_obs <- nrow(data)
    
    if (n_obs < 3L) {
      message("stat_fourier: Fewer than 3 distinct `x` values in group. ",
              "Skipping.")
      return(data.frame(x = numeric(0L), y = numeric(0L)))
    }
    
    # 3. Guard against zero x-range
    x_min <- min(data$x)
    x_max <- max(data$x)
    x_range <- x_max - x_min   # span of the observed data; NOT the FFT period
    
    if (x_range < .Machine$double.eps * max(abs(x_min), abs(x_max), 1)) {
      message("stat_fourier: All `x` values are effectively identical. ",
              "Skipping.")
      return(data.frame(x = numeric(0L), y = numeric(0L)))
    }
    
    # 3b. Warn about highly irregular spacing
    if (n_obs > 3L) {
      x_diffs <- diff(data$x)
      cv_spacing <- stats::sd(x_diffs) / mean(x_diffs)
      if (cv_spacing > 0.5) {
        message(
          "stat_fourier: Highly irregular x-spacing detected ",
          "(CV = ",
          signif(cv_spacing, 3),
          "). ",
          "The uniform-grid interpolation may introduce artefacts."
        )
      }
    }
    
    # 3c. Resolve n_points, i.e. number of evenly-spaced points used
    # to draw the reconstructed curve.
    # At least two evaluation points per data interval ensures geom_line can
    # trace the curve shape between observations, not just hit exact data
    # positions. The output matrix is (n_points x k_paired), so this is
    # O(n_obs^2) in memory; fine up to ~10k observations.
    n_points <- max(200L, 2L * n_obs)
    
    # 4. Interpolate to a uniform grid (required for the FFT)
    # Grid spacing is derived from the observed data span and count.
    # The FFT period P = N * dx uses a half-open convention so that the
    # boundary point x_min + P is NOT included; this prevents the Gibbs
    # ringing that occurs when y(0) != y(end) and a closed interval is used.
    dx     <- x_range / (n_obs - 1L)        # spacing matching the source data
    x_even <- x_min + dx * seq(0L, n_obs - 1L)
    
    # Floating-point arithmetic can push the last element of x_even a tiny
    # epsilon beyond x_max. stats::approx() with its default rule = 1 returns
    # NA for any xout value outside [min(x), max(x)], so we clamp the
    # endpoints explicitly to the known bounds before interpolating.
    x_even[1L]    <- x_min
    x_even[n_obs] <- x_max
    y_even <- stats::approx(data$x, data$y, xout = x_even, rule = 2L)$y
    period <- n_obs * dx                    # = x_range + dx  (half-open length)
    
    # 5. De-trending
    df_fit <- data.frame(x = x_even, y = y_even)
    
    fit <- if (!is.null(detrend)) {
      switch(detrend,
             "lm" = stats::lm(y ~ x, data = df_fit),
             "loess" = {
               if (n_obs < 4L) {
                 message(
                   "stat_fourier: Too few observations for LOESS ",
                   "(need >= 4). Falling back to detrend = 'lm'."
                 )
                 stats::lm(y ~ x, data = df_fit)
               } else {
                 tryCatch(
                   stats::loess(y ~ x, data = df_fit),
                   error = function(e) {
                     message(
                       "stat_fourier: LOESS failed ('",
                       conditionMessage(e),
                       "'). Falling back to detrend = 'lm'."
                     )
                     stats::lm(y ~ x, data = df_fit)
                   }
                 )
               }
             })
    } else {
      NULL
    }
    
    y_trend_even <- if (!is.null(fit)) {
      stats::predict(fit, newdata = df_fit)
    } else {
      rep(0, n_obs)
    }
    
    y_res <- y_even - y_trend_even   # residual after de-trending
    
    # 6. Forward FFT (normalised)
    fft_vals <- stats::fft(y_res) / n_obs
    
    # 7. Harmonic selection
    # For N observations the Nyquist limit is floor(N / 2).
    max_k <- floor(n_obs / 2L)
    
    k_requested <- if (is.null(n_harmonics)) max_k else n_harmonics
    
    if (!is.null(n_harmonics) && n_harmonics > max_k) {
      message(
        "stat_fourier: `n_harmonics` (",
        n_harmonics,
        ") exceeds the Nyquist limit for this group (",
        max_k,
        "). Using ",
        max_k,
        "."
      )
    }
    
    k_use <- min(k_requested, max_k)
    
    # For even N, the component at index N/2 + 1 is the Nyquist frequency.
    # It is purely real and has no conjugate pair in the FFT output vector,
    # so it must be handled separately.
    has_nyquist <- (n_obs %% 2L == 0L) && (k_use == max_k)
    k_paired    <- if (has_nyquist) k_use - 1L else k_use
    
    # 8. Extract DFT coefficients
    # Index 1                             -> DC term (mean of residual)
    # Indices 2 ... k_paired+1            -> positive frequencies
    # Indices n_obs ... n_obs-k_paired+1  -> negative frequencies (conjugates)
    # Index max_k + 1                     -> Nyquist term (even N only)
    c_mean <- Re(fft_vals[1L])   # DC component is real by construction
    
    k_vec <- c_pos <- c_neg <- NULL   # initialise
    
    if (k_paired > 0L) {
      k_vec <- seq_len(k_paired)
      c_pos <- fft_vals[1L + k_vec]           # indices 2, 3, ..., k_paired+1
      c_neg <- fft_vals[n_obs - k_vec + 1L]   # indices N, N-1, ...
    }
    
    c_nyq <- if (has_nyquist)
      Re(fft_vals[max_k + 1L])
    else
      0
    
    # 9. Vectorised Fourier reconstruction
    # Normalised time t in [0, 1) where t = (x - x_min) / period.
    #
    # Reconstruction formula:
    # https://en.wikipedia.org/wiki/Fourier_series#Synthesis
    #   y(t) = c_mean
    #         + sum_{k=1}^{k_paired} [ c_pos[k] * exp(2*pi*i*k*t)
    #                                + c_neg[k] * exp(-2*pi*i*k*t) ]
    #         + c_nyq * cos(pi * N * t)     <- Nyquist term (even N only)
    #
    # The paired-harmonic sum is vectorised via an outer-product phase matrix
    # (n_points x k_paired), avoiding any scalar loop over t.
    
    # Guard against very large matrix allocations
    if (!is.null(k_vec) && as.double(n_points) * k_paired > 1e8) {
      warning(
        "stat_fourier: Large reconstruction matrix (",
        n_points,
        " x ",
        k_paired,
        "). Consider reducing n_points or n_harmonics.",
        call. = FALSE
      )
    }
    
    calc_fourier_vec <- function(t_vec) {
      n_t <- length(t_vec)
      val <- rep(c_mean + 0i, n_t)   # always a plain complex vector
      
      if (!is.null(k_vec)) {
        # phase_mat[i, k] = t_vec[i] * k_vec[k]  (n_t x k_paired matrix)
        phase_mat <- outer(t_vec, k_vec)
        
        # %*% with a complex vector RHS returns an (n_t x 1) matrix.
        # as.vector() strips the dim attribute so that adding to `val`
        # (a plain vector) does not silently promote val to a matrix,
        # which would cause max(Im(val)) to return NA on some R builds.
        tmp <- 2i * pi * phase_mat
        harmonic_sum <- as.vector(exp(tmp) %*% c_pos + exp(-tmp) %*% c_neg)
        val <- val + harmonic_sum
      }
      
      if (has_nyquist) {
        val <- val + c_nyq * cos(pi * n_obs * t_vec)
      }
      
      # Sanity check: for a real-valued, uniformly-spaced input the imaginary
      # residual after reconstruction must be at machine-epsilon level.
      imag_part <- Im(val)
      if (anyNA(imag_part)) {
        warning(
          "stat_fourier: NaN in reconstruction output. ",
          "Please report this as a bug.",
          call. = FALSE
        )
      } else {
        max_imag <- max(abs(imag_part))
        if (max_imag > 1e-8) {
          warning(
            "stat_fourier: Unexpectedly large imaginary residual (",
            signif(max_imag, 3),
            "). Results may be unreliable. ",
            "Check for irregular `x` spacing or unresolved duplicates.",
            call. = FALSE
          )
        }
      }
      
      Re(val)
    }
    
    # 10. Evaluate on a fine output grid
    # x_fine spans [x_min, x_max] (closed interval for plotting purposes).
    # Mapping to t via period keeps t safely inside [0, x_range/period) which
    # is a strict subset of [0, 1), so the boundary is never reached.
    x_fine <- seq(x_min, x_max, length.out = n_points)
    t_fine <- (x_fine - x_min) / period   # in [0, x_range/period) c [0, 1)
    
    y_fourier <- calc_fourier_vec(t_fine)
    
    # Re-add the trend on the fine grid
    y_trend_fine <- if (!is.null(fit)) {
      stats::predict(fit, newdata = data.frame(x = x_fine))
    } else {
      rep(0, n_points)
    }
    
    data.frame(x = x_fine, y = y_fourier + y_trend_fine)
    
  }
)

#' @rdname geom_fourier
#' @export
stat_fourier <- function(mapping = NULL,
                         data = NULL,
                         geom = "line",
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         n_harmonics = NULL,
                         detrend = NULL,
                         ...) {
  ggplot2::layer(
    stat = StatFourier,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      n_harmonics = n_harmonics,
      detrend = detrend,
      ...
    )
  )
}
