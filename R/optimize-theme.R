
coarse_fine_threshold_auto <- function(
  fn, lower, upper, threshold,
  decreasing = TRUE,
  decimals = 1,                  # finest grid = 10^(-decimals)
  target_coarse_pts = 20,        # ~how many coarse points across the range
  refine = TRUE,
  return_trace = FALSE,
  ...
) {
  stopifnot(upper > lower)

  direction <- ifelse(decreasing, "decreasing", "increasing")

  meet <- if (direction == "increasing") function(s) s >= threshold else function(s) s <= threshold
  
  unit <- 10^(-decimals)         # finest step
  
  rng  <- upper - lower
  
  # choose coarse step ≈ range / target_coarse_pts, snapped to unit * 10^k
  raw_coarse <- max(rng / target_coarse_pts, unit)
  
  k <- floor(log10(raw_coarse / unit))
  
  coarse <- unit * (10^k)
  
  mid <- max(unit, coarse / 5)   # a reasonable middle step
  
  # snap mid to a multiple of unit
  mid <- unit * round(mid / unit)
  
  steps <- sort(unique(c(coarse, mid, unit)), decreasing = TRUE)
  
  order_seq <- function(lo, hi, by) {
    if (direction == "increasing") {
      seq(lo, hi, by = by)

    } else {
      seq(hi, lo, by = -by)
    }
  }

  clamp <- function(x, lo, hi) max(lo, min(hi, x))
  
  lo <- lower
  hi <- upper
  
  total_evals <- 0L
  
  crossed <- FALSE
  
  best_x <- NA_real_
  best_s <- NA_real_
  
  tx <- numeric(0)
  ts <- numeric(0)

  for (k in seq_along(steps)) {
    by <- steps[k]
    xs <- order_seq(lo, hi, by)

    # ensure endpoint included (floating seq guard)
    if (direction == "increasing" && tail(xs, 1) < hi - 1e-12) {
      xs <- c(xs, hi)
    }

    if (direction == "decreasing" && tail(xs, 1) > lo + 1e-12) {
      xs <- c(xs, lo)
    }

    prev_x       <- NA_real_
    crossed_here <- FALSE

    # browser()

    for (x in xs) {
      s <- fn(x, ...)

      total_evals <- total_evals + 1L
      
      if (return_trace) {
        tx <- c(tx, x)
        ts <- c(ts, s)
      }

      if (is.na(best_s) || abs(s - threshold) < abs(best_s - threshold)) {
        best_s <- s
        best_x <- x
      }
      
      if (meet(s)) {
        crossed <- TRUE
        crossed_here <- TRUE
        
        if (k == length(steps) || !refine) {
          res <- list(
            value   = x,
            dist    = s,
            crossed = TRUE,
            evals   = total_evals,
            steps   = steps,
            trace   = if (return_trace) data.frame(x = tx, dist = ts) else NULL
          )

          return(res)
        }

        # bracket for next (finer) pass
        if (is.na(prev_x)) {
          if (direction == "increasing") {
            lo <- clamp(x - by, lower, upper)
            hi <- x
          
          } else {
            lo <- x
            hi <- clamp(x + by, lower, upper)
          }
        } else {
          lo <- min(prev_x, x)
          hi <- max(prev_x, x)
        }

        break()
      }

      prev_x <- x
    }
    
    if (!crossed_here && k == length(steps)) {
      res <- list(
        value   = best_x,
        dist    = best_s,
        crossed = FALSE,
        evals   = total_evals,
        steps   = steps,
        trace   = if (return_trace) data.frame(x = tx, dist = ts) else NULL
      )

      return(res)
    }
  }
  
  res <- list(
    value   = best_x,
    dist    = best_s,
    crossed = crossed,
    evals   = total_evals,
    steps   = steps,
    trace   = if (return_trace) data.frame(x = tx, dist = ts) else NULL)
  
  res
}
