# ==========================================================
# analyze_duration_QA (using duration_days only)
# ----------------------------------------------------------
# All thresholds are parameters. Chart labels, condition text,
# and printed messages are built from those parameters, so
# changing a threshold changes every label that describes it.
# ==========================================================
analyze_duration_QA <- function(
    DT,
    created_col      = "created_date",
    closed_col       = "closed_date",
    key_col          = "unique_key",
    tz               = "UTC",
    
    # --- Duration thresholds (days) ---
    lower_neg_days   = -2 * 365.25,
    extreme_neg_days = -5 * 365.25,
    upper_pos_days   =  2 * 365.25,
    extreme_pos_days =  5 * 365.25,
    max_outlier_days = 10 * 365.25,
    
    # --- Short-duration thresholds (seconds) ---
    near_zero_secs   = 29,   # 0 < duration <= near_zero_secs
    one_sec_max_secs = 2,    # 0 < duration <  one_sec_max_secs
    
    # --- Minimum agency counts for each category chart ---
    min_agency_obs_negative       = 5,
    min_agency_obs_zero           = 4,
    min_agency_obs_one_sec        = 4,
    min_agency_obs_large_positive = 4,
    min_agency_obs_extreme_pos    = 3,
    
    show_examples    = 10,
    show_samples     = TRUE,
    sample_size      = 10,
    in_place         = TRUE,
    print_summary    = TRUE,
    extra_keep_cols  = "agency",
    genesis_date     = as.POSIXct("2003-03-09", tz = tz),
    chart_dir        = "charts",
    example_seed     = NULL
) {
  
  stopifnot(data.table::is.data.table(DT))
  
  # --- Threshold validation ---
  stopifnot(is.numeric(lower_neg_days), is.numeric(extreme_neg_days),
            is.numeric(upper_pos_days), is.numeric(extreme_pos_days),
            is.numeric(max_outlier_days))
  # negatives: extreme < lower < 0
  stopifnot(extreme_neg_days < lower_neg_days, lower_neg_days < 0)
  # positives: 0 < upper < extreme < max_outlier
  stopifnot(0 < upper_pos_days,
            upper_pos_days < extreme_pos_days,
            extreme_pos_days < max_outlier_days)
  # short durations
  stopifnot(near_zero_secs > 0, one_sec_max_secs > 0)
  
  if (!is.null(example_seed)) set.seed(example_seed)
  
  # --- Derived thresholds ---
  secs_per_day     <- 86400
  days_per_year    <- 365.25
  near_zero_days   <- near_zero_secs   / secs_per_day
  one_sec_max_days <- one_sec_max_secs / secs_per_day
  
  # --- Label helpers ---
  fmt_d <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
  fmt_y <- function(x) formatC(x / days_per_year, format = "g", digits = 3)
  fmt_s <- function(x) formatC(x, format = "g", digits = 4)
  
  # --- Chart labels and condition text (all built from parameters) ---
  lbl_negative        <- "NEGATIVE"
  lbl_large_negative  <- sprintf("LARGE NEGATIVE (%s to %s yrs)",
                                 fmt_y(extreme_neg_days), fmt_y(lower_neg_days))
  lbl_extreme_neg     <- sprintf("EXTREME NEGATIVE (<= %s yrs)",
                                 fmt_y(extreme_neg_days))
  lbl_zero            <- "ZERO"
  lbl_one_sec         <- sprintf("ONE-SECOND (< %s sec)", fmt_s(one_sec_max_secs))
  lbl_near_zero       <- sprintf("NEAR-ZERO (<= %s sec)", fmt_s(near_zero_secs))
  lbl_large_positive  <- sprintf("LARGE POSITIVE (%s to %s yrs)",
                                 fmt_y(upper_pos_days), fmt_y(extreme_pos_days))
  lbl_extreme_pos     <- sprintf("EXTREME POSITIVE (%s to %s yrs)",
                                 fmt_y(extreme_pos_days), fmt_y(max_outlier_days))
  
  cond_large_negative <- sprintf("(<= %s days and > %s days)",
                                 fmt_d(lower_neg_days), fmt_d(extreme_neg_days))
  cond_extreme_neg    <- sprintf("(<= %s days)", fmt_d(extreme_neg_days))
  cond_zero           <- "(= 0 days)"
  cond_one_sec        <- sprintf("(0 < duration < %s sec)", fmt_s(one_sec_max_secs))
  cond_near_zero      <- sprintf("(0 < duration <= %s sec)", fmt_s(near_zero_secs))
  cond_large_positive <- sprintf("(> %s and <= %s days)",
                                 fmt_d(upper_pos_days), fmt_d(extreme_pos_days))
  cond_extreme_pos    <- sprintf("(> %s and < %s days)",
                                 fmt_d(extreme_pos_days), fmt_d(max_outlier_days))
  
  if (!all(c(created_col, closed_col) %in% names(DT))) {
    stop("created_col and/or closed_col not found in DT.")
  }
  
  X <- if (in_place) DT else data.table::copy(DT)
  cols_before <- data.table::copy(names(X))   # copy: names() of a data.table
                                               # updates in place when columns are added
  
  # Check if durations already exist, if not calculate them
  if (!"duration_days" %in% names(X)) {
    calculate_durations(
      X, created_col, closed_col, tz,
      in_place = TRUE, keep_parsed_timestamps = TRUE
    )
  } else {
    # Ensure parsed timestamps exist
    to_posix <- function(v) {
      if (inherits(v, "POSIXct")) {
        v
      } else if (requireNamespace("fasttime", quietly = TRUE)) {
        fasttime::fastPOSIXct(as.character(v), tz = tz)
      } else {
        as.POSIXct(as.character(v), tz = tz)
      }
    }
    
    if (!"created_ts" %in% names(X)) {
      X[, created_ts := to_posix(.SD[[1]]), .SDcols = created_col]
    }
    if (!"closed_ts" %in% names(X)) {
      X[, closed_ts := to_posix(.SD[[1]]), .SDcols = closed_col]
    }
  }
  
  # Denominator: rows with both timestamps present
  base_filter <- !is.na(X$created_ts) & !is.na(X$closed_ts)
  
  N_denom <- sum(base_filter)
  if (N_denom == 0L) warning("No rows with both timestamps; summary is empty.")
  
  # Independent counts taken directly from X (used for QA table and checks)
  neg_total <- if (N_denom) X[base_filter, sum(duration_days < 0,  na.rm = TRUE)] else 0L
  zero_total <- if (N_denom) X[base_filter, sum(duration_days == 0, na.rm = TRUE)] else 0L
  pos_total <- if (N_denom) X[base_filter, sum(duration_days > 0,  na.rm = TRUE)] else 0L
  near_zero_excl_zero <- if (N_denom) X[base_filter, sum(duration_days > 0 &
                                                         duration_days <= near_zero_days,
                                                         na.rm = TRUE)] else 0L
  
  pct <- function(x) if (N_denom) sprintf("%.2f%%", 100 * x / N_denom) else "NA"
  
  # Skew-oriented quantiles for positive durations (days)
  qs <- if (N_denom) {
    X[base_filter & duration_days > 0,
      as.list(stats::quantile(duration_days,
                              probs = c(0, .5, .9, .95, .99, 1),
                              na.rm = TRUE))]
  } else {
    as.list(rep(NA_real_, 6))
  }
  names(qs) <- c("min", "median", "p90", "p95", "p99", "max")
  
  qa_summary_dt <- data.table::data.table(
    metric = c(
      "denominator_rows",
      "negative_total", "zero_total", "positive_total", "near_zero_excl_zero",
      "pct_negative_total", "pct_zero", "pct_positive_total",
      "pct_near_zero_excl_zero", "pos_days_min", "pos_days_median", "pos_days_p90",
      "pos_days_p95", "pos_days_p99", "pos_days_max"
    ),
    value = c(
      formatC(N_denom,             format = "d", big.mark = ","),
      formatC(neg_total,           format = "d", big.mark = ","),
      formatC(zero_total,          format = "d", big.mark = ","),
      formatC(pos_total,           format = "d", big.mark = ","),
      formatC(near_zero_excl_zero, format = "d", big.mark = ","),
      pct(neg_total), pct(zero_total), pct(pos_total),
      pct(near_zero_excl_zero),
      if (is.na(qs$min)) rep(NA_character_, 6) else
        as.character(round(unlist(qs), 3))
    )
  )
  
  keep_cols <- intersect(
    c(key_col, created_col, closed_col,
      "created_ts", "closed_ts",
      "duration_days",
      extra_keep_cols),
    names(X)
  )
  
  # =========
  # Subsets
  # =========
  negative_all <- X[base_filter & duration_days < 0, .SD, .SDcols = keep_cols]
  
  negative_small <- X[base_filter & duration_days < 0 &
                        duration_days > lower_neg_days, .SD, .SDcols = keep_cols]
  
  negative_large <- X[base_filter &
                        duration_days <= lower_neg_days &
                        duration_days >  extreme_neg_days, .SD, .SDcols = keep_cols]
  
  negative_extreme <- X[base_filter &
                          duration_days <= extreme_neg_days, .SD, .SDcols = keep_cols]
  
  zero_all <- X[base_filter & duration_days == 0, .SD, .SDcols = keep_cols]
  
  one_sec_all <- X[base_filter & duration_days > 0 &
                     duration_days < one_sec_max_days, .SD, .SDcols = keep_cols]
  
  # Positive only, matching near_zero_excl_zero in the QA table and the label
  near_zero_ex <- X[base_filter & duration_days > 0 &
                      duration_days <= near_zero_days, .SD, .SDcols = keep_cols]
  
  # All positive and small positive durations are ~15M rows each, so they
  # are summarised from the duration vector instead of copied as tables.
  dd             <- X$duration_days
  pos_mask       <- base_filter & !is.na(dd) & dd > 0
  pos_small_mask <- pos_mask & dd <= upper_pos_days
  
  positive_large <- X[base_filter &
                        duration_days >  upper_pos_days &
                        duration_days <= extreme_pos_days, .SD, .SDcols = keep_cols]
  
  positive_extreme <- X[base_filter &
                          duration_days >  extreme_pos_days &
                          duration_days <  max_outlier_days, .SD, .SDcols = keep_cols]
  
  excluded_extreme <- X[base_filter &
                          duration_days >= max_outlier_days, .SD, .SDcols = keep_cols]
  
  if (nrow(excluded_extreme)) {
    cat(sprintf(
      "\nExcluded %s rows with duration_days >= %s days (%s yrs; beyond max_outlier_days cap):\n",
      prettyNum(nrow(excluded_extreme), big.mark = ","),
      fmt_d(max_outlier_days), fmt_y(max_outlier_days)
    ))
    nshow <- min(nrow(excluded_extreme), show_examples)
    tmp <- rsample(excluded_extreme, nshow)[order(-duration_days)]
    print(tmp[, c(.SD, .(duration_days = round(duration_days, 2))),
              .SDcols = setdiff(names(tmp), "duration_days")],
          row.names = FALSE, right = FALSE)
  }
  
  print_cols <- setdiff(keep_cols, c("created_ts", "closed_ts"))
  
  # Counts
  total_closed    <- N_denom
  neg_count       <- nrow(negative_all)
  neg_small       <- nrow(negative_small)
  neg_large       <- nrow(negative_large)
  neg_extreme_cnt <- nrow(negative_extreme)
  zero_cnt        <- nrow(zero_all)
  one_sec_cnt     <- nrow(one_sec_all)
  nearz_cnt       <- nrow(near_zero_ex)
  pos_count       <- sum(pos_mask)
  pos_small       <- sum(pos_small_mask)
  pos_large       <- nrow(positive_large)
  pos_extreme_cnt <- nrow(positive_extreme)
  pos_excluded    <- nrow(excluded_extreme)
  
  # Summary stat helper
  stat_or_na <- function(DT, fun) {
    if (nrow(DT) > 0) round(fun(DT$duration_days, na.rm = TRUE), 2) else NA_real_
  }
  
  # --- Negative durations ---
  avg_neg_all     <- stat_or_na(negative_all,     mean)
  avg_neg_small   <- stat_or_na(negative_small,   mean)
  avg_neg_large   <- stat_or_na(negative_large,   mean)
  avg_neg_extreme <- stat_or_na(negative_extreme, mean)
  
  min_neg_all     <- stat_or_na(negative_all,     min)
  min_neg_small   <- stat_or_na(negative_small,   min)
  min_neg_large   <- stat_or_na(negative_large,   min)
  min_neg_extreme <- stat_or_na(negative_extreme, min)
  
  max_neg_all     <- stat_or_na(negative_all,     max)
  max_neg_small   <- stat_or_na(negative_small,   max)
  max_neg_large   <- stat_or_na(negative_large,   max)
  max_neg_extreme <- stat_or_na(negative_extreme, max)
  
  # Same summary for a plain vector (all and small positive durations)
  stat_vec <- function(v, fun) if (length(v)) round(fun(v, na.rm = TRUE), 2) else NA_real_
  v_all   <- dd[pos_mask]
  v_small <- dd[pos_small_mask]
  pos_small_in_range <- !length(v_small) || all(v_small > 0 & v_small <= upper_pos_days)
  
  # --- Positive durations ---
  avg_pos_all     <- stat_vec(v_all,   mean)
  avg_pos_small   <- stat_vec(v_small, mean)
  avg_pos_large   <- stat_or_na(positive_large,   mean)
  avg_pos_extreme <- stat_or_na(positive_extreme, mean)
  
  min_pos_all     <- stat_vec(v_all,   min)
  min_pos_small   <- stat_vec(v_small, min)
  min_pos_large   <- stat_or_na(positive_large,   min)
  min_pos_extreme <- stat_or_na(positive_extreme, min)
  
  max_pos_all     <- stat_vec(v_all,   max)
  max_pos_small   <- stat_vec(v_small, max)
  rm(v_all, v_small, pos_mask, pos_small_mask, dd)
  max_pos_large   <- stat_or_na(positive_large,   max)
  max_pos_extreme <- stat_or_na(positive_extreme, max)
  
  # Print negative breakdown
  cat("\nNegative duration breakdown:\n")
  cat("  Total closed:       ", total_closed, "\n")
  cat("  Negative total:     ", neg_count, "\n")
  cat("    Negative small:   ", neg_small, "\n")
  cat("    Negative large:   ", neg_large, "\n")
  cat("    Negative extreme: ", neg_extreme_cnt, "\n")
  
  # Define summary table
  pct_of_closed <- function(n) if (total_closed) round(100 * n / total_closed, 2) else NA_real_
  
  summary_dt <- data.table::data.table(
    denominator_closed_rows = total_closed,
    negative_total       = neg_count,
    negative_small       = neg_small,
    negative_large       = neg_large,
    negative_extreme     = neg_extreme_cnt,
    pct_negative_total   = pct_of_closed(neg_count),
    pct_negative_small   = pct_of_closed(neg_small),
    pct_negative_large   = pct_of_closed(neg_large),
    pct_negative_extreme = pct_of_closed(neg_extreme_cnt),
    
    avg_neg_all     = avg_neg_all,
    avg_neg_small   = avg_neg_small,
    avg_neg_large   = avg_neg_large,
    avg_neg_extreme = avg_neg_extreme,
    
    min_neg_all     = min_neg_all,
    min_neg_small   = min_neg_small,
    min_neg_large   = min_neg_large,
    min_neg_extreme = min_neg_extreme,
    
    max_neg_all     = max_neg_all,
    max_neg_small   = max_neg_small,
    max_neg_large   = max_neg_large,
    max_neg_extreme = max_neg_extreme,
    
    zero_total           = zero_cnt,
    near_zero_leq_days   = near_zero_days,
    near_zero_leq_secs   = near_zero_secs,
    one_sec_total        = one_sec_cnt,
    near_zero_excl_zero  = nearz_cnt,
    
    positive_total       = pos_count,
    positive_small       = pos_small,
    positive_large       = pos_large,
    positive_extreme     = pos_extreme_cnt,
    positive_excluded    = pos_excluded,
    
    pct_positive_total   = pct_of_closed(pos_count),
    pct_positive_small   = pct_of_closed(pos_small),
    pct_positive_large   = pct_of_closed(pos_large),
    pct_positive_extreme = pct_of_closed(pos_extreme_cnt),
    
    avg_pos_all     = avg_pos_all,
    avg_pos_small   = avg_pos_small,
    avg_pos_large   = avg_pos_large,
    avg_pos_extreme = avg_pos_extreme,
    
    min_pos_all     = min_pos_all,
    min_pos_small   = min_pos_small,
    min_pos_large   = min_pos_large,
    min_pos_extreme = min_pos_extreme,
    
    max_pos_all     = max_pos_all,
    max_pos_small   = max_pos_small,
    max_pos_large   = max_pos_large,
    max_pos_extreme = max_pos_extreme,
    
    pct_zero                = pct_of_closed(zero_cnt),
    pct_one_sec_total       = pct_of_closed(one_sec_cnt),
    pct_near_zero_excl_zero = pct_of_closed(nearz_cnt)
  )
  
  if (isTRUE(print_summary)) {
    cat("\n\nDuration QA summary (rows with both created and closed timestamps):\n")
    
    vals_num <- as.numeric(unlist(as.list(summary_dt[1, ]), use.names = FALSE))
    existing <- names(summary_dt)
    
    preferred <- c(
      "denominator_closed_rows",
      "negative_total", "negative_small", "negative_large", "negative_extreme",
      "zero_total", "near_zero_leq_days", "near_zero_leq_secs", "near_zero_excl_zero",
      "one_sec_total",
      "positive_total", "positive_small", "positive_large", "positive_extreme",
      "positive_excluded",
      "pct_negative_total", "pct_negative_small", "pct_negative_large", "pct_negative_extreme",
      "pct_zero", "pct_one_sec_total", "pct_near_zero_excl_zero",
      "pct_positive_total", "pct_positive_small", "pct_positive_large", "pct_positive_extreme"
    )
    
    preferred_existing <- intersect(preferred, existing)
    extras             <- setdiff(existing, preferred_existing)
    levels_all         <- c(preferred_existing, extras)
    
    summary_vert <- data.table::data.table(
      metric = factor(existing, levels = levels_all),
      value  = vals_num
    )
    data.table::setorder(summary_vert, metric)
    
    fmt_num <- function(x) ifelse(is.na(x), "—",
                                  prettyNum(x, big.mark = ",", scientific = FALSE,
                                            preserve.width = "individual"))
    fmt_pct <- function(x) ifelse(is.na(x), "—", sprintf("%.2f%%", x))
    
    summary_vert[
      grepl("^pct_", as.character(metric)), display := fmt_pct(value)
    ][
      !grepl("^pct_", as.character(metric)), display := fmt_num(value)
    ]
    
    dt_print <- summary_vert[, .(metric = as.character(metric), value = display)]
    dfp <- as.data.frame(dt_print)
    w   <- max(nchar(dfp$metric), na.rm = TRUE)
    dfp$metric <- format(dfp$metric, width = w, justify = "left")
    dfp$value  <- format(dfp$value,  justify = "right")
    
    print(noquote(dfp), row.names = FALSE)
  }
  
  # -------------------------------
  # Call duration category analyzers
  # -------------------------------
  analyze_duration_category(
    DT             = negative_all,
    label          = lbl_negative,
    chart_dir      = chart_dir,
    alt_DT         = negative_small,
    min_agency_obs = min_agency_obs_negative,
    show_examples  = show_examples,
    key_col        = key_col,
    print_cols     = print_cols,
    sort_desc      = FALSE,
    show_threshold_80_flag = FALSE,
    show_count_labels = TRUE,
    make_boxplot   = TRUE
  )
  
  analyze_duration_category(
    DT             = negative_large,
    label          = lbl_large_negative,
    condition_text = cond_large_negative,
    chart_dir      = chart_dir,
    threshold_days = extreme_neg_days,
    show_examples  = show_examples,
    key_col        = key_col,
    show_threshold_80_flag = FALSE,
    show_count_labels = TRUE,
    print_cols     = print_cols
  )
  
  analyze_duration_category(
    DT             = negative_extreme,
    label          = lbl_extreme_neg,
    condition_text = cond_extreme_neg,
    chart_dir      = chart_dir,
    threshold_days = extreme_neg_days,
    show_examples  = show_examples,
    key_col        = key_col,
    make_boxplot   = FALSE,
    show_threshold_80_flag = TRUE,
    show_count_labels = TRUE,
    print_cols     = print_cols
  )
  
  analyze_duration_category(
    DT             = zero_all,
    label          = lbl_zero,
    condition_text = cond_zero,
    chart_dir      = chart_dir,
    show_examples  = show_examples,
    key_col        = key_col,
    print_cols     = print_cols,
    make_boxplot   = FALSE,
    show_threshold_80_flag = TRUE,
    show_count_labels = TRUE,
    min_agency_obs = min_agency_obs_zero
  )
  
  analyze_duration_category(
    DT             = one_sec_all,
    label          = lbl_one_sec,
    condition_text = cond_one_sec,
    chart_dir      = chart_dir,
    show_examples  = show_examples,
    key_col        = key_col,
    print_cols     = print_cols,
    make_boxplot   = FALSE,
    show_threshold_80_flag = TRUE,
    show_count_labels = TRUE,
    min_agency_obs = min_agency_obs_one_sec
  )
  
  analyze_duration_category(
    DT             = near_zero_ex,
    label          = lbl_near_zero,
    condition_text = cond_near_zero,
    chart_dir      = chart_dir,
    show_examples  = show_examples,
    key_col        = key_col,
    show_threshold_80_flag = TRUE,
    show_count_labels = TRUE,
    print_cols     = print_cols
  )
  
  analyze_duration_category(
    DT             = positive_large,
    label          = lbl_large_positive,
    condition_text = cond_large_positive,
    chart_dir      = chart_dir,
    threshold_days = extreme_pos_days,
    show_examples  = show_examples,
    key_col        = key_col,
    print_cols     = print_cols,
    make_boxplot   = TRUE,
    show_threshold_80_flag = TRUE,
    show_count_labels = TRUE,
    min_agency_obs = min_agency_obs_large_positive,
    sort_desc      = TRUE
  )
  
  analyze_duration_category(
    DT             = positive_extreme,
    label          = lbl_extreme_pos,
    condition_text = cond_extreme_pos,
    chart_dir      = chart_dir,
    threshold_days = extreme_pos_days,
    show_examples  = show_examples,
    key_col        = key_col,
    print_cols     = print_cols,
    min_agency_obs = min_agency_obs_extreme_pos,
    make_boxplot   = TRUE,
    show_count_labels = TRUE,
    show_threshold_80_flag = FALSE,
    sort_desc      = TRUE
  )
  
  # -------------------------------
  # Integrity checks (independent of summary_dt)
  # -------------------------------
  # 1. Bins partition the totals counted directly from X
  if (neg_small + neg_large + neg_extreme_cnt != neg_total) {
    stop(sprintf("Negative bins (%s) do not sum to negative total (%s).",
                 neg_small + neg_large + neg_extreme_cnt, neg_total))
  }
  if (pos_small + pos_large + pos_extreme_cnt + pos_excluded != pos_total) {
    stop(sprintf("Positive bins (%s) do not sum to positive total (%s).",
                 pos_small + pos_large + pos_extreme_cnt + pos_excluded, pos_total))
  }
  
  # 2. Each bin's durations fall inside its own thresholds
  in_range <- function(DT, lo, hi, lo_incl = FALSE, hi_incl = TRUE) {
    if (!nrow(DT)) return(TRUE)
    d <- DT$duration_days
    lo_ok <- if (lo_incl) all(d >= lo) else all(d > lo)
    hi_ok <- if (hi_incl) all(d <= hi) else all(d < hi)
    lo_ok && hi_ok
  }
  stopifnot(
    in_range(negative_small,   lower_neg_days,   0,                hi_incl = FALSE),
    in_range(negative_large,   extreme_neg_days, lower_neg_days),
    in_range(negative_extreme, -Inf,             extreme_neg_days),
    pos_small_in_range,
    in_range(positive_large,   upper_pos_days,   extreme_pos_days),
    in_range(positive_extreme, extreme_pos_days, max_outlier_days, hi_incl = FALSE),
    in_range(excluded_extreme, max_outlier_days, Inf,              lo_incl = TRUE)
  )
  
  # Remove helper timestamp columns this function added to DT (the subsets
  # keep their own copies of created_ts / closed_ts)
  added <- intersect(setdiff(names(X), cols_before), c("created_ts", "closed_ts"))
  if (in_place && length(added)) X[, (added) := NULL]
  
  invisible(list(
    summary          = summary_dt,
    qa_summary_table = qa_summary_dt,
    thresholds       = list(
      lower_neg_days   = lower_neg_days,
      extreme_neg_days = extreme_neg_days,
      upper_pos_days   = upper_pos_days,
      extreme_pos_days = extreme_pos_days,
      max_outlier_days = max_outlier_days,
      near_zero_secs   = near_zero_secs,
      one_sec_max_secs = one_sec_max_secs
    ),
    negative_all     = negative_all,
    negative_small   = negative_small,
    negative_large   = negative_large,
    negative_extreme = negative_extreme,
    zero_rows        = zero_all,
    one_sec_rows     = one_sec_all,
    near_zero_rows   = near_zero_ex,
    positive_large   = positive_large,
    positive_extreme = positive_extreme,
    excluded_extreme = excluded_extreme
  ))
}
