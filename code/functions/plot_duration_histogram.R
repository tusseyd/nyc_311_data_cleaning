################################################################################
# plot_duration_histogram.R
# Histogram of short SR durations (e.g. 2-90 seconds) with bins of a fixed
# width, bars at or below a threshold highlighted, and an optional
# cumulative chart. Prints the "Duration histogram summary" block.
#
# Drawing is done by plot_histogram(); this wrapper handles the duration-
# specific filters, bin choice, labels, and console summary. Arguments are
# unchanged from the earlier version, so existing calls still work.
#
# Requires: plot_histogram.R, chart_style.R, david_theme.R
################################################################################

plot_duration_histogram <- function(
    DT,
    duration_col      = "duration_sec",   # "duration_sec" or "duration_days"
    chart_dir,
    filename          = "duration_histogram.pdf",
    title             = NULL,
    subtitle          = NULL,
    max_value         = NULL,             # same units as duration_col
    min_value         = 1,
    include_zero      = FALSE,
    include_one       = FALSE,
    exclude_negative  = TRUE,
    bin_width         = 1,
    bin_type          = c("custom", "individual", "minutes", "hours", "days", "auto"),
    create_cumulative = TRUE,
    bar_color         = "#009E73",
    threshold_numeric = NULL,             # threshold, same units as duration_col
    left_bar_color    = "#D55E00",        # color for bars at or below the threshold
    x_axis_angle      = 0,
    width_in          = NULL,             # NULL = chart_style()
    height_in         = NULL,

    # --- Deprecated: accepted so existing calls still run (ignored) ---
    text_size         = NULL,
    x_label_skip      = NULL
) {
  stopifnot(data.table::is.data.table(DT))
  if (!duration_col %in% names(DT)) stop(sprintf("DT must have a '%s' column.", duration_col))

  if (duration_col == "duration_sec") {
    data_units <- "seconds"; sec_conversion <- 1
  } else if (duration_col == "duration_days") {
    data_units <- "days";    sec_conversion <- 86400
  } else {
    stop("duration_col must be either 'duration_sec' or 'duration_days'.")
  }

  # ---- Filters ----
  d <- data.table::data.table(duration = as.numeric(DT[[duration_col]]))[!is.na(duration)]
  if (exclude_negative)  d <- d[duration >= 0]
  if (!include_zero)     d <- d[duration != 0]
  if (!include_one)      d <- d[duration != 1]
  if (is.null(max_value)) max_value <- if (data_units == "seconds") 300 else 300 / 86400
  d <- d[duration >= min_value & duration <= max_value]
  if (!nrow(d)) {
    message(sprintf("No data in range [%g, %g] %s after filters.", min_value, max_value, data_units))
    return(invisible(NULL))
  }

  # ---- Bin width ----
  bin_type <- match.arg(bin_type)
  if (bin_type != "custom") {
    rng <- max_value - min_value
    bin_width <- if (data_units == "seconds") {
      switch(bin_type, individual = 1, minutes = 60, hours = 3600, days = 86400,
             auto = if (rng <= 120) 1 else if (rng <= 3600) 60 else if (rng <= 86400) 3600 else 86400)
    } else {
      switch(bin_type, individual = 1/86400, minutes = 60/86400, hours = 3600/86400, days = 1,
             auto = if (rng <= 120/86400) 1/86400 else if (rng <= 1) 60/86400 else 1)
    }
  }
  if (bin_width <= 0) stop("bin_width must be > 0.")

  bw_sec <- bin_width * sec_conversion
  bin_label <- if (isTRUE(all.equal(bw_sec, 1))) "1-second bins"
               else if (isTRUE(all.equal(bw_sec, 60))) "1-minute bins"
               else if (isTRUE(all.equal(bw_sec, 3600))) "1-hour bins"
               else if (isTRUE(all.equal(bw_sec, 86400))) "1-day bins"
               else sprintf("Binned by %g %s", bin_width, data_units)

  if (is.null(title)) {
    title <- sprintf("Distribution of Durations (%s to %s %s)",
                     scales::comma(min_value), scales::comma(max_value), data_units)
  }
  if (is.null(subtitle)) subtitle <- sprintf("n = %s | %s", scales::comma(nrow(d)), bin_label)

  # ---- Draw (plot_histogram) ----
  res <- plot_histogram(
    DT              = d,
    value_col       = "duration",
    chart_dir       = chart_dir,
    filename        = filename,
    title           = title,
    subtitle        = subtitle,
    x_label         = sprintf("Duration (%s)", data_units),
    y_label         = NULL,
    bin_width       = bin_width,
    min_value       = min_value,
    max_value       = max_value,
    threshold       = threshold_numeric,
    threshold_label = "Suspicious threshold",
    highlight_color = left_bar_color,
    fill_color      = bar_color,
    alpha           = 1,
    add_stats       = FALSE,
    x_axis_angle    = x_axis_angle,
    cumulative      = create_cumulative,
    print_summary   = FALSE,
    width_in        = width_in,
    height_in       = height_in
  )

  # ---- Console summary ----
  vals_sec <- d$duration * sec_conversion
  m_sec <- mean(vals_sec); md_sec <- stats::median(vals_sec); s_sec <- stats::sd(vals_sec)
  cat(sprintf("\nDuration histogram summary (%s to %s %s):\n",
              scales::comma(min_value), scales::comma(max_value), data_units))
  cat(sprintf("Total observations: %s\n", scales::comma(nrow(d))))
  cat(sprintf("Mean:   %s seconds (%.6f days)\n", scales::comma(round(m_sec, 2)), m_sec / 86400))
  cat(sprintf("Median: %s seconds (%.6f days)\n", scales::comma(round(md_sec, 2)), md_sec / 86400))
  cat(sprintf("Std dev: %s seconds (%.6f days)\n", scales::comma(round(s_sec, 2)), s_sec / 86400))
  cat(sprintf("Bin width: %g %s (%s seconds, %.6f days)\n",
              bin_width, data_units, scales::comma(bw_sec), bw_sec / 86400))

  invisible(list(
    histogram_plot  = res$plot,
    cumulative_plot = res$cumulative_plot,
    data            = res$data,
    files           = res$files
  ))
}
