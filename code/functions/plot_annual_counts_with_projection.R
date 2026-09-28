################################################################################
# plot_annual_counts_with_projection.R
# Annual SR counts as a bar chart, with an optional projected bar for a
# partial year, a trendline over the actual years, and multi-year
# statistics printed to the console.
#
# The chart is drawn by plot_barchart(); this function only builds the
# annual table and prints the statistics. Arguments are unchanged from the
# earlier version, so existing calls still work.
#
# Requires: plot_barchart.R, chart_style.R, david_theme.R
################################################################################

plot_annual_counts_with_projection <- function(
    DT,
    created_col    = "created_date",
    estimate_flag  = FALSE,
    estimate_date  = NULL,         # e.g., "2025-08-24"
    estimate_value = NULL,         # SRs so far in that year, as of estimate_date
    chart_dir      = "charts",
    filename       = "annual_trend_plot.pdf",
    title          = "Annual 311 Service Requests",
    subtitle       = NULL,
    include_projection_in_growth = FALSE,
    include_projection_in_stats  = FALSE,
    show_projection_bar          = FALSE
) {
  stopifnot(data.table::is.data.table(DT), created_col %in% names(DT))
  tz <- attr(DT[[created_col]], "tzone"); if (is.null(tz) || !nzchar(tz)) tz <- "America/New_York"

  # ---- Annual counts (actuals) ----
  annual <- DT[, .(count = .N), by = .(year = data.table::year(get(created_col)))][order(year)]
  annual[, actual := TRUE]

  # ---- Projected year (full-year estimate from a partial count) ----
  projection <- NULL
  if (isTRUE(estimate_flag)) {
    stopifnot(!is.null(estimate_date), !is.null(estimate_value))
    est_date   <- as.Date(estimate_date)
    year_est   <- as.integer(format(est_date, "%Y"))
    days_in_yr <- if (lubridate::leap_year(year_est)) 366 else 365
    projection <- data.table::data.table(
      year   = year_est,
      count  = as.integer(round(estimate_value * days_in_yr / lubridate::yday(est_date))),
      actual = FALSE
    )
  }

  plot_data <- if (!is.null(projection) && isTRUE(show_projection_bar)) {
    data.table::rbindlist(list(annual, projection))
  } else {
    data.table::copy(annual)
  }
  plot_data[, status := ifelse(actual, "Actual", "Projected")]
  plot_data[, bar_label := ifelse(actual, scales::comma(count),
                                  paste0(scales::comma(count), " (proj.)"))]

  # ---- Growth over the trend years ----
  growth_data <- if (isTRUE(include_projection_in_growth) && !is.null(projection)) {
    data.table::rbindlist(list(annual, projection))[order(year)]
  } else {
    annual
  }
  growth_pct <- growth_data[.N, count] / growth_data[1, count] - 1

  # ---- Chart (drawn by plot_barchart) ----
  n_total <- annual[, sum(count)]
  n_growth <- sprintf("n = %s; growth %d-%d: %s", scales::comma(n_total),
                      growth_data[1, year], growth_data[.N, year],
                      scales::percent(growth_pct, accuracy = 0.1))
  subtitle_final <- if (is.null(subtitle) || !nzchar(subtitle)) n_growth else paste0(subtitle, " (", n_growth, ")")

  p <- plot_barchart(
    DT               = plot_data,
    x_col            = "year",
    y_col            = "count",
    title            = title,
    subtitle         = subtitle_final,
    fill_col         = "status",
    fill_colors      = c(Actual = "#009E73", Projected = "#F0E442"),
    label_col        = "bar_label",
    add_trendline    = TRUE,
    show_trend_stats = FALSE,
    trend_filter_col = if (isTRUE(include_projection_in_growth)) NULL else "actual",
    show_summary     = FALSE,
    chart_dir        = chart_dir,
    filename         = filename
  )

  # ---- Multi-year statistics (console) ----
  stats_data <- if (isTRUE(include_projection_in_stats) && !is.null(projection)) {
    data.table::rbindlist(list(annual, projection))
  } else {
    annual
  }

  daily   <- DT[, .(n = .N), by = .(date = as.Date(get(created_col), tz = tz))]
  monthly <- DT[, .(n = .N), by = .(month = format(get(created_col), "%Y-%m", tz = tz))]

  cat("\nMulti-year statistics ",
      if (isTRUE(include_projection_in_stats)) "(Actuals + Projected):\n" else "(Actuals only):\n",
      sep = "")
  cat("  Years covered: ", paste(range(stats_data$year), collapse = "–"), "\n", sep = "")
  cat("  Total Records: ", scales::comma(sum(stats_data$count)), "\n", sep = "")
  cat("  Yearly Mean:   ", scales::comma(round(mean(stats_data$count), 1)), "\n", sep = "")
  cat("  Yearly Median: ", scales::comma(round(stats::median(stats_data$count), 1)), "\n", sep = "")
  cat("  StdDev:        ", scales::comma(round(stats::sd(stats_data$count), 1)), "\n", sep = "")
  cat("  Max Year: ", stats_data[which.max(count), year], " (",
      scales::comma(max(stats_data$count)), ")\n", sep = "")
  cat("  Min Year: ", stats_data[which.min(count), year], " (",
      scales::comma(min(stats_data$count)), ")\n", sep = "")
  cat("  Growth %: ", scales::percent(growth_pct, accuracy = 0.1), "\n", sep = "")
  cat("  Busiest Month: ", monthly[which.max(n), month], " (", scales::comma(max(monthly$n)), ")\n", sep = "")
  cat("  Least Busy Month: ", monthly[which.min(n), month], " (", scales::comma(min(monthly$n)), ")\n", sep = "")
  cat("  Busiest Day: ", format(daily[which.max(n), date]), " (", scales::comma(max(daily$n)), ")\n", sep = "")
  cat("  Least Busy Day: ", format(daily[which.min(n), date]), " (", scales::comma(min(daily$n)), ")\n", sep = "")

  cat("\nYear-by-year counts:\n")
  all_years <- if (!is.null(projection)) data.table::rbindlist(list(annual, projection)) else annual
  for (i in seq_len(nrow(all_years))) {
    cat("  ", all_years$year[i], ": ", scales::comma(all_years$count[i]),
        if (!all_years$actual[i]) " (Projected)" else "", "\n", sep = "")
  }
  cat("\n")

  invisible(list(
    data  = all_years,
    plot  = p,
    stats = data.table::data.table(
      years_start = min(stats_data$year), years_end = max(stats_data$year),
      mean = mean(stats_data$count), median = stats::median(stats_data$count),
      sd = stats::sd(stats_data$count),
      max_year = stats_data[which.max(count), year], max_count = max(stats_data$count),
      min_year = stats_data[which.min(count), year], min_count = min(stats_data$count),
      growth_pct = growth_pct,
      busiest_month = monthly[which.max(n), month], busiest_month_count = max(monthly$n),
      least_busy_month = monthly[which.min(n), month], least_busy_month_count = min(monthly$n),
      busiest_date = daily[which.max(n), date], busiest_count = max(daily$n),
      least_busy_date = daily[which.min(n), date], least_busy_count = min(daily$n),
      included_projection = include_projection_in_stats
    )
  ))
}
