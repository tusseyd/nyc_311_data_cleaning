analyze_datetime_patterns <- function(
    DT,
    datetime_col,           # unquoted column (e.g., closed_date)
    chart_dir,
    label = "Closed",       # label for titles ("Closed" or "Created")
    agency_col = agency             # unquoted column with agency name
) {
  stopifnot(data.table::is.data.table(DT))
  
  
  # No need for deparse/substitute - already strings
  datetime_name <- datetime_col
  agency <- agency_col
  
  class(agency)
  length(agency)
  head(agency)
  
  # Ensure POSIXct
  if (!inherits(DT[[datetime_name]], "POSIXct")) {
    stop(sprintf("%s must be POSIXct", datetime_name))
  }
  
  # --- Time zone diagnostics & TZ-safe year -------------------------------
  LOCAL_TZ <- "America/New_York"
  tz_att <- attr(DT[[datetime_name]], "tzone")
  
  # Check if we need to convert from UTC to local
  if (is.null(tz_att) || tz_att == "" || tz_att == "UTC") {
    # Convert from UTC to local and extract components
    DT[, `:=`(
      year_local = lubridate::year(lubridate::with_tz(get(datetime_name), LOCAL_TZ)),
      hour = lubridate::hour(lubridate::with_tz(get(datetime_name), LOCAL_TZ)),
      minute = lubridate::minute(lubridate::with_tz(get(datetime_name), LOCAL_TZ)),
      second = lubridate::second(lubridate::with_tz(get(datetime_name), LOCAL_TZ))
    )]
  } else {
    # Already America/New_York → just extract components
    DT[, `:=`(
      year_local = lubridate::year(get(datetime_name)),
      hour = lubridate::hour(get(datetime_name)),
      minute = lubridate::minute(get(datetime_name)),
      second = lubridate::second(get(datetime_name))
    )]
  }
  
  # ===============================
  # Hour-of-day distribution (ALL 24 hours, ordered by working hours)
  # ===============================
  all_hours <- data.table(hour = 0:23)
  hour_summary <- DT[!is.na(hour), .N, by = hour]
  hour_summary <- merge(all_hours, hour_summary, by = "hour", all.x = TRUE)
  hour_summary[is.na(N), N := 0]
  
  # Create working hours order: 08-23, then 00-07
  working_hours_order <- c(8:23, 0:7)
  hour_summary[, display_order := match(hour, working_hours_order)]
  setorder(hour_summary, display_order)
  
  # Percent and cumulative percent
  hour_summary[, pct := round(100 * N / sum(N), 2)]
  hour_summary[, cum_pct := cumsum(pct)]
  
  # For the chart, use the same working hours order as the table
  hour_summary_chart <- copy(hour_summary)
  hour_summary_chart[, hour_factor := factor(sprintf("%02d", hour), 
                                             levels = sprintf("%02d", 
                                                              working_hours_order))]
  
  if (sum(hour_summary_chart$N) > 0) {
    plot_barchart(
      DT        = hour_summary_chart,
      x_col     = "hour_factor",
      y_col     = "N",
      title     = sprintf("%s Requests by Hour of Day (Working Hours Order)", label),
      subtitle  = sprintf("n = %s | Working hours: 08:00–23:59", 
                          format(sum(hour_summary$N), big.mark = ",", scientific = FALSE)),
      bar_width = 0.8,
      rows_to_print = 24,
      add_mean  = TRUE,
      add_3sd = TRUE,
      show_labels = FALSE,
      x_axis_angle  = 0,
      y_axis_labels = scales::comma,
      chart_width = 13,
      chart_height = 8.5,
      chart_dir = chart_dir,
      filename  = sprintf("%s_hour_distribution_working_order.pdf", 
                          tolower(label))
    )
  }
  
  # ===============================
  # Analysis for EXACTLY 00:00:00 (Midnight Moment)
  # ===============================
  
  exact_midnight_moment <- DT[hour == 0 & minute == 0 & second == 0]
  
  if (nrow(exact_midnight_moment) > 0) {
    
    total_n_exact <- nrow(exact_midnight_moment)
    
    # ----------------------------------------------------
    # BARCHART Section (Requires > 1 unique year for mean/sd)
    # ----------------------------------------------------
    cy_summary <- exact_midnight_moment[, .N, by = year_local]
    
    if (nrow(cy_summary) > 1) { 
      
      # Assign the factorized year_local to a new column called 'CY' for plotting
      cy_summary[, CY := factor(year_local)]  
      
      # Determine the total count for the plot subtitle
      total_n_cy <- sum(cy_summary$N)
      
      # 2. Call the plot_barchart function
      plot_barchart(
        DT        = cy_summary,
        x_col     = "CY", # QUOTED column name
        y_col     = "N",  
        title     = sprintf("%s at Exactly Midnight (00:00:00) by Year ",
                            label),
        subtitle  = sprintf("n = %s | Total records exactly at midnight", 
                            format(total_n_cy, big.mark = ",")),
        bar_width    = 0.8,
        add_mean     = TRUE,
        add_3sd      = TRUE,
        show_labels  = TRUE,
        x_axis_angle = 0,
        y_axis_labels = scales::comma,
        chart_width  = 10,
        chart_height = 6,
        chart_dir = chart_dir,
        filename  = sprintf("%s_exact_midnight_cy_distribution.pdf", tolower(label))
      )
    }
    
    # ----------------------------------------------------
    # PARETO Section (Requires > 2 unique agencies)
    # ----------------------------------------------------
    # Calculate unique agencies using the column name stored in the 'agency' variable (a string)
    n_unique_agencies <- uniqueN(exact_midnight_moment[[agency]])
    
    if (n_unique_agencies > 2) { 
      plot_pareto_combo(
        DT        = exact_midnight_moment,
        x_col     = agency, # 'agency' is the string name of the column
        chart_dir = chart_dir,
        filename  = sprintf("pareto_%s_exact_midnight_agency.pdf", tolower(label)),
        title     = sprintf("Agencies with %s at Exactly Midnight (00:00:00)", label),
        subtitle  = sprintf("n = %s | %s unique agencies", 
                            format(total_n_exact, big.mark = ","), n_unique_agencies),
        top_n     = 30,
        include_na = FALSE
      )
    } 
  }
  
  # ===============================
  # Analysis for EXACTLY NOON (12:00:00)
  # ===============================
  
  # 1. Filter for the exact noon moment
  exact_noon_moment <- DT[hour == 12 & minute == 0 & second == 0]
  
  if (nrow(exact_noon_moment) > 0) {
    
    total_n_noon <- nrow(exact_noon_moment)
    
    # ----------------------------------------------------
    # A. BARCHART Section (Requires > 1 unique year for mean/sd)
    # ----------------------------------------------------
    cy_summary_noon <- exact_noon_moment[, .N, by = year_local]
    
    if (nrow(cy_summary_noon) > 1) {
      
      cy_summary_noon[, CY := factor(year_local)] # Ensure CY is treated as a factor
      
      plot_barchart(
        DT        = cy_summary_noon,
        x_col     = "CY",
        y_col     = "N",
        title     = sprintf(" %s at Exactly Noon (12:00:00) by Year ",
                            label),
        subtitle  = sprintf("n = %s | Total records exactly at noon", 
                            format(total_n_noon, big.mark = ",")),
        bar_width    = 0.8,
        add_3sd      = TRUE,
        add_mean    = TRUE,
        add_median = FALSE,
        show_labels  = TRUE,
        x_axis_angle = 0,
        y_axis_labels = scales::comma,
        chart_width  = 10,
        chart_height = 6,
        chart_dir = chart_dir,
        filename  = sprintf("%s_exact_noon_cy_distribution.pdf", tolower(label))
      )
    }
    
    # ----------------------------------------------------
    # B. PARETO Section (Requires > 2 unique agencies)
    # ----------------------------------------------------
    # Calculate unique agencies using the column name stored in the 'agency' variable (a string)
    n_unique_agencies <- uniqueN(exact_noon_moment[[agency]])
    
    if (n_unique_agencies > 2) { 
      plot_pareto_combo(
        DT        = exact_noon_moment,
        x_col     = agency,
        chart_dir = chart_dir,
        filename  = sprintf("pareto_%s_exact_noon_agency.pdf", tolower(label)),
        title     = sprintf("Agencies with %s at  Exactly Noon (12:00:00)", label),
        subtitle  = sprintf("n = %s | %s unique agencies", 
                            format(total_n_noon, big.mark = ","), n_unique_agencies),
        top_n     = 30,
        include_na = FALSE
      )
    }
  }
  
  # ===============================
  # Top-of-the-hour analysis (minute = 00 and second = 00)
  # ===============================
  total_records_top_hour <- nrow(DT[!is.na(minute) & !is.na(second)])
  top_hour_records <- nrow(DT[minute == 0 & second == 0])
  
  # BARCHART CHECK: Only plot if top_hour_records > 0 and hourly_top_hour has > 1 row
  if (top_hour_records > 0) {
    
    top_hour_pct <- round(100 * top_hour_records / total_records_top_hour, 3)
    expected_pct <- round(100 / (60 * 60), 3)
    ratio_to_expected <- round(top_hour_pct / expected_pct, 1)
    
    cat(sprintf("\n%s Records Exactly at Top-of-the-Hour (00:00):\n", label))
    cat(sprintf("  Total records with minute+second data: %s\n", 
                format(total_records_top_hour, big.mark = ",")))
    cat(sprintf("  Top-of-the-hour records (00:00): %s (%.3f%%)\n", 
                format(top_hour_records, big.mark = ","), top_hour_pct))
    
    hourly_top_hour <- DT[!is.na(hour) & !is.na(minute) & !is.na(second), .(
      total = .N,
      top_of_hour = sum(minute == 0 & second == 0)
    ), by = hour][order(hour)]
    hourly_top_hour[, pct_top_hour := round(100 * top_of_hour / total, 3)]
    
    cat(sprintf("\nTop-of-the-hour percentages by hour:\n"))
    hourly_display <- hourly_top_hour[, .(
      hour = sprintf("%02d", hour),
      total = format(total, big.mark = ","),
      top_of_hour = format(top_of_hour, big.mark = ","),
      pct = sprintf("%.3f%%", pct_top_hour)
    )]
    print(hourly_display, row.names = FALSE, right = FALSE)
    
    # BARCHART CHECK: Requires > 1 unique hour with top_of_hour records
    if (nrow(hourly_top_hour[top_of_hour > 0]) > 1) {
      
      # Reorder factor levels to start at 08:00 and wrap to 07:00
      hour_order <- c(8:23, 0:7)
      hourly_top_hour[, hour := factor(hour, levels = hour_order)]
      
      plot_barchart(
        DT        = hourly_top_hour,
        x_col     = "hour",
        y_col     = "top_of_hour",
        title      = "",
        subtitle  = sprintf("n = %s | %.2f%% occur exactly at top-of-the-hour (%.1fx expected)",
                            format(top_hour_records, big.mark = ","),
                            top_hour_pct, 
                            ratio_to_expected),
        bar_width = 0.8,
        add_median = FALSE,
        add_3sd   = TRUE,
        show_labels = FALSE,
        x_axis_angle  = 0,
        y_axis_labels = scales::comma,
        chart_width = 13,
        chart_height = 8.5,
        chart_dir = chart_dir,
        filename  = sprintf("%s_top_of_hour_distribution.pdf", tolower(label))
      )
    }
  }
  
  invisible(list(
    hour_summary          = hour_summary
  ))
}