analyze_closed_created_patterns <- function(
    DT,
    chart_dir,
    created_col = "created_date",
    closed_col  = "closed_date",
    key_col     = "unique_key"
) {
  stopifnot(data.table::is.data.table(DT))
  
  # -----------------------------------
  # CLOSED: Hour of day distribution
  # -----------------------------------
  DT[, closed_hour := as.integer(format(get(closed_col), "%H"))]
  hour_summary <- DT[!is.na(closed_hour), .N, by = closed_hour][order(closed_hour)]
  hour_summary[, pct := round(100 * N / sum(N), 2)]
  hour_summary[, label := sprintf("%02d (%.1f%%)", closed_hour, pct)]
  
  cat("\nService Requests Closed by Hour of Day:\n")
  print(hour_summary[, .(
    hour = sprintf("%02d", closed_hour),
    count = format(N, big.mark = ","),
    pct = sprintf("%.2f%%", pct)
  )], row.names = FALSE, right = FALSE)
  
  create_basic_bar_chart(
    DT        = hour_summary,
    x_col     = "closed_hour",
    y_col     = "N",
    title     = "Service Requests Closed by Hour of Day (24-hr clock)",
    subtitle  = sprintf("n = %s", format(sum(hour_summary$N), big.mark = ",", scientific = FALSE)),
    mean_line = TRUE,
    chart_dir = chart_dir,
    filename  = "closed_hour_distribution.pdf"
  )
  
  # -----------------------------------
  # CLOSED: Midnight minute breakdown
  # -----------------------------------
  DT[, closed_minute := as.integer(format(get(closed_col), "%M"))]
  midnight_summary <- DT[closed_hour == 0, .N, by = closed_minute][order(closed_minute)]
  midnight_cases <- DT[closed_hour == 0]
  
  plot_pareto_combo(
    DT        = midnight_cases,
    x_col     = agency,
    chart_dir = chart_dir,
    filename  = "pareto_midnight_closure_agency.pdf",
    title     = "Agencies with Midnight Closures (00:00–00:59)",
    top_n     = 30,
    include_na = FALSE
  )
  
  midnight_summary[, pct := round(100 * N / sum(N), 2)]
  cat("\nService Requests Closed During Midnight Hour (00:00–00:59):\n")
  row00 <- midnight_summary[closed_minute == 0]
  others <- midnight_summary[closed_minute != 0][sample(.N, min(.N, 10))]
  to_print <- rbind(row00, others)[order(closed_minute)]
  print(to_print[, .(
    minute = sprintf("%02d", closed_minute),
    count  = format(N, big.mark = ","),
    pct    = sprintf("%.2f%%", pct)
  )], row.names = FALSE, right = FALSE)
  
  create_basic_bar_chart(
    DT        = midnight_summary,
    x_col     = "closed_minute",
    y_col     = "N",
    mean_line = FALSE,
    title     = "Service Requests Closed by Minute (Midnight Hour: 00:00–00:59)",
    subtitle  = sprintf("n = %s | Expect spike at 00:00",
                        format(sum(midnight_summary$N), big.mark = ",", scientific = FALSE)),
    chart_dir  = chart_dir,
    filename   = "closed_midnight_minute_distribution.pdf"
  )
  
  # -----------------------------------
  # CLOSED: First minute (seconds)
  # -----------------------------------
  DT[, closed_second := as.integer(format(get(closed_col), "%S"))]
  first_minute_summary <- DT[closed_hour == 0 & closed_minute == 0, .N, by = closed_second][order(closed_second)]
  first_minute_cases   <- DT[closed_hour == 0 & closed_minute == 0]
  
  plot_pareto_combo(
    DT        = first_minute_cases,
    x_col     = agency,
    chart_dir = chart_dir,
    filename  = "pareto_closed_first_minute_agency.pdf",
    title     = "Agencies with First-Minute Closures",
    top_n     = 30,
    include_na = FALSE
  )
  
  first_minute_summary[, pct := round(100 * N / sum(N), 2)]
  midnight_total <- DT[closed_hour == 0, .N]
  first_minute_total <- sum(first_minute_summary$N)
  first_minute_pct <- round(100 * first_minute_total / midnight_total, 2)
  cat(sprintf("\nClosures in first minute: %s (%.2f%% of midnight-hour closures)\n",
              format(first_minute_total, big.mark = ","), first_minute_pct))
  
  create_basic_bar_chart(
    DT         = first_minute_summary,
    x_col      = "closed_second",
    y_col      = "N",
    mean_line  = FALSE,
    title      = "Service Requests Closed by Second (00:00:00–00:00:59)",
    subtitle   = sprintf("n = %s | Expect spike at exactly 00:00:00",
                         format(sum(first_minute_summary$N), big.mark = ",", scientific = FALSE)),
    chart_dir  = chart_dir,
    filename   = "closed_first_minute_second_distribution.pdf"
  )
  
  zero_second_cases <- DT[closed_hour == 0 & closed_minute == 0 & closed_second == 0]
  plot_pareto_combo(
    DT        = zero_second_cases,
    x_col     = agency,
    chart_dir = chart_dir,
    filename  = "pareto_closed_zero_second_agency.pdf",
    title     = "Agencies with Closures at Exactly 00:00:00",
    top_n     = 30,
    include_na = FALSE
  )
  zero_second_count <- nrow(zero_second_cases)
  zero_second_pct <- round(100 * zero_second_count / first_minute_total, 2)
  cat(sprintf("\nClosures in first second of first minute: exactly 00:00:00: %s (%.2f%% of first-minute closures)\n",
              format(zero_second_count, big.mark = ","), zero_second_pct))
  
  # -----------------------------------
  # CREATED: Hour of day distribution
  # -----------------------------------
  DT[, created_hour := as.integer(format(get(created_col), "%H"))]
  created_hour_summary <- DT[!is.na(created_hour), .N, by = created_hour][order(created_hour)]
  created_hour_summary[, pct := round(100 * N / sum(N), 2)]
  
  cat("\nService Requests Created by Hour of Day:\n")
  print(created_hour_summary[, .(
    hour  = sprintf("%02d", created_hour),
    count = format(N, big.mark = ","),
    pct   = sprintf("%.2f%%", pct)
  )], row.names = FALSE, right = FALSE)
  
  create_basic_bar_chart(
    DT         = created_hour_summary,
    x_col      = "created_hour",
    y_col      = "N",
    mean_line  = TRUE,
    title      = "Service Requests Created by Hour of Day (24-hr clock)",
    subtitle   = sprintf("n = %s", format(sum(created_hour_summary$N), big.mark = ",", scientific = FALSE)),
    chart_dir  = chart_dir,
    filename   = "created_hour_distribution.pdf"
  )
  
  invisible(list(
    closed_hour_summary   = hour_summary,
    midnight_summary      = midnight_summary,
    first_minute_summary  = first_minute_summary,
    created_hour_summary  = created_hour_summary
  ))
}
