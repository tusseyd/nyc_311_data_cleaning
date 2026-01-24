summarize_backlog <- function(DT, 
                              tz = NULL, 
                              start_year = 2020, 
                              data_dir = NULL, 
                              prior_year_file = "311_Service_Requests_for_2019.csv") {
  
  stopifnot(data.table::is.data.table(DT))
  if (is.null(tz)) tz <- (attr(DT$created_date, "tzone") %||% "America/New_York")
  
  yrs <- sort(unique(lubridate::year(DT$created_date)))
  yrs <- yrs[yrs >= start_year]
  
  res <- data.table::rbindlist(lapply(yrs, function(y) {
    start <- as.POSIXct(sprintf("%d-01-01 00:00:00", y), tz = tz)
    
    # backlog_in criteria:
    #  1. created before start of year
    #  2. closed_date not NA
    #  3. status != "CLOSED"
    backlog_in <- if (y == start_year) {
      # Hardcoded 2019 backlog value
      backlog_count <- 18013L
      cat(sprintf("Using hardcoded backlog from %d: %s\n", 
                  y - 1, 
                  format(backlog_count, big.mark = ",")))
      backlog_count
      
    } else {
      # Use main dataset for subsequent years
      DT[
        created_date < start &
          !is.na(closed_date) &
          toupper(status) != "CLOSED",
        .N
      ]
    }
    
    data.table::data.table(
      year       = y,
      backlog_in = backlog_in
    )
  }))
  
  # cumulative backlog
  res[, backlog_cum := cumsum(backlog_in)]
  
  # pretty print
  cat("\n[Backlog by year]\n")
  print(res[, .(
    year,
    backlog_in  = format(backlog_in, big.mark = ","),
    backlog_cum = format(backlog_cum, big.mark = ",")
  )], row.names = FALSE)
  
  # Create plots
  plot_barchart(
    DT = res,
    x_col = "year",
    y_col = "backlog_in",
    title = "Annual Backlog Entering Each Year",
    subtitle = sprintf("Total backlog entering across all years: %s", 
                       format(sum(res$backlog_in), big.mark = ",")),
    x_label = "Year",
    y_label = "Backlog In",
    show_labels = TRUE,
    add_trendline = TRUE,
    show_trend_stats = TRUE,
    chart_dir = chart_dir,
    filename = "annual_backlog_bar_chart",
    console_print_title = "Annual Backlog Distribution"
  )
  
  invisible(res)
}