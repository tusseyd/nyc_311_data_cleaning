analyze_exact_minute_durations <- function(
    DT,                         # data.table with duration_sec column
    chart_dir,
    min_duration_sec = 60,      # minimum duration to consider (exclude very short durations)
    max_duration_sec = NULL,    # maximum duration to consider (NULL = no limit)
    print_summary = TRUE,
    create_charts = TRUE
) {
  if (!data.table::is.data.table(DT)) stop("DT must be a data.table.")
  if (!"duration_sec" %in% names(DT)) stop("DT must have a 'duration_sec' column.")
  if (!"agency" %in% names(DT)) stop("DT must have an 'agency' column.")
  
  # Filter to relevant duration range
  df <- DT[!is.na(duration_sec) & duration_sec >= min_duration_sec]
  if (!is.null(max_duration_sec)) {
    df <- df[duration_sec <= max_duration_sec]
  }
  
  if (!nrow(df)) {
    message("No data found in specified duration range")
    return(invisible(NULL))
  }
  
  # Identify exact minute durations (duration_sec is divisible by 60)
  df[, is_exact_minute := (duration_sec %% 60 == 0)]
  
  # Create subsets
  exact_minute_rows <- df[is_exact_minute == TRUE]
  non_exact_rows <- df[is_exact_minute == FALSE]
  
  # Calculate summary statistics
  total_rows <- nrow(df)
  exact_count <- nrow(exact_minute_rows)
  non_exact_count <- nrow(non_exact_rows)
  exact_pct <- round(100 * exact_count / total_rows, 2)
  
  if (print_summary) {
    range_text <- sprintf("≥%d sec", min_duration_sec)
    if (!is.null(max_duration_sec)) {
      range_text <- sprintf("%d-%d sec", min_duration_sec, max_duration_sec)
    }
    
    cat(sprintf("\nExact Minute Duration Analysis (%s):\n", range_text))
    cat(sprintf("Total durations analyzed: %s\n", format(total_rows, big.mark = ",")))
    cat(sprintf("Exact minute durations: %s (%.2f%%)\n", format(exact_count, big.mark = ","), exact_pct))
    cat(sprintf("Non-exact minute durations: %s (%.2f%%)\n", format(non_exact_count, big.mark = ","), 100 - exact_pct))
    
    # Agency breakdown
    if (exact_count > 0) {
      agency_stats <- exact_minute_rows[, .(
        exact_minute_count = .N,
        total_count = df[agency == .BY$agency, .N],
        pct_exact = round(100 * .N / df[agency == .BY$agency, .N], 2)
      ), by = agency][order(-exact_minute_count)]
      
      cat("\nTop 10 agencies with exact minute durations:\n")
      top_10 <- head(agency_stats, 10)
      print(top_10, row.names = FALSE)
      
      # Find agencies with very high percentages of exact minute durations
      high_exact_pct <- agency_stats[pct_exact >= 50 & total_count >= 10][order(-pct_exact)]
      if (nrow(high_exact_pct) > 0) {
        cat(sprintf("\nAgencies with ≥50%% exact minute durations (≥10 total durations):\n"))
        print(high_exact_pct, row.names = FALSE)
      }
    }
  }
  
  # Create charts if requested
  if (create_charts && exact_count > 0) {
    cat("\nCreating charts for exact minute duration analysis...\n")
    
    # Chart 1: Pareto of agencies with exact minute durations
    plot_pareto_combo(
      DT = exact_minute_rows,
      x_col = agency,
      chart_dir = chart_dir,
      filename = "pareto_exact_minute_agencies.pdf",
      title = "Agencies with Exact Minute Durations",
      top_n = 30,
      min_count = 5
    )
    
    # Chart 2: Histogram of exact minute durations (convert to minutes for better visualization)
    exact_minute_rows[, duration_minutes := duration_sec / 60]
    
    # Use max_duration_sec parameter if provided, otherwise set reasonable default
    if (!is.null(max_duration_sec)) {
      max_min_to_show <- max_duration_sec / 60
    } else {
      max_min_to_show <- max(exact_minute_rows$duration_minutes, na.rm = TRUE)
    }
    max_min_to_show <- ceiling(max_min_to_show)  # round up to nearest minute
    
    # Create a custom histogram since the main function doesn't handle minutes
    minute_counts <- exact_minute_rows[, .(count = .N), by = .(minutes = floor(duration_minutes))][order(minutes)]
    
    # Fill in missing minutes with 0 counts
    all_minutes <- data.table(minutes = 1:max_min_to_show)
    minute_counts <- minute_counts[all_minutes, on = "minutes"]
    minute_counts[is.na(count), count := 0]
    
    # Create plot
    p <- ggplot2::ggplot(minute_counts, ggplot2::aes(x = minutes, y = count)) +
      ggplot2::geom_col(width = 0.8, fill = "#44AA99", color = "white", linewidth = 0.1) +
      ggplot2::labs(
        title = "Distribution of Exact Minute Durations",
        subtitle = sprintf("n = %s", format(sum(minute_counts$count), big.mark = ",")),
        x = "Duration (minutes)",
        y = "Count"
      ) +
      ggplot2::scale_x_continuous(
        breaks = seq(0, max_min_to_show, by = ifelse(max_min_to_show <= 60, 10, 30)),
        expand = c(0.01, 0)
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        expand = c(0, 0)
      ) +
      ggplot2::theme(
        legend.position      = "none",
        panel.grid.major.y   = ggplot2::element_line(color = "white", linewidth = 0.8),
        panel.grid.major.x   = ggplot2::element_line(color = "white", linewidth = 0.8),
        panel.grid.minor     = ggplot2::element_blank(),
        panel.background     = ggplot2::element_rect(fill = "gray97", color = "gray97"),
        plot.title           = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle        = ggplot2::element_text(hjust = 0, size = 12),
        axis.text.x          = ggplot2::element_text(size = 12),
        axis.text.y          = ggplot2::element_text(size = 12),
        axis.title.x         = ggplot2::element_text(size = 13),
        axis.title.y         = ggplot2::element_text(size = 13),
        axis.ticks.length    = grid::unit(0.3, "cm")
      )
    
    print(p)
    outfile <- file.path(chart_dir, "histogram_exact_minute_durations.pdf")
    ggplot2::ggsave(outfile, plot = p, width = 13, height = 8.5, dpi = 300)
    cat(sprintf("Saved histogram to: %s\n", outfile))
    
    Sys.sleep(2)  # Brief pause between charts
  }
  
  # Return analysis results
  invisible(list(
    summary = list(
      total_rows = total_rows,
      exact_minute_count = exact_count,
      exact_minute_pct = exact_pct,
      non_exact_count = non_exact_count
    ),
    exact_minute_data = exact_minute_rows,
    non_exact_data = non_exact_rows,
    agency_breakdown = if (exact_count > 0) {
      exact_minute_rows[, .(
        exact_minute_count = .N,
        total_count = df[agency == .BY$agency, .N],
        pct_exact = round(100 * .N / df[agency == .BY$agency, .N], 2)
      ), by = agency][order(-exact_minute_count)]
    } else NULL
  ))
}