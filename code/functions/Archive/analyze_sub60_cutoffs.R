analyze_sub60_cutoffs <- function(
    DT,                         # data.table with duration_sec column
    max_seconds = 59,           # analyze up to this many seconds
    normal_range_start = 23,    # where to start calculating "normal" statistics
    exclude_seconds = c(1),     # specific seconds to exclude from analysis (default: exclude 1-sec outlier)
    print_summary = TRUE,
    create_plot = TRUE,         # whether to create visualization
    chart_dir = NULL            # directory for saving plot (required if create_plot = TRUE)
) {
  if (!data.table::is.data.table(DT)) stop("DT must be a data.table.")
  if (!"duration_sec" %in% names(DT)) stop("DT must have a 'duration_sec' column.")
  
  # Get counts by second for durations 1-max_seconds
  df <- DT[!is.na(duration_sec) & duration_sec >= 1 & duration_sec <= max_seconds]
  if (!nrow(df)) {
    message("No data found in 1-59 second range")
    return(invisible(NULL))
  }
  
  # Create histogram data
  df[, duration_binned := floor(duration_sec)]
  hist_data <- df[, .(count = .N), by = duration_binned][order(duration_binned)]
  
  # Fill in missing seconds with 0 counts
  all_seconds <- data.table(duration_binned = 1:max_seconds)
  hist_data <- hist_data[all_seconds, on = "duration_binned"]
  hist_data[is.na(count), count := 0]
  
  if (print_summary) {
    cat("\n=== SUB-60 SECOND CUTOFF ANALYSIS ===\n")
    cat(sprintf("Analyzing durations 1-%d seconds\n", max_seconds))
    if (length(exclude_seconds) > 0) {
      cat(sprintf("Excluding from statistics: %s seconds\n", paste(exclude_seconds, collapse = ", ")))
    }
    cat(sprintf("Total observations: %s\n", format(sum(hist_data$count), big.mark = ",")))
  }
  
  # Create analysis dataset (excluding specified outliers from statistical calculations)
  analysis_data <- hist_data[!duration_binned %in% exclude_seconds]
  
  # Method 1: IQR Method (1.5 factor) - using analysis_data (excluding outliers)
  normal_data <- analysis_data[duration_binned >= normal_range_start]
  if (nrow(normal_data) >= 4) {  # need at least 4 points for quartiles
    q1 <- quantile(normal_data$count, 0.25)
    q3 <- quantile(normal_data$count, 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    
    # Find first second where count is >= lower_bound (EXCLUDING outliers from search)
    search_data <- hist_data[!duration_binned %in% exclude_seconds]
    iqr_cutoff <- search_data[count >= lower_bound, min(duration_binned)]
    if (is.infinite(iqr_cutoff)) iqr_cutoff <- max_seconds
  } else {
    iqr_cutoff <- NA
    lower_bound <- NA
  }
  
  # Method 2: Standard Deviation Method (2 SD below mean) - using analysis_data
  if (nrow(normal_data) >= 2) {
    mean_normal <- mean(normal_data$count)
    sd_normal <- sd(normal_data$count)
    sd_threshold <- mean_normal - 2 * sd_normal
    
    # EXCLUDING outliers from search
    search_data <- hist_data[!duration_binned %in% exclude_seconds]
    sd_cutoff <- search_data[count >= sd_threshold, min(duration_binned)]
    if (is.infinite(sd_cutoff)) sd_cutoff <- max_seconds
  } else {
    sd_cutoff <- NA
    sd_threshold <- NA
    mean_normal <- NA
    sd_normal <- NA
  }
  
  # Method 3: Percentage of Normal Method - using analysis_data
  if (nrow(normal_data) >= 1) {
    median_normal <- median(normal_data$count)
    pct50_threshold <- median_normal * 0.5  # 50% of normal
    pct25_threshold <- median_normal * 0.25 # 25% of normal
    
    # EXCLUDING outliers from search
    search_data <- hist_data[!duration_binned %in% exclude_seconds]
    pct50_cutoff <- search_data[count >= pct50_threshold, min(duration_binned)]
    pct25_cutoff <- search_data[count >= pct25_threshold, min(duration_binned)]
    
    if (is.infinite(pct50_cutoff)) pct50_cutoff <- max_seconds
    if (is.infinite(pct25_cutoff)) pct25_cutoff <- max_seconds
  } else {
    pct50_cutoff <- NA
    pct25_cutoff <- NA
    median_normal <- NA
  }
  
  # Method 4: Visual Inspection Points
  visual_cutoffs <- c(10, 15, 20, 30)
  
  # Calculate statistics for each potential cutoff
  cutoff_results <- data.table(
    method = c("IQR_1.5", "StdDev_2SD", "50%_Normal", "25%_Normal", 
               paste0("Visual_", visual_cutoffs)),
    cutoff_seconds = c(iqr_cutoff, sd_cutoff, pct50_cutoff, pct25_cutoff, visual_cutoffs)
  )
  
  # Calculate impact for each cutoff
  cutoff_results[, `:=`(
    suspicious_count = sapply(cutoff_seconds, function(x) {
      if (is.na(x)) return(NA)
      sum(hist_data[duration_binned < x, count])
    }),
    suspicious_pct = sapply(cutoff_seconds, function(x) {
      if (is.na(x)) return(NA)
      round(100 * sum(hist_data[duration_binned < x, count]) / sum(hist_data$count), 2)
    }),
    retained_count = sapply(cutoff_seconds, function(x) {
      if (is.na(x)) return(NA)
      sum(hist_data[duration_binned >= x, count])
    })
  )]
  
  if (print_summary) {
    cat(sprintf("\nNormal range statistics (seconds %d-%d, excluding %s):\n", 
                normal_range_start, max_seconds, paste(exclude_seconds, collapse = ", ")))
    if (!is.na(mean_normal)) {
      cat(sprintf("  Mean: %.1f\n", mean_normal))
      cat(sprintf("  Median: %.1f\n", median_normal))
      cat(sprintf("  Std Dev: %.1f\n", sd_normal))
      cat(sprintf("  Q1: %.1f, Q3: %.1f, IQR: %.1f\n", q1, q3, iqr))
      cat(sprintf("  IQR Lower Bound: %.1f\n", lower_bound))
      
      # NEW: Detailed IQR calculation explanation
      cat("\n=== IQR CALCULATION DETAILS ===\n")
      cat(sprintf("Normal range assumption: Durations of %d-%d seconds represent legitimate processing times\n", 
                  normal_range_start, max_seconds))
      cat(sprintf("Data used for IQR: Count values for each second in the %d-%d second range\n", 
                  normal_range_start, max_seconds))
      cat(sprintf("Sample of normal counts: %s\n", 
                  paste(head(normal_data$count, 8), collapse = ", ")))
      cat(sprintf("IQR calculation: Q3 (%.1f) - Q1 (%.1f) = %.1f\n", q3, q1, iqr))
      cat(sprintf("Lower bound formula: Q1 - 1.5×IQR = %.1f - 1.5×%.1f = %.1f\n", q1, iqr, lower_bound))
      cat(sprintf("Interpretation: Any second with count < %.1f is considered suspicious\n", lower_bound))
      cat(sprintf("CUTOFF RESULT: %d seconds is the minimum legitimate duration\n", iqr_cutoff))
      cat(sprintf("MEANING: Durations < %d seconds are flagged as likely data quality issues\n", iqr_cutoff))
    }
    
    cat("\n=== CUTOFF RECOMMENDATIONS ===\n")
    print(cutoff_results, row.names = FALSE)
    
    cat("\n=== SPECIFIC BREAKDOWNS ===\n")
    
    # Show impact of 1-second outlier
    one_sec_count <- hist_data[duration_binned == 1, count]
    one_sec_pct <- round(100 * one_sec_count / sum(hist_data$count), 2)
    cat(sprintf("1-second durations: %s (%s%% of total)\n", 
                format(one_sec_count, big.mark = ","), one_sec_pct))
    
    # Show first 10 seconds
    cat("\nAll seconds breakdown (1-59):\n")
    all_breakdown <- hist_data[, .(duration_binned, count, 
                                   pct = round(100 * count / sum(hist_data$count), 2))]
    print(all_breakdown, row.names = FALSE)
    
    # NEW: Duration-based outlier analysis (treating durations as the data points)
    cat("\n=== DURATION-BASED OUTLIER ANALYSIS ===\n")
    cat("Alternative approach: Treating individual duration values as data points for outlier detection\n")
    
    # Create expanded dataset where each duration appears count times
    expanded_durations <- rep(hist_data$duration_binned, hist_data$count)
    
    # Calculate statistics on the duration values themselves
    dur_mean <- mean(expanded_durations)
    dur_median <- median(expanded_durations)
    dur_sd <- sd(expanded_durations)
    dur_q1 <- quantile(expanded_durations, 0.25)
    dur_q3 <- quantile(expanded_durations, 0.75)
    dur_iqr <- dur_q3 - dur_q1
    
    # Calculate outlier thresholds
    dur_iqr_lower <- dur_q1 - 1.5 * dur_iqr
    dur_iqr_upper <- dur_q3 + 1.5 * dur_iqr
    dur_2sd_lower <- dur_mean - 2 * dur_sd
    dur_2sd_upper <- dur_mean + 2 * dur_sd
    dur_3sd_lower <- dur_mean - 3 * dur_sd
    dur_3sd_upper <- dur_mean + 3 * dur_sd
    
    cat(sprintf("Duration statistics (based on %s individual duration values):\n", 
                format(length(expanded_durations), big.mark = ",")))
    cat(sprintf("  Mean: %.1f seconds\n", dur_mean))
    cat(sprintf("  Median: %.1f seconds\n", dur_median))
    cat(sprintf("  Std Dev: %.1f seconds\n", dur_sd))
    cat(sprintf("  Q1: %.1f, Q3: %.1f, IQR: %.1f seconds\n", dur_q1, dur_q3, dur_iqr))
    
    cat("\nOutlier thresholds (duration-based):\n")
    cat(sprintf("  IQR 1.5: Durations < %.1f or > %.1f seconds\n", dur_iqr_lower, dur_iqr_upper))
    cat(sprintf("  2 SD: Durations < %.1f or > %.1f seconds\n", dur_2sd_lower, dur_2sd_upper))
    cat(sprintf("  3 SD: Durations < %.1f or > %.1f seconds\n", dur_3sd_lower, dur_3sd_upper))
    
    # Count outliers by each method
    outliers_iqr <- sum(expanded_durations < dur_iqr_lower | expanded_durations > dur_iqr_upper)
    outliers_2sd <- sum(expanded_durations < dur_2sd_lower | expanded_durations > dur_2sd_upper)
    outliers_3sd <- sum(expanded_durations < dur_3sd_lower | expanded_durations > dur_3sd_upper)
    
    cat(sprintf("\nOutlier counts (duration-based):\n"))
    cat(sprintf("  IQR 1.5 method: %s outliers (%.2f%%)\n", 
                format(outliers_iqr, big.mark = ","), 
                100 * outliers_iqr / length(expanded_durations)))
    cat(sprintf("  2 SD method: %s outliers (%.2f%%)\n", 
                format(outliers_2sd, big.mark = ","), 
                100 * outliers_2sd / length(expanded_durations)))
    cat(sprintf("  3 SD method: %s outliers (%.2f%%)\n", 
                format(outliers_3sd, big.mark = ","), 
                100 * outliers_3sd / length(expanded_durations)))
    
    # Suggested cutoffs
    suggested_iqr_cutoff <- ceiling(max(1, dur_iqr_lower))
    suggested_2sd_cutoff <- ceiling(max(1, dur_2sd_lower))
    suggested_3sd_cutoff <- ceiling(max(1, dur_3sd_lower))
    
    cat(sprintf("\nSuggested cutoffs (duration-based outlier detection):\n"))
    cat(sprintf("  IQR 1.5: Durations < %d seconds are outliers\n", suggested_iqr_cutoff))
    cat(sprintf("  2 SD: Durations < %d seconds are outliers\n", suggested_2sd_cutoff))
    cat(sprintf("  3 SD: Durations < %d seconds are outliers\n", suggested_3sd_cutoff))
  }
  
  # Create visualization if requested
  if (create_plot) {
    if (is.null(chart_dir)) stop("chart_dir must be provided when create_plot = TRUE")
    if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required for plotting.")
    if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Prepare data for plotting
    plot_data <- copy(hist_data)
    plot_data[, `:=`(
      is_excluded = duration_binned %in% exclude_seconds,
      is_suspicious = duration_binned < iqr_cutoff
    )]
    
    # Create bar chart with cutoff lines
    p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = duration_binned, y = count)) +
      # Color bars based on status
      ggplot2::geom_col(ggplot2::aes(fill = ifelse(is_excluded, "Excluded", 
                                                   ifelse(is_suspicious, "Suspicious", "Normal"))),
                        width = 0.8, color = "white", linewidth = 0.1) +
      
      # Add cutoff lines
      ggplot2::geom_vline(xintercept = iqr_cutoff, color = "red", linewidth = 1.2, linetype = "solid") +
      ggplot2::geom_vline(xintercept = sd_cutoff, color = "blue", linewidth = 1, linetype = "dashed") +
      
      # Labels and styling
      ggplot2::labs(
        title = "Sub-60 Second Duration Analysis with Cutoff Recommendations",
        subtitle = sprintf("n = %s | IQR 1.5 cutoff: %d sec | 2SD cutoff: %d sec", 
                           format(sum(hist_data$count), big.mark = ","), iqr_cutoff, sd_cutoff),
        x = "Duration (seconds)",
        y = "Count",
        fill = "Classification"
      ) +
      ggplot2::scale_fill_manual(
        values = c("Excluded" = "gray40", "Suspicious" = "#E69F00", "Normal" = "#44AA99")
      ) +
      ggplot2::scale_x_continuous(
        breaks = seq(0, max_seconds, by = 5),
        expand = c(0.01, 0)
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        expand = c(0, 0)
      ) +
      ggplot2::theme(
        panel.grid.major.y   = ggplot2::element_line(color = "white", linewidth = 0.8),
        panel.grid.major.x   = ggplot2::element_line(color = "white", linewidth = 0.8),
        panel.grid.minor     = ggplot2::element_blank(),
        panel.background     = ggplot2::element_rect(fill = "gray97", color = "gray97"),
        plot.title           = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle        = ggplot2::element_text(hjust = 0, size = 12),
        axis.text.x          = ggplot2::element_text(size = 11),
        axis.text.y          = ggplot2::element_text(size = 11),
        axis.title.x         = ggplot2::element_text(size = 12),
        axis.title.y         = ggplot2::element_text(size = 12),
        legend.position      = "bottom",
        legend.title         = ggplot2::element_text(size = 11),
        legend.text          = ggplot2::element_text(size = 10),
        axis.ticks.length    = grid::unit(0.3, "cm")
      )
    
    # Create boxplot of the normal range counts
    if (!is.na(mean_normal)) {
      boxplot_data <- normal_data[, .(duration_sec = duration_binned, count = count)]
      
      p2 <- ggplot2::ggplot(boxplot_data, ggplot2::aes(x = "", y = count)) +
        ggplot2::geom_boxplot(fill = "#44AA99", alpha = 0.7, width = 0.5) +
        ggplot2::geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 2, color = "#0072B2") +
        ggplot2::geom_hline(yintercept = lower_bound, color = "red", linewidth = 1.2, linetype = "solid") +
        ggplot2::labs(
          title = sprintf("IQR Analysis: Count Distribution for %d-%d Second Durations", 
                          normal_range_start, max_seconds),
          subtitle = sprintf("Q1: %.1f | Q3: %.1f | IQR: %.1f | Lower Bound: %.1f", 
                             q1, q3, iqr, lower_bound),
          x = "Normal Duration Range",
          y = "Count per Second"
        ) +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::annotate("text", x = 1.3, y = lower_bound, 
                          label = sprintf("IQR Lower Bound\n(%.1f)", lower_bound),
                          color = "red", size = 3.5, hjust = 0) +
        ggplot2::theme(
          panel.grid.major.y   = ggplot2::element_line(color = "white", linewidth = 0.8),
          panel.grid.major.x   = ggplot2::element_blank(),
          panel.grid.minor     = ggplot2::element_blank(),
          panel.background     = ggplot2::element_rect(fill = "gray97", color = "gray97"),
          plot.title           = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle        = ggplot2::element_text(hjust = 0.5, size = 12),
          axis.text.x          = ggplot2::element_text(size = 11),
          axis.text.y          = ggplot2::element_text(size = 11),
          axis.title.x         = ggplot2::element_text(size = 12),
          axis.title.y         = ggplot2::element_text(size = 12),
          axis.ticks.length    = grid::unit(0.3, "cm")
        )
      
      print(p2)
      outfile2 <- file.path(chart_dir, "iqr_boxplot_analysis.pdf")
      ggplot2::ggsave(outfile2, plot = p2, width = 10, height = 8, dpi = 300)
      cat(sprintf("Saved IQR boxplot to: %s\n", outfile2))
    }
    
    # Create duration-based analysis plots if we have the data
    if (!is.na(mean_normal) && exists("expanded_durations")) {
      # Histogram of actual duration values
      dur_hist_data <- data.table(duration = expanded_durations)
      
      p3 <- ggplot2::ggplot(dur_hist_data, ggplot2::aes(x = duration)) +
        ggplot2::geom_histogram(bins = 59, fill = "#44AA99", alpha = 0.7, color = "white") +
        ggplot2::geom_vline(xintercept = dur_mean, color = "blue", linewidth = 1, linetype = "solid") +
        ggplot2::geom_vline(xintercept = dur_iqr_lower, color = "red", linewidth = 1.2, linetype = "dashed") +
        ggplot2::geom_vline(xintercept = dur_2sd_lower, color = "orange", linewidth = 1, linetype = "dotted") +
        ggplot2::labs(
          title = "Duration Distribution with Outlier Thresholds",
          subtitle = sprintf("Mean: %.1f sec | IQR Lower: %.1f sec | 2SD Lower: %.1f sec", 
                             dur_mean, dur_iqr_lower, dur_2sd_lower),
          x = "Duration (seconds)",
          y = "Frequency"
        ) +
        ggplot2::scale_x_continuous(breaks = seq(0, max_seconds, by = 5)) +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::theme(
          panel.grid.major.y   = ggplot2::element_line(color = "white", linewidth = 0.8),
          panel.grid.major.x   = ggplot2::element_line(color = "white", linewidth = 0.8),
          panel.grid.minor     = ggplot2::element_blank(),
          panel.background     = ggplot2::element_rect(fill = "gray97", color = "gray97"),
          plot.title           = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle        = ggplot2::element_text(hjust = 0.5, size = 12),
          axis.text.x          = ggplot2::element_text(size = 11),
          axis.text.y          = ggplot2::element_text(size = 11),
          axis.title.x         = ggplot2::element_text(size = 12),
          axis.title.y         = ggplot2::element_text(size = 12),
          axis.ticks.length    = grid::unit(0.3, "cm")
        )
      
      # Duration boxplot
      p4 <- ggplot2::ggplot(dur_hist_data, ggplot2::aes(x = "", y = duration)) +
        ggplot2::geom_boxplot(fill = "#44AA99", alpha = 0.7, width = 0.5, outlier.color = "red", outlier.size = 1) +
        ggplot2::geom_hline(yintercept = dur_iqr_lower, color = "red", linewidth = 1.2, linetype = "dashed") +
        ggplot2::geom_hline(yintercept = dur_2sd_lower, color = "orange", linewidth = 1, linetype = "dotted") +
        ggplot2::labs(
          title = "Duration Boxplot with Outlier Thresholds",
          subtitle = sprintf("IQR Lower: %.1f sec | 2SD Lower: %.1f sec", dur_iqr_lower, dur_2sd_lower),
          x = "All Duration Values",
          y = "Duration (seconds)"
        ) +
        ggplot2::scale_y_continuous(breaks = seq(0, max_seconds, by = 5)) +
        ggplot2::annotate("text", x = 1.3, y = dur_iqr_lower, 
                          label = sprintf("IQR Lower\n(%.1f)", dur_iqr_lower),
                          color = "red", size = 3.5, hjust = 0) +
        ggplot2::annotate("text", x = 1.3, y = dur_2sd_lower, 
                          label = sprintf("2SD Lower\n(%.1f)", dur_2sd_lower),
                          color = "orange", size = 3.5, hjust = 0) +
        ggplot2::theme(
          panel.grid.major.y   = ggplot2::element_line(color = "white", linewidth = 0.8),
          panel.grid.major.x   = ggplot2::element_blank(),
          panel.grid.minor     = ggplot2::element_blank(),
          panel.background     = ggplot2::element_rect(fill = "gray97", color = "gray97"),
          plot.title           = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle        = ggplot2::element_text(hjust = 0.5, size = 12),
          axis.text.x          = ggplot2::element_text(size = 11),
          axis.text.y          = ggplot2::element_text(size = 11),
          axis.title.x         = ggplot2::element_text(size = 12),
          axis.title.y         = ggplot2::element_text(size = 12),
          axis.ticks.length    = grid::unit(0.3, "cm")
        )
      
      print(p3)
      outfile3 <- file.path(chart_dir, "duration_histogram_outliers.pdf")
      ggplot2::ggsave(outfile3, plot = p3, width = 13, height = 8, dpi = 300)
      cat(sprintf("Saved duration histogram to: %s\n", outfile3))
      
      print(p4)
      outfile4 <- file.path(chart_dir, "duration_boxplot_outliers.pdf")
      ggplot2::ggsave(outfile4, plot = p4, width = 10, height = 8, dpi = 300)
      cat(sprintf("Saved duration boxplot to: %s\n", outfile4))
    }
    
    print(p1)
    outfile <- file.path(chart_dir, "sub60_cutoff_analysis.pdf")
    ggplot2::ggsave(outfile, plot = p1, width = 13, height = 8.5, dpi = 300)
    cat(sprintf("Saved cutoff analysis plot to: %s\n", outfile))
    
  } else {
    p1 <- NULL
    p2 <- NULL
  }
  
  invisible(list(
    histogram_data = hist_data,
    cutoff_analysis = cutoff_results,
    normal_stats = if (!is.na(mean_normal)) {
      list(mean = mean_normal, median = median_normal, sd = sd_normal,
           q1 = q1, q3 = q3, iqr = iqr, lower_bound = lower_bound)
    } else NULL,
    plots = if (create_plot) list(
      histogram = p1, 
      count_boxplot = if (exists("p2")) p2 else NULL,
      duration_histogram = if (exists("p3")) p3 else NULL,
      duration_boxplot = if (exists("p4")) p4 else NULL
    ) else NULL
  ))
}