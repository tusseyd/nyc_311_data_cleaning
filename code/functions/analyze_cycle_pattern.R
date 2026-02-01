# ============================================================================
# FUNCTION: Analyze Cycle Pattern by Agency
# ============================================================================
# Purpose: Analyzes timestamp cycle patterns (3-min, 5-min, 15-min) and 
#          identifies which agencies are responsible for the elevated frequency
#          Uses pre-computed minute distribution results to avoid duplication
# 
# Parameters:
#   data: data.table with date/time column and agency column
#   minute_results: results object from analyze_second_minute_distribution()
#   date_col: name of the date/time column to analyze (default: "created_date")
#   agency_col: name of the agency column (default: "agency")
#   cycle_type: type of cycle to detect ("3min", "5min", "15min")
#   elevated_minutes: custom vector of elevated minutes (overrides cycle_type)
#   output_prefix: prefix for output files (default: uses date_col name)
#   create_chart: whether to create Pareto chart (default: TRUE)
#
# Returns: List containing:
#   - agency_analysis: full agency-level analysis
#   - pattern_summary: summary of cycle pattern
#   - agencies_with_excess: agencies showing excess
#   - agencies_without_excess: agencies at or below expected
#   - minute_data: minute distribution with elevated flags
#   - elevated_minutes: vector of elevated minutes
#   - data_exact_cycle: rows exactly at cycle timestamps (HH:Mx:00)
#   - exact_cycle_summary: Pareto summary of exact cycle timestamps by agency
# ============================================================================

analyze_cycle_pattern <- function(data, 
                                  minute_results,
                                  date_col = "created_date",
                                  agency_col = "agency",
                                  cycle_type = "3min",
                                  elevated_minutes = NULL,
                                  output_prefix = NULL,
                                  create_chart = TRUE) {
  
  # Validate inputs
  if (!date_col %in% names(data)) {
    stop(paste("Column", date_col, "not found in data"))
  }
  if (!agency_col %in% names(data)) {
    stop(paste("Column", agency_col, "not found in data"))
  }
  if (missing(minute_results) || is.null(minute_results$minute_summary)) {
    stop("minute_results must be provided and contain $minute_summary component")
  }
  
  # Set output prefix
  if (is.null(output_prefix)) {
    output_prefix <- tolower(gsub("_", "", date_col))
  }
  
  # Create readable column name for output
  col_display <- toupper(gsub("_", " ", date_col))
  
  # Define elevated minutes based on cycle type
  if (!is.null(elevated_minutes)) {
    cycle_desc <- "Custom Pattern"
    cycle_short <- "custom"
    pattern_explanation <- paste0("Custom elevated minutes: ", 
                                  paste(sprintf("%02d", elevated_minutes), collapse = ", "))
  } else if (cycle_type == "3min") {
    elevated_minutes <- c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 
                          41, 44, 47, 50, 53, 56, 59)
    cycle_desc <- "3-Minute Cycle Pattern"
    cycle_short <- "3min"
    pattern_explanation <- "Every 3rd minute shows elevated frequency (minute %% 3 == 2)"
  } else if (cycle_type == "5min") {
    elevated_minutes <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)
    cycle_desc <- "5-Minute Interval Pattern"
    cycle_short <- "5min"
    pattern_explanation <- "Minutes at 5-minute intervals show elevated frequency (:05, :10, :15, :20, :25, :30, :35, :40, :45, :50, :55)"
  } else if (cycle_type == "15min") {
    elevated_minutes <- c(15, 30, 45)
    cycle_desc <- "15-Minute Interval Pattern (Quarter Hours)"
    cycle_short <- "15min"
    pattern_explanation <- "Quarter-hour minutes show elevated frequency (:15, :30, :45)"
  } else {
    stop("cycle_type must be '3min', '5min', '15min', or provide custom elevated_minutes")
  }
  
  # ============================================================================
  # PART 1: CYCLE PATTERN ANALYSIS (using pre-computed minute data)
  # ============================================================================
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat(toupper(cycle_desc), ": CYCLE PATTERN ANALYSIS\n", sep = "")
  cat("Date Field: ", col_display, "\n", sep = "")
  cat(rep("=", 80), "\n", sep = "")
  
  # Extract minute data (excluding TOTAL row) - already computed!
  minute_data <- copy(minute_results$minute_summary[!is.na(minute_value)])
  minute_data[, minute_label := sprintf("%02d", minute_value)]
  
  # Add elevated flag
  minute_data[, is_elevated := minute_value %in% elevated_minutes]
  
  # Calculate statistics by elevated vs non-elevated
  pattern_summary <- minute_data[, .(
    count_minutes = .N,
    mean_pct = round(mean(pct), 3),
    min_pct = round(min(pct), 2),
    max_pct = round(max(pct), 2),
    total_count = sum(count)
  ), by = is_elevated][order(is_elevated)]
  
  cat("\n", cycle_desc, " SUMMARY:\n", sep = "")
  print(pattern_summary)

  # Show which minutes are elevated
  cat("\nELEVATED MINUTES:\n")
  cat(paste(sprintf("%02d", elevated_minutes), collapse = ", "), "\n")
  
  cat("\nNORMAL MINUTES (non-elevated):\n")
  normal_minutes <- minute_data[is_elevated == FALSE, minute_value]
  cat(paste(sprintf("%02d", head(normal_minutes, 20)), collapse = ", "), "...\n")
  
  # Calculate the pattern
  cat("\n", rep("-", 80), "\n", sep = "")
  cat("PATTERN INTERPRETATION:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  elevated_pct <- pattern_summary[is_elevated == TRUE, mean_pct]
  normal_pct <- pattern_summary[is_elevated == FALSE, mean_pct]
  ratio <- elevated_pct / normal_pct
  
  cat("Elevated minutes: ", elevated_pct, "% average\n", sep = "")
  cat("Normal minutes: ", normal_pct, "% average\n", sep = "")
  cat("Ratio: ", round(ratio, 3), "x\n", sep = "")
  
  expected_pct <- length(elevated_minutes) / 59 * 100
  excess_records <- round(pattern_summary[is_elevated == TRUE, total_count] - 
                            (sum(minute_data$count) * (length(elevated_minutes)/59)))
  
  cat("\nExpected percentage in elevated minutes: ", round(expected_pct, 2), "%\n", sep = "")
  cat("Actual percentage in elevated minutes: ", 
      round(pattern_summary[is_elevated == TRUE, total_count] / sum(minute_data$count) * 100, 2), "%\n", sep = "")
  cat("This represents approximately ", 
      format(excess_records, big.mark = ","), 
      " excess records\n", sep = "")
  
  # ============================================================================
  # VISUALIZATION: MINUTE DISTRIBUTION WITH PATTERN HIGHLIGHTED
  # ============================================================================
  
  # minute_data[, pattern_label := factor(is_elevated, 
  #                                       levels = c(FALSE, TRUE),
  #                                       labels = c("Normal Minutes", 
  #                                                  "Elevated Minutes"))]
  # 
  # p_minute <- ggplot(minute_data, aes(x = minute_label, y = pct, fill = pattern_label)) +
  #   geom_bar(stat = "identity", width = 0.8) +
  #   scale_fill_manual(values = c(
  #     "Normal Minutes" = "#0072B2",
  #     "Elevated Minutes" = "#D55E00"
  #   )) +
  #   geom_hline(yintercept = 1.69, color = "firebrick4", linetype = "dashed", linewidth = 1) +
  #   annotate("text", x = 55, y = 1.69 + 0.03, 
  #            label = "Expected: 1.69% (uniform)", 
  #            color = "firebrick4", size = 3.5, hjust = 1, vjust = -2) +
  #   labs(
  #     title = paste0(cycle_desc, ": ", col_display),
  #     subtitle = pattern_explanation,
  #     x = "Minute Value (01-59)",
  #     y = "Percentage (%)",
  #     fill = "Pattern"
  #   ) +
  #   theme_minimal(base_size = 12) +
  #   theme(
  #     axis.text.x = element_text(angle = 0, size = 7),
  #     legend.position = "top"
  #   ) +
  #   scale_x_discrete(breaks = sprintf("%02d", seq(1, 59, by = 3)))
  # 
  # print(p_minute)
  # Sys.sleep(3)
  # 
  # chart_file <- paste0("./charts/", output_prefix, "_", cycle_short, "_minute_pattern.pdf")
  # ggsave(chart_file, plot = p_minute, width = 18, height = 8.5)
  # cat("\nMinute distribution chart saved to: ", chart_file, "\n", sep = "")
  
  # ============================================================================
  # EXACT CYCLE TIMESTAMPS: ROWS AT HH:Mx:00
  # ============================================================================
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("EXACT CYCLE TIMESTAMPS ANALYSIS (HH:Mx:00 only)\n")
  cat("Date Field: ", col_display, "\n", sep = "")
  cat(rep("=", 80), "\n", sep = "")
  
  # Create temporary working copy
  data_temp <- copy(data)
  data_temp[, `:=`(
    temp_minute = minute(get(date_col)),
    temp_second = second(get(date_col))
  )]
  
  # Extract rows EXACTLY at cycle minutes with 0 seconds
  data_exact_cycle <- data_temp[temp_minute %in% elevated_minutes & temp_second == 0]
  
  cat("\nTotal rows in dataset: ", format(nrow(data_temp), big.mark = ","), "\n", sep = "")
  cat("Rows EXACTLY at cycle timestamps (e.g., HH:02:00, HH:05:00, etc.): ", 
      format(nrow(data_exact_cycle), big.mark = ","), "\n", sep = "")
  cat("Percentage: ", round(nrow(data_exact_cycle) / nrow(data_temp) * 100, 4), "%\n\n", sep = "")
  
  # Create Pareto table by agency
  exact_cycle_summary <- NULL
  if (nrow(data_exact_cycle) > 0) {
    exact_cycle_summary <- data_exact_cycle[, .N, by = get(agency_col)][order(-N)]
    setnames(exact_cycle_summary, "get", agency_col)
    
    exact_cycle_summary[, `:=`(
      pct = round(N / sum(N) * 100, 2),
      cum_pct = round(cumsum(N) / sum(N) * 100, 2)
    )]
    setnames(exact_cycle_summary, "N", "count")
    exact_cycle_summary[, count_fmt := format(count, big.mark = ",")]
    
    # Add total row
    total_row <- data.table(
      count = sum(exact_cycle_summary$count),
      pct = 100.00,
      cum_pct = 100.00,
      count_fmt = format(sum(exact_cycle_summary$count), big.mark = ",")
    )
    total_row[[agency_col]] <- "TOTAL"
    setcolorder(total_row, names(exact_cycle_summary))
    
    exact_cycle_with_total <- rbind(exact_cycle_summary, total_row)
    
    cat(rep("=", 80), "\n", sep = "")
    cat("PARETO TABLE: EXACT CYCLE TIMESTAMPS BY AGENCY\n")
    cat("Cycle Minutes: ", paste(sprintf("%02d", elevated_minutes), collapse = ", "), "\n", sep = "")
    cat("Seconds: 00 only (e.g., HH:02:00, HH:05:00, HH:08:00, etc.)\n")
    cat(rep("=", 80), "\n", sep = "")
    
    print(exact_cycle_with_total[, .SD, .SDcols = c(agency_col, "count_fmt", "pct", "cum_pct")])
    
    cat(rep("=", 80), "\n\n", sep = "")
    
    # Show top 3 agencies with sample timestamps
    cat("\nTOP 3 AGENCIES - SAMPLE EXACT CYCLE TIMESTAMPS:\n")
    cat(rep("-", 80), "\n", sep = "")
    
    top3_agencies <- exact_cycle_summary[1:min(3, nrow(exact_cycle_summary)), get(agency_col)]
    
    for (ag in top3_agencies) {
      sample_times <- data_exact_cycle[get(agency_col) == ag, head(get(date_col), 5)]
      cat(sprintf("\n%s: %s\n", ag, exact_cycle_summary[get(agency_col) == ag, count_fmt]))
      cat("  Sample timestamps: ", paste(format(sample_times, "%Y-%m-%d %H:%M:%S"), collapse = ", "), "\n", sep = "")
    }
    cat("\n")
    
  } else {
    cat("No rows found exactly at cycle timestamps with 0 seconds.\n")
  }
  
  cat(rep("=", 80), "\n\n", sep = "")
  
  # ============================================================================
  # PART 2: AGENCY-LEVEL ANALYSIS
  # ============================================================================
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat(toupper(cycle_desc), ": AGENCY-LEVEL ANALYSIS\n", sep = "")
  cat("Date Field: ", col_display, "\n", sep = "")
  cat(rep("=", 80), "\n", sep = "")
  cat("\nElevated minutes:\n")
  cat(paste(sprintf("%02d", elevated_minutes), collapse = ", "), "\n\n")
  
  # Extract all rows where date_col minute is in elevated_minutes
  data_elevated <- data_temp[temp_minute %in% elevated_minutes]
  
  cat("Total rows: ", format(nrow(data_temp), big.mark = ","), "\n", sep = "")
  cat("Rows in elevated minutes: ", format(nrow(data_elevated), big.mark = ","), "\n", sep = "")
  cat("Percentage: ", round(nrow(data_elevated) / nrow(data_temp) * 100, 2), "%\n\n", sep = "")
  
  # Create simple Pareto table of elevated minutes by agency
  agency_summary <- data_elevated[, .N, by = get(agency_col)][order(-N)]
  setnames(agency_summary, "get", agency_col)
  agency_summary[, `:=`(
    pct = round(N / sum(N) * 100, 2),
    cum_pct = round(cumsum(N) / sum(N) * 100, 2)
  )]
  setnames(agency_summary, "N", "count")
  agency_summary[, count_fmt := format(count, big.mark = ",")]
  
  # Add total row
  total_row <- data.table(
    count = sum(agency_summary$count),
    pct = 100.00,
    cum_pct = 100.00,
    count_fmt = format(sum(agency_summary$count), big.mark = ",")
  )
  total_row[[agency_col]] <- "TOTAL"
  setcolorder(total_row, names(agency_summary))
  
  agency_summary_with_total <- rbind(agency_summary, total_row)
  
  cat(rep("=", 70), "\n", sep = "")
  cat("PARETO TABLE: ELEVATED MINUTES BY AGENCY (RAW COUNTS)\n")
  cat(rep("=", 70), "\n", sep = "")
  print(agency_summary_with_total[, .SD, .SDcols = c(agency_col, "count_fmt", "pct", "cum_pct")])
  cat(rep("=", 70), "\n\n", sep = "")
  
  # ============================================================================
  # PART 3: EXCESS ANALYSIS (OBSERVED VS EXPECTED)
  # ============================================================================
  
  # Create flags on temp data
  data_temp[, is_elevated := temp_minute %in% elevated_minutes]
  
  # Count total records by agency
  agency_totals <- data_temp[, .(total_records = .N), by = get(agency_col)]
  setnames(agency_totals, "get", agency_col)
  
  # Count elevated minute records by agency
  agency_elevated_counts <- data_temp[is_elevated == TRUE, .(elevated_count = .N), by = get(agency_col)]
  setnames(agency_elevated_counts, "get", agency_col)
  
  # Merge the two
  agency_analysis <- merge(agency_totals, agency_elevated_counts, by = agency_col, all.x = TRUE)
  agency_analysis[is.na(elevated_count), elevated_count := 0]
  
  # Calculate expected elevated count
  expected_pct_calc <- length(elevated_minutes) / 60 * 100
  
  agency_analysis[, `:=`(
    expected_elevated = round(total_records * (length(elevated_minutes) / 60)),
    elevated_pct = round(elevated_count / total_records * 100, 2),
    excess = elevated_count - round(total_records * (length(elevated_minutes) / 60)),
    ratio = round((elevated_count / total_records) / (length(elevated_minutes) / 60), 3)
  )]
  
  # Sort by excess (descending)
  agency_analysis <- agency_analysis[order(-excess)]
  
  # Split into agencies with/without excess
  agencies_with_excess <- agency_analysis[excess > 0]
  agencies_without_excess <- agency_analysis[excess <= 0]
  
  # Calculate percentages for agencies with excess
  if (nrow(agencies_with_excess) > 0) {
    total_positive_excess <- sum(agencies_with_excess$excess)
    
    agency_analysis[excess > 0, pct_of_excess := round(excess / total_positive_excess * 100, 2)]
    agency_analysis[excess <= 0, pct_of_excess := 0]
    
    # Calculate cumulative percentage
    agency_analysis[excess > 0, cum_pct := round(cumsum(pct_of_excess), 2)]
    agency_analysis[excess <= 0, cum_pct := 0]
    
    # Format columns for display
    agency_analysis[, `:=`(
      total_records_fmt = format(total_records, big.mark = ","),
      elevated_count_fmt = format(elevated_count, big.mark = ","),
      expected_elevated_fmt = format(expected_elevated, big.mark = ","),
      excess_fmt = format(excess, big.mark = ",")
    )]
  }
  
  # ============================================================================
  # PRINT AGENCIES WITH EXCESS
  # ============================================================================
  
  if (nrow(agencies_with_excess) > 0) {
    cat("\n", rep("=", 80), "\n", sep = "")
    cat("AGENCIES WITH EXCESS RECORDS IN ELEVATED MINUTES\n")
    cat(rep("=", 80), "\n", sep = "")
    cat("(More records than expected in elevated minutes)\n\n")
    
    print_cols <- c(agency_col, "elevated_count_fmt", "expected_elevated_fmt", 
                    "elevated_pct", "ratio", "excess_fmt")
    print_table <- agency_analysis[excess > 0, .SD, .SDcols = print_cols]
    setnames(print_table, c(agency_col, "elevated", "expected", "actual_pct", "ratio", "excess"))
    print_table[, expected_pct := round(expected_pct_calc, 2)]
    setcolorder(print_table, c(agency_col, "elevated", "expected", "actual_pct", "expected_pct", "ratio", "excess"))
    
    print(print_table)
    
    cat("\nTotal agencies with excess: ", nrow(agencies_with_excess), "\n", sep = "")
    cat("Total excess (records above expected): ", 
        format(sum(agencies_with_excess$excess), big.mark = ","), "\n", sep = "")
  }
  
  # ============================================================================
  # PRINT AGENCIES WITHOUT EXCESS
  # ============================================================================
  
  if (nrow(agencies_without_excess) > 0) {
    cat("\n", rep("=", 80), "\n", sep = "")
    cat("AGENCIES WITHOUT EXCESS (At or Below Expected)\n")
    cat(rep("=", 80), "\n", sep = "")
    cat("(At or below expected distribution in elevated minutes)\n\n")
    
    # Format columns for display
    agencies_without_excess[, `:=`(
      elevated_count_fmt = format(elevated_count, big.mark = ","),
      expected_elevated_fmt = format(expected_elevated, big.mark = ","),
      deficit_fmt = format(abs(excess), big.mark = ",")
    )]
    
    print_cols <- c(agency_col, "elevated_count_fmt", "expected_elevated_fmt", 
                    "elevated_pct", "ratio", "deficit_fmt")
    print_table <- agencies_without_excess[, .SD, .SDcols = print_cols]
    setnames(print_table, c(agency_col, "elevated", "expected", "actual_pct", "ratio", "deficit"))
    print_table[, expected_pct := round(expected_pct_calc, 2)]
    setcolorder(print_table, c(agency_col, "elevated", "expected", "actual_pct", "expected_pct", "ratio", "deficit"))
    
    print(print_table)
    
    cat("\nTotal agencies without excess: ", nrow(agencies_without_excess), "\n", sep = "")
    total_deficit <- sum(abs(agencies_without_excess$excess))
    cat("Total deficit (records below expected): ", 
        format(total_deficit, big.mark = ","), "\n", sep = "")
  } else {
    cat("\n", rep("=", 80), "\n", sep = "")
    cat("AGENCIES WITHOUT EXCESS\n")
    cat(rep("=", 80), "\n", sep = "")
    cat("No agencies found with fewer than expected records in elevated minutes.\n")
    cat("All agencies show excess records in the pattern.\n")
    total_deficit <- 0
  }
  
  # ============================================================================
  # BALANCE CHECK
  # ============================================================================
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("BALANCE CHECK\n")
  cat(rep("=", 80), "\n", sep = "")
  
  total_excess_sum <- sum(agencies_with_excess$excess)
  total_deficit_sum <- if (nrow(agencies_without_excess) > 0) {
    sum(abs(agencies_without_excess$excess))
  } else {
    0
  }
  
  cat("Total excess (agencies above expected): ", 
      format(total_excess_sum, big.mark = ","), "\n", sep = "")
  cat("Total deficit (agencies below expected): ", 
      format(total_deficit_sum, big.mark = ","), "\n", sep = "")
  cat("Net difference: ", 
      format(total_excess_sum - total_deficit_sum, big.mark = ","), "\n", sep = "")
  
  ratio_excess_deficit <- if (total_deficit_sum > 0) {
    round(total_excess_sum / total_deficit_sum, 1)
  } else {
    Inf
  }
  
  cat("Excess:Deficit ratio: ", 
      if (is.finite(ratio_excess_deficit)) paste0(ratio_excess_deficit, ":1") else "∞:1 (no deficit)",
      "\n", sep = "")
  
  cat("\n", rep("-", 80), "\n", sep = "")
  cat("INTERPRETATION:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  if (abs(total_excess_sum - total_deficit_sum) < (total_excess_sum + total_deficit_sum) * 0.1) {
    cat("Net difference is SMALL relative to total (within 10%).\n")
    cat("CONCLUSION: Pattern is primarily a REDISTRIBUTION among agencies.\n")
    cat("            Some agencies have excess, others have deficit, they balance out.\n")
    cat("            This suggests AGENCY-SPECIFIC data entry practices or systems.\n")
  } else {
    cat("Net difference is LARGE relative to total (>10%).\n")
    cat("CONCLUSION: Pattern shows SYSTEM-WIDE BIAS.\n")
    cat("            Overall, far more records in elevated minutes than expected.\n")
    
    # Check if it's really one agency driving it
    if (nrow(agencies_with_excess) > 0) {
      top_agency_pct <- agency_analysis[excess > 0][1, pct_of_excess]
      if (top_agency_pct > 90) {
        cat("            However, ", sprintf("%.1f%%", top_agency_pct), " of excess is from ", 
            agency_analysis[excess > 0][1, get(agency_col)], ".\n", sep = "")
        cat("            This suggests an AGENCY-SPECIFIC issue rather than platform-wide.\n")
      } else {
        cat("            This suggests a PLATFORM-LEVEL issue affecting multiple agencies:\n")
        cat("            - Central batch processing at regular intervals\n")
        cat("            - System-wide automated task scheduler\n")
        cat("            - Infrastructure-level timestamp artifacts\n")
      }
    }
  }
  
  # ============================================================================
  # TOP CONTRIBUTORS
  # ============================================================================
  
  if (nrow(agencies_with_excess) > 0) {
    top_agencies <- agency_analysis[excess > 0 & cum_pct <= 80]
    
    cat("\n", rep("=", 80), "\n", sep = "")
    cat("TOP CONTRIBUTORS (80% of excess):\n")
    cat(rep("=", 80), "\n", sep = "")
    
    if (nrow(top_agencies) > 0) {
      for (i in 1:nrow(top_agencies)) {
        cat(sprintf("  %d. %s: %s excess records (%.1f%% of total excess)\n",
                    i, 
                    top_agencies[i, get(agency_col)], 
                    top_agencies$excess_fmt[i],
                    top_agencies$pct_of_excess[i]))
      }
    } else {
      top_agency <- agency_analysis[excess > 0][1]
      cat(sprintf("  1. %s: %s excess records (%.1f%% of total excess)\n",
                  top_agency[, get(agency_col)], 
                  top_agency$excess_fmt,
                  top_agency$pct_of_excess))
      cat("  Note: Single agency accounts for majority of pattern.\n")
    }
  }
  
  # ============================================================================
  # CREATE PARETO CHART
  # ============================================================================
  
  if (create_chart && nrow(agencies_with_excess) > 0) {
    cat("\n", rep("=", 80), "\n", sep = "")
    cat("CREATING PARETO CHART...\n")
    cat(rep("=", 80), "\n", sep = "")
    
    chart_data <- agency_analysis[excess > 0]
    
    p <- ggplot(chart_data, aes(x = reorder(.data[[agency_col]], -excess))) +
      geom_bar(aes(y = excess), stat = "identity", fill = "#D55E00", width = 0.7) +
      geom_line(aes(y = cum_pct * max(excess) / 100, group = 1), 
                color = "#0072B2", linewidth = 1.5) +
      geom_point(aes(y = cum_pct * max(excess) / 100), 
                 color = "#0072B2", size = 3) +
      scale_y_continuous(
        name = "Excess Records in Pattern",
        labels = scales::comma,
        sec.axis = sec_axis(~ . * 100 / max(chart_data$excess), 
                            name = "Cumulative Percentage (%)")
      ) +
      labs(
        title = paste0("Pareto Chart: ", cycle_desc, " - ", col_display),
        subtitle = paste0("Excess records in elevated minutes (", 
                          paste(sprintf("%02d", head(elevated_minutes, 5)), collapse = ", "),
                          ", ...) vs expected"),
        x = "Agency"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      ) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5)
    
    print(p)
    Sys.sleep(3)
    
    pareto_file <- paste0("./charts/", output_prefix, "_", cycle_short, "_pareto_by_agency.pdf")
    ggsave(pareto_file, plot = p, width = 14, height = 8.5)
    
    cat("Chart saved to: ", pareto_file, "\n", sep = "")
  }
  
  # ============================================================================
  # DETAILED BREAKDOWN: TOP 5 AGENCIES
  # ============================================================================
  
  if (nrow(agencies_with_excess) > 0) {
    cat("\n", rep("=", 80), "\n", sep = "")
    cat("DETAILED BREAKDOWN: TOP 5 AGENCIES\n")
    cat(rep("=", 80), "\n", sep = "")
    
    top5 <- agency_analysis[excess > 0][1:min(5, nrow(agencies_with_excess))]
    
    for (i in 1:nrow(top5)) {
      cat("\n", rep("-", 80), "\n", sep = "")
      cat(top5[i, get(agency_col)], "\n", sep = "")
      cat(rep("-", 80), "\n", sep = "")
      cat("  Total records: ", top5$total_records_fmt[i], "\n", sep = "")
      cat("  Records in elevated minutes: ", top5$elevated_count_fmt[i], 
          " (", top5$elevated_pct[i], "%)\n", sep = "")
      cat("  Expected: ", top5$expected_elevated_fmt[i], 
          " (", round(expected_pct_calc, 2), "%)\n", sep = "")
      cat("  Excess: ", top5$excess_fmt[i], "\n", sep = "")
      cat("  Ratio: ", top5$ratio[i], "x\n", sep = "")
      cat("  Contribution to total excess: ", top5$pct_of_excess[i], "%\n", sep = "")
    }
  }
  
  # ============================================================================
  # POTENTIAL CAUSES
  # ============================================================================
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("POTENTIAL CAUSES TO INVESTIGATE:\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("\n1. BATCH PROCESSING: Automated system creating records at regular intervals\n")
  cat("2. DATA IMPORT: Scheduled imports from external systems\n")
  cat("3. CLOCK SYNCHRONIZATION: System time adjustments at regular intervals\n")
  cat("4. MONITORING/POLLING: Automated data collection at fixed intervals\n")
  cat("5. TIME ROUNDING: Application logic or manual rounding to convenient times\n")
  cat("\nRecommendation: Review the systems used by the top contributing agencies\n")
  cat("to identify if they share common infrastructure or data entry processes.\n")
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("END OF ", toupper(cycle_desc), ": ", col_display, "\n", sep = "")
  cat(rep("=", 80), "\n\n", sep = "")
  
  # Return the analysis results
  invisible(list(
    agency_analysis = agency_analysis,
    pattern_summary = pattern_summary,
    agencies_with_excess = agencies_with_excess,
    agencies_without_excess = agencies_without_excess,
    minute_data = minute_data,
    elevated_minutes = elevated_minutes,
    data_exact_cycle = data_exact_cycle,
    exact_cycle_summary = exact_cycle_summary
  ))
}