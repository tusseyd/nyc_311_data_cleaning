# ============================================================================
# FUNCTION: Analyze Second-Level and Minute-Level Timestamp Distribution
# ============================================================================
# Purpose: Analyzes the distribution of timestamps across all 60 seconds
#          (00-59) and all 60 minutes (00-59) to identify patterns and 
#          anomalies in timestamp precision. Includes chi-square tests.
# 
# Parameters:
#   data: data.table with date/time column
#   date_col: name of the date/time column to analyze (default: "created_date")
#
# Returns: List containing:
#   - second_summary: counts and percentages for each second (00-59)
#   - chisq_seconds_all: chi-square test for all seconds 00-59
#   - summary_stats: overall distribution statistics
#   - minute_summary_full: all minutes 00-59
#   - chisq_all_minutes: chi-square test for all minutes 00-59
#   - minute_summary: minutes 01-59 only
# ============================================================================

analyze_second_minute_distribution <- function(data, date_col = "created_date") {
  
  # Validate inputs
  if (!date_col %in% names(data)) {
    stop(paste("Column", date_col, "not found in data"))
  }
  
  # Create readable column name for output
  col_display <- toupper(gsub("_", " ", date_col))
  
  # ==========================================================================
  # PART 1: SECOND DISTRIBUTION ANALYSIS (00-59)
  # ==========================================================================
  
  # Extract seconds from timestamp
  data_with_seconds <- data[, .(second_value = second(get(date_col)))]
  
  # Count occurrences of each second (0-59)
  second_counts <- data_with_seconds[, .N, by = second_value][order(second_value)]
  
  # Ensure all seconds 0-59 are represented (fill in zeros if missing)
  all_seconds <- data.table(second_value = 0:59)
  second_summary <- merge(all_seconds, second_counts, by = "second_value", all.x = TRUE)
  second_summary[is.na(N), N := 0]
  
  # Calculate percentage and cumulative percentage
  total_records <- nrow(data)
  second_summary[, `:=`(
    count = N,
    pct = round(N / total_records * 100, 2),
    cum_pct = round(cumsum(N) / total_records * 100, 2)
  )]
  
  # Add formatted count column
  second_summary[, count_formatted := format(count, big.mark = ",")]
  
  # Remove the N column (now redundant with count)
  second_summary[, N := NULL]
  
  # Reorder columns
  setcolorder(second_summary, c("second_value", "count", "count_formatted", "pct", "cum_pct"))
  
  # Add a total row
  total_row <- data.table(
    second_value = NA_integer_,
    count = sum(second_summary$count),
    count_formatted = format(sum(second_summary$count), big.mark = ","),
    pct = 100.00,
    cum_pct = 100.00
  )
  
  second_summary_with_total <- rbind(second_summary, total_row)
  
  # Print header
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("SECOND-LEVEL DISTRIBUTION: ", col_display, " Timestamp Precision Analysis\n", sep = "")
  cat(rep("=", 80), "\n", sep = "")
  cat("\nDistribution of timestamps across all 60 seconds (00-59)\n")
  cat("Total records analyzed: ", format(total_records, big.mark = ","), "\n\n", sep = "")
  
  # Print the table
  display_table <- copy(second_summary_with_total)
  display_table[, second_display := as.character(second_value)]
  display_table[is.na(second_value), second_display := "TOTAL"]
  display_table[second_display != "TOTAL", second_display := sprintf("%02d", as.integer(second_display))]
  
  print(display_table[, .(second = second_display, count = count_formatted, pct, cum_pct)])
  
  # ==========================================================================
  # Summary Statistics - Seconds
  # ==========================================================================
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("SUMMARY STATISTICS - SECONDS\n")
  cat(rep("=", 80), "\n", sep = "")
  
  # Calculate statistics (excluding total row)
  stats <- second_summary[, .(
    mean_count = mean(count),
    median_count = median(count),
    sd_count = sd(count),
    min_count = min(count),
    max_count = max(count),
    min_second = second_value[which.min(count)],
    max_second = second_value[which.max(count)]
  )]
  
  cat("\nExpected count per second (if uniform): ", format(round(total_records / 60), big.mark = ","), "\n", sep = "")
  cat("Actual mean count per second: ", format(round(stats$mean_count), big.mark = ","), "\n", sep = "")
  cat("Median count per second: ", format(round(stats$median_count), big.mark = ","), "\n", sep = "")
  cat("Standard deviation: ", format(round(stats$sd_count), big.mark = ","), "\n", sep = "")
  cat("\nMinimum: ", format(stats$min_count, big.mark = ","), " (second ", sprintf("%02d", stats$min_second), ")\n", sep = "")
  cat("Maximum: ", format(stats$max_count, big.mark = ","), " (second ", sprintf("%02d", stats$max_second), ")\n", sep = "")
  cat("Range: ", format(stats$max_count - stats$min_count, big.mark = ","), "\n", sep = "")
  
  # Coefficient of variation
  cv <- (stats$sd_count / stats$mean_count) * 100
  cat("Coefficient of variation: ", round(cv, 2), "%\n", sep = "")
  
  # Identify notable seconds
  on_minute_count <- second_summary[second_value == 0, count]
  on_minute_pct <- second_summary[second_value == 0, pct]
  
  cat("\n", rep("-", 80), "\n", sep = "")
  cat("NOTABLE PATTERNS:\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nSecond :00 (on the minute): ", format(on_minute_count, big.mark = ","), 
      " (", on_minute_pct, "%)\n", sep = "")
  
  # Check for other high-frequency seconds
  high_seconds <- second_summary[count > stats$mean_count * 1.5 & second_value != 0]
  if (nrow(high_seconds) > 0) {
    cat("\nOther high-frequency seconds (>150% of mean):\n")
    for (i in 1:nrow(high_seconds)) {
      cat("  Second :", sprintf("%02d", high_seconds$second_value[i]), 
          " - ", format(high_seconds$count[i], big.mark = ","), 
          " (", high_seconds$pct[i], "%)\n", sep = "")
    }
  }
  
  # Check for suspiciously low-frequency seconds
  low_seconds <- second_summary[count < stats$mean_count * 0.5 & count > 0]
  if (nrow(low_seconds) > 0) {
    cat("\nLow-frequency seconds (<50% of mean):\n")
    for (i in 1:nrow(low_seconds)) {
      cat("  Second :", sprintf("%02d", low_seconds$second_value[i]), 
          " - ", format(low_seconds$count[i], big.mark = ","), 
          " (", low_seconds$pct[i], "%)\n", sep = "")
    }
  }
  
  # Check for zero-count seconds
  zero_seconds <- second_summary[count == 0]
  if (nrow(zero_seconds) > 0) {
    cat("\nSeconds with ZERO occurrences:\n")
    cat("  ", paste(sprintf("%02d", zero_seconds$second_value), collapse = ", "), "\n", sep = "")
  }
  
  # ==========================================================================
  # Chi-Square Test: All Seconds (00-59)
  # ==========================================================================
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("CHI-SQUARE TEST: Uniformity of ALL Seconds (00-59)\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("\nQUESTION: Are timestamps uniformly distributed across all 60 seconds,\n")
  cat("          or do certain seconds occur more/less frequently than expected?\n\n")
  cat("NULL HYPOTHESIS: Timestamps are uniformly distributed. Each second should\n")
  cat("                 have approximately 1.67% (1/60) of all records.\n\n")
  
  # Perform chi-square test on all seconds
  observed_counts_all <- second_summary$count
  expected_prob_all <- rep(1/60, 60)
  
  chisq_seconds_all <- chisq.test(observed_counts_all, p = expected_prob_all)
  
  print(chisq_seconds_all)
  
  # Interpretation
  cat("\n", rep("-", 80), "\n", sep = "")
  cat("INTERPRETATION:\n")
  cat(rep("-", 80), "\n", sep = "")
  
  # Determine significance level
  if (chisq_seconds_all$p.value < 2.2e-16) {
    sig_desc <- "ASTRONOMICALLY SIGNIFICANT"
    p_display <- "< 2.2e-16 (essentially zero)"
  } else if (chisq_seconds_all$p.value < 0.001) {
    sig_desc <- "HIGHLY SIGNIFICANT"
    p_display <- sprintf("= %.2e", chisq_seconds_all$p.value)
  } else if (chisq_seconds_all$p.value < 0.01) {
    sig_desc <- "VERY SIGNIFICANT"
    p_display <- sprintf("= %.4f", chisq_seconds_all$p.value)
  } else if (chisq_seconds_all$p.value < 0.05) {
    sig_desc <- "STATISTICALLY SIGNIFICANT"
    p_display <- sprintf("= %.4f", chisq_seconds_all$p.value)
  } else {
    sig_desc <- "NOT SIGNIFICANT"
    p_display <- sprintf("= %.4f", chisq_seconds_all$p.value)
  }
  
  cat("\nSTATISTICAL SIGNIFICANCE: ", sig_desc, "\n", sep = "")
  cat("p-value ", p_display, "\n", sep = "")
  cat("Chi-square statistic: ", format(round(chisq_seconds_all$statistic, 2), big.mark = ","), "\n", sep = "")
  cat("Degrees of freedom: ", chisq_seconds_all$parameter, "\n", sep = "")
  
  if (chisq_seconds_all$p.value < 0.05) {
    cat("\nCONCLUSION: We REJECT the null hypothesis.\n")
    cat("            The distribution is NOT uniform - timestamps show\n")
    cat("            systematic patterns in their second values.\n")
    cat("            This is expected due to the spike at second :00.\n")
  } else {
    cat("\nCONCLUSION: We CANNOT reject the null hypothesis.\n")
    cat("            The distribution appears reasonably uniform.\n")
  }
  
  if (chisq_seconds_all$p.value < 0.05) {
    # Calculate how much the distribution deviates from uniform
    ratio_00 <- (on_minute_pct / 1.67)
    if (ratio_00 > 2) {
      cat("\nDATA QUALITY IMPLICATION:\n")
      cat("   Second :00 occurs ", round(ratio_00, 1), "x more frequently than expected,\n", sep = "")
      cat("   indicating significant timestamp rounding.\n")
    }
  }
  
  # ==========================================================================
  # PART 2: MINUTE DISTRIBUTION ANALYSIS - ALL MINUTES (00-59)
  # ==========================================================================
  
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("MINUTE DISTRIBUTION ANALYSIS: ", col_display, " (ALL minutes 00-59)\n", sep = "")
  cat(rep("=", 70), "\n", sep = "")
  cat("\nAnalyzing distribution of timestamps across ALL 60 minutes (00-59)\n\n")
  
  # Extract minutes from timestamp
  data_with_all_minutes <- data[, .(minute_value = minute(get(date_col)))]
  
  # Count occurrences of ALL minutes (0-59)
  all_minute_counts <- data_with_all_minutes[, .N, by = minute_value][order(minute_value)]
  
  # Ensure all minutes 0-59 are represented
  all_minutes_full <- data.table(minute_value = 0:59)
  minute_summary_full <- merge(all_minutes_full, all_minute_counts, by = "minute_value", all.x = TRUE)
  minute_summary_full[is.na(N), N := 0]
  
  # Calculate percentage and cumulative percentage
  total_all_minutes <- nrow(data)
  minute_summary_full[, `:=`(
    count = N,
    pct = round(N / total_all_minutes * 100, 2),
    cum_pct = round(cumsum(N) / total_all_minutes * 100, 2)
  )]
  
  minute_summary_full[, N := NULL]
  minute_summary_full[, count_formatted := format(count, big.mark = ",")]
  
  # Statistics for ALL minutes
  minute_stats_full <- minute_summary_full[, .(
    mean_count = mean(count),
    median_count = median(count),
    sd_count = sd(count),
    min_count = min(count),
    max_count = max(count),
    min_minute = minute_value[which.min(count)],
    max_minute = minute_value[which.max(count)]
  )]
  
  minute_00_count <- minute_summary_full[minute_value == 0, count]
  minute_00_pct <- minute_summary_full[minute_value == 0, pct]
  
  cat("Total records analyzed: ", format(total_all_minutes, big.mark = ","), "\n", sep = "")
  cat("Expected count per minute (if uniform): ", 
      format(round(total_all_minutes / 60), big.mark = ","), " (1.67%)\n", sep = "")
  cat("\nMinute :00 (on the hour):\n")
  cat("  Count: ", format(minute_00_count, big.mark = ","), 
      " (", minute_00_pct, "%)\n", sep = "")
  cat("  Ratio to expected: ", round(minute_00_pct / 1.67, 2), "x\n", sep = "")
  
  # Create subset for 01-59 for display
  minute_summary_ex00 <- minute_summary_full[minute_value != 0]
  minute_summary_ex00[, count_formatted := format(count, big.mark = ",")]
  
  # Add total row for 01-59
  minute_total_row <- data.table(
    minute_value = NA_integer_,
    count = sum(minute_summary_ex00$count),
    pct = 100.00,
    cum_pct = 100.00,
    count_formatted = format(sum(minute_summary_ex00$count), big.mark = ",")
  )
  
  minute_summary_with_total <- rbind(minute_summary_ex00, minute_total_row)
  
  # Print the table (01-59)
  cat("\n", rep("-", 70), "\n", sep = "")
  cat("MINUTE DISTRIBUTION TABLE (01-59):\n")
  cat(rep("-", 70), "\n", sep = "")
  
  display_minute_table <- minute_summary_with_total[, .(
    minute = fifelse(is.na(minute_value), "TOTAL", sprintf("%02d", minute_value)),
    count = count_formatted,
    pct = pct,
    cum_pct = cum_pct
  )]
  
  print(display_minute_table)
  
  # Chi-square test for ALL minutes 00-59
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("CHI-SQUARE TEST: Uniformity of ALL Minutes 00-59\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\nQUESTION: Are ALL minutes (including :00) uniformly distributed?\n\n")
  cat("NULL HYPOTHESIS: All 60 minutes are uniformly distributed.\n")
  cat("                 Each should have ~1.67% (1/60) of records.\n\n")
  
  observed_all_minutes <- minute_summary_full$count
  expected_all_prob <- rep(1/60, 60)
  
  chisq_all_minutes <- chisq.test(observed_all_minutes, p = expected_all_prob)
  
  print(chisq_all_minutes)
  
  cat("\n", rep("-", 70), "\n", sep = "")
  cat("INTERPRETATION:\n")
  cat(rep("-", 70), "\n", sep = "")
  
  # Determine significance level
  if (chisq_all_minutes$p.value < 2.2e-16) {
    sig_desc_all <- "ASTRONOMICALLY SIGNIFICANT"
    p_display_all <- "< 2.2e-16 (essentially zero)"
  } else if (chisq_all_minutes$p.value < 0.001) {
    sig_desc_all <- "HIGHLY SIGNIFICANT"
    p_display_all <- sprintf("= %.2e", chisq_all_minutes$p.value)
  } else if (chisq_all_minutes$p.value < 0.01) {
    sig_desc_all <- "VERY SIGNIFICANT"
    p_display_all <- sprintf("= %.4f", chisq_all_minutes$p.value)
  } else if (chisq_all_minutes$p.value < 0.05) {
    sig_desc_all <- "STATISTICALLY SIGNIFICANT"
    p_display_all <- sprintf("= %.4f", chisq_all_minutes$p.value)
  } else {
    sig_desc_all <- "NOT SIGNIFICANT"
    p_display_all <- sprintf("= %.4f", chisq_all_minutes$p.value)
  }
  
  cat("\nSTATISTICAL SIGNIFICANCE: ", sig_desc_all, "\n", sep = "")
  cat("p-value ", p_display_all, "\n", sep = "")
  cat("Chi-square statistic: ", format(round(chisq_all_minutes$statistic, 2), big.mark = ","), "\n", sep = "")
  
  if (chisq_all_minutes$p.value < 0.05) {
    cat("\nCONCLUSION: ALL minutes 00-59 are NOT uniformly distributed.\n")
    cat("            Significant patterns exist in minute-level timestamps.\n")
    cat("            This suggests systematic rounding or automated processes\n")
    cat("            creating records at specific minutes.\n")
  } else {
    cat("\nCONCLUSION: All minutes appear uniformly distributed.\n")
    cat("            No systematic minute-level patterns detected.\n")
  }
  
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("END OF MINUTE DISTRIBUTION ANALYSIS\n")
  cat(rep("=", 70), "\n", sep = "")
  
  # Return results invisibly for further analysis if needed
  invisible(list(
    second_summary = second_summary_with_total,
    chisq_seconds_all = chisq_seconds_all,       # Test for all seconds 00-59
    summary_stats = stats,
    on_minute_count = on_minute_count,
    on_minute_pct = on_minute_pct,
    minute_summary_full = minute_summary_full,   # All minutes 0-59
    chisq_all_minutes = chisq_all_minutes,       # Test for all minutes 00-59
    minute_summary = minute_summary_with_total   # Minutes 1-59 for display
  ))
}