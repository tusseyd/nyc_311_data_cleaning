# ---- Response Time Summary by Complaint Type ----
summarize_complaint_response <- function(
    DT, 
    min_records = 1L, 
    print_top = 15L,
    lower_exclusion_limit = NULL,
    upper_exclusion_limit = NULL
) {
  stopifnot(data.table::is.data.table(DT))
  
  # Create a working copy to apply exclusions
  working_dt <- DT[!is.na(complaint_type) & !is.na(duration_days)]
  
  # Track exclusions for reporting
  n_original <- nrow(working_dt)
  n_lower_excluded <- 0L
  n_upper_excluded <- 0L
  
  # Apply lower exclusion limit
  if (!is.null(lower_exclusion_limit)) {
    n_lower_excluded <- working_dt[duration_days < lower_exclusion_limit, .N]
    working_dt <- working_dt[duration_days >= lower_exclusion_limit]
  }
  
  # Apply upper exclusion limit
  if (!is.null(upper_exclusion_limit)) {
    n_upper_excluded <- working_dt[duration_days > upper_exclusion_limit, .N]
    working_dt <- working_dt[duration_days <= upper_exclusion_limit]
  }
  
  n_final <- nrow(working_dt)
  n_total_excluded <- n_original - n_final
  
  # Compute statistics on filtered data - ALL IN DAYS
  stats_dt <- working_dt[
    ,
    .(
      N      = .N,
      Min_days    = round(min(duration_days), 2),
      Median_days = round(stats::median(duration_days), 2),
      Mean_days   = round(mean(duration_days), 2),
      Max_days    = round(max(duration_days), 2)
    ),
    by = complaint_type
  ][N >= min_records]
  
  # ---- Console output: Exclusion summary ----
  cat("\n=== Complaint Type Response Time Summary ===\n")
  cat(sprintf("Total complaint types: %d (N >= %d)\n", 
              nrow(stats_dt), min_records))
  
  if (!is.null(lower_exclusion_limit) || !is.null(upper_exclusion_limit)) {
    cat("\n--- Exclusion Limits Applied ---\n")
    if (!is.null(lower_exclusion_limit)) {
      cat(sprintf("Lower limit: %.2f days (excluded %s records)\n", 
                  lower_exclusion_limit, 
                  format(n_lower_excluded, big.mark = ",")))
    }
    if (!is.null(upper_exclusion_limit)) {
      cat(sprintf("Upper limit: %.2f days (excluded %s records)\n", 
                  upper_exclusion_limit, 
                  format(n_upper_excluded, big.mark = ",")))
    }
    cat(sprintf("Total excluded: %s of %s records (%.2f%%)\n",
                format(n_total_excluded, big.mark = ","),
                format(n_original, big.mark = ","),
                100 * n_total_excluded / n_original))
  }
  cat("\n")
  
  # ---- Console output: Sort 1 - By Median (ascending) ----
  cat(strrep("=", 80), "\n")
  cat("SORT 1: By Median Duration (ascending) - All values in days\n")
  cat(strrep("=", 80), "\n\n")
  
  stats_by_median <- stats_dt[order(Median_days, complaint_type)]
  to_print <- as.data.frame(stats_by_median)
  row.names(to_print) <- NULL
  print(to_print, row.names = FALSE)
  
  # ---- Console output: Sort 2 - By N (descending) ----
  cat("\n\n")
  cat(strrep("=", 80), "\n")
  cat("SORT 2: By Count (N, descending) - All values in days\n")
  cat(strrep("=", 80), "\n\n")
  
  stats_by_count <- stats_dt[order(-N, complaint_type)]
  to_print <- as.data.frame(stats_by_count)
  row.names(to_print) <- NULL
  print(to_print, row.names = FALSE)
  
  # ---- Console output: Sort 3 - Alphabetically ----
  cat("\n\n")
  cat(strrep("=", 80), "\n")
  cat("SORT 3: Alphabetically by Complaint Type - All values in days\n")
  cat(strrep("=", 80), "\n\n")
  
  stats_alphabetical <- stats_dt[order(complaint_type)]
  to_print <- as.data.frame(stats_alphabetical)
  row.names(to_print) <- NULL
  print(to_print, row.names = FALSE)
  
  # Return the original (median-sorted) version
  invisible(stats_by_median)
}