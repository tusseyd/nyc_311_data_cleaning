# ---- Response Time Summary by Complaint Type ----
summarize_complaint_response <- function(
    DT, 
    min_records = 1L, 
    print_top = 15L
) {
  stopifnot(data.table::is.data.table(DT))
  
  stats_dt <- DT[
    !is.na(complaint_type) & !is.na(duration_days),
    .(
      N      = .N,
      Min    = round(min(duration_days), 2),
      Median = round(stats::median(duration_days), 2),
      Mean   = round(mean(duration_days), 2),
      Max    = round(max(duration_days), 2),
      Median_hours = round(stats::median(duration_days) * 24, 4)
    ),
    by = complaint_type
  ][N >= min_records]
  
  # ---- Console output: Sort 1 - By Median (ascending) ----
  cat("\n=== Complaint Type Response Time Summary ===\n")
  cat(sprintf("Total complaint types: %d (N >= %d)\n\n",
              nrow(stats_dt), min_records))
  
  cat(strrep("=", 80), "\n")
  cat("SORT 1: By Median Duration (ascending)\n")
  cat(strrep("=", 80), "\n\n")
  
  stats_by_median <- stats_dt[order(Median, complaint_type)]
  to_print <- as.data.frame(stats_by_median)
  row.names(to_print) <- NULL
  print(to_print, row.names = FALSE)
  
  # ---- Console output: Sort 2 - By N (descending) ----
  cat("\n\n")
  cat(strrep("=", 80), "\n")
  cat("SORT 2: By Count (N, descending)\n")
  cat(strrep("=", 80), "\n\n")
  
  stats_by_count <- stats_dt[order(-N, complaint_type)]
  to_print <- as.data.frame(stats_by_count)
  row.names(to_print) <- NULL
  print(to_print, row.names = FALSE)
  
  # ---- Console output: Sort 3 - Alphabetically ----
  cat("\n\n")
  cat(strrep("=", 80), "\n")
  cat("SORT 3: Alphabetically by Complaint Type\n")
  cat(strrep("=", 80), "\n\n")
  
  stats_alphabetical <- stats_dt[order(complaint_type)]
  to_print <- as.data.frame(stats_alphabetical)
  row.names(to_print) <- NULL
  print(to_print, row.names = FALSE)
  
  # Return the original (median-sorted) version
  invisible(stats_by_median)
}