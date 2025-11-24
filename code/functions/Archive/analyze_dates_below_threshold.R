analyze_dates_below_threshold <- function(data, date_field, threshold_date = "2003-01-01", field_name = NULL) {
  # Convert field_name for display (use actual column name if not provided)
  if (is.null(field_name)) {
    field_name <- deparse(substitute(date_field))
  }
  
  # Extract the date column
  date_col <- data[[date_field]]
  
  # Convert threshold to POSIXct if it's a string
  if (is.character(threshold_date)) {
    threshold_date <- as.POSIXct(threshold_date)
  }
  
  # Filter dates below threshold (removing NAs)
  early_dates <- date_col[!is.na(date_col) & date_col < threshold_date]
  
  # Get total count
  total_count <- length(early_dates)
  
  # Print summary only if there are records
  if (total_count > 0) {
    cat("\n=== Analysis for field:", field_name, "===\n")
    cat("Threshold date:", format(threshold_date, "%Y-%m-%d"), "\n")
    cat("Total records below threshold:", total_count, "\n")
    
    # … continue with deeper analysis here …
    
  } else {
    cat("\n=== Analysis for field:", field_name, "===\n")
    cat("Threshold date:", format(threshold_date, "%Y-%m-%d"), "\n")
    cat("No dates found below threshold.\n")
    return(invisible(NULL))
  }
  
  # Get counts by unique date values
  date_counts <- table(early_dates)
  date_counts <- sort(date_counts, decreasing = TRUE)
  
  cat("Unique dates below threshold:", length(date_counts), "\n")
  cat("Date range:", format(min(early_dates), "%Y-%m-%d %H:%M:%S"), 
      "to", format(max(early_dates), "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Display the counts
  cat("\nCounts by unique date:\n")
  print(date_counts)
  
  # Return the results invisibly for further use
  return(invisible(list(
    field_name = field_name,
    total_count = total_count,
    unique_dates = length(date_counts),
    date_counts = date_counts,
    threshold = threshold_date
  )))
}