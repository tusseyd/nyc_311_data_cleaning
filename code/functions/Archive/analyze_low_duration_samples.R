analyze_low_duration_samples <- function(dt, sample_size = 25) {
  cat("=== LOW DURATION SERVICE REQUEST ANALYSIS ===\n\n")
  
  # Helper function to safely truncate description
  truncate_desc <- function(x, max_chars = 100) {
    if (is.na(x) || is.null(x)) return("[No Description]")
    x <- as.character(x)
    if (nchar(x) <= max_chars) return(x)
    return(paste0(substr(x, 1, max_chars), "..."))
  }
  
  # Define duration categories
  categories <- list(
    "Zero Duration (0 seconds)" = quote(duration_sec == 0),
    "One Second Duration" = quote(duration_sec == 1),
    "Short Duration (3-28 seconds)" = quote(duration_sec >= 3 & duration_sec <= 28)
  )
  
  # Process each category
  for (cat_name in names(categories)) {
    cat(sprintf("=== %s ===\n", toupper(cat_name)))
    
    # Filter data for this category
    filtered_data <- dt[eval(categories[[cat_name]]) & !is.na(duration_sec)]
    
    cat(sprintf("Total records in category: %s\n", 
                scales::comma(nrow(filtered_data))))
    
    if (nrow(filtered_data) == 0) {
      cat("No records found in this category.\n\n")
      next
    }
    
    # Sample records (or all if fewer than sample_size)
    sample_n <- min(sample_size, nrow(filtered_data))
    if (nrow(filtered_data) > sample_size) {
      sample_data <- filtered_data[sample(.N, sample_size)]
    } else {
      sample_data <- filtered_data
    }
    
    cat(sprintf("Showing %d sample records:\n\n", sample_n))
    
    # Prepare display data
    display_data <- sample_data[, .(
      unique_key,
      agency,
      complaint_type,
      duration_sec,
      created_date,
      closed_date,
      description_truncated = sapply(descriptor, truncate_desc, 100)
    )]
    
    # Print each record
    for (i in 1:nrow(display_data)) {
      row <- display_data[i]
      cat(sprintf("Record %d:\n", i))
      cat(sprintf("  Unique Key: %s\n", row$unique_key))
      cat(sprintf("  Agency: %s\n", row$agency))
      cat(sprintf("  Complaint: %s\n", row$complaint_type))
      cat(sprintf("  Duration: %d seconds\n", row$duration_sec))
      cat(sprintf("  Created: %s\n", row$created_date))
      cat(sprintf("  Closed: %s\n", row$closed_date))
      cat(sprintf("  Description: %s\n", row$description_truncated))
      cat("\n")
    }
    
    # Show agency breakdown for this category
    agency_summary <- filtered_data[, .N, by = agency][order(-N)][1:10]
    cat("Top agencies in this duration category:\n")
    for (j in 1:min(10, nrow(agency_summary))) {
      cat(sprintf("  %s: %s records\n", 
                  agency_summary[j]$agency, 
                  scales::comma(agency_summary[j]$N)))
    }
    
    # Show complaint type breakdown
    complaint_summary <- filtered_data[, .N, by = complaint_type][order(-N)][1:5]
    cat("\nTop complaint types in this duration category:\n")
    for (k in 1:min(5, nrow(complaint_summary))) {
      cat(sprintf("  %s: %s records\n", 
                  complaint_summary[k]$complaint_type, 
                  scales::comma(complaint_summary[k]$N)))
    }
    
    cat("\n" %+% paste(rep("-", 80), collapse = "") %+% "\n\n")
  }
  
  # Overall summary
  cat("=== OVERALL SUMMARY ===\n")
  total_low_duration <- dt[
    (duration_sec == 0 | duration_sec == 1 | (duration_sec >= 3 & duration_sec <= 28)) & 
      !is.na(duration_sec), .N
  ]
  total_records <- nrow(dt)
  
  cat(sprintf("Total low duration records: %s\n", scales::comma(total_low_duration)))
  cat(sprintf("Total records in dataset: %s\n", scales::comma(total_records)))
  cat(sprintf("Percentage of low duration: %.2f%%\n", 
              (total_low_duration / total_records) * 100))
  
  invisible(list(
    zero_duration = dt[duration_sec == 0 & !is.na(duration_sec)],
    one_second = dt[duration_sec == 1 & !is.na(duration_sec)],
    short_duration = dt[duration_sec >= 3 & duration_sec <= 28 & !is.na(duration_sec)]
  ))
}

# Run the analysis
low_duration_analysis <- analyze_low_duration_samples(d311, sample_size = 25)