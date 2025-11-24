########## Functions ##########
analyze_invalid_values <- function(dataset, 
                                   field_name, 
                                   valid_values_list, 
                                   valid_field_name,
                                   sample_seed = 42
) {
  
  # Step 1: Remove NAs from the specified field
  data_no_na <- dataset[ !is.na(dataset[[field_name]]) & 
                           dataset[[field_name]] != "", ]
  
  # Step 2: Inspect for invalid values. Add boolean column for valid values
  data_no_na$field_validity <- data_no_na[[field_name]] %in% 
    valid_values_list[[valid_field_name]]
  
  # Step 3: Find minimum count per year_month to use as sample size
  total_per_month <- data_no_na[, .N, by = year_month]
  min_count <- min(total_per_month$N)
  
  # Step 4: Create the sampled dataset
  sampled_data <- data.table()
  set.seed(sample_seed)
  
  for (month in unique(data_no_na$year_month)) {
    month_data <- data_no_na[year_month == month]
    month_sample <- month_data[sample(.N, min_count, replace = FALSE)]
    sampled_data <- rbind(sampled_data, month_sample)
  }
  
  # Step 4a: Inspect the sample for valid values
  sampled_data$field_validity <- sampled_data[[field_name]] %in%
    valid_values_list[[valid_field_name]]
  
  # Step 5: Standardized column names for consistency, dataset & sample
  full_summary <- data_no_na[, .(
    count = sum(!field_validity),
    N = .N,
    fraction = sum(!field_validity) / .N
  ), by = year_month]  # year_month is already grouped, no need to repeat
  
  sampled_summary <- sampled_data[, .(
    count = sum(!field_validity),
    N = .N,
    fraction = sum(!field_validity) / .N
  ), by = year_month]
  
  # Step 6: Add dataset column to match structure
  full_summary[, dataset := field_name]
  sampled_summary[, dataset := paste0(field_name, "_sampled")]
  
  return(list(full_summary
              , sampled_summary)
  )
}
