################################################################################
# For analyze_data_conditions function:
analyze_data_conditions <- function(
    data, 
    sample_seed = 42)
{
  
  data <- copy(data)
  
  ####################
  # Calculate the minimum monthly count to use as sample size
  total_per_month <- data[, .N, by = year_month]
  min_count <- min(total_per_month$N)
  
  # Take random samples of equal size from each month
  sampled_data <- data.table()
  
  # Set seed for reproducibility
  set.seed(sample_seed)
  
  # Loop through each month and collect a sample of rows
  sampled_list <- vector("list", length(unique(data$year_month)))
  i <- 1
  for (month in unique(data$year_month)) {
    month_data <- data[year_month == month]
    month_sample <- month_data[sample(.N, min_count, replace = FALSE)]
    sampled_list[[i]] <- month_sample
    i <- i + 1
  }
  sampled_data <- rbindlist(sampled_list)
  
  ####################
  # Create indicators for all conditions (in both sampled and full data)
  # First for sampled data
  sampled_data[, SRs_with_a_midnight_closed_date := 
                 ifelse(!is.na(closed_date), 
                        format(closed_date, "%H:%M:%S") == "00:00:00", FALSE)]
  
  sampled_data[, SRs_with_a_resolution_update_date_occurring_before_creation := 
                 ifelse(!is.na(resolution_action_updated_date) & !is.na(created_date), 
                        resolution_action_updated_date < created_date, FALSE)]
  
  sampled_data[, SRs_with_not_closed_status_but_with_a_closed_date := 
                 ifelse(!is.na(closed_date) & !is.na(status), 
                        !is.na(closed_date) & status != "CLOSED", FALSE)]
  
  sampled_data[, SRs_with_status_set_to_closed_but_no_closed_date 
               := ifelse(!is.na(status), status == "CLOSED" 
                         & is.na(closed_date), FALSE)]
  
  sampled_data[, SRs_with_late_resolution_updates := 
                 ifelse(!is.na(closed_date) & !is.na(resolution_action_updated_date), 
                        as.numeric(difftime(resolution_action_updated_date, 
                                            closed_date, units = "days")) > 90, FALSE)]
  
  sampled_data[, SRs_with_negative_or_zero_duration := 
                 ifelse(!is.na(closed_date) & !is.na(created_date), 
                        closed_date <= created_date, FALSE)]
  
  sampled_data[, SRs_with_cross_intersection_1_matching_intersection_street_1 := 
                 ifelse(cross_street_1 != intersection_street_1, FALSE, TRUE)]
  
  sampled_data[, SRs_with_cross_intersection_2_matching_intesection_street_2 := 
                 ifelse(cross_street_2 != intersection_street_2, FALSE, TRUE)]
  
  sampled_data[, SRs_wth_street_name_matching_landmark := 
                 ifelse(street_name != landmark, FALSE, TRUE)]
  
  ####################
  # Now for full data
  data[, SRs_with_a_midnight_closed_date := 
         ifelse(!is.na(closed_date), 
                format(closed_date, "%H:%M:%S") == "00:00:00", FALSE)]
  
  data[, SRs_with_a_resolution_update_date_occurring_before_creation := 
         ifelse(!is.na(resolution_action_updated_date) & 
                  !is.na(created_date), resolution_action_updated_date 
                < created_date, FALSE)]
  
  data[, SRs_with_not_closed_status_but_with_a_closed_date := 
         ifelse(!is.na(closed_date) & !is.na(status), 
                !is.na(closed_date) & status != "CLOSED", FALSE)]
  
  data[, SRs_with_status_set_to_closed_but_no_closed_date := 
         ifelse(!is.na(status), status == "CLOSED" & is.na(closed_date), FALSE)]
  
  data[, SRs_with_late_resolution_updates := ifelse(!is.na(closed_date) & 
                                                      !is.na(resolution_action_updated_date), 
                                                    as.numeric(difftime(resolution_action_updated_date, 
                                                                        closed_date, units = "days")) >90, FALSE)]
  
  data[, SRs_with_negative_or_zero_duration := 
         ifelse(!is.na(closed_date) & !is.na(created_date), 
                closed_date <= created_date, FALSE)]
  
  data[, SRs_with_cross_intersection_1_matching_intersection_street_1 := 
         ifelse(cross_street_1 != intersection_street_1, FALSE, TRUE)]
  
  data[, SRs_with_cross_intersection_2_matching_intesection_street_2  := 
         ifelse(cross_street_2 != intersection_street_2, FALSE, TRUE)]
  
  data[, SRs_wth_street_name_matching_landmark  := 
         ifelse(street_name != landmark, FALSE, TRUE)]
  
  ####################
  # Create a list to store all results
  results <- list()
  
  # Define conditions
  conditions <- c(
    "SRs_with_a_midnight_closed_date", 
    "SRs_with_a_resolution_update_date_occurring_before_creation", 
    "SRs_with_not_closed_status_but_with_a_closed_date", 
    "SRs_with_status_set_to_closed_but_no_closed_date", 
    "SRs_with_late_resolution_updates", 
    "SRs_with_negative_or_zero_duration",
    "SRs_with_cross_intersection_1_matching_intersection_street_1",
    "SRs_with_cross_intersection_2_matching_intesection_street_2",
    "SRs_wth_street_name_matching_landmark"
  )
  
  ####################
  # Process each condition for both sampled and full data
  for (condition in conditions) {
    
    # 1. For sampled data (for QCC and QIC)
    sampled_result <- sampled_data[, .(
      count = sum(get(condition), na.rm = TRUE),
      N = .N,
      fraction = sum(get(condition), na.rm = TRUE) / .N
    ), by = year_month]
    
    # 2. For full data (for ggplot)
    full_result <- data[, .(
      count = sum(get(condition), na.rm = TRUE),
      N = .N,
      fraction = sum(get(condition), na.rm = TRUE) / .N
    ), by = year_month]
    
    # Make sure the data is in chronological order
    sampled_result <- sampled_result[order(year_month)]
    full_result <- full_result[order(year_month)]
    
    # Convert year_month from character to Date for proper plotting
    sampled_result[, year_month := as.Date(paste0(year_month, "-01"))]
    full_result[, year_month := as.Date(paste0(year_month, "-01"))]
    
    # Store in results list
    results[[paste0(condition, "_sampled")]] <- copy(sampled_result)
    results[[condition]] <- copy(full_result)
  }
  
  return(results)
}
