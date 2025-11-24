################################################################################
create_condition_plot <- function( 
    data, 
    title = NULL, 
    y_label = NULL, 
    subtitle = NULL, 
    value_field = "fraction",
    bias_value = NULL,  # Changed to NULL by default to enable auto-detection
    date_field = "year_month",
    loess_threshold = 10,
    auto_scale = TRUE  # New parameter to enable/disable auto-scaling
) {
  
  # Convert to data.table for efficient handling
  data <- as.data.table(data)
  
  # Coerce the date_field to date if it is not already a date/time class
  if (!inherits(data[[date_field]], c("POSIXct", "POSIXt", "Date"))) {
    cat("\nConverting", title, "to date format...")
    
    # If format looks like "YYYY-MM", assume it's monthly and add "-01"
    if (is.character(data[[date_field]]) &&
        all(grepl("^\\d{4}-\\d{2}$", na.omit(data[[date_field]])))) {
      data[[date_field]] <- as.Date(paste0(data[[date_field]], "-01"))
    } else {
      stop(paste("Unable to coerce", date_field, "to POSIXct format."))
    }
  }
  
  # Calculate process mean before any bias is applied
  raw_process_mean <- mean(data[[value_field]], na.rm = TRUE)
  
  # Auto-detect if values are small and set bias accordingly
  if (auto_scale && is.null(bias_value)) {
    if (raw_process_mean < 0.001) {
      bias_value <- 1000  # For extremely small values (< 0.001)
      y_label_suffix <- " (×1000)"
    } else if (raw_process_mean < 0.01) {
      bias_value <- 100   # For very small values (< 0.01)
      y_label_suffix <- " (×100)"
    } else if (raw_process_mean < 0.1) {
      bias_value <- 10    # For small values (< 0.1)
      y_label_suffix <- " (×10)"
    } else {
      bias_value <- 1     # For normal values
      y_label_suffix <- ""
    }
    
    # Update y-axis label to indicate scaling
    if (!is.null(y_label) && bias_value > 1) {
      y_label <- paste0(y_label, y_label_suffix)
    }
    
    cat("\nAuto-scaling applied for", title, "- Bias factor:", bias_value)
  } 
  
  # Apply bias
  data[[value_field]] <- data[[value_field]] * bias_value
  
  # Rest of the function remains the same...
  # Extract the date column once for reuse
  date_vector <- data[[date_field]]
  data$year <- year(date_vector)
  data$condition_name <- title
  
  # Save full dataset to global environment for inspection/debug
  all_results <<- list()
  all_results[[title]] <- data
  
  # Determine first and last year
  first_year <- min(data$year, na.rm = TRUE)
  last_year  <- max(data$year, na.rm = TRUE)
  
  # Compute means for first and last year
  year_data_first <- data[year == first_year]
  year_data_last  <- data[year == last_year]
  
  mean_first_year <- mean(year_data_first[[value_field]], na.rm = TRUE)
  mean_last_year  <- mean(year_data_last[[value_field]], na.rm = TRUE)
  
  # x-axis segment endpoints
  first_year_start <- min(year_data_first[[date_field]])
  first_year_end   <- max(year_data_first[[date_field]])
  
  last_year_start  <- min(year_data_last[[date_field]])
  last_year_end    <- max(year_data_last[[date_field]])
  
  # Global stats (now using the biased values)
  process_mean <- mean(data[[value_field]], na.rm = TRUE)
  process_sd   <- sd(data[[value_field]], na.rm = TRUE)
  
  # Determine smoothing method based on number of data points
  smooth_method <- ifelse(nrow(data) >= loess_threshold, "loess", "lm")
  
  # Create the plot with basic elements
  chart_object <- ggplot(data, aes(x = .data[[date_field]], 
                                   y = .data[[value_field]])) +
    geom_line(color = "steelblue2", linewidth = 0.55) +
    geom_point(size = 3.5, color = "steelblue4", shape = 18) +
    geom_hline(yintercept = process_mean, color = "darkred", 
               linetype = "dashed", linewidth = 0.8)
  
  # Add appropriate geom_smooth based on method
  if (smooth_method == "loess") {
    chart_object <- chart_object +
      geom_smooth(method = "loess", se=TRUE, color = "gray40", fill = "gray85", 
                  alpha = 0.3, linewidth = 0.85, span = 0.9)
  } else {
    chart_object <- chart_object +
      geom_smooth(method = "lm", se = TRUE, color = "gray40", fill = "gray85", 
                  alpha = 0.3, linewidth = 0.85)
  }
  
  # Add the rest of the plot elements
  chart_object <- chart_object +
    # Overall mean label
    annotate("label", 
             x = min(date_vector) + (max(date_vector) - min(date_vector))/2,
             y = process_mean,
             label = paste("Overall mean:", format(round(process_mean, 5), 
                                                   nsmall = 5)),
             hjust = 0.5, vjust = -0.2, size = 3.5, color = "darkred", 
             fill = "gray99", label.size = NA) +
    
    # Segment lines and mean labels
    annotate("segment", x = first_year_start, xend = first_year_end,
             y = mean_first_year, yend = mean_first_year,
             color = "black", linetype = "dashed", linewidth = 1) +
    annotate("segment", x = last_year_start, xend = last_year_end,
             y = mean_last_year, yend = mean_last_year,
             color = "black", linetype = "dashed", linewidth = 1) +
    
    annotate("label", x = first_year_start, y = mean_first_year,
             label = paste0(first_year, " mean: ", 
                            format(round(mean_first_year, 5), nsmall = 5)),
             hjust = 0, vjust = -0.2, size = 4, color = "black", 
             fill = "gray99", label.size = NA) +
    annotate("label", x = last_year_end, y = mean_last_year,
             label = paste0(last_year, " mean: ", 
                            format(round(mean_last_year, 5), nsmall = 5)),
             hjust = 1, vjust = -0.2, size = 4, color = "black", 
             fill = "gray99", label.size = NA) +
    
    labs(
      title = title,
      subtitle = paste(subtitle, "\n\n"),
      x = NULL,
      y = y_label,
      caption = paste("Mean:", format(round(process_mean, 5), nsmall = 5), 
                      ifelse(bias_value > 1, paste0("(Raw mean: ", 
                                                    format(round(raw_process_mean, 5), 
                                                           nsmall = 5), ")"), ""))
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = expansion(mult = c(0.05, 0.1))
    ) +
    scale_y_continuous(labels = function(x) format(round(x, 2), nsmall = 5)) +
    theme(
      plot.title = element_text(size = 14, face = "bold", color = "black", 
                                margin = margin(b = 10)),
      plot.subtitle = element_text(size = 11, color = "black", 
                                   margin = margin(b = 15)),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
      axis.text.x = element_text(angle = 20, hjust = 1, color = "black"),  
      panel.grid.major = element_line(color = "gray88", linewidth = 0.5),  
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),  
      panel.background = element_rect(fill = "gray99", color = NA),  
      plot.background = element_rect(fill = "gray90", color = NA),   
      plot.caption = element_text(hjust = 1, size = 9, color = "black"),
      axis.title.y = element_text(margin=margin(r = 10, l = 0), color = "black"),
      axis.title.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black")
    )
  
  return(list(
    plot = chart_object,
    title = title,
    first_year_mean = mean_first_year,
    last_year_mean = mean_last_year,
    smooth_method = smooth_method,
    bias_value = bias_value,  # Return the bias value that was used
    raw_process_mean = raw_process_mean  # Return the original mean before scaling
  ))
}
