create_qcc_style_plot <- function(count_data, 
                                  sample_sizes, 
                                  title = "P-Chart",
                                  xlab = "Sample",
                                  ylab = "Proportion",
                                  x_labels = NULL) {
  
  # Calculate proportions
  p <- count_data / sample_sizes
  
  # Calculate center line (overall proportion)
  p_bar <- sum(count_data) / sum(sample_sizes)
  
  # Calculate control limits for each sample
  # UCL and LCL vary with sample size
  ucl <- p_bar + 3 * sqrt(p_bar * (1 - p_bar) / sample_sizes)
  lcl <- p_bar - 3 * sqrt(p_bar * (1 - p_bar) / sample_sizes)
  lcl <- pmax(lcl, 0)  # LCL can't be negative
  
  # Create data frame for plotting
  plot_data <- data.table(
    sample = 1:length(p),
    proportion = p,
    ucl = ucl,
    lcl = lcl,
    sample_size = sample_sizes
  )
  
  # Use custom labels if provided
  if (!is.null(x_labels)) {
    plot_data$label <- x_labels
  } else {
    plot_data$label <- as.character(plot_data$sample)
  }
  
  # Identify out-of-control points
  plot_data[, out_of_control := proportion > ucl | proportion < lcl]
  
  # Create the plot
  gg <- ggplot(plot_data, aes(x = sample, y = proportion)) +
    # Control limits (varying by sample size)
    geom_line(aes(y = ucl), color = "red", linetype = "dashed", linewidth = 0.8) +
    geom_line(aes(y = lcl), color = "red", linetype = "dashed", linewidth = 0.8) +
    
    # Center line
    geom_hline(yintercept = p_bar, color = "blue", linetype = "solid", 
               linewidth = 1) +
    
    # Data points
    geom_line(color = "black", linewidth = 0.6) +
    geom_point(aes(color = out_of_control), size = 2.5) +
    
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"),
                       guide = "none") +
    
    # Labels and theme
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
    ) +
    
    # Annotate with control limit values
    annotate("text", x = length(p) * 0.95, y = p_bar, 
             label = sprintf("CL = %.4f", p_bar), 
             vjust = -0.5, hjust = 1, color = "blue", size = 3.5) +
    annotate("text", x = length(p) * 0.95, y = max(ucl), 
             label = sprintf("UCL = %.4f", max(ucl)), 
             vjust = -0.5, hjust = 1, color = "red", size = 3.5) +
    annotate("text", x = length(p) * 0.95, y = min(lcl), 
             label = sprintf("LCL = %.4f", min(lcl)), 
             vjust = 1.5, hjust = 1, color = "red", size = 3.5)
  
  # Optionally set x-axis labels if provided
  if (!is.null(x_labels)) {
    # For large datasets, show only every nth label
    n_labels <- length(x_labels)
    label_freq <- ceiling(n_labels / 20)  # Show ~20 labels max
    
    label_positions <- seq(1, n_labels, by = label_freq)
    gg <- gg + scale_x_continuous(
      breaks = label_positions,
      labels = x_labels[label_positions]
    )
  }
  
  return(gg)
}