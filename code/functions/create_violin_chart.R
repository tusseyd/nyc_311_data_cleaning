create_violin_chart <- function(
    dataset,
    x_axis_title = NULL,
    x_axis_field,
    chart_title,
    chart_file_name,
    chart_directory,
    chart_width = 10,
    chart_height = 7,
    margin_top = 1,
    margin_right = 2,
    margin_bottom = 1,
    margin_left = 2,
    x_axis_tick_size = 14,
    x_axis_label_size = 14,
    x_axis_tick_length = unit(0.3, "cm"),
    subtitle_offset_pos = 45,  # Offset for positive distributions (left margin)
    subtitle_offset_neg = 30   # Offset for negative distributions (right margin)
) {
  
  # Compute count
  n <- nrow(dataset)
  
  # Determine if distribution is entirely negative
  x_values <- dataset[[x_axis_field]]
  x_range <- range(x_values, na.rm = TRUE)
  is_negative <- x_range[2] < 0  # max < 0 means all negative
  
  # Set subtitle alignment and margin based on distribution
  if (is_negative) {
    subtitle_text <- paste0("n = ", format(n, big.mark = ","))
    subtitle_hjust <- 1  # Right align
    subtitle_margin <- margin(l = 0, r = subtitle_offset_neg, unit = "pt")
  } else {
    subtitle_text <- paste0("n = ", format(n, big.mark = ","))
    subtitle_hjust <- 0  # Left align
    subtitle_margin <- margin(l = subtitle_offset_pos, r = 0, unit = "pt")
  }
  
  # Create the violin chart
  violin_chart <- ggplot(
    data = dataset,
    aes(x = !!rlang::sym(x_axis_field), y = factor(1))) +
    
    geom_jitter(width = 0.2, height = 0.4, alpha = 0.85, color = "#0072B2", size = 2, shape = 17) +
    
    geom_violin(linewidth = 0.7, fill = "transparent", color = "black") +
    
    geom_boxplot(width = 0.25, fill = "#E69F00", color = "black", alpha = 0.65, 
                 outlier.colour = "black", outlier.size = 0.75) +
    
    scale_y_discrete(expand = c(0, 0)) +
    
    labs(
      title = chart_title,
      subtitle = subtitle_text,
      x = x_axis_title,
      y = NULL
    ) +
    
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = subtitle_hjust, face = "bold", 
                                   margin = subtitle_margin),
      axis.text.x = element_text(face = "bold", size = x_axis_tick_size),
      axis.title.x = element_text(size = x_axis_label_size, face = "bold"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.length = x_axis_tick_length,
      panel.background = element_rect(fill = "gray96", color = "gray96"),
      plot.margin = margin(t = margin_top, r = margin_right, 
                           b = margin_bottom, l = margin_left, unit = "pt")
    )
  
  suppressMessages(print(violin_chart))
  
  chart_path <- file.path(chart_directory, chart_file_name)
  ggsave(chart_path, plot = violin_chart, dpi = 300,
         width = chart_width, height = chart_height)
}