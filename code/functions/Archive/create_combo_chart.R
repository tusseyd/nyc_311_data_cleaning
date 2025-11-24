create_combo_chart <- function(
    dataset,
    x_col,                      # <— unquoted column name (e.g., complaint_type)
    chart_title = NULL,
    chart_file_name,
    chart_directory,
    rows_to_print = 20,
    chart_width = 13,
    chart_height = 8.5,
    annotation_size = 4.5,
    num_x_labels = 20,
    skip_frequency = 2,
    x_axis_tick_size = 14,
    x_axis_label_angle = 90,
    include_na = FALSE
) {
  stopifnot(dir.exists(chart_directory))
  
  # 1) Summarize
  df <- dataset %>%
    { if (include_na) . else filter(., !is.na({{x_col}})) } %>%
    count({{x_col}}, name = "count") %>%
    arrange(desc(count)) %>%
    mutate(
      percentage = count / sum(count),
      cumulative_percentage = cumsum(percentage)
    ) %>%
    slice_head(n = rows_to_print) %>%
    rename(x = {{x_col}})
  
  # Order factor for plotting
  df <- df %>% mutate(x = factor(x, levels = x))
  
  # rows_to_print <- 10L  # set as needed
  n <- nrow(df)
  
  # Header: include "first <rows_to_print>" only if there are more rows than the limit
  cat(
    "\nData Summary",
    if (n > rows_to_print) sprintf(" (first %d of %d rows)", rows_to_print, n) else sprintf(" (%d rows)", n),
    ":\n",
    sep = ""
  )
  
  # Print the table (rounded) and only up to rows_to_print when needed
  df %>%
    mutate(
      percentage = round(percentage, 4),
      cumulative_percentage = round(cumulative_percentage, 4)
    ) %>%
    slice_head(n = min(n, rows_to_print)) %>%   # all rows if n <= rows_to_print
    as.data.frame() %>%
    print(row.names = FALSE, right = FALSE)
  
  # Label skipping logic
  total_x_labels <- length(unique(df$x))
  blanker <- function(v) {
    if (total_x_labels > num_x_labels) ifelse(seq_along(v) %% skip_frequency == 0, "", v) else v
  }
  count_labels <- blanker(df$count)
  cum_labels   <- blanker(round(df$cumulative_percentage, 2))
  
  # 2) Chart
  max_count <- max(df$count)
  combo_chart <- ggplot(df) +
    geom_bar(aes(x = x, y = count), stat = "identity", fill = "#44AA99", width = 0.55) +
    geom_text(aes(x = x, y = count, label = count_labels),
              colour = "black", hjust = 0.5, vjust = -0.5, size = annotation_size) +
    geom_text(aes(x = x, y = max_count * cumulative_percentage, label = cum_labels),
              size = annotation_size, colour = "black", hjust = 0.5, vjust = 1.7) +
    { if (nrow(df) > 1)
      geom_line(aes(x = x, y = cumulative_percentage * max_count, group = 1),
                colour = "black", linewidth = 1, linetype = "dotted") } +
    scale_y_continuous(
      labels = scales::comma,
      sec.axis = sec_axis(~ . / max_count)
    ) +
    labs(title = chart_title, x = NULL, y = NULL) +
    theme(
      axis.text.x        = element_text(angle = x_axis_label_angle, vjust = 1, hjust = 1,
                                        face = "bold", size = x_axis_tick_size),
      axis.text.y        = element_text(face = "bold", size = annotation_size + 3),
      axis.text.y.right  = element_text(color = "black", face = "bold", size = annotation_size + 3),
      panel.background   = element_rect(fill = "gray96", color = "gray96"),
      panel.grid.major.y = element_line(linetype = "dotted", color = "gray35", linewidth = 0.5),
      panel.grid.minor   = element_blank(),
      legend.position    = "none",
      plot.title         = element_text(hjust = 0.5, size = annotation_size + 7),
      plot.margin        = margin(1, 2, 1, 2)
    )
  
  print(combo_chart)
  Sys.sleep(3)
  
  # 3) Save
  ggsave(file.path(chart_directory, chart_file_name),
         plot = combo_chart, width = chart_width, height = chart_height, dpi = 300)
}
