plot_barchart <- function(
    DT,
    x_col,
    y_col,
    
    # Titles and labels
    title = NULL,
    subtitle = NULL,
    x_label = "",
    y_label = "",
    
    # Appearance
    fill_color = "#009E73",
    fill_col = NULL,              # NEW: Column name for conditional coloring
    fill_colors = NULL,           # NEW: Named vector of colors for fill_col levels
    bar_width = 0.8,
    
    # Data summary and printing
    console_print_title = NULL,
    rows_to_print = 20,
    show_summary = TRUE,
    
    # Statistical annotations
    add_mean = FALSE,
    add_median = FALSE,
    add_maximum = FALSE,
    add_minimum = FALSE,
    
    # Standard deviation customization
    add_3sd = FALSE,
    sd_color = "#D55E00",
    sd_linetype = "dashed",
    sd_size = 1.2,
    
    # Mean line customization
    mean_color = "#D55E00",
    mean_linetype = "dotted",
    mean_size = 1.2,
    
    # Data labels
    show_labels = TRUE,
    label_col = NULL,
    label_angle = 0,
    label_size = 3,
    label_color = "black",
    label_vjust = -0.5,
    label_hjust = -0.5,
    
    # Trendline
    add_trendline = FALSE,
    trendline_color = "#D55E00",
    trendline_method = "lm",
    trendline_size = 1.2,
    show_r_squared = FALSE,          # NEW: Show R² on plot
    show_trend_stats = FALSE,        # NEW: Show growth/decline %
    trend_stats_x_pos = "left",      # NEW: Position for stats ("left", "right", "center")
    trend_stats_y_pos = "top",       # NEW: Position for stats ("top", "bottom")
    
    # Extra annotations
    extra_line = NULL,
    
    # Axis customization
    x_label_every = 1,
    x_breaks = NULL,        # NEW: Manual specification of which labels to show
    x_axis_angle = 0,
    x_axis_size = 10,
    y_axis_labels = scales::comma,
    sort_by_count = FALSE,
    
    # Theme and sizing
    text_size = 12,
    chart_width = 13,
    chart_height = 8.5,
    
    # Save options
    chart_dir = NULL,
    filename = NULL,
    dpi = 300
) {
  
  require(data.table)
  require(ggplot2)
  require(scales)
  
  # Ensure data.table
  if (!data.table::is.data.table(DT)) {
    DT <- data.table::setDT(copy(DT))
  } else {
    DT <- copy(DT)
  }
  
  # Validate columns
  stopifnot(x_col %in% names(DT), y_col %in% names(DT))
  
  # Add percentage and cumulative percentage
  DT[, percentage := round(get(y_col) / sum(get(y_col), na.rm = TRUE) * 100, 2)]
  DT[, cumulative_percentage := round(cumsum(get(y_col)) / sum(get(y_col), na.rm = TRUE) * 100, 2)]
  
  # Console output
  if (show_summary) {
    if (is.null(console_print_title)) {
      console_print_title <- paste("Data Summary for", x_col, "vs", y_col)
    }
    
    rows_to_show <- min(nrow(DT), rows_to_print)
    cat("\n", console_print_title, " (first ", rows_to_show, " rows):\n", sep = "")
    
    print(DT[1:rows_to_show, c(x_col, y_col, "percentage", "cumulative_percentage"), with = FALSE],
          row.names = FALSE)
    
    # Yearly summary if available
    if ("year" %in% names(DT)) {
      cat("\nSummary by Year (from 'year' column):\n")
      year_summary <- DT[, .(N = sum(get(y_col), na.rm = TRUE)), by = year][order(year)]
      year_summary[, pct := round(100 * N / sum(N), 2)]
      print(year_summary)
      
    } else if (inherits(DT[[x_col]], c("Date", "POSIXct"))) {
      cat("\nSummary by Year (derived from x_col):\n")
      DT[, year_tmp := year(get(x_col))]
      year_summary <- DT[, .(N = sum(get(y_col), na.rm = TRUE)), by = year_tmp][order(year_tmp)]
      year_summary[, pct := round(100 * N / sum(N), 2)]
      setnames(year_summary, "year_tmp", "year")
      print(year_summary)
    }
  }
  
  # Sorting logic
  if (sort_by_count) {
    if (inherits(DT[[x_col]], c("Date", "POSIXct"))) {
      setorderv(DT, c(y_col, x_col), order = c(-1, 1))
    } else {
      setorderv(DT, y_col, order = -1)
    }
  } else {
    setorderv(DT, x_col)
  }
  
  # Determine x-axis scale based on data type and label spacing
  x_scale <- if (inherits(DT[[x_col]], "Date")) {
    date_range <- range(DT[[x_col]], na.rm = TRUE)
    date_span <- as.numeric(date_range[2] - date_range[1])
    n_points <- nrow(DT)
    
    is_monthly <- all(day(DT[[x_col]]) == 1, na.rm = TRUE) && n_points >= 12
    
    if (is_monthly) {
      break_interval <- paste(x_label_every, "months")
      date_labels <- "%Y-%m"
    } else if (date_span <= 400 && n_points <= 366) {
      break_interval <- paste(x_label_every, "days")
      date_labels <- "%m-%d"
    } else if (n_points <= 10 && date_span > 1000) {
      break_interval <- paste(x_label_every, "years")
      date_labels <- "%Y"
    } else {
      break_interval <- paste(x_label_every, "days")
      date_labels <- "%Y-%m-%d"
    }
    
    scale_x_date(
      expand = c(0.01, 0), 
      labels = scales::date_format(date_labels), 
      breaks = scales::date_breaks(break_interval)
    )
    
  } else if (inherits(DT[[x_col]], c("POSIXct", "POSIXt"))) {
    break_interval <- paste(x_label_every, "hours")
    scale_x_datetime(
      expand = c(0.01, 0), 
      labels = scales::date_format("%H:%M"), 
      breaks = scales::date_breaks(break_interval)
    )
  } else if (is.numeric(DT[[x_col]])) {
    x_range <- range(DT[[x_col]], na.rm = TRUE)
    x_breaks <- seq(from = x_range[1], to = x_range[2], by = x_label_every)
    scale_x_continuous(
      expand = c(0.01, 0),
      breaks = x_breaks
    )
  } else {
    unique_values <- unique(DT[[x_col]])
    
    # Use manual breaks if provided, otherwise calculate automatically
    if (!is.null(x_breaks)) {
      x_breaks_to_use <- x_breaks
    } else if (x_label_every > 1) {
      break_indices <- seq(1, length(unique_values), by = x_label_every)
      x_breaks_to_use <- unique_values[break_indices]
    } else {
      x_breaks_to_use <- unique_values
    }
    
    scale_x_discrete(
      expand = c(0.01, 0),
      breaks = x_breaks_to_use
    )
  }
  
  # Create base plot with conditional fill
  if (!is.null(fill_col) && fill_col %in% names(DT)) {
    # Conditional fill based on a column
    p <- ggplot(DT, aes(x = .data[[x_col]], y = .data[[y_col]], fill = .data[[fill_col]])) +
      geom_col(width = bar_width, color = NA)
    
    # Apply custom colors if provided
    if (!is.null(fill_colors)) {
      p <- p + scale_fill_manual(values = fill_colors)
    }
  } else {
    # Single color fill
    p <- ggplot(DT, aes(x = .data[[x_col]], y = .data[[y_col]])) +
      geom_col(fill = fill_color, width = bar_width, color = NA)
  }
    
    
   p <- p + x_scale +
    scale_y_continuous(labels = y_axis_labels) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    )
  
  # Apply theme
  if (exists("david_theme", mode = "function")) {
    p <- p + david_theme(text_size = text_size, x_axis_angle = x_axis_angle)
  } else {
    p <- p + theme_minimal() +
      theme(
        axis.text.x = element_text(angle = x_axis_angle, 
                                   hjust = ifelse(x_axis_angle > 0, 1, 0.5)),
        text = element_text(size = text_size)
      )
  }
  
  # Statistical annotations
  if (add_mean || add_median || add_maximum || add_minimum) {
    y_values <- DT[[y_col]]
    
    if (add_mean) {
      mean_val <- mean(y_values, na.rm = TRUE)
      p <- p +
        geom_hline(yintercept = mean_val, linetype = mean_linetype, 
                   color = mean_color, linewidth = mean_size)
      
      if (inherits(DT[[x_col]], c("Date", "POSIXct"))) {
        x_pos <- DT[[x_col]][1]
      } else {
        x_pos <- 1
      }
      
      p <- p + annotate("text", x = x_pos, y = mean_val,
                        label = paste0("Mean: ", format(round(mean_val), big.mark = ",")),
                        hjust = -0.1, vjust = -1.6, color = mean_color, size = 3.5)
    }
    
    if (add_median) {
      median_val <- median(y_values, na.rm = TRUE)
      p <- p +
        geom_hline(yintercept = median_val, linetype = "dashed", 
                   color = mean_color, linewidth = mean_size)
      
      if (inherits(DT[[x_col]], c("Date", "POSIXct"))) {
        x_pos <- DT[[x_col]][1]
      } else {
        x_pos <- 1
      }
      
      p <- p + annotate("text", x = x_pos, y = median_val,
                        label = paste0("Median: ", format(median_val, big.mark = ",")),
                        hjust = -0.1, vjust = 1.5, color = "#0072B2", size = 3.5)
    }
    
    if (add_maximum) {
      max_row <- DT[which.max(get(y_col))]
      p <- p +
        annotate("text", x = max_row[[x_col]], y = max_row[[y_col]],
                 label = paste0("Max: ", format(max_row[[y_col]], big.mark = ",")),
                 vjust = -0.5, hjust = 0.5, size = 3.5, color = "#E69F00")
    }
    
    if (add_minimum) {
      min_row <- DT[which.min(get(y_col))]
      p <- p +
        annotate("text", x = min_row[[x_col]], y = min_row[[y_col]],
                 label = paste0("Min: ", format(min_row[[y_col]], big.mark = ",")),
                 vjust = 1.5, hjust = 0.5, size = 3.5, color = "#D55E00")
    }
  }
  
  # Trendline with optional R² and trend statistics
  if (add_trendline) {
    # Add the trendline
    p <- p + geom_smooth(
      method = trendline_method,
      se = FALSE,
      color = trendline_color,
      linewidth = trendline_size
    )
    
    # Calculate statistics if requested
    if (show_r_squared || show_trend_stats) {
      # Convert x to numeric if needed for regression
      if (inherits(DT[[x_col]], c("Date", "POSIXct"))) {
        x_numeric <- as.numeric(DT[[x_col]])
      } else if (is.factor(DT[[x_col]]) || is.character(DT[[x_col]])) {
        x_numeric <- seq_len(nrow(DT))
      } else {
        x_numeric <- DT[[x_col]]
      }
      
      # Fit linear model
      lm_fit <- lm(DT[[y_col]] ~ x_numeric)
      
      # Build annotation text
      annotation_lines <- character(0)
      
      if (show_r_squared) {
        r_squared <- summary(lm_fit)$r.squared
        annotation_lines <- c(annotation_lines, sprintf("R² = %.3f", r_squared))
      }
      
      if (show_trend_stats) {
        # Calculate percentage change from first to last
        first_val <- DT[[y_col]][1]
        last_val <- DT[[y_col]][nrow(DT)]
        pct_change <- ((last_val - first_val) / first_val) * 100
        
        # Determine if growth or decline
        trend_word <- ifelse(pct_change >= 0, "Growth", "Decline")
        annotation_lines <- c(annotation_lines, 
                              sprintf("%s: %.1f%%", trend_word, abs(pct_change)))
      }
      
      # Combine annotation lines
      annotation_text <- paste(annotation_lines, collapse = "\n")
      
      # Determine position
      x_range <- range(x_numeric, na.rm = TRUE)
      y_range <- range(DT[[y_col]], na.rm = TRUE)
      
      x_pos <- switch(trend_stats_x_pos,
                      "left" = x_range[1] + 0.05 * diff(x_range),
                      "right" = x_range[2] - 0.05 * diff(x_range),
                      "center" = mean(x_range),
                      x_range[1] + 0.05 * diff(x_range))
      
      y_pos <- switch(trend_stats_y_pos,
                      "top" = y_range[2] - 0.05 * diff(y_range),
                      "bottom" = y_range[1] + 0.15 * diff(y_range),
                      y_range[2] - 0.05 * diff(y_range))
      
      hjust_val <- switch(trend_stats_x_pos,
                          "left" = 0,
                          "right" = 1,
                          "center" = 0.5,
                          0)
      
      # Convert x_pos back to original type if needed
      if (inherits(DT[[x_col]], "Date")) {
        x_pos <- as.Date(x_pos, origin = "1970-01-01")
      } else if (inherits(DT[[x_col]], "POSIXct")) {
        x_pos <- as.POSIXct(x_pos, origin = "1970-01-01", tz = attr(DT[[x_col]], "tzone"))
      }
      
      # Add annotation
      p <- p + annotate("text",
                        x = x_pos,
                        y = y_pos,
                        label = annotation_text,
                        hjust = hjust_val,
                        vjust = 1,
                        color = trendline_color,
                        size = 3.5,
                        fontface = "bold")
    }
  }
  
  # Add 3SD line if requested
  if (add_3sd) {
    y_vals <- DT[[y_col]]
    mu <- mean(y_vals, na.rm = TRUE)
    sigma <- sd(y_vals, na.rm = TRUE)
    threshold <- mu + 3 * sigma
    
    p <- p + 
      geom_hline(yintercept = threshold,
                 color = sd_color,
                 linetype = sd_linetype,
                 linewidth = sd_size) +
      annotate("text",
               x = Inf, y = threshold,
               label = sprintf("3SD ≈ %.0f", threshold),
               hjust = 1.1, vjust = -0.5,
               color = sd_color, size = 3)
  }
  
  # Data labels
  if (show_labels) {
    if (is.null(label_col)) {
      label_col <- y_col
    }
    
    if (label_col %in% names(DT)) {
      p <- p + geom_text(
        aes(label = .data[[label_col]]),
        angle = label_angle,
        size = label_size,
        color = label_color,
        vjust = label_vjust,
        hjust = label_hjust
      )
    }
  }
  
  # Extra annotations
  if (!is.null(extra_line)) {
    p <- p + extra_line
  }
  
  # Display plot
  print(p)
  Sys.sleep(3)
  
  # Save plot
  if (!is.null(chart_dir) && !is.null(filename)) {
    if (!dir.exists(chart_dir)) {
      dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    filepath <- file.path(chart_dir, paste0(filename, ".pdf"))
    
    ggsave(
      filename = filepath,
      plot = p,
      width = chart_width,
      height = chart_height
    )
    
    message("File saved: ", filepath)
  }
  
  
  invisible(p)
}
