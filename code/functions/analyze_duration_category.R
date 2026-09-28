analyze_duration_category <- function(
    DT,
    label,
    condition_text = "",
    chart_dir,
    threshold_days = NULL,
    show_examples = 10,
    key_col,
    print_cols,
    alt_DT = NULL,
    min_agency_obs = 5,
    make_boxplot = TRUE,
    pareto = TRUE,
    sort_desc = FALSE,
    show_threshold_80_flag = TRUE,
    show_count_labels = TRUE,
    hours_for_days_only = 100,
    hours_for_min_plus_hours = 12
) {
  if (nrow(DT) > 0) {
    nshow <- min(nrow(DT), show_examples)
    # --- Examples header
    cat(sprintf(
      "\n\nExamples of %s durations %s (random %d of %s):\n",
      label,
      condition_text,
      nshow, prettyNum(nrow(DT), big.mark = ",")
    ))
    
    # --- Sample and order
    tmp <- rsample(DT, nshow)
    if (sort_desc) {
      tmp <- tmp[order(-duration_days, created_ts, get(key_col))]
    } else {
      tmp <- tmp[order(duration_days, created_ts, get(key_col))]
    }
    
    # --- Add computed duration columns (for display only)
    tmp[, duration_min   := round(duration_days * 24 * 60, 2)]      # days to minutes
    tmp[, duration_hours := round(duration_days * 24, 2)]           # days to hours
    tmp[, duration_days_display := round(duration_days, 2)]         # rounded days
    
    # --- Decide which duration columns to show
    if (all(abs(tmp$duration_hours) > hours_for_days_only, na.rm = TRUE)) {
      duration_cols <- c("duration_days_display")
    } else if (all(abs(tmp$duration_hours) >= hours_for_min_plus_hours, na.rm = TRUE)) {
      duration_cols <- c("duration_min", "duration_hours")
    } else if (any(abs(tmp$duration_min) > 1 & abs(tmp$duration_hours) < 
                   hours_for_min_plus_hours, na.rm = TRUE)) {
      duration_cols <- c("duration_min")
    } else {
      duration_cols <- c("duration_min", "duration_hours")
    }
    
    # --- Force consistent ordering
    duration_order <- c("duration_min", "duration_hours", "duration_days_display")
    duration_cols <- intersect(duration_order, duration_cols)
    
    # --- Clean base_cols (remove durations + agency from print_cols)
    base_cols <- setdiff(print_cols, c("duration_days", "duration_min",
                                       "duration_hours", "duration_days_display", 
                                       "agency"))
    
    # --- Final display columns
    display_cols <- c(base_cols, duration_cols, "agency")
    
    # --- Print
    print(
      tmp[, ..display_cols],
      row.names = FALSE, right = FALSE
    )
    
    # --- Dataset for charting
    chart_DT <- if (!is.null(alt_DT) && nrow(alt_DT) > 0) alt_DT else DT
    
    
    # Use the correct filter for pareto decisions
    agency_counts <- chart_DT[, .N, by = agency][N > min_agency_obs]
    
    if (pareto && nrow(agency_counts) > 0) {
      cat(sprintf("\nPlotting %s charts (%d agencies with >%d observations)\n",
                  label, nrow(agency_counts), min_agency_obs))
      
      # Normalize label for filenames (handles Unicode & extra spaces)
      base_name <- tolower(trimws(gsub("[^A-Za-z0-9]+", "_", iconv(label, 
                                                                   to = "ASCII//TRANSLIT"))))
      base_name <- gsub("_+", "_", base_name)  # collapse multiple underscores
      base_name <- gsub("^_|_$", "", base_name)  # remove leading/trailing underscores
      
      # Filenames
      pareto_file  <- paste0(base_name, "_duration_pareto_combo_chart.pdf")
      boxplot_file <- paste0("boxplot_", base_name, "_days_by_agency.pdf")
      
      # Titles
      title_case <- tools::toTitleCase(tolower(label))
      plot_title <- if (nzchar(condition_text)) {
        paste0(title_case, " Duration ", condition_text, " SRs by agency")
      } else {
        paste0(title_case, " Duration SRs by agency")
      }
      
      # Pareto chart
      plot_pareto_combo(
        DT        = chart_DT,
        x_col     = agency,
        chart_dir = chart_dir,
        filename  = pareto_file,
        title     = plot_title,
        top_n     = 30,
        min_count = min_agency_obs,
        show_threshold_80 = show_threshold_80_flag,
        show_labels = show_count_labels,
        flip      = FALSE
      )
      
      # Optional boxplot
      if (make_boxplot) {
        # cat("\n>>> Starting boxplot section...\n")
        # cat("make_boxplot is TRUE\n")
        
        # Check if we have enough agencies with min_count >= 5
        agency_counts_plot <- chart_DT[, .N, by = agency][N >= 5]
        if (nrow(agency_counts_plot) == 0) {
          cat("\n*** WARNING: No agencies with >= 5 observations. Boxplot will not be created. ***\n")
          cat("*** Summary statistics will still show all agencies with >", min_agency_obs, "observations ***\n\n")
        } else {
          # cat("Proceeding with boxplot for", nrow(agency_counts_plot), "agencies\n")
        }
        
        # Check if duration_days exists
        if (!"duration_days" %in% names(chart_DT)) {
          cat("ERROR: duration_days column not found in chart_DT!\n")
          cat("Available columns:", paste(names(chart_DT), collapse = ", "), "\n")
        } else {
          # cat("duration_days column exists\n")
          # cat("Range of duration_days:",
          #     min(chart_DT$duration_days, na.rm = TRUE), "to",
          #     max(chart_DT$duration_days, na.rm = TRUE), "\n")
          # cat("NA values in duration_days:", sum(is.na(chart_DT$duration_days)), "\n")
        }
        
        # Compute min and max
        min_val <- min(chart_DT$duration_days, na.rm = TRUE)
        max_val <- max(chart_DT$duration_days, na.rm = TRUE)
        
        # Determine margin based on max_val
        margin <- ifelse(max_val >= 2000, 0.02, 0.1)
        
        # Adjust by margin based on sign
        lower_limit <- ifelse(min_val >= 0, min_val * (1 - margin), 
                              min_val * (1 + margin))
        upper_limit <- ifelse(max_val >= 0, max_val * (1 + margin), 
                              max_val * (1 - margin))
        
        plot_boxplot(
          DT           = chart_DT,
          value_col    = duration_days,
          by_col       = agency,
          chart_dir    = chart_dir,
          filename     = boxplot_file,
          title        = paste0(title_case, " Duration (days) by agency"),
          order_by     = "count",
          x_scale_type = "pseudo_log",
          x_limits     = c(lower_limit, upper_limit),
          show_count_labels = show_count_labels
        )
        # (By-agency violin removed: the box plot above shows the same data.
        #  save_chart() in plot_boxplot() already displays and pauses.)
      }
    } else if (pareto) {
      cat(sprintf("\nSkipping %s charts - no agencies with >%d observations\n",
                  label, min_agency_obs))
    }
    
  } else {
    cat(sprintf("\nNo %s durations found.\n", label))
  }
}