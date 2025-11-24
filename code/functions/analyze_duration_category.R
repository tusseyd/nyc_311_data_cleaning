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
    min_agency_obs = 1,
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
      tmp <- tmp[order(-duration_sec, created_ts, get(key_col))]
    } else {
      tmp <- tmp[order(duration_sec, created_ts, get(key_col))]
    }
    
    # --- Add computed duration columns
    tmp[, duration_min   := round(duration_sec / 60, 2)]
    tmp[, duration_hours := round(duration_sec / 3600, 2)]
    tmp[, duration_days  := round(duration_days, 2)]
    tmp[, duration_sec := as.numeric(duration_sec)]
    
    # --- Decide which duration columns to show
    if (all(abs(tmp$duration_hours) > hours_for_days_only, na.rm = TRUE)) {
      duration_cols <- c("duration_days")
    } else if (all(abs(tmp$duration_hours) >= hours_for_min_plus_hours, na.rm = TRUE)) {
      duration_cols <- c("duration_min", "duration_hours")
    } else if (any(abs(tmp$duration_sec) > 60 & abs(tmp$duration_hours) < 
                   hours_for_min_plus_hours, na.rm = TRUE)) {
      duration_cols <- c("duration_min")
    } else {
      duration_cols <- c("duration_sec", "duration_min")
    }
    
    # --- Force consistent ordering
    duration_order <- c("duration_sec", "duration_min", "duration_hours", 
                        "duration_days")
    duration_cols <- intersect(duration_order, duration_cols)
    
    # --- Clean base_cols (remove durations + agency from print_cols)
    base_cols <- setdiff(print_cols, c("duration_sec", "duration_min",
                                       "duration_hours", "duration_days", 
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
    
    # # DEBUG: Print info about chart_DT
    # cat("\n=== DEBUG INFO ===\n")
    # cat("Total rows in chart_DT:", nrow(chart_DT), "\n")
    # cat("Columns in chart_DT:", paste(names(chart_DT), collapse = ", "), "\n")
    
    # Calculate agency counts for summary stats (using min_agency_obs)
    # agency_counts_summary <- chart_DT[, .N, by = agency][N > min_agency_obs]
    # cat("\nAgencies with > min_agency_obs (", min_agency_obs, ") for SUMMARY STATS:\n")
    # print(agency_counts_summary[order(-N)])
    
    # Calculate agency counts for plotting (using min_count = 5)
    # agency_counts_plot <- chart_DT[, .N, by = agency][N >= 5]
    # cat("\nAgencies with >= 5 observations for BOXPLOT:\n")
    # print(agency_counts_plot[order(-N)])
    
    # cat("\nComparison:")
    # cat("\n  - Agencies in summary stats:", nrow(agency_counts_summary))
    # cat("\n  - Agencies that will be plotted:", nrow(agency_counts_plot))
    # cat("\n==================\n\n")
    
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
      # cat("\n>>> Calling plot_pareto_combo...\n")
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
      # cat(">>> plot_pareto_combo completed\n")
      Sys.sleep(3)
      
      # Optional boxplot
      if (make_boxplot) {
        # cat("\n>>> Starting boxplot section...\n")
        # cat("make_boxplot is TRUE\n")
        
        # Check if we have enough agencies with min_count >= 5
        # agency_counts_plot <- chart_DT[, .N, by = agency][N >= 5]
        # if (nrow(agency_counts_plot) == 0) {
        #   cat("\n*** WARNING: No agencies with >= 5 observations. Boxplot will not be created. ***\n")
        #   cat("*** Summary statistics will still show all agencies with >", min_agency_obs, "observations ***\n\n")
        # } else {
        #   cat("Proceeding with boxplot for", nrow(agency_counts_plot), "agencies\n")
        # }
        
        # Check if duration_days exists
        # if (!"duration_days" %in% names(chart_DT)) {
        #   cat("ERROR: duration_days column not found in chart_DT!\n")
        #   cat("Available columns:", paste(names(chart_DT), collapse = ", "), "\n")
        # } else {
        #   cat("duration_days column exists\n")
        #   cat("Range of duration_days:", 
        #       min(chart_DT$duration_days, na.rm = TRUE), "to",
        #       max(chart_DT$duration_days, na.rm = TRUE), "\n")
        #   cat("NA values in duration_days:", sum(is.na(chart_DT$duration_days)), "\n")
        # }
        
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
        
        # cat("Calculated limits: lower =", lower_limit, ", upper =", upper_limit, "\n")
        
        # Determine if we need right-justified labels (for negative data)
        label_hjust <- if (max_val <= 0) 0 else 1 
        
        # cat("\n>>> Calling plot_boxplot...\n")
        # cat("Parameters being sent to plot_boxplot:\n")
        # cat("  - chart_dir:", chart_dir, "\n")
        # cat("  - filename:", boxplot_file, "\n")
        # cat("  - min_count: 5 (FIXED - will filter to agencies with >= 5 observations)\n")
        # cat("  - top_n: 30\n")
        # cat("  - Number of rows in data:", nrow(chart_DT), "\n")
        # cat("  - Number of unique agencies:", length(unique(chart_DT$agency)), "\n")
        # agency_counts_plot <- chart_DT[, .N, by = agency][N >= 5]
        # cat("  - Expected number of agencies in plot:", min(nrow(agency_counts_plot), 30), "\n")
        
        # tryCatch({
        plot_result <- plot_boxplot(
          DT        = chart_DT,
          value_col = duration_days,
          by_col    = agency,
          chart_dir = chart_dir,
          filename  = boxplot_file,
          title     = paste0(title_case, " Duration (days) by agency"),
          top_n     = 30,
          y_axis_tick_size = 10,
          order_by  = "count",
          flip      = TRUE,
          x_scale_type = "pseudo_log",
          x_limits = c(lower_limit, upper_limit),
          min_count = 5,  # FIXED: was min_agency_obs (which defaults to 1)
          jitter_size = 1.3,
          jitter_alpha = 0.55,
          outlier_size = 1.4,
          count_label_hjust = label_hjust,
          show_count_labels = show_count_labels  
        )
        
        # Display boxplot in RStudio
        if (!is.null(plot_result) && !is.null(plot_result$plot)) {
          print(plot_result$plot)
          Sys.sleep(3)
        }
        
        # if (is.null(plot_result)) {
        #   cat(">>> plot_boxplot returned NULL - likely no groups passed the filters\n")
        # } else {
        #   cat(">>> plot_boxplot completed successfully\n")
        #   cat(">>> Output file:", plot_result$file, "\n")
        #   if (file.exists(plot_result$file)) {
        #     cat(">>> Confirmed: PDF file exists\n")
        #   } else {
        #     cat(">>> WARNING: PDF file was not created!\n")
        #   }
        # }
        # }, error = function(e) {
        #   cat("ERROR in plot_boxplot:", e$message, "\n")
        #   cat("Full error:\n")
        #   print(e)
        # })
        
        # Before calling plot_violin_boxplot, assign to a simple name
        violin_data <- chart_DT
        
        # Compute min and max
        min_val <- min(violin_data$duration_days, na.rm = TRUE)
        max_val <- max(violin_data$duration_days, na.rm = TRUE)
        
        # Determine margin based on max_val
        margin <- ifelse(max_val >= 2000, 0.04, 0.07)
        
        # Adjust by margin based on sign
        lower_limit <- ifelse(min_val >= 0, min_val * (1 - margin), 
                              min_val * (1 + margin))
        upper_limit <- ifelse(max_val >= 0, max_val * (1 + margin), 
                              max_val * (1 - margin))
        
        # cat("\n>>> Calling plot_violin_boxplot...\n")
        # tryCatch({
        violin_result <- plot_violin_boxplot(
          DT               = violin_data,
          value_col        = duration_days,
          chart_dir        = chart_dir,
          filename         = gsub("boxplot", "violin_boxplot", boxplot_file),
          title     = paste0(title_case, 
                                  " Duration (days) by agency - Violin chart"),
          by_col           = agency,
          include_na_group = FALSE,
          top_n            = 30L,
          order_by         = "count",
          flip             = TRUE,
          plot_type        = "hybrid",
          violin_alpha     = 0.6,
          violin_trim      = FALSE,
          zero_line        = TRUE,
          x_scale_type     = "pseudo_log",
          y_axis_side      = "left",
          x_limits = c(lower_limit, upper_limit),
          y_axis_label_size = 12,
          y_axis_tick_size = 13,
          x_axis_tick_size = 15,
          plot_title_size  = 14,
          min_count        = 5L
        )
        
        # Display violin plot in RStudio
        if (!is.null(violin_result) && !is.null(violin_result$plot)) {
          print(violin_result$plot)
          Sys.sleep(3)
        }
        
        # if (is.null(violin_result)) {
        #   cat(">>> plot_violin_boxplot returned NULL\n")
        # } else {
        #   cat(">>> plot_violin_boxplot completed successfully\n")
        #   if (!is.null(violin_result$file) && file.exists(violin_result$file)) {
        #     cat(">>> Confirmed: Violin PDF file exists\n")
        #   }
        # }
        # }, error = function(e) {
        #   cat("ERROR in plot_violin_boxplot:", e$message, "\n")
        #   cat("Full error:\n")
        #   print(e)
        # })
        
      } # else {
      #   cat("\nmake_boxplot is FALSE - skipping boxplot generation\n")
      # }
    } else if (pareto) {
      cat(sprintf("\nSkipping %s charts - no agencies with >%d observations\n",
                  label, min_agency_obs))
    }
    
  } else {
    cat(sprintf("\nNo %s durations found.\n", label))
  }
}