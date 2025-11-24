plot_boxplot <- function(
    DT,
    value_col,
    chart_dir,
    filename     = "boxplot.pdf",
    title        = NULL,
    by_col,
    include_na_group = FALSE,
    na_group_label  = "(NA)",
    top_n        = 30L,
    order_by        = c("median","count","median_desc"),
    flip         = TRUE,
    box_width    = 0.5,
    show_count_labels = TRUE,
    count_label_hjust = 0.5,
    jitter       = TRUE,
    jitter_width = 0.15,
    jitter_size  = 2,
    jitter_alpha = 0.5,
    jitter_shape_inlier = 17,
    jitter_color_inlier = "#0072B2",
    use_raster = TRUE,
    raster_dpi = 300,
    outlier_size = 1.8,
    outlier_shape = 16,
    outlier_color = "gray40",
    outlier_alpha = 0.8,
    jitter_sample_inliers = 5000L,
    jitter_sample_outliers = NULL,
    x_axis_tick_size   = 15,
    x_axis_label_angle = 0,
    y_axis_tick_size   = 13,
    y_axis_label_size  = NULL,
    y_axis_side        = c("auto","left","right"),
    plot_title_size    = 15,
    plot_subtitle_size = 12,
    width_in     = 13,
    height_in    = 8.5,
    coef_iqr     = 1.5,
    zero_line    = TRUE,
    x_scale_type = c("linear", "pseudo_log", "sqrt_signed"),
    x_limits     = NULL,
    outlier_threshold = NULL,
    min_count    = 5L               # affects plotting only
) {
  # ---- Checks ----
  # cat("\n=== plot_boxplot: Starting ===\n")
  
  if (!data.table::is.data.table(DT)) stop("DT must be a data.table.")
  v_expr <- substitute(value_col)
  if (!is.name(v_expr)) stop("value_col must be unquoted.")
  v_str  <- deparse(v_expr)
  if (!v_str %in% names(DT)) stop(sprintf("Column '%s' not found.", v_str))
  
  # cat("Input data dimensions:", nrow(DT), "rows x", ncol(DT), "columns\n")
  # cat("Value column:", v_str, "\n")
  
  for (pkg in c("ggplot2","scales","grid")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop(sprintf("Package '%s' is required.", pkg))
  }
  if (use_raster && !requireNamespace("ggrastr", quietly = TRUE)) {
    stop("Package 'ggrastr' is required when use_raster=TRUE.")
  }
  if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- Prep ----
  df <- data.table::copy(DT)[, val := as.numeric(get(v_str))][!is.na(val)]
  # cat("After removing NA values:", nrow(df), "rows\n")
  
  by_given <- !missing(by_col)
  if (by_given) {
    g_expr <- substitute(by_col); if (!is.name(g_expr)) stop("by_col must be unquoted.")
    g_str  <- deparse(g_expr);    if (!g_str %in% names(df)) stop(sprintf("Group column '%s' not found.", g_str))
    # cat("Group column:", g_str, "\n")
    
    if (isTRUE(include_na_group)) { df[, grp := get(g_str)]; df[is.na(grp), grp := na_group_label] }
    else { df <- df[!is.na(get(g_str))]; df[, grp := get(g_str)] }
  } else {
    df[, grp := factor("(all)")]
  }
  
  # cat("Number of unique groups:", length(unique(df$grp)), "\n")
  
  # Keep a full, unfiltered copy for tables
  df_all <- data.table::copy(df)
  
  # ---- Group stats for ordering / filtering (on df) ----
  choices <- c("median","count","median_desc")
  ob <- if (missing(order_by) || is.null(order_by)) "median" else tolower(trimws(as.character(order_by)[1]))
  if (!ob %in% choices) ob <- "median"
  
  # cat("\nCalculating group statistics...\n")
  stats <- df[, .(N = .N, med = stats::median(val)), by = grp]
  # cat("Groups before filtering:\n")
  # print(stats[order(-N)][1:min(10, nrow(stats))])
  
  # Optional group threshold by median
  if (!is.null(outlier_threshold)) {
    stats <- stats[med >= outlier_threshold]
    cat(sprintf("Filtered to groups with median >= %.3f (for plotting/order).\n", outlier_threshold))
  }
  
  # Sort groups
  if (ob == "median")      data.table::setorder(stats, med, grp)
  if (ob == "median_desc") data.table::setorder(stats, -med, grp)
  if (ob == "count")       data.table::setorder(stats, -N, grp)
  
  # Apply plotting-only min_count filter and top_n
  # cat(sprintf("\nApplying min_count filter: N >= %d\n", min_count))
  if (min_count > 1L) {
    stats_plot <- stats[N >= min_count]
    if (nrow(stats_plot) < nrow(stats)) {
      cat(sprintf("Filtering plotting data to groups with N >= %d\n", min_count))
      # cat(sprintf("  Before filter: %d groups\n", nrow(stats)))
      # cat(sprintf("  After filter:  %d groups\n", nrow(stats_plot)))
      # cat(sprintf("  Removed:       %d groups\n", nrow(stats) - nrow(stats_plot)))
    }
  } else {
    stats_plot <- data.table::copy(stats)
  }
  
  if (!nrow(stats_plot)) {
    warning("No groups remain after plotting filters (min_count/top_n/outlier_threshold). No plot produced.")
    # cat("*** RETURNING NULL - No groups passed the min_count filter ***\n")
    # cat("=== plot_boxplot: Aborted (no groups) ===\n\n")
    return(invisible(NULL))
  }
  
  # cat(sprintf("\nApplying top_n limit: keeping top %d groups\n", top_n))
  keep_levels <- stats_plot$grp[seq_len(min(top_n, nrow(stats_plot)))]
  # cat(sprintf("Groups to plot: %d\n", length(keep_levels)))
  
  # ---- Build plotting df (df_plot) ----
  df_plot <- df[grp %in% keep_levels]
  # lock factor order
  df_plot[, grp := factor(as.character(grp), levels = keep_levels)]
  
  # cat(sprintf("Rows in plotting data: %d\n", nrow(df_plot)))
  
  if (!nrow(df_plot)) {
    warning("No rows in plotting data after filters. No plot produced.")
    # cat("*** RETURNING NULL - No rows in plotting data ***\n")
    # cat("=== plot_boxplot: Aborted (no rows) ===\n\n")
    return(invisible(NULL))
  }
  
  # ---- Outlier tagging for plotting df ----
  # cat("\nTagging outliers...\n")
  bounds <- df_plot[, {
    q1  <- as.numeric(stats::quantile(val, 0.25, na.rm = TRUE))
    q3  <- as.numeric(stats::quantile(val, 0.75, na.rm = TRUE))
    iqr <- q3 - q1
    .(lo = q1 - coef_iqr*iqr, hi = q3 + coef_iqr*iqr)
  }, by = grp]
  
  df_plot <- bounds[df_plot, on = "grp"]
  df_plot[, is_outlier := (val < lo) | (val > hi)]
  
  inliers  <- df_plot[is_outlier == FALSE]
  outliers <- df_plot[is_outlier == TRUE]
  
  # cat(sprintf("  Inliers:  %d\n", nrow(inliers)))
  # cat(sprintf("  Outliers: %d\n", nrow(outliers)))
  
  # Sample inliers/outliers if large
  if (isTRUE(jitter) && nrow(inliers) > jitter_sample_inliers) {
    set.seed(42L); inliers <- inliers[sample(.N, jitter_sample_inliers)]
    # cat(sprintf("  Sampled inliers down to %d\n", jitter_sample_inliers))
  }
  if (isTRUE(jitter) && !is.null(jitter_sample_outliers) && nrow(outliers) > jitter_sample_outliers) {
    set.seed(43L); outliers <- outliers[sample(.N, jitter_sample_outliers)]
    # cat(sprintf("  Sampled outliers down to %d\n", jitter_sample_outliers))
  }
  
  # ---- Axis side / label size ----
  y_axis_side <- match.arg(y_axis_side)
  if (y_axis_side == "auto") y_axis_side <- if (isTRUE(flip)) "right" else "left"
  label_size <- if (is.null(y_axis_label_size)) y_axis_tick_size else y_axis_label_size
  
  # ---- Build plot ----
  # cat("\nBuilding plot...\n")
  x_scale_type <- match.arg(x_scale_type)
  if (isTRUE(flip)) {
    # Horizontal: x = val, y = grp
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = val, y = grp)) +
      ggplot2::geom_boxplot(
        orientation   = "y",
        width         = box_width,
        outlier.shape = NA,
        fill          = "#E69F00",
        color         = "gray30",
        alpha         = 0.8,
        linewidth     = 0.7
      )
    
    # Inliers
    if (isTRUE(jitter) && nrow(inliers)) {
      if (isTRUE(use_raster)) {
        p <- p + ggrastr::geom_point_rast(
          data = inliers,
          mapping = ggplot2::aes(x = val, y = grp),
          size  = jitter_size, alpha = jitter_alpha,
          shape = jitter_shape_inlier, color = jitter_color_inlier,
          raster.dpi = raster_dpi
        )
      } else {
        p <- p + ggplot2::geom_jitter(
          data = inliers,
          mapping = ggplot2::aes(x = val, y = grp),
          width = 0, height = jitter_width,
          size  = jitter_size, alpha = jitter_alpha,
          shape = jitter_shape_inlier, color = jitter_color_inlier
        )
      }
    }
    
    # Outliers — flip case
    if (nrow(outliers)) {
      if (isTRUE(use_raster)) {
        p <- p + ggrastr::geom_point_rast(
          data = outliers,
          mapping = ggplot2::aes(x = val, y = grp),
          size  = outlier_size,
          alpha = outlier_alpha,
          shape = outlier_shape,
          color = outlier_color,
          raster.dpi = raster_dpi
        )
      } else {
        p <- p + ggplot2::geom_jitter(
          data  = outliers,
          mapping = ggplot2::aes(x = val, y = grp),
          width = 0, height = jitter_width,
          size  = outlier_size,
          alpha = outlier_alpha,
          shape = outlier_shape,
          color = outlier_color
        )
      }
    }
    
    
    # Optional zero line
    if (isTRUE(zero_line)) {
      p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, linetype = "solid", color = "gray30")
    }
    
    # Scale and coord
    x_scale <- switch(
      x_scale_type,
      "pseudo_log" = ggplot2::scale_x_continuous(trans = "pseudo_log", expand = ggplot2::expansion(mult = c(0.05,0.05)),
                                                 oob = scales::oob_squish, limits = x_limits),
      "sqrt_signed" = ggplot2::scale_x_continuous(
        trans = scales::trans_new("sqrt_signed",
                                  transform = function(x) sign(x)*sqrt(abs(x)),
                                  inverse   = function(x) sign(x)*x^2),
        expand = ggplot2::expansion(mult = c(0.05,0.05)),
        oob = scales::oob_squish, limits = x_limits
      ),
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05,0.05)),
                                  oob = scales::oob_squish, limits = x_limits)
    )
    p <- p + x_scale +
      ggplot2::scale_y_discrete(position = y_axis_side, limits = rev(keep_levels)) +
      ggplot2::coord_cartesian(clip = "off")
    
  } else {
    # Vertical: x = grp, y = val
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = grp, y = val)) +
      ggplot2::geom_boxplot(
        width = box_width,
        outlier.shape = NA,
        fill = "#E69F00", color = "gray40",
        alpha = 0.85, linewidth = 0.7
      )
    if (isTRUE(jitter) && nrow(inliers)) {
      p <- p + ggplot2::geom_jitter(
        data = inliers,
        mapping = ggplot2::aes(x = grp, y = val),
        width = jitter_width, height = 0,
        size  = jitter_size, alpha = jitter_alpha,
        shape = jitter_shape_inlier, color = jitter_color_inlier
      )
    }
    if (nrow(outliers)) {
      p <- p + ggplot2::geom_jitter(
        data = outliers,
        mapping = ggplot2::aes(x = grp, y = val),
        width = jitter_width, height = 0,
        size  = outlier_size, alpha = outlier_alpha,
        shape = outlier_shape, color = outlier_color
      )
    }
    if (isTRUE(zero_line)) {
      p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, linetype = "solid", color = "gray40")
    }
    p <- p + ggplot2::scale_y_continuous(position = y_axis_side, oob = scales::oob_squish, limits = x_limits)
  }
  
  # In your plot_boxplot function, find the section where you apply the theme
  # Around line 280-295, modify the theme section:
  
  # Theme and titles
  if (!is.null(title)) {
    total_n <- nrow(df_plot)
    p <- p + ggplot2::ggtitle(title) +
      ggplot2::labs(subtitle = sprintf("n = %s", format(total_n, big.mark = ",")))
  }
  
  # Apply comprehensive theme
  p <- p + ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = plot_title_size, face = "bold", hjust = 0.5),  # Changed hjust from 0 to 0.5
      plot.subtitle = ggplot2::element_text(size = plot_subtitle_size, hjust = 0.5),  # Changed hjust from 0 to 0.5
      axis.text.x = ggplot2::element_text(size = x_axis_tick_size, angle = x_axis_label_angle, hjust = if(x_axis_label_angle != 0) 1 else 0.5),
      axis.text.y = ggplot2::element_text(size = y_axis_tick_size),
      axis.title.x = ggplot2::element_text(size = label_size),  # Added explicit x-axis title
      axis.title.y = ggplot2::element_blank(),  # ADDED: Remove y-axis label
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none"
    )
  
  # Save
  # cat("\nSaving plot...\n")
  outfile <- file.path(chart_dir, filename)
  # cat("Output file:", outfile, "\n")
  
  # tryCatch({
  ggplot2::ggsave(filename = outfile, plot = p, width = width_in, height = height_in, dpi = 300)
  
  print(p)
  Sys.sleep(3)
  
  # cat("Plot saved successfully\n")
  
  # if (file.exists(outfile)) {
  #   file_size <- file.info(outfile)$size
  #   cat(sprintf("Confirmed: File exists (%.1f KB)\n", file_size / 1024))
  # } else {
  #   cat("WARNING: File does not exist after ggsave!\n")
  # }
  # }, error = function(e) {
  #   cat("ERROR during ggsave:", e$message, "\n")
  #   stop(e)
  # })
  
  # ---- Summary statistics (from UNFILTERED df_all) ----
  # cat("\nCalculating summary statistics from UNFILTERED data...\n")
  summary_stats <- df_all[, .(
    N      = .N,
    Min    = round(min(val, na.rm = TRUE), 2),
    Q1     = round(stats::quantile(val, 0.25, na.rm = TRUE, type = 7), 2),
    Median = round(stats::median(val, na.rm = TRUE), 2),
    Q3     = round(stats::quantile(val, 0.75, na.rm = TRUE, type = 7), 2),
    Max    = round(max(val, na.rm = TRUE), 2),
    Mean   = round(mean(val, na.rm = TRUE), 2),
    SD     = round(stats::sd(val, na.rm = TRUE), 2)
  ), by = grp][order(grp)]
  
  cat("\n=== Summary statistics (All groups with N > 0)")
  if (!is.null(title)) cat(sprintf(" for %s", title))
  cat(" ===\n")
  # cat(sprintf("Total groups in summary: %d\n", nrow(summary_stats)))
  # cat(sprintf("Groups plotted: %d\n", length(keep_levels)))
  print(summary_stats)
  
  # cat("\n=== plot_boxplot: Complete ===\n\n")
  

  
  invisible(list(
    plot = p,
    file = outfile,
    data = df_plot[, .(grp, val, is_outlier)],
    summary_stats = summary_stats
  ))
}