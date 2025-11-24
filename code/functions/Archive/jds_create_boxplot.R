plot_boxplot <- function(
    DT,                         # MUST be a data.table
    value_col,                  # UNQUOTED numeric column (e.g., duration_sec)
    chart_dir,
    filename     = "boxplot.pdf",
    title        = NULL,
    by_col,                     # OPTIONAL UNQUOTED grouping column (e.g., agency)
    include_na_group = FALSE,
    na_group_label  = "(NA)",
    top_n        = 30L,
    order_by     = c("count","median","median_desc"),
    flip         = FALSE,       # FALSE = vertical
    # appearance / jitter & outlier control
    box_width    = 0.5,
    jitter       = TRUE,
    jitter_width = 0.15,
    jitter_size  = 2,           # ← matches your prior default
    jitter_alpha = 0.85,        # ← matches your prior default
    jitter_shape_inlier = 17,   # ← triangle, as before
    jitter_color_inlier = "#0072B2",  # ← steelblue (as before)
    outlier_size = 1.8,
    outlier_shape = 16,
    outlier_color = "black",
    jitter_sample_inliers = 5000L,  # sample INLIERS only; plot ALL outliers
    x_axis_tick_size   = 15,
    x_axis_label_angle = 0,
    plot_title_size    = 16,
    width_in     = 13,
    height_in    = 8.5,
    coef_iqr     = 1.5
) {
  if (!data.table::is.data.table(DT)) stop("DT must be a data.table.")
  v_expr <- substitute(value_col); if (!is.name(v_expr)) stop("value_col must be unquoted.")
  v_str  <- deparse(v_expr);      if (!v_str %in% names(DT)) stop(sprintf("Column '%s' not found.", v_str))
  for (pkg in c("ggplot2","scales","grid")) if (!requireNamespace(pkg, quietly = TRUE)) stop(sprintf("Package '%s' is required.", pkg))
  if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
  
  df <- data.table::copy(DT)[, val := as.numeric(get(v_str))][!is.na(val)]
  by_given <- !missing(by_col)
  
  if (by_given) {
    g_expr <- substitute(by_col); if (!is.name(g_expr)) stop("by_col must be unquoted.")
    g_str  <- deparse(g_expr);    if (!g_str %in% names(df)) stop(sprintf("Group column '%s' not found.", g_str))
    if (include_na_group) { df[, grp := get(g_str)]; df[is.na(grp), grp := na_group_label] }
    else { df <- df[!is.na(get(g_str))]; df[, grp := get(g_str)] }
    
    order_by <- match.arg(order_by)
    stats <- df[, .(N = .N, med = stats::median(val)), by = grp]
    if (order_by == "count") data.table::setorder(stats, -N, grp)
    else if (order_by == "median") data.table::setorder(stats, med, grp)
    else data.table::setorder(stats, -med, grp)
    keep_levels <- stats$grp[seq_len(min(top_n, nrow(stats)))]
    df <- df[grp %in% keep_levels]
    df[, grp := factor(grp, levels = keep_levels)]
  } else {
    df[, grp := factor("(all)")]
  }
  if (!nrow(df)) { message("plot_boxplot: no data to plot after filtering."); return(invisible(NULL)) }
  
  # 1.5*IQR bounds per group
  bounds <- df[, {
    q1  <- as.numeric(stats::quantile(val, 0.25, na.rm = TRUE))
    q3  <- as.numeric(stats::quantile(val, 0.75, na.rm = TRUE))
    iqr <- q3 - q1
    .(lo = ifelse(is.finite(iqr), q1 - coef_iqr*iqr, -Inf),
      hi = ifelse(is.finite(iqr), q3 + coef_iqr*iqr,  Inf))
  }, by = grp]
  df <- bounds[df, on = "grp"]
  df[, is_outlier := (val < lo) | (val > hi)]
  
  # inliers/outliers (be explicit so data.table treats them as columns)
  inliers  <- df[is_outlier == FALSE]
  outliers <- df[is_outlier == TRUE]
  
  # sample inliers for jitter overlay to keep plots light
  if (isTRUE(jitter) && nrow(inliers) > jitter_sample_inliers) {
    set.seed(42L); inliers <- inliers[sample(.N, jitter_sample_inliers)]
  }
  
  # plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = grp, y = val)) +
    ggplot2::geom_boxplot(
      width = box_width,
      outlier.shape = NA,             # we draw outliers ourselves
      fill = "#E69F00", color = "black",
      alpha = 0.65, linewidth = 0.7
    )
  
  if (isTRUE(jitter)) {
    # inliers: steelblue triangles (your prior style)
    if (nrow(inliers)) {
      p <- p + ggplot2::geom_jitter(
        data = inliers,
        ggplot2::aes(x = grp, y = val),
        width = jitter_width, height = 0,
        size  = jitter_size, alpha = jitter_alpha,
        shape = jitter_shape_inlier, color = jitter_color_inlier
      )
    }
    # outliers: black dots
    if (nrow(outliers)) {
      p <- p + ggplot2::geom_jitter(
        data = outliers,
        ggplot2::aes(x = grp, y = val),
        width = jitter_width, height = 0,
        size  = outlier_size, alpha = 1,
        shape = outlier_shape, color = outlier_color
      )
    }
  }
  
  p <- p +
    ggplot2::labs(
      title = if (is.null(title)) sprintf("Boxplot of %s%s", v_str, if (by_given) " by group" else "") else title,
      x = if (by_given) NULL else "", y = NULL
    ) +
    ggplot2::theme(
      legend.position      = "none",
      panel.grid.major.y   = ggplot2::element_line(color = "white", linewidth = 0.8),
      panel.grid.major.x   = ggplot2::element_line(color = "white", linewidth = 0.8),
      panel.grid.minor     = ggplot2::element_blank(),
      plot.title           = ggplot2::element_text(face = "bold", size = plot_title_size, hjust = 0.5),
      axis.text.x          = ggplot2::element_text(angle = x_axis_label_angle, size = x_axis_tick_size),
      axis.ticks.length    = grid::unit(0.3, "cm"),
      panel.background     = ggplot2::element_rect(fill = "gray97", color = "gray97")
    )
  
  if (isTRUE(flip)) p <- p + ggplot2::coord_flip()
  
  print(p)
  outfile <- file.path(chart_dir, filename)
  ggplot2::ggsave(outfile, plot = p, width = width_in, height = height_in, dpi = 300)
  cat(sprintf("\nSaved chart to: %s\n", outfile))
  
  invisible(list(plot = p, file = outfile, data = df[, .(grp, val, is_outlier)]))
}
