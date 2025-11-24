plot_violin <- function(
    DT,                         # MUST be a data.table
    value_col,                  # UNQUOTED numeric column (e.g., duration_days)
    chart_dir,
    filename     = "violin.pdf",
    title        = NULL,
    by_col,                     # OPTIONAL UNQUOTED grouping column (e.g., agency)
    include_na_group = FALSE,
    na_group_label  = "(NA)",
    top_n        = 30L,
    order_by        = c("median","count","median_desc"),
    flip         = TRUE,        # horizontal by default
    # appearance
    violin_alpha = 0.8,
    violin_fill  = "#E69F00",   # match boxplot scheme
    show_box     = TRUE,        # overlay skinny boxplot for clarity
    box_width    = 0.15,
    x_axis_tick_size   = 15,
    x_axis_label_angle = 0,
    y_axis_tick_size   = 13,
    y_axis_label_size  = NULL,
    y_axis_side        = c("auto","left","right"),
    plot_title_size    = 15,
    plot_subtitle_size = 12,
    width_in     = 13,
    height_in    = 8.5,
    zero_line    = TRUE,
    # scale options
    x_scale_type = c("linear","pseudo_log","sqrt_signed"),
    x_limits     = NULL,
    outlier_threshold = NULL,
    min_count    = 1L
) {
  
  if (!data.table::is.data.table(DT)) stop("DT must be a data.table.")
  v_expr <- substitute(value_col); if (!is.name(v_expr)) stop("value_col must be unquoted.")
  v_str  <- deparse(v_expr);      if (!v_str %in% names(DT)) stop(sprintf("Column '%s' not found.", v_str))
  for (pkg in c("ggplot2","scales")) if (!requireNamespace(pkg, quietly = TRUE)) stop(sprintf("Package '%s' is required.", pkg))
  if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
  
  df <- data.table::copy(DT)[, val := as.numeric(get(v_str))][!is.na(val)]
  by_given <- !missing(by_col)
  
  # ---- GROUPING & ORDERING ----
  keep_levels <- "(all)"
  if (by_given) {
    g_expr <- substitute(by_col); if (!is.name(g_expr)) stop("by_col must be unquoted.")
    g_str  <- deparse(g_expr);    if (!g_str %in% names(df)) stop(sprintf("Group column '%s' not found.", g_str))
    if (include_na_group) { df[, grp := get(g_str)]; df[is.na(grp), grp := na_group_label] }
    else { df <- df[!is.na(get(g_str))]; df[, grp := get(g_str)] }
    
    # normalize order_by
    choices <- c("median","count","median_desc")
    default <- "median"
    ob <- if (missing(order_by) || is.null(order_by) || !length(order_by)) default else order_by
    ob <- tolower(trimws(as.character(ob)[1]))
    if (!nzchar(ob) || !(ob %in% choices)) {
      warning(sprintf("order_by='%s' not recognized; defaulting to '%s'", ob, default))
      ob <- default
    }
    order_by <- ob
    
    # Compute per-group stats
    stats <- df[, .(N = .N, med = stats::median(val)), by = grp]
    if (!is.null(outlier_threshold)) {
      stats <- stats[med >= outlier_threshold]
      cat(sprintf("Filtered to groups with median >= %.3f\n", outlier_threshold))
    }
    if (min_count > 1L) {
      stats <- stats[N >= min_count]
    }
    
    # Order groups
    if (order_by == "median") {
      data.table::setorder(stats, med, grp)
    } else if (order_by == "median_desc") {
      data.table::setorder(stats, -med, grp)
    } else if (order_by == "count") {
      data.table::setorder(stats, -N, grp)
    }
    
    keep_levels <- stats$grp[seq_len(min(top_n, nrow(stats)))]
    df <- df[grp %in% keep_levels]
    df[, grp := factor(as.character(grp), levels = keep_levels)]
    
  } else {
    df[, grp := factor("(all)")]
  }
  
  if (!nrow(df)) { message("plot_violin: no data to plot after filtering."); return(invisible(NULL)) }
  
  # ---- Y-axis side & label size ----
  y_axis_side <- match.arg(y_axis_side)
  if (y_axis_side == "auto") y_axis_side <- if (isTRUE(flip)) "right" else "left"
  label_size <- if (is.null(y_axis_label_size)) y_axis_tick_size else y_axis_label_size
  
  # -------- Build plot --------
  if (isTRUE(flip)) {
    # Horizontal: x = val, y = grp
    p <- ggplot2::ggplot(df, ggplot2::aes(x = val, y = grp)) +
      ggplot2::geom_violin(
        orientation = "y",
        fill = violin_fill, color = "black",
        alpha = violin_alpha, trim = TRUE
      )
    if (isTRUE(show_box)) {
      p <- p + ggplot2::geom_boxplot(
        orientation = "y",
        width = box_width, outlier.shape = NA,
        fill = violin_fill, color = "black", alpha = 0.3
      )
    }
    if (isTRUE(zero_line)) {
      if (is.null(x_limits) || (0 >= x_limits[1] && 0 <= x_limits[2])) {
        p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, color = "black")
      }
    }
    p <- p + ggplot2::scale_y_discrete(
      position = y_axis_side, limits = rev(keep_levels)
    )
    x_scale_type <- match.arg(x_scale_type)
    x_scale <- switch(
      x_scale_type,
      "pseudo_log" = ggplot2::scale_x_continuous(trans = "pseudo_log", limits = x_limits),
      "sqrt_signed" = ggplot2::scale_x_continuous(
        trans = scales::trans_new(
          name      = "sqrt_signed",
          transform = function(x) sign(x) * sqrt(abs(x)),
          inverse   = function(x) sign(x) * x^2
        ),
        limits = x_limits
      ),
      ggplot2::scale_x_continuous(limits = x_limits)
    )
    p <- p + x_scale
    
  } else {
    # Vertical: x = grp, y = val
    p <- ggplot2::ggplot(df, ggplot2::aes(x = grp, y = val)) +
      ggplot2::geom_violin(
        fill = violin_fill, color = "black",
        alpha = violin_alpha, trim = TRUE
      )
    if (isTRUE(show_box)) {
      p <- p + ggplot2::geom_boxplot(
        width = box_width, outlier.shape = NA,
        fill = violin_fill, color = "black", alpha = 0.3
      )
    }
    if (isTRUE(zero_line)) {
      p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, color = "black")
    }
    p <- p + ggplot2::scale_y_continuous(oob = scales::oob_squish, limits = x_limits)
  }
  
  # Theme + title/subtitle
  p <- p + david_theme(
    text_size = label_size,
    plot_title_size = plot_title_size,
    x_axis_angle = x_axis_label_angle,
    remove_x_title = TRUE,
    remove_y_title = TRUE
  )
  if (!is.null(title)) {
    total_n <- nrow(df)
    p <- p +
      ggplot2::ggtitle(title) +
      ggplot2::labs(subtitle = sprintf("n = %s", total_n)) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size = plot_subtitle_size))
  }
  
  print(p)
  Sys.sleep(3)
  
  outfile <- file.path(chart_dir, filename)
  ggplot2::ggsave(outfile, plot = p, width = width_in, height = height_in, dpi = 300)
  
  invisible(list(plot = p, file = outfile, data = df[, .(grp, val)]))
}
