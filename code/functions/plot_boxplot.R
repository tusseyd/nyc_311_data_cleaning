################################################################################
# plot_boxplot.R
# Box plot, violin plot, or both ("hybrid") of a numeric column by group,
# with jittered inliers and outliers.
#
# Sizes come from chart_style() (see chart_style.R). Every size argument
# defaults to NULL, meaning "use the standard size". Flipped (horizontal)
# box plots grow in height with the number of groups.
#
# Requires: chart_style.R and david_theme.R (ggrastr if use_raster = TRUE)
################################################################################

plot_boxplot <- function(
    DT,
    value_col,
    chart_dir,
    filename          = "boxplot.pdf",
    title             = NULL,
    subtitle          = NULL,       # NULL = "n = ..."
    by_col,                         # bare name or "string"; omit for one group
    plot_type         = c("boxplot", "violin", "hybrid"),
    value_label       = "Duration (days)",  # axis title for the value axis
    include_na_group  = FALSE,
    na_group_label    = "(NA)",
    top_n             = 30L,
    order_by          = c("median", "count", "median_desc"),
    flip              = TRUE,
    show_count_labels = TRUE,   # append "(n)" to each group label
    min_count         = 5L,     # affects plotting only
    outlier_threshold = NULL,
    coef_iqr          = 1.5,
    zero_line         = "auto", # TRUE, FALSE, or "auto" (only when data span zero)
    x_scale_type      = c("linear", "pseudo_log", "sqrt_signed"),
    x_limits          = NULL,
    y_axis_side       = c("auto", "left", "right"),

    # --- Violin (plot_type "violin" or "hybrid") ---
    violin_scale  = c("width", "area", "count"),
    violin_fill   = "#A9D8F3",     # opaque light blue (no transparency: prints reliably)
    violin_alpha  = 1,
    violin_trim   = TRUE,          # stop each violin at its data range

    # --- Points ---
    jitter                 = FALSE, # TRUE adds a sample of inlier points (busy; slow to print)
    jitter_width           = 0.15,
    jitter_alpha           = 0.5,
    jitter_shape_inlier    = 17,
    jitter_color_inlier    = "#0072B2",
    jitter_sample_inliers  = 5000L,
    jitter_sample_outliers = NULL,
    outlier_shape          = 16,
    outlier_color          = "gray55",
    outlier_alpha          = 1,
    use_raster             = TRUE,
    raster_dpi             = 300,

    # --- Boxes ---
    box_width      = 0.5,
    box_fill       = "#E69F00",
    print_summary  = TRUE,          # console table of summary statistics

    # --- Size overrides (NULL = standard size from chart_style()) ---
    width_in           = NULL,
    height_in          = NULL,
    jitter_size        = NULL,
    outlier_size       = NULL,
    box_line_width     = NULL,
    x_axis_label_angle = 0,

    # --- Deprecated: accepted so existing calls still run (ignored) ---
    x_axis_tick_size   = NULL,
    y_axis_tick_size   = NULL,
    y_axis_label_size  = NULL,
    plot_title_size    = NULL,
    plot_subtitle_size = NULL,
    count_label_hjust  = NULL
) {

  `%||%` <- function(a, b) if (is.null(a)) b else a
  st <- chart_style()

  # ---- Checks ----
  if (!data.table::is.data.table(DT)) stop("DT must be a data.table.")
  # Columns may be given bare (duration_days), as a string ("duration_days"),
  # or as a variable holding the name
  caller <- parent.frame()
  col_name <- function(expr) {
    if (is.character(expr)) return(expr)
    if (is.name(expr) && deparse(expr) %in% names(DT)) return(deparse(expr))
    val <- tryCatch(eval(expr, caller), error = function(e) NULL)
    if (is.character(val) && length(val) == 1) return(val)
    stop(sprintf("Could not resolve column '%s'.", deparse(expr)), call. = FALSE)
  }
  v_str <- col_name(substitute(value_col))
  if (!v_str %in% names(DT)) stop(sprintf("Column '%s' not found.", v_str))

  plot_type    <- match.arg(plot_type)
  violin_scale <- match.arg(violin_scale)
  if (is.null(jitter)) jitter <- FALSE
  if (use_raster && !requireNamespace("ggrastr", quietly = TRUE)) {
    stop("Package 'ggrastr' is required when use_raster = TRUE.")
  }

  # ---- Prep ----
  df <- data.table::copy(DT)[, val := as.numeric(get(v_str))][!is.na(val)]

  if (!missing(by_col)) {
    g_str  <- col_name(substitute(by_col))
    if (!g_str %in% names(df)) stop(sprintf("Group column '%s' not found.", g_str))
    if (isTRUE(include_na_group)) {
      df[, grp := get(g_str)]; df[is.na(grp), grp := na_group_label]
    } else {
      df <- df[!is.na(get(g_str))]; df[, grp := get(g_str)]
    }
  } else {
    df[, grp := factor("(all)")]
  }
  df_all <- data.table::copy(df)   # unfiltered copy for the summary table

  # ---- Group stats, ordering, filters ----
  ob <- if (missing(order_by) || is.null(order_by)) "median" else tolower(trimws(as.character(order_by)[1]))
  if (!ob %in% c("median", "count", "median_desc")) ob <- "median"

  stats <- df[, .(N = .N, med = stats::median(val)), by = grp]
  if (!is.null(outlier_threshold)) {
    stats <- stats[med >= outlier_threshold]
    cat(sprintf("Filtered to groups with median >= %.3f (for plotting/order).\n", outlier_threshold))
  }
  if (ob == "median")      data.table::setorder(stats, med, grp)
  if (ob == "median_desc") data.table::setorder(stats, -med, grp)
  if (ob == "count")       data.table::setorder(stats, -N, grp)

  stats_plot <- if (min_count > 1L) stats[N >= min_count] else data.table::copy(stats)
  if (min_count > 1L && nrow(stats_plot) < nrow(stats)) {
    cat(sprintf("Filtering plotting data to groups with N >= %d\n", min_count))
  }
  if (!nrow(stats_plot)) {
    warning("No groups remain after plotting filters (min_count/top_n/outlier_threshold). No plot produced.")
    return(invisible(NULL))
  }

  stats_plot  <- stats_plot[seq_len(min(top_n, nrow(stats_plot)))]
  keep_levels <- as.character(stats_plot$grp)

  df_plot <- df[as.character(grp) %in% keep_levels]
  df_plot[, grp := factor(as.character(grp), levels = keep_levels)]
  if (!nrow(df_plot)) {
    warning("No rows in plotting data after filters. No plot produced.")
    return(invisible(NULL))
  }

  # ---- Outlier tagging ----
  bounds <- df_plot[, {
    q1 <- as.numeric(stats::quantile(val, 0.25, na.rm = TRUE))
    q3 <- as.numeric(stats::quantile(val, 0.75, na.rm = TRUE))
    .(lo = q1 - coef_iqr * (q3 - q1), hi = q3 + coef_iqr * (q3 - q1))
  }, by = grp]
  df_plot <- bounds[df_plot, on = "grp"]
  df_plot[, is_outlier := (val < lo) | (val > hi)]

  inliers  <- df_plot[is_outlier == FALSE]
  outliers <- df_plot[is_outlier == TRUE]
  if (isTRUE(jitter) && nrow(inliers) > jitter_sample_inliers) {
    set.seed(42L); inliers <- inliers[sample(.N, jitter_sample_inliers)]
  }
  if (isTRUE(jitter) && !is.null(jitter_sample_outliers) && nrow(outliers) > jitter_sample_outliers) {
    set.seed(43L); outliers <- outliers[sample(.N, jitter_sample_outliers)]
  }

  # ---- Sizes ----
  pt_size  <- jitter_size    %||% st$point_size %||% 0.8
  out_size <- outlier_size   %||% st$point_size %||% 0.8
  box_lw   <- box_line_width %||% st$line_width %||% 0.3
  ref_lw   <- st$ref_line_width %||% 0.5
  w_in     <- width_in  %||% st$width_in
  h_in     <- height_in %||% (if (isTRUE(flip)) auto_height(length(keep_levels), st) else st$height_in)

  if (identical(zero_line, "auto")) {
    rng <- range(df_plot$val, na.rm = TRUE)
    zero_line <- rng[1] < 0 && rng[2] > 0
  }
  
  y_axis_side <- match.arg(y_axis_side)
  if (y_axis_side == "auto") y_axis_side <- if (isTRUE(flip)) "right" else "left"

  # Group labels, optionally with counts: "DPR (56,406)"
  grp_labels <- if (isTRUE(show_count_labels)) {
    stats::setNames(sprintf("%s (%s)", keep_levels,
                            scales::comma(stats_plot$N[match(keep_levels, as.character(stats_plot$grp))])),
                    keep_levels)
  } else {
    stats::setNames(keep_levels, keep_levels)
  }

  # Point layer helper: raster or vector, horizontal or vertical
  add_points <- function(p, data, size, alpha, shape, color) {
    if (!nrow(data)) return(p)
    aes_xy <- if (isTRUE(flip)) ggplot2::aes(x = val, y = grp) else ggplot2::aes(x = grp, y = val)
    if (isTRUE(use_raster)) {
      p + ggrastr::geom_jitter_rast(data = data, mapping = aes_xy,
                                    width  = if (isTRUE(flip)) 0 else jitter_width,
                                    height = if (isTRUE(flip)) jitter_width else 0,
                                    size = size, alpha = alpha, shape = shape, color = color,
                                    raster.dpi = raster_dpi)
    } else {
      p + ggplot2::geom_jitter(data = data, mapping = aes_xy,
                               width  = if (isTRUE(flip)) 0 else jitter_width,
                               height = if (isTRUE(flip)) jitter_width else 0,
                               size = size, alpha = alpha, shape = shape, color = color)
    }
  }

  # ---- Build plot ----
  x_scale_type <- match.arg(x_scale_type)
  value_trans <- switch(
    x_scale_type,
    "pseudo_log"  = "pseudo_log",
    "sqrt_signed" = scales::trans_new("sqrt_signed",
                                      transform = function(x) sign(x) * sqrt(abs(x)),
                                      inverse   = function(x) sign(x) * x^2),
    "identity"
  )

  p <- if (isTRUE(flip)) ggplot2::ggplot(df_plot, ggplot2::aes(x = val, y = grp))
       else                ggplot2::ggplot(df_plot, ggplot2::aes(x = grp, y = val))

  if (plot_type %in% c("violin", "hybrid")) {
    p <- p + ggplot2::geom_violin(orientation = if (isTRUE(flip)) "y" else "x",
                                  scale = violin_scale, trim = violin_trim,
                                  fill = violin_fill, color = "gray40",
                                  alpha = violin_alpha, linewidth = box_lw)
  }
  if (plot_type %in% c("boxplot", "hybrid")) {
    p <- p + ggplot2::geom_boxplot(orientation = if (isTRUE(flip)) "y" else "x",
                                   width = if (plot_type == "hybrid") box_width * 0.3 else box_width,
                                   outlier.shape = NA, fill = box_fill, color = "gray30",
                                   alpha = 1,
                                   linewidth = box_lw)
  }

  if (isTRUE(jitter)) {
    p <- add_points(p, inliers, pt_size, jitter_alpha, jitter_shape_inlier, jitter_color_inlier)
  }
  if (isTRUE(jitter) || plot_type %in% c("boxplot", "hybrid")) {
    p <- add_points(p, outliers, out_size, outlier_alpha, outlier_shape, outlier_color)
  }

  # Powers-of-ten breaks for the pseudo-log scale: ..., -100, -10, -1, 0, 1, 10, 100, ...
  log_breaks <- function(lims) {
    m <- max(abs(lims), 1)
    pw <- 10^(0:ceiling(log10(m)))
    b <- sort(unique(c(-pw, 0, pw)))
    b[b >= lims[1] & b <= lims[2]]
  }
  value_scale_args <- list(trans = value_trans, oob = scales::oob_squish, limits = x_limits,
                           labels = scales::comma,
                           breaks = if (x_scale_type == "pseudo_log") log_breaks else ggplot2::waiver(),
                           expand = ggplot2::expansion(mult = c(0.05, 0.05)))

  if (isTRUE(flip)) {
    if (isTRUE(zero_line)) p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = ref_lw, color = "gray30")
    p <- p +
      do.call(ggplot2::scale_x_continuous, value_scale_args) +
      ggplot2::scale_y_discrete(position = y_axis_side, limits = rev(keep_levels),
                                labels = grp_labels) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::labs(x = value_label, y = NULL)
  } else {
    if (isTRUE(zero_line)) p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = ref_lw, color = "gray30")
    p <- p +
      do.call(ggplot2::scale_y_continuous, c(value_scale_args, list(position = y_axis_side))) +
      ggplot2::scale_x_discrete(labels = grp_labels) +
      ggplot2::labs(x = NULL, y = value_label)
  }

  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title,
                           subtitle = subtitle %||% sprintf("n = %s", scales::comma(nrow(df_plot))))
  }

  p <- p + chart_theme(x_axis_angle = x_axis_label_angle,
                       remove_y_title = isTRUE(flip),
                       remove_x_title = !isTRUE(flip))

  outfile <- save_chart(p, chart_dir, filename, width_in = w_in, height_in = h_in)

  # ---- Summary statistics (from UNFILTERED df_all) ----
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

  if (isTRUE(print_summary)) {
    cat("\n=== Summary statistics (All groups with N > 0)")
    if (!is.null(title)) cat(sprintf(" for %s", title))
    cat(" ===\n")
    print(summary_stats)
  }

  invisible(list(
    plot = p,
    file = outfile,
    data = df_plot[, .(grp, val, is_outlier)],
    summary_stats = summary_stats
  ))
}
