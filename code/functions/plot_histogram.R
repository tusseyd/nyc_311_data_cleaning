################################################################################
# plot_histogram.R
# General histogram of one numeric column, with optional:
#   - fixed bin width (bin_width) or a number of bins (bins)
#   - value range (min_value / max_value) or percentile trimming
#   - log10 x-axis labelled in the original units (log_x), or the older
#     log10-transformed axis (log_transform)
#   - mean and/or median reference lines with labels
#   - overlaid groups (group_col), e.g. NYPD vs other agencies
#   - threshold: bars at or below it highlighted, dashed line + label
#   - count labels on bars
#   - companion cumulative-% chart saved as <filename>_cumulative.pdf
#
# Counts are computed with data.table before plotting, so ggplot only
# receives one row per bin (fast on millions of rows).
#
# Sizes come from chart_style(); size arguments default to NULL.
# Requires: chart_style.R and david_theme.R
################################################################################

plot_histogram <- function(
    DT,
    value_col,
    chart_dir          = "charts",
    filename           = NULL,       # NULL = display only, no file
    title              = NULL,
    subtitle           = NULL,       # NULL = stats line (add_stats) or "n = ..."
    x_label            = NULL,
    y_label            = "Count",

    # --- Binning and range ---
    bins               = 30,         # used when bin_width is NULL
    bin_width          = NULL,       # fixed bin width, in the column's units
                                     # (in log10 units when log_x = TRUE)
    min_value          = NULL,       # keep values >= min_value
    max_value          = NULL,       # keep values <= max_value
    outlier_percentile = 0.95,       # trim above this percentile (1 = no trim);
                                     # ignored when max_value is given
    log_x              = FALSE,      # log10 x-axis, ticks in original units
    log_transform      = FALSE,      # older style: axis shows log10 values
    xlim               = NULL,       # zoom only (does not drop data)

    # --- Mean / median lines ---
    show_mean          = FALSE,
    show_median        = FALSE,
    stat_units         = "",         # e.g. " days", appended to the line labels
    mean_color         = "grey25",
    median_color       = "#D55E00",

    # --- Overlaid groups ---
    group_col          = NULL,       # column name; one histogram per level
    group_colors       = NULL,       # named vector; NULL = Okabe-Ito colours

    # --- Threshold highlight ---
    threshold          = NULL,       # values <= threshold highlighted; line drawn
                                     # at the edge of the last highlighted bar
    threshold_label    = "Threshold",
    highlight_color    = "#D55E00",

    # --- Appearance ---
    fill_color         = "#0072B2",
    alpha              = 0.8,
    bar_outline        = "white",    # thin line between bars; NA = none
    bar_outline_width  = 0.1,        # mm
    add_stats          = TRUE,       # n / mean / median / SD in the subtitle
    add_labels         = FALSE,      # count labels on bars
    x_axis_angle       = 0,
    cumulative         = FALSE,      # also save a cumulative-% chart
    cumulative_ref_pct = 90,         # reference line on the cumulative chart
    print_summary      = TRUE,       # console summary

    # --- Size overrides (NULL = chart_style()) ---
    width_in           = NULL,
    height_in          = NULL,
    label_pt           = NULL,

    # --- Deprecated: accepted so existing calls still run ---
    width      = NULL,               # use width_in
    height     = NULL,               # use height_in
    label_size = NULL,               # ignored; use label_pt
    label_color = "gray25"
) {

  `%||%` <- function(a, b) if (is.null(a)) b else a
  st <- chart_style()
  stopifnot(data.table::is.data.table(DT), value_col %in% names(DT))
  if (!is.null(group_col) && !group_col %in% names(DT))
    stop(sprintf("group_col '%s' not found in DT.", group_col))
  grouped <- !is.null(group_col)

  # ---- Values (and group labels) ----
  d <- data.table::data.table(
    v = as.numeric(DT[[value_col]]),
    g = if (grouped) as.character(DT[[group_col]]) else "all"
  )[!is.na(v)]
  if (grouped) d <- d[!is.na(g)]
  if (!nrow(d)) {
    cat("No non-NA values found in column:", value_col, "\n")
    return(invisible(NULL))
  }
  if (!is.null(min_value)) d <- d[v >= min_value]
  if (!is.null(max_value)) d <- d[v <= max_value]
  if (isTRUE(log_x) && any(d$v <= 0)) {
    message(sprintf("  log_x: dropping %s values <= 0",
                    format(sum(d$v <= 0), big.mark = ",")))
    d <- d[v > 0]
  }
  if (!nrow(d)) {
    message(sprintf("No values of '%s' in the requested range.", value_col))
    return(invisible(NULL))
  }
  values <- d$v

  # Percentile trim (only when no explicit max_value)
  n_trimmed <- 0L
  pd <- d
  if (is.null(max_value) && outlier_percentile < 1) {
    upper <- stats::quantile(values, outlier_percentile, na.rm = TRUE)
    pd <- d[v <= upper]
    n_trimmed <- nrow(d) - nrow(pd)
    if (n_trimmed > 0 && isTRUE(print_summary)) {
      cat(sprintf("Trimmed %s extreme values (%.1f%%) above %.2f percentile\n",
                  prettyNum(n_trimmed, big.mark = ","),
                  100 * n_trimmed / length(values), outlier_percentile))
    }
  }

  # Plotting scale: log10 for log_x / log_transform, else the raw values
  on_log <- isTRUE(log_x) || isTRUE(log_transform)
  if (isTRUE(log_x)) {
    pd <- pd[, .(v = log10(v), g)]
  } else if (isTRUE(log_transform)) {
    if (any(pd$v <= 0)) {
      cat("Warning: log transform requested but data contains values <= 0. Adding 1 to all values.\n")
      pd <- pd[, .(v = log10(v + 1), g)]
    } else {
      pd <- pd[, .(v = log10(v), g)]
    }
  }
  to_plot <- function(x) if (on_log) log10(x) else x   # raw value -> plot position

  # ---- Bin counts (data.table) ----
  lo <- if (!is.null(min_value) && !on_log) min_value else min(pd$v)
  hi <- if (!is.null(max_value) && !on_log) max_value else max(pd$v)
  bw <- bin_width %||% ((hi - lo) / bins)
  if (!is.finite(bw) || bw <= 0) bw <- 1

  hist_data <- pd[, .(count = .N),
                  by = .(group = g, bin_start = lo + floor((v - lo) / bw) * bw)][order(group, bin_start)]
  hist_data[, bin_mid := bin_start + bw / 2]
  thr_pos <- if (is.null(threshold)) NULL else to_plot(threshold)
  hist_data[, highlight := !grouped & !is.null(thr_pos) & bin_start <= (thr_pos %||% -Inf)]
  # Threshold line goes at the right edge of the bin holding the threshold,
  # i.e. between the highlighted and the plain bars. With 1-second bins and
  # threshold = 28, bars 2..28 are highlighted and the line sits at 29.
  thr_line <- if (is.null(thr_pos)) NULL else lo + (floor((thr_pos - lo) / bw) + 1) * bw

  # ---- Labels ----
  if (is.null(title)) {
    title <- paste("Distribution of", value_col)
    if (isTRUE(log_transform)) title <- paste(title, "(Log10 Scale)")
    if (n_trimmed > 0) title <- paste0(title, sprintf(" [%d%% trimmed]", round(100 * outlier_percentile)))
  }
  if (is.null(x_label)) {
    x_label <- if (isTRUE(log_x)) paste0(value_col, " (log scale)")
               else if (isTRUE(log_transform)) paste0("Log10(", value_col, ")") else value_col
  }
  if (is.null(subtitle)) {
    subtitle <- if (grouped) {
      paste(d[, sprintf("%s n = %s", g, scales::comma(.N)), by = g]$V1, collapse = ", ")
    } else if (isTRUE(add_stats)) {
      sprintf("n = %s  Mean = %.2f  Median = %.2f  SD = %.2f",
              scales::comma(length(values)), mean(values), stats::median(values), stats::sd(values))
    } else {
      sprintf("n = %s", scales::comma(nrow(pd)))
    }
  }

  lab_mm <- pt_to_mm(label_pt %||% st$label_pt)
  ann_mm <- pt_to_mm((label_pt %||% st$label_pt) + 0.5)
  ref_lw <- st$ref_line_width %||% 0.5
  w_in   <- width_in  %||% width  %||% st$width_in
  h_in   <- height_in %||% height %||% st$height_in

  # Group colours (Okabe-Ito order)
  if (grouped) {
    lv <- unique(hist_data$group)
    group_colors <- group_colors %||%
      stats::setNames(c("#0072B2", "#009E73", "#E69F00", "#CC79A7", "#56B4E9", "#D55E00")[seq_along(lv)], lv)
  }

  # x axis: powers of ten in original units for log_x
  x_scale <- if (isTRUE(log_x)) {
    brk <- seq(floor(min(pd$v)), ceiling(max(pd$v)))
    ggplot2::scale_x_continuous(
      breaks = brk,
      labels = format(10^brk, big.mark = ",", scientific = FALSE, drop0trailing = TRUE, trim = TRUE),
      expand = ggplot2::expansion(mult = 0.01))
  } else {
    ggplot2::scale_x_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = 0.01))
  }

  # ---- Histogram ----
  if (grouped) {
    p <- ggplot2::ggplot(hist_data, ggplot2::aes(x = bin_mid, y = count, fill = group)) +
      ggplot2::geom_col(width = bw, position = "identity", alpha = alpha,
                        color = bar_outline, linewidth = bar_outline_width) +
      ggplot2::scale_fill_manual(values = group_colors, name = NULL)
  } else {
    p <- ggplot2::ggplot(hist_data, ggplot2::aes(x = bin_mid, y = count, fill = highlight)) +
      ggplot2::geom_col(width = bw, color = bar_outline,
                        linewidth = bar_outline_width, alpha = alpha) +
      ggplot2::scale_fill_manual(values = c(`FALSE` = fill_color, `TRUE` = highlight_color),
                                 guide = "none")
  }
  p <- p + x_scale +
    ggplot2::scale_y_continuous(labels = scales::comma,
                                expand = ggplot2::expansion(mult = c(0, if (isTRUE(show_mean) || isTRUE(show_median)) 0.2 else 0.08))) +
    ggplot2::labs(title = title, subtitle = subtitle, x = x_label, y = y_label) +
    chart_theme(x_axis_angle = x_axis_angle)
  if (grouped) {
    # Legend inside the top-left corner (ggplot2 3.5+ syntax, else the older one)
    legend_pos <- if (utils::packageVersion("ggplot2") >= "3.5.0") {
      ggplot2::theme(legend.position = "inside", legend.position.inside = c(0.01, 0.98))
    } else {
      ggplot2::theme(legend.position = c(0.01, 0.98))
    }
    p <- p + legend_pos + ggplot2::theme(
      legend.justification   = c(0, 1),
      legend.background      = ggplot2::element_rect(fill = "white", color = "grey60",
                                                     linewidth = 0.2),
      legend.key.size        = ggplot2::unit(0.3, "cm"))
  }

  if (isTRUE(add_labels)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = scales::comma(count)),
                                vjust = -0.5, size = lab_mm, color = label_color)
  }

  # ---- Mean / median lines ----
  fmt_stat <- function(x) sprintf("%s%s", format(round(x, 2), big.mark = ",", nsmall = 2), stat_units)
  if (isTRUE(show_mean) || isTRUE(show_median)) {
    stats_dt <- d[, .(mean = mean(v), median = stats::median(v)), by = .(group = g)]
    if (!grouped) {
      # One histogram: mean and median labels on opposite sides of their lines
      m <- stats_dt$mean; md <- stats_dt$median
      mean_right <- m >= md
      if (isTRUE(show_mean)) {
        p <- p +
          ggplot2::geom_vline(xintercept = to_plot(m), color = mean_color,
                              linetype = "dashed", linewidth = ref_lw) +
          ggplot2::annotate("text", x = to_plot(m), y = Inf, label = paste("Mean =", fmt_stat(m)),
                            hjust = if (mean_right) -0.08 else 1.08, vjust = 1.5,
                            size = ann_mm, color = mean_color, fontface = "bold")
      }
      if (isTRUE(show_median)) {
        p <- p +
          ggplot2::geom_vline(xintercept = to_plot(md), color = median_color,
                              linetype = "dotted", linewidth = ref_lw) +
          ggplot2::annotate("text", x = to_plot(md), y = Inf, label = paste("Median =", fmt_stat(md)),
                            hjust = if (mean_right) 1.08 else -0.08, vjust = 1.5,
                            size = ann_mm, color = median_color, fontface = "bold")
      }
    } else {
      # Groups: lines in each group's colour, labels written along the line
      lines <- data.table::rbindlist(list(
        if (isTRUE(show_median)) stats_dt[, .(group, x = to_plot(median), lab = paste("Median:", fmt_stat(median)), lt = "dashed")],
        if (isTRUE(show_mean))   stats_dt[, .(group, x = to_plot(mean),   lab = paste("Mean:",   fmt_stat(mean)),   lt = "dotted")]
      ))
      lines[, vj := 1.5 + 1.6 * (seq_len(.N) - 1) %% 2]   # stagger label heights
      p <- p +
        ggplot2::geom_vline(data = lines, ggplot2::aes(xintercept = x, color = group, linetype = lt),
                            linewidth = ref_lw, show.legend = FALSE) +
        ggplot2::geom_text(data = lines, ggplot2::aes(x = x, y = Inf, label = lab, color = group, vjust = vj),
                           inherit.aes = FALSE, hjust = -0.06, size = ann_mm,
                           fontface = "bold", show.legend = FALSE) +
        ggplot2::scale_color_manual(values = group_colors, guide = "none") +
        ggplot2::scale_linetype_identity()
    }
  }

  if (!is.null(threshold)) {
    ymax <- max(hist_data$count)
    p <- p +
      ggplot2::geom_vline(xintercept = thr_line, color = highlight_color,
                          linewidth = ref_lw, linetype = "dashed", alpha = 0.85) +
      ggplot2::annotate("text", x = thr_line, y = ymax * 0.95, label = threshold_label,
                        hjust = -0.05, size = ann_mm, color = highlight_color)
  }

  if (!is.null(xlim)) p <- p + ggplot2::coord_cartesian(xlim = xlim)

  # ---- Console summary ----
  if (isTRUE(print_summary)) {
    cat(sprintf("\nHistogram for '%s':\n", value_col))
    cat(sprintf("  Total records: %s\n", prettyNum(length(values), big.mark = ",")))
    cat(sprintf("  Min: %.4f\n", min(values)))
    cat(sprintf("  Max: %.4f\n", max(values)))
    cat(sprintf("  Mean: %.4f\n", mean(values)))
    cat(sprintf("  Median: %.4f\n", stats::median(values)))
    cat(sprintf("  SD: %.4f\n", stats::sd(values)))
    if (n_trimmed > 0) {
      cat(sprintf("  Trimmed: %s values (%.1f%%)\n",
                  prettyNum(n_trimmed, big.mark = ","), 100 * n_trimmed / length(values)))
    }
  }

  # ---- Save / display ----
  outfile <- NULL
  if (!is.null(filename)) {
    outfile <- save_chart(p, chart_dir, filename, width_in = w_in, height_in = h_in)
  } else {
    print(p); if ((st$pause_sec %||% 3) > 0) Sys.sleep(st$pause_sec %||% 3)
  }

  # ---- Cumulative companion chart (single histogram only) ----
  p2 <- NULL; outfile2 <- NULL
  if (isTRUE(cumulative) && grouped) {
    message("  cumulative chart is not drawn when group_col is used")
  } else if (isTRUE(cumulative)) {
    cum <- data.table::copy(hist_data)[, cum_pct := 100 * cumsum(count) / sum(count)]
    p2 <- ggplot2::ggplot(cum, ggplot2::aes(x = bin_start + bw, y = cum_pct)) +
      ggplot2::geom_line(color = "#0072B2", linewidth = st$line_width %||% 0.3) +
      ggplot2::geom_point(color = "#0072B2",
                          size = st$line_point_size %||% st$point_size %||% 0.8) +
      ggplot2::geom_hline(yintercept = cumulative_ref_pct, color = "#CC79A7",
                          linetype = "dashed", linewidth = ref_lw) +
      x_scale +
      ggplot2::scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100),
                                  expand = c(0, 0)) +
      ggplot2::labs(title = paste("Cumulative", title),
                    subtitle = sprintf("n = %s | Line at %g%%",
                                       scales::comma(sum(cum$count)), cumulative_ref_pct),
                    x = x_label, y = "Cumulative %") +
      chart_theme(x_axis_angle = x_axis_angle)
    if (!is.null(threshold)) {
      p2 <- p2 +
        ggplot2::geom_vline(xintercept = thr_line, color = highlight_color,
                            linewidth = ref_lw, linetype = "dashed", alpha = 0.85) +
        ggplot2::annotate("text", x = thr_line, y = cumulative_ref_pct, label = threshold_label,
                          hjust = -0.05, vjust = -0.5, size = ann_mm, color = highlight_color)
    }
    if (!is.null(filename)) {
      outfile2 <- save_chart(p2, chart_dir, sub("(\\.pdf)?$", "_cumulative.pdf", filename),
                             width_in = w_in, height_in = h_in)
    }
  }

  invisible(list(
    plot = p, cumulative_plot = p2, data = hist_data,
    stats = list(n = length(values), mean = mean(values),
                 median = stats::median(values), sd = stats::sd(values)),
    n_trimmed = n_trimmed, bin_width = bw,
    files = list(histogram = outfile, cumulative = outfile2)
  ))
}
