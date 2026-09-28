################################################################################
# plot_barchart.R
# Bar chart with optional mean / median / 3-sigma lines, trendline, and
# data labels.
#
# Sizes come from chart_style() (see chart_style.R). Every size argument
# defaults to NULL, meaning "use the standard size". Pass a value only when
# one chart needs to differ from the rest.
#
# Requires: chart_style.R and david_theme.R
################################################################################

plot_barchart <- function(
    DT,
    x_col,
    y_col,

    # Titles and labels
    title    = NULL,
    subtitle = NULL,
    x_label  = "",
    y_label  = "",

    # Appearance
    fill_color  = "#009E73",
    fill_col    = NULL,           # column name for conditional coloring
    fill_colors = NULL,           # named vector of colors for fill_col levels
    bar_width   = 0.8,

    # Data summary and printing
    console_print_title = NULL,
    rows_to_print       = 20,
    show_summary        = TRUE,

    # Statistical annotations
    add_mean    = FALSE,
    add_median  = FALSE,
    add_maximum = FALSE,
    add_minimum = FALSE,
    add_3sd     = FALSE,
    ref_line_band = 0.10,         # draw the 3-sigma line only if the tallest bar
                                  # reaches within this fraction of it (NULL = always)
    mean_color    = "#D55E00",
    mean_linetype = "dotted",
    sd_color      = "#D55E00",
    sd_linetype   = "dashed",

    # Data labels
    show_labels = TRUE,
    label_col   = NULL,
    label_angle = 0,
    label_color = "black",
    label_vjust = -0.5,
    label_hjust = NULL,           # default: 0.5 for horizontal labels, -0.1 when angled

    # Trendline
    add_trendline     = FALSE,
    trendline_color   = "#D55E00",
    trendline_method  = "lm",
    show_r_squared    = FALSE,
    show_trend_stats  = FALSE,
    trend_stats_x_pos = "left",   # "left", "right", "center"
    trend_stats_y_pos = "top",    # "top", "bottom"
    trend_filter_col  = NULL,     # logical column: fit trend/growth only where TRUE

    # Extra annotations
    extra_line = NULL,

    # Axis customization
    x_label_every = 1,
    x_breaks      = NULL,
    x_axis_angle  = 0,
    y_axis_labels = scales::comma,
    sort_by_count = FALSE,

    # --- Size overrides (NULL = standard size from chart_style()) ---
    width_in        = NULL,
    height_in       = NULL,
    label_pt        = NULL,       # bar labels, points
    annotation_pt   = NULL,       # mean/median/max/min/3-sigma/trend text, points
    line_width      = NULL,       # trendline (mm)
    ref_line_width  = NULL,       # mean/median/3-sigma lines (mm)

    # --- Deprecated: accepted so existing calls still run ---
    chart_width  = NULL,          # use width_in
    chart_height = NULL,          # use height_in
    text_size    = NULL,          # ignored; text sizes come from chart_style()
    label_size   = NULL,          # ignored; use label_pt
    x_axis_size  = NULL,          # ignored
    mean_size = NULL, sd_size = NULL, trendline_size = NULL,
    mean_text_size = NULL, median_text_size = NULL,
    sd_text_size = NULL, trend_stats_size = NULL,

    # Save options
    chart_dir = NULL,
    filename  = NULL
) {

  `%||%` <- function(a, b) if (is.null(a)) b else a
  st <- chart_style()

  # --- Resolve sizes: explicit value, then deprecated alias, then style ---
  w_in     <- width_in  %||% chart_width  %||% st$width_in
  h_in     <- height_in %||% chart_height %||% st$height_in
  lab_mm   <- pt_to_mm(label_pt %||% st$label_pt)
  ann_mm   <- pt_to_mm(annotation_pt %||% (st$label_pt + 0.5))
  data_lw  <- line_width     %||% trendline_size %||% st$line_width %||% 0.3
  ref_lw   <- ref_line_width %||% st$ref_line_width %||% 0.5
  lab_hjust <- label_hjust %||% (if (label_angle == 0) 0.5 else -0.1)

  # Ensure data.table (work on a copy)
  DT <- if (data.table::is.data.table(DT)) data.table::copy(DT) else data.table::as.data.table(DT)
  stopifnot(x_col %in% names(DT), y_col %in% names(DT))

  # Percentage and cumulative percentage (for the console summary)
  DT[, percentage := round(get(y_col) / sum(get(y_col), na.rm = TRUE) * 100, 2)]
  DT[, cumulative_percentage := round(cumsum(get(y_col)) / sum(get(y_col), na.rm = TRUE) * 100, 2)]

  # --- Console output ---
  if (show_summary) {
    if (is.null(console_print_title)) {
      console_print_title <- paste("Data Summary for", x_col, "vs", y_col)
    }
    rows_to_show <- min(nrow(DT), rows_to_print)
    cat("\n", console_print_title, " (first ", rows_to_show, " rows):\n", sep = "")
    print(DT[1:rows_to_show, c(x_col, y_col, "percentage", "cumulative_percentage"), with = FALSE],
          row.names = FALSE)

    if ("year" %in% names(DT)) {
      cat("\nSummary by Year (from 'year' column):\n")
      year_summary <- DT[, .(N = sum(get(y_col), na.rm = TRUE)), by = year][order(year)]
      year_summary[, pct := round(100 * N / sum(N), 2)]
      print(year_summary)
    } else if (inherits(DT[[x_col]], c("Date", "POSIXct"))) {
      cat("\nSummary by Year (derived from x_col):\n")
      DT[, year_tmp := data.table::year(get(x_col))]
      year_summary <- DT[, .(N = sum(get(y_col), na.rm = TRUE)), by = year_tmp][order(year_tmp)]
      year_summary[, pct := round(100 * N / sum(N), 2)]
      data.table::setnames(year_summary, "year_tmp", "year")
      print(year_summary)
    }
  }

  # --- Sorting ---
  if (sort_by_count) {
    if (inherits(DT[[x_col]], c("Date", "POSIXct"))) {
      data.table::setorderv(DT, c(y_col, x_col), order = c(-1, 1))
    } else {
      data.table::setorderv(DT, y_col, order = -1)
    }
  } else {
    data.table::setorderv(DT, x_col)
  }

  # --- X-axis scale by data type ---
  x_scale <- if (inherits(DT[[x_col]], "Date")) {
    date_range <- range(DT[[x_col]], na.rm = TRUE)
    date_span  <- as.numeric(date_range[2] - date_range[1])
    n_points   <- nrow(DT)
    is_monthly <- all(data.table::mday(DT[[x_col]]) == 1, na.rm = TRUE) && n_points >= 12

    if (is_monthly) {
      break_interval <- paste(x_label_every, "months"); date_labels <- "%Y-%m"
    } else if (date_span <= 400 && n_points <= 366) {
      break_interval <- paste(x_label_every, "days");   date_labels <- "%m-%d"
    } else if (n_points <= 10 && date_span > 1000) {
      break_interval <- paste(x_label_every, "years");  date_labels <- "%Y"
    } else {
      break_interval <- paste(x_label_every, "days");   date_labels <- "%Y-%m-%d"
    }
    ggplot2::scale_x_date(expand = c(0.01, 0),
                          labels = scales::date_format(date_labels),
                          breaks = scales::date_breaks(break_interval))

  } else if (inherits(DT[[x_col]], c("POSIXct", "POSIXt"))) {
    ggplot2::scale_x_datetime(expand = c(0.01, 0),
                              labels = scales::date_format("%H:%M"),
                              breaks = scales::date_breaks(paste(x_label_every, "hours")))

  } else if (is.numeric(DT[[x_col]])) {
    x_range <- range(DT[[x_col]], na.rm = TRUE)
    ggplot2::scale_x_continuous(expand = c(0.01, 0),
                                breaks = seq(from = x_range[1], to = x_range[2], by = x_label_every))

  } else {
    unique_values <- unique(DT[[x_col]])
    x_breaks_to_use <- if (!is.null(x_breaks)) x_breaks
                       else if (x_label_every > 1) unique_values[seq(1, length(unique_values), by = x_label_every)]
                       else unique_values
    ggplot2::scale_x_discrete(expand = c(0.01, 0), breaks = x_breaks_to_use)
  }

  # --- Base plot ---
  if (!is.null(fill_col) && fill_col %in% names(DT)) {
    p <- ggplot2::ggplot(DT, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]],
                                          fill = .data[[fill_col]])) +
      ggplot2::geom_col(width = bar_width, color = NA)
    if (!is.null(fill_colors)) p <- p + ggplot2::scale_fill_manual(values = fill_colors)
  } else {
    p <- ggplot2::ggplot(DT, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
      ggplot2::geom_col(fill = fill_color, width = bar_width, color = NA)
  }

  p <- p + x_scale +
    ggplot2::scale_y_continuous(labels = y_axis_labels,
                                expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::labs(title = title, subtitle = subtitle, x = x_label, y = y_label) +
    chart_theme(x_axis_angle = x_axis_angle)

  # Left end of the x axis, for the mean/median labels (numeric x such as
  # year uses its smallest value; a discrete axis uses position 1)
  x_first <- if (inherits(DT[[x_col]], c("Date", "POSIXct")) || is.numeric(DT[[x_col]]))
    min(DT[[x_col]], na.rm = TRUE) else 1

  # --- Mean / median / max / min ---
  y_values <- DT[[y_col]]
  if (add_mean) {
    mean_val <- mean(y_values, na.rm = TRUE)
    p <- p +
      ggplot2::geom_hline(yintercept = mean_val, linetype = mean_linetype,
                          color = mean_color, linewidth = ref_lw) +
      ggplot2::annotate("text", x = x_first, y = mean_val,
                        label = paste0("Mean: ", format(round(mean_val), big.mark = ",")),
                        hjust = -0.1, vjust = -0.6, color = mean_color,
                        size = ann_mm, fontface = "bold")
  }
  if (add_median) {
    median_val <- stats::median(y_values, na.rm = TRUE)
    p <- p +
      ggplot2::geom_hline(yintercept = median_val, linetype = "dashed",
                          color = mean_color, linewidth = ref_lw) +
      ggplot2::annotate("text", x = x_first, y = median_val,
                        label = paste0("Median: ", format(median_val, big.mark = ",")),
                        hjust = -0.1, vjust = 1.5, color = "#0072B2",
                        size = ann_mm, fontface = "bold")
  }
  if (add_maximum) {
    max_row <- DT[which.max(get(y_col))]
    p <- p + ggplot2::annotate("text", x = max_row[[x_col]], y = max_row[[y_col]],
                               label = paste0("Max: ", format(max_row[[y_col]], big.mark = ",")),
                               vjust = -0.5, hjust = 0.5, size = ann_mm, color = "#E69F00")
  }
  if (add_minimum) {
    min_row <- DT[which.min(get(y_col))]
    p <- p + ggplot2::annotate("text", x = min_row[[x_col]], y = min_row[[y_col]],
                               label = paste0("Min: ", format(min_row[[y_col]], big.mark = ",")),
                               vjust = 1.5, hjust = 0.5, size = ann_mm, color = "#D55E00")
  }

  # --- Trendline with optional R² / growth annotation ---
  if (add_trendline) {
    # Rows used for the trendline and growth %: all rows, or only those
    # where trend_filter_col is TRUE (e.g. actual years, not a projection)
    TD <- if (!is.null(trend_filter_col)) DT[get(trend_filter_col) %in% TRUE] else DT

    p <- p + ggplot2::geom_smooth(data = TD,
                                  mapping = ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]]),
                                  inherit.aes = FALSE,
                                  method = trendline_method, formula = y ~ x, se = FALSE,
                                  color = trendline_color, linewidth = data_lw)

    if (show_r_squared || show_trend_stats) {
      x_numeric <- if (inherits(TD[[x_col]], c("Date", "POSIXct"))) as.numeric(TD[[x_col]])
                   else if (is.factor(TD[[x_col]]) || is.character(TD[[x_col]])) seq_len(nrow(TD))
                   else TD[[x_col]]
      lm_fit <- stats::lm(TD[[y_col]] ~ x_numeric)

      annotation_lines <- character(0)
      if (show_r_squared) {
        annotation_lines <- c(annotation_lines,
                              sprintf("R² = %.3f", summary(lm_fit)$r.squared))
      }
      if (show_trend_stats) {
        first_val  <- TD[[y_col]][1]
        last_val   <- TD[[y_col]][nrow(TD)]
        pct_change <- (last_val - first_val) / first_val * 100
        annotation_lines <- c(annotation_lines,
                              sprintf("%s: %.1f%%", if (pct_change >= 0) "Growth" else "Decline",
                                      abs(pct_change)))
      }

      x_range <- range(x_numeric, na.rm = TRUE)
      y_range <- range(DT[[y_col]], na.rm = TRUE)
      x_pos <- switch(trend_stats_x_pos,
                      "right"  = x_range[2] - 0.05 * diff(x_range),
                      "center" = mean(x_range),
                      x_range[1] + 0.05 * diff(x_range))
      y_pos <- switch(trend_stats_y_pos,
                      "bottom" = y_range[1] + 0.15 * diff(y_range),
                      y_range[2] - 0.05 * diff(y_range))
      hjust_val <- switch(trend_stats_x_pos, "right" = 1, "center" = 0.5, 0)

      if (inherits(DT[[x_col]], "Date")) {
        x_pos <- as.Date(x_pos, origin = "1970-01-01")
      } else if (inherits(DT[[x_col]], "POSIXct")) {
        x_pos <- as.POSIXct(x_pos, origin = "1970-01-01", tz = attr(DT[[x_col]], "tzone"))
      }

      p <- p + ggplot2::annotate("text", x = x_pos, y = y_pos,
                                 label = paste(annotation_lines, collapse = "\n"),
                                 hjust = hjust_val, vjust = 1, color = trendline_color,
                                 size = ann_mm, fontface = "bold")
    }
  }

  # --- 3-sigma line (skipped when every bar is far below it) ---
  if (add_3sd) {
    threshold <- mean(y_values, na.rm = TRUE) + 3 * stats::sd(y_values, na.rm = TRUE)
    max_bar   <- max(y_values, na.rm = TRUE)
    if (!is.null(ref_line_band) && max_bar < (1 - ref_line_band) * threshold) {
      message(sprintf("  3-sigma line omitted: tallest bar (%s) is more than %d%% below it (%s)",
                      format(round(max_bar), big.mark = ","), round(100 * ref_line_band),
                      format(round(threshold), big.mark = ",")))
      add_3sd <- FALSE
    }
  }
  if (add_3sd) {
    p <- p +
      ggplot2::geom_hline(yintercept = threshold, color = sd_color,
                          linetype = sd_linetype, linewidth = ref_lw) +
      ggplot2::annotate("text", x = Inf, y = threshold,
                        label = sprintf("mu + 3*sigma %%~~%% '%s'",
                                        format(round(threshold), big.mark = ",")),
                        parse = TRUE, hjust = 1.1, vjust = -0.5,
                        color = sd_color, size = ann_mm, fontface = "bold")
  }

  # --- Data labels ---
  if (show_labels) {
    label_col <- label_col %||% y_col
    if (label_col %in% names(DT)) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = if (is.numeric(.data[[label_col]]))
          scales::comma(.data[[label_col]]) else .data[[label_col]]),
        angle = label_angle, size = lab_mm, color = label_color,
        vjust = label_vjust, hjust = lab_hjust
      )
    }
  }

  if (!is.null(extra_line)) p <- p + extra_line

  # --- Save (or just display) ---
  if (!is.null(chart_dir) && !is.null(filename)) {
    filepath <- save_chart(p, chart_dir, filename, width_in = w_in, height_in = h_in)
    message("File saved: ", filepath)
  } else {
    print(p)
    if ((st$pause_sec %||% 3) > 0) Sys.sleep(st$pause_sec %||% 3)
  }

  invisible(p)
}
