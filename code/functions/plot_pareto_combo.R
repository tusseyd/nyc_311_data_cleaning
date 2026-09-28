################################################################################
# plot_pareto_combo.R
# Pareto chart: bars of counts by group, cumulative-% line, optional 80% line.
#
# Sizes come from chart_style() (see chart_style.R). Every size argument
# below defaults to NULL, meaning "use the standard size". Pass a value only
# when one chart needs to differ from the rest.
#
# Requires: chart_style.R (chart_style, chart_theme, save_chart, pt_to_mm,
#           auto_height) and david_theme.R
################################################################################

plot_pareto_combo <- function(
    DT,
    x_name = NULL,         # existing style (string)
    x_col  = "NULL",       # legacy style, bare or string
    x_expr = NULL,         # very legacy NSE style, bare/string/var
    chart_dir,
    filename,
    title,
    subtitle          = NULL,
    top_n             = 30L,
    include_na        = FALSE,
    na_label          = "(NA)",
    min_count         = 1L,
    flip              = FALSE,
    show_threshold_80 = TRUE,
    threshold_80_band = 0.10,   # draw the 80% line only if a cumulative point on the
                                # chart falls within 80% +/- this (NULL = always)
    show_labels       = TRUE,
    x_label_wrap      = NULL,   # wrap long category labels at N characters
    weight_col        = NULL,   # column to sum per group instead of counting rows
                                # (e.g. "excess" from a pre-aggregated table)

    # --- Size overrides (NULL = standard size from chart_style()) ---
    width_in           = NULL,  # inches
    height_in          = NULL,  # inches; flipped charts grow with the number of bars
    title_pt           = NULL,
    plot_subtitle_size = NULL,  # points
    x_axis_label_size  = NULL,  # points
    y_axis_label_pt    = NULL,  # points
    label_pt           = NULL,  # count labels above bars, points
    x_axis_label_angle = NULL,  # default 30 (0 when flipped)

    # --- Appearance (rarely changed) ---
    bar_width            = 0.55,
    bar_fill             = "#009E73",
    label_comma          = TRUE,
    y_headroom           = 0.12,
    cum_line_width       = NULL,   # NULL = chart_style()$line_width
    cum_point_size       = NULL,   # NULL = chart_style()$point_size
    threshold_line_width = NULL    # NULL = chart_style()$ref_line_width
) {

  `%||%` <- function(a, b) if (is.null(a)) b else a
  st <- chart_style()
  caller <- parent.frame()   # where x_col / x_name variables are looked up

  .resolve_expr_to_name <- function(expr, env = caller) {
    if (is.null(expr)) return(NULL)
    if (is.symbol(expr)) {
      val <- tryCatch(eval(expr, env), error = function(e) NULL)
      if (is.character(val) && length(val) == 1 && nzchar(val)) return(val)
      return(deparse(expr))
    } else if (is.character(expr) && length(expr) == 1 && nzchar(expr)) {
      return(expr)
    } else {
      val <- tryCatch(eval(expr, env), error = function(e) NULL)
      if (is.character(val) && length(val) == 1 && nzchar(val)) return(val)
      return(NULL)
    }
  }

  # --- Resolve the grouping column (x_col legacy first, then x_name) ---
  col_from_x_col  <- .resolve_expr_to_name(substitute(x_col))
  col_from_x_name <- {
    if (is.character(x_name) && length(x_name) == 1 && nzchar(x_name)) x_name
    else .resolve_expr_to_name(substitute(x_name))
  }
  resolved_col <- if (!is.null(col_from_x_col) && col_from_x_col %in% names(DT))
    col_from_x_col else col_from_x_name
  if (is.null(resolved_col)) {
    stop("plot_pareto_combo(): unable to resolve grouping column. ",
         "Use x_col = agency (bare) or x_col = \"agency\", or x_name = \"agency\".",
         call. = FALSE)
  }
  if (!data.table::is.data.table(DT))
    stop("DT must be a data.table. Use data.table::setDT() first.")
  if (!(resolved_col %in% names(DT))) {
    stop(sprintf("plot_pareto_combo(): column '%s' not found in DT.", resolved_col),
         call. = FALSE)
  }
  x_str <- resolved_col

  # --- Aggregate: row counts, or the sum of weight_col ---
  if (!is.null(weight_col) && !weight_col %in% names(DT))
    stop(sprintf("plot_pareto_combo(): weight_col '%s' not found in DT.", weight_col), call. = FALSE)
  rows <- if (include_na) DT else DT[!is.na(get(x_str))]
  agg <- if (is.null(weight_col)) {
    rows[, .(N = .N), by = .(group = get(x_str))][order(-N)]
  } else {
    rows[, .(N = sum(get(weight_col), na.rm = TRUE)), by = .(group = get(x_str))][order(-N)]
  }
  if (!nrow(agg)) {
    message(sprintf("plot_pareto_combo: no rows to plot for '%s'.", x_str))
    return(invisible(NULL))
  }
  if (include_na) agg[is.na(group), group := na_label]

  totalN <- sum(agg$N)
  agg[, `:=`(pct = N / totalN, cum_pct = cumsum(N) / totalN)]

  # --- Filter out groups with too few observations ---
  if (min_count > 1L) {
    before_count <- nrow(agg)
    agg <- agg[N >= min_count]
    if (before_count > nrow(agg)) {
      cat(sprintf("\nFiltered out %d groups with < %d observations\n",
                  before_count - nrow(agg), min_count))
    }
    totalN <- sum(agg$N)
    agg[, `:=`(pct = N / totalN, cum_pct = cumsum(N) / totalN)]
  }

  # --- Summary table (top N) ---
  total_groups <- nrow(agg)
  df <- agg[1:min(top_n, total_groups)]
  df[, `:=`(pct = round(pct, 2), cum_pct = round(cum_pct, 2))]
  df[, group := factor(group, levels = df$group)]  # lock order

  cat("\nPareto summary by ", x_str,
      if (total_groups > nrow(df)) sprintf(" (first %d of %d)", nrow(df), total_groups) else "",
      ":\n", sep = "")
  old_opt <- options(datatable.print.class = FALSE); on.exit(options(old_opt), add = TRUE)
  print(data.frame(
    group   = format(df$group, justify = "left"),
    N       = format(df$N, justify = "right"),
    pct     = format(round(df$pct, 4), justify = "right"),
    cum_pct = format(round(df$cum_pct, 4), justify = "right")
  ), row.names = FALSE)

  # --- Sizes: explicit arguments win, otherwise chart_style() ---
  axis_pt   <- y_axis_label_pt   %||% st$y_axis_text_pt
  x_axis_pt <- x_axis_label_size %||% st$x_axis_text_pt
  lab_mm    <- pt_to_mm(label_pt %||% st$label_pt)
  angle     <- x_axis_label_angle %||% (if (isTRUE(flip)) 0 else 30)
  cum_lw    <- cum_line_width       %||% st$line_width     %||% 0.3
  cum_pt    <- cum_point_size       %||% st$point_size     %||% 0.8
  thr_lw    <- threshold_line_width %||% st$ref_line_width %||% 0.5
  w_in      <- width_in  %||% st$width_in
  h_in      <- height_in %||% (if (isTRUE(flip)) auto_height(nrow(df), st) else st$height_in)

  # --- Plot ---
  maxN <- max(df$N)
  df[, count_label := if (isTRUE(label_comma)) scales::comma(N) else as.character(N)]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = N)) +
    ggplot2::geom_col(width = bar_width, fill = bar_fill)

  if (isTRUE(show_labels)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = count_label),
      colour = "black", hjust = 0.5, vjust = -0.5, size = lab_mm
    )
  }

  p <- p +
    ggplot2::geom_line(ggplot2::aes(y = cum_pct * maxN, group = 1), linewidth = cum_lw) +
    ggplot2::geom_point(ggplot2::aes(y = cum_pct * maxN), size = cum_pt)

  # 80% line only when a cumulative point is near it
  if (isTRUE(show_threshold_80) && !is.null(threshold_80_band) &&
      !any(abs(df$cum_pct - 0.8) <= threshold_80_band + 1e-9)) {
    message(sprintf("  80%% line omitted: no cumulative point within %d-%d%%",
                    round(100 * (0.8 - threshold_80_band)), round(100 * (0.8 + threshold_80_band))))
    show_threshold_80 <- FALSE
  }
  if (isTRUE(show_threshold_80)) {
    p <- p +
      ggplot2::geom_hline(yintercept = 0.8 * maxN, linetype = "dotted",
                          linewidth = thr_lw, color = "#D55E00", alpha = 0.7) +
      ggplot2::annotate("text", x = Inf, y = 0.8 * maxN, label = "80%",
                        hjust = 1.2, vjust = -0.3, size = lab_mm * 1.1,
                        color = "#D55E00", fontface = "bold")
  }

  x_labels <- if (!is.null(x_label_wrap)) scales::label_wrap(x_label_wrap) else ggplot2::waiver()

  p <- p +
    ggplot2::scale_x_discrete(labels = x_labels) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, y_headroom)),
      sec.axis = ggplot2::sec_axis(~ . / maxN,
                                   labels = scales::percent_format(accuracy = 1),
                                   name = NULL)
    ) +
    ggplot2::labs(
      title    = if (is.null(title)) sprintf("Pareto by %s (counts & cumulative %%)", x_str) else title,
      subtitle = if (is.null(subtitle)) sprintf("n = %s", scales::comma(totalN)) else subtitle,
      x = NULL, y = NULL
    ) +
    chart_theme(
      plot_title_size    = title_pt           %||% st$title_pt,
      plot_subtitle_size = plot_subtitle_size %||% st$subtitle_pt,
      x_axis_text_size   = x_axis_pt,
      y_axis_text_size   = axis_pt,
      x_axis_angle       = angle
    )

  if (isTRUE(flip)) p <- p + ggplot2::coord_flip()

  outfile <- save_chart(p, chart_dir, filename, width_in = w_in, height_in = h_in)

  invisible(list(plot = p, table = df, file = outfile))
}
