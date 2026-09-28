################################################################################
# chart_style.R
# One place for chart sizing. Every plot function reads its defaults from
# chart_style(), so calls only pass sizes when a chart is a true exception.
#
# Units
#   *_in     inches (page size)
#   *_pt     points (all text)
#   *_width  millimetres (line thickness, ggplot linewidth)
#   *_size   ggplot point size
#
# To change every chart at once, edit the defaults below. To change them for
# one session only (e.g. a test run), set an option before plotting:
#   options(chart_style = list(width_in = 6.5, axis_text_pt = 6))
#
# Used by: plot_pareto_combo(), plot_barchart(), plot_boxplot(),
#          plot_date_field_analysis(), plot_annual_counts_with_projection()
# Requires: david_theme.R
################################################################################

chart_style <- function() {
  defaults <- list(

    # --- Page size (inches) ---
    width_in       = 6,     # printed width; about 0.9\textwidth
    height_in      = 3,

    # --- Text (points) ---
    # base_pt        = 9,     # fallback for any text not listed below
    # title_pt       = 9,
    # subtitle_pt    = 6.5,
    # axis_title_pt  = 6,     # axis titles, e.g. "Duration (days)"
    # x_axis_text_pt = 5.25,     # x-axis tick labels
    # y_axis_text_pt = 5.25,     # y-axis tick labels (both sides)
    # label_pt       = 6.25,   # data labels on bars; annotations use label_pt + 0.5

    base_pt        = 9,
    title_pt       = 9,
    subtitle_pt    = 8,
    axis_title_pt  = 9,
    x_axis_text_pt = 7,
    y_axis_text_pt = 7,
    label_pt       = 7,    # data labels on bars
    
    # --- Lines (mm) and points ---
    line_width     = 0.25,   # data lines: cumulative %, trendlines, box outlines
    ref_line_width = 0.5,   # reference lines: 80%, mean, median, 3-sigma, zero
    point_size     = 0.4,   # points on lines, jittered points

    # --- Charts with one row per category (flipped box plots / Paretos) ---
    # Height grows with the number of categories beyond 10.
    height_per_group_in = 0.18,
    height_min_in       = 3,
    height_max_in       = 7,
    point_size      = 0.6,   # jittered points (box plots)
    line_point_size = 0.6,   # points on lines (Pareto cumulative %)

    # --- Display ---
    pause_sec      = 2      # pause after showing each chart in RStudio (0 = none)
  )
  utils::modifyList(defaults, getOption("chart_style", list()))
}

# Points -> ggplot text size (geom_text / annotate use mm)
pt_to_mm <- function(pt) pt / ggplot2::.pt

# Height for a chart with one row per category (rounded up to whole inches)
auto_height <- function(n_groups, st = chart_style()) {
  h <- st$height_min_in + max(0, n_groups - 10) * st$height_per_group_in
  ceiling(min(max(h, st$height_min_in), st$height_max_in))
}

# Shared save step: adds ".pdf" if missing, saves with cairo_pdf,
# then shows the chart in the plot pane
save_chart <- function(p, chart_dir, filename,
                       width_in = NULL, height_in = NULL, show = TRUE) {
  st <- chart_style()
  if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
  outfile <- file.path(chart_dir, sub("(\\.pdf)?$", ".pdf", filename))
  ggplot2::ggsave(outfile, plot = p,
                  width  = if (is.null(width_in))  st$width_in  else width_in,
                  height = if (is.null(height_in)) st$height_in else height_in,
                  device = grDevices::cairo_pdf)
  if (isTRUE(show)) {
    print(p)
    if (st$pause_sec > 0) Sys.sleep(st$pause_sec)
  }
  invisible(outfile)
}

# david_theme() with the style's sizes. Any david_theme() argument passed
# here (e.g. x_axis_angle = 30) overrides the style for that chart.
chart_theme <- function(..., st = chart_style()) {
  args <- utils::modifyList(
    list(text_size          = st$base_pt,
         plot_title_size    = st$title_pt,
         plot_subtitle_size = st$subtitle_pt,
         axis_title_size    = st$axis_title_pt,
         x_axis_text_size   = st$x_axis_text_pt,
         y_axis_text_size   = st$y_axis_text_pt),
    list(...))
  do.call(david_theme, args)
}
