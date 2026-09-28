################################################################################
# plot_date_field_analysis.R
# Pareto chart by agency for one date-field anomaly (e.g. midnight closed
# dates). Skips the chart when too few agencies are involved, builds the
# file name and title from the label, and prints a short summary.
#
# The chart is drawn by plot_pareto_combo(), so sizes come from
# chart_style(). width_in / height_in are only needed for a one-off size.
################################################################################

plot_date_field_analysis <- function(
    DT,
    date_col,
    group_col,
    chart_dir,
    label            = "Date Field",
    condition_text   = "",
    min_agency_count = 2,
    top_n            = 30L,
    width_in         = NULL,   # NULL = standard size from chart_style()
    height_in        = NULL
) {

  date_col_name  <- deparse(substitute(date_col))
  group_col_name <- deparse(substitute(group_col))

  # Records with a value in the date field
  DT_filtered <- DT[!is.na(get(date_col_name))]
  if (nrow(DT_filtered) == 0) {
    cat(sprintf("\nSkipping %s - no records with valid %s\n", label, date_col_name))
    return(invisible(NULL))
  }

  n_agencies <- DT_filtered[, data.table::uniqueN(get(group_col_name))]
  total_obs  <- nrow(DT_filtered)

  if (n_agencies < min_agency_count) {
    cat(sprintf("\nSkipping %s - need %d+ agencies (have %d with %d total observations)\n",
                label, min_agency_count, n_agencies, total_obs))
    return(invisible(NULL))
  }

  cat(sprintf("\n=== %s ===\n", label))
  cat(sprintf("Total observations: %d\n", total_obs))
  cat(sprintf("Number of agencies: %d\n", n_agencies))

  # File name from the label: "Midnight Closed Dates" -> midnight_closed_dates_pareto_combo_chart.pdf
  base_name <- label |>
    iconv(to = "ASCII//TRANSLIT") |>
    gsub("[^A-Za-z0-9]+", "_", x = _) |>
    gsub("_+", "_", x = _) |>
    gsub("^_|_$", "", x = _) |>
    tolower()
  pareto_file <- paste0(base_name, "_pareto_combo_chart.pdf")

  title_case <- tools::toTitleCase(tolower(label))
  plot_title <- if (nzchar(condition_text)) {
    paste0(title_case, " ", condition_text, " SRs by Agency")
  } else {
    paste0(title_case, " SRs by Agency")
  }

  cat(sprintf("Creating Pareto chart: %s\n", pareto_file))

  plot_pareto_combo(
    DT         = DT_filtered,
    x_col      = group_col_name,
    chart_dir  = chart_dir,
    filename   = pareto_file,
    title      = plot_title,
    subtitle   = sprintf("n = %s", scales::comma(total_obs)),
    top_n      = top_n,
    include_na = FALSE,
    width_in   = width_in,
    height_in  = height_in
  )

  invisible(list(
    label       = label,
    n_agencies  = n_agencies,
    n_records   = total_obs,
    pareto_file = pareto_file
  ))
}
