# Rebuild main Figure 2 from the complete 24-hour counts in the saved output.
# Run from the project root; no raw 311 data are needed for these two plots.
library(data.table)
library(ggplot2)
source("code/functions/david_theme.R")
source("code/functions/plot_barchart.R")
grDevices::pdf(NULL)  # Discard the plotting function's console preview.

output <- readLines(
  "reference_console_output/referenc_JDS_datacleaning_console_output.txt"
)
for (field in c("Created", "Closed")) {
  heading <- grep(paste0("^", field, " Records Exactly at Top-of-the-Hour"), output)
  stopifnot(length(heading) == 1L)
  start <- heading + 6L
  rows <- strsplit(trimws(output[start:(start + 23L)]), "\\s+")
  stopifnot(all(lengths(rows) == 4L))
  hours <- as.integer(vapply(rows, `[`, "", 1L))
  counts <- as.integer(gsub(",", "", vapply(rows, `[`, "", 3L)))
  stopifnot(identical(hours, 0:23), !anyNA(counts))
  expected_total <- if (field == "Created") 86918L else 1541893L
  stopifnot(sum(counts) == expected_total)
  hourly <- data.table(hour = factor(hours, levels = c(7:23, 0:6)),
                       top_of_hour = counts)
  plot_barchart(
    DT = hourly, x_col = "hour", y_col = "top_of_hour",
    x_label = "Hour of day", y_label = "SRs at HH:00:00",
    title = "", subtitle = paste("n =", format(sum(counts), big.mark = ",")),
    add_3sd = TRUE, show_labels = FALSE, x_axis_angle = 0,
    y_axis_labels = scales::comma, show_summary = FALSE,
    chart_width = 6, chart_height = 2,
    chart_dir = "manuscript/images",
    filename = paste0(tolower(field), "_top_of_hour_distribution")
  )
}
grDevices::dev.off()
