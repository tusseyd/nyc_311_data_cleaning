################################################################################
################################################################################

main_data_file <-  
  "5-year_311SR_01-01-2020_thru_12-31-2024_AS_OF_09-23-2025.rds"
 
# Boolean flag. TRUE to redirect console output to text file
# FALSE to display console outpx`t on the screen
enable_sink <- TRUE        

# Memory-management flags (to compare runs with them on and off)
#   use_free_objects: TRUE = free_objects() removes the objects it is given
#                     FALSE = objects are left in memory
#   use_gc:           TRUE = explicit gc() calls run (in free_objects() and at
#                     the standalone gc points); FALSE = R collects on its own
# Memory and elapsed time are logged at each of these points either way, and
# written to memory_trace_datacleaning_<flags>.csv in the console folder.
use_free_objects <- TRUE
use_gc           <- TRUE

# Live memory monitor: a separate window (its own R process) prints the
# session's memory, CPU and system RAM every monitor_interval_sec seconds,
# and logs them to memory_monitor_<computer>_<flags>_<time>.csv in the
# console folder. mem_messages = TRUE also prints a line in this console at
# each free_objects() / run_gc() point. Needs the ps package.
monitor_memory       <- FALSE
monitor_interval_sec <- 60
mem_messages         <- FALSE

#The "as of" date in "YYYY-MM-DD" format
projection_date <- "2025-11-30"   

#Number of SRs for the year through the projection_date  
projection_SR_count <- 3218088  

# Okabe-Ito palette for colorblind safe 
palette(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
          "#0072B2", "#D55E00", "#CC79A7", "#999999"))

################################################################################

# ------------------------------------------------------ -------
# 📦 CREATE REQUIRED DIRECTORY STRUCTURE
# -------------------------------------------------------------
################################################################################
# Main Analysis Script
################################################################################

# STEP 1: Create directory structure (inline)
# Set base directory to current working directory

# Build the path
wd_path <- file.path(
  "C:",
  "Users",
  "David",
  "OneDrive",
  "Documents",
  "datacleaningproject",
  "journal_of_data_science",
  "nyc_311_data_cleaning"
)

# Set the working directory
setwd(wd_path)

base_dir <- getwd()
cat("Base directory:", base_dir, "\n")

# Define all paths relative to base_dir
analytics_dir <- file.path(base_dir, "analytics")
chart_dir     <- file.path(base_dir, "charts")
code_dir      <- file.path(base_dir, "code")
console_dir   <- file.path(base_dir, "console_output")
data_dir      <- file.path(base_dir, "data")
functions_dir <- file.path(base_dir, "code", "functions")
raw_data_dir  <- file.path(base_dir, "data", "raw_data")
# write_dir     <- file.path(base_dir, "misc")

dirs_to_create <- c(analytics_dir, chart_dir, code_dir, console_dir, 
                    data_dir, functions_dir, raw_data_dir)

for (dir in dirs_to_create) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("✅ Created directory:", dir, "\n")
  } else {
    cat("📁 Directory exists:", dir, "\n")
  }
}

cat("\nDirectory paths set:\n")
cat("  Analytics:", analytics_dir, "\n")
cat("  Charts:", chart_dir, "\n")
cat("  Code:", code_dir, "\n")
cat("  Console output:", console_dir, "\n")
cat("  Data:", data_dir, "\n")
cat("  Functions:", functions_dir, "\n")
cat("  Raw data:", raw_data_dir, "\n")
# cat("  Write:", write_dir, "\n")

# STEP 2: Source and run setup function
source(file.path(functions_dir, "setup_project.R"))

timing <- setup_project(
  enable_sink = enable_sink,
  console_filename = "JDS_datacleaning_console_output.txt",
  functions_dir = functions_dir,
  verbose = TRUE
)

# Memory tools (free_objects, run_gc, mem_log, start_memory_monitor,
# mem_report) are in functions/free_objects.R, shared with the data prep
# program. setup_project() normally loads it; this line covers the case
# where it has not.
if (!exists("free_objects", mode = "function")) source(file.path(functions_dir, "free_objects.R"))
mem_init("datacleaning")
if (isTRUE(monitor_memory)) start_memory_monitor(monitor_interval_sec)

# Progress messages (progress_msg() in free_objects.R) appear on screen with
# the clock time and elapsed time, even when enable_sink = TRUE sends the
# rest of the output to the console file.
progress_msg("Data cleaning analysis started (29 steps)")

# Fixed random seed: the random example rows and the sampled summary
# tables (e.g. negative resolution updates by agency) are then identical
# every time the program runs on the same data.
set.seed(20250923)

################################################################################
# Extract the date after "AS_OF_"
extracted_date <- sub(".*AS_OF_([0-9-]+).*", "\\1", main_data_file)
as_of_date <- as.POSIXct(
  paste0(extracted_date, " 00:00:00"),
  format = "%m-%d-%Y %H:%M:%S",
  tz = "America/New_York"
)

# NYC 311 system launch. Any date field earlier than this is impossible
# and indicates a placeholder or entry error.
genesis_date <- as.POSIXct("2003-03-09 00:00:00", tz = "America/New_York")

# Convert to POSIXct format
max_closed_date <- as.POSIXct(extracted_date, format = "%m-%d-%Y", 
                              tz = "America/New_York")
#print(paste("Parsed date:", max_closed_date))

# Add time to end of day
max_closed_date <- max_closed_date + (23*3600 + 59*60 + 59)
#print(paste("Final datetime:", max_closed_date))

################################################################################
# Load the USPS zipcode file
progress_msg("Step 1 of 29: Reading the USPS ZIP code file")

USPS_zipcode_file_path <- file.path(data_dir, "USPS_zipcodes.rds")

USPSzipcodes <- readRDS(USPS_zipcode_file_path)
if (!is.data.table(USPSzipcodes)) setDT(USPSzipcodes)  # converts in place

################################################################################
# Load the main 311 SR data file. Set the read & write paths.
progress_msg("Step 2 of 29: Reading the main 311 SR data file")

main_data_file_path <- file.path( data_dir, main_data_file)

d311 <- readRDS(main_data_file_path)
if (!is.data.table(d311)) setDT(d311)  # converts in place

num_rows_d311 <- nrow(d311)
num_columns_d311 <- ncol(d311)

# Guard: ensure column exists
stopifnot("unique_key" %in% names(d311))
setindex(d311, unique_key)   # no reorder; speeds joins/subsets on unique_key

################################################################################
# Regenerate input data if necessary

#copy_raw_data <- d311
#d311 <- copy_raw_data

print(table(lubridate::year(d311$closed_date)) )

################################################################################
# Check for unique keys and index
progress_msg("Step 3 of 29: Creating the unique_key index")

# Basic stats
n_na_keys   <- sum(is.na(d311$unique_key))
n_unique    <- uniqueN(d311$unique_key)
all_unique  <- (n_unique == num_rows_d311) && (n_na_keys == 0L)

if (all_unique) {
  # Fast lookups without reordering
  setindex(d311, unique_key)
  # Optional belt-and-suspenders assertion
  stopifnot(anyDuplicated(d311$unique_key) == 0L)
#  cat("Index set on 'unique_key' (secondary index; row order preserved).\n")
  } else {
  # Diagnose duplicates and/or NAs
  if (n_na_keys > 0L) {
    cat("Found ", n_na_keys, " NA unique_key value(s).\n", sep = "")
  }
    
  # Keys that occur more than once
  dup_keys <- d311[, .N, by = unique_key][!is.na(unique_key) & N > 1L][order(-N)]
  
  if (nrow(dup_keys)) {
    # Total duplicate rows = sum(N-1) over dup keys
    total_dup_rows <- dup_keys[, sum(N - 1L)]
    cat("Found ", total_dup_rows, " duplicate row(s) across ",
        nrow(dup_keys), " unique_key value(s).\n", sep = "")
    cat("\nTop duplicate keys (count per key):\n")
    print(dup_keys[1:min(10L, .N)], nrows = min(10L, nrow(dup_keys)))
    # Optional: peek at one offending key’s rows
    eg_key <- dup_keys$unique_key[1L]
    cat("\nExample rows for unique_key = ", eg_key, ":\n", sep = "")
    print(d311[unique_key == eg_key][1:min(.N, 5L)])
  }
  cat("\nResolve NA/duplicate keys before indexing.\n")
  # Do NOT setindex here to avoid masking an issue.
  }

################################################################################

cat("\n\n********** Missing entires by column **********")

################################################################################
# --- Per-column completeness analysis (percent filled) ----------------------
progress_msg("Step 4 of 29: Counting missing entries by field")

na_counts <- vapply(d311, function(x) sum(is.na(x)), integer(1L))

completenessPerColumn <- data.table(
  field        = names(na_counts),
  NA_count     = unname(na_counts),
  filled_count = num_rows_d311 - unname(na_counts)
)
completenessPerColumn[, `:=`(
  pct_missing = round(100 * NA_count / num_rows_d311, 4),
  pct_of_total = round(100 * filled_count / num_rows_d311, 4)
)]

# Sort ascending so lowest completeness appears first (left side of chart)
setorder(completenessPerColumn, pct_of_total)

# Apply factor levels based on that order
completenessPerColumn[, field := factor(field, levels = field)]

cat("\nNumber and % COMPLETE entries per column:\n")
print(completenessPerColumn[, .(field, filled_count, pct_of_total)], 
      row.names = FALSE)

# --- Overall dataset completeness summary ---
total_cells <- num_rows_d311 * ncol(d311)
total_na <- sum(na_counts)
total_filled <- total_cells - total_na
overall_pct_missing <- round(100 * total_na / total_cells, 2)
overall_pct_complete <- round(100 * total_filled / total_cells, 2)

cat("\n\n--- Overall Dataset Completeness ---\n")
cat(sprintf("Total cells: %s\n", format(total_cells, big.mark = ",")))
cat(sprintf("Total NA/missing: %s\n", format(total_na, big.mark = ",")))
cat(sprintf("Total filled: %s\n", format(total_filled, big.mark = ",")))
cat(sprintf("Percent missing: %s%%\n", overall_pct_missing))
cat(sprintf("Percent complete: %s%%\n", overall_pct_complete))

completeness_thresholds <- c(94, 53)

# Plot
create_basic_bar_chart(
  DT            = completenessPerColumn,
  x_col         = "field",
  y_col         = "pct_of_total",
  title         = "Data Completeness by Field",
  use_color_groups = TRUE,
  horizontal = TRUE,
  group_thresholds = completeness_thresholds,
  group_labels     = c(
    sprintf("Excellent (≥%d%%)", completeness_thresholds[1]),
    sprintf("Fair (%d–%d%%)",    completeness_thresholds[2], completeness_thresholds[1]),
    sprintf("Poor (<%d%%)",      completeness_thresholds[2])),
  group_colors     = c("#009E73", "#F0E442", "#D55E00"),
  legend_position  = "top",
  text_size        = 9,
  x_axis_angle     = 50,
  x_axis_face      = "bold",
  remove_x_title   = TRUE,
  remove_y_title   = TRUE,
  chart_dir        = chart_dir,
  filename         = "data_completeness_by_field_bar_chart.pdf"
)

################################################################################
# Determine field usage by Agency. Produce Excel spreadsheet.
# Initialize the list of fields (excluding "agency")
progress_msg("Step 5 of 29: Computing field usage by agency")

# Columns to summarize (exclude the grouping column)
fields_to_summarize <- setdiff(names(d311), "agency")

# Treat NA and "" as missing for character/factor; NA for everything else
has_usable_value <- function(x) {
  ok <- !is.na(x)
  if (is.character(x)) {
    ok <- ok & nzchar(x)
  } else if (is.factor(x)) {
    blank_lvl <- match("", levels(x), nomatch = 0L)
    if (blank_lvl != 0L) ok <- ok & (as.integer(x) != blank_lvl)
  }
  ok
}

# Long format counts: for each field, count usable values by agency
long_counts_by_agency_dt <- rbindlist(
  lapply(fields_to_summarize, function(field_name) {
    usable_row <- has_usable_value(d311[[field_name]])
    d311[usable_row & !is.na(agency), .(count = .N), by = agency][
      , field := field_name][]
  }),
  use.names = TRUE
)

# Wide summary: one row per field, one column per agency (counts)
field_usage_summary_dt <- dcast(
  long_counts_by_agency_dt,
  field ~ agency,
  value.var = "count",
  fill = 0L
)

# Row totals and ordering
agency_cols <- setdiff(names(field_usage_summary_dt), "field")
field_usage_summary_dt[, total := rowSums(.SD), .SDcols = agency_cols]
setorder(field_usage_summary_dt, -total)

# Per-row percentages (0–100, one decimal); create parallel *_pct columns
pct_cols <- paste0(agency_cols, "_pct")
field_usage_summary_dt[
  , (pct_cols) := lapply(.SD, function(x) fifelse(total > 0, 
                        round(100 * x / total, 5), 0)), .SDcols = agency_cols
]

# Console preview
cat("\nField usage by agency (counts):\n")
DT_to_print <- field_usage_summary_dt[, c("field", agency_cols, "total"), 
                                      with = FALSE]

# Pre-format columns for alignment
display_df <- data.frame(
  field = format(DT_to_print$field, justify = "left"),
  lapply(DT_to_print[, agency_cols, with = FALSE], function(x) format(x, 
      justify = "right")), total = format(DT_to_print$total, justify = "right")
)

# Set column names to match original
names(display_df) <- c("field", agency_cols, "total")

print(display_df, row.names = FALSE)
# Save to CSV fast
summary_table_file_path <- file.path(analytics_dir, 
                                     "field_usage_summary_table.csv")
fwrite(field_usage_summary_dt, summary_table_file_path)

#cat("\nCSV written to:\n", summary_table_file_path, "\n")

################################################################################
progress_msg("Step 6 of 29: Removing unused fields")
# Clean-up memory by removing non-utilzied columns

# Keep ONLY the necessary columns
columns_to_keep <- c(
  "unique_key",
  "created_date", 
  "closed_date",
  "agency", 
  "complaint_type",
  "incident_zip",
  "street_name", 
  "cross_street_1",
  "cross_street_2", 
  "intersection_street_1", 
  "intersection_street_2",
  "address_type", 
  "landmark",
  "status", 
  "due_date",
  "resolution_action_updated_date", 
  "community_board",
  "borough", 
  "x_coordinate_state_plane",
  "y_coordinate_state_plane", 
  "open_data_channel_type", 
  "park_borough", 
  "vehicle_type", 
  "taxi_company_borough",
  "latitude",
  "longitude", 
  "location"
)

# Remove unnecessary columns to free up memory. Garbage collection.
d311[, setdiff(names(d311), columns_to_keep) := NULL]
run_gc("after dropping unused columns")

# Make a copy for troubleshooting purposes to avoid re-reading in data
#copy_d311 <- d311

################################################################################

cat("\n\n**********CROSS STREET/INTERSECTION STREET ANALYSYS**********\n")
progress_msg("Step 7 of 29: Cross street and intersection street analysis")

# Define street pairs to analyze
street_pairs <- list(
  list(street1 = "cross_street_1", street2 = "intersection_street_1"),
  list(street1 = "cross_street_2", street2 = "intersection_street_2"),
  list(street1 = "landmark", street2 = "street_name")
)

# Initialize results storage
all_comparison_results <- list()

# Loop through each street pair
for (i in seq_along(street_pairs)) {
  pair <- street_pairs[[i]]
  
  cat("\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat(sprintf("STREET PAIR ANALYSIS %d of %d\n", i, length(street_pairs)))
  #  cat(paste(rep("=", 80), collapse = ""), "\n")
  
  # Run comparison analysis
  comparison_result <- compare_street_analyses(
    dt = d311,
    street1_col = pair$street1,
    street2_col = pair$street2,
    chart_dir = chart_dir,
    run_enhanced = FALSE
  )
  
  # Store results with descriptive name
  pair_name <- sprintf("%s_vs_%s", pair$street1, pair$street2)
  all_comparison_results[[pair_name]] <- comparison_result
}

# Summary across all pairs
cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY ACROSS ALL STREET PAIRS\n")
#cat(paste(rep("=", 80), collapse = ""), "\n")

for (i in seq_along(all_comparison_results)) {
  result <- all_comparison_results[[i]]
  pair_name <- names(all_comparison_results)[i]
  
  cat(sprintf("\n%s:\n", toupper(gsub("_", " ", pair_name))))
  cat(sprintf("  Match rate:     %5.4f%%\n", 
              result$raw_analysis$match_rate * 100))
  cat(sprintf("  Cleaned match rate: %5.4f%%\n", 
              result$cleaned_analysis$match_rate * 100))
  cat(sprintf("  Improvement:        %5.4f%% (%s records)\n", 
              result$comparison$match_rate_improvement * 100,
              scales::comma(result$comparison$match_improvement)))
}

cat(sprintf("\nTotal street pair analyses completed: %d\n", 
            length(all_comparison_results)))

# Remove cross and intersections streets immediately
d311[, c(
  "cross_street_1", 
  "intersection_street_1", 
  "cross_street_2", 
  "intersection_street_2",
  "landmark",
  "street_name") := NULL]  # Remove immediately

run_gc("after dropping address columns")

################################################################################

cat("\n\n**********DATA SUMMARY**********")

################################################################################
# assume d311 is a data.table and created_date is POSIXct
tz_out  <-  "America/New_York"                    
fmt_ts  <- "%Y-%m-%d %H:%M:%S"
fmt_day <- "%Y-%m-%d"               

# Compute once
rng <- d311[, range(created_date, na.rm = TRUE)]  # POSIXct min,max
if (any(is.infinite(rng))) stop("created_date has no non-missing values")

earliest_date <- rng[1L]
latest_date   <- rng[2L]

# First created_date in the NYC Open Data extract used for this analysis.
extract_start_date <- as.POSIXct("2020-01-01 00:00:00", tz = "America/New_York")

n_before_extract <- d311[created_date < extract_start_date, .N]
cat(sprintf("  Created before extract start (%s): %s\n",
            format(extract_start_date, "%Y-%m-%d"),
            format(n_before_extract, big.mark = ",")))

# Timestamp strings
earliest_date_formatted <- format(earliest_date, fmt_ts, tz = tz_out)
latest_date_formatted   <- format(latest_date,   fmt_ts, tz = tz_out)

# Date-only strings for titles
earliest_title <- format(earliest_date, fmt_day, tz = tz_out)
latest_title   <- format(latest_date,   fmt_day, tz = tz_out)

################################################################################
# Probe right before function call
print(range(d311$created_date))
print(summary(attr(d311$created_date, "tzone")))

# Plot yearly growth of 311 SRs.
progress_msg("Step 8 of 29: Annual SR counts and projection")

yearly_bar_chart <- plot_annual_counts_with_projection(
  DT = d311,
  created_col = "created_date",
  estimate_flag = TRUE,
  estimate_date = projection_date,
  estimate_value = projection_SR_count,
  chart_dir = chart_dir,
  filename = "annual_trend_with_projection_bar_chart.pdf",
  title = "NYC 311 Service Requests by Year",
  subtitle = "w/2025 projection",
  include_projection_in_growth = FALSE,
  include_projection_in_stats  = FALSE,
  show_projection_bar = TRUE    
  )

################################################################################
# fraction in 0–1 with 4 decimals
sorted_by_agency <- d311[, .(count = .N), by = agency][order(-count)]
sorted_by_agency[, percentage := round(count / sum(count), 4)]
sorted_by_agency[, cumulative_percentage := cumsum(percentage)]

print(
  sorted_by_agency[, .(
    agency,
    count = comma(count),
    percentage = percent(percentage, accuracy = 0.01),
    cumulative_percentage = percent(cumulative_percentage, accuracy = 0.01)
  )]
)

# At the top of your script
options(warn = 2)  # Turn warnings into errors

plot_pareto_combo(
  DT               = d311,
  x_col            = agency,
  title            = "SR Volume by Agency",
  filename         = "SR_volume_by_agency_pareto_combo_chart.pdf",
  chart_dir        = chart_dir,
  width_in = 6,
  height_in = 3,
  show_labels      = FALSE,
  show_threshold_80 = TRUE   # whether to draw the 80% reference line
)

# Display the results
cat("\nRows in the 311 SR dataset:", format(num_rows_d311, big.mark = ","))
cat("\nColumns in the 311 SR dataset:", format(num_columns_d311, big.mark = ","))
cat("\nAgencies represented:", length(unique(d311$agency)))
cat("\n\nData contains SRs from", earliest_date_formatted, 
    "through", latest_date_formatted)

################################################################################
# Convert coordinate columns to numeric
coord_cols <- c("latitude", "longitude")
d311[, (coord_cols) := lapply(.SD, as.numeric), .SDcols = coord_cols]

################################################################################
progress_msg("Step 9 of 29: Organizing complaint types")

cat("\n\n********** COMPLAINT TYPES **********")

total_rows <- nrow(d311)

# One pass: frequency + agency labeling per complaint_type
complaint_summary_dt <- d311[
  , .(
    count = .N,
    unique_agency_count = uniqueN(agency, na.rm = TRUE),
    agency = {
      u <- unique(agency)
      u <- u[!is.na(u)]
      if (length(u) == 1L) u else if (length(u) == 0L) NA_character_ else "MULTIPLE"
    }
  ),
  by = complaint_type
][
  order(-count)
][
  # compute percents from counts to avoid cumulative rounding drift
  , `:=`(
    percent = round(100 * count / total_rows, 2),
    cumulative_percent = round(100 * cumsum(count) / total_rows, 2)
  )
][]

cat("\nThere are", nrow(complaint_summary_dt), "different complaint_type(s).\n")

# ---- Console reports ----
top_n <- 20L
cat("\nTop ", top_n, " complaint_type(s) and responsible agency:\n", sep = "")
top_dt <- complaint_summary_dt[1:min(.N, top_n), .(complaint_type, count, 
                                           percent, cumulative_percent, agency)]
top_df <- data.frame(
  complaint_type = format(top_dt$complaint_type, justify = "left"),
  count = format(top_dt$count, justify = "right"),
  percent = format(top_dt$percent, justify = "right"),
  cumulative_percent = format(top_dt$cumulative_percent, justify = "right"),
  agency = format(top_dt$agency, justify = "left")
)
print(top_df, row.names = FALSE)

cat("\nBottom ", top_n, " complaint_type(s) and responsible agency:\n", sep = "")
bottom_dt <- tail(complaint_summary_dt[, .(complaint_type, count, agency)], 
                  top_n)
bottom_df <- data.frame(
  complaint_type = format(bottom_dt$complaint_type, justify = "left"),
  count = format(bottom_dt$count, justify = "right"),
  agency = format(bottom_dt$agency, justify = "left")
)
print(bottom_df, row.names = FALSE)

cat("\nComplaints with multiple responsible agencies:\n")
multiple_agency_dt <- complaint_summary_dt[agency == "MULTIPLE", 
                                           .(complaint_type, count, percent)]
multiple_df <- data.frame(
  complaint_type = format(head(multiple_agency_dt, top_n)$complaint_type, 
                          justify = "left"),
  count = format(head(multiple_agency_dt, top_n)$count, justify = "right"),
  percent = format(head(multiple_agency_dt, top_n)$percent, justify = "right")
)
print(multiple_df, row.names = FALSE)

# ---- Noise complaints (prefix "NOISE") ----
noise_dt <- complaint_summary_dt[startsWith(complaint_type, "NOISE")]

cat("\nThere are", nrow(noise_dt), "categories of noise complaints:\n")

noise_display_dt <- noise_dt[, .(complaint_type, count, percent)]

noise_df <- data.frame(
  complaint_type = format(noise_display_dt$complaint_type, justify = "left"),
  count = format(noise_display_dt$count, justify = "right"),
  percent = format(noise_display_dt$percent, justify = "right")
)

print(head(noise_df, top_n), row.names = FALSE)

noise_total <- noise_dt[, sum(count)]
noise_pct   <- round(100 * noise_total / total_rows, 1)
cat(
  "\nNoise complaints of all ", nrow(noise_dt), "types number ",
  format(noise_total, big.mark = ","),
  ", constituting ", noise_pct, "% of all SRs.\n", sep = ""
)

# chart
plot_pareto_combo(
  DT                = d311,
  x_col             = complaint_type,
  title             = "Pareto Analysis of Complaint Types (Top 10)",
  filename          = "SR_by_complaint_type_pareto_combo_chart.pdf",
  chart_dir         = chart_dir,
  top_n             = 10,
  show_labels       = FALSE,   # no count labels on the bars
  show_threshold_80 = FALSE,
  x_label_wrap      = 18,       # break long complaint names over two lines
  x_axis_label_size = 5
  
)

################################################################################
# Determine status of SRs
cat("\n\nSRs by Status (including NA if present)\n")

# Build a labeled status on the fly:
status_summary_dt <- d311[
  , .(count = .N),
  by = .(status = fcase(
    is.na(status),                "NA",
    nzchar(as.character(status)), as.character(status),
    default = "(blank)"
  ))
][order(-count)]

# Percentages from counts
denom <- status_summary_dt[, sum(count)]
status_summary_dt[, `:=`(
  percentage            = round(100 * count / denom, 2),
  cumulative_percentage = round(100 * cumsum(count) / denom, 2)
)]

# Console print (pretty counts)
to_print <- copy(status_summary_dt)
to_print_df <- data.frame(
  status = format(to_print$status, justify = "left"),
  count = format(to_print$count, big.mark = ",", justify = "right"),
  percent = format(to_print$percent, justify = "right"),
  cumulative_percent = format(to_print$cumulative_percent, justify = "right")
)
print(to_print_df, row.names = FALSE)

################################################################################

cat("\n\n**********VALIDATING DATA TYPES**********\n")

################################################################################
# determine if the incident_zip field contain 5 numeric digits
# Find non-compliant ZIP5 values (format-only; no mutation)
progress_msg("Step 10 of 29: Validating data types")

find_noncompliant_zip5 <- function(DT, zip_col = "incident_zip", sample_n = 10L, 
                                   include_na = FALSE) {
  stopifnot(is.data.table(DT))
  stopifnot("unique_key" %in% names(DT), zip_col %in% names(DT))
  
  x <- trimws(DT[[zip_col]])                 # incident_zip is character
  is_na    <- is.na(x)
  zip_len  <- nchar(x, keepNA = TRUE)
  is_len5  <- zip_len == 5L
  is_digit <- !is_na & grepl("^[0-9]+$", x)
  ok       <- is_len5 & is_digit
  
  # indices to include as "non-compliant"
  bad_idx <- if (include_na) which(!ok | is_na) else which(!is_na & !ok)
  
  # Reason labels (format-only). 'NA' reason only used if include_na = TRUE
  reason <- fcase(
    !is_na & x == "",   "blank",
    !is_na & !is_len5,  "wrong_length",
    !is_na & !is_digit, "non_digit",
    is_na,              "NA",
    default = NA_character_
  )
  
  noncompliant <- DT[bad_idx, .(unique_key, agency, 
                    incident_zip = get(zip_col))][, `:=`(reason = reason[bad_idx], 
                    zip_length = zip_len[bad_idx])]
  setindex(noncompliant, unique_key)         # no reorder
  
  # Summaries
  by_reason <- noncompliant[, .N, by = reason][order(-N)]
  by_agency <- noncompliant[, .N, by = agency][order(-N)]
  
  # Console
  cat("\nNon-compliant ", zip_col, " values (format-only",
      if (!include_na) "; NA excluded", "): ",
      format(nrow(noncompliant), big.mark=","), "\n", sep = "")
  if (nrow(noncompliant)) {
    cat("\nBy reason:\n");  print(by_reason, nrows = nrow(by_reason))
    cat("\nExamples:\n");   print(noncompliant[1:min(.N, sample_n)], 
                                  nrows = min(.N, sample_n))
    cat("\nBy agency (top 10):\n"); print(by_agency[1:min(10L, .N)])
  }
  
  invisible(list(rows = noncompliant, by_reason = by_reason, 
                 by_agency = by_agency))
}

# Call (NAs are NOT counted as invalid):
res <- find_noncompliant_zip5(d311, zip_col = "incident_zip", sample_n = 10L)

################################################################################
# determine if various fields are numeric values

# Type-only: TRUE if every non-NA token is numeric text 
# Options let you decide how strict to be without touching range.
are_all_numbers <- function(x, allow_na = TRUE, allow_nonfinite = FALSE, 
                            allow_sci = TRUE) {
  if (is.numeric(x)) {
    nf_ok <- allow_nonfinite || all(is.finite(x[!is.na(x)]))
    na_ok <- allow_na || all(!is.na(x))
    return(nf_ok && na_ok)
  }
  s <- trimws(as.character(x))
  pattern <- if (allow_sci)
    "^[+-]?(?:\\d+\\.?\\d*|\\d*\\.\\d+)(?:[eE][+-]?\\d+)?$" # scientific notation
  else
    "^[+-]?(?:\\d+\\.?\\d*|\\d*\\.\\d+)$"
  ok <- nzchar(s) & grepl(pattern, s)
  if (allow_na) all(ok | is.na(s)) else all(ok & !is.na(s))
}

cols_to_check <- c("x_coordinate_state_plane", "y_coordinate_state_plane", 
                   "latitude", "longitude")
checks <- sapply(cols_to_check, function(col) are_all_numbers(d311[[col]]))

cat("\n\nType-only numeric checks:\n")
checks_df <- data.frame(
  column = names(checks),
  is_numeric = checks
)
print(checks_df, row.names = FALSE)

################################################################################

cat("\n\n********** Latitude/Longitude Precision Analysis **********\n")

################################################################################
# --- Analyze decimal precision in lat/lon fields ---
progress_msg("Step 11 of 29: Lat/long decimal precision")

##################
# Analyze latitude
cat("\n--- Latitude Precision Analysis ---\n")
lat_precision <- analyze_decimal_precision(
  DT             =  d311,
  field_name     = "latitude",
  verbose        = TRUE,
  chart_dir      = chart_dir,       
  generate_plots = TRUE   
  )

##################
# Analyze longitude
cat("\n--- Longitude Precision Analysis ---\n")
lon_precision <- analyze_decimal_precision(
  DT             =  d311,
  field_name     = "longitude",
  verbose        = TRUE,
  chart_dir      = chart_dir,       
  generate_plots = TRUE   
)

##################
# Combined summary statistics
cat("\n--- Summary Statistics ---\n")
if (!is.null(lat_precision)) {
  cat(sprintf("Latitude:\n Min precision: %d,  Max precision: %d,  
              Median: %.2f\n", 
              min(lat_precision$summary_precision$decimal_places),
              max(lat_precision$summary_precision$decimal_places),
              median(lat_precision$summary_precision$decimal_places)))  
}
if (!is.null(lon_precision)) {
  cat(sprintf("Longitude:\n Min precision: %d,  Max precision: %d,  
              Median: %.2f\n", 
              min(lon_precision$summary_precision$decimal_places),
              max(lon_precision$summary_precision$decimal_places),
              median(lon_precision$summary_precision$decimal_places)))
}

##################
# Create a combined comparison table
if (!is.null(lat_precision) && !is.null(lon_precision)) {
  cat("\n--- Side-by-side Comparison ---\n")
  
  # Get all unique decimal place values
  all_decimals <- sort(unique(c(lat_precision$summary_precision$decimal_places, 
                              lon_precision$summary_precision$decimal_places)))
  
  comparison <- data.table(decimal_places = all_decimals)
  
  # Merge latitude data - reference the summary_precision data.table directly
  comparison <- merge(comparison, 
                      lat_precision$summary_precision[, .(decimal_places, 
                                                          lat_count = N, 
                                                          lat_pct = pct)],
                      by = "decimal_places", all.x = TRUE)
  
  # Merge longitude data - reference the summary_precision data.table directly
  comparison <- merge(comparison,
                      lon_precision$summary_precision[, .(decimal_places, 
                                                          lon_count = N, 
                                                          lon_pct = pct)],
                      by = "decimal_places", all.x = TRUE)
  
  # Replace NAs with 0
  comparison[is.na(lat_count), `:=`(lat_count = 0, lat_pct = 0.00)]
  comparison[is.na(lon_count), `:=`(lon_count = 0, lon_pct = 0.00)]
  
  # Sort by decimal places
  setorder(comparison, decimal_places)
  
  # Calculate cumulative percentages
  comparison[, lat_cum_pct := cumsum(lat_pct)]
  comparison[, lon_cum_pct := cumsum(lon_pct)]
  
  # Create total row
  total_row <- data.table(
    decimal_places = NA_integer_,  # Keep as integer initially
    lat_count = sum(comparison$lat_count),
    lat_pct = sum(comparison$lat_pct),
    lat_cum_pct = NA_real_,
    lon_count = sum(comparison$lon_count),
    lon_pct = sum(comparison$lon_pct),
    lon_cum_pct = NA_real_
  )
  
  # Combine with total row
  comparison_with_total <- rbindlist(list(comparison, total_row), 
                                     use.names = TRUE)
  
  # Convert decimal_places to character BEFORE assigning "TOTAL"
  comparison_with_total[, decimal_places := as.character(decimal_places)]
  comparison_with_total[is.na(decimal_places), decimal_places := "TOTAL"]
  
  print(comparison_with_total, row.names = FALSE)
}

################################################################################

cat("\n\n**********CHECKING FOR DUPLICATE VALUES**********\n")

################################################################################
progress_msg("Step 12 of 29: Checking fields for duplicates")

# Filter complete cases
valid_data <- d311[!is.na(location) & !is.na(latitude) & !is.na(longitude)]
if (nrow(valid_data) == 0) {
  cat("No complete location data found for comparison.\n")
  redundancy_pct <- 0
} else {
  # Use strcapture for safer parsing of "(lat, lon)" strings
  coords <- strcapture(
    pattern = "^\\(([^,]+),\\s*([^)]+)\\)$",
    x = valid_data$location,
    proto = list(lat = numeric(), lon = numeric())
  )
  
  # Combine safely into valid_data
  valid_data[, `:=`(
    location_lat = coords$lat,
    location_lon = coords$lon
  )]
  
  # Drop rows where regex failed
  complete_rows <- valid_data[!is.na(location_lat) & !is.na(location_lon)]
  
  total_rows <- nrow(complete_rows)
  
  if (total_rows == 0) {
    cat("No rows with extractable location coordinates found.\n")
    redundancy_pct <- 0
  } else {
    # Round to 5 decimal places, then allow small variance (±0.00001)
    complete_rows[, lat_match := abs(round(as.numeric(latitude), 6) - 
                               round(as.numeric(location_lat), 6)) <= 0.000001]
    complete_rows[, lon_match := abs(round(as.numeric(longitude), 6) - 
                               round(as.numeric(location_lon), 6)) <= 0.000001]
    complete_rows[, both_match := lat_match & lon_match]
    
    lat_matches  <- sum(complete_rows$lat_match, na.rm = TRUE)
    lon_matches  <- sum(complete_rows$lon_match, na.rm = TRUE)
    exact_matches <- sum(complete_rows$both_match, na.rm = TRUE)
    redundancy_pct <- 100 * exact_matches / total_rows
    
    cat("\nLocation redundancy check (5 decimals ±0.00001):\n")
    cat(sprintf("  Complete rows analyzed: %d\n", total_rows))
    cat(sprintf("  Latitude matches: %d (%.2f%%)\n", lat_matches, 
                100 * lat_matches / total_rows))
    cat(sprintf("  Longitude matches: %d (%.2f%%)\n", lon_matches, 
                100 * lon_matches / total_rows))
    cat(sprintf("  Both match exactly: %d (%.2f%%)\n", exact_matches, 
                redundancy_pct))
    
    if (redundancy_pct > 99) {
      cat("\n  → REDUNDANT: Location column is duplicate of lat/lon columns\n")
    } else {
      cat("  → NOT REDUNDANT: Location column contains different information\n")
      mismatches <- complete_rows[!both_match][1:3]
      if (nrow(mismatches) > 0) {
        cat("\nSample mismatches:\n")
        print(mismatches[, .(latitude, longitude, location, location_lat, 
                             location_lon)])
      }
    }
  }
}

free_objects("valid_data", "complete_rows", "coords")

################################################################################
# check to see if there are any non-matches between 'borough' and 'park_borough'

# reference + duplicates
reference_field <- "borough"
dup_fields <- c("park_borough", "taxi_company_borough")

# (optional) sanity check that columns exist
missing <- setdiff(c(reference_field, dup_fields), names(d311))
if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "))

# Run the checks (NA==NA counted as match; text normalized; Pareto by agency)
res_list <- lapply(dup_fields, function(dup) {
  detect_duplicates(
    d311,
    reference_field    = reference_field,
    duplicate_field    = dup,
    trim_ws            = TRUE,
    case_insensitive   = TRUE,
    blank_is_na        = TRUE,
    na_equal           = TRUE,          # <-- count matching NAs as matches
    percent_base       = "all_rows",
    make_pareto        = TRUE,          # <-- Pareto for non-matches
    pareto_group_col   = "agency",
    chart_dir          = chart_dir
  )
})
names(res_list) <- dup_fields

# Roll up your key metric: % duplication (matches) for each duplicate field
dup_summary <- data.table::rbindlist(lapply(seq_along(res_list), function(i) {
  data.table::data.table(
    duplicate_field = names(res_list)[i],
    pct_duplication = res_list[[i]]$stats$pct_matching
  )
}))

print(dup_summary[order(-pct_duplication)])


################################################################################

cat("\n\n**********CHECKING FOR ALLOWABLE AND VALID VALUES**********\n")

################################################################################
# Check lat/long values to see if they are within NYC city limits
progress_msg("Step 13 of 29: Checking allowable and valid values")

################################################################################
# Check for non-agreement with closed 'status' and 'close_date'

status_res <- report_status_closed_date_exceptions(
  d311,
  status_col    = "status",
  closed_col    = "closed_date",
  closed_values = c("Closed"),
  id_col        = "unique_key",
  n_show        = 10,
  make_charts   = TRUE,
  chart_dir     = file.path(chart_dir),
  x_col_name    = "agency",      # or "complaint_type", etc.
  top_n         = 30,
  flip          = FALSE,
  min_count     = 1
)

################################################################################
# --- NYC bounding box (from NYBBWI metadata) ---
# Source: New York City Borough Boundary — Water Included (NYBBWI) dataset. 
nyc_lat_north <- 40.917691
nyc_lat_south <- 40.477211
nyc_lon_west  <- -74.260380
nyc_lon_east  <- -73.699211

flag_out_of_bounds_latlon <- function(
    DT,
    lat_col = "latitude",
    lon_col = "longitude",
    bounds = list(
      lat = c(nyc_lat_south, nyc_lat_north),   # south, north
      lon = c(nyc_lon_west, nyc_lon_east)      # west, east
    ),
    id_cols = c("unique_key", "agency", "city"),
    sample_n = 20L,
    tolerance_m = 100    # ≈100 m margin
) {
  stopifnot(is.data.table(DT))
  stopifnot(lat_col %in% names(DT), lon_col %in% names(DT))
  
  # Convert tolerance (m) → degrees
  mean_lat <- mean(bounds$lat)
  deg_lat <- tolerance_m / 111000
  deg_lon <- tolerance_m / (111000 * cos(mean_lat * pi / 180))
  
  lat_min <- bounds$lat[1] - deg_lat
  lat_max <- bounds$lat[2] + deg_lat
  lon_min <- bounds$lon[1] - deg_lon
  lon_max <- bounds$lon[2] + deg_lon
  
  # Numeric conversion if needed (no rounding)
  lat_num <- suppressWarnings(as.numeric(DT[[lat_col]]))
  lon_num <- suppressWarnings(as.numeric(DT[[lon_col]]))
  
  # Identify out-of-bounds points
  lat_out_idx <- which(!is.na(lat_num) & !between(lat_num, lat_min, lat_max))
  lon_out_idx <- which(!is.na(lon_num) & !between(lon_num, lon_min, lon_max))
  both_out_idx <- intersect(lat_out_idx, lon_out_idx)
  
  keep_cols <- intersect(c(id_cols, lat_col, lon_col), names(DT))
  bad_lat  <- DT[lat_out_idx, ..keep_cols]
  bad_lon  <- DT[lon_out_idx, ..keep_cols]
  bad_both <- DT[both_out_idx, ..keep_cols]
  
  n_total <- nrow(DT)
  n_lat   <- nrow(bad_lat)
  n_lon   <- nrow(bad_lon)
  n_both  <- nrow(bad_both)
  
  cat("\nOut-of-bounds check (raw coords, ±", tolerance_m, " m buffer):\n", 
      sep = "")
  cat("Latitude out-of-bounds:  ", format(n_lat, big.mark=","), " (",
      round(100 * n_lat / n_total, 4), "%)\n", sep = "")
  if (n_lat) print(bad_lat[1:min(.N, sample_n)], nrows = min(n_lat, sample_n))
  
  cat("\nLongitude out-of-bounds: ", format(n_lon, big.mark=","), " (",
      round(100 * n_lon / n_total, 4), "%)\n", sep = "")
  if (n_lon) print(bad_lon[1:min(.N, sample_n)], nrows = min(n_lon, sample_n))
  
  if (n_both) {
    cat("\nBoth lat & lon out-of-bounds: ", format(n_both, big.mark=","), " (",
        round(100 * n_both / n_total, 4), "%)\n", sep = "")
    print(bad_both[1:min(.N, sample_n)], nrows = min(n_both, sample_n))
  }
  
  invisible(list(bad_lat = bad_lat, bad_lon = bad_lon, bad_both = bad_both))
}

# --- Example call ---
res <- flag_out_of_bounds_latlon(
  d311,
  lat_col = "latitude",
  lon_col = "longitude",
  bounds = list(
    lat = c(nyc_lat_south, nyc_lat_north),
    lon = c(nyc_lon_west, nyc_lon_east)
  ),
  id_cols = c("unique_key", "agency", "city"),
  sample_n = 20L,
  tolerance_m = 100
)

# Remove unused fields type immediately
d311[, c(
  "latitude",
  "longitude",
  "location"
) := NULL]  # Remove immediately

################################################################################
# Check to see if any of the x or y state plane coordinates fall outside 
# the extreme points of New York City.
# Define the latitude and longitude points.
# Build points data frame using those variables
points_df <- data.frame(
  name = c("Northernmost", "Easternmost", "Southernmost", "Westernmost"),
  lat  = c(nyc_lat_north, nyc_lat_south, nyc_lat_south, nyc_lat_north),
  lon  = c(nyc_lon_east,  nyc_lon_east,  nyc_lon_west,  nyc_lon_west)
)

# Convert to an sf object and apply the State Plane projection.
# WGS84 lat/long.
points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326) 
points_sp <- st_transform(points_sf, crs = 2263) # Convert to State Plane

# Change the x/y_coordinate_state_plane fields to "numeric" to enable comparison.
d311$x_coordinate_state_plane <- as.numeric(d311$x_coordinate_state_plane)
d311$y_coordinate_state_plane <- as.numeric(d311$y_coordinate_state_plane)

# Extract the bounding box (xmin, ymin, xmax, ymax) from the sf object
bbox <- st_bbox(points_sp)

# Assign the individual values to variables
xmin <- as.numeric(bbox["xmin"])
xmax <- as.numeric(bbox["xmax"])
ymin <- as.numeric(bbox["ymin"])
ymax <- as.numeric(bbox["ymax"])

# Filter NA values
d311_clean <- d311[!is.na(d311$x_coordinate_state_plane) & 
                     !is.na(d311$y_coordinate_state_plane), ]

# Check for x-coordinate outliers
x_outliers <- d311_clean[
  d311_clean$x_coordinate_state_plane < xmin |
    d311_clean$x_coordinate_state_plane > xmax,
]

# Check for y-coordinate outliers
y_outliers <- d311_clean[
  d311_clean$y_coordinate_state_plane < ymin |
    d311_clean$y_coordinate_state_plane > ymax,
]

# Print status for x-coordinate outliers
if (nrow(x_outliers) == 0) {
  cat("\nAll x_coordinate_state_plane values lie within the boundaries of NYC.")
} else {
  cat("\n\nThere is/are", nrow(x_outliers), 
      "x_coordinate_state_plane values outside the boundaries of NYC.\n")
  cat("\nHere are the first few rows of x-coordinate outliers:\n")
  print(head(x_outliers[, c("unique_key", "agency", "x_coordinate_state_plane")]))
}

# Print status for y-coordinate outliers
if (nrow(y_outliers) == 0) {
  cat("\nAll y_coordinate_state_plane values lie within the boundaries of NYC.")
} else {
  cat("\n\nThere are", nrow(y_outliers), 
      "y_coordinate_state_plane values outside the boundaries of NYC.\n")
  cat("\nHere are the first few rows of y-coordinate outliers:\n")
  print(head(y_outliers[, c("unique_key", "agency", "y_coordinate_state_plane")]))
}

# Remove unused fields type immediately
d311[, c(
  "y_coordinate_state_plane", 
  "x_coordinate_state_plane"
  ) := NULL]  # Remove immediately

free_objects("d311_clean", "x_outliers", "y_outliers")

################################################################################
# --- Allowed sets -------------------------------------------------------------

valid_address_types <- c(
  "ADDRESS","BBL","BLOCKFACE","INTERSECTION","PLACENAME","UNRECOGNIZED"
)

valid_statuses <- c(
  "ASSIGNED","CANCEL","CLOSED","IN PROGRESS","OPEN","PENDING","STARTED",
  "UNSPECIFIED"
)

valid_boroughs <- c(
  "BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND","UNSPECIFIED"
)

valid_channels <- c("MOBILE","ONLINE","OTHER","PHONE","UNKNOWN")

valid_vehicle_types <- c(
  "AMBULETTE / PARATRANSIT","CAR","CAR SERVICE","COMMUTER VAN",
  "GREEN TAXI","OTHER","SUV","TRUCK","VAN"
)

# check for allowable values in the 'community_board' field
valid_community_boards <-
  c(
    "01 BRONX", "01 BROOKLYN", "01 MANHATTAN", "01 QUEENS", "01 STATEN ISLAND",
    "02 BRONX", "02 BROOKLYN", "02 MANHATTAN", "02 QUEENS", "02 STATEN ISLAND",
    "03 BRONX", "03 BROOKLYN", "03 MANHATTAN", "03 QUEENS", "03 STATEN ISLAND",
    "04 BRONX", "04 BROOKLYN", "04 MANHATTAN", "04 QUEENS",
    "05 BRONX", "05 BROOKLYN", "05 MANHATTAN", "05 QUEENS",
    "06 BRONX", "06 BROOKLYN", "06 MANHATTAN", "06 QUEENS",
    "07 BRONX", "07 BROOKLYN", "07 MANHATTAN", "07 QUEENS",
    "08 BRONX", "08 BROOKLYN", "08 MANHATTAN", "08 QUEENS",
    "09 BRONX", "09 BROOKLYN", "09 MANHATTAN", "09 QUEENS",
    "10 BRONX", "10 BROOKLYN", "10 MANHATTAN", "10 QUEENS",
    "11 BRONX", "11 BROOKLYN", "11 MANHATTAN", "11 QUEENS",
    "12 BRONX", "12 BROOKLYN", "12 MANHATTAN", "12 QUEENS",
    "13 BROOKLYN", "13 QUEENS",
    "14 BROOKLYN", "14 QUEENS",
    "15 BROOKLYN",
    "16 BROOKLYN",
    "17 BROOKLYN",
    "18 BROOKLYN",
    "UNSPECIFIED BRONX", "UNSPECIFIED BROOKLYN", "UNSPECIFIED MANHATTAN",
    "UNSPECIFIED QUEENS", "UNSPECIFIED STATEN ISLAND",
    "0 UNSPECIFIED"
  )

# Check for invalid zip codes in d311$incident_zip using USPSzipcodesOnly
valid_USPS_zipcodes <- as.list(USPSzipcodes$zip)

# Field to include "agency" in the computed dataset
valid_agencies <- unique(d311$agency)

# --- Centralized spec: field -> allowed values --------------------------------

valid_spec <- list(
  agency                 = valid_agencies,        # Add this line
  address_type           = valid_address_types,
  status                 = valid_statuses,
  borough                = valid_boroughs,
  park_borough           = valid_boroughs,
  taxi_company_borough   = valid_boroughs,
  open_data_channel_type = valid_channels,
  vehicle_type           = valid_vehicle_types,
  community_board        = valid_community_boards,
  incident_zip           = valid_USPS_zipcodes
)

# --- Run all validations in one pass ------------------------------------------

all_validation_results <- lapply(names(valid_spec), function(fld) {
  validate_values(
    x             = d311[[fld]],
    allowed       = valid_spec[[fld]],
    field         = fld,
    ignore_case   = TRUE,
    use_fastmatch = TRUE,
    quiet         = FALSE
  )
})

names(all_validation_results) <- names(valid_spec)

# --- Generate Pareto charts for fields with invalid data ------------------
cat("\nGenerating Pareto charts for fields with invalid data...\n")

for (field_name in names(all_validation_results)) {
  result <- all_validation_results[[field_name]]
  
  # Only create chart if there are invalid values
  if (result$counts$invalid > 0) {
    cat(sprintf("\nCreating Pareto chart for: %s (%d invalid values)\n", 
                field_name, result$counts$invalid))
    
    # Create mask for rows with invalid values for this field
    invalid_mask <- d311[[field_name]] %in% result$invalid_values
    invalid_rows <- d311[invalid_mask]
    
    cat("Number of invalid rows found:", nrow(invalid_rows), "\n")
    
    if (nrow(invalid_rows) > 0) {
      plot_pareto_combo(
        DT = invalid_rows,
        x_col = agency,
        chart_dir = chart_dir,
        filename = paste0("pareto_agency_", field_name, ".pdf"),
        title = sprintf("Agencies with Invalid %s Values", field_name),
        top_n = 30,
        show_labels       = TRUE,
        show_threshold_80 = TRUE,   # whether to draw the 80% reference line
        include_na = TRUE
      )
    }
  } else {
    cat(sprintf("Skipping %s - no invalid values found\n", field_name))
  }
}

free_objects("invalid_rows", "invalid_mask", "USPSzipcodes", 
             "valid_USPS_zipcodes")

# --- Preserve your original per-field variables (for drop-in compatibility) ----

address_type_results          <- all_validation_results[["address_type"]]
statusResults                 <- all_validation_results[["status"]]
boroughResults                <- all_validation_results[["borough"]]
park_boroughResults           <- all_validation_results[["park_borough"]]
taxi_company_boroughResults <- all_validation_results[["taxi_company_borough"]]
open_data_channelResults    <- all_validation_results[["open_data_channel_type"]]
vehicle_typeResults           <- all_validation_results[["vehicle_type"]]
community_boardResults        <- all_validation_results[["community_board"]]   
incident_zipResults           <- all_validation_results[["incident_zip"]]

# --- Compact summary table (programmatic dashboard) ----------------------------
summary_dt <- data.table::rbindlist(
  lapply(all_validation_results, function(r) {
    data.table::data.table(
      field       = r$field,
      valid       = r$valid,
      invalid     = r$counts$invalid,
      nonblank    = r$counts$nonblank,
      pct_invalid = r$pct_invalid_nonblank
    )
  }),
  use.names = TRUE, fill = TRUE
)[order(-invalid, field)]

cat("\nValidation summary (sorted by invalid desc):\n")
summary_df <- data.frame(
  field = format(summary_dt$field, justify = "left"),
  valid = format(summary_dt$valid, justify = "right"),
  invalid = format(summary_dt$invalid, justify = "right"),
  nonblank = format(summary_dt$nonblank, justify = "right"),
  pct_invalid = format(summary_dt$pct_invalid, justify = "right")
)
print(summary_df, row.names = FALSE)

# Remove unused fields type immediately
d311[, c(
  "address_type", 
  "open_data_channel_type",
  "incident_zip",
  "vehicle_type") := NULL]  # Remove immediately

################################################################################

cat("\n\n**********CHECKING FOR DATE FIELD ISSUES **********\n")

################################################################################

# ==============================================================================
# SECTION 0: DURATION CALCULATIONS -- Do this first
# ==============================================================================
# Calculate service request durations between created_date and closed_date
# Uses America/New_York timezone for consistent temporal calculations and DST

progress_msg("Step 14 of 29: Date fields: calculating SR durations")
cat("\n=== CALCULATING SR DURATIONS FOR LATER USE ===\n")

calculate_durations(d311, "created_date", "closed_date",
                    tz = "America/New_York", in_place = TRUE)

# ==============================================================================
# SECTION 1: CREATED DATE ANALYSIS
# ==============================================================================

progress_msg("Step 15 of 29: Date fields: created_date")
cat("\n=== SUMMARY DATE ANALYSIS ===\n")

# Assuming date_cols is a character vector of column names
date_cols <- c(
  "resolution_action_updated_date",
  "due_date",
  "created_date", 
  "closed_date"
)

for (col in date_cols) {
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("Date Field:", col, "\n")
  cat(rep("=", 60), "\n\n", sep = "")
  
  # Extract year and create frequency table
  years <- lubridate::year(d311[[col]])
  freq_table <- table(years)
  
  # Create data.table for plotting
  summary_dt <- data.table(
    year = as.integer(names(freq_table)),
    count = as.integer(freq_table)
  )
  
  # Special handling for closed_date to prevent timeline plotting
  if (col == "closed_date") {
    summary_dt$year <- factor(summary_dt$year)
  }
  
  # Call plot_barchart with the proper parameters
  plot_barchart(
    DT = summary_dt,
    x_col = "year",
    y_col = "count",
    title = paste("Year Distribution -", col),
    show_labels = TRUE,
    x_label = "",
    y_label = "",
    console_print_title = paste("Year Distribution for", col),
    chart_dir = chart_dir,
    filename = paste0("Yearly_Distribution - ", col)
    
  )
  
  cat("\n")
}

free_objects("years", "freq_table")

cat("\n=== CREATED DATE ANALYSIS ===\n")


# total rows in d311
total_rows <- num_rows_d311

fmt_count_pct <- function(n, total) {
  # Handle NA or zero totals gracefully
  if (is.null(total) || is.na(total) || total == 0) {
    return(sprintf("%s (n/a)", prettyNum(n, big.mark = ",")))
  }
  
  pct <- 100 * n / total
  sprintf("%s (%.2f%%)", prettyNum(n, big.mark = ","), pct)
}


  # Anomaly checks
  future_created_dates        <- d311[created_date > as_of_date]
  past_created_dates          <- d311[created_date < genesis_date]
  missing_created_dates       <- d311[is.na(created_date)]
  midnight_only_created_dates <- d311[format(created_date, 
                                             "%H:%M:%S") == "00:00:00"]
  noon_only_created_dates     <- d311[format(created_date, 
                                             "%H:%M:%S") == "12:00:00"]

  # Summary
  cat("Created_date anomaly check (tz = America/New_York):\n")
  cat("  Future created dates:   ", 
      fmt_count_pct(nrow(future_created_dates), total_rows), "\n")
  cat("  Past created dates:     ", 
      fmt_count_pct(nrow(past_created_dates), total_rows), "\n")
  cat("  Midnight-only created:  ", 
      fmt_count_pct(nrow(midnight_only_created_dates), total_rows), "\n")
   cat("  Noon-only created:      ", 
      fmt_count_pct(nrow(noon_only_created_dates), total_rows), "\n")
  
  # Define anomaly datasets and their labels
  anomaly_list_created <- list(
    list(
      dt = future_created_dates,
      label = "Future Created Dates",
      condition = "Created in Future"
    ),
    list(
      dt = past_created_dates,
      label = paste0("Past Created Dates: before ",
                     format(genesis_date, "%Y-%m-%d"), " (311 launch)"),
      condition = "Created Before 311 launch"
    ),
    list(
      dt = midnight_only_created_dates,
      label = "Midnight Created Dates",
      condition = "Created at Midnight"
    ),
    list(
      dt = noon_only_created_dates,
      label = "Noon Created Dates",
      condition = "Created at Noon"
    )
  )
  
  # Loop through and create charts
  # Simplified loop
  for (anomaly in anomaly_list_created) {
    if (nrow(anomaly$dt) > 0) {
      plot_date_field_analysis(
        DT = anomaly$dt,
        date_col = created_date,
        group_col = agency,
        chart_dir = chart_dir,
        label = anomaly$label,
        condition_text = anomaly$condition,
        min_agency_count = 2,
        top_n = 30
      )
    } else {
      cat(sprintf("\nSkipping %s - no records\n", anomaly$label))
    }
  }
  
  # Handle missing_created_dates separately
  if (nrow(missing_created_dates) > 0) {
    cat("\n=== Missing Created Dates Summary ===\n")
    cat(sprintf("Total records with missing created_date: %s\n", 
                format(nrow(missing_created_dates), big.mark = ",")))
    
    missing_summary <- missing_created_dates[, .N, by = agency][order(-N)]
    cat("\nTop agencies with missing created dates:\n")
    print(head(missing_summary, 10))
  } else {
    cat("\n=== Missing Created Dates Summary ===\n")
    cat("No records with missing created_date found.\n")
  }
  
  # end of Section 1 (created)
  free_objects("future_created_dates", "past_created_dates", "missing_created_dates",
               "midnight_only_created_dates", "noon_only_created_dates",
               "anomaly_list_created")
    
# ==============================================================================
# SECTION 2: DUE DATE ANALYSIS
# ==============================================================================

# Identify service requests with due_date before created_date
# Logical impossibility indicating data entry errors

progress_msg("Step 16 of 29: Date fields: due_date")
cat("\n=== DUE DATE ANALYSIS ===\n")
  
  # Anomaly checks
  future_due_dates        <- d311[due_date > as_of_date]
  past_due_dates          <- d311[due_date < genesis_date]
  missing_due_dates       <- d311[is.na(due_date)]
  midnight_only_due_dates <- d311[format(due_date, "%H:%M:%S") == "00:00:00"]
  noon_only_due_dates     <- d311[format(due_date, "%H:%M:%S") == "12:00:00"]
  
  # Exclude NAs from both columns before the comparison
  # Create the subset with the new column
  due_before_created <- d311[
    !is.na(due_date) & 
      !is.na(created_date) & 
      due_date < created_date
  ]
  
  # Summary
  cat("Due_date anomaly check (tz = America/New_York):\n")
  cat("  Future dates:   ", fmt_count_pct(nrow(future_due_dates), 
                                          total_rows), "\n")
  cat("  Past dates:     ", fmt_count_pct(nrow(past_due_dates), 
                                          total_rows), "\n")
  cat("  Missing dates:  ", fmt_count_pct(nrow(missing_due_dates), 
                                          total_rows), "\n")
  cat("  Midnight-only:  ", fmt_count_pct(nrow(midnight_only_due_dates), 
                                          total_rows), "\n")
  cat("  Noon-only:      ", fmt_count_pct(nrow(noon_only_due_dates), 
                                          total_rows), "\n")
  cat("  Due < Created   ", fmt_count_pct(nrow(due_before_created), 
                                          total_rows), "\n")
  
  # Define anomaly datasets and their labels for due_date
  anomaly_list_due <- list(
    list(
      dt = future_due_dates,
      label = "Future Due Dates",
      condition = "Due in Future"
    ),
    list(
      dt = past_due_dates,
      label = "Past Due Dates",
      condition = "Due Before Genesis"
    ),
    list(
      dt = midnight_only_due_dates,
      label = "Midnight Due Dates",
      
      condition = "Due at Midnight"
    ),
    list(
      dt = noon_only_due_dates,
      label = "Noon Due Dates",
      condition = "Due at Noon"
    ),
    list(
      dt = due_before_created,
      label = "Due Dates before Created Date",
      condition = "Due < Created"
    )
  )
  
  # Loop through and create charts
  for (anomaly in anomaly_list_due) {
    if (nrow(anomaly$dt) > 0) {
      plot_date_field_analysis(
        DT = anomaly$dt,
        date_col = due_date,
        group_col = agency,
        chart_dir = chart_dir,
        label = anomaly$label,
        condition_text = anomaly$condition,
        min_agency_count = 2,
        top_n = 30
      )
    } else {
      cat(sprintf("\nSkipping %s - no records\n", anomaly$label))
    }
  }

  # Handle missing_due_dates separately
  if (nrow(missing_due_dates) > 0) {
    cat("\n=== Missing Due Dates Summary ===\n")
    cat(sprintf("Total records with missing due_date: %s\n", 
                format(nrow(missing_due_dates), big.mark = ",")))
    
    missing_summary <- missing_due_dates[, .N, by = agency][order(-N)]
    cat("\nTop agencies with missing due dates:\n")
    print(head(missing_summary, 10))
  } else {
    cat("\n=== Missing Due Dates Summary ===\n")
    cat("No records with missing due_date found.\n")
  }
 
  res <- report_due_before_created(
    DT = d311,
    chart_dir = chart_dir,
    boxplot_file = "due_before_created_boxplot.pdf",
    pareto_file = "due_before_created_pareto.pdf"
  )
  
  
  # end of Section 2 (due), after report_due_before_created()
  free_objects("future_due_dates", "past_due_dates", "missing_due_dates",
               "midnight_only_due_dates", "noon_only_due_dates",
               "due_before_created", "anomaly_list_due")
  
# ==============================================================================
# SECTION 3: RESOLUTION UPDATE DATE ANALYSIS
# ==============================================================================
# Check for resolution_action_updated_date occurring before created_date
# Excludes same-day cases where resolution time = 00:00:00 (likely defaults)

progress_msg("Step 17 of 29: Date fields: resolution_action_updated_date")
cat("\n=== RESOLUTION UPDATE DATE ANALYSIS ===\n")

# Anomaly checks
future_resolution_action_updated_dates        <- 
  d311[resolution_action_updated_date > as_of_date]
past_resolution_action_updated_dates          <- 
  d311[resolution_action_updated_date < genesis_date]
missing_resolution_action_updated_dates       <- 
  d311[is.na(resolution_action_updated_date)]
midnight_only_resolution_action_updated_dates <- 
  d311[format(resolution_action_updated_date, "%H:%M:%S") == "00:00:00"]
noon_only_resolution_action_updated_dates     <- 
  d311[format(resolution_action_updated_date, "%H:%M:%S") == "12:00:00"]  

# Exclude NAs from both columns in the comparison
updates_before_created <- d311[
  !is.na(resolution_action_updated_date) & 
  !is.na(created_date) & 
  resolution_action_updated_date < created_date
]

# Summary
cat("Resolution_action_updated_date anomaly check (tz = America/New_York):\n")
cat("  Future dates:   ", 
    fmt_count_pct(nrow(future_resolution_action_updated_dates), 
                  total_rows), "\n")
cat("  Past dates:     ", 
    fmt_count_pct(nrow(past_resolution_action_updated_dates), 
                  total_rows), "\n")
cat("  Missing dates:  ", 
    fmt_count_pct(nrow(missing_resolution_action_updated_dates), 
                  total_rows), "\n")
cat("  Midnight-only:  ", 
    fmt_count_pct(nrow(midnight_only_resolution_action_updated_dates), 
                  total_rows), "\n")
cat("  Noon-only:      ", 
    fmt_count_pct(nrow(noon_only_resolution_action_updated_dates), 
                  total_rows), "\n")
cat("  Updates < Created:", 
    fmt_count_pct(nrow(updates_before_created), total_rows), "\n")

# Define anomaly datasets and their labels for resolution_action_updated_date

anomaly_list_resolution <- list(
  list(
    dt = future_resolution_action_updated_dates,
    label = "Future Resolution Action Updated Dates",
    condition = "Resolution Action Updated in Future"
  ),
  list(
    dt = past_resolution_action_updated_dates,
    label = "Past Resolution Action Updated Dates",
    condition = "Resolution Action Updated Before 311 Genesis"
  ),
  list(
    dt = midnight_only_resolution_action_updated_dates,
    label = "Midnight Resolution Action Updated Dates",
    condition = "Resolution Action Updated at Midnight"
  ),
  list(
    dt = noon_only_resolution_action_updated_dates,
    label = "Noon Resolution Action Updated Dates",
    condition = "Resolution Action Updated at Noon"
  ),
  list(
    dt = updates_before_created,
    label = "Resolution Action Updated Dates before Created Date",
    condition = "Resolution Action Updated < Created"
    
  )
)

# Loop through and create charts
for (anomaly in anomaly_list_resolution) {
  if (nrow(anomaly$dt) > 0) {
    plot_date_field_analysis(
      DT = anomaly$dt,
      date_col = resolution_action_updated_date,
      group_col = agency,
      chart_dir = chart_dir,
      label = anomaly$label,
      condition_text = anomaly$condition,
      min_agency_count = 2,
      top_n = 20
    )
  } else {
    cat(sprintf("\nSkipping %s - no records\n", anomaly$label))
  }
}

# Handle missing_resolution_action_updated_dates separately
if (nrow(missing_resolution_action_updated_dates) > 0) {
  cat("\n=== Missing Resolution Action Updated Dates Summary ===\n")
  cat(sprintf("Total records with missing resolution_action_updated_date: %s\n", 
        format(nrow(missing_resolution_action_updated_dates), big.mark = ",")))
  
  missing_summary <- missing_resolution_action_updated_dates[, .N, 
                                                         by = agency][order(-N)]
  cat("\nTop agencies with missing resolution action updated dates:\n")
  print(head(missing_summary, 10))
} else {
  cat("\n=== Missing Resolution Action Updated Dates Summary ===\n")
  cat("No records with missing resolution_action_updated_date found.\n")
}

res <- report_resolution_update_before_created(
  DT = d311,
  created_col = "created_date",
  resolution_col = "resolution_action_updated_date",
  agency_col = "agency",
  unique_key_col = "unique_key",
  chart_dir = chart_dir,
  sample_size = 10,
  make_pareto = TRUE,
  make_boxplot = TRUE
)

# end of Section 3 (resolution), after report_resolution_update_before_created()
free_objects("future_resolution_action_updated_dates",
             "past_resolution_action_updated_dates",
             "missing_resolution_action_updated_dates",
             "midnight_only_resolution_action_updated_dates",
             "noon_only_resolution_action_updated_dates",
             "updates_before_created", "anomaly_list_resolution")

# ==============================================================================
# SECTION 4: POST-CLOSED RESOLUTION UPDATE ANALYSIS
# ==============================================================================
# Identify service requests with resolution updates occurring long after closure
# Flags potential process violations or data entry delays

progress_msg("Step 18 of 29: Date fields: post-closed resolution updates")
cat("\n=== POST-CLOSED RESOLUTION UPDATE ANALYSIS ===\n")

res <- report_post_closed_updates(
    DT = d311,
    resolution_action_threshold = 30,        # Days after closure
    too_large_threshold = 365 * 3,        # 3 years in days
    chart_dir = chart_dir
)

# ==============================================================================
# SECTION 5: CLOSED DATE ANALYSIS
# ==============================================================================
# Identify service requests with closed dates in the future
# Flags records where closed_date > max(created_date) + 1 day

progress_msg("Step 19 of 29: Date fields: closed_date")
cat("\n===  CLOSED DATE ANALYSIS ===\n")

# Anomaly checks
future_closed_dates           <- d311[closed_date > as_of_date]
past_closed_dates             <- d311[closed_date < genesis_date]
missing_closed_dates          <- d311[is.na(closed_date)]
midnight_only_closed_dates    <- 
  d311[format(closed_date, "%H:%M:%S") == "00:00:00"]
noon_only_closed_dates        <- 
  d311[format(closed_date, "%H:%M:%S") == "12:00:00"]
closed_before_created         <- d311[closed_date < created_date]

# Summary
cat("closed_date anomaly check (tz = America/New_York):\n")
cat("  Future closed dates:   ", 
    fmt_count_pct(nrow(future_closed_dates), total_rows), "\n")
cat("  Past closed dates:     ", 
    fmt_count_pct(nrow(past_closed_dates), total_rows), "\n")
cat("  Missing closed dates:  ", 
    fmt_count_pct(nrow(missing_closed_dates), total_rows), "\n")
cat("  Midnight-only closed:  ", 
    fmt_count_pct(nrow(midnight_only_closed_dates), total_rows), "\n")
cat("  Noon-only closed:      ", 
    fmt_count_pct(nrow(noon_only_closed_dates), total_rows), "\n")
cat("  Closed < Created:      ", 
    fmt_count_pct(nrow(closed_before_created), total_rows), "\n")


# Define anomaly datasets and their labels for closed_date
anomaly_list_closed <- list(
  list(
    dt = future_closed_dates,
    label = "Closed Dates in the Future",
    
    condition = "Closed in Future"
  ),
  list(
    dt = past_closed_dates,
    label = "Closed Dates in the Past",
    
    condition = "Closed Before 311 Genesis"
  ),
  list(
    dt = midnight_only_closed_dates,
    label = "Midnight Closed Dates",
    
    condition = "Closed at Midnight"
  ),
  list(
    dt = noon_only_closed_dates,
    label = "Noon Closed Dates",
    
    condition = "Closed at Noon"
  ),
  list(
    dt = closed_before_created,
    label = "Closed before Created",
    include_boxplot = TRUE,
    condition = "Closed < Created"
  )
)

# Loop through and create charts
for (anomaly in anomaly_list_closed) {
  if (nrow(anomaly$dt) > 0) {
    plot_date_field_analysis(
      DT = anomaly$dt,
      date_col = closed_date,
      group_col = agency,
      chart_dir = chart_dir,
      label = anomaly$label,
      condition_text = anomaly$condition,
      min_agency_count = 2,
      top_n = 20
    )
  } else {
    cat(sprintf("\nSkipping %s - no records\n", anomaly$label))
  }
}

# Handle missing_closed_dates separately
if (nrow(missing_closed_dates) > 0) {
  cat("\n=== Missing Closed Dates Summary ===\n")
  cat(sprintf("Total records with missing closed_date: %s\n", 
              format(nrow(missing_closed_dates), big.mark = ",")))
  
  missing_summary <- missing_closed_dates[, .N, by = agency][order(-N)]
  cat("\nTop agencies with missing closed dates:\n")
  print(head(missing_summary, 10))
} else {
  cat("\n=== Missing Closed Dates Summary ===\n")
  cat("No records with missing closed_date found.\n")
}

res <- report_future_closed(
  DT = d311,
  make_plots = TRUE,
  chart_dir = chart_dir,
  max_closed_date = max_closed_date,
  boxplot_file = "future_closed_boxplot.pdf",
  pareto_file = "future_closed_pareto.pdf"
)


# end of Section 5 (closed), after report_future_closed()
free_objects("future_closed_dates", "past_closed_dates", "missing_closed_dates",
             "midnight_only_closed_dates", "noon_only_closed_dates",
             "closed_before_created", "anomaly_list_closed", "missing_summary")

# ==============================================================================
# SECTION 6: DAYLIGHT SAVING TIME ANALYSIS
# ==============================================================================
# Analyze potential data quality issues related to DST transitions
# Examines both fall-back and spring-forward periods

progress_msg("Step 20 of 29: Date fields: Daylight Saving Time")
cat("\n\n=== DAYLIGHT SAVING TIME ANALYSIS ===\n")

# DST Fall-back analysis (November - clocks fall back)
cat("\n--- DST Fall-back Analysis ---\n")
dst_end_summary <- analyze_dst_fallback(
      DT            = d311,
      chart_dir     = chart_dir,
      show_examples = 10
)

# DST Spring-forward analysis (March - clocks spring forward)
cat("\n--- DST Spring-forward Analysis ---\n")
dst_start_summary <- analyze_dst_springforward(
    DT            = d311,
    chart_dir     = chart_dir,
    show_examples = 10
) 

################################################################################

progress_msg("Step 21 of 29: SR backlog by year")
result <- summarize_backlog(DT = d311, data_dir = data_dir)

################################################################################
# ==============================================================================
# SECTION 6: TEMPORAL PATTERN ANALYSIS
# ==============================================================================
# Analyze datetime patterns for both created_date and closed_date
# Identifies unusual temporal clustering or systematic patterns

progress_msg("Step 22 of 29: Temporal patterns (created and closed)")
cat("\n=== ANALYZING TEMPORAL PATTERNS ===\n")

# 1. Define the columns to iterate over (as strings)
date_cols <- c(
  "resolution_action_updated_date",
  "created_date", 
  "closed_date"
)

# 2. Define agency column name as a string (to be injected into function call)
agency_col_string <- "agency"

# 3. Initialize a list to store the results from each run
all_patterns_results <- list()

# 4. Loop through each date column
for (col_name in date_cols) {
  
  label <- gsub("_date|[^A-Za-z]", "", col_name) 
  label <- tools::toTitleCase(label)
  
  cat(sprintf("\n--- %s Date Pattern Analysis ---\n", label))
  
  result_name <- paste0(tolower(label), "_patterns")
  
  # Pass strings directly - NO as.name()
  results <- analyze_datetime_patterns(
    DT = d311,
    datetime_col = col_name,              # Just the string
    chart_dir = chart_dir,
    label = label,
    agency_col = agency_col_string        # Just the string
  )
  
  all_patterns_results[[result_name]] <- results
}

################################################################################

cat("\n\n********** DURATION ISSUES **********\n")

################################################################################

progress_msg("Step 23 of 29: Durations: positive durations")

# ==============================================================================
# SECTION 1: POSITIVE DURATION ANALYSIS
# ==============================================================================
# Analyze service requests with positive (normal) durations
# Creates histogram for visualization of completion time distributions

cat("\n=== ANALYZING POSITIVE DURATIONS ===\n")

# Filter to positive duration records only
positive_data <- d311[duration_days > 0 & !is.na(duration_days), 
                      .(created_date, closed_date, duration_days, 
                        complaint_type, agency)]
# Histogram of all positive durations (log x-axis, mean and median lines)
plot_histogram(
  DT          = positive_data,
  value_col   = "duration_days",
  chart_dir   = chart_dir,
  filename    = "positive_all_agencies.pdf",
  title       = "Distribution of Positive SR Durations - All City Agencies",
  x_label     = "Days (log scale)",
  log_x       = TRUE,
  bins        = 200,
  outlier_percentile = 1,
  show_mean   = TRUE,
  show_median = TRUE,
  stat_units  = " days",
  add_stats   = FALSE,
  print_summary = FALSE
)

# Create the summary and transpose it
summary_stats <- positive_data[, .(
  n = .N,
  median_days = median(duration_days),
  mean_days = mean(duration_days),
  sd_days = sd(duration_days),
  skewness = skewness(duration_days),
  p95 = quantile(duration_days, 0.95),
  p99 = quantile(duration_days, 0.99),
  max_days = max(duration_days)
)]

cat("\n=== Summary Statistics ===\n")
print(data.table(
  Statistic = c("N", "Median (days)", "Mean (days)", "Std Dev (days)", 
                "Skewness", "95th percentile", "99th percentile", "Maximum (days)"),
  Value = sprintf("%.4f", as.numeric(summary_stats[1,]))
))

cat("\n=== Bowley Skewness ===\n")
print(positive_data[, {
  q <- quantile(duration_days, c(0.25, 0.5, 0.75), na.rm = TRUE)
  .(bowley_skew = round((q[3] + q[1] - 2*q[2]) / (q[3] - q[1]), 4))
}])

cat("\n=== Hartigan's Dip Test for Multimodality ===\n")
print(dip.test(log10(positive_data$duration_days)))

# 2. Find the valley/separation point
density_est <- density(log10(positive_data$duration_days), n = 2048)
plot(density_est)

# Find the valley between bimodal peaks
log_dens <- density(log10(positive_data$duration_days), n = 2048)

# Search for valley between 0.1 and 10 days (log10(-1) to log10(1))
valley_region <- log_dens$x > -1 & log_dens$x < 1
valley_idx <- which.min(log_dens$y[valley_region])
valley_cutoff <- 10^log_dens$x[valley_region][valley_idx]

cat(sprintf("\n=== Valley Analysis ===\n"))
cat(sprintf("Valley minimum at: %.3f days\n", valley_cutoff))

# Split data at valley threshold
positive_data[, mode_group := ifelse(duration_days < valley_cutoff, 
                                     "Fast", "Standard")]

# Characterize each mode
cat("\n=== Mode Characterization ===\n")
print(positive_data[, .(
  n = .N,
  pct = 100 * .N / nrow(positive_data),
  median_days = median(duration_days),
  mean_days = mean(duration_days),
  p95 = quantile(duration_days, 0.95)
), by = mode_group])

# Analyze by agency - which agencies drive each mode?
cat("\n=== Agency Distribution by Mode ===\n")
print(positive_data[, .N, by = .(agency, mode_group)][
  , pct := 100 * N / sum(N), by = agency
][order(agency, mode_group)])

# Create formatted console table
cat("\n=== Mode Distribution Summary by Agency ===\n\n")

# Calculate counts by agency and mode_group
mode_summary <- positive_data[, .N, by = .(agency, mode_group)]

# Pivot to wide format
mode_wide <- dcast(mode_summary, agency ~ mode_group, value.var = "N", fill = 0)

# Calculate totals and percentages
mode_wide[, Total_N := Fast + Standard]
mode_wide[, pct_Fast := 100 * Fast / Total_N]
mode_wide[, pct_Standard := 100 * Standard / Total_N]

# Sort by total count descending
setorder(mode_wide, -Total_N)

# Calculate overall totals
total_fast_n <- sum(mode_wide$Fast)
total_standard_n <- sum(mode_wide$Standard)
total_all_n <- total_fast_n + total_standard_n
total_fast_pct <- 100 * total_fast_n / total_all_n
total_standard_pct <- 100 * total_standard_n / total_all_n

# Print header
cat(sprintf("%-20s", "Agency"))
cat(sprintf("%15s %8s", "Fast", ""))
cat(sprintf("%15s %8s", "Standard", ""))
cat(sprintf("%12s\n", "Total N"))

cat(sprintf("%-20s", ""))
cat(sprintf("%12s %8s", "N", "%"))
cat(sprintf("%12s %8s", "N", "%"))
cat("\n")
cat(strrep("-", 85), "\n")

# Print each agency
for (i in 1:nrow(mode_wide)) {
  cat(sprintf("%-20s", mode_wide$agency[i]))
  cat(sprintf("%12s %7.1f%%", 
              format(mode_wide$Fast[i], big.mark = ","), 
              mode_wide$pct_Fast[i]))
  cat(sprintf("%12s %7.1f%%", 
              format(mode_wide$Standard[i], big.mark = ","), 
              mode_wide$pct_Standard[i]))
  cat(sprintf("%12s\n", format(mode_wide$Total_N[i], big.mark = ",")))
}

# Print separator and totals
cat(strrep("-", 85), "\n")
cat(sprintf("%-20s%12s %7.1f%%%12s %7.1f%%%12s\n",
            "All Agencies",
            format(total_fast_n, big.mark = ","), 
            total_fast_pct,
            format(total_standard_n, big.mark = ","), 
            total_standard_pct,
            format(total_all_n, big.mark = ",")))
cat("\n")

# Create NYPD subset
nypd_data <- positive_data[agency == "NYPD"]
other_data <- positive_data[agency != "NYPD"]

# NYPD and non-NYPD histograms (same layout, different colour)
for (grp in list(
  list(data = nypd_data,  fill = "#0072B2", file = "nypd_only_positive_durations.pdf",
       title = "NYPD-only Service Requests with Positive Durations"),
  list(data = other_data, fill = "#009E73", file = "others_only_positive_durations.pdf",
       title = "Non-NYPD Service Requests with Positive Durations"))) {
  plot_histogram(
    DT          = grp$data,
    value_col   = "duration_days",
    chart_dir   = chart_dir,
    filename    = grp$file,
    title       = grp$title,
    x_label     = "Days (log scale)",
    log_x       = TRUE,
    bins        = 150,
    outlier_percentile = 1,
    fill_color  = grp$fill,
    alpha       = 1,
    show_mean   = TRUE,
    show_median = TRUE,
    stat_units  = " days",
    add_stats   = FALSE,
    print_summary = FALSE
  )
}

# NYPD vs other agencies, overlaid (Supplement figure)
positive_data[, agency_group := data.table::fifelse(agency == "NYPD", "NYPD", "Other Agencies")]
plot_histogram(
  DT           = positive_data,
  value_col    = "duration_days",
  group_col    = "agency_group",
  group_colors = c("NYPD" = "#0072B2", "Other Agencies" = "#009E73"),
  chart_dir    = chart_dir,
  filename     = "nypd_vs_others_combined.pdf",
  title        = "Bimodal Duration Distribution: NYPD vs Other Agencies",
  x_label      = "Days (log scale)",
  log_x        = TRUE,
  bins         = 150,
  outlier_percentile = 1,
  alpha        = 0.6,
  show_median  = TRUE,
  print_summary = FALSE
)

# Histogram of positive durations up to 90 days, 1-day bins
upper_limit <- 30*3    # Maximum days to display (90 days)

# Generate summary statistics
cat("\n=== Summary of duration_days ===\n")
print(summary(positive_data$duration_days))

plot_histogram(
  DT         = positive_data,
  value_col  = "duration_days",
  title      = sprintf("Positive Duration Distribution (<= %s days)", upper_limit),
  x_label    = "Duration (days)",
  chart_dir  = chart_dir,
  filename   = "positive_duration_histogram.pdf",
  max_value  = upper_limit,
  bin_width  = 1
)


free_objects("positive_data", "nypd_data", "other_data",
             "density_est", "log_dens")

# ==============================================================================
# SECTION 2: NEGATIVE DURATION ANALYSIS
# ==============================================================================
# Analyze service requests with negative durations (data quality issues)
# Negative durations indicate closed_date occurs before created_date

progress_msg("Step 24 of 29: Durations: negative durations")
cat("\n=== ANALYZING NEGATIVE DURATIONS ===\n")

# Filter to negative duration records
negative_data <- d311[duration_days < 0 & !is.na(duration_days)]

# Chart limits for negative values
upper_limit_neg <- 0      # Less negative (closer to zero)
lower_limit_neg <- -365   # More negative (further from zero)

# Bounded dataset for the charts
limited_negative_data <- negative_data[
  duration_days >= lower_limit_neg & duration_days <= upper_limit_neg
]

# Generate summary statistics
print(summary(negative_data$duration_days))

# Histogram (explicit min/max, so no percentile trimming)
plot_histogram(
  DT         = limited_negative_data,
  value_col  = "duration_days",
  title      = sprintf("Negative Duration Distribution (%d to %d days)",
                       lower_limit_neg, upper_limit_neg),
  x_label    = "Duration (days)",
  chart_dir  = chart_dir,
  filename   = "negative_duration_histogram.pdf",
  bins       = 100,
  min_value  = lower_limit_neg,
  max_value  = upper_limit_neg,
  fill_color = "#D55E00",
  alpha      = 0.7
)

# Box plot by agency (log axis)
plot_boxplot(
  DT           = limited_negative_data,
  value_col    = duration_days,
  by_col       = agency,
  chart_dir    = chart_dir,
  filename     = "negative_duration_SR_boxplot.pdf",
  title        = "Negative Duration (days) by agency",
  order_by     = "count",
  x_scale_type = "pseudo_log",
  x_limits     = c(lower_limit_neg, upper_limit_neg)
)

# Single violin of all negative durations (print-safe version)
create_violin_chart(
  dataset         = limited_negative_data,
  x_axis_field    = "duration_days",
  chart_directory = chart_dir,
  chart_file_name = "negative_duration_SR_violin.pdf",
  chart_title     = "Distribution of Negative Duration Days"
)

free_objects("negative_data", "limited_negative_data", "plot_result")

# ==============================================================================
# SECTION 3: SHORT DURATION ANALYSIS & THRESHOLD DETERMINATION
# ==============================================================================
# Analyze very short durations to identify suspicious patterns
# Determines statistical threshold for flagging anomalously short durations

progress_msg("Step 25 of 29: Durations: short durations and threshold")
cat("\n=== ANALYZING SHORT DURATIONS & SETTING THRESHOLDS ===\n")

# Run comprehensive skewed duration analysis
skewed_result <- analyze_skewed_durations(
  DT = d311,
  duration_col = "duration_sec",
  minimum_cutoff_sec = 2,
  upper_cutoff_sec = 60*60*24*2L,  # 2 days in seconds
  print_summary = TRUE,
  create_plots = TRUE,
  chart_dir = chart_dir
)

# Extract the LogNormal 3-standard-deviation threshold
threshold <- skewed_result$thresholds$log_3sd_lower
threshold_numeric <- round(as.numeric(threshold), 0)

# Histogram of 2-90 seconds, 1-second bins. Bars for durations
# <= threshold_numeric are highlighted (the near-zero rule used later:
# 0 < duration <= threshold_numeric), with the line just after them.
plot_duration_histogram(
  DT                = d311,
  duration_col      = "duration_sec",
  bin_width         = 1,
  min_value         = 2L,
  max_value         = 90,            # Focus on first 90 seconds
  threshold_numeric = threshold_numeric,
  chart_dir         = chart_dir
)

# Display threshold value
cat("LogNormal_3SD threshold:", threshold_numeric, "seconds\n")

################################################################################
# TIMESTAMP DISTRIBUTION ANALYSIS
################################################################################
#
# Purpose: Analyze minute-level timestamp distributions
#          to identify patterns, rounding, and data quality issues
#
################################################################################

progress_msg("Step 26 of 29: Timestamp distributions (minutes and seconds)")
# ============================================================================
# CREATED_DATE Analysis
# ============================================================================

# Run foundational minute distribution analysis
results_created_dist <- analyze_second_minute_distribution(d311, 
                                                     date_col = "created_date")

# Prepare MINUTE data with 3-minute cycle pattern
minute_data_created <- results_created_dist$minute_summary[!is.na(minute_value)]
minute_data_created[, minute_label := sprintf("%02d", minute_value)]

# Define 3-minute cycle elevated minutes
elevated_minutes_3min <- c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 
                           41, 44, 47, 50, 53, 56, 59)

# Add elevated pattern
minute_data_created[, is_elevated := minute_value %in% elevated_minutes_3min]
minute_data_created[, pattern_label := factor(is_elevated, 
                                              levels = c(FALSE, TRUE),
                                              labels = c("Normal Minutes", "Elevated Minutes"))]

# Create factor with all levels and set breaks to show elevated minutes
minute_data_created[, minute_label_factor := factor(minute_label,                                                   levels = sprintf("%02d", 0:59))]

# Create chart with colored patterns
p_minute_created <- plot_barchart(
  DT = minute_data_created,
  x_col = "minute_label_factor",
  y_col = "count",
  title = "Distribution of Service Requests by Minute Value",
  subtitle = "created_date; orange = minutes 02, 05, 08, ... 59 (3-minute cycle)",
  x_label = "Minute value (00-59)",
  fill_col = "pattern_label",
  fill_colors = c("Normal Minutes" = "#0072B2", "Elevated Minutes" = "#D55E00"),
  add_mean = FALSE,
  add_3sd = TRUE,
  show_labels = FALSE,
  x_breaks = sprintf("%02d", elevated_minutes_3min),   # label only the elevated minutes
  console_print_title = "CREATED_DATE: MINUTE VALUE DISTRIBUTION",
  chart_dir = chart_dir,
  filename = "created_date_minute_distribution"
)

# Run cycle pattern analysis (for agency-level investigation)
results_created_3min <- analyze_cycle_pattern(
  data = d311,
  minute_results = results_created_dist,
  date_col = "created_date",
  cycle_type = "3min",
  output_prefix = "createddate",
  create_chart = TRUE
)

# ============================================================================
# CLOSED_DATE Analysis
# ============================================================================

# Run foundational minute distribution analysis
results_closed_dist <- analyze_second_minute_distribution(d311, 
                                                          date_col = "closed_date")

# Prepare MINUTE data with 5-minute cycle pattern
minute_data_closed <- results_closed_dist$minute_summary[!is.na(minute_value)]
minute_data_closed[, minute_label := sprintf("%02d", minute_value)]

# Define 5-minute cycle elevated minutes
elevated_minutes_5min <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)

# Add elevated pattern
minute_data_closed[, is_elevated := minute_value %in% elevated_minutes_5min]
minute_data_closed[, pattern_label := factor(is_elevated, 
                                             levels = c(FALSE, TRUE),
                                             labels = c("Normal Minutes", "Elevated Minutes"))]

# Create factor with all levels
minute_data_closed[, minute_label_factor := factor(minute_label, 
                                                   levels = sprintf("%02d", 0:59))]

# Create chart with colored patterns
p_minute_closed <- plot_barchart(
  DT = minute_data_closed,
  x_col = "minute_label_factor",
  y_col = "count",
  title = "Distribution of Service Requests by Minute Value",
  subtitle = "closed_date; orange = minutes 05, 10, 15, ... 55 (5-minute intervals)",
  x_label = "Minute value (00-59)",
  fill_col = "pattern_label",
  fill_colors = c("Normal Minutes" = "#0072B2", "Elevated Minutes" = "#D55E00"),
  add_mean = FALSE,
  add_3sd = TRUE,
  show_labels = FALSE,
  x_breaks = sprintf("%02d", elevated_minutes_5min),   # label only the elevated minutes
  console_print_title = "CLOSED_DATE: MINUTE VALUE DISTRIBUTION",
  chart_dir = chart_dir,
  filename = "closed_date_minute_distribution"
)

# Run 5-minute cycle pattern analysis
results_closed_5min <- analyze_cycle_pattern(
  data = d311,
  minute_results = results_closed_dist,
  date_col = "closed_date",
  cycle_type = "5min",
  output_prefix = "closeddate",
  create_chart = TRUE
)

# ============================================================================
# PART 3: COMBINED MINUTE:SECOND ANALYSIS (FIRST N SECONDS OF EACH HOUR)
# ============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("ANALYZING COMBINED MINUTE:SECOND DISTRIBUTIONS\n")
cat(rep("=", 80), "\n\n", sep = "")

# ----------------------------------------------------------------------------
# 3A. Set Analysis Parameters
# ----------------------------------------------------------------------------

# PARAMETER: Set the maximum number of seconds to analyze from start of each hour
second_limit <- 601  # Seconds after the hour to include (0 to second_limit)
# Common values: 180 (3 min), 300 (5 min), 600 (10 min), 
#                900 (15 min), 3599 (full hour)

# Calculate end time label for display
end_time_label <- sprintf("%02d:%02d", second_limit %/% 60, second_limit %% 60)

cat("Analyzing first ", second_limit, " seconds (00:00 to ", end_time_label, 
    ") of each hour\n", sep = "")
cat("Date field: created_date\n\n")

# ----------------------------------------------------------------------------
# 3B. Extract and Aggregate Data
# ----------------------------------------------------------------------------

# Seconds from the start of the hour (0-3599), counted for 0..second_limit.
# Computed in one pass; no 16M-row helper table is kept.
second_counts <- d311[, .(total_seconds = minute(created_date) * 60 + second(created_date))][
  total_seconds <= second_limit, .N, by = total_seconds][order(total_seconds)]

# Calculate percentage and cumulative percentage
total_records <- sum(second_counts$N)
second_counts[, `:=`(
  pct = round(N / total_records * 100, 3),
  cum_pct = round(cumsum(N) / total_records * 100, 2)
)]

# Add time label (MM:SS format)
second_counts[, time_label := sprintf("%02d:%02d", 
                                      total_seconds %/% 60, 
                                      total_seconds %% 60)]

# Format count with commas
second_counts[, count_fmt := format(N, big.mark = ",")]

# ----------------------------------------------------------------------------
# 3C. Display Summary Table
# ----------------------------------------------------------------------------

cat("\n", rep("=", 70), "\n", sep = "")
cat("TIMESTAMP DISTRIBUTION: First ", second_limit, " seconds (00:00 to ", 
    end_time_label, ")\n", sep = "")
cat(rep("=", 70), "\n", sep = "")
cat("Total records in this window: ", format(total_records, 
                                             big.mark = ","), "\n\n", sep = "")

console_rows <- min(181, nrow(second_counts))

# Temporarily increase print rows, then restore the previous setting
old_opt <- options(datatable.print.nrows = console_rows)
print(second_counts[1:console_rows, .(time_label, count = count_fmt, pct, cum_pct)])
options(old_opt)

cat("\n", rep("=", 70), "\n", sep = "")

# ------------------------------------------------------------------------------
# 3D. Create Visualization
# ------------------------------------------------------------------------------

cat("\nCreating combined minute:second distribution chart...\n")

# Label every 15 s (windows up to 2 min), 30 s (up to 10 min), else 60 s
break_interval <- if (second_limit <= 120) 15 else if (second_limit <= 600) 30 else 60
break_labels <- second_counts[total_seconds %% break_interval == 0, time_label]

plot_barchart(
  DT           = second_counts,
  x_col        = "time_label",
  y_col        = "N",
  title        = paste0("Service Request Distribution: First ", second_limit, " Seconds"),
  subtitle     = paste0("created_date, 00:00 to ", end_time_label, " after each hour"),
  x_label      = "Time from hour start (MM:SS)",
  fill_color   = "#0072B2",   # same blue as the Supplement figure
  bar_width    = 1,
  show_labels  = FALSE,
  show_summary = FALSE,
  x_breaks     = break_labels,
  x_axis_angle = 45,
  chart_dir    = chart_dir,
  filename     = paste0("created_date_first_", second_limit, "_seconds")
)

# ------------------------------------------------------------------------------
# 3E. Summary Statistics
# ------------------------------------------------------------------------------

cat("\n", rep("-", 70), "\n", sep = "")
cat("KEY OBSERVATIONS:\n")
cat(rep("-", 70), "\n", sep = "")

# Find peaks (top 5 seconds by count)
top_seconds <- second_counts[order(-N)][1:5]
cat("\nTop 5 seconds with highest counts:\n")
for (i in 1:nrow(top_seconds)) {
  cat(sprintf("  %d. %s - %s records (%.2f%%)\n",
              i,
              top_seconds$time_label[i],
              top_seconds$count_fmt[i],
              top_seconds$pct[i]))
}

# Check for on-the-minute spikes (seconds ending in :00)
on_minute_counts <- second_counts[total_seconds %% 60 == 0]
if (nrow(on_minute_counts) > 0) {
  cat("\nOn-the-minute timestamps (:00 seconds):\n")
  for (i in 1:nrow(on_minute_counts)) {
    cat(sprintf("  %s - %s records (%.2f%%)\n",
                on_minute_counts$time_label[i],
                on_minute_counts$count_fmt[i],
                on_minute_counts$pct[i]))
  }
}

cat("\n", rep("=", 70), "\n", sep = "")
cat("END OF COMBINED MINUTE:SECOND ANALYSIS\n")
cat(rep("=", 70), "\n\n", sep = "")

free_objects("second_counts", "top_seconds", "on_minute_counts")

# ============================================================================
# ANALYSIS COMPLETE
# ============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("TIMESTAMP DISTRIBUTION ANALYSIS COMPLETE\n")
cat(rep("=", 80), "\n", sep = "")
cat("\nKey Findings:\n")
cat("  - created_date: Check for 3-minute cycle pattern (DEP agency)\n")
cat("  - closed_date: Check for 5-minute and 15-minute rounding patterns\n")
cat("  - Combined minute:second: Check first ", second_limit, 
    " seconds for detailed patterns\n", sep = "")
cat("\nAll charts saved to ./charts/ directory\n\n")

free_objects("timestamp_analysis", "p_combined")

################################################################################
# END OF TIMESTAMP DISTRIBUTION ANALYSIS
################################################################################


# ==============================================================================
# SECTION 4: COMPREHENSIVE DURATION CATEGORY ANALYSIS
# ==============================================================================
# Systematic analysis of all duration categories:
# - Negative (small, large, extreme)
# - Zero durations
# - Near-zero (seconds-level)
# - One-second durations
# - Positive (small, large, extreme)

progress_msg("Step 27 of 29: Durations: category QA")
cat("\n=== COMPREHENSIVE DURATION CATEGORY ANALYSIS ===\n")
duration_analysis <- analyze_duration_QA(
  d311,
  lower_neg_days   = -2 * 365,
  extreme_neg_days = -5 * 365,
  upper_pos_days   = 2 * 365,
  extreme_pos_days = 5 * 365,
  max_outlier_days = 10 * 365,
  near_zero_secs   = threshold_numeric,
  genesis_date     = genesis_date,
  chart_dir = chart_dir)

if (isTRUE(use_free_objects)) {
  duration_analysis[c("positive_all", "positive_small",
                      "negative_all", "negative_small")] <- NULL
}
run_gc("after analyze_duration_QA")

# ==================================================================
# Long-duration SRs by agency and complaint type
# ------------------------------------------------------------------
#
# Bins come from analyze_duration_QA():
#   positive_large   : > upper_pos_days   and <= extreme_pos_days
#   positive_extreme : > extreme_pos_days and <  max_outlier_days
#
# pct     = share of ALL SRs in the bin (before the count cutoff)
# cum_pct = running total down the table, sorted by count
# ==================================================================

# --- Count cutoffs: complaint types below these are omitted ---
positive_large_count_cutoff   <- 50
positive_extreme_count_cutoff <- 1

# --- Bin boundaries, taken from the QA run so labels match the analysis ---
thr     <- duration_analysis$thresholds
fmt_yrs <- function(d) formatC(d / 365.25, format = "g", digits = 3)
fmt_day <- function(d) formatC(d, format = "f", digits = 0, big.mark = ",")

large_label   <- sprintf("POSITIVE LARGE (%s to %s yrs; > %s and <= %s days)",
                     fmt_yrs(thr$upper_pos_days),   fmt_yrs(thr$extreme_pos_days),
                     fmt_day(thr$upper_pos_days),   fmt_day(thr$extreme_pos_days))
extreme_label <- sprintf("POSITIVE EXTREME (%s to %s yrs; > %s and < %s days)",
                     fmt_yrs(thr$extreme_pos_days), fmt_yrs(thr$max_outlier_days),
                     fmt_day(thr$extreme_pos_days), fmt_day(thr$max_outlier_days))

progress_msg("Step 28 of 29: Long-duration SRs by agency and complaint type")

# ---------------------------------------------
# 1. Positive large (2-5 years by default)
# ---------------------------------------------
res_large <- summarize_by_complaint(duration_analysis$positive_large,
                                    d311,
                                    min_count = positive_large_count_cutoff,
                                    label     = "positive_large")

cat(sprintf("\n%s\nComplaint types with count >= %d\n\n",
            large_label, positive_large_count_cutoff))
print(res_large, nrows = Inf)          # show every row in the console

# ---------------------------------------------
# 2. Positive extreme (5-10 years by default)
# ---------------------------------------------
res_extreme <- summarize_by_complaint(duration_analysis$positive_extreme,
                                      d311,
                                      min_count = positive_extreme_count_cutoff,
                                      label     = "positive_extreme")

cat(sprintf("\n%s\nComplaint types with count >= %d\n\n",
            extreme_label, positive_extreme_count_cutoff))
print(res_extreme, nrows = Inf)        # show every row in the console

# Batch-closure check (closures concentrated on single days):
# duration_analysis$positive_extreme[, .N, by = .(agency, close_day = as.Date(closed_ts))][order(-N)][1:10]


# ---------------------------------------------
# 3. Batch-closure check: close dates for one agency
#    Large N on one close date, with requests created
#    over many earlier dates, indicates a batch closure.
# ---------------------------------------------
batch_agency  <- "DPR"
batch_source  <- duration_analysis$positive_extreme   
batch_top_n   <- 15

message("Batch-closure check: ", batch_agency, " (", 
        format(nrow(batch_source), big.mark = ","), " SRs in source)")

batch_by_date <- batch_source[agency == batch_agency, .(
  N             = .N,
  first_created = min(as.Date(created_ts)),
  last_created  = max(as.Date(created_ts)),
  created_days  = uniqueN(as.Date(created_ts)),
  med_age_days  = round(median(duration_days))
), by = .(close_day = as.Date(closed_ts))]

# Share of the agency's SRs closed on each date
batch_by_date[, pct := round(100 * N / sum(N), 2)]

# Sort largest close dates first, then build the running total in that order
setorder(batch_by_date, -N, close_day)
batch_by_date[, cum_pct := round(100 * cumsum(N) / sum(N), 2)]

cat(sprintf("\n%s close dates, top %d by count\n\n", batch_agency, batch_top_n))
print(head(batch_by_date, batch_top_n), nrows = Inf)

# ==============================================================================
# SECTION 5: RESPONSE TIMES BY COMPLAINT TYPE
# ==============================================================================

progress_msg("Step 29 of 29: Response times by complaint type")
cat("\n=== RESPONSE TIMES BY COMPLAINT CATEGORY ANALYSIS ===\n")
# Exclude durations <= threshold_numeric seconds and > 10 years
complaint_stats <- summarize_complaint_response(
                      d311, 
                      min_records = 100,
                      lower_exclusion_limit = threshold_numeric / 86400,
                      upper_exclusion_limit = 365*10
  )
  
# ==============================================================================
# END OF DURATION ANALYSIS
# ==============================================================================

################################################################################

progress_msg("Data cleaning analysis finished")

# Memory trace: one row per free_objects() / run_gc() point
mem_report()

# Close program
close_program(
  program_start = timing$program_start,
  enable_sink = enable_sink,
  verbose = TRUE
)

################################################################################
################################################################################