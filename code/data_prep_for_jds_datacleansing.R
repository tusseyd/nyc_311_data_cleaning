################################################################################
# Read-in raw csv data (main_data_file)
# Convert text columns to upper case
# Convert date columns to POSIXct format
# Modify and standardize column names
# Check for presence of mandatory fields
# Combine NYC Agencies to accommodate name changes
# Replace missing values with NA for standardization
# Write the 311 data and the USPS ZIP codes as RDS files (CSV output optional)

################################################################################
main_data_file <- "5-year_311SR_01-01-2020_thru_12-31-2024_AS_OF_09-23-2025.csv"

# Set to TRUE to redirect console output to text file (default)
# Set to FALSE to display console output on the screen
enable_sink <- TRUE      

# Memory-management flags and monitor (same meaning as in jds_datacleansings.R;
# the tools themselves are in functions/free_objects.R)
use_free_objects     <- TRUE
use_gc               <- TRUE
monitor_memory       <- FALSE
monitor_interval_sec <- 60
mem_messages         <- FALSE

################################################################################

# ------------------------------------------------------ -------
# 📦 CREATE REQUIRED DIRECTORY STRUCTURE
# -------------------------------------------------------------
################################################################################
# Main Analysis Script
################################################################################

# STEP 1: Create directory structure (inline)
# Project folder (same as in jds_datacleansings.R), so the program works
# no matter which folder RStudio starts in
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
  console_filename = "JDS_data_prep_console_output.txt",
  functions_dir = functions_dir,
  verbose = TRUE
)

# Memory tools (shared with jds_datacleansings.R)
if (!exists("free_objects", mode = "function")) source(file.path(functions_dir, "free_objects.R"))
mem_init("dataprep")
if (isTRUE(monitor_memory)) start_memory_monitor(monitor_interval_sec)

# Progress messages (progress_msg() in free_objects.R) appear on screen even
# when enable_sink = TRUE sends the rest of the output to the console file.
progress_msg("Data preparation started (8 steps)")

################################################################################

# ========= Main Execution =========

################################################################################

valid_date_columns <- c(
  "Created Date",
  "Closed Date",
  "Due Date",
  "Resolution Action Updated Date"
)

# Read raw 311 data
progress_msg("Step 1 of 8: Reading the raw 311 CSV (the longest step)")
cat("\nReading in raw 311 Service Request data... \n")
main_data_path <- file.path(raw_data_dir, main_data_file)

# When first reading the file
raw_data <- fread(
  main_data_path,
  nThread       = max(1L, parallel::detectCores() - 1L),
  check.names   = FALSE,
  strip.white   = TRUE,
  showProgress  = TRUE,
  colClasses    = "character"
)

# Make copy for troubleshooting purposes.
#copy_raw_data <- raw_data

num_rows_raw_data <- nrow(raw_data)
cat("\nRaw Data row count:", format(num_rows_raw_data, big.mark = ","))
mem_log("after reading raw CSV")

################################################################################
# Standardize column names
progress_msg(sprintf("Step 2 of 8: Standardizing column names and missing values (%s rows read)",
                     format(num_rows_raw_data, big.mark = ",")))
raw_data <- modify_column_names(raw_data)
cat("\n\nColumn names standardized")

################################################################################
# Standardize the representation of missing data to all be NA
standardize_missing_chars(raw_data)
cat("\nMissing data representation standardized to NA.")

################################################################################
# Define mandatory fields and ensure they are there
mandatory_fields <- c(
  "unique_key",
  "created_date",
  "agency",
  "complaint_type",
  "status"
)

# Keep only those fields that actually exist in the data
progress_msg("Step 3 of 8: Removing rows missing a mandatory field")
present_mandatory_fields <- intersect(mandatory_fields, names(raw_data))

# Remove rows with NA or blank values in any present mandatory field.
# One combined filter, so the full table is copied once rather than once
# per field.
keep_row <- rep(TRUE, nrow(raw_data))
for (field in present_mandatory_fields) {
  v <- raw_data[[field]]
  keep_row <- keep_row & !is.na(v) & trimws(v) != ""
}
raw_data <- raw_data[keep_row]
free_objects("keep_row", "v")   # also releases the pre-filter copy of raw_data

removed_rows <- num_rows_raw_data - nrow(raw_data)

if (removed_rows > 0) {
  cat("\n\nRows removed due to missing mandatory fields:", removed_rows, "\n")
} else {
  cat("\nNo rows removed due to missing mandatory fields.\n")
}

################################################################################
# consolidate Agencies (DCA, DOITT, NYC311-PRD)
progress_msg("Step 4 of 8: Consolidating agencies")
raw_data <- consolidate_agencies( DT = raw_data,
                                  drop_agencies = NULL)

################################################################################
# Convert date columns to POSIXct (America/New_York).
# parse_date_column() reports, for each field: the format used, missing
# values, DST repairs, the ambiguous fall-back hour, and midnight/noon
# counts. (The earlier text-based check, date_checks_character(), was
# removed: it looked for "00:00:00" and so found no midnights in the raw
# export, which writes them as "12:00:00 AM".)
date_columns <- c(
  "created_date",
  "closed_date",
  "due_date",
  "resolution_action_updated_date"
)

progress_msg("Step 5 of 8: Parsing date columns")
result <- parse_date_column(
  temp_raw_data      = raw_data,
  valid_date_columns = date_columns
)

raw_data <- result$parsed_data
failed_to_parse <- result$failures
parsed_summary  <- result$summary
free_objects("result")          # releases the unparsed text date columns

################################################################################
# Define columns containing text to convert to uppercase
columns_to_upper <- c(
  "address_type",
  "agency",
  "agency_name",
  "borough",
  "bridge_highway_direction",
  "bridge_highway_name",
  "bridge_highway_segment",
  "city",
  "community_board",
  "complaint_type",
  "cross_street_1",
  "cross_street_2",
  "descriptor",
  "facility_type",
  "incident_address",
  "intersection_street_1",
  "intersection_street_2",
  "landmark",
  "location_type",
  "open_data_channel_type",
  "park_borough",
  "park_facility_name",
  "resolution_description",
  "road_ramp",
  "status",
  "street_name",
  "taxi_company_borough",
  "taxi_pick_up_location",
  "vehicle_type"
)

progress_msg("Step 6 of 8: Converting text columns to upper case")

# (a) Ensure columns where text to be converted actually exist in dataset
existing_cols <- columns_to_upper[columns_to_upper %in% names(raw_data)]

# Warn about any missing columns
missing_cols <- setdiff(columns_to_upper, existing_cols)
if (length(missing_cols) > 0) {
  cat("\n[WARNING] The following columns were not found in raw_data:\n",
      paste(" -", missing_cols, collapse = "\n"), "\n")
} 

# (b) Keep only those that are character columns
valid_cols <- existing_cols[sapply(raw_data[, ..existing_cols], is.character)]

# (c) Convert to uppercase
for (col in valid_cols) {
  raw_data[, (col) := toupper(get(col))]
  cat("\n -", col, "converted to upper case")
}

cat("\nAll text Columns converted to uppercase")

################################################################################
# Extract AS_OF date from filename (e.g., ...AS_OF_2025-08-10.csv -> 2025-08-10)
as_of_date <- sub(".*AS_OF_([0-9-]+)\\.csv$", "\\1", main_data_file)
local_tz <- "America/New_York"

# Coverage from the data
min_date <- suppressWarnings(min(raw_data$created_date, na.rm = TRUE))
max_date <- suppressWarnings(max(raw_data$created_date, na.rm = TRUE))
if (!is.finite(min_date) || !is.finite(max_date)) 
  stop("created_date has no finite values.")
min_date <- as.POSIXct(min_date, tz = local_tz)
max_date <- as.POSIXct(max_date, tz = local_tz)

cat("\n========================================\n")
cat("Data Coverage Summary\n")
cat("========================================\n")
cat(sprintf("Full dataset range: %s to %s\n", 
            format(min_date, "%Y-%m-%d"), 
            format(max_date, "%Y-%m-%d")))
cat(sprintf("Data AS_OF date: %s\n", as_of_date))

# Spans to generate
selected_year_spans <- c(5)
cat(sprintf("\nSelected year spans: %s\n", paste(selected_year_spans, collapse = ", ")))

# Ensure output directory exists
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

################################################################################
# Find max complete year (exclude the partial year of the as-of date).
# Uses the as-of date in the file name, not today's date, so the result
# does not change with the day the program is run.
current_year <- as.integer(sub(".*-", "", as_of_date))   # as_of_date is MM-DD-YYYY
max_complete_year <- raw_data[year(created_date) < current_year, 
                              year(max(created_date, na.rm = TRUE))]
date_tz <- attr(raw_data$created_date, "tzone")

cat(sprintf("\nCurrent year: %d\n", current_year))
cat(sprintf("Max complete year: %d\n", max_complete_year))
cat(sprintf("Timezone: %s\n", date_tz))

# Collect results for summary
progress_msg("Step 7 of 8: Saving the 311 data as RDS")
summary_list <- list()

cat("\n========================================\n")
cat("Processing Year Spans\n")
cat("========================================\n")

for (span in selected_year_spans) {
  start_date <- as.POSIXct(
    sprintf("%d-01-01 00:00:00", max_complete_year - span + 1), tz = date_tz
  )
  end_date <- as.POSIXct(
    sprintf("%d-01-01 00:00:00", max_complete_year + 1), tz = date_tz
  )
  
  cat("\n----------------------------------------\n")
  cat(sprintf("Processing %d-year span\n", span))
  cat("----------------------------------------\n")
  cat(sprintf("Year range: %d-%d\n", 
              max_complete_year - span + 1, 
              max_complete_year))
  cat(sprintf("Date range: %s to %s\n",
              format(start_date, "%Y-%m-%d %H:%M:%S"),
              format(end_date, "%Y-%m-%d %H:%M:%S")))
  
  # When every row is already inside the span (the usual case: the raw
  # file covers exactly these years), use raw_data as is instead of making
  # a second full copy of the table.
  in_span <- raw_data$created_date >= start_date & raw_data$created_date < end_date
  filtered_data <- if (all(in_span, na.rm = TRUE) && !anyNA(in_span)) raw_data else raw_data[in_span]
  rm(in_span)
  
  if (nrow(filtered_data) == 0L) {
    cat(sprintf("WARNING: No data found for %d-year span [%s to %s]\n",
                span,
                format(start_date, "%Y-%m-%d"),
                format(end_date, "%Y-%m-%d")))
    next
  }
  
  cat(sprintf("Records filtered: %s\n", format(nrow(filtered_data), big.mark = ",")))
  
  s_date <- format(start_date, "%m-%d-%Y")
  e_date <- format(max(filtered_data$created_date), "%m-%d-%Y")
  
  file_name <- sprintf(
    "%d-year_311SR_%s_thru_%s_AS_OF_%s.rds",
    span, s_date, e_date, as_of_date
  )
  full_path <- file.path(data_dir, file_name)
  
  cat(sprintf("Output file: %s\n", file_name))
###########################################
# Save and verify both the .rds and .csv format (optional)  
 
  save_and_verify( dt = filtered_data, 
                   path = full_path,
                   write_csv = FALSE,
                   csv_path = NULL
                   )

##########################################
  
  # Collect summary
  summary_list[[length(summary_list) + 1]] <- data.table(
    span_years = span,
    years_included = sprintf("%d–%d", max_complete_year - span + 1, 
                                                            max_complete_year),
    start_date = format(start_date, "%Y-%m-%d"),
    end_date   = format(end_date - 1, "%Y-%m-%d"),  # inclusive end
    rows       = nrow(filtered_data),
    file       = basename(full_path)
  )
}

# ============================= Summary ===============================
if (length(summary_list) > 0) {
  summary_dt <- rbindlist(summary_list)
  
  # Format rows with commas
  summary_dt[, rows := prettyNum(rows, big.mark = ",")]
  
  cat("\n=== Summary of Generated Datasets ===\n")
  print(summary_dt)
}

# The 311 data are saved; release them before the USPS step
free_objects("raw_data", "filtered_data")

################################################################################
usps_data_file <- "zip_code_database.csv"
usps_path      <- file.path(raw_data_dir, usps_data_file)
usps_rds_file  <- file.path(data_dir, "USPS_zipcodes.rds")

if (!file.exists(usps_path)) {
  stop("USPS CSV not found at: ", usps_path)
}

progress_msg("Step 8 of 8: Processing USPS ZIP code data")
cat("\nProcessing USPS Zipcode data...\n")

# Read only the 'zip' column; if not found exactly, read headers and detect it
zipcode_data <- tryCatch(
  fread(
    usps_path,
    select      = "zip",
    colClasses  = c(zip = "character"),
    nThread     = max(1L, parallel::detectCores() - 1L),
    check.names = FALSE,
    strip.white = TRUE,
    showProgress = TRUE
  ),
  error = function(e) {
    # Fallback: read header to find a plausible ZIP column (case-insensitive)
    hdr <- names(fread(usps_path, nrows = 0, check.names = FALSE, 
                                                          showProgress = TRUE))
    cand <- grep("^zip(code)?$", hdr, ignore.case = TRUE, value = TRUE)
    if (length(cand) == 0L) 
                  stop("No 'zip' column found (case-insensitive) in USPS CSV.")
    fread(
      usps_path,
      select      = cand[1],
      colClasses  = setNames("character", cand[1]),
      nThread     = max(1L, parallel::detectCores() - 1L),
      check.names = FALSE,
      strip.white = TRUE,
      showProgress = TRUE
    )
  }
)

# Ensure the column is named exactly 'zip'
if (!identical(names(zipcode_data), "zip")) {
  setnames(zipcode_data, 1L, "zip")
}

# Light cleanup
zipcode_data[, zip := trimws(zip)]

# Save just the single column, always overwrite
saveRDS(zipcode_data[, .(zip)], usps_rds_file)

################################################################################

progress_msg("Data preparation finished")

# Memory trace for this program (also closes the memory monitor window)
mem_report()

# Close program
close_program(
  program_start = timing$program_start,
  enable_sink = enable_sink,
  verbose = TRUE
)

################################################################################
################################################################################
