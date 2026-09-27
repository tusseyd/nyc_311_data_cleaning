# =====================================================================
# check_embedded_commas.R
#
# Purpose
#   Scan the raw NYC 311 Service Request CSV, as downloaded from NYC
#   Open Data and unmodified, for embedded commas in every field.
#   Demonstrates that a quote-aware CSV parser reads the file
#   correctly, and estimates how many lines a quote-unaware parser
#   (a naive split on commas) would misread.
#
# Input
#   5-year 311 SR extract, 2020-01-01 through 2024-12-31,
#   downloaded 2025-10-10 from NYC Open Data.
#
# Outputs (console, plus CSV files in out_dir)
#   1. Row and column counts, with a strict column-count check
#   2. For each column:
#        - number and percent of values containing a comma
#        - number of values containing an embedded newline
#        - number of values containing a double quote
#        - one example value containing a comma
#   3. Number of records with at least one embedded comma
#   4. Naive comma-split check on the first naive_lines raw lines
#   5. R and package versions used
#
# Requirements
#   R (>= 4.0) and data.table. Reading the full file as character
#   requires substantial memory (roughly 25-40 GB for ~16M rows x 41
#   columns); the naive check reads only naive_lines raw lines.
#
# Author: David [surname]
# Date:   [date]
# =====================================================================

library(data.table)

run_start <- Sys.time()
message("check_embedded_commas.R started: ", format(run_start, "%Y-%m-%d %H:%M:%S"))

# ---- Settings (edit these) -------------------------------------------
# Path to the raw CSV. Relative paths are resolved from the working
# directory; set the working directory to the project root before running.
csv_file      <- file.path("data", "raw_data",
                           "5-year_311SR_01-01-2020_thru_12-31-2024_AS_OF_10-10-2025.csv")
out_dir       <- file.path("console_output", "embedded_commas")
expected_cols <- 41L     # columns in the NYC Open Data 311 SR export
naive_lines   <- 1e6     # raw lines checked by the naive-split test
example_chars <- 80L     # characters shown per example value
compute_md5   <- TRUE    # MD5 checksum identifies the exact input file

stopifnot(file.exists(csv_file))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 0. Identify the input file --------------------------------------
# File size and MD5 checksum let a reader confirm they are running the
# script against the same file used in the paper.
file_mb <- file.size(csv_file) / 1024^2
cat(sprintf("\nInput file: %s\nSize: %s MB\n",
            basename(csv_file), format(round(file_mb), big.mark = ",")))

if (isTRUE(compute_md5)) {
  message("Computing MD5 checksum (may take a few minutes for a large file)...")
  cat(sprintf("MD5:  %s\n", unname(tools::md5sum(csv_file))))
}

# ---- 1. Read everything as character ---------------------------------
# colClasses = "character": no type conversion, so each value is the
# exact text in the file, minus the enclosing quotes.
# na.strings = NULL: empty fields stay as "" rather than becoming NA.
# quote = "\"": standard RFC 4180 quoting, as used by NYC Open Data.
#
# fread reports a malformed record (wrong number of fields) as a
# warning and may stop reading early. Warnings are therefore promoted
# to errors, so the script cannot finish on a partial read.
message("Step 1: reading CSV (all columns as character)...")
t0 <- Sys.time()
dt <- withCallingHandlers(
  fread(
    csv_file,
    colClasses   = "character",
    na.strings   = NULL,
    quote        = "\"",
    showProgress = TRUE
  ),
  warning = function(w) {
    stop("fread warning treated as an error (possible malformed record): ",
         conditionMessage(w), call. = FALSE)
  }
)
cat(sprintf("\nRead %s rows x %d columns in %.1f min\n",
            format(nrow(dt), big.mark = ","), ncol(dt),
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

# With warnings promoted to errors, reaching this point means every
# record parsed to the same number of fields. The check below confirms
# that number is the expected one.
if (ncol(dt) != expected_cols) {
  stop(sprintf("Expected %d columns, found %d.", expected_cols, ncol(dt)))
}
cat(sprintf("Column count check passed: all records have %d fields\n", expected_cols))

# ---- 2. Per-column scan ----------------------------------------------
# fixed = TRUE does a literal search (no regular expressions), which is
# much faster on millions of strings. Records with any embedded comma
# are accumulated in the same pass, so the data is scanned only once.
message("Step 2: scanning ", ncol(dt), " columns for commas, newlines, and quotes...")

any_comma <- logical(nrow(dt))
results   <- vector("list", ncol(dt))

for (j in seq_along(dt)) {
  col       <- names(dt)[j]
  x         <- dt[[j]]
  has_comma <- grepl(",", x, fixed = TRUE)
  n_comma   <- sum(has_comma)
  any_comma <- any_comma | has_comma
  
  results[[j]] <- list(
    column    = col,
    n_comma   = n_comma,
    pct_comma = 100 * n_comma / length(x),
    n_newline = sum(grepl("\n", x, fixed = TRUE)),
    n_quote   = sum(grepl("\"", x, fixed = TRUE)),
    example   = if (n_comma > 0) substr(x[which(has_comma)[1]], 1, example_chars)
                else NA_character_
  )
  message(sprintf("  [%2d/%d] %-32s commas: %s",
                  j, ncol(dt), col, format(n_comma, big.mark = ",")))
}

results <- rbindlist(results)
setorder(results, -n_comma, column)
n_records_comma <- sum(any_comma)

# ---- 3. Report --------------------------------------------------------
cat("\n==== Columns containing embedded commas ====\n")
print(results[n_comma > 0,
              .(column,
                n_comma   = format(n_comma,   big.mark = ","),
                pct_comma = sprintf("%.2f%%", pct_comma),
                n_newline = format(n_newline, big.mark = ","),
                n_quote   = format(n_quote,   big.mark = ","))],
      nrows = Inf)

cat("\n==== Example value per column ====\n")
print(results[n_comma > 0, .(column, example)], nrows = Inf)

cat(sprintf("\n%d of %d columns contain at least one embedded comma.\n",
            results[n_comma > 0, .N], ncol(dt)))
cat(sprintf("Total values with an embedded comma: %s\n",
            format(sum(results$n_comma), big.mark = ",")))
cat(sprintf("Records with at least one embedded comma: %s of %s (%.2f%%)\n",
            format(n_records_comma, big.mark = ","),
            format(nrow(dt), big.mark = ","),
            100 * n_records_comma / nrow(dt)))

rm(dt, any_comma); invisible(gc())   # free the full table before Step 4

# ---- 4. Naive comma-split check --------------------------------------
# Simulates a quote-unaware parser that splits each physical line on
# every comma. A line is misread if that split does not produce exactly
# expected_cols fields. This catches both embedded commas and embedded
# newlines (a quoted field spanning two lines splits one record into
# two short lines).
#
# The check uses the first naive_lines data lines of the file (a
# sequential sample, not a random one).
message("Step 4: naive comma-split check on the first ",
        format(naive_lines, big.mark = ","), " lines...")

raw_lines <- readLines(csv_file, n = naive_lines + 1L, encoding = "UTF-8")
raw_lines <- raw_lines[-1L]                          # drop header line
n_fields  <- nchar(raw_lines, type = "bytes") -
             nchar(gsub(",", "", raw_lines, fixed = TRUE), type = "bytes") + 1L
n_bad     <- sum(n_fields != expected_cols)

naive_summary <- data.table(
  lines_checked  = length(raw_lines),
  lines_misread  = n_bad,
  pct_misread    = round(100 * n_bad / length(raw_lines), 2),
  too_many_split = sum(n_fields >  expected_cols),
  too_few_split  = sum(n_fields <  expected_cols)
)

cat("\n==== Naive comma-split check ====\n")
cat(sprintf("Lines checked: %s\n", format(length(raw_lines), big.mark = ",")))
cat(sprintf("Lines a naive comma split would misread: %s (%.2f%%)\n",
            format(n_bad, big.mark = ","), 100 * n_bad / length(raw_lines)))
cat(sprintf("  Split into more than %d fields: %s\n", expected_cols,
            format(naive_summary$too_many_split, big.mark = ",")))
cat(sprintf("  Split into fewer than %d fields: %s\n", expected_cols,
            format(naive_summary$too_few_split, big.mark = ",")))
cat("The quote-aware read in Step 1 parsed every record to",
    expected_cols, "fields.\n")

rm(raw_lines, n_fields)

# ---- 5. Save results --------------------------------------------------
fwrite(results,       file.path(out_dir, "embedded_commas_by_column.csv"))
fwrite(naive_summary, file.path(out_dir, "naive_split_check.csv"))
message("Results written to: ", normalizePath(out_dir))

# ---- 6. Environment ---------------------------------------------------
cat("\n==== Environment ====\n")
cat(sprintf("R version:          %s\n", R.version.string))
cat(sprintf("data.table version: %s\n", as.character(packageVersion("data.table"))))
cat(sprintf("Platform:           %s\n", R.version$platform))

message(sprintf("check_embedded_commas.R finished in %.1f min",
                as.numeric(difftime(Sys.time(), run_start, units = "mins"))))
