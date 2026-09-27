# =====================================================================
# recreate_pre2020_backlog_values.R
#
# Purpose
#   Recreate, from the NYC Open Data API, every value hardcoded in
#   summarize_backlog() for service requests (SRs) created before 2020,
#   and compare each one with the value in the function.
#
# Why these values are hardcoded
#   The study extract contains SRs created 2020-01-01 through
#   2024-12-31. SRs created before 2020 that were still open at the
#   January 1 boundaries of 2020-2024 are not in the extract, so they
#   were counted once from NYC Open Data, "311 Service Requests from
#   2010 to 2019" (dataset 76ig-c548), and the counts were fixed in
#   the code. This keeps the analysis self-contained. This script
#   shows exactly how each value was produced.
#
# Hardcoded values and their derivation (queries run 2026-09-26)
#
#   prior_resolved_later   Pre-2020 SRs with a valid closed_date on or
#                          after each boundary.
#                          Query 1 counts pre-2020 SRs closed on or
#                          after 2020-01-01, by year closed. For
#                          boundary Y, the value is the sum over valid
#                          closing years >= Y. Closing years after the
#                          year the query is run are invalid (e.g.
#                          2047, 3027) and are excluded.
#
#   prior_invalid_closed   Pre-2020 SRs whose closed_date is in the
#                          future (Query 1, invalid years).
#
#   prior_closed_no_date   Pre-2020 SRs with no closed_date whose
#                          status is "Closed" or "Closed - Testing"
#                          (Query 2). Closure time unknown; excluded.
#
#   PRE2020_STILL_OPEN_BY_YEAR
#                          Pre-2020 SRs with no closed_date and a
#                          status other than Closed, counted by year
#                          created (cy) and year of last resolution
#                          update (uy; NA = never updated) (Query 3).
#                          Its total is the pre-2020 still-open count
#                          used by Option A. Option B uses it to count
#                          SRs with no activity for abandon_years:
#                          abandoned at boundary Y when
#                          max(cy, uy) <= Y - abandon_years - 1.
#
#   Check                  Query 4 counts the strict backlog on
#                          2020-01-01 in a single query:
#                          prior_resolved_later[2020] + still-open total.
#
# Notes
#   - The dataset is updated daily, so counts from a later run may
#     differ slightly from the 2026-09-26 values.
#   - Each query can take 10-15 minutes. Results are saved to out_dir
#     and reused on later runs; delete a saved file to requery it.
#   - Run this script separately from the main analysis.
# =====================================================================

library(data.table)

# ---- Settings ----------------------------------------------------------
fn_file       <- file.path("code", "functions", "summarize_backlog.R")  # <-- edit if needed
out_dir       <- file.path("data", "pre2020_backlog_queries")
base          <- "https://data.cityofnewyork.us/resource/76ig-c548.json"
boundaries    <- 2020:2024
abandon_years <- 3L

options(timeout = 1800)   # allow each query up to 30 minutes
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
run_date <- Sys.Date()
run_year <- as.integer(format(run_date, "%Y"))

before_2020 <- "created_date<'2020-01-01T00:00:00'"

# ---- Query helper: runs a SoQL query once and saves the JSON result ----
soql <- function(name, select, where, group = NULL, order = NULL, limit = 5000) {
  path <- file.path(out_dir, paste0(name, ".json"))
  if (file.exists(path)) {
    message("Using saved result: ", path)
  } else {
    q <- paste0("?$select=", select, "&$where=", where,
                if (!is.null(group)) paste0("&$group=", group) else "",
                if (!is.null(order)) paste0("&$order=", order) else "",
                "&$limit=", limit)
    message("Querying ", name, " (this may take 10-15 minutes)...")
    t0 <- Sys.time()
    download.file(utils::URLencode(paste0(base, q)), path, mode = "wb", quiet = TRUE)
    message(sprintf("  finished in %.1f min",
                    as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  as.data.table(jsonlite::fromJSON(path))
}

# ---- Query 1: pre-2020 SRs closed on or after 2020-01-01, by year closed ----
q1 <- soql("q1_closed_after_2020_by_year",
           select = "date_extract_y(closed_date) AS yr, count(*) AS n",
           where  = paste0(before_2020, " AND closed_date>='2020-01-01T00:00:00'"),
           group  = "yr", order = "yr")
q1[, `:=`(yr = as.integer(yr), n = as.integer(n))]

recreated_resolved_later <- sapply(boundaries, function(y) q1[yr <= run_year & yr >= y, sum(n)])
names(recreated_resolved_later) <- boundaries
recreated_invalid_closed <- q1[yr > run_year, sum(n)]

cat("\nQuery 1: pre-2020 SRs closed on or after 2020-01-01, by year closed\n")
print(q1, row.names = FALSE)

# ---- Query 2: pre-2020 SRs with no closed date, by status ----------------
q2 <- soql("q2_no_closed_date_by_status",
           select = "status, count(*) AS n",
           where  = paste0(before_2020, " AND closed_date IS NULL"),
           group  = "status", order = "n DESC")
q2[, n := as.integer(n)]
q2[, closed_type := grepl("^Closed", status)]

recreated_closed_no_date <- q2[closed_type == TRUE,  sum(n)]
recreated_still_open     <- q2[closed_type == FALSE, sum(n)]

cat("\nQuery 2: pre-2020 SRs with no closed date, by status\n")
print(q2, row.names = FALSE)

# ---- Query 3: pre-2020 still-open SRs, by year created and year updated --
q3 <- soql("q3_still_open_by_created_and_update_year",
           select = paste0("date_extract_y(created_date) AS cy, ",
                           "date_extract_y(resolution_action_updated_date) AS uy, ",
                           "count(*) AS n"),
           where  = paste0(before_2020, " AND closed_date IS NULL ",
                           "AND status not in ('Closed','Closed - Testing')"),
           group  = "cy,uy", order = "cy,uy")
if (!"uy" %in% names(q3)) q3[, uy := NA_character_]
q3 <- q3[, .(cy = as.integer(cy), uy = as.integer(uy), n = as.integer(n))]

count_abandoned <- function(tab, y, k) {
  last_yr <- pmax(tab$cy, tab$uy, na.rm = TRUE)
  sum(tab$n[last_yr <= y - k - 1L])
}

# ---- Query 4: single-query check of the 2020 strict baseline ------------
validity_limit <- format(run_date + 1, "%Y-%m-%dT00:00:00")
q4 <- soql("q4_strict_baseline_2020",
           select = "count(*) AS n",
           where  = paste0(before_2020, " AND ((closed_date>='2020-01-01T00:00:00' ",
                           "AND closed_date<'", validity_limit, "') ",
                           "OR (closed_date IS NULL ",
                           "AND status not in ('Closed','Closed - Testing')))"))
recreated_strict_2020 <- as.integer(q4$n)

# ---- Values embedded in summarize_backlog() ------------------------------
source(fn_file)
fm <- formals(summarize_backlog)
embedded_resolved_later <- eval(fm$prior_resolved_later)
embedded_closed_no_date <- eval(fm$prior_closed_no_date)
embedded_invalid_closed <- eval(fm$prior_invalid_closed)
embedded_table          <- PRE2020_STILL_OPEN_BY_YEAR

# ---- Comparison ----------------------------------------------------------
cmp <- rbindlist(list(
  data.table(value     = paste0("prior_resolved_later[", boundaries, "]"),
             embedded  = as.integer(embedded_resolved_later[as.character(boundaries)]),
             recreated = as.integer(recreated_resolved_later)),
  data.table(value     = "prior_invalid_closed",
             embedded  = embedded_invalid_closed,
             recreated = recreated_invalid_closed),
  data.table(value     = "prior_closed_no_date",
             embedded  = embedded_closed_no_date,
             recreated = recreated_closed_no_date),
  data.table(value     = "pre-2020 still open (table total)",
             embedded  = sum(embedded_table$n),
             recreated = recreated_still_open),
  data.table(value     = paste0("abandoned_pre2020[", boundaries, "] (", abandon_years, "-yr rule)"),
             embedded  = sapply(boundaries, function(y) count_abandoned(embedded_table, y, abandon_years)),
             recreated = sapply(boundaries, function(y) count_abandoned(q3, y, abandon_years))),
  data.table(value     = "strict backlog 2020 (single query)",
             embedded  = as.integer(embedded_resolved_later[["2020"]] + sum(embedded_table$n)),
             recreated = recreated_strict_2020)
))
cmp[, difference := recreated - embedded]
cmp[, match := difference == 0L]

cat("\n==== Hardcoded values: embedded vs recreated ====\n")
print(cmp[, .(value,
              embedded   = format(embedded,  big.mark = ","),
              recreated  = format(recreated, big.mark = ","),
              difference = format(difference, big.mark = ","),
              match)],
      row.names = FALSE)

# Cell-by-cell comparison of the created-year x update-year table
tab_cmp <- merge(embedded_table[, .(cy, uy, embedded = n)],
                 q3[, .(cy, uy, recreated = n)],
                 by = c("cy", "uy"), all = TRUE)
tab_cmp[is.na(embedded),  embedded  := 0L]
tab_cmp[is.na(recreated), recreated := 0L]
n_diff <- tab_cmp[embedded != recreated, .N]
cat(sprintf("\nPRE2020_STILL_OPEN_BY_YEAR: %d of %d cells differ\n", n_diff, nrow(tab_cmp)))
if (n_diff) print(tab_cmp[embedded != recreated], row.names = FALSE)

cat(sprintf("\nRecreated on %s. Embedded values were queried on 2026-09-26.\n", run_date))
cat("Small differences reflect daily updates to the source dataset.\n")