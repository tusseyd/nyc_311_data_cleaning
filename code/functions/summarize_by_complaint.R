# ==========================================================
# summarize_by_complaint
# ----------------------------------------------------------
# Summarizes a subset of service requests (e.g., one duration
# bin from analyze_duration_QA) by agency and complaint_type.
#
# The subset tables carry unique_key, agency, and duration_days
# but not complaint_type, so complaint_type is looked up from
# the full 311 table (source_dt) by unique_key.
#
# Output columns:
#   agency, complaint_type
#   avg_duration_days : mean duration for that agency/complaint pair
#   count             : number of SRs in the pair
#   pct               : count as a % of ALL SRs in the subset
#                       (computed before the min_count filter)
#   cum_pct           : running total of pct down the sorted table
#
# Because pct uses the full subset as its denominator, cum_pct
# reaches 100 only when min_count keeps every group. The gap
# shows how much of the subset falls below the cutoff.
#
# Sort order: count (descending), then agency, then complaint_type.
# ==========================================================
summarize_by_complaint <- function(subset_dt,
                                   source_dt,
                                   min_count = 50L,
                                   label     = deparse(substitute(subset_dt)),
                                   verbose   = TRUE) {
  
  # --- Progress messages (suppressed when verbose = FALSE) ---
  msg <- function(...) if (isTRUE(verbose)) message(sprintf(...))
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE)
  t0  <- Sys.time()
  
  msg("[summarize_by_complaint] %s: starting", label)
  
  # --- Input checks ---
  stopifnot(data.table::is.data.table(subset_dt),
            data.table::is.data.table(source_dt))
  
  need_sub <- c("unique_key", "agency", "duration_days")
  need_src <- c("unique_key", "complaint_type")
  miss_sub <- setdiff(need_sub, names(subset_dt))
  miss_src <- setdiff(need_src, names(source_dt))
  if (length(miss_sub)) stop("subset_dt is missing: ", paste(miss_sub, collapse = ", "))
  if (length(miss_src)) stop("source_dt is missing: ", paste(miss_src, collapse = ", "))
  
  n_subset <- nrow(subset_dt)
  if (n_subset == 0L) {
    msg("[summarize_by_complaint] %s: subset is empty; nothing to summarize", label)
    return(data.table::data.table(
      agency = character(), complaint_type = character(),
      avg_duration_days = numeric(), count = integer(),
      pct = numeric(), cum_pct = numeric()
    ))
  }
  msg("  Subset rows: %s", fmt(n_subset))
  
  # --- Lookup table: only the two columns needed from the large table ---
  lookup <- source_dt[, .(unique_key, complaint_type)]
  
  # --- Match key types: convert the small side, not the large side ---
  sub <- subset_dt[, .(unique_key, agency, duration_days)]
  if (is.numeric(lookup$unique_key)) {
    sub[, unique_key := as.numeric(unique_key)]
  } else {
    sub[, unique_key := as.character(unique_key)]
  }
  
  # --- Join complaint_type onto the subset by unique_key ---
  msg("  Joining complaint_type from source table (%s rows)", fmt(nrow(source_dt)))
  dt <- lookup[sub, on = "unique_key"]
  
  n_miss <- dt[is.na(complaint_type), .N]
  if (n_miss > 0L) {
    warning(sprintf("%s: %s rows had no complaint_type match on unique_key",
                    label, fmt(n_miss)))
  }
  
  # --- Aggregate by agency and complaint_type ---
  # pct uses every SR in the subset as the denominator,
  # so it is computed before the min_count filter.
  out <- dt[, .(avg_duration_days = round(mean(duration_days)),
                count             = .N),
            by = .(agency, complaint_type)]
  out[, pct_raw := 100 * count / n_subset]
  n_groups <- nrow(out)
  
  # --- Apply the count threshold ---
  out <- out[count >= min_count]
  msg("  Groups: %s total, %s with count >= %s (covering %s of %s SRs)",
      fmt(n_groups), fmt(nrow(out)), fmt(min_count),
      fmt(sum(out$count)), fmt(n_subset))
  
  # --- Sort, then build the running percentage ---
  data.table::setorder(out, -count, agency, complaint_type)
  out[, `:=`(pct     = round(pct_raw, 2),
             cum_pct = round(cumsum(pct_raw), 2))]
  out[, pct_raw := NULL]
  
  msg("[summarize_by_complaint] %s: done in %.1f sec",
      label, as.numeric(difftime(Sys.time(), t0, units = "secs")))
  
  out[]
}
