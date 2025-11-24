# Flag and quantify DST effects while keeping UTC as truth
annotate_dst_effects <- function(
    DT,
    created_col = "created_date",
    closed_col  = "closed_date",
    local_tz    = "America/New_York"
) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(created_col %in% names(DT), closed_col %in% names(DT))
  
  # Convert UTC instants to local-zone instants for reporting/flags
  DT[, `:=`(
    created_local = as.POSIXct(format(get(created_col), tz = local_tz, usetz = TRUE), tz = local_tz),
    closed_local  = as.POSIXct(format(get(closed_col),  tz = local_tz, usetz = TRUE), tz = local_tz)
  )]
  
  # Portable offset (seconds) from UTC for each local instant
  offset_sec <- function(x) {
    z <- format(x, "%z")        # e.g. "-0500", "+0400"
    s <- ifelse(substr(z,1,1) == "-", -1L, 1L)
    hh <- as.integer(substr(z,2,3)); mm <- as.integer(substr(z,4,5))
    s * (hh*3600L + mm*60L)
  }
  
  DT[, `:=`(
    off_created_sec = offset_sec(created_local),
    off_closed_sec  = offset_sec(closed_local)
  )]
  
  # Canonical (correct) duration from UTC instants
  DT[, duration_utc_sec := as.numeric(difftime(get(closed_col), get(created_col), units = "secs"))]
  
  # Naive local duration (what you'd get if you subtracted local timestamps)
  DT[, duration_local_naive_sec := as.numeric(difftime(closed_local, created_local, units = "secs"))]
  
  # Offset delta captures DST boundary crossing (+3600 for spring-forward, −3600 for fall-back)
  DT[, offset_delta_sec := off_closed_sec - off_created_sec]
  
  # Correct the naive local duration by removing offset delta — should equal UTC
  DT[, duration_local_corrected_sec := duration_local_naive_sec - offset_delta_sec]
  
  # Flags
  DT[, `:=`(
    spans_dst       = offset_delta_sec != 0,
    dst_kind        = data.table::fifelse(offset_delta_sec > 0, "spring-forward",
                                          data.table::fifelse(offset_delta_sec < 0, "fall-back", "none")),
    # tiny numerical jitter can happen — treat <0.5s as equal
    duration_match  = abs(duration_local_corrected_sec - duration_utc_sec) < 0.5
  )]
  
  # Optional: quick yearly counts by local year (from created)
  DT[, created_year_local := as.POSIXlt(created_local)$year + 1900L]
  dst_counts <- DT[spans_dst == TRUE, .N, by = .(created_year_local, dst_kind)][order(created_year_local, dst_kind)]
  
  invisible(list(dst_counts = dst_counts))
}
