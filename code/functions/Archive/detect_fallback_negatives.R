detect_fallback_negatives <- function(DT, created_col = "created_date", closed_col = "closed_date") {
  stopifnot(is.data.table(DT))
  
  DT_copy <- copy(DT)
  DT_copy[, year := year(get(created_col))]
  
  # Join DST end info for each year
  dst_info <- unique(DT_copy[, .(year)])[, get_dst_end(year), by = year]
  DT_copy  <- merge(DT_copy, dst_info, by = "year", all.x = TRUE)
  
  # Only rows where both created and closed are on the DST end date
  DT_copy <- DT_copy[
    as.Date(get(created_col)) == dst_end_date &
      as.Date(get(closed_col))  == dst_end_date
  ]
  
  # Candidate fallback cases: created/closed in 01:00–01:59 AND closed < created
  DT_copy <- DT_copy[
    get(created_col) >= dst_start & get(created_col) < dst_end &
      get(closed_col)  >= dst_start & get(closed_col) < dst_end &
      get(closed_col) < get(created_col)
  ]
  
  # Per-row negative minutes
  DT_copy[, neg_mins := as.numeric(difftime(get(closed_col), get(created_col), units = "mins"))]
  
  # Yearly summary
  summary <- DT_copy[, .(
    n_rows = .N,
    avg_negative_mins = round(mean(neg_mins), 2)
  ), by = year][order(year)]
  
  # Weighted average across all years
  if (nrow(summary) > 0) {
    total <- DT_copy[, .(
      year = "TOTAL",
      n_rows = .N,
      avg_negative_mins = round(sum(neg_mins) / .N, 2)
    )]
    summary <- rbind(summary, total, fill = TRUE)
  }
  
  summary
}
