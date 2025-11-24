detect_springforward_overlaps <- function(
    DT,
    created_col = "created_date",
    closed_col  = "closed_date"
) {
  stopifnot(data.table::is.data.table(DT))
  
  DT_copy <- copy(DT)
  DT_copy[, year := year(get(created_col))]
  DT_copy[, dst_start := get_dst_start(year), by = year]  # <-- helper like get_dst_end
  
  # Candidate: created before the jump, closed after the jump
  DT_copy[, springforward_flag := (
    get(created_col) < dst_start &
      get(closed_col)  > dst_start
  )]
  
  summary <- DT_copy[springforward_flag == TRUE,
                     .(n_rows = .N),
                     by = year][order(year)]
  
  if (nrow(summary) > 0) {
    total <- DT_copy[springforward_flag == TRUE,
                     .(year = "TOTAL", n_rows = .N)]
    summary <- rbind(summary, total, fill = TRUE)
  }
  
  summary
}
