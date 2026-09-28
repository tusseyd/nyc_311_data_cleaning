# ==========================================================
# fix_dst_spring_forward.R
# ----------------------------------------------------------
# Helpers for US daylight saving time (DST) transitions, and a
# repair for timestamps that fall in the nonexistent spring-
# forward hour (02:00:00-02:59:59 on the spring-forward date).
#
# Such clock times never occur in America/New_York, so they
# fail to parse in that time zone. The repair moves each one
# forward one hour (e.g. 02:15:00 -> 03:15:00), the reading a
# DST-aware clock would have shown. Only values that are in the
# nonexistent hour are changed; all other failures are left as NA.
#
# The repair is format-agnostic: failed strings are re-read in UTC
# (which has no DST gap) using the same candidate formats as the
# main parser, so any date format recognised there works here.
# ==========================================================

# US DST transition dates for the given years.
#   2007 onward: second Sunday in March, first Sunday in November.
#   Before 2007: first Sunday in April, last Sunday in October.
us_dst_dates <- function(years) {
  years <- sort(unique(as.integer(years[!is.na(years)])))
  if (!length(years)) {
    return(data.table::data.table(year = integer(), spring = as.Date(character()),
                                  fall = as.Date(character())))
  }
  first_sunday <- function(y, m) {
    d <- as.Date(sprintf("%d-%02d-01", y, m))
    d + (7L - as.POSIXlt(d)$wday) %% 7L
  }
  data.table::data.table(
    year   = years,
    spring = as.Date(ifelse(years >= 2007L, first_sunday(years, 3L) + 7L,
                            first_sunday(years, 4L)), origin = "1970-01-01"),
    fall   = as.Date(ifelse(years >= 2007L, first_sunday(years, 11L),
                            first_sunday(years, 11L) - 7L), origin = "1970-01-01")
  )
}

fix_dst_spring_forward <- function(vals,
                                   fail_idx,
                                   formats,
                                   local_tz   = "America/New_York",
                                   unique_key = NULL,
                                   agency     = NULL) {
  
  empty <- list(fixed_idx = integer(), fixed_values = as.POSIXct(character(), tz = local_tz),
                log = data.table::data.table())
  if (!length(fail_idx)) return(empty)
  
  # Re-read the failed strings in UTC, trying each candidate format
  s <- vals[fail_idx]
  u <- as.POSIXct(rep(NA_real_, length(s)), origin = "1970-01-01", tz = "UTC")
  for (f in formats) {
    todo <- which(is.na(u))
    if (!length(todo)) break
    p <- suppressWarnings(as.POSIXct(s[todo], format = f, tz = "UTC"))
    u[todo[!is.na(p)]] <- p[!is.na(p)]
  }
  
  # In the nonexistent hour: spring-forward date, hour 02
  spring_dates <- us_dst_dates(data.table::year(u))$spring
  in_gap <- !is.na(u) &
            as.Date(u, tz = "UTC") %in% spring_dates &
            as.POSIXlt(u, tz = "UTC")$hour == 2L
  if (!any(in_gap)) return(empty)
  
  # Move forward one hour and parse in the local time zone
  shifted <- format(u[in_gap] + 3600, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  newp    <- as.POSIXct(shifted, format = "%Y-%m-%d %H:%M:%S", tz = local_tz)
  ok      <- !is.na(newp)
  
  idx <- fail_idx[in_gap][ok]
  list(
    fixed_idx    = idx,
    fixed_values = newp[ok],
    log = data.table::data.table(
      unique_key = if (!is.null(unique_key)) unique_key[idx] else NA,
      agency     = if (!is.null(agency))     agency[idx]     else NA,
      before     = vals[idx],
      after      = format(newp[ok], "%Y-%m-%d %H:%M:%S %Z", tz = local_tz)
    )
  )
}
