# ==========================================================
# summarize_backlog
# ----------------------------------------------------------
# Backlog definition
#   The backlog at the start of year Y consists of service requests
#   (SRs) created before 00:00 on January 1 of Y that had not been
#   closed before that moment. It is based on recorded dates at the
#   year boundary, not on present status.
#
#   Each SR created before the boundary is placed in one group:
#
#     resolved_later       valid closed_date on or after the boundary
#     still_open           no closed_date; status is not Closed
#     closed_no_date       no closed_date; status begins "Closed"
#                          (closure time unknown; excluded)
#     invalid_closed_date  closed_date before created_date, or after
#                          the as-of date (impossible; excluded)
#
#   SRs closed before the boundary with a valid closed_date are not
#   in the backlog.
#
# Three backlog figures are reported for each year
#   backlog_strict  resolved_later + still_open
#   backlog_A       backlog_strict, excluding all still-open SRs
#                   created before 2020
#   backlog_B       backlog_strict, excluding still-open SRs judged
#                   abandoned: no activity for abandon_years or more
#                   before the boundary
#
#   Abandoned (Option B): a still-open SR whose most recent activity,
#   the later of created_date and resolution_action_updated_date, is
#   before January 1 of (Y - abandon_years). The rule is applied
#   identically to pre-2020 SRs and to SRs in the main dataset.
#   Only still-open SRs can be abandoned; SRs closed after the
#   boundary were demonstrably still in process.
#
#   Yearly values are point-in-time counts and are not summed.
#
# Pre-2020 SRs
#   The main dataset contains SRs created from 2020 onward. Pre-2020
#   SRs come from NYC Open Data, "311 Service Requests from 2010 to
#   2019" (dataset 76ig-c548), via API queries run on 2026-09-26.
#   The results are fixed below, so no second dataset is needed.
#
#   Base: https://data.cityofnewyork.us/resource/76ig-c548.json
#
#   (1) Resolved after the boundary, by year closed
#       $select=date_extract_y(closed_date) AS yr, count(*) AS n
#       $where=created_date<'2020-01-01T00:00:00'
#              AND closed_date>='2020-01-01T00:00:00'
#       $group=yr
#       -> 2020: 68,695  2021: 11,270  2022: 7,725  2023: 20,993
#          2024: 18,884  2025: 11,308
#          invalid years (2047, 2100, 2201, 2209, 3027): 9
#       prior_resolved_later[Y] = sum of valid years >= Y
#
#   (2) No closed date, by status
#       $select=status, count(*) AS n
#       $where=created_date<'2020-01-01T00:00:00' AND closed_date IS NULL
#       $group=status
#       -> Closed 2,151 + Closed - Testing 29 = 2,180 (closed_no_date)
#          all other statuses = 492,497 (still_open)
#
#   (3) Still open, by year created and year of last resolution update
#       $select=date_extract_y(created_date) AS cy,
#               date_extract_y(resolution_action_updated_date) AS uy,
#               count(*) AS n
#       $where=created_date<'2020-01-01T00:00:00' AND closed_date IS NULL
#              AND status not in ('Closed','Closed - Testing')
#       $group=cy,uy
#       -> 104 rows totalling 492,497 (PRE2020_STILL_OPEN_BY_YEAR below)
#
#   Single-query check of the 2020 strict baseline (expected 631,372):
#       $select=count(*) AS n
#       $where=created_date<'2020-01-01T00:00:00'
#              AND ((closed_date>='2020-01-01T00:00:00'
#                    AND closed_date<'2026-09-27T00:00:00')
#                   OR (closed_date IS NULL
#                       AND status not in ('Closed','Closed - Testing')))
# ==========================================================

# Pre-2020 still-open SRs by year created (cy) and year of last
# resolution update (uy; NA = never updated). Source: query (3) above.
PRE2020_STILL_OPEN_BY_YEAR <- data.table::rbindlist(list(
  data.table::data.table(cy = 2010L, uy = c(2003L, 2009L, 2010:2021, 2025L, NA),
                         n = c(1L, 8L, 35142L, 1446L, 295L, 181L, 66L, 88L, 35L, 24L, 49L, 11L,
                               9L, 5L, 1L, 10348L)),
  data.table::data.table(cy = 2011L, uy = c(2010:2021, 2023L, NA),
                         n = c(12L, 124444L, 1274L, 299L, 128L, 83L, 35L, 28L, 64L, 20L, 6L, 5L,
                               1L, 5664L)),
  data.table::data.table(cy = 2012L, uy = c(2011:2020, NA),
                         n = c(55L, 19720L, 1413L, 122L, 103L, 26L, 30L, 33L, 4L, 3L, 6528L)),
  data.table::data.table(cy = 2013L, uy = c(2012:2021, 2023L, NA),
                         n = c(19L, 18512L, 772L, 651L, 55L, 31L, 45L, 14L, 6L, 3L, 1L, 11200L)),
  data.table::data.table(cy = 2014L, uy = c(2013:2020, NA),
                         n = c(3L, 19435L, 3079L, 88L, 35L, 27L, 10L, 9L, 13098L)),
  data.table::data.table(cy = 2015L, uy = c(2015:2021, 2023L, NA),
                         n = c(26629L, 1895L, 412L, 99L, 43L, 10L, 2L, 1L, 18532L)),
  data.table::data.table(cy = 2016L, uy = c(2015:2021, 2024L, NA),
                         n = c(5L, 23209L, 2445L, 546L, 168L, 20L, 1L, 1L, 21127L)),
  data.table::data.table(cy = 2017L, uy = c(2016:2021, NA),
                         n = c(1L, 22376L, 2717L, 595L, 15L, 5L, 19079L)),
  data.table::data.table(cy = 2018L, uy = c(2018:2025, NA),
                         n = c(25401L, 3483L, 270L, 26L, 3L, 3L, 3L, 2L, 9859L)),
  data.table::data.table(cy = 2019L, uy = c(2019:2025, NA),
                         n = c(24428L, 2857L, 614L, 12L, 40L, 28L, 23L, 10610L))
))

summarize_backlog <- function(DT,
                              tz                       = NULL,
                              start_year               = 2020,
                              abandon_years            = 3L,
                              as_of_date               = get0("as_of_date", envir = .GlobalEnv,
                                                              ifnotfound = NULL),
                              prior_resolved_later     = c(`2020` = 138875L, `2021` = 70180L,
                                                           `2022` =  58910L, `2023` = 51185L,
                                                           `2024` =  30192L),
                              prior_closed_no_date     = 2180L,
                              prior_invalid_closed     = 9L,
                              prior_still_open_by_year = PRE2020_STILL_OPEN_BY_YEAR,
                              chart_metrics            = c("backlog_A", "backlog_B"),
                              data_dir                 = NULL,   # kept for compatibility; unused
                              chart_dir                = get0("chart_dir", envir = .GlobalEnv,
                                                              ifnotfound = "charts")) {
  
  # --- Input checks ---
  need <- c("created_date", "closed_date", "status", "resolution_action_updated_date")
  stopifnot(data.table::is.data.table(DT), all(need %in% names(DT)))
  stopifnot(is.numeric(abandon_years), length(abandon_years) == 1L,
            abandon_years >= 1, abandon_years == round(abandon_years))
  abandon_years <- as.integer(abandon_years)
  
  prior_still_open <- sum(prior_still_open_by_year$n)
  if (prior_still_open != 492497L) {
    message(sprintf("  Note: pre-2020 still-open table totals %s (query result was 492,497)",
                    format(prior_still_open, big.mark = ",")))
  }
  
  if (is.null(tz)) {
    tz <- attr(DT$created_date, "tzone")
    if (is.null(tz) || !nzchar(tz)) tz <- "America/New_York"
  }
  
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE)
  message(sprintf("[summarize_backlog] Main dataset: %s SRs; abandonment cutoff: %d years",
                  fmt(nrow(DT)), abandon_years))
  
  # --- Row-level flags for the main dataset, computed once ---
  created       <- DT$created_date
  closed        <- DT$closed_date
  updated       <- DT$resolution_action_updated_date
  closed_status <- grepl("^CLOSED", toupper(trimws(DT$status)))   # NA status -> FALSE
  
  close_before_created <- !is.na(closed) & !is.na(created) & closed < created
  close_after_as_of    <- if (is.null(as_of_date)) {
    message("  as_of_date not supplied; future closed and update dates are not checked")
    rep(FALSE, length(closed))
  } else {
    !is.na(closed) & closed > as_of_date
  }
  invalid_close <- close_before_created | close_after_as_of
  valid_close   <- !is.na(closed) & !invalid_close
  still_open    <- is.na(closed) & !closed_status
  
  message(sprintf("  Invalid closed dates excluded: %s before created_date, %s after as-of date",
                  fmt(sum(close_before_created)), fmt(sum(close_after_as_of))))
  
  # Most recent activity: later of created and a valid resolution update
  if (!is.null(as_of_date)) updated[!is.na(updated) & updated > as_of_date] <- NA
  last_activity <- pmax(created, updated, na.rm = TRUE)
  
  # Pre-2020 year of most recent activity
  prior_last_yr <- pmax(prior_still_open_by_year$cy, prior_still_open_by_year$uy, na.rm = TRUE)
  
  # --- Backlog at the start of each year ---
  yrs <- sort(unique(lubridate::year(created)))
  yrs <- yrs[yrs >= start_year]
  
  res <- data.table::rbindlist(lapply(yrs, function(y) {
    boundary <- as.POSIXct(sprintf("%d-01-01 00:00:00", y), tz = tz)
    cutoff   <- as.POSIXct(sprintf("%d-01-01 00:00:00", y - abandon_years), tz = tz)
    before   <- !is.na(created) & created < boundary
    
    prior_rl <- prior_resolved_later[as.character(y)]
    if (is.na(prior_rl)) {
      message(sprintf("  No pre-2020 resolved_later value for %d; using 0", y))
      prior_rl <- 0L
    }
    prior_ab <- sum(prior_still_open_by_year$n[prior_last_yr <= y - abandon_years - 1L])
    
    main_rl <- sum(before & valid_close & closed >= boundary)
    main_so <- sum(before & still_open)
    main_ab <- sum(before & still_open & last_activity < cutoff, na.rm = TRUE)
    main_cn <- sum(before & is.na(closed) & closed_status)
    main_iv <- sum(before & invalid_close)
    
    data.table::data.table(
      year                   = y,
      resolved_later         = as.integer(main_rl + prior_rl),
      still_open             = as.integer(main_so + prior_still_open),
      abandoned              = as.integer(main_ab + prior_ab),
      closed_no_date         = as.integer(main_cn + prior_closed_no_date),
      invalid_closed_date    = as.integer(main_iv + prior_invalid_closed),
      resolved_later_pre2020 = as.integer(prior_rl),
      still_open_pre2020     = as.integer(prior_still_open),
      abandoned_pre2020      = as.integer(prior_ab)
    )
  }))
  
  res[, backlog_strict := resolved_later + still_open]
  res[, backlog_A      := backlog_strict - still_open_pre2020]
  res[, backlog_B      := backlog_strict - abandoned]
  data.table::setcolorder(res, c("year", "backlog_strict", "backlog_A", "backlog_B",
                                 "resolved_later", "still_open", "abandoned",
                                 "closed_no_date", "invalid_closed_date"))
  
  # --- Console output: each table reads left to right as a calculation ---
  show <- function(DT, header) {
    cat("\n", header, "\n", sep = "")
    out <- data.table::copy(DT)
    num <- setdiff(names(out), "year")
    out[, (num) := lapply(.SD, fmt), .SDcols = num]
    print(out, row.names = FALSE)
  }
  
  cat("\n[Backlog: SRs created before 00:00 Jan 1 and not closed before it]\n")
  cat("  Source for pre-2020 SRs: NYC Open Data 76ig-c548, queried 2026-09-26\n")
  
  # 1. Strict backlog
  show(res[, .(year,
               resolved_later,
               `+ still_open`      = still_open,
               `= backlog_strict`  = backlog_strict)],
       "1. Strict backlog: open on Jan 1 (closed afterward, or never closed)")
  
  # 2. Option A
  show(res[, .(year,
               backlog_strict,
               `- still_open_pre2020` = still_open_pre2020,
               `= backlog_A`          = backlog_A)],
       "2. Option A: remove all never-closed SRs created before 2020")
  
  # 3. Option B
  show(res[, .(year,
               backlog_strict,
               `- abandoned_pre2020`  = abandoned_pre2020,
               `- abandoned_2020_on`  = abandoned - abandoned_pre2020,
               `= backlog_B`          = backlog_B)],
       sprintf("3. Option B: remove never-closed SRs with no activity for %d+ years",
               abandon_years))
  
  # 4. Excluded from all three
  show(res[, .(year, closed_no_date, invalid_closed_date)],
       paste0("4. Excluded from all measures: status Closed with no closed date;\n",
              "   closed date before created date or after the as-of date"))
  
  # 5. Activity among pre-2020 never-closed SRs
  n_updated_2020_on <- sum(prior_still_open_by_year$n[!is.na(prior_still_open_by_year$uy) &
                                                        prior_still_open_by_year$uy >= 2020L])
  cat(sprintf(paste0("\n5. Pre-2020 never-closed SRs with a resolution update in 2020 or later: ",
                     "%s of %s (%.1f%%)\n"),
              fmt(n_updated_2020_on), fmt(prior_still_open),
              100 * n_updated_2020_on / prior_still_open))
  
  # --- Charts (one bar chart per metric in chart_metrics) ---
  # Sizes come from chart_style(). Growth is shown in the subtitle; no
  # trendline, because with five bars it runs through the bar labels.
  chart_labels <- c(
    backlog_strict = "SRs open on January 1",
    backlog_A      = "SRs open on January 1, excluding pre-2020 SRs never closed",
    backlog_B      = sprintf("SRs open on January 1, excluding SRs inactive %d+ years",
                             abandon_years)
  )
  stopifnot(all(chart_metrics %in% names(chart_labels)))
  
  for (m in chart_metrics) {
    first <- res[1L, get(m)]
    last  <- res[.N, get(m)]
    growth <- sprintf("growth %d-%d: %.1f%%", res[1L, year], res[.N, year],
                      100 * (last - first) / first)
    plot_barchart(
      DT            = res,
      x_col         = "year",
      y_col         = m,
      title         = "SR Backlog at Start of Year",
      subtitle      = paste0(chart_labels[[m]], "; ", growth),
      show_summary  = FALSE,
      chart_dir     = chart_dir,
      filename      = paste0("annual_", m, "_bar_chart")
    )
  }
  
  invisible(res)
}
