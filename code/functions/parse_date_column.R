# ==========================================================
# parse_date_column.R
# ----------------------------------------------------------
# Converts character date columns to POSIXct in the local time
# zone (America/New_York), recording every step in the console.
#
# NYC Open Data has published the 311 dates in more than one text
# format (e.g. "2025-01-01 09:02:20" and "01/02/2025 05:05:54 PM"),
# so the format is not assumed. For each column:
#
#   1. Detect the format: try each candidate format on an evenly
#      spaced sample of values (parsed in UTC, which has no DST gaps)
#      and use the one that parses the most. Ties go to the earlier
#      format in `formats`; formats with AM/PM are listed before
#      24-hour ones, because a 24-hour format would silently ignore
#      a trailing "PM".
#   2. Parse the whole column in the local time zone.
#   3. Retry any failures with the other candidate formats, in case
#      a column mixes formats.
#   4. Repair values in the nonexistent spring-forward hour with
#      fix_dst_spring_forward() (moved forward one hour). These are
#      found from their clock time, not only from parse failures,
#      because on some platforms as.POSIXct silently shifts them
#      back an hour instead of returning NA.
#   5. Report what remains unparsed, the ambiguous fall-back-hour
#      values and the offset R assigned to them, and true midnight
#      and noon counts.
#
# Ambiguous fall-back times (01:00-01:59 on the fall-back date occur
# twice) are not changed: they parse successfully, and R assigns each
# to one of the two occurrences. The source data carries no UTC
# offset, so the true occurrence cannot be recovered; the assigned
# offsets are reported for transparency.
#
# Requires fix_dst_spring_forward.R (fix_dst_spring_forward, us_dst_dates).
# ==========================================================
parse_date_column <- function(
    temp_raw_data,
    valid_date_columns,
    unique_key_col = "unique_key",
    agency_col     = "agency",
    formats        = c("%Y-%m-%d %H:%M:%OS",      # 2025-01-01 09:02:20
                       "%Y-%m-%dT%H:%M:%OS",      # 2025-01-01T09:02:20.000
                       "%m/%d/%Y %I:%M:%S %p",    # 01/02/2025 05:05:54 PM
                       "%m/%d/%Y %H:%M:%S"),      # 01/02/2025 17:05:54
    fmt            = NULL,                        # optional: a format to try first
    local_tz       = "America/New_York",
    detect_n       = 10000L,
    show_n         = 10L) {
  
  # Make an explicit copy to avoid shallow copy warning
  temp_raw_data <- data.table::copy(temp_raw_data)
  if (!is.null(fmt)) formats <- unique(c(fmt, formats))
  
  fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE)
  parse_with <- function(v, f, tz) suppressWarnings(as.POSIXct(v, format = f, tz = tz))
  
  keys     <- temp_raw_data[[unique_key_col]]
  agencies <- if (agency_col %in% names(temp_raw_data)) temp_raw_data[[agency_col]] else NULL
  
  col_summary  <- list()
  remaining    <- list()
  dst_fix_logs <- list()
  
  for (col in valid_date_columns) {
    cat(sprintf("\n[%s] Converting to POSIXct (%s)...\n", col, local_tz))
    
    vals <- trimws(as.character(temp_raw_data[[col]]))
    vals[vals == ""] <- NA_character_
    nonmiss <- which(!is.na(vals))
    
    # --- 1. Detect the format from an evenly spaced sample ---
    chosen <- formats[1]
    if (length(nonmiss)) {
      samp  <- nonmiss[unique(round(seq(1, length(nonmiss),
                                        length.out = min(detect_n, length(nonmiss)))))]
      rates <- vapply(formats, function(f) mean(!is.na(parse_with(vals[samp], f, "UTC"))),
                      numeric(1))
      chosen <- formats[which.max(rates)]
      cat("  Format detection (share of sample parsed):\n")
      for (i in seq_along(formats)) {
        cat(sprintf("    %-24s %6.2f%%%s\n", formats[i], 100 * rates[i],
                    if (formats[i] == chosen) "  <- used" else ""))
      }
      cat(sprintf("  Example value: %s\n", vals[samp[1]]))
    }
    
    # --- 2. Parse the whole column in the local time zone ---
    parsed  <- parse_with(vals, chosen, local_tz)
    fail    <- which(!is.na(vals) & is.na(parsed))
    n_first <- length(fail)
    
    # --- 3. Retry failures with the other formats ---
    n_other <- 0L
    for (f in setdiff(formats, chosen)) {
      if (!length(fail)) break
      p  <- parse_with(vals[fail], f, local_tz)
      ok <- !is.na(p)
      if (any(ok)) {
        parsed[fail[ok]] <- p[ok]
        n_other <- n_other + sum(ok)
        cat(sprintf("  Recovered %s values with alternative format %s\n",
                    fmt_n(sum(ok)), f))
      }
      fail <- fail[!ok]
    }
    
    # --- 4. Repair the nonexistent spring-forward hour ---
    # Depending on the platform, as.POSIXct either returns NA for a
    # nonexistent time or silently shifts it (e.g. 02:15 -> 01:15 EST).
    # Candidates are therefore the failures plus any parsed value on a
    # spring-forward date between 01:00 and 03:59; the fix re-reads each
    # candidate's clock time in UTC and repairs only true 02:xx values.
    cand <- fail
    ok_idx <- which(!is.na(parsed))
    if (length(ok_idx)) {
      d_loc  <- as.Date(parsed[ok_idx], tz = local_tz)
      on_sf  <- ok_idx[d_loc %in% us_dst_dates(data.table::year(d_loc))$spring]
      if (length(on_sf)) {
        h    <- as.POSIXlt(parsed[on_sf], tz = local_tz)$hour
        cand <- sort(c(cand, on_sf[h >= 1L & h <= 3L]))
      }
      rm(d_loc)
    }
    dst <- fix_dst_spring_forward(vals, cand,
                                  formats    = c(chosen, setdiff(formats, chosen)),
                                  local_tz   = local_tz,
                                  unique_key = keys,
                                  agency     = agencies)
    n_dst <- length(dst$fixed_idx)
    if (n_dst) {
      parsed[dst$fixed_idx] <- dst$fixed_values
      fail <- setdiff(fail, dst$fixed_idx)
      dst_fix_logs[[col]] <- dst$log[, column := col]
      cat(sprintf("  DST spring-forward repair: %s values moved forward one hour\n",
                  fmt_n(n_dst)))
      print(utils::head(dst$log, show_n), row.names = FALSE)
    }
    
    # --- 5a. Report what remains unparsed ---
    if (length(fail)) {
      remaining[[col]] <- data.table::data.table(
        column         = col,
        unique_key     = keys[fail],
        agency         = if (!is.null(agencies)) agencies[fail] else NA_character_,
        original_value = vals[fail]
      )
      cat(sprintf("  WARNING: %s values could not be parsed; set to NA. Examples:\n",
                  fmt_n(length(fail))))
      print(utils::head(remaining[[col]], show_n), row.names = FALSE)
    } else {
      cat("  All non-missing values parsed.\n")
    }
    
    # --- 5b. Ambiguous fall-back hour: count and assigned offset ---
    n_amb <- 0L; n_edt <- 0L; n_est <- 0L
    ok_idx <- which(!is.na(parsed))
    if (length(ok_idx)) {
      d        <- as.Date(parsed[ok_idx], tz = local_tz)
      fb_dates <- us_dst_dates(data.table::year(d))$fall
      cand     <- ok_idx[d %in% fb_dates]
      if (length(cand)) {
        amb <- cand[as.POSIXlt(parsed[cand], tz = local_tz)$hour == 1L]
        if (length(amb)) {
          zone  <- format(parsed[amb], "%Z", tz = local_tz)
          n_amb <- length(amb)
          n_edt <- sum(zone == "EDT")
          n_est <- sum(zone == "EST")
          cat(sprintf(paste0("  Ambiguous fall-back hour (01:00-01:59): %s values ",
                             "(assigned EDT %s, EST %s)\n"),
                      fmt_n(n_amb), fmt_n(n_edt), fmt_n(n_est)))
        }
      }
    }
    
    # --- 5c. True midnight / noon (local clock time) ---
    n_mid <- 0L; n_noon <- 0L
    if (length(ok_idx)) {
      lt   <- as.POSIXlt(parsed[ok_idx], tz = local_tz)
      secs <- lt$hour * 3600L + lt$min * 60L + floor(lt$sec)
      n_mid  <- sum(secs == 0L)
      n_noon <- sum(secs == 12L * 3600L)
      rm(lt, secs)
    }
    cat(sprintf("  Midnights: %s (%.4f%%), Noons: %s (%.4f%%)\n",
                fmt_n(n_mid),  100 * n_mid  / max(1L, length(ok_idx)),
                fmt_n(n_noon), 100 * n_noon / max(1L, length(ok_idx))))
    
    col_summary[[col]] <- data.table::data.table(
      column             = col,
      format_used        = chosen,
      non_missing        = length(nonmiss),
      failed_first_pass  = n_first,
      recovered_other    = n_other,
      dst_spring_fixed   = n_dst,
      unparsed_set_na    = length(fail),
      ambiguous_fallback = n_amb,
      ambiguous_EDT      = n_edt,
      ambiguous_EST      = n_est,
      midnight           = n_mid,
      noon               = n_noon
    )
    
    # --- Replace the text column with the parsed POSIXct column ---
    temp_raw_data[, (col) := parsed]
  }
  
  summary_dt <- data.table::rbindlist(col_summary)
  cat("\n--- Date Parsing Summary ---\n")
  print(summary_dt, row.names = FALSE)
  
  cat("\nColumn classes after conversion:\n")
  print(sapply(temp_raw_data[, ..valid_date_columns], function(x) class(x)[1]))
  
  invisible(list(
    parsed_data = temp_raw_data,
    summary     = summary_dt,
    failures    = if (length(remaining)) data.table::rbindlist(remaining) else NULL,
    dst_fixes   = if (length(dst_fix_logs)) data.table::rbindlist(dst_fix_logs) else NULL
  ))
}
