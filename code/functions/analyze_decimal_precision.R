analyze_decimal_precision <- function(
    DT,
    field_name,
    low_precision_threshold = 10L,
    verbose = TRUE,
    chart_dir = NULL,       # NEW
    generate_plots = TRUE   # NEW
) {
  stopifnot(data.table::is.data.table(DT))
  if (!field_name %in% names(DT)) stop(sprintf("Field '%s' not found in DT.", field_name))
  
  field_vals <- DT[[field_name]]
  valid_idx  <- !is.na(field_vals)
  valid_vals <- field_vals[valid_idx]
  
  if (!length(valid_vals)) {
    warning(sprintf("%s: No valid values found", field_name))
    return(NULL)
  }
  
  # Count decimals
  char_values <- as.character(valid_vals)
  decimal_places <- vapply(char_values, function(x) {
    if (!grepl("\\.", x)) return(0L)
    parts <- unlist(strsplit(x, "\\."))
    if (length(parts) == 2 && nchar(parts[2]) > 0) return(nchar(parts[2]))
    0L
  }, integer(1L))
  
  # Full-length vector for DT
  full_decimal_places <- rep(NA_integer_, nrow(DT))
  full_decimal_places[valid_idx] <- decimal_places
  
  # Write to DT
  dec_col  <- paste0(field_name, "_dec")
  flag_col <- paste0("low_", field_name)
  DT[, (dec_col)  := full_decimal_places]
  DT[, (flag_col) := full_decimal_places < low_precision_threshold]
  
  # Precision summary
  precision_summary <- data.table(decimal_places = decimal_places)[
    , .(N = .N), by = decimal_places][order(decimal_places)]
  
  total_n <- sum(precision_summary$N)
  precision_summary[, pct := if (total_n > 0L) round(100 * N / total_n, 2) else 0]
  precision_summary[, cum_pct := round(cumsum(pct), 2)]
  
  # Agency Pareto Summary
  agency_summary <- DT[get(flag_col) == TRUE & !is.na(agency),
                       .(N = .N), by = agency][order(-N)]
  
  if (nrow(agency_summary)) {
    total_low <- sum(agency_summary$N)
    agency_summary[, pct := round(100 * N / total_low, 2)]
    agency_summary[, cum_pct := round(cumsum(pct), 2)]
  }
  
  # Printout (only if verbose)
  if (isTRUE(verbose)) {
    cat(sprintf("\n=== %s Precision Summary ===\n", toupper(field_name)))
    print(precision_summary, row.names = FALSE)
    
    low_count <- sum(DT[[flag_col]], na.rm = TRUE)
    cat(sprintf("\nLow precision threshold: < %d decimals\n", low_precision_threshold))
    cat(sprintf("Low precision total: %s (%.2f%%)\n",
                format(low_count, big.mark=","), 
                100 * low_count / sum(valid_idx)))
    
    if (nrow(agency_summary)) {
      cat("\n--- Low Precision by Agency (Pareto) ---\n")
      print(head(agency_summary, 20), row.names = FALSE)
    }
  }
  
  # === NEW PARETO CHART CREATION ===
  if (generate_plots && nrow(agency_summary) > 0) {
    # Default chart directory if missing
    if (is.null(chart_dir)) chart_dir <- getwd()
    if (!dir.exists(chart_dir)) {
      dir.create(chart_dir, recursive = TRUE)
    }
    
    filename <- sprintf("pareto_low_precision_%s_by_agency.pdf", field_name)
    
    # Pass the FILTERED raw data, not the aggregated summary
    low_precision_dt <- DT[get(flag_col) == TRUE & !is.na(agency)]
    
    plot_pareto_combo(
      DT = low_precision_dt,           # Raw data with low precision rows only
      x_col = agency,
      chart_dir = chart_dir,
      filename = filename,
      title = sprintf("Low Precision %s by Agency", tools::toTitleCase(field_name)),
      subtitle = sprintf("Threshold < %d decimal places", low_precision_threshold),
      top_n = 30L
    )
    cat(sprintf("\nSaved Pareto chart: %s\n",
                file.path(chart_dir, filename)))
  }
  
  invisible(list(
    summary_precision = precision_summary[],
    summary_agency    = agency_summary[],
    col_dec           = dec_col,
    col_flag          = flag_col
  ))
}
