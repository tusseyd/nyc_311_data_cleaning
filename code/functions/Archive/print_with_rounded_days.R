# Helper to print data.table with duration_days rounded
print_with_rounded_days <- function(DT, print_cols, digits = 2, ...) {
  stopifnot(data.table::is.data.table(DT))
  
  if (!"duration_days" %in% names(DT)) {
    warning("No 'duration_days' column found in DT; printing as-is.")
    print(DT[, ..print_cols], ...)
    return(invisible(NULL))
  }
  
  out <- DT[, c(.SD, .(duration_days = round(duration_days, digits))),
            .SDcols = setdiff(print_cols, "duration_days")]
  
  print(out, ...)
  invisible(out)
}
