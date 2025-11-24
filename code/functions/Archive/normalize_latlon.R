normalize_latlon <- function(
    d311,
    lat_col      = "latitude",
    lon_col      = "longitude",
    decimals     = 5L,              # 5 → ~1.1 m in latitude
    create_raw   = TRUE,            # keep *_raw copies for provenance
    rebuild_location = FALSE,       # optionally rewrite a canonical 'location'
    location_col = "location"
) {
  stopifnot(data.table::is.data.table(d311))
  stopifnot(lat_col %in% names(d311), lon_col %in% names(d311))
  
  # preserve raw once (idempotent)
  if (isTRUE(create_raw)) {
    raw_lat <- paste0(lat_col, "_raw")
    raw_lon <- paste0(lon_col, "_raw")
    if (!raw_lat %in% names(d311)) d311[, (raw_lat) := as.numeric(get(lat_col))]
    if (!raw_lon %in% names(d311)) d311[, (raw_lon) := as.numeric(get(lon_col))]
  }
  
  # coerce & round working columns
  d311[, (lat_col) := round(as.numeric(get(lat_col)), decimals)]
  d311[, (lon_col) := round(as.numeric(get(lon_col)), decimals)]
  
  # optional canonical location string
  if (isTRUE(rebuild_location)) {
    fmt <- sprintf("POINT (%%.%df %%.%df)", decimals, decimals)  # "POINT (lon lat)"
    d311[, (location_col) := sprintf(fmt, get(lon_col), get(lat_col))]
  }
  
  tol <- 0.5 * 10^(-decimals)   # half-ULP at chosen precision
  invisible(list(decimals = decimals, tol = tol))
}
