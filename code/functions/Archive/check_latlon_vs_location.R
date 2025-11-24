check_latlon_vs_location <- function(
    d311,
    lat_col                = "latitude",
    lon_col                = "longitude",
    location_col           = "location",
    tol                    = 5e-6,                 # ~= 0.5 ULP at 5 decimals
    round_before_compare   = FALSE,                # if TRUE, round both sides first
    latlon_decimals        = NULL,                 # required only if round_before_compare = TRUE
    location_order         = c("auto","lat_lon","lon_lat"),
    coerce_blank_to_na     = TRUE,                 # treat "" as NA for lat/lon/location
    require_bounds_check   = TRUE,                 # enforce |lat|<=90, |lon|<=180 as invalid_bounds
    make_pareto            = FALSE,                # optional Pareto by agency for problems
    chart_dir              = "charts",
    pareto_file            = "latlon_location_mismatches_pareto.pdf",
    sample_n               = 5L                    # printed sample of problem rows
) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  if (!data.table::is.data.table(d311)) data.table::setDT(d311)
  
  # ---- Column checks --------------------------------------------------------
  needed <- c(lat_col, lon_col, location_col)
  miss <- setdiff(needed, names(d311))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse = ", "))
  
  has_agency     <- "agency"     %in% names(d311)
  has_unique_key <- "unique_key" %in% names(d311)
  
  # ---- Helpers --------------------------------------------------------------
  numify <- function(x, blank_to_na = TRUE) {
    if (is.numeric(x)) return(x)
    x <- as.character(x)
    if (blank_to_na) x[trimws(x) == ""] <- NA_character_
    suppressWarnings(as.numeric(x))
  }
  
  # Extract first two numeric tokens from free-form location strings.
  # Accepts signs and decimals; ignores "POINT (" / commas / spaces etc.
  extract_first_two_nums <- function(x) {
    x <- as.character(x)
    if (coerce_blank_to_na) x[trimws(x) == ""] <- NA_character_
    nums_list <- regmatches(x, gregexpr("[-+]?\\d*\\.?\\d+", x))
    getk <- function(v, k) if (length(v) >= k) v[[k]] else NA_character_
    n1 <- suppressWarnings(as.numeric(vapply(nums_list, getk, character(1), k = 1L)))
    n2 <- suppressWarnings(as.numeric(vapply(nums_list, getk, character(1), k = 2L)))
    list(n1 = n1, n2 = n2)
  }
  
  # ---- Coerce lat/lon and parse location -----------------------------------
  lat <- numify(d311[[lat_col]],  coerce_blank_to_na)
  lon <- numify(d311[[lon_col]],  coerce_blank_to_na)
  loc_parsed <- extract_first_two_nums(d311[[location_col]])
  loc1 <- loc_parsed$n1
  loc2 <- loc_parsed$n2
  
  # ---- Optional pre-rounding to a chosen working precision ------------------
  if (isTRUE(round_before_compare)) {
    if (is.null(latlon_decimals) || !is.finite(latlon_decimals)) {
      stop("round_before_compare=TRUE requires a finite 'latlon_decimals' value.")
    }
    lat  <- round(lat,  latlon_decimals)
    lon  <- round(lon,  latlon_decimals)
    loc1 <- round(loc1, latlon_decimals)
    loc2 <- round(loc2, latlon_decimals)
  }
  
  # ---- Availability & bounds ------------------------------------------------
  have_all <- is.finite(lat) & is.finite(lon) & is.finite(loc1) & is.finite(loc2)
  invalid_bounds <- if (isTRUE(require_bounds_check)) {
    have_all & (abs(lat) > 90 | abs(lon) > 180)
  } else {
    rep(FALSE, length(have_all))
  }
  
  # ---- Compare both possible orders with tolerance -------------------------
  match_latlon <- have_all & (abs(lat - loc1) <= tol) & (abs(lon - loc2) <= tol)
  match_lonlat <- have_all & (abs(lat - loc2) <= tol) & (abs(lon - loc1) <= tol)
  
  # ---- Choose the location order -------------------------------------------
  location_order <- match.arg(location_order)
  if (location_order == "auto") {
    n_latlon <- sum(match_latlon, na.rm = TRUE)
    n_lonlat <- sum(match_lonlat, na.rm = TRUE)
    
    if (n_lonlat > n_latlon) {
      chosen <- "lon_lat"
    } else if (n_latlon > n_lonlat) {
      chosen <- "lat_lon"
    } else {
      # Tie-break heuristic: if the FIRST parsed number frequently exceeds 90 in abs value,
      # it's likely longitude first → choose "lon_lat".
      frac_first_gt90 <- mean(abs(loc1) > 90, na.rm = TRUE)
      chosen <- if (is.finite(frac_first_gt90) && frac_first_gt90 > 0.5) "lon_lat" else "lat_lon"
    }
  } else {
    chosen <- location_order
  }
  
  # ---- Final status labels --------------------------------------------------
  status <- data.table::fcase(
    !have_all,                                "missing_component",
    invalid_bounds,                           "invalid_bounds",
    chosen == "lat_lon" & match_latlon,       "match",
    chosen == "lon_lat" & match_lonlat,       "match",
    chosen == "lat_lon" & match_lonlat,       "opposite_order",
    chosen == "lon_lat" & match_latlon,       "opposite_order",
    default                                   = "mismatch"
  )
  
  # ---- Assemble result table ------------------------------------------------
  res <- data.table::data.table(
    latitude  = lat,
    longitude = lon,
    loc_1     = loc1,
    loc_2     = loc2,
    status    = status
  )
  if (has_unique_key) res[, unique_key := d311[["unique_key"]]]
  if (has_agency)     res[, agency     := d311[["agency"]]]
  data.table::setcolorder(res, c(intersect(c("unique_key","agency"), names(res)),
                                 "latitude","longitude","loc_1","loc_2","status"))
  
  # ---- Summary & console output --------------------------------------------
  total_n <- nrow(res)
  tab <- res[, .N, by = status][order(-N)]
  cat("\n— Latitude/Longitude vs Location Check —\n")
  if (location_order == "auto") {
    cat(sprintf("Order chosen: %s  |  tol = %.1e degrees\n", chosen, tol))
  } else {
    cat(sprintf("Order specified: %s  |  tol = %.1e degrees\n", chosen, tol))
  }
  print(tab[, .(status, N, pct = round(100 * N / total_n, 4))], row.names = FALSE, right = FALSE)
  
  # Problem rows (do not count 'opposite_order' as a problem once order is chosen)
  issues <- res[status %chin% c("mismatch","invalid_bounds")]
  
  if (nrow(issues)) {
    cat("\nSample problem rows:\n")
    show_cols <- c("unique_key","agency","latitude","longitude","loc_1","loc_2","status")
    show_cols <- intersect(show_cols, names(issues))
    print(issues[, ..show_cols][1:min(.N, sample_n)], row.names = FALSE, right = FALSE)
    
    # Optional Pareto by agency for problem rows
    if (isTRUE(make_pareto) && has_agency) {
      if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
      if (exists("plot_pareto_combo", mode = "function")) {
        # Try tidy-eval signature first (x_col = agency), then counts-table fallback
        ok <- FALSE
        try({
          plot_pareto_combo(
            DT        = issues,
            x_col     = agency,
            title     = "Lat/Lon vs Location mismatches by agency",
            filename  = pareto_file,
            chart_dir = chart_dir
          )
          ok <- TRUE
        }, silent = TRUE)
        if (!ok) {
          pareto_dt <- issues[, .(category = agency, count = .N), by = agency]
          try({
            plot_pareto_combo(
              DT           = pareto_dt,
              category_col = "category",
              count_col    = "count",
              title        = "Lat/Lon vs Location mismatches by agency",
              chart_dir    = chart_dir,
              filename     = pareto_file
            )
            ok <- TRUE
          }, silent = TRUE)
        }
        if (ok) cat("Pareto saved to: ", file.path(chart_dir, pareto_file), "\n", sep = "")
        if (!ok) cat("NOTE: plot_pareto_combo() available but signature mismatch; skipped chart.\n")
      } else {
        cat("NOTE: plot_pareto_combo() not found; skipping Pareto chart.\n")
      }
    }
  } else {
    cat("\nAll rows with sufficient data are consistent (within tolerance).\n")
  }
  
  invisible(list(
    results        = res,
    issues         = issues,
    status_table   = tab,
    chosen_order   = chosen,
    tol            = tol,
    rounded        = isTRUE(round_before_compare),
    decimals       = if (isTRUE(round_before_compare)) latlon_decimals else NA_integer_
  ))
}
