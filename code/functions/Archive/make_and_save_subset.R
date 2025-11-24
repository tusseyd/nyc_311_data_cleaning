################################################################################
# Function to generate and save each subset as an RDS file

make_and_save_subset <- function(years_back, max_year) {

  start_date <- as.POSIXct(paste0(max_year - years_back + 1, 
                                  "-01-01 00:00:00"), tz = "UTC")
  end_date <- as.POSIXct(paste0(max_year, "-12-31 23:59:59"), tz = "UTC")
  
  subset <- raw_data[created_date >= start_date & created_date <= end_date]
  
  s_date <- format(start_date, "%m-%d-%Y")
  e_date <- format(end_date, "%m-%d-%Y")
  
  file_name <- sprintf(
    "%d-year_311SR_%s_thru_%s_AS_OF_%s.rds",
    years_back, s_date, e_date, as_of_date
  )
  
  full_path <- file.path(data_dir, file_name)
  saveRDS(subset, full_path)
  
  cat(sprintf("\n%d-year dataset: %s rows written to %s\n", years_back, 
              nrow(subset), full_path))
}