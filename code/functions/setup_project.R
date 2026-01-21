################################################################################
# setup_project.R
# Project initialization function
################################################################################

setup_project <- function(
    enable_sink = FALSE,
    console_filename = "console_output.txt",
    verbose = TRUE
) {
  
  # ----------------------------------------------------------
  # STEP 1: Start timing and capture start time
  # ----------------------------------------------------------
  
  programStart <- as.POSIXct(Sys.time())
  formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")
  
  if (verbose) cat("\n***** Program initialization *****\n")
  message("Execution begins at:", formattedStartTime)
  
  # ----------------------------------------------------------
  # STEP 2: Load all required packages
  # ----------------------------------------------------------
  
  if (verbose) cat("\n========================================\n")
  if (verbose) cat("LOADING PACKAGES\n")
  if (verbose) cat("========================================\n\n")
  
  required_packages <- c(
    "data.table",      # Load FIRST - most important to avoid masking
    "arrow",
    "fasttime",
    "here",
    "zoo",
    "ggpmisc",
    "ggpattern",
    "ggrastr",
    "qcc",
    "qicharts2",
    "grid",
    "gridExtra",
    "sf",              # Load before tidyverse - can mask dplyr functions
    "stringdist",
    "tidyverse",       # Load late - includes dplyr, ggplot2, stringr, lubridate, scales
    "bslib",
    "shiny",
    "DT",
    "gt",
    "styler",
    "rlang",
    "renv",
    "remotes",
    "moments",
    "diptest"
  )
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (verbose) message(sprintf("📦 Installing missing package: %s", pkg))
      tryCatch(
        install.packages(pkg),
        error = function(e) message(sprintf("❌ Failed to install %s: %s", 
                                            pkg, e$message))
      )
    }
    
    # Try loading the package
    tryCatch({
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      if (verbose) message(sprintf("✅ Loaded: %s", pkg))
    }, error = function(e) {
      message(sprintf("❌ Failed to load %s: %s", pkg, e$message))
    })
  }
  
  # ----------------------------------------------------------
  # STEP 3: Set global options
  # ----------------------------------------------------------
  
  if (verbose) cat("\n========================================\n")
  if (verbose) cat("SETTING GLOBAL OPTIONS\n")
  if (verbose) cat("========================================\n\n")
  
  options(scipen = 999)                    # Avoid scientific notation
  options(digits = 15)                     # Max decimal places observed
  options(datatable.print.class = FALSE)   # Cleaner data.table output
  options(max.print = 100000)              # Allow more console output
  
  if (verbose) cat("✅ Global options set\n")
  
  # ----------------------------------------------------------
  # STEP 4: Source function files
  # ----------------------------------------------------------
  
  if (verbose) cat("\n========================================\n")
  if (verbose) cat("SOURCING FUNCTIONS\n")
  if (verbose) cat("========================================\n\n")
  
  # Use the globally defined functions_dir from directory setup
  if (!exists("functions_dir")) {
    stop("Error: functions_dir not found. Did you run directory setup first?")
  }
  
  function_files <- list.files(functions_dir, pattern = "\\.R$", 
                               full.names = TRUE)
  
  # Exclude setup_project.R from being sourced (avoid recursion)
  function_files <- function_files[!grepl("setup_project\\.R$", function_files)]
  
  sourced_count <- 0
  failed_count <- 0
  
  for (file in function_files) {
    tryCatch({
      # Get function count before sourcing
      functions_before <- sum(sapply(ls(.GlobalEnv), 
                                     function(x) is.function(get(x))))
      
      # Source the file
      source(file, local = FALSE)
      
      # Verify sourcing worked
      functions_after <- sum(sapply(ls(.GlobalEnv), 
                                    function(x) is.function(get(x))))
      
      if (verbose) {
        message("Successfully sourced: ", basename(file), 
                " (added ", functions_after - functions_before, " functions)")
      }
      sourced_count <- sourced_count + 1
      
    }, error = function(e) {
      message("ERROR sourcing: ", basename(file), " - ", e$message)
      failed_count <- failed_count + 1
    })
  }
  
  if (verbose) {
    message("\nSourcing complete: ", sourced_count, " files sourced, ", 
            failed_count, " failed")
  }
  
  # ----------------------------------------------------------
  # STEP 5: Set up console output sink (if requested)
  # ----------------------------------------------------------
  
  if (verbose) cat("\n========================================\n")
  if (verbose) cat("CONSOLE OUTPUT SETUP\n")
  if (verbose) cat("========================================\n\n")
  
  # Use the globally defined console_dir from directory setup
  if (!exists("console_dir")) {
    stop("Error: console_dir not found. Did you run directory setup first?")
  }
  
  console_output_file <- file.path(console_dir, console_filename)
  
  if (isTRUE(enable_sink)) {
    sink(console_output_file)
    if (verbose) cat("✅ Console output redirected to:", console_output_file, "\n")
  } else {
    if (verbose) cat("Console output: screen only (sink disabled)\n")
  }
  
  # Print execution start to both console and sink (if enabled)
  if (sink.number(type = "output") > 0L) {
    cat("\nExecution begins at:", formattedStartTime, "\n")
  }
  
  # Always print to screen (will only show if sink is off)
  cat("\nExecution begins at:", formattedStartTime, "\n")
  
  # ----------------------------------------------------------
  # STEP 6: Return timing info
  # ----------------------------------------------------------
  
  if (verbose) cat("\n========================================\n")
  if (verbose) cat("SETUP COMPLETE\n")
  if (verbose) cat("========================================\n\n")
  
  timing_info <- list(
    console_file  = console_output_file,
    program_start = programStart,
    start_time    = formattedStartTime
  )
  
  invisible(timing_info)  # Return timing info invisibly
}