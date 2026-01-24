################################################################################
# close_program.R
# Program termination and timing function
################################################################################

close_program <- function(program_start, enable_sink = FALSE, verbose = TRUE) {
  
  # ----------------------------------------------------------
  # Calculate program timing
  # ----------------------------------------------------------
  
  programStop <- as.POSIXct(Sys.time())
  formatted_end_time <- format(programStop, "%Y-%m-%d %H:%M:%S")
  
  # Calculate the duration of the program (in seconds)
  duration_seconds <- as.numeric(difftime(programStop, program_start, 
                                          units = "secs"))
  
  # Convert the duration to a formatted string (hours, minutes, and seconds)
  hours <- floor(duration_seconds / 3600)
  minutes <- floor((duration_seconds %% 3600) / 60)
  seconds <- round(duration_seconds %% 60, 4)  # Round to 4 decimal places
  
  # Create the formatted duration string
  duration_string <- paste0(
    if (hours > 0) paste0(hours, " hours, ") else "", 
    if (minutes > 0) paste0(minutes, " minutes, ") else "",
    seconds, " seconds"
  )
  
  # ----------------------------------------------------------
  # Print to sink file (if enabled)
  # ----------------------------------------------------------
  
  if (sink.number(type = "output") > 0L) {
    cat("\n\n*****END OF PROGRAM*****\n")
    cat("\n📅 Execution ends at:", formatted_end_time, "\n")
    cat("\n⏱️ Program run-time:", duration_string, "\n")
  }
  
  # ----------------------------------------------------------
  # Handle sink closure with verbose messaging
  # ----------------------------------------------------------
  
  sink_was_active <- sink.number(type = "output") > 0L
  
  if (isTRUE(enable_sink) && sink_was_active) {
    # Close the sink
    sink()
    
    # Confirm closure to screen
    if (verbose) {
      cat("\n========================================\n")
      cat("CONSOLE OUTPUT\n")
      cat("========================================\n")
      cat("✅ Sink file closed - console output restored to screen\n")
    }
  } else if (isTRUE(enable_sink) && !sink_was_active) {
    # Sink was requested but not active
    if (verbose) {
      cat("\n========================================\n")
      cat("CONSOLE OUTPUT\n")
      cat("========================================\n")
      cat("⚠️  Sink was enabled but no active sink connection found\n")
    }
  } else {
    # Sink was not enabled
    if (verbose) {
      cat("\n========================================\n")
      cat("CONSOLE OUTPUT\n")
      cat("========================================\n")
      cat("📺 No sink file used - all output was to screen\n")
    }
  }
  
  # ----------------------------------------------------------
  # Print final timing to screen
  # ----------------------------------------------------------
  
  if (verbose) {
    cat("\n*****END OF PROGRAM*****\n")
    cat("\n📅 Execution ends at:", formatted_end_time, "\n")
    cat("\n⏱️ Program run-time:", duration_string, "\n\n")
  }
  
  # ----------------------------------------------------------
  # Return timing information
  # ----------------------------------------------------------
  
  timing_info <- list(
    end_time = programStop,
    formatted_end_time = formatted_end_time,
    duration_seconds = duration_seconds,
    duration_string = duration_string,
    sink_was_active = sink_was_active
  )
  
  invisible(timing_info)
}

