################################################################################
# close_program.R
# Program termination and timing function
#
# Writes the closing summary (end time, run time) to the console output
# file when a sink is active, and always echoes the same summary to the
# screen with message(). message() writes to stderr, which sink() does
# not capture, so the operator sees the summary on screen even while
# output is being diverted to the file.
#
# If the program stops with an error, close_program() is never reached,
# so the absence of this summary on screen means the run did not finish.
################################################################################

close_program <- function(program_start,
                          enable_sink  = FALSE,
                          verbose      = TRUE,
                          program_name = NULL) {

  # ----------------------------------------------------------
  # Calculate program timing
  # ----------------------------------------------------------
  program_stop       <- Sys.time()
  formatted_start    <- format(program_start, "%Y-%m-%d %H:%M:%S")
  formatted_end_time <- format(program_stop,  "%Y-%m-%d %H:%M:%S")

  duration_seconds <- as.numeric(difftime(program_stop, program_start, units = "secs"))

  hours   <- floor(duration_seconds / 3600)
  minutes <- floor((duration_seconds %% 3600) / 60)
  seconds <- round(duration_seconds %% 60, 1)

  duration_string <- paste0(
    if (hours   > 0) paste0(hours,   " hours, ")   else "",
    if (minutes > 0) paste0(minutes, " minutes, ") else "",
    seconds, " seconds"
  )

  # ----------------------------------------------------------
  # Closing summary (same text for the file and the screen)
  # ----------------------------------------------------------
  summary_lines <- c(
    "",
    "*****END OF PROGRAM*****",
    if (!is.null(program_name)) paste("Program:            ", program_name),
    paste("Status:              Completed successfully"),
    paste("Execution started:  ", formatted_start),
    paste("Execution ended:    ", formatted_end_time),
    paste("Program run-time:   ", duration_string),
    ""
  )

  sink_was_active <- sink.number(type = "output") > 0L

  # 1. Write the summary to the console output file, if a sink is active
  if (sink_was_active) {
    cat(summary_lines, sep = "\n")
  }

  # 2. Close the sink, if requested
  sink_status <- if (isTRUE(enable_sink) && sink_was_active) {
    sink()
    "Sink file closed - console output restored to screen"
  } else if (isTRUE(enable_sink)) {
    "Sink was enabled but no active sink connection was found"
  } else if (sink_was_active) {
    "Sink still active (enable_sink = FALSE); output continues to the file"
  } else {
    "No sink file used - all output was to screen"
  }

  # 3. Always echo the summary to the screen (message() bypasses sink)
  if (isTRUE(verbose)) {
    message(paste(c(summary_lines[-length(summary_lines)],
                    paste("Console output:     ", sink_status),
                    ""),
                  collapse = "\n"))
  }

  # ----------------------------------------------------------
  # Return timing information
  # ----------------------------------------------------------
  invisible(list(
    start_time         = program_start,
    end_time           = program_stop,
    formatted_end_time = formatted_end_time,
    duration_seconds   = duration_seconds,
    duration_string    = duration_string,
    sink_was_active    = sink_was_active
  ))
  
  options(warn = 0)
}
