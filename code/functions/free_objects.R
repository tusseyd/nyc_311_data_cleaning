################################################################################
# free_objects.R
# Memory tools shared by jds_datacleansings.R and
# data_prep_for_jds_datacleansing.R (one copy, used by both programs).
#
#   mem_init(program)      start a new memory trace for this program run
#   mem_log(label)         record memory and elapsed time (no gc(), so it does
#                          not change what it measures)
#   run_gc(label)          explicit gc() when use_gc is TRUE, then mem_log()
#   free_objects(...)      remove the named objects when use_free_objects is
#                          TRUE (ignores names that don't exist), then run_gc()
#   start_memory_monitor() open memory_monitor.R (in code_dir) in its own
#                          window; it reports every few seconds
#   mem_report()           print the trace, save it as a CSV, and tell the
#                          monitor window to take a last reading and close
#   progress_msg(text)     one-line progress message with clock time and
#                          elapsed time; shown on screen even when the
#                          console output is being written to a file (sink)
#
# Settings are read from the calling program (defaults in brackets):
#   use_free_objects [TRUE], use_gc [TRUE], mem_messages [TRUE],
#   console_dir, code_dir
# Memory readings need the ps package; without it they are NA.
################################################################################

.mem_flag <- function(name, default = TRUE) {
  isTRUE(get0(name, envir = .GlobalEnv, ifnotfound = default))
}

.mem_run_label <- function() {
  sprintf("free%s_gc%s", substr(.mem_flag("use_free_objects"), 1, 1),
          substr(.mem_flag("use_gc"), 1, 1))
}

mem_init <- function(program = "program") {
  assign("mem_trace",   data.table::data.table(), envir = .GlobalEnv)
  assign("mem_t0",      Sys.time(),               envir = .GlobalEnv)
  assign("mem_program", program,                  envir = .GlobalEnv)
  invisible(NULL)
}

mem_log <- function(label) {
  if (!exists("mem_trace", envir = .GlobalEnv)) mem_init()
  info <- if (requireNamespace("ps", quietly = TRUE)) ps::ps_memory_info() else NULL
  mb <- function(field) {
    if (is.null(info) || !field %in% names(info)) NA_real_
    else round(as.numeric(info[[field]]) / 1024^2)
  }
  if (.mem_flag("mem_messages")) {
    message(sprintf("[memory] %s  %s MB  (%s)", format(Sys.time(), "%H:%M:%S"),
                    if (is.na(mb("rss"))) "NA" else format(mb("rss"), big.mark = ","), label))
  }
  tr <- get("mem_trace", envir = .GlobalEnv)
  t0 <- get("mem_t0",    envir = .GlobalEnv)
  assign("mem_trace", data.table::rbindlist(list(tr, data.table::data.table(
    step        = nrow(tr) + 1L,
    label       = label,
    elapsed_sec = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1),
    rss_mb      = mb("rss"),
    peak_mb     = mb("peak_wset")     # Windows only; NA elsewhere
  ))), envir = .GlobalEnv)
  invisible(NULL)
}

run_gc <- function(label = "gc") {
  if (.mem_flag("use_gc")) invisible(gc())
  mem_log(label)
}

free_objects <- function(...) {
  objs <- intersect(c(...), ls(envir = .GlobalEnv))
  if (.mem_flag("use_free_objects") && length(objs)) rm(list = objs, envir = .GlobalEnv)
  run_gc(paste0("free_objects: ", paste(c(...), collapse = ", ")))
}

start_memory_monitor <- function(interval_sec = 10,
                                 script = file.path(get0("code_dir", envir = .GlobalEnv), "memory_monitor.R")) {
  if (!requireNamespace("ps", quietly = TRUE)) {
    message("Memory monitor not started: install.packages(\"ps\")"); return(invisible(NULL))
  }
  if (!length(script) || !file.exists(script)) {
    message("Memory monitor not started: memory_monitor.R not found in code_dir"); return(invisible(NULL))
  }
  program   <- get0("mem_program", envir = .GlobalEnv, ifnotfound = "program")
  run_label <- paste0(program, "_", .mem_run_label())
  log_csv   <- normalizePath(file.path(get("console_dir", envir = .GlobalEnv),
                                       sprintf("memory_monitor_%s_%s_%s.csv", Sys.info()[["nodename"]],
                                               run_label, format(Sys.time(), "%Y%m%d_%H%M"))),
                             winslash = "/", mustWork = FALSE)
  # mem_report() creates this file to tell the monitor the program is done
  stop_file <- sub("\\.csv$", ".stop", log_csv)
  unlink(stop_file)
  assign("mem_stop_file", stop_file, envir = .GlobalEnv)
  rscript   <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  if (.Platform$OS.type == "windows") {
    # "start" opens a new console window for the monitor
    shell(sprintf('start "R memory monitor" "%s" "%s" %d %g "%s" "%s" "%s"',
                  rscript, normalizePath(script, winslash = "/"), Sys.getpid(),
                  interval_sec, log_csv, run_label, stop_file), wait = FALSE)
  } else {
    system2(rscript, c(shQuote(script), Sys.getpid(), interval_sec, shQuote(log_csv), run_label,
                       shQuote(stop_file)),
            wait = FALSE, stdout = sub("\\.csv$", ".txt", log_csv))
  }
  message("Memory monitor started (every ", interval_sec, " sec); log: ", log_csv)
  invisible(log_csv)
}

mem_report <- function() {
  mem_log("end of program")
  tr <- get("mem_trace", envir = .GlobalEnv)
  program <- get0("mem_program", envir = .GlobalEnv, ifnotfound = "program")
  cat(sprintf("\n=== Memory trace: %s (use_free_objects = %s, use_gc = %s) ===\n",
              program, .mem_flag("use_free_objects"), .mem_flag("use_gc")))
  print(tr, nrows = Inf, row.names = FALSE)
  peak <- suppressWarnings(max(tr$peak_mb, tr$rss_mb, na.rm = TRUE))
  cat(sprintf("Peak memory: %s MB; total elapsed: %s sec\n",
              if (is.finite(peak)) format(peak, big.mark = ",") else "NA (install ps)",
              format(max(tr$elapsed_sec), big.mark = ",")))
  out <- file.path(get("console_dir", envir = .GlobalEnv),
                   sprintf("memory_trace_%s_%s.csv", program, .mem_run_label()))
  data.table::fwrite(tr, out)
  # Tell the monitor window (if running) to take a last reading and close
  sf <- get0("mem_stop_file", envir = .GlobalEnv, ifnotfound = NULL)
  if (!is.null(sf)) file.create(sf)
  invisible(out)
}

progress_msg <- function(text) {
  t0 <- get0("mem_t0", envir = .GlobalEnv, ifnotfound = Sys.time())
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  line <- sprintf("[%s  +%02d:%02d:%02d]  %s", format(Sys.time(), "%H:%M:%S"),
                  as.integer(el %/% 3600), as.integer(el %% 3600 %/% 60),
                  as.integer(el %% 60), text)
  # Record it in the console output file (if output is being sunk there)
  if (sink.number() > 0) cat("\n", line, "\n", sep = "")
  # Show it on screen even when messages are also being sunk to a file:
  # briefly restore the message stream to the console, then put it back.
  msg_con <- sink.number(type = "message")
  if (msg_con != 2L) {
    con <- getConnection(msg_con)
    sink(type = "message")
    on.exit(sink(con, type = "message"), add = TRUE)
  }
  message(line)
  invisible(NULL)
}
