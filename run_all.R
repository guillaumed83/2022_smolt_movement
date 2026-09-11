refit_state_space <- FALSE
refit_cmr_models <- FALSE
run_bootstraps <- TRUE
write_outputs <- TRUE

local({
  if (!file.exists("data/historical_rst_summary.csv")) stop("Run from the repository root.")
  required_packages <- c("coda", "ggplot2")
  if (refit_state_space || refit_cmr_models) required_packages <- c(required_packages, "rjags")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages)) stop("Restore dependencies first. Missing: ", paste(missing_packages, collapse = ", "))
  timezone <- "America/Toronto"
  for (folder in c("output/figures", "output/tables")) dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  read_input <- function(path, fields, key = NULL) {
    if (!file.exists(path)) stop("Required input is missing: ", path)
    x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                  colClasses = if ("fish_id" %in% fields) c(fish_id = "character") else NA)
    missing <- setdiff(fields, names(x))
    if (length(missing)) stop(path, " is missing fields: ", paste(missing, collapse = ", "))
    if (!is.null(key) && (anyNA(x[key]) || anyDuplicated(x[key]))) stop(path, " has missing or duplicate keys.")
    x
  }
  parse_time <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S%z", tz = timezone)
  write_table <- function(x, name) {
    if (write_outputs) write.csv(x, file.path("output/tables", paste0(name, ".csv")), row.names = FALSE, na = "NA")
    invisible(x)
  }
  save_figure <- function(name, width, height, draw) {
    if (!write_outputs) return(invisible(NULL))
    for (extension in c("png", "pdf")) {
      path <- file.path("output/figures", paste0(name, ".", extension))
      if (extension == "png") png(path, width = width, height = height, units = "in", res = 300, pointsize = 12, type = "cairo")
      else pdf(path, width = width, height = height, pointsize = 12, useDingbats = FALSE)
      tryCatch(draw(), finally = dev.off())
    }
    invisible(NULL)
  }
  stage_times <- list()
  warnings_seen <- character()
  started <- proc.time()[[3]]
  gc(reset = TRUE)
  for (script in c("01_historical_recaptures.R", "02_telemetry_descriptives.R", "03_fit_state_space.R",
                   "04_state_space_outputs.R", "05_cmr_sensitivity.R", "06_verify_reported_results.R")) {
    message("Running ", script)
    stage_started <- proc.time()[[3]]
    withCallingHandlers(sys.source(file.path("R", script), envir = environment()),
                        warning = function(w) warnings_seen <<- c(warnings_seen, conditionMessage(w)))
    stage_times[[script]] <- data.frame(stage = script, elapsed_seconds = proc.time()[[3]] - stage_started)
  }
  memory <- gc()
  if (write_outputs) {
    write.csv(do.call(rbind, stage_times), "output/stage_runtimes.csv", row.names = FALSE)
    write.csv(data.frame(elapsed_seconds = proc.time()[[3]] - started,
                         peak_R_heap_mb = sum(memory[, 6]), refit_state_space = refit_state_space,
                         refit_cmr_models = refit_cmr_models, run_bootstraps = run_bootstraps),
              "output/run_metrics.csv", row.names = FALSE)
    writeLines(capture.output(sessionInfo()), "output/session_info.txt")
    writeLines(unique(warnings_seen), "output/warnings.txt")
    files <- sort(c(list.files("output/figures", full.names = TRUE), list.files("output/tables", full.names = TRUE)))
    write.csv(data.frame(file = files, md5 = unname(tools::md5sum(files))), "output/checksums.csv", row.names = FALSE)
  }
  message("Completed in ", round(proc.time()[[3]] - started, 2), " seconds.")
})
