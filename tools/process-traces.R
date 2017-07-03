#!/usr/bin/env Rscript

options(error=function() { traceback(2); if (!interactive()) quit("no", status=1, runLast=FALSE) })
options(genthat.show_progress=TRUE)
## options(genthat.debug=T)

library(testthat)
library(devtools)
library(readr)
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))
library(RSQLite)

task <- function(name, expr) {
    message("Running ", name, "...")
    r <- stopwatch(expr)
    message("Finished ", name, " in ", r$time/1000, " sec")

    invisible(r$result)
}

devtools::load_all()

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
  stop("Usage: <RDS>")
}

traces_file <- args[1]
stopifnot(file.exists(traces_file))

db_file <- paste0(tools::file_path_sans_ext(traces_file), ".sqlite3")
if (file.exists(db_file)) {
    stop("The db file: ", db_file, " exits")
}

db <- src_sqlite(db_file, create=T)

traces <- task("reading traces", {
    readRDS(traces_file)
})

tests <- task("generating tests", {
    genthat::generate_tests(traces, include_trace_dump=TRUE)
})

runs <- task("running generated tests", {
    tests %>%
        dplyr::filter(!is.na(code)) %>%
        genthat::run_generated_tests()
})

## task("running generated tests in C++", {
##     .Call('genthat_run_generated_tests_cpp', PACKAGE = 'genthat', tests, FALSE)
## })

task(paste("saving to database", db_file), {
    message("- test generation errors")
    tests %>%
        dplyr::filter(is.na(code)) %>%
        copy_to(dest=db, name="test_gen_errors", temporary=FALSE) %>%
        invisible()

    message("- ran tests")
    runs %>%
        copy_to(dest=db, name="test_runs", temporary=FALSE) %>%
        invisible()
})

message("Done processing: ", traces_file)

num_traces <- length(traces)
message("Traces: ", num_traces)

generated <- tests %>% dplyr::filter(!is.na(code)) %>% nrow()
message("Generated tests: ", generated, " (", generated / num_traces * 100, "%)")

all <- nrow(runs)
passed <- runs %>% dplyr::filter(is.na(error)) %>% nrow()
message("Passed tests: ", passed, " (", passed / all * 100, "%)")

