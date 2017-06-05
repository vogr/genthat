#!/usr/bin/env Rscript

options(error=function() { traceback(2); if (!interactive()) quit("no", status=1, runLast=FALSE) })
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

result <- task("running generated tests", {
    genthat::run_generated_tests(tests$tests)
})

task(paste("saving to database", db_file), {
    message("- generation errors")
    list(name=names(tests$errors), message=unlist(tests$errors)) %>%
        as.data.frame() %>%
        copy_to(dest=db, name="test_gen_errors", temporary=FALSE) %>%
        invisible()

    message("- passed tests")
    result$passed %>%
        copy_to(dest=db, name="test_passed", temporary=FALSE) %>%
        invisible()

    message("- failed tests")
    result$failed %>%
        copy_to(dest=db, name="test_failed", temporary=FALSE) %>%
        invisible()

})

message("Done processing: ", traces_file)
