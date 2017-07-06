#!/usr/bin/env Rscript

options(error=function() { traceback(2); if (!interactive()) quit("no", status=1, runLast=FALSE) })
options(genthat.debug=isTRUE(as.logical(Sys.getenv("genthat.debug"))))

library(testthat)
library(devtools)
library(readr)
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))
library(genthat)

task <- function(name, expr) {
    message("PROCESS: Running ", name, "...")
    time <- system.time(r <- expr)
    message("PROCESS: Finished ", name, " in ", time["elapsed"], " sec")

    invisible(r)
}

process_batch <- function(batch, db, timestamp) {
    tests <- task("generating tests", {
        genthat::generate_tests(batch, quiet=FALSE, include_trace_dump=FALSE)
    })

    tests_generated <- tests %>% dplyr::filter(!is.na(code)) %>% nrow()
    message("PROCESS: # of generated test: ", tests_generated)

    runs <- task("running generated tests", {
        genthat::run_generated_tests(tests, quiet=FALSE)
    })

    tests_passed <- runs %>% dplyr::filter(result == 1) %>% nrow()
    message("PROCESS: # of passed tests: ", tests_passed)

    task("saving to database table test_gens_error", {
        tests %>%
            dplyr::filter(!is.na(error)) %>%
            dplyr::mutate(
                timestamp=timestamp,
                traces_file=traces_file
            ) %>%
            dplyr::select(timestamp, traces_file, dplyr::everything()) %>%
            dplyr::db_insert_into(con=db$con, table="test_gens_error", values=.) %>%
            invisible()
    })

    task("saving to database table test_runs", {
        runs %>%
            dplyr::mutate(
                timestamp=timestamp,
                traces_file=traces_file
            ) %>%
            dplyr::select(timestamp, traces_file, dplyr::everything()) %>%
            dplyr::db_insert_into(con=db$con, table="test_runs", values=.) %>%
            invisible()
    })

    data_frame(
        num_generated=tests_generated,
        num_passed=tests_passed,
    )
}

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
    stop("Usage: <RDS> <batch-size>")
}

traces_file <- args[1]
stopifnot(file.exists(traces_file))
batch_size <- as.numeric(args[2])
stopifnot(batch_size > 1)

db <- src_mysql(
    dbname="genthat",
    host="ginger.ele.fit.cvut.cz",
    password="genthat",
    port=6612
)
message("PROCESS: Connected to ", format(db))

traces <- task("loading traces", {
    readRDS(traces_file)
})
message("PROCESS: Loaded: ", length(traces), " traces")

if (length(traces) == 0) {
    message("PROCESS: no traces")
    quit(save="no")
}

timestamp <- Sys.time()
indexes <- 1:ceiling(length(traces) / batch_size)

result <- lapply(indexes, function(x) {
    lower <- (x - 1) * batch_size + 1
    upper <- min(length(traces), lower + batch_size)
    batch <- traces[lower:upper]

    # add the trace_id
    names(batch) <- lower:upper

    message("PROCESS: traces ", lower, "-", upper, "/", length(traces))
    process_batch(batch, db=db, timestamp=timestamp)
})

result <- dplyr::bind_rows(result)
message("PROCESS: Done processing: ", traces_file)

stats <- result %>% dplyr::summarise(
    timestamp=timestamp,
    traces_file=traces_file,
    num_traces=length(traces),
    num_generated=sum(num_generated),
    generated=num_generated / num_traces * 100,
    num_passed=sum(num_passed),
    passed=num_passed / num_generated * 100,
    success_rate=num_passed / num_traces * 100
)

db_insert_into(con=db$con, table="stats", values=stats)
print(stats, width=Inf)
