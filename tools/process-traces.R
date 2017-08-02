#!/usr/bin/env Rscript

options(error=function() { traceback(2); if (!interactive()) quit("no", status=1, runLast=FALSE) })
options(genthat.debug=isTRUE(as.logical(Sys.getenv("genthat.debug"))))

library(testthat)
library(devtools)
library(readr)
library(knitr)
suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))
tryCatch(library(genthat), error=function(e) devtools::load_all())

task <- function(name, expr) {
    message("PROCESS: Running ", name, "...")
    time <- system.time(expr)
    message("PROCESS: Finished ", name, " in ", time["elapsed"], " sec")
}

process_trace <- function(id, trace) {
    result <- list(
        id=id,
        trace=format(trace),
        fun=trace$fun,
        test_gen_code=NA,
        test_gen_error=NA,
        test_gen_elapsed=NA,
        run_status=NA,
        run_error=NA,
        run_output=NA,
        run_elapsed=NA
        )

    result <- tryCatch({
        test <- generate_test(trace)

        result$test_gen_code <- test$code
        result$test_gen_error <- test$error
        result$test_gen_elapsed <- test$elapsed

        result
    }, error=function(e) {
        result$test_gen_error <- e$message

        result
    })

    if (!is.na(test$code)) {
        return(result)
    }

    result <- tryCatch({
        run <- run_generated_test(test)

        result$run_status <- run$status
        result$run_error <- run$error
        result$run_output <- run$output
        result$run_elapsed <- run$elapsed

        result
    }, error=function(e) {
        result$run_error <- e$message

        result
    })

    result
}


process_traces <- function(traces, db, timestamp) {
    task("generating tests", {
        tests <- genthat::generate_tests(traces, quiet=FALSE, include_trace_dump=FALSE)
    })

    tests_generated <- tests %>% dplyr::filter(!is.na(code)) %>% nrow()
    message("PROCESS: # of generated test: ", tests_generated)

    task("running generated tests", {
        runs <- genthat::run_generated_tests(teststests_generated, quiet=FALSE)
    })

    tests_passed <- runs %>% dplyr::filter(status == 1) %>% nrow()
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

option_list <-
    list(
        make_option("--db-host", type="character", help="DB hostname", metavar="HOST"),
        make_option("--db-port", type="integer", help="DB port", metavar="PORT"),
        make_option("--db-name", type="character", help="DB name", metavar="NAME"),
        make_option("--db-user", type="character", help="DB username", metavar="USER"),
        make_option("--db-password", type="character", help="DB password", metavar="PASSWORD"),
        make_option("--traces", type="character", help="Package to trace", metavar="PATH")
    )

parser <- OptionParser(option_list=option_list)
opt <- {
    tryCatch({
        parse_args(parser)
    }, error=function(e) {
        message("Error: ", e$message)
        print_help(parser)
        quit(save="no", status=1)
    })
}

traces_file <- opt$traces
stopifnot(file.exists(traces_file))

db <-
    src_mysql(
        dbname=opt$`db-name`,
        host=opt$`db-host`,
        port=opt$`db-port`,
        user=opt$`db-user`,
        password=opt$`db-password`
    )
message("PROCESS: Connected to ", format(db))

task("loading traces", {
    traces <- readRDS(traces_file)
})
message("PROCESS: Loaded: ", length(traces), " traces from ", traces_file)

if (length(traces) == 0) {
    message("PROCESS: no traces")
    quit(save="no")
}

start_time <- Sys.time()
result <- process_traces(traces, db=db, timestamp=start_time)
elapsed <- Sys.time() - start_time

message("PROCESS: Done processing: ", traces_file, " in ", elapsed)

stats <-
    result %>%
    dplyr::summarise(
        timestamp=start_time,
        traces_file=traces_file,
        num_traces=length(traces),
        num_generated=sum(num_generated),
        generated=num_generated / num_traces * 100,
        num_passed=sum(num_passed),
        passed=num_passed / num_generated * 100,
        success_rate=num_passed / num_traces * 100,
        elapsed=elapsed
    )

invisible(db_insert_into(con=db$con, table="stats", values=stats))
knitr::kable(stats)
