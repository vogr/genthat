#!/usr/bin/env Rscript

options(error=function() { traceback(3); quit(status=1, save="no") })

suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))
suppressPackageStartupMessages(library(genthat))

genthat_version <- devtools::as.package(find.package("genthat"))$version

store_stats <- function(db, table, stats, types) {
    i <- 0
    while(i < 3) {
        tryCatch({
            dbWriteTable(db, name=table, value=stats, append=TRUE, row.names=FALSE, field.types=types)
            break()
        }, error=function(e) {
            message("Storing to DB did not work: ", e$message, " - retrying")
            i <<- i + 1
        })
    }
}

option_list <-
    list(
        make_option("--db-host", type="character", help="DB hostname", metavar="HOST"),
        make_option("--db-port", type="integer", help="DB port", metavar="PORT"),
        make_option("--db-name", type="character", help="DB name", metavar="NAME"),
        make_option("--db-user", type="character", help="DB username", metavar="USER"),
        make_option("--db-password", type="character", help="DB password", metavar="PASSWORD"),
        make_option("--package", type="character", help="Package to trace", metavar="PATH"),
        make_option("--type", type="character", help="Type of code to run (exeamples, tests, vignettes)", metavar="TYPE"),
        make_option("--batch-size", type="integer", help="Batch size", default=1000, metavar="NUM"),
        make_option("--output", type="character", help="Name of the output directory for traces", default=tempfile(file="trace-package"), metavar="PATH"),
        make_option("--timestamp", type="character", help="Timestamp", metavar="TIMESTAMP"),
        make_option("--decorator", type="character", help="Decorator (onentry/onexit/onboth/trycatch)", metavar="DECORATOR", default="onexit"),
        make_option("--tracer", type="character", help="Tracer (sequence/set)", metavar="TRACER", default="set"),
        make_option("--run-only", help="Do not trace, just run the code", action="store_true", default=FALSE),
        make_option(c("-d", "--debug"), help="Debug output", action="store_true", default=FALSE),
        make_option(c("-q", "--quiet"), help="Quiet output", action="store_true", default=FALSE)
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

package <- opt$package
type <- match.arg(opt$type, c("examples", "tests", "vignettes"), several.ok=FALSE)
traces_dir <- opt$output
batch_size <- opt$`batch-size`
quiet <- opt$quiet
timestamp <- opt$timestamp

stopifnot(nchar(timestamp) > 0)

options(genthat.debug=opt$debug)

if (is.null(opt$`db-name`)) {
    db <- NULL
} else {
    db <- dbConnect(
        RMySQL::MySQL(),
        dbname=opt$`db-name`,
        host=opt$`db-host`,
        port=opt$`db-port`,
        user=opt$`db-user`,
        password=opt$`db-password`
    )

    db_info <- dbGetInfo(db)
    message("Connected to ", db_info$dbname, "@", db_info$conType)
}

run <- function() {

    stopwatch <- new.env(parent=emptyenv())

    runner <- function(fname, quiet) {
        tag <- tools::file_path_sans_ext(basename(fname))
        time <- system.time(
            ret <- genthat:::run_r_script(fname, quiet=quiet)
        )

        assign(tag, time["elapsed"], envir=stopwatch)

        ret
    }

    tryCatch({
        message("Running ", package, " ", type, " (quiet: ", quiet, ")")

        time <- system.time(
            status <- genthat::run_package(package, types=type, quiet=quiet, runner=runner)
        )

        time <- time["elapsed"]
        tags <- tools::file_path_sans_ext(basename(names(status[[1]])))
        rows <- data_frame(tag=tags, status=as.numeric(status))
        times <- data_frame(tag=tags, running_time=as.numeric(as.list(stopwatch)))

        rows <- full_join(rows, times, by="tag")
        rows %>% print(width=Inf, n=Inf)

        if (!is.null(db)) {
            rows <- rows %>%
                mutate(ts=timestamp, genthat=genthat_version) %>%
                select(ts, genthat, everything())

            types <- as.list(sapply(names(rows), function(x) dbDataType(RMySQL::MySQL(), rows[[x]])))
            types$tag <- "text"
            types$status <- "integer"
            types$running_time <- "double"

            store_stats(db, "runs", rows, types)
        }

        message("\nRunning of ", package, " ", type, " finished in ", time)

        invisible(NULL)
    }, error=function(e) {
        message("Running of ", package, " ", type, " failed with: ", e$message, "\n")
        stop(e$message)
    })
}

trace <- function() {
    stopifnot(batch_size > 0)
    stopifnot(dir.exists(traces_dir) || dir.create(traces_dir))
    options(genthat.default_decorate_method=opt$decorator)
    options(genthat.default_tracer=opt$tracer)

    tryCatch({
        message("Tracing ", package, " ", type, " (quiet: ", quiet, ", batch_size: ", batch_size, ") in ", traces_dir)

        time <- system.time(
            rows <- genthat::trace_package(package, type, traces_dir, quiet=quiet, batch_size=batch_size)
        )

        time <- time["elapsed"]
        rows <- as_data_frame(rows)

        rows %>% print(width=Inf, n=Inf)

        if (!is.null(db)) {
            rows <- rows %>%
                mutate(ts=timestamp, genthat=genthat_version) %>%
                select(ts, genthat, everything())

            types <- as.list(sapply(names(rows), function(x) dbDataType(RMySQL::MySQL(), rows[[x]])))
            types$tag <- "text"
            types$filename <- "text"
            types$n_traces <- "integer"
            types$n_complete <- "integer"
            types$n_entry <- "integer"
            types$n_error <- "integer"
            types$n_failures <- "integer"
            types$status <- "integer"
            types$running_time <- "double"

            store_stats(db, "traces", rows, types)
        }

        message("\nTracing of ", package, " ", type, " using ", opt$decorator, "/", opt$tracer, " finished in ", time, " with ", sum(rows$n_traces), " traces")

        invisible(NULL)
    }, error=function(e) {
        message("Tracing of ", package, " ", type, " using ", opt$decorator, "/", opt$tracer, " failed with: ", e$message, "\n")
        stop(e$message)
    })
}


if (opt$`run-only`) {
    run()
} else {
    trace()
}
