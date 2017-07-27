#!/usr/bin/env Rscript

options(error=function() { traceback(3); quit(status=1, save="no") })

suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

tryCatch(library(genthat), error=function(e) devtools::load_all())

default_or_val <- function(val, default) {
    if (is.null(val)) {
        default
    } else {
        val
    }
}

do_trace_package <- function(package, type, traces_dir, batch_size, clean) {
    timestamp <- Sys.time()

    run <- genthat::capture(
        trace_result <- genthat::gen_from_package(
            package, type, traces_dir, batch_size=batch_size, quiet=FALSE, clean=clean
        ),
        split=TRUE
    )

    # the ret variable from the code supplied above
    run_pkg <- trace_result$run$image$ret[[type]]

    # TODO: the DB truncates the output
    ## log_file <- file.path(traces_dir, "run_package.log")
    ## write(run_pkg$output, log_file)

    row <-
        data_frame(
            timestamp=timestamp,
            type=type,
            package=package,

            n_traced_functions=length(trace_result$traced_functions),
            traced_functions=paste(trace_result$traced_functions, collapse="\n"),
            decorating_time=default_or_val(trace_result$decorating_time, 0),
            n_traces=default_or_val(trace_result$n_traces, 0),
            trace_files=paste(trace_result$trace_files, collapse="\n"),
            trace_saving_time=default_or_val(trace_result$saving_time, 0),

            trace_package_output=paste(run$stdout, run$stderr, collapse="\n"),
            run_status=trace_result$run$status,
            run_output=trace_result$run$output,
            run_command=trace_result$run$command,
            run_time=trace_result$run$elapsed,

            run_package_output=run_pkg$output,
            run_package_status=run_pkg$status,
            run_package_time=run_pkg$elapsed
        )

    field_types <- as.list(sapply(names(row), function(x) dbDataType(RMySQL::MySQL(), row[[x]])))
    field_types$trace_package_output <- "longtext"
    field_types$run_status <- "integer"
    field_types$run_output <- "longtext"
    field_types$run_package_status <- "integer"
    field_types$run_package_output <- "longtext"

    attr(row, "types") <- field_types
    row
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
        make_option("--output", type="character", help="Name of the output directory for traces", metavar="PATH"),
        make_option("--no-clean", help="Leave the temporary files in place", action="store_true", default=FALSE),
        make_option(c("-d", "--debug"), help="Debug output", action="store_true", default=FALSE)
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
clean <- !opt$`no-clean`

options(genthat.debug=opt$`debug`)

stopifnot(batch_size > 0)
stopifnot(dir.exists(traces_dir) || dir.create(traces_dir))

db <-
    dbConnect(
        RMySQL::MySQL(),
        dbname=opt$`db-name`,
        host=opt$`db-host`,
        port=opt$`db-port`,
        user=opt$`db-user`,
        password=opt$`db-password`
    )

db_info <- dbGetInfo(db)
message("Connected to ", db_info$dbname, "@", db_info$conType)

tryCatch({
    message("Tracing ", package, " ", type, " (batch_size: ", batch_size, ") in ", traces_dir)
    time <- system.time(row <- do_trace_package(package, type, traces_dir, batch_size, clean))

    i <- 0
    while(i < 3) {
        tryCatch({
            dbWriteTable(db, name="traces", value=row, append=TRUE, row.names=FALSE, field.types=attr(row, "types"))
            break()
        }, error=function(e) {
            message("Storing to DB did not work: ", e$message, " - retrying")
            i <<- i + 1
        })
    }

    row %>%
        dplyr::select(type, package,n_traced_functions, n_traces, run_status, run_time) %>%
        knitr::kable() %>%
        print()

    message("\n\n Tracing of ", package, " ", type, " finished in ", time["elapsed"])
}, error=function(e) {
    message("Tracing of ", package, " ", type, " failed with: ", e$message)
    stop(e$message)
})
