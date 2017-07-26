#!/usr/bin/env Rscript

options(error=function() { traceback(2); if (!interactive()) quit("no", status=1, runLast=FALSE) })

suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(RMySQL))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

tryCatch(library(genthat), error=function(e) devtools::load_all())

do_trace_package <- function(package, type, traces_dir, batch_size, clean) {
    timestamp <- Sys.time()

    # We need the package name for the run_package which can only run
    # a package that is installed. The trace_package will take care of that.
    # This way we support all that trace_package supports
    pkg_name <- genthat:::resolve_package_name(package)

    code <- substitute({
        options(genthat.debug=DEBUG)

        # the ret variables will be available in the image
        # after the run is completed
        ret <- genthat::run_package(PKG_NAME, types=TYPE, quiet=FALSE, clean=CLEAN)
        },
        list(
            PKG_NAME=pkg_name,
            TYPE=type,
            CLEAN=clean,
            DEBUG=getOption("genthat.debug")
        )
    )

    run <- genthat::capture(
        trace_result <- genthat::trace_package(
            package, code, traces_dir, batch_size=batch_size, quiet=FALSE, clean=clean
        ),
        split=TRUE
    )

    # the ret variable from the code supplied above
    run_pkg <- trace_result$run$image$ret[[type]]
    log_file <- normalizePath(file.path(traces_dir, "execution.log"), mustWork=TRUE)

    row <-
        data_frame(
            timestamp=timestamp,
            type=type,

            name=pkg_name,
            package=package,

            n_traced_functions=length(trace_result$traced_functions),
            traced_functions=paste(trace_result$traced_functions, collapse="\n"),
            decorating_time=trace_result$decorating_time,
            n_traces=trace_result$n_traces,
            trace_files=paste(trace_result$trace_files, collapse="\n"),
            trace_saving_time=trace_result$saving_time,

            trace_package_output=paste(run$stdout, run$stderr, collapse="\n"),
            run_status=trace_result$run$status,
            run_output=trace_result$run$output,
            run_output_log=log_file,
            run_command=trace_result$run$command,
            run_time=trace_result$run$elapsed,

            run_package_output=run_pkg$output,
            run_package_status=run_pkg$status,
            run_package_time=run_pkg$elapsed
        )

    # TODO: the DB truncates the output anyway
    write(run_pkg$output, log_file)

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
message("TRACE: Connected to ", db_info$dbname, "@", db_info$conType)

tryCatch({
    message("TRACE: Tracing: ", package, " type: ", type, " (batch_size: ", batch_size, ") in ", traces_dir)
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
        dplyr::select(type, name, n_traced_functions, n_traces, run_status, run_time) %>%
        knitr::kable() %>%
        print()

    message("\n\nTRACE: ", package, " finished in ", time["elapsed"])
}, error=function(e) {
    message("TRACE: ", package, " error while processing: ", e$message)
    stop(e$message)
})
