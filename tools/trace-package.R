#!/usr/bin/env Rscript

options(error=function() { traceback(2); if (!interactive()) quit("no", status=1, runLast=FALSE) })
options(genthat.debug=TRUE)

suppressPackageStartupMessages(library("optparse"))
tryCatch(library(genthat), error=function(e) devtools::load_all())
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

trace_package <- function(pkg, type, traces_dir, batch_size) {
    timestamp <- Sys.time()

    code <- substitute(
        time <- system.time(
            ret <- genthat::run_package(PACKAGE, types=TYPE, quiet=FALSE)
        ),
        list(PACKAGE=pkg$package, TYPE=type)
    )

    run <- genthat::capture(trace_result <- genthat::trace_package(pkg$path, code), split=TRUE)
    out <- c(run$stdout, run$stderr, trace_result$result$output)
    traces <- trace_result$traces
    replacements <- trace_result$replacements
    dname <- file.path(traces_dir, type)
    stopifnot(dir.exists(dname) || dir.create(dname))

    if (length(traces) > 0) {
        batches <- ceiling(length(traces) / batch_size)
        for (i in 1:batches) {
            lower <- (i - 1) * batch_size + 1
            upper <- min(length(traces), lower + batch_size)
            batch <- traces[lower:upper]

            fname <- file.path(dname, paste0(strftime(timestamp, "%Y-%m-%d-%H%M"), "-", i, ".RDS"))

            message("TRACE: [", i,"/", batches, "] saving traces to: ", fname)
            saveRDS(batch, fname)
        }
    }

    data_frame(
        timestamp=timestamp,
        type=type,
        package=pkg$package,
        version=pkg$version,
        replacements_size=length(trace_result$replacements),
        replacements=paste(trace_result$replacements, collapse="\n"),
        traces_size=length(traces),
        traces_file=dname,
        driver_status=trace_result$result$status,
        driver_output=paste(out, collapse="\n"),
        driver_time=trace_result$result$image$time["elapsed"],
        run_status=trace_result$result$image$ret[[type]]$status,
        output=paste(trace_result$result$image$ret[[type]]$output, collapse="\n"),
        fail=paste(trace_result$result$image$ret[[type]]$fail, collapse="\n")
    )
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
        make_option("--batch-size", type="integer", help="Batch size", default=500, metavar="NUM"),
        make_option("--output", type="character", help="Name of the output directory for traces", metavar="PATH")
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

pkg_dir <- opt$package
type <- match.arg(opt$type, c("examples", "tests", "vignettes"), several.ok=FALSE)
traces_dir <- opt$output
batch_size <- opt$`batch-size`

stopifnot(dir.exists(traces_dir))
stopifnot(batch_size > 0)

db <-
    src_mysql(
        dbname=opt$`db-name`,
        host=opt$`db-host`,
        port=opt$`db-port`,
        user=opt$`db-user`,
        password=opt$`db-password`
    )
message("TRACE: Connected to ", format(db))

if (!dir.exists(pkg_dir)) {
    # is this a package name instead?
    pkg_dir <- find.package(pkg_dir)
}

pkg <- devtools::as.package(pkg_dir)
traces_dir <- file.path(traces_dir, pkg$package)

stopifnot(dir.exists(traces_dir) || dir.create(traces_dir))

tryCatch({
    message("TRACE: ", pkg$package, " (type: ", type, ", batch_size: ", batch_size, ")")
    time <- system.time(df <- trace_package(pkg, type, traces_dir, batch_size))

    i <- 0
    while(i < 3) {
        tryCatch({
            db_insert_into(con=db$con, table="traces", values=df)
            break()
        }, error=function(e) {
            message("Storing to DB did not work: ", e$message, " - retrying")
            i <- i + 1
        })
    }

    message("TRACE: ", pkg$package, " finished in ", time["elapsed"])
}, error=function(e) {
    message("TRACE: ", pkg$package, " error while processing: ", e$message)
    stop(e)
})
