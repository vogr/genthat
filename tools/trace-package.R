#!/usr/bin/env Rscript

options(error=function() { traceback(2); if (!interactive()) quit("no", status=1, runLast=FALSE) })
options(genthat.show_progress=TRUE)
options(genthat.debug=TRUE)

library(devtools)
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

trace_package <- function(pkg, traces_dir) {
    types <- c("examples", "tests", "vignettes")
    all_traces <- lapply(types, function(x) {
        code <- substitute(
            time <- system.time(ret <- genthat::run_package(PACKAGE, types=X)),
            list(PACKAGE=pkg$package, X=x)
        )

        out <- capture.output(ret <- genthat::trace_package(pkg$path, code), type="message")
        ret$result$out <- c(out, ret$result$out)
        ret
    })

    timestamp <- Sys.time()

    dfs <- lapply(genthat:::zip(name=types, value=all_traces), function(x) {
        value <- x$value
        type <- x$name
        fname <-
            file.path(
                traces_dir,
                paste0(strftime(timestamp, "%Y-%m-%d-%H%M"), "-", type, ".RDS")
            )

        saveRDS(value$traces, fname)

        data_frame(
            timestamp=timestamp,
            type=type,
            package=pkg$package,
            version=pkg$version,
            replacements_size=length(value$replacements),
            replacements=paste(value$replacements, collapse="\n"),
            traces_size=length(value$traces),
            traces_file=fname,
            driver_status=value$result$status,
            driver_output=paste(value$result$out, collapse="\n"),
            driver_time=value$result$image$time["elapsed"],
            success=value$result$image$ret[[type]]$success,
            output=paste(value$result$image$ret[[type]]$output, collapse="\n"),
            fail=paste(value$result$image$ret[[type]]$fail, collapse="\n")
        )
    })

    df <- bind_rows(dfs)
    df
}

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop("Usage: <path to package directory> <traces_dir>")
}
pkg_dir <- args[1]
traces_dir <- args[2]
stopifnot(dir.exists(pkg_dir))
stopifnot(dir.exists(traces_dir))

db <-
    src_mysql(
        dbname="genthat",
        host="ginger.ele.fit.cvut.cz",
        password="genthat",
        port=6612
    )
    ## src_sqlite("traces.sqlite3", create=!file.exists("traces.sqlite3"))

pkg <- devtools::as.package(pkg_dir)
traces_dir <- file.path(traces_dir, pkg$package)

stopifnot(dir.exists(traces_dir) || dir.create(traces_dir))

tryCatch({
    message("TRACING: ", pkg$package)
    time <- system.time(df <- trace_package(pkg, traces_dir))

    db_insert_into(con=db$con, table="traces", values=df)

    message("TRACING: ", pkg$package, " finished in ", time["elapsed"])
}, error=function(e) {
    message("TRACING: ", pkg$package, " error while processing: ", e$message)
    stop(e)
})
