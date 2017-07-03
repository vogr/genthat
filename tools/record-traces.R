#!/usr/bin/env Rscript

options(error=function() { traceback(2); if (!interactive()) quit("no", status=1, runLast=FALSE) })
options(genthat.show_progress=TRUE)
options(genthat.debug=TRUE)

library(devtools)
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

devtools::load_all()

trace_one <- function(pkg_dir) {
    pkg <- devtools::as.package(pkg_dir)

    types <- c("examples", "tests", "vignettes")
    all_traces <- lapply(types, function(x) {
        code <- substitute(
            time <- system.time(ret <- genthat::run_package(PACKAGE, types=X)),
            list(PACKAGE=pkg$package, X=x)
        )

        out <- capture.output(ret <- genthat::trace_package(pkg_dir, code), type="message")
        ret$result$out <- c(out, ret$result$out)
        ret
    })

    dfs <- lapply(zip(name=types, value=all_traces), function(x) {
        value=x$value
        traces_str <- serialize(value$traces, NULL)

        data_frame(
            type=x$name,
            package=pkg$package,
            version=pkg$version,
            replacements_size=length(value$replacements),
            replacements=paste(value$replacements, collapse="\n"),
            traces_size=length(value$traces),
            traces=I(list(traces_str)),
            driver_status=value$result$status,
            driver_output=paste(value$result$out, collapse="\n"),
            driver_time=value$result$image$time["elapsed"],
            success=value$result$image$ret[[x$name]]$success,
            output=paste(value$result$image$ret[[x$name]]$output, collapse="\n"),
            fail=paste(value$result$image$ret[[x$name]]$fail, collapse="\n")
        )
    })

    df <- bind_rows(dfs)
    df
}

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
  stop("Usage: <path to package directory>")
}
pkg_dir <- args[1]
stopifnot(dir.exists(pkg_dir))

db <-
    src_mysql(
        db_name="genthat",
        host="ginger.ele.fit.cvut.cz",
        password="genthat",
        port="6612"
    )

tryCatch({
    time <- system.time(df <- trace_one(pkg_dir))
    db_insert_into(con=db$con, table="traces", values=df)
    message("Processed ", pkg, " in ", time["elapsed"])
}, error=function(e) {
    message("Error while processing", pkg, " : ", e$message)
}, finally={
    dbDisconnect(db$con)
})

