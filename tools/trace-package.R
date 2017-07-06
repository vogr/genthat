#!/usr/bin/env Rscript

## options(error=function() { traceback(2); if (!interactive()) quit("no", status=1, runLast=FALSE) })
options(error=recover)
options(genthat.show_progress=TRUE)
options(genthat.debug=TRUE)

tryCatch(library(genthat), error=function(e) devtools::load_all())

suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

trace_package <- function(pkg, type, traces_dir, batch_size) {
    timestamp <- Sys.time()

    code <- substitute(
        time <- system.time(
            ret <- genthat::run_package(PACKAGE, types=X)
        ),
        list(PACKAGE=pkg$package, X=type)
    )

    out <- capture.output(run <- genthat::trace_package(pkg$path, code), type="message")
    out <- c(out, run$result$out)
    traces <- run$traces
    replacements <- run$replacements
    dname <- file.path(traces_dir, type)
    stopifnot(dir.exists(dname) || dir.create(dname))

    if (length(traces) > 0) {
        for (i in 1:ceiling(length(traces) / batch_size)) {
            lower <- (i - 1) * batch_size + 1
            upper <- min(length(traces), lower + batch_size)
            batch <- traces[lower:upper]


            fname <- file.path(dname, paste0(strftime(timestamp, "%Y-%m-%d-%H%M"), "-", i, ".RDS"))

            message("TRACE: saving traces to: ", fname)
            saveRDS(batch, fname)
        }
    }

    data_frame(
        timestamp=timestamp,
        type=type,
        package=pkg$package,
        version=pkg$version,
        replacements_size=length(run$replacements),
        replacements=paste(run$replacements, collapse="\n"),
        traces_size=length(traces),
        traces_file=dname,
        driver_status=run$result$status,
        driver_output=paste(out, collapse="\n"),
        driver_time=run$result$image$time["elapsed"],
        success=run$result$image$ret[[type]]$success,
        output=paste(run$result$image$ret[[type]]$output, collapse="\n"),
        fail=paste(run$result$image$ret[[type]]$fail, collapse="\n")
    )
}

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 4) {
  stop("Usage: <path to package directory> <examples|tests|vignettes> <traces_dir> <batch-size>")
}

pkg_dir <- args[1]
type <- match.arg(args[2], c("examples", "tests", "vignettes"), several.ok=FALSE)
traces_dir <- args[3]
batch_size <- as.numeric(args[4])

stopifnot(dir.exists(traces_dir))
stopifnot(batch_size > 0)

db <-
    src_mysql(
        dbname="genthat",
        host="ginger.ele.fit.cvut.cz",
        password="genthat",
        port=6612
    )
message("PROCESS: Connected to ", format(db))

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
            continue()
        })
    }

    message("TRACE: ", pkg$package, " finished in ", time["elapsed"])
}, error=function(e) {
    message("TRACE: ", pkg$package, " error while processing: ", e$message)
    stop(e)
})
