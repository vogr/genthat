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
  stop("Usage: <path to packages directory>")
}
pkgs_dir <- args[1]
stopifnot(dir.exists(pkgs_dir))

all_pkgs <- list.files(pkgs_dir, pattern="tar\\.gz$", full.names=TRUE)

db_file <- "traces.sqlite3"
db <- src_sqlite(db_file, create=!file.exists(db_file))

pb <- utils::txtProgressBar(min=0, max=length(all_pkgs), initial=0, style=3)
for (pkg in all_pkgs) {
    tmp <- tempfile()

    tryCatch({
        untar(pkg, exdir=tmp)

        pkg_dir <- list.dirs(tmp, recursive=FALSE)
        if (length(pkg_dir) != 1) {
            stop("Wrong package archive: ", pkg)
        }

        df <- trace_one(pkg_dir)

        db_insert_into(con=db$con, table="traces", values=df)
    }, error=function(e) {
        message("Error while processing", pkg, " : ", e$message)
    }, finally={
        unlink(tmp, recursive=TRUE)
    })

    utils::setTxtProgressBar(pb, utils::getTxtProgressBar(pb) + 1)
}
close(pb)
