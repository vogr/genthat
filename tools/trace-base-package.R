#!/usr/bin/env Rscript
library(methods)

# the package to be run
# it has to be installed, ideally with:
# INSTALL_opts=c('--example', '--install-tests', '--with-keep.source', '--no-multiarch')
pkg <- commandArgs(trailingOnly=TRUE)[1]
# this will kill the process if the package does not exist
invisible(find.package(pkg))

# the package to be decorated
pkg_decorate <- "base"
# what to run (examples/tests/vignettes/all)
type <- "all"
# where to store the output
output_dir <- file.path(getwd(), pkg)
stopifnot(dir.exists(output_dir) || dir.create(output_dir))
# how many traces per file
batch_size <- 1000L
quiet <- FALSE


get_dims <- function(x) {
    t <- typeof(x)

    if (t %in% c("integer", "double", "character", "logical", "complex")) {
        paste0("[", length(x), "]")
    } else {
        ""
    }
}

record_types <- function(name, pkg=NULL, args, retv, error,
                        env=parent.frame(), tracer=genthat:::get_tracer()) {
    types <- lapply(args, typeof)
    dims <- lapply(args, get_dims)
    res <- paste0(types, dims)

    if (!missing(retv)) {
        retv <- paste0(typeof(retv), get_dims(retv))
    }

    trace <- genthat:::create_trace(name, pkg, args=res, retv=retv)

    genthat:::store_trace(tracer, trace)
}

save_traces <- function(tracer=genthat::get_tracer()) {
    .Internal(options(genthat.tracing=FALSE))
    on.exit(.Internal(options(genthat.tracing=TRUE)))

    traces <- genthat::copy_traces(tracer)

    genthat::export_traces(traces, output_dir, batch_size=batch_size)
    genthat::reset_traces(tracer)
}

runner <- function(fname, quiet) {
    tryCatch({
        source(fname)
        0L
    }, error=function(x) {
        message("Error sourcing ", fname, ": ", x$message)
        1L
    }, finally={
        save_traces()
    })
}

options(error=function() {traceback(3); if(!interactive()) quit(status=1, save='no')})
options(genthat.debug=!quiet)

# from here everything will be slow
genthat::decorate_environment("base", record_fun=substitute(record_types))
message("Decorated: ", length(genthat::get_decorator()$decorations))

genthat::run_package(pkg=pkg, type=type, quiet=quiet, runner=runner)
