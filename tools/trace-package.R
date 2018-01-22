#!/usr/bin/env Rscript

options(error=function() { traceback(3); quit(status=1, save="no") })

suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(genthat))
suppressPackageStartupMessages(library(pbapply))
suppressPackageStartupMessages(library(readr))

pboptions(type="none")

genthat_version <- devtools::as.package(find.package("genthat"))$version

revdep_option_list <- list(
    make_option("--package", type="character", help="Package to trace", metavar="PATH"),
    make_option("--dep", type="character", help="Package to run", metavar="PATH"),
    make_option("--output", type="character", help="Name of the output directory for results", default=tempfile(file="genthat-tests"), metavar="PATH"),
    make_option(c("-q", "--quiet"), help="Quiet output", action="store_true", default=FALSE)
)

coverage_option_list <- list(
    make_option("--package", type="character", help="Package to trace", metavar="PATH"),
    make_option("--types", type="character", help="Type of code to run (exeamples, tests, vignettes)", metavar="TYPE"),
    make_option("--output", type="character", help="Name of the output directory for results", default=tempfile(file="genthat-tests"), metavar="PATH"),
    make_option(c("-q", "--quiet"), help="Quiet output", action="store_true", default=FALSE)
)

trace_option_list <- list(
    make_option("--package", type="character", help="Package to trace", metavar="PATH"),
    make_option("--types", type="character", help="Type of code to run (exeamples, tests, vignettes)", metavar="TYPE"),
    make_option("--output", type="character", help="Name of the output directory for traces", default=tempfile(file="genthat-traces"), metavar="PATH"),
    make_option("--config", type="character", help="decorator+tracer", metavar="CONFIG", default="onexit+set"),
    make_option("--action", type="character", help="action", metavar="ACTION", default="generate"),
    make_option("--prune-tests", help="Prune tests", action="store_true", default=FALSE),
    make_option(c("-d", "--debug"), help="Debug output", action="store_true", default=FALSE),
    make_option(c("-q", "--quiet"), help="Quiet output", action="store_true", default=FALSE)
)

log_debug <- function(...) {
    msg <- paste0(...)
    cat(msg, "\n")
}

revdep_task <- function(package, dep, output, quiet) {
    stopifnot(dir.exists(output) || dir.create(output, recursive=TRUE))

    working_dir <- file.path(output, "tmp")
    stopifnot(dir.exists(working_dir) || dir.create(working_dir, recursive=TRUE))

    options(genthat.debug=T)

    res <- genthat::gen_from_package(
        package,
        dep,
        types="all",
        action="generate",
        output_dir=output,
        decorator="on.exit",
        tracer="set",
        working_dir=working_dir,
        prune_tests=T,
        quiet=quiet
    )

    saveRDS(res, file.path(output, paste0(package, "-", dep, ".RDS")))
    print(attr(res, "stats"))
}

coverage_task <- function(package, types, output, quiet) {
    stopifnot(dir.exists(output) || dir.create(output, recursive=TRUE))
    res <- covr::package_coverage(path=file.path("~/R/CRAN", package), type=types, quiet=quiet)
    saveRDS(res, file.path(output, "covr.RDS"))
}

trace_task <- function(package, config, types, output, action, prune_tests, max_trace_size, debug, quiet) {
    stopifnot(dir.exists(output) || dir.create(output, recursive=TRUE))

    types <- match.arg(types, c("examples", "tests", "vignettes", "all"), several.ok=FALSE)

    config <- str_split_fixed(config, "--", n=2)
    stopifnot(nrow(config) == 1)
    decorator <- config[, 1]
    tracer <- config[, 2]

    working_dir <- file.path(output, "tmp")
    stopifnot(dir.exists(working_dir) || dir.create(working_dir, recursive=TRUE))

    if (decorator == "none") {
        decorator <- NULL
    }

    if (length(tracer) == 0 || nchar(tracer) == 0) {
        tracer <- "set"
    }

    # TODO: move to the main
    options(genthat.debug=debug)
    options(genthat.max_trace_size=as.integer(max_trace_size))

    res <- genthat::gen_from_package(
        package,
        types=types,
        output_dir=output,
        action=action,
        decorator=decorator,
        tracer=tracer,
        working_dir=working_dir,
        prune_tests=prune_tests,
        quiet=quiet
    )

    readr::write_csv(res, file.path(output, "genthat-tracing.csv"))
    readr::write_csv(attr(res, "errors"), file.path(output, "genthat-tracing-errors.csv"))
    readr::write_lines(attr(res, "stats"), file.path(output, "genthat-tracing-stats.txt"))
}

main <- function(args) {
    # TODO handle help option
    # TODO handle debug option

    task_name <- match.arg(args[1], c("coverage", "generate", "revdep", "trace"), several.ok=FALSE)

    task_fun <- get(str_c(task_name, "_task"))
    option_list <- get(str_c(task_name, "_option_list"))

    parser <- OptionParser(option_list=option_list)
    options <- {
        tryCatch({
            parse_args(parser, args=args[-1])
        }, error=function(e) {
            log_debug("Error: ", e$message)
            print_help(parser)
            quit(save="no", status=1)
        })
    }

    names(options) <- str_replace_all(names(options), "-", "_")
    options[["help"]] <- NULL

    log_debug("Running: ", task_name, " [", str_c(names(options), options, sep="=", collapse=", "), "]")

    time <- system.time(
        do.call(task_fun, options)
    )
    time <- time["elapsed"]

    log_debug("\nFinished in ", time)
}

main(commandArgs(trailingOnly=TRUE))
