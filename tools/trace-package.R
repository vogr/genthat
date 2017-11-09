#!/usr/bin/env Rscript

options(error=function() { traceback(3); quit(status=1, save="no") })

suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(genthat))
suppressPackageStartupMessages(library(pbapply))

genthat_version <- devtools::as.package(find.package("genthat"))$version

generate_option_list <- list(
    make_option("--path", type="character", help="Path to trace files", metavar="PATH"),
    make_option(c("-q", "--quiet"), help="Quiet output", action="store_true", default=FALSE)
)

run_option_list <- list(
    make_option("--package", type="character", help="Package to trace", metavar="PATH"),
    make_option("--type", type="character", help="Type of code to run (exeamples, tests, vignettes)", metavar="TYPE"),
    make_option("--output", type="character", help="Name of the output directory for traces", default=tempfile(file="trace-package"), metavar="PATH"),
    make_option(c("-q", "--quiet"), help="Quiet output", action="store_true", default=FALSE)
)

trace_option_list <- list(
    make_option("--package", type="character", help="Package to trace", metavar="PATH"),
    make_option("--type", type="character", help="Type of code to run (exeamples, tests, vignettes)", metavar="TYPE"),
    make_option("--batch-size", type="integer", help="Batch size", default=1000, metavar="NUM"),
    make_option("--output", type="character", help="Name of the output directory for traces", default=tempfile(file="trace-package"), metavar="PATH"),
    make_option("--decorator", type="character", help="Decorator (onentry/onexit/onboth/trycatch)", metavar="DECORATOR", default="onexit"),
    make_option("--tracer", type="character", help="Tracer (sequence/set)", metavar="TRACER", default="set"),
    make_option(c("-d", "--debug"), help="Debug output", action="store_true", default=FALSE),
    make_option(c("-q", "--quiet"), help="Quiet output", action="store_true", default=FALSE)
)

log_debug <- function(...) {
    msg <- paste0(...)
    cat(msg, "\n")
}

generate_task <- function(path, quiet) {
    rdss <- list.files(path=path, pattern="\\.RDS$", full.names=TRUE, recursive=FALSE)

    if (!quiet) {
        log_debug("Found ", length(rdss), " files")
    }

    generate <- function(rds) {
        tryCatch({
            traces <- genthat:::import_traces(rds)
            genthat:::generate_and_save(traces, output_dir=file.path(path, "generated-tests"), quiet=quiet)
        }, error=function(e) {
            log_debug("Unable to generate tests for traces from ", rds)
            print(e)
            0
        })
    }

    n_tests <- pblapply(rdss, generate)

    if (!quiet) {
        log_debug("Generated in total ", sum(as.integer(n_tests), na.rm=TRUE), " tests")
    }
}

run_task <- function(package, type, output, quiet) {
    # TODO: output stats
    stopifnot(dir.exists(output) || dir.create(output))
    type <- match.arg(type, c("examples", "tests", "vignettes", "all"), several.ok=FALSE)

    runner <- function(fname, quiet) {
        tag <- tools::file_path_sans_ext(basename(fname))
        genthat:::run_r_script(fname, quiet=quiet)
    }

    genthat::run_package(package, types=type, quiet=quiet, runner=runner)
}

trace_task <- function(package, decorator, tracer, type, output, batch_size, debug, quiet) {
    stopifnot(batch_size > 0)
    stopifnot(dir.exists(output) || dir.create(output))
    type <- match.arg(type, c("examples", "tests", "vignettes", "all"), several.ok=FALSE)

    options(genthat.default_decorate_method=decorator)
    options(genthat.default_tracer=tracer)
    options(genthat.debug=debug)

    genthat::trace_package(
        package,
        type,
        output,
        working_dir=output,
        quiet=quiet,
        batch_size=batch_size
    )
}

main <- function(args) {
    task_name <- match.arg(args[1], c("run", "trace", "generate"), several.ok=FALSE)

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
