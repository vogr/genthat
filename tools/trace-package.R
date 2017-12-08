#!/usr/bin/env Rscript

options(error=function() { traceback(3); quit(status=1, save="no") })

suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(genthat))
suppressPackageStartupMessages(library(pbapply))

genthat_version <- devtools::as.package(find.package("genthat"))$version

generate_option_list <- list(
    make_option("--traces", type="character", help="Path to RDS trace files", metavar="PATH"),
    make_option("--output", type="character", help="Name of the output directory for tests", default=tempfile(file="genthat-tests"), metavar="PATH"),
    make_option(c("-q", "--quiet"), help="Quiet output", action="store_true", default=FALSE)
)

run_option_list <- list(
    make_option("--tests", type="character", help="Path to generated tests", metavar="PATH"),
    make_option("--output", type="character", help="Name of the output directory for results", default=tempfile(file="genthat-tests"), metavar="PATH"),
    make_option(c("-q", "--quiet"), help="Quiet output", action="store_true", default=FALSE)
)

coverage_option_list <- list(
    make_option("--package", type="character", help="Package to trace", metavar="PATH"),
    make_option("--output", type="character", help="Name of the output directory for results", default=tempfile(file="genthat-tests"), metavar="PATH"),
    make_option(c("-q", "--quiet"), help="Quiet output", action="store_true", default=FALSE)
)

trace_option_list <- list(
    make_option("--package", type="character", help="Package to trace", metavar="PATH"),
    make_option("--type", type="character", help="Type of code to run (exeamples, tests, vignettes)", metavar="TYPE"),
    make_option("--batch-size", type="integer", help="Batch size", default=1000, metavar="NUM"),
    make_option("--output", type="character", help="Name of the output directory for traces", default=tempfile(file="genthat-traces"), metavar="PATH"),
    make_option("--config", type="character", help="decorator+tracer", metavar="CONFIG", default="onexit+set"),
    make_option("--stats-file", type="character", help="Name of the CSV file with stats", metavar="FILE", default="genthat-traces.csv"),
    make_option(c("-d", "--debug"), help="Debug output", action="store_true", default=FALSE),
    make_option(c("-q", "--quiet"), help="Quiet output", action="store_true", default=FALSE)
)

log_debug <- function(...) {
    msg <- paste0(...)
    cat(msg, "\n")
}

generate_task <- function(traces, output, quiet) {
    stopifnot(dir.exists(traces))
    stopifnot(dir.exists(output) || dir.create(output, recursive=TRUE))

    rdss <- list.files(path=traces, pattern="\\.RDS$", full.names=TRUE, recursive=FALSE)

    if (!quiet) {
        log_debug("Found ", length(rdss), " files")
    }

    if (length(rdss) == 0) {
        # in order to generate the genthat-generate.csv file we fake empty traces
        rdss <- tempfile()
        saveRDS(list(), rdss)
    }

    generate <- function(rds) {
        tryCatch({
            traces <- genthat:::import_traces(rds)
            genthat:::generate_and_save(traces, output_dir=output, quiet=quiet)
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


run_task <- function(tests, output, quiet) {
    stopifnot(dir.exists(tests))
    stopifnot(dir.exists(output) || dir.create(output, recursive=TRUE))

    tests <- list.files(path=tests, pattern="test-[0-9]+\\.R$", full.names=TRUE, recursive=TRUE)

    if (!quiet) {
        log_debug("Found ", length(tests), " files")
    }

    run <- function(test) {
        tryCatch({
            genthat::run_generated_test(test, quiet=quiet)
        }, error=function(e) {
            data_frame(file=test, exception=e$message)
        })
    }

    runs <- pblapply(tests, run)
    runs <- dplyr::bind_rows(runs)

    readr::write_csv(runs, file.path(output, "genthat-run-tests.csv"))

    if (!quiet) {
        n <- sum(runs$nb)
        f <- sum(runs$failed)
        log_debug("Run ", n-f, "/", n, " tests")
    }
}

coverage_task <- function(package, output, quiet) {
    stopifnot(dir.exists(output) || dir.create(output, recursive=TRUE))
    res <- covr::package_coverage(path=file.path("~/CRAN/extr", package), type="all", quiet=quiet)
    saveRDS(res, file.path(output, "covr.RDS"))
}

trace_task <- function(package, config, type, output, batch_size, stats_file, debug, quiet) {
    stopifnot(is.integer(batch_size))
    stopifnot(dir.exists(output) || dir.create(output, recursive=TRUE))
    stopifnot(length(stats_file) == 1, nchar(stats_file) > 0)
    # TODO: sync type vs types
    type <- match.arg(type, c("examples", "tests", "vignettes", "all"), several.ok=FALSE)

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

    options(genthat.debug=debug)

    print(str_c(
        "genthat::trace_package('",
        package,
        "', type='",type,
        "', output='",output,
        "', decorator='",decorator,
        "', tracer='",tracer,
        "', working_dir='",working_dir,
        "', quiet=",quiet,
        ", batch_size=",batch_size,
        ")"
    ))
    res <- genthat::trace_package(
        package,
        type=type,
        output=output,
        decorator=decorator,
        tracer=tracer,
        working_dir=working_dir,
        quiet=quiet,
        batch_size=batch_size
    )

    readr::write_csv(res, file.path(output, stats_file))
}

main <- function(args) {
    # TODO handle help option
    # TODO handle debug option

    task_name <- match.arg(args[1], c("coverage", "generate", "run", "trace"), several.ok=FALSE)

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
