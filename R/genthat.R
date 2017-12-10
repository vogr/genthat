#' genthat: A framework for unit tests generation
#'
#' @docType package
#' @name genthat
#' @useDynLib genthat
#' @importFrom Rcpp evalCpp
NULL

# TODO: sync API with covr?
# TODO: gen_from_function
# TODO: gen_from_code
# TODO: gen_from_source
# TODO: trace_package

#' @export
#'
trace_from_source_package <- function(path, quiet=TRUE, ...) {
    package <- devtools::as.package(path)

    withr::with_temp_libpaths({
        utils::install.packages(
            path,
            repos=NULL,
            lib=.libPaths()[1],
            type="source",
            INSTALL_opts = c(
                "--example",
                "--install-tests",
                "--with-keep.source",
                "--no-multiarch"
            ),
            quiet=quiet
        )

        trace_package(package$package, lib_paths=.libPaths()[1], quiet=quiet, ...)
    })
}

#' @title Decorate all functions from given package and runs package code
#'
#' @description Decorates all functions in a package and then generates test cases based on
#' the code contained in the package examples, vignettes and tests.
#'
#' @param output_dir the name of the directory where to output traces or NULL if traces should not be saved
#'
#' @export
#'
# TODO: update the signature to make it more flexible
# TODO: update the documentation
# TODO: sync names usage pkg ~ package
trace_package <- function(pkg, types=c("examples", "tests", "vignettes", "all"),
                          output_dir=".", filter=NULL, working_dir=tempfile(pattern="genthat-trace-"),
                          decorator="onexit", tracer="set",
                          quiet=TRUE, lib_paths=NULL) {

    stopifnot(is.character(pkg) && length(pkg) == 1)

    if ("all" %in% types) {
        types <- c("examples", "tests", "vignettes")
    }
    types <- match.arg(arg=types, choices=c("examples", "tests", "vignettes"), several.ok=TRUE)

    if (!is.null(output_dir)) {
        stopifnot(is_chr_scalar(output_dir))
        stopifnot(dir.exists(output_dir) || dir.create(output_dir, recursive=TRUE))
        output_dir <- normalizePath(output_dir, mustWork=TRUE)
    }

    stopifnot(is_chr_scalar(working_dir))
    stopifnot(dir.exists(working_dir) || dir.create(working_dir))
    working_dir <- normalizePath(working_dir, mustWork=TRUE)

    # TODO: check tracer
    # TODO: check decorator

    pkg_dir <- find.package(pkg, lib_paths)

    # used for export traces during run
    stats_file <- tempfile("genthat-traces", tmpdir=working_dir, fileext=".csv")
    set_tracer_session_file <- tempfile("set-tracer-session", tmpdir=working_dir, fileext=".RDS")

    runner <- function(fname, quiet) {
        # e.g. examples/my-fun.Rd
        tag <- file.path(basename(dirname(fname)), basename(fname))

        site_file <- if (!is.null(decorator)) {
            decorator_code <- paste0('genthat::create_decorator("', decorator, '")')
            tracer_code <-
                if (tracer == "set") {
                    paste0('genthat::create_tracer("set", session_file="', set_tracer_session_file, '")')
                } else {
                    paste0('genthat::create_tracer("', tracer, '")')
                }

            create_tracing_site_file(
                packages=pkg,
                output_dir=output_dir,
                tag=tag,
                decorator_code=decorator_code,
                tracer_code=tracer_code,
                discard_traces=discard_traces,
                stats_file=stats_file
            )
        } else {
            NULL
        }

        run_r_script(fname, site_file=site_file, quiet=quiet, lib_paths=lib_paths)
    }

    runs <- lapply(types, function(type) {
        run <- run_package(
            pkg,
            pkg_dir=pkg_dir,
            types=type,
            filter=filter,
            working_dir=working_dir,
            quiet=quiet,
            runner=runner
        )
        run <- run[[type]]

        df <- if (is.null(run) || all(is.na(run))) {
            # nothing has been run we need to return an empty frame
            data.frame(
                tag=NA,
                trace=NA,
                type=NA,
                error=NA,
                row.names=NULL,
                stringsAsFactors=FALSE
            )
        } else {
            traces <- if (file.exists(stats_file)) {
                st <- read_stats_file(stats_file)

                # we do not need it anymore and it could create conflicts
                # if examples / tests / vignettes shares the same tags
                file.remove(stats_file)

                st
            } else {
                # Running either failed or there were no calls to the traced
                # functions (e.g. a data file) so the finalizer exporting the
                # traces did not kicked in. based on the status we can figure
                # out what has happened.
                data.frame(
                    tag=tools::file_path_sans_ext(names(run)),
                    trace=NA,
                    type=NA,
                    error=NA,
                    row.names=NULL,
                    stringsAsFactors=FALSE
                )
            }

            traces
        }

        cbind(data.frame(package=pkg, stringsAsFactors=FALSE, row.names=NULL), df)
    })

    if (file.exists(set_tracer_session_file)) {
        file.remove(set_tracer_session_file)
    }

    do.call(rbind, runs)
}

#' @export
#'
export_traces <- function(traces, output_dir, stats_file, tag=NA) {
    stopifnot(is.list(traces))
    stopifnot(is.null(output_dir) || is_chr_scalar(output_dir))
    stopifnot(is_chr_scalar(stats_file))
    stopifnot(is.na(tag) || is_chr_scalar(tag))

    n_traces <- length(traces)
    if (n_traces == 0) {
        return(invisible(character()))
    } else {
        if (is.null(output_dir)) {
            log_debug("Processing ", n_traces, " traces")
        } else {
            log_debug("Saving ", n_traces, " traces into ", output_dir)
        }
    }

    sub <- as.character(10:99)

    save_trace <- function(trace) {
        fname <- if (!is.null(output_dir)) {
            # ../00/00/...RDS
            dname <- file.path(output_dir, sample(sub, 1), sample(sub, 1))
            stopifnot(dir.exists(dname) || dir.create(dname, recursive=TRUE))

            fname <- paste0(trace$fun, ".RDS")
            fname <- next_file_in_row(file.path(dname, fname))

            saveRDS(trace, fname)

            fname
        } else {
            NA
        }

        type <- switch(
            class(trace),
            genthat_trace="C",
            genthat_trace_entry="I", # I == incomplete
            genthat_trace_error="E",
            genthat_trace_failure="F"
        )

        error <- if (type == "F") {
            trimws(trace$failure$message, which="both")
        } else {
            NA
        }

        c(tag, fname, type, error)
    }

    stats <- lapply(traces, save_trace)
    stats <- matrix(unlist(stats, use.names=FALSE, recursive=FALSE), ncol=4, byrow=TRUE)
    colnames(stats) <- c("tag", "trace", "type", "error")

    stats_file_exists <- file.exists(stats_file)

    write.table(
        stats,
        file=stats_file,
        row.names=FALSE,
        col.names=!stats_file_exists,
        append=stats_file_exists,
        qmethod="double",
        sep=","
    )

    stats
}

#' @export
#'
import_traces <- function(filenames) {
    stopifnot(all(file.exists(filenames)))

    lapply(filenames, readRDS)
}

create_tracing_site_file <- function(...) {
    site_file_code <- create_tracing_preable(...)
    site_file <- tempfile()

    cat(site_file_code, file=site_file)

    site_file
}

create_tracing_preable <- function(packages, output_dir, tag="",
                                   decorator_code, tracer_code,
                                   debug=getOption("genthat.debug", FALSE),
                                   discard_traces=FALSE,
                                   stats_file) {

    stopifnot(is.character(packages) && length(packages) > 0)
    stopifnot(is.null(output_dir) || is_chr_scalar(output_dir))
    stopifnot(is_chr_scalar(decorator_code))
    stopifnot(is_chr_scalar(tracer_code))
    stopifnot(is_chr_scalar(stats_file))

    log_file <- file.path(
        output_dir,
        paste0("genthat-", strftime(Sys.time(), "%Y-%m-%d-%H%M%S"), ".log")
    )

    paste(c(
        '## genthat tracing preamble',
        'options(error=function() { traceback(3); quit(status=1, save="no") })',
        paste0('options(genthat.debug=', debug, ')'),
        paste0('options(genthat.log_file="', log_file, '")'),
        '',
        # TODO: make invisible and only print details when debug is on
        paste0('genthat::set_decorator(', decorator_code, ')'),
        paste0('genthat::set_tracer(', tracer_code, ')'),

        sapply(packages, function(x) paste0('genthat::decorate_environment("', x, '")')),
        '',
        # TODO: why is this needed?
        'library(methods)',
        '',
        # the finalizer will trigger exporting traces
        'reg.finalizer(loadNamespace("genthat"), onexit=TRUE, function(x) {',
        paste0(
            '  genthat::export_traces(',
            '    traces=genthat::copy_traces(genthat::get_tracer()), ',
            '    output_dir=', ifelse(is.null(output_dir), 'NULL', paste0('"', output_dir, '"')), ', ',
            '    tag="', tag, '", ',
            '    stats_file="', stats_file, '"',
            ')'
        ),
        '})',
        '',
        ''
    ), collapse="\n")
}

#' @export
#'
format.genthat_traces <- function(x, ...) {
    format(as.data.frame(x))
}

#' @export
#'
print.genthat_traces <- function(x, ...) {
    print(as.data.frame(x))
}

# TODO: we should have this state in C so it can be accessed in a unified way
#regardless if base library is decorated or not
#
#' @export
#'
enable_tracing <- function() {
    options(genthat.tracing=TRUE)
}

#' @export
#'
disable_tracing <- function() {
    options(genthat.tracing=FALSE)
}

#' @export
#'
is_tracing_enabled <- function() {
    isTRUE(getOption("genthat.tracing"))
}

#' @export
#'
is_debug_enabled <- function() {
    isTRUE(getOption("genthat.debug"))
}

#' @export
#'
read_stats_file <- function(fname) {
    read.csv(
        fname,
        stringsAsFactors=FALSE
    )
}

run_integration_tests <- function() {
    withr::with_options(list(genthat.run_itests=T), devtools::test(filter="integration"))
}
