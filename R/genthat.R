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
#' @export
#'
# TODO: update the signature to make it more flexible
# TODO: update the documentation
trace_package <- function(pkg, types=c("examples", "tests", "vignettes", "all"),
                          output_dir=".", working_dir=tempfile(pattern="genthat-trace-"),
                          batch_size=0, quiet=TRUE, lib_paths=NULL) {

    if ("all" %in% types) {
        types <- c("examples", "tests", "vignettes")
    }

    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(length(output_dir) == 1)
    stopifnot(dir.exists(output_dir) || dir.create(output_dir))
    stopifnot(length(working_dir) == 1)
    stopifnot(dir.exists(working_dir) || dir.create(working_dir))

    output_dir <- normalizePath(output_dir, mustWork=TRUE)
    working_dir <- normalizePath(working_dir, mustWork=TRUE)
    pkg_dir <- find.package(pkg, lib_paths)
    stats_file <- file.path(working_dir, "genthat-trace.csv")

    stopwatch <- new.env(parent=emptyenv())

    runner <- function(fname, quiet) {
        tag <- tools::file_path_sans_ext(basename(fname))
        site_file <- genthat_tracing_site_file(
            pkg,
            output_dir,
            tag=tag,
            stats_file=stats_file,
            batch_size=batch_size
        )

        time <- stopwatch(
            ret <- run_r_script(fname, site_file=site_file, quiet=quiet, lib_paths=lib_paths)
        )

        assign(tag, time, envir=stopwatch)

        ret
    }

    runs <- lapply(types, function(type) {
        run <- run_package(pkg, pkg_dir, type, working_dir, quiet=quiet, runner)
        run <- run[[type]]

        # TODO: under which circumstances does this return a list of NA?
        # isn't it always null?
        df <- if (is.null(run) || all(is.na(run))) {
            # nothing has been run we need to return an empty frame
            # TODO: this is not necessary - enough to have just one NA
            data.frame(tag=NA, filename=NA,
                n_traces=NA, n_complete=NA, n_entry=NA, n_error=NA, n_failures=NA,
                status=NA, running_time=NA,
                row.names=NULL, stringsAsFactors=FALSE
            )
        } else {
            traces <- if (file.exists(stats_file)) {
                read_stats_file(stats_file)
            } else {
                # this is a rare case, in which something has been run, but no
                # traces were generated
                # TODO: this is also not necessary - enough to have one NA
                data.frame(tag=NA, filename=NA, n_traces=0, n_complete=NA, n_entry=NA, n_error=NA, n_failures=NA,
                    row.names=NULL, stringsAsFactors=FALSE
                )
            }

            ## extract times
            times_list <- as.list(stopwatch)
            rm(list=ls(stopwatch), envir=stopwatch)

            times <- data.frame(
                tag=names(times_list),
                running_time=as.numeric(times_list),
                stringsAsFactors=FALSE,
                row.names=NULL
            )

            ## extract status
            tags <- tools::file_path_sans_ext(names(run))
            status <- data.frame(tag=tags, status=run, stringsAsFactors=FALSE, row.names=NULL)

            df <- merge(traces, status, by="tag", all.y=T)
            df <- merge(df, times, by="tag", all.y=T)
            df$n_traces[is.na(df$n_traces)] <- 0
            df
        }

        cbind(data.frame(package=pkg, type=type, stringsAsFactors=FALSE, row.names=NULL), df)
    })

    # TODO: use bind rows
    do.call(rbind, runs)
}

#' @export
#'
export_traces <- function(traces, output_dir,
                         tag=NA,
                         stats_file=NULL,
                         batch_size=0) {

    stopifnot(is.list(traces))
    stopifnot(length(output_dir) == 1)
    stopifnot(dir.exists(output_dir) || dir.create(output_dir))
    stopifnot(is.null(stats_file) || (is.character(stats_file) && length(stats_file) == 1))
    stopifnot(is.na(tag) || (is.character(tag) && length(tag) == 1 && nchar(tag) > 0))
    stopifnot(batch_size >= 0)

    n_traces <- length(traces)
    if (n_traces == 0) {
        return(invisible(character()))
    } else {
        message("Saving ", n_traces, " traces into ", output_dir)
    }

    if (batch_size == 0) {
        batch_size <- n_traces
    }

    file_prefix <- if (is.na(tag)) {
        ""
    } else {
        paste0(tag, "-")
    }

    n_batches <- ceiling(n_traces / batch_size)

    # TODO: use next_file_in_row
    n_existing <- length(Sys.glob(path=file.path(output_dir, paste0(file_prefix, "*", ".RDS"))))

    fnames <- sapply(1:n_batches, function(i) {
        lower <- (i - 1) * batch_size + 1
        upper <- min(n_traces, lower + batch_size - 1)

        fname <- file.path(output_dir, paste0(file_prefix, (i + n_existing), ".RDS"))
        batch <- traces[lower:upper]

        message("Saving traces [", i, "/", n_batches, "] to: ", fname)
        saveRDS(batch, fname)

        fname
    }, USE.NAMES=FALSE)

    if (!is.null(stats_file)) {
        trace_classes <- sapply(traces, function(x) {
            switch(class(x),
                genthat_trace=1,
                genthat_trace_entry=2,
                genthat_trace_error=3,
                genthat_trace_failure=4)
        })

        n_complete <- length(trace_classes[trace_classes == 1])
        n_entry <- length(trace_classes[trace_classes == 2])
        n_error <- length(trace_classes[trace_classes == 3])
        n_failures <- length(trace_classes[trace_classes == 4])

        stats <- data.frame(
            tag=tag,
            filename=paste(fnames, collapse="\n"),
            n_traces=n_traces,
            n_complete=n_complete,
            n_entry=n_entry,
            n_error=n_error,
            n_failures=n_failures,
            stringsAsFactors=FALSE,
            row.names=NULL
        )

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
    }

    fnames
}

generate_and_save <- function(traces, output_dir, info_file=file.path(output_dir, "genthat-generate.csv"), quiet=TRUE) {
    if (!quiet) {
        log_debug("Using info file: ", info_file)
        log_debug("Loaded ", length(traces), " traces, generating tests...")
    }

    tests <- generate_tests(traces, quiet=quiet)
    test_files <- save_tests(tests, output_dir)

    info <- dplyr::bind_cols(tests, test_files)
    info <- info %>% dplyr::select(fun, pkg, test_file, error, elapsed)
    readr::write_csv(info, path=info_file, append=file.exists(info_file))

    n_tests <- sum(!is.na(tests$code))

    if (!quiet) {
        log_debug("Generated ", n_tests, " tests")
    }

    # TODO: perhaps in can return something smarter
    n_tests
}


genthat_tracing_site_file <- function(...) {
    site_file_code <- genthat_tracing_preamble(...)
    site_file <- tempfile()

    cat(site_file_code, file=site_file)

    site_file
}

genthat_tracing_preamble <- function(pkgs,
                                    output_dir,
                                    tag="",
                                    debug=getOption("genthat.debug", FALSE),
                                    default_decorate_method=getOption("genthat.default_decorate_method"),
                                    default_tracer=getOption("genthat.default_tracer"),
                                    stats_file=NULL,
                                    batch_size=0) {

    stopifnot(is.character(pkgs) && length(pkgs) > 0)

    paste(c(
        '## genthat tracing preamble',
        'options(error=function() { traceback(3); quit(status=1, save="no") })',
        paste0('options(genthat.debug=', debug, ')'),
        paste0('options(genthat.default_decorate_method="', default_decorate_method, '")'),
        paste0('options(genthat.default_tracer="', default_tracer, '")'),
        paste0('options(genthat.log_file="', file.path(output_dir, paste0("genthat-", strftime(Sys.time(), "%Y-%m-%d-%H%M%S"), ".log")), '")'),
        '',
        sapply(pkgs, function(x) paste0('genthat::decorate_environment("', x, '")')),
        '',
        paste0('if (genthat::is_debug_enabled()) message("Decorator: ", "', default_decorate_method, '", genthat::get_decorator())'),
        '',
        # TODO: why is this needed?
        'library(methods)',
        '',
        'reg.finalizer(loadNamespace("genthat"), onexit=TRUE, function(x) {',
        paste0(
            '  genthat::export_traces(genthat::copy_traces(genthat::get_tracer()), ',
            '"', output_dir, '", ',
            'tag="', tag, '", ',
            'batch_size=', batch_size, ', ',
            'stats_file="', stats_file, '"',
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
        header=FALSE,
        stringsAsFactors=FALSE,
        col.names=c("tag", "filename", "n_traces", "n_complete", "n_entry", "n_error", "n_failures")
    )
}

run_integration_tests <- function() {
    withr::with_options(list(genthat.run_itests=T), devtools::test(filter="integration"))
}
