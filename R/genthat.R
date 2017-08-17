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
# TODO: gen_from_package

#' @title Generate test cases for a package
#'
#' @description Decorates all functions in a package and then generates test cases based on
#' the code contained in the package examples, vignettes and tests.
#' @export
#'
gen_from_package <- function(pkg, types=c("examples", "tests", "vignettes"),
                            output_dir=".",
                            working_dir=tempfile(pattern="gen_from_package-"),
                            batch_size=0,
                            quiet=TRUE,
                            lib_paths=NULL) {

    stopifnot(is.character(pkg) && length(pkg) == 1)
    stopifnot(length(output_dir) == 1)
    stopifnot(dir.exists(output_dir) || dir.create(output_dir))
    stopifnot(length(working_dir) == 1)
    stopifnot(dir.exists(working_dir) || dir.create(working_dir))

    output_dir <- normalizePath(output_dir, mustWork=TRUE)
    working_dir <- normalizePath(working_dir, mustWork=TRUE)
    pkg_dir <- find.package(pkg, lib_paths)
    stats_file <- file.path(working_dir, "genthat-exports.csv")

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

        time <- system.time(
            ret <- run_r_script(fname, site_file=site_file, quiet=quiet, lib_paths=lib_paths)
        )

        assign(tag, time["elapsed"], envir=stopwatch)

        ret
    }

    runs <- lapply(types, function(type) {
        run <- run_package(pkg, pkg_dir, type, working_dir, quiet=quiet, runner)
        run <- run[[type]]

        df <- if (all(is.na(run))) {
            # nothing has been run we need to return an empty frame
            data.frame(tag=NA, filename=NA, n_traces=NA, status=NA, running_time=NA, row.names=NULL, stringsAsFactors=FALSE)
        } else {
            traces <- if (file.exists(stats_file)) {
                read_stats_file(stats_file)
            } else {
                # this is a rare case, in which something has been run, but no
                # traces were generated
                data.frame(tag=NA, filename=NA, n_traces=0, row.names=NULL, stringsAsFactors=FALSE)
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

    do.call(rbind, runs)
}

#' @export
#'
export_traces <- function(traces, output_dir,
                         tag=NA,
                         stats_file=NULL,
                         batch_size=0) {
    saveRDS(list(sys.calls(), traces, as.list(match.call())), file.path("/tmp", basename(tempfile(pattern="export_traces", fileext=".RDS"))))
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
                genthat_trace_error=2,
                genthat_trace_entry=3)
        })

        n_complete <- length(trace_classes[trace_classes == 1])
        n_error <- length(trace_classes[trace_classes == 2])
        n_entry <- length(trace_classes[trace_classes == 3])

        stats <- data.frame(
            tag=tag,
            filename=paste(fnames, collapse="\n"),
            n_traces=n_traces,
            n_complete=n_complete,
            n_error=n_error,
            n_entry=n_entry,
            stringsAsFactors=FALSE,
            row.names=NULL
        )

        write.table(
            stats,
            file=stats_file,
            row.names=FALSE,
            col.names=FALSE,
            append=TRUE,
            qmethod="double",
            sep=","
        )
    }

    fnames
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
                                    default_decorate_method=getOption("genthat.default_decorate_method", "trycatch"),
                                    stats_file=NULL,
                                    batch_size=0) {

    stopifnot(is.character(pkgs) && length(pkgs) > 0)

    paste(c(
        '## genthat tracing preamble',
        paste0('options(genthat.debug=', debug, ')'),
        paste0('options(genthat.default_decorate_method="', default_decorate_method, '")'),
        '',
        sapply(pkgs, function(x) paste0('genthat::decorate_environment("', x, '")')),
        '',
        paste0('if (genthat::is_debug_enabled()) message("Decorator: ", "', default_decorate_method, '", genthat::get_decorator())'),
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
        col.names=c("tag", "filename", "n_traces", "n_complete", "n_error", "n_entry")
    )
}

run_integration_tests <- function() {
    withr::with_options(list(genthat.run_itests=T), devtools::test(filter="integration"))
}
