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

gen_from_package <- function(pkgs_to_trace, pkgs_to_run=pkgs_to_trace,
                             types=c("examples", "tests", "vignettes", "all"),
                             filter=NULL,
                             ...) {

    working_dir <- tempfile(pattern="genthat-gen_from_package")
    files <- lapply(pkgs_to_run, extract_package_code, types=types, output_dir=working_dir, filter=filter)
    files <- unlist(files)

    if (length(files) == 0) {
        return(data.frame(file=character(), trace=character(), type=character(), error=character()))
    }

    ret <- trace_package(
        pkgs=pkgs_to_trace,
        files_to_run=files,
        ...
    )

    ret <- lapply(1:length(ret), function(i) {
        x <- ret[[i]]

        if (is.data.frame(x)) {
            x
        } else if (x == 0) {
            # there were no traces
            data.frame(file=files[i], trace=NA, type=NA, error=NA)
        } else if (is.numeric(x) || is.character(x)) {
            error <- if (is.character(x)) {
                paste("Running R file failed:", x)
            } else {
                paste("Running R file exited with:", x)
            }

            data.frame(
                file=files[i],
                trace=NA,
                type="F",
                error=error,
                stringsAsFactors=FALSE
            )
        } else {
            stop("Unknown output from trace_package at index ", i, ": ", x)
        }
    })

    ret <- do.call(rbind, ret)
    row.names(ret) <- NULL
    ret
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
trace_package <- function(pkgs, files_to_run,
                          output_dir=".",
                          decorator="onexit", tracer="set",
                          action=c("export", "generate"),
                          working_dir=tempfile(pattern="genthat-trace-"),
                          quiet=TRUE, lib_paths=NULL) {

    stopifnot(is.character(files_to_run))
    if (length(files_to_run) == 0) {
        return(character())
    }

    if (is.null(names(files_to_run))) {
        names(files_to_run) <- files_to_run
    }

    stopifnot(is.character(pkgs), length(pkgs) > 0)
    # check if packages exists
    find.package(pkgs, lib_paths)

    if (!is.null(output_dir)) {
        stopifnot(is_chr_scalar(output_dir))
        stopifnot(dir.exists(output_dir) || dir.create(output_dir, recursive=TRUE))
        output_dir <- normalizePath(output_dir, mustWork=TRUE)
    }

    stopifnot(is_chr_scalar(working_dir))
    stopifnot(dir.exists(working_dir) || dir.create(working_dir))
    working_dir <- normalizePath(working_dir, mustWork=TRUE)

    action <- match.arg(action, c("export", "generate"), several.ok=FALSE)

    # TODO: check tracer
    # TODO: check decorator

    # used for export traces during run
    stats_file <- tempfile("genthat-tracing", tmpdir=working_dir, fileext=".csv")
    set_tracer_session_file <- tempfile("set-tracer-session", tmpdir=working_dir, fileext=".RDS")
    on.exit({
        if (file.exists(stats_file)) file.remove(stats_file)
        if (file.exists(set_tracer_session_file)) file.remove(set_tracer_session_file)
    })

    run_file <- function(fname) {
        env <- c()
        site_file <- NULL

        # this is to support a simple run of the files without any genthat involved
        if (!is.null(decorator)) {
            vars <- list()

            vars$debug <- is_debug_enabled()
            if (!is.null(output_dir)) {
                vars$log_file <-
                    file.path(output_dir, paste0("genthat-", tools::file_path_sans_ext(basename(fname)), ".log"))
            }
            vars$decorator <- decorator
            vars$tracer <- tracer
            vars$session_file <- set_tracer_session_file
            vars$action <- action
            vars$current_file <- fname
            vars$output_dir <- output_dir
            vars$stats_file <- stats_file
            vars$pkgs <- paste(pkgs, sep=",")

            # convert the variables to the expected format GENTHAT_<VAR>=<value>
            env <- sapply(names(vars), function(x) {
                paste(toupper(paste0("genthat_", x)), as.character(vars[[x]]), sep="=")
            }, USE.NAMES=FALSE)
            site_file <- system.file("genthat-tracing-site-file.R", package="genthat")
        }

        tryCatch({
            run <- run_r_script(fname, site_file=site_file, env=env, quiet=quiet, lib_paths=lib_paths)

            if (run == 0 && file.exists(stats_file)) {
                st <- read_stats_file(stats_file)
                file.remove(stats_file)
                st
            } else {
                # Running either failed or there were no calls to the traced
                # functions (e.g. a data file) so the finalizer exporting the
                # traces did not kicked in. based on the status we can figure
                # out what has happened.
                run
            }
        }, error=function(e) {
            e$message
        })
    }

    pbapply::pblapply(files_to_run, run_file)
}

#' @export
#'
export_traces <- function(traces, file, output_dir, stats_file) {
    stopifnot(is.list(traces))
    stopifnot(is_chr_scalar(file))
    stopifnot(is.null(output_dir) || is_chr_scalar(output_dir))
    stopifnot(is_chr_scalar(stats_file))

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

    save_trace <- function(trace) {
        fname <- if (!is.null(output_dir)) {
            pkg <- if (is.null(trace$pkg)) "_NULL_" else trace$pkg
            fun <- if (is.null(trace$fun)) "_NULL_" else trace$fun

            dname <- file.path(output_dir, pkg, fun)
            stopifnot(dir.exists(dname) || dir.create(dname, recursive=TRUE))

            fname <- next_file_in_row(file.path(dname, "trace.RDS"))
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

        c(file, fname, type, error)
    }

    stats <- lapply(traces, save_trace)
    stats <- matrix(unlist(stats, use.names=FALSE, recursive=FALSE), ncol=4, byrow=TRUE)
    colnames(stats) <- c("file", "trace", "type", "error")

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
