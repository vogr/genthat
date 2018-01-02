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

        gen_from_package(package$package, lib_paths=.libPaths()[1], quiet=quiet, ...)
    })
}

#' @export
#'
gen_from_package <- function(pkgs_to_trace, pkgs_to_run=pkgs_to_trace,
                             types=c("examples", "tests", "vignettes", "all"),
                             filter=NULL,
                             ...) {

    working_dir <- tempfile(pattern="genthat-gen_from_package")
    files <- lapply(pkgs_to_run, extract_package_code, types=types, output_dir=working_dir, filter=filter)
    files <- unlist(files)

    if (length(files) == 0) {
        return(data.frame(file=character(), output=character(),  error=character()))
    }

    ret <- trace_package(
        pkgs=pkgs_to_trace,
        files_to_run=files,
        ...
    )

    ret <- lapply(1:length(ret), function(i) {
        x <- ret[[i]]

        if (length(x) == 1 && (is.numeric(x) || is.character(x))) {
            error <- if (is.character(x)) {
                paste("Running R file failed:", x)
            } else {
                paste("Running R file exited with:", x)
            }

            x <- data.frame(
                output=NA,
                error=error,
                stringsAsFactors=FALSE,
                row.names=NULL
            )
        } else if (!is.data.frame(x)) {
            stop("Unknown output from trace_package at index ", i, ": ", x)
        }

        if (nrow(x) > 0) {
            x <- cbind(data.frame(file=files[i], stringsAsFactors=FALSE, row.names=NULL), x)
        } else {
            x <- cbind(data.frame(file=character(), stringsAsFactors=FALSE, row.names=NULL), x)
        }

        x
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
                          decorator="on.exit", tracer="set",
                          action=c("stats", "export", "generate"),
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

    # both output_dir and working_dir must be absolute because the future R
    # instances will be run from the dirs where the files_to_run are
    stopifnot(is_chr_scalar(output_dir))
    stopifnot(dir.exists(output_dir) || dir.create(output_dir, recursive=TRUE))
    output_dir <- normalizePath(output_dir, mustWork=TRUE)

    stopifnot(is_chr_scalar(working_dir))
    stopifnot(dir.exists(working_dir) || dir.create(working_dir))
    working_dir <- normalizePath(working_dir, mustWork=TRUE)

    action <- match.arg(action, c("stats", "export", "generate"), several.ok=FALSE)

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
                st <- read.csv(stats_file, stringsAsFactors=FALSE)
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

save_trace_file <- function(trace, output_dir, name) {
    pkg <- if (is.null(trace$pkg)) "_NULL_" else trace$pkg
    fun <- if (is.null(trace$fun)) "_NULL_" else trace$fun

    dname <- file.path(output_dir, pkg, fun)
    stopifnot(dir.exists(dname) || dir.create(dname, recursive=TRUE))

    fname <- next_file_in_row(file.path(dname, paste0(name, ".RDS")))
    saveRDS(trace, fname)
    fname
}

generate_action <- function(trace, output_dir, save_failed_trace=TRUE) {
    tryCatch({
        testfile <- generate_test_file(trace, output_dir)
        error <- NA

        c("testfile"=testfile, "error"=error)
    }, error=function(e) {
        testfile <- NA
        error <- trimws(e$message, which="both")

        if (save_failed_trace) {
            testfile <- save_trace_file(trace, output_dir, "failed-trace")
        }

        c("testfile"=testfile, "error"=error)
    })

}

export_action <- function(trace, output_dir, save_trace=TRUE) {
    error <- NA
    tracefile <- NA

    if (is(trace, "genthat_trace_failure")) {
        error <- trimws(trace$failure$message, which="both")
    } else if (save_trace) {
        tracefile <- save_trace_file(trace, output_dir, "trace")
    }

    c(tracefile, error)
}

#' @param file that has been running
#' @export
#'
process_traces <- function(traces, output_dir, action) {
    stopifnot(is.list(traces))
    stopifnot(is_chr_scalar(output_dir))

    action <- match.arg(action, c("stats", "export", "generate"), several.ok=FALSE)

    if (length(traces) == 0) {
        log_debug("No traces")

        # it will still be store in the stats file
        ret <- matrix(character(), ncol=2)
    } else {
        if (action %in% c("stats", "export")) {
            save_traces <- action == "export"

            action_fun <- function(trace) export_action(trace, output_dir, save_traces)
        } else {
            save_failed_traces <- as.logical(getOption("genthat.save_failed_traces", TRUE))

            action_fun <- function(trace) generate_action(trace, output_dir, save_failed_traces)
        }

        ret <- pbapply::pblapply(traces, action_fun)
        ret <- matrix(unlist(ret, use.names=FALSE, recursive=FALSE), ncol=2, byrow=TRUE)
    }

    colnames(ret) <- c("output", "error")
    ret
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

run_integration_tests <- function() {
    withr::with_options(list(genthat.run_itests=T), devtools::test(filter="integration"))
}
