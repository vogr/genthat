#' genthat: A framework for unit tests generation
#'
#' @docType package
#' @name genthat
#' @useDynLib genthat
#' @import Rcpp
NULL


# TODO: gen_from_package - calls gen_from_source
# TODO: gen_from_function - calls gen_from_source
# TODO: gen_from_code - calls gen_from_source
# TODO: gen_from_source - this shall be the main entry point
# TODO: trace_package - this is the most flexible variant configuring all

#' Extract unit tests for a package
#'
#' This function extracts units tests for a given package on the `path`. By
#' default it runs package examples, tests and vignettes.
#'
#' @param path file path to the package.
#' @param from a character vector indicating which package(s) code shall be used
#'     to extract the code from. Default `NULL` stands for the package in
#'     `path`.
#' @param types which code artifacts to run from the `from` packages,
#'     'examples', 'tests', 'vignettes' or 'all'
#'
#' @examples
#' \dontrun{gen_from_package(".")}
#'
#' # in case the package has been already installed with necessary artifacts
#' \dontrun{gen_from_package(find.package("my-packages"))}
#'
#' @importFrom tibble data_frame
#' @importFrom magrittr %>%
#' @importFrom devtools as.package
#'
#' @export
#'
gen_from_package <- function(path, from=NULL,
                             types=c("examples", "tests", "vignettes", "all"),
                             action=c("stats", "export", "generate"),
                             filter=NULL,
                             prune_tests=TRUE,
                             quiet=TRUE,
                             ...) {

    stopifnot(is_chr_scalar(path))
    stopifnot(dir.exists(path))
    stopifnot(action != "generate" || prune_tests)

    package <- devtools::as.package(path)
    working_dir <- tempfile(pattern="genthat-gen_from_package")

    if (is.null(from)) {
        # the extract_package_code only work on packages that are installed
        # we need to install the package if it has not yet been installed
        files <- extract_package_code(dir=path, types=types, output_dir=working_dir, filter=filter)
        from <- package$package
    } else {
        files <- lapply(from, extract_package_code, types=types, output_dir=working_dir, filter=filter)
    }

    files <- unlist(files, use.names=FALSE)

    if (length(files) == 0) {
        if (is.null(from)) {
            warning("The package does not contain any runnable code in ", paste(types, sep=","))
        } else {
            warning("No runnable code was found, make sure that the `from` packages were installed with `INSTALL_opts=c('--example', '--install-tests')")
        }

        return(data.frame(file=character(), output=character(),  error=character()))
    }

    tracing <- trace_package(
        packages=package$package,
        files=files,
        action=action,
        quiet=quiet,
        ...
    )

    tracing <- lapply(1:length(tracing), function(i) {
        x <- tracing[[i]]

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

    # DF(test, output, error)
    tracing <- do.call(rbind, tracing)
    row.names(tracing) <- NULL

    # this is the raw data.frame from covr::tally_coverage
    # available only for the generate action
    raw_coverage <- NULL
    if (action == "generate") {
        output <- tracing$output[is.na(tracing$error)]
        log_debug("Generated ", length(output), "/", nrow(tracing), " tests")

        if (length(output) > 0) {
            if (prune_tests) {
                output_size <- file.size(output)
                output <- output[order(output_size)]
                runs <- compute_tests_coverage(path, output, quiet=quiet)
                coverage <- runs
                raw_coverage <- attr(runs, "raw_coverage")
                attr(coverage, "errors") <- NULL
                attr(coverage, "elapsed") <- NULL
                elapsed <- attr(runs, "elapsed")

                log_debug("Ran ", sum(!is.na(runs)), "/", length(runs)," tests ", max(coverage, na.rm=TRUE), "% coverage")
            } else {
                runs <- run_generated_test(output, quiet=quiet)
                elapsed <- runs
                attr(elapsed, "errors") <- NULL
                coverage <- NA

                log_debug("Ran ", sum(!is.na(runs)), "/", length(runs), " tests")
            }

            result <- data.frame(
                order=1:length(output),
                output=output,
                coverage=coverage,
                elapsed=elapsed,
                error=attr(runs, "errors"),
                row.names=NULL,
                stringsAsFactors=FALSE
            )

            result <- merge(tracing, result, by="output", all=TRUE)
            result$error <- ifelse(is.na(result$error.x), result$error.y, result$error.x)
            result$error.x <- NULL
            result$error.y <- NULL
        } else {
            runs <- numeric()
            result <- tracing
            result$coverage <- rep(NA, nrow(result))
            result$elapsed <- rep(NA, nrow(result))
        }

        # TODO: try to rewrite using base package
        errors <-
            dplyr::filter(result, !is.na(error)) %>%
            dplyr::select(file, output, error)

        if (prune_tests && length(output) > 0) {
            # select unique tests based on their size
            # TODO: try to rewrite using base package
            result <-
                dplyr::arrange(result, coverage) %>%
                dplyr::group_by(coverage) %>%
                dplyr::top_n(1, -order) %>%
                dplyr::ungroup()
        }

        # TODO: try to rewrite using base package
        result <-
            dplyr::filter(result, is.na(error)) %>%
            dplyr::select(file, output, elapsed, coverage)

        if (!getOption("genthat.keep_failed_tests", FALSE) && length(output) > 0) {
            # remove the duplicated tests and empty test directories
            to_remove <- output[!(output %in% result$output)]
            to_check_dirs <- unique(dirname(to_remove))

            # remove from errors
            errors <- dplyr::mutate(errors, output=ifelse(output %in% to_remove, NA, output))

            if (length(to_remove) > 0) {
                log_debug("Removing ", length(to_remove), " tests")

                to_remove <- lapply(to_remove, function(x) {
                    p <- sub(pattern="\\.R$", replacement=".*", x)
                    Sys.glob(p)
                })

                to_remove <- unlist(to_remove, use.names=FALSE, recursive=FALSE)

                file.remove(to_remove)

                to_remove <- filter(to_check_dirs, function(x) length(list.files(x)) == 0)

                if (length(to_remove) > 0) {
                    log_debug("Removing ", length(to_remove), " empty test directories")

                    file.remove(to_remove)
                }
            }
        }

        attr(result, "errors") <- errors
        attr(result, "stats") <- data_frame(
            all=nrow(tracing),
            generated=length(output),
            ran=sum(!is.na(runs)),
            kept=nrow(result),
            coverage=if (nrow(result) > 0) max(result$coverage) else 0,
            elapsed=sum(result$elapsed)
        )
        attr(result, "raw_coverage") <- raw_coverage

        result
    } else {
        tracing
    }
}

#' @title Decorate all functions from given package and runs package code
#'
#' @description Decorates all functions in a package and then generates test
#'     cases based on the code contained in the package examples, vignettes and
#'     tests.
#'
#' @param packages chr vector of package names whose functions should be traced.
#'     All the packages need to be installed in the `lib_paths`.
#' @param files chr vector of paths to R files that shall be run
#' @param output_dir the name of the directory where to output traces or NULL if
#'     traces should not be saved
#'
#' @importFrom utils read.csv
#'
#' @export
#'
# TODO: this shall call trace_functions
trace_package <- function(packages, files,
                          output_dir=".",
                          decorator="on.exit", tracer="set",
                          action=c("stats", "export", "generate"),
                          working_dir=tempfile(pattern="genthat-trace-"),
                          quiet=TRUE, lib_paths=NULL) {

    stopifnot(is.character(files))
    if (length(files) == 0) {
        return(character())
    }

    if (is.null(names(files))) {
        names(files) <- files
    }

    stopifnot(is.character(packages), length(packages) > 0)
    # check if packages exists in the provided libraries
    find.package(packages, lib_paths)

    # both output_dir and working_dir must be absolute because the future R
    # instances will be run from the dirs where the files are
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
        log_debug("Running ", fname, " ...")

        env <- c()
        site_file <- NULL

        if (length(decorator) == 0 || decorator == "none") {
            decorator <- NULL
        }

        # this is to support a simple run of the files without any genthat involved
        if (!is.null(decorator)) {
            vars <- list()

            vars$debug <- is_debug_enabled()
            vars$keep_all_traces <- getOption("genthat.keep_all_traces", FALSE)
            vars$keep_failed_traces <- getOption("genthat.keep_failed_traces", FALSE)

            vars$decorator <- decorator
            vars$tracer <- tracer
            vars$session_file <- set_tracer_session_file
            vars$action <- action
            vars$current_file <- fname
            vars$output_dir <- output_dir
            vars$stats_file <- stats_file
            vars$max_trace_size <- getOption("genthat.max_trace_size", .Machine$integer.max)
            vars$packages <- paste(packages, sep=",")

            # convert the variables to the expected format GENTHAT_<VAR>=<value>
            env <- sapply(names(vars), function(x) {
                paste(toupper(paste0("genthat_", x)), as.character(vars[[x]]), sep="=")
            }, USE.NAMES=FALSE)

            site_file <- system.file("genthat-tracing-site-file.R", package="genthat")
        }

        if (!file.exists(fname)) {
            paste(fname, "does not exist")
        } else {
            tryCatch({
                run <- run_r_script(fname, site_file=site_file, env=env, quiet=quiet, lib_paths=lib_paths)

                if (file.exists(stats_file)) {
                    # running of the file might have failed, but still some traces
                    # might have been generated

                    # TODO: do we want to indicate somewhere that the running has failed?
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
    }

    log_debug("Running ", length(files), " files")
    lapply(files, run_file)
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

generate_action <- function(trace, output_dir, keep_failed_trace=FALSE) {
    tryCatch({
        testfile <- generate_test_file(trace, output_dir)
        log_debug("Saving test into: ", testfile)
        error <- NA

        if (getOption("genthat.keep_all_traces", FALSE)) {
            tracefile <- save_trace_file(trace, output_dir, basename(testfile))
            log_debug("Saving trace into: ", tracefile)
        }

        c("testfile"=testfile, "error"=error)
    }, error=function(e) {
        testfile <- NA
        error <- trimws(e$message, which="both")

        if (keep_failed_trace || getOption("genthat.keep_all_traces", FALSE)) {
            testfile <- save_trace_file(trace, output_dir, "failed-trace")
            log_debug("Saving failed trace into: ", testfile)
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

#' Process traces based on given action
#' 
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
            keep_failed_traces <- as.logical(getOption("genthat.keep_failed_traces", TRUE))

            action_fun <- function(trace) generate_action(trace, output_dir, keep_failed_traces)
        }

        log_debug("Processing ", length(traces), " traces")
        ret <- lapply(traces, action_fun)
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
