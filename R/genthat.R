#' genthat: A framework for unit tests generation
#'
#' @docType package
#' @name genthat
#' @useDynLib genthat
#' @import Rcpp
NULL

#' Extract unit tests for a package
#'
#' This function extracts units tests for a given package on the `path`. By
#' default it runs package examples, tests and vignettes.
#'
#' @param path file path to the package.
#' @param from a character vector of package names, from which code shall be
#'     used to extract the code from. Default \code{NULL} stands for the package
#'     in `path`. For non-\code{NULL} values, the packages shall be installed
#'     with the appropriate options, i.e. \code{INSTALL_opts('--example',
#'     '--install-tests)} in order to contain the required code. For
#'     \code{NULL}, the package will be installed with the appropriate options
#'     based on the \code{types}.
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
gen_from_package <- function(path, from=NULL, types=c("examples", "tests", "vignettes", "all"), filter=NULL, ...) {
    stopifnot(is_chr_scalar(path) && dir.exists(path))

    path <- normalizePath(path, mustWork=TRUE)
    package <- as.package(path)$package
    install_path <- tryCatch(find.package(package), error=function(e) NULL)

    temp_dir <- tempfile(pattern="genthat-gen_from_package")
    on.exit(unlink(temp_dir, recursive=TRUE))

    phase2 <- function() {
        files <- if (is.null(from)) {
            # the extract_package_code only work on packages that are installed
            # we need to install the package if it has not yet been installed
            extract_package_code(package=package, types=types, output_dir=temp_dir, filter=filter)
        } else {
            lapply(from, extract_package_code, types=types, output_dir=temp_dir, filter=filter)
        }

        files <- unlist(files, use.names=FALSE)

        if (length(files) == 0) {
            if (is.null(from)) {
                warning("The package does not contain any runnable code in ", paste(types, sep=","))
            } else {
                warning("No runnable code was found, make sure that the `from` packages were installed with ",
                    "`INSTALL_opts=c('--example', '--install-tests')")
            }

            # TODO: is this correct?
            data.frame(file=character(), output=character(),  error=character())
        } else {
            gen_from_source(path, files=files, ...)
        }
    }

    if (!is.null(install_path)) {
        # package has been already installed
        phase2()
    } else {
        # we first need to install the package
        withr::with_temp_libpaths({
            log_debug("Installing package from path ", path, " into temporary location ", .libPaths[1])
            install_package(path, types)

            phase2()
        })
    }
}

#' @export
gen_from_source <- function(path, files, action, output_dir, prune_tests=TRUE, quiet=TRUE, ...) {
    stopifnot(is_chr_scalar(path) && dir.exists(path))
    stopifnot(action != "generate" || prune_tests)

    package <- devtools::as.package(path)

    # TODO: add a flag to trace only exported functions
    tracing <- trace_package(
        package=package$package,
        files=files,
        action=action,
        output_dir=output_dir,
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
