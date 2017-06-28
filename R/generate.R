# TODO: rename to format_calling_args
format_args <- function(args) {
    args <- lapply(args, serialize_value)

    args_str <-
        if (!is.null(names(args))) {
            pairs <- zip(name=names(args), value=args)
            lapply(pairs, function(x) {
                if(!is_empty_str(x$name)) {
                    paste0(escape_name(x$name), "=", x$value)
                } else {
                    x$value
                }
            })
        } else {
            args
        }

    paste(args_str, collapse=", ")
}

dump_raw_trace <- function(trace) {
    s <- utils::capture.output(utils::str(trace))
    s <- paste("# ", s, collapse="\n")
    s <- paste0("# TRACE:\n", s, "\n\n")
    s
}

#' @title Generate test case code from a trace
#' @description Given a genthat trace it generates a corresponding test case
#'
#' @param trace trace value
#' @param include_trace_dump whether to include raw trace at the beginning of the test
#' the trace is formatted using `str` function.
#'
#' @importFrom utils str
#' @export
generate_test_code <- function(trace, include_trace_dump=FALSE) {
    UseMethod("generate_test_code")
}

#' @export
generate_test_code.genthat_trace <- function(trace, include_trace_dump=FALSE) {
    stopifnot(is.character(trace$fun))
    stopifnot(is.list(trace$args))

    fun <- trace$fun
    args <- format_args(trace$args)
    globals <- paste(names(trace$globals), lapply(trace$globals, serialize_value), sep=" <- ", collapse="\n")
    retv <- serialize_value(trace$retv)

    header <- "library(testthat)\n\n"
    if (include_trace_dump) {
        header <- paste(header, dump_raw_trace(trace), sep="\n")
    }

    paste0(
        header,
        'test_that("', fun, '", {\n',
        globals,
        '\nexpect_equal(', fun, '(', args, '), ', retv, ')\n})'
    )
}

#' @export
generate_test_code.default <- function(trace, include_trace_dump) {
    NULL
}

#' @title Generates test cases from traces
#'
#' @param traces from which to generate test cases
#' @param show_progress (log) show progress during test generation
#' @param ... additional arguments supplied to `generate_test_code` function.
#'
#' @return a list of two elements:
#' - `tests`: a data frame with three columns and one row per successfully generated test
#'   - `code`: (chr) string scalar with the actual test code
#'   - `trace`: (chr) raw trace used to generate the test code formatted using `str` function
#'   - `id`: (chr) the index of the trace from the `traces` argument
#' - `errors`: a data frame with three columns and one row per unsuccessfully generated test
#'   - `message`: (chr) the cause of unsuccessful generation, error message thrown from `generate_test_code` function
#'   - `trace`: (chr) raw trace used to generate the test code formatted using `str` function
#'   - `id`: (chr) the index of the trace from the `traces` argument
#'
#' @description Generates tests cases from the captured traces.
#' @importFrom utils capture.output
#' @importFrom utils getTxtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom utils str
#' @importFrom utils txtProgressBar
#' @export
#'
generate_tests <- function(traces, show_progress=isTRUE(getOption("genthat.show_progress")), ...) {
    stopifnot(is.list(traces) || is.environment(traces))

    if (length(traces) == 0) {
        return(list())
    }

    if (show_progress) {
        pb <- utils::txtProgressBar(min=0, max=length(traces), initial=0, style=3)

        after_one_trace <- function() utils::setTxtProgressBar(pb, utils::getTxtProgressBar(pb)+1)
        after_all_traces <- function() close(pb)
    } else {
        after_one_trace <- function() {}
        after_all_traces <- function() {}
    }

    tests <-
        lapply(traces, function(x) {

            if (is_debug_enabled()) {
                message("Generating test from a trace of ", x$fun)
            }

            result <- list(
                trace=paste(utils::capture.output(utils::str(x)), collapse="\n"),
                trace_type=class(x)
            )

            tryCatch({
                code <- generate_test_code(x, ...)

                if (!is.null(code)) {
                    if (is_debug_enabled()) {
                        message("Generated test for trace of ", x$fun)
                    }

                    result$code <- code
                } else {
                    result$code <- NA
                }

                result$error <- NA
            }, error=function(e) {
                msg <- e$message

                if (is_debug_enabled()) {
                    message("Unable to generate test for trace of ", x$fun, " ", msg)
                }

                result$code <- NA
                result$error <- msg
            }, finally={
                after_one_trace()
            })

            as.data.frame(result, stringsAsFactors=FALSE)
        })
    after_all_traces()

    if (requireNamespace("dplyr", quietly=TRUE)) {
        dplyr::bind_rows(tests)
    } else {
        message("dplyr is not available, which is a pity since it will speed up things")
        do.call(rbind, tests)
    }
}

save_tests <- function(output_dir, tests) {
    stopifnot(is.character(output_dir) && length(output_dir) == 1)
    stopifnot(is.data.frame(tests))

    if (!dir.exists(output_dir)) {
        if (!dir.create(output_dir)) {
            stop("Couldn't create ", output_dir)
        }
    }

    lapply(tests, function(x) {
        fname <- file.path(output_dir, paste0("test-", x$id, ".R"))
        write(x$test, file=fname)
    })

}
