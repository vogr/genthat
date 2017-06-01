# TODO: rename to format_calling_args
format_args <- function(args) {
    args <- lapply(args, serialize_value)

    args_str <- if (!is.null(names(args))) {
                   pairs <- zip(name=names(args), value=args)
                   lapply(pairs, function(x) {
                       if(!is_empty_str(x$name)) {
                           paste0(x$name, "=", x$value)
                       } else {
                           x$value
                       }
                   })
               } else {
                   args
               }

    paste(args_str, collapse=", ")
}

#' @title Generate test case code from a trace
#' @description Given a genthat trace it generates a corresponding test case
#'
#' @param trace trace value
#'
#' @export
generate_test_code <- function(trace, include_trace_dump=FALSE) {
    UseMethod("generate_test_code")
}

#' @export
generate_test_code.genthat_trace <- function(trace, include_trace_dump) {
    stopifnot(is.character(trace$fun))
    stopifnot(is.list(trace$args))

    fun <- trace$fun
    args <- format_args(trace$args)
    globals <- paste(names(trace$globals), lapply(trace$globals, serialize_value), sep=" <- ", collapse="\n")
    retv <- serialize_value(trace$retv)

    header <-
        paste0(
            "library(testthat)\n\n",
            if (include_trace_dump) {
                ts <- capture.output(str(trace))
                ts <- paste("# ", ts, collapse="\n")
                ts <- paste0("# TRACE:\n", ts, "\n\n")
                ts
            } else {
                ""
            }
        )

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

# TODO: to remove
## stopwatch <- function(expr) {
##     t <- as.numeric(Sys.time())*1000
##     r <- force(expr)
##     t <- as.numeric(Sys.time())*1000 - t

##     list(result=r, time=t)
## }

#' @title Generates test cases from traces
#'
#' @param traces from which to generate test cases
#' @param output_dir target directory where to place generated test
#'
#' @description Generates tests cases from the captured traces.
#' @export
#'
generate_tests <- function(traces, output_dir="generated_tests", include_trace_dump=FALSE) {
    stopifnot(is.list(traces) || is.environment(traces))
    stopifnot(is.character(output_dir))

    if (length(traces) == 0) {
        return(list())
    }

    tests <-
        lapply(traces, function(x) {
            if (is_debug_enabled()) {
                message("Generating test trace of ", x$fun)
            }

            tryCatch({
                 generate_test_code(x, include_trace_dump)
            }, error=function(e) {
                warning("Unable to generate test trace of", x$fun, " ", e$message)
                NULL
            })
        })

    tests <- filter_not(tests, is.null)

    if (length(tests) == 0) {
        return(list())
    }

    if (!dir.exists(output_dir)) {
        if (!dir.create(output_dir)) {
            stop("Couldn't create ", output_dir)
        }
    }

    fnames <- file.path(output_dir, paste0("test-", 1:length(tests), ".R"))
    tests_with_fnames <- zip(fname=fnames, test=tests)

    lapply(tests_with_fnames, function(x) write(x$test, file=x$fname))

    fnames
}
