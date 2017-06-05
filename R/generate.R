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

#' @title Generates test cases from traces
#'
#' @param traces from which to generate test cases
#'
#' @description Generates tests cases from the captured traces.
#' @export
#'
generate_tests <- function(traces, include_trace_dump=FALSE) {
    stopifnot(is.list(traces) || is.environment(traces))

    if (length(traces) == 0) {
        return(list())
    }

    pb <- txtProgressBar(min=0, max=length(traces), initial=0, style=3)
    results <-
        lapply(traces, function(x) {
            if (is_debug_enabled()) {
                message("Generating test from a trace of ", x$fun)
            }

            trace_str <- paste(capture.output(str(x)), collapse="\n")

            tryCatch({
                test <- generate_test_code(x, include_trace_dump)

                if (!is.null(test)) {
                    if (is_debug_enabled()) {
                        message("Generated test for trace of ", x$fun)
                    }

                    structure(data.frame(list(code=test, trace=trace_str)), generated=TRUE)
                } else {
                    NULL
                }
            }, error=function(e) {
                msg <- e$message

                if (is_debug_enabled()) {
                    message("Unable to generate test for trace of ", x$fun, " ", msg)
                }

                structure(data.frame(list(message=msg, trace=trace_str)), generated=FALSE)
            }, finally={
                setTxtProgressBar(pb, getTxtProgressBar(pb)+1)
            })
        })
    close(pb)

    tests <- filter(results, has_attr, name="generated", value=TRUE)
    tests <- do.call(rbind, tests)
    tests <- cbind(tests, id=row.names(tests))
    row.names(tests) <- NULL

    errors <- filter(results, has_attr, name="generated", value=FALSE)
    errors <- do.call(rbind, errors)
    errors <- cbind(errors, id=row.names(errors))
    row.names(errors) <- NULL

    list(tests=tests, errors=errors)
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
