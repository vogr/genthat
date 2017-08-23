format_calling_args <- function(args, include_names=TRUE) {
    args <- lapply(args, serialize_value)

    if (include_names && !is.null(names(args))) {
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
}

dump_raw_trace <- function(trace) {
    s <- utils::capture.output(utils::str(trace))
    s <- paste("# ", s, collapse="\n")
    s <- paste0("# TRACE:\n", s, "\n\n")
    s
}

generate_call <- function(trace) {
    stopifnot(is.character(trace$fun))
    stopifnot(is.list(trace$args))

    fun <- trace$fun
    pkg <- trace$pkg
    if (is.null(pkg)) {
        pkg <- ""
    }

    if (is_infix_fun(fun) && pkg == "base") {
        args <- format_calling_args(trace$args, include_names=FALSE)

        if (length(args) != 2) {
            stop("Call to infix function: `", fun,
                 "` does not have exactly two arguments, instead it has: ",
                 paste(args, collapse=", "))
        }

        sep <- if (is_infix_fun_no_space(fun)) "" else " "

        paste(args[[1]], fun, args[[2]], collapse=sep)
    } else {
        args <- format_calling_args(trace$args, include_names=TRUE)
        args_str <- paste(args, collapse=", ")
        fun <- escape_name(fun)
        pkg <- trace$pkg
        pkg <- if (is.null(trace$pkg)) "" else paste0(pkg, ":::")

        paste0(pkg, fun, '(', args_str, ')')
    }
}

generate_globals <- function(globals) {
    names <- sapply(names(globals), escape_name, USE.NAMES=FALSE)

    paste(names, lapply(globals, serialize_value), sep=" <- ", collapse="\n")
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
generate_test_code.genthat_trace <- function(trace, include_trace_dump=FALSE, format_code=TRUE) {
    call <- generate_call(trace)
    globals <- generate_globals(trace$globals)
    retv <- serialize_value(trace$retv)

    header <- "library(testthat)\n\n"
    if (include_trace_dump) {
        header <- paste(header, dump_raw_trace(trace), sep="\n")
    }

    code <- paste0(
        header,
        'test_that("', trace$fun, '", {\n',
        globals,
        '\nexpect_equal(', call, ', ', retv, ')\n})'
    )

    if (format_code) {
        code <- reformat_code(code)
    }

    code
}

#' @export
generate_test_code.default <- function(trace, include_trace_dump) {
    NULL
}


#' @title generate test from a trace
#' @description given a trace, it generates a test
#' @return a data frame or a tibble with the following
#' trace      : chr
#' fun        : chr
#' code       : chr (can be NA)
#' error      : chr (can be NA)
#' elapsed    : numeric
#'
#' @export
#'
generate_test <- function(trace, ...) {
    UseMethod("generate_test")
}

generate_test_result <- function(trace, code=NA, error=NA, elapsed=NA) {
    list(
        fun=if (is.null(trace$fun)) NA else trace$fun,
        pkg=if (is.null(trace$pkg)) NA else trace$pkg,
        trace=format(trace),
        code=code,
        error=error,
        elapsed=elapsed
    )
}

generate_test.genthat_trace_entry <- function(trace, ...) {
    generate_test_result(trace, error="Generate error: No return value")
}

generate_test.genthat_trace_error <- function(trace, ...) {
    generate_test_result(trace, error=paste("Code error:", trace$error$message))
}

generate_test.genthat_trace_failure <- function(trace, ...) {
    generate_test_result(trace, error=paste("Trace error:", trace$failure$message))
}

#' @importFrom utils capture.output
#' @importFrom utils getTxtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom utils str
generate_test.genthat_trace <- function(trace, ...) {
    tryCatch({
        time <- stopwatch(code <- generate_test_code(trace, ...))

        if (is.null(code)) {
            stop("generate_test_code returned NULL")
        }

        generate_test_result(trace, code=code, elapsed=time)
    }, error=function(e) {
        generate_test_result(trace, error=paste("Generate error:", e$message))
    })
}

#' @title Generates test cases from traces
#'
#' @param traces from which to generate test cases
#' @param show_progress (log) show progress during test generation
#' @param ... additional arguments supplied to `generate_test_code` function.
#'
#' @return a data frame or a tibble with the following
#' fun        : chr
#' trace      : chr
#' trace_type : chr
#' code       : chr (can be NA)
#' error      : chr (can be NA)
#'
#' @description Generates tests cases from the captured traces.
#' @export
#'
generate_tests <- function(traces, quiet=TRUE, ...) {
    stopifnot(is.list(traces) || is.environment(traces))

    if (length(traces) == 0) {
        return(dplyr::data_frame())
    }

    tests <- lapply(traces, function(x) {
        test <- generate_test(x, ...)
        dplyr::as_data_frame(test)
    })

    dplyr::bind_rows(tests)
}

#' @export
#' @importFrom magrittr %>%
#'
save_tests <- function(tests, output_dir) {
    if (length(tests) == 0) {
        return(list())
    }

    stopifnot(is.character(output_dir) && length(output_dir) == 1)
    stopifnot(dir.exists(output_dir) || dir.create(output_dir))
    stopifnot(is.data.frame(tests))

    if (nrow(tests) == 0) {
        return(list())
    }

    save_test <- function(fun, pkg, code, id) {
        if (is.na(code)) {
            return(NA)
        }

        dname <- file.path(output_dir, pkg, fun)
        stopifnot(dir.exists(dname) || dir.create(dname, recursive=TRUE))

        fname <- file.path(dname, paste0("test-", id, ".R"))
        write(code, file=fname)
        fname
    }

    tests %>%
        dplyr::group_by(pkg, fun) %>%
        dplyr::mutate(id=row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(test_file=save_test(fun, pkg, code, id)) %>%
        dplyr::select(test_file)
}

reformat_code <- function(code) {
    tryCatch({
        code <- formatR::tidy_source(
            text=code,
            output=FALSE,
            comment=FALSE,
            blank=TRUE,
            arrow=TRUE,
            brace.newline=FALSE,
            indent=4,
            width.cutoff=120
        )

        code <- code$text.tidy

        paste(code, collapse="\n")
    }, error=function(e) {
        warning("Unable to format code: ", code)
        code
    })
}
