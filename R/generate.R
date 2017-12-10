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
generate_test <- function(trace, ...) {
    UseMethod("generate_test")
}

#' @export
generate_test.genthat_trace <- function(trace, include_trace_dump=FALSE, format_code=TRUE) {
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
generate_test.genthat_trace_entry <- function(trace, ...) {
    stop("Generate error: No return value")
}

#' @export
generate_test.genthat_trace_error <- function(trace, ...) {
    stop(paste("Code error:", trace$error$message))
}

#' @export
generate_test.genthat_trace_failure <- function(trace, ...) {
    stop(paste("Trace error:", trace$failure$message))
}

#' @param tests this should be a data.frame with class genthat_tests, a result
#'     from calling `generate_tests`.
#' @export
#'
save_test <- function(pkg, fun, code, output_dir) {
    stopifnot(is_chr_scalar(pkg))
    stopifnot(is_chr_scalar(fun))
    stopifnot(is_chr_scalar(code))
    stopifnot(is_chr_scalar(output_dir))

    dname <- file.path(output_dir, pkg, fun)
    stopifnot(dir.exists(dname) || dir.create(dname, recursive=TRUE))

    fname <- next_file_in_row(file.path(dname, "test.R"))
    write(code, file=fname)
    fname
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

#' @title generate test from a trace
#' @description given a trace, it generates a test and stores it in a file
#' @return a data frame with the following
#' pkg        : chr
#' fun        : chr
#' filename   : chr (can be NA in which case error must be a chr)
#' error      : chr (can be NA in which case code must be NA)
#' elapsed    : numeric (can be NA)
#'
#' @export
#'
generate_test_file <- function(trace, output_dir, ...) {
    testfile <- NA
    error <- NA

    tryCatch({
        code <- generate_test(trace, ...)
        testfile <- save_test(trace$pkg, trace$fun, code, output_dir)
    }, error=function(e) {
        error <<- e$message
    })

    tibble::data_frame(
        test=testfile,
        error=error
    )
}

#' @export
generate_test_files <- function(tracefiles, output_dir, ...) {
    generate <- function(tracefile) {
        tryCatch({
            trace <- readRDS(tracefile)
            generate_test_file(trace, output_dir=output_dir)
        }, error=function(e) {
            tibble::data_frame(
                test=NA,
                error=str_c("Generate error: ", e$message)
            )
        })
    }

    ret <- if (length(tracefiles) == 0) {
        tibble::data_frame(test=character(), error=character())
    } else {
        pbapply::pblapply(tracefiles, generate)
    }

    ret <- dplyr::bind_rows(ret)
    ret <- dplyr::mutate(ret, trace=tracefiles)
    ret <- dplyr::select(ret, trace, everything())
    ret
}
