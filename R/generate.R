# TODO: rename to format_calling_args
format_args <- function(args) {
    args <- lapply(args, format_value)

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

#' @export
format_value <- function(x, ...) {
    UseMethod("format_value")
}

#' @export
format_value.genthat_closure <- function(x, ...) {
    args <- paste(names(x$args), lapply(x$args, format_value), sep="=", collapse=", ")
    body <- paste(deparse(x$body, control=c("quoteExpressions")), collapse="\n")

    env <- if (length(x$globals) == 0) {
        "new.env()"
    } else {
        globals <- paste(names(x$globals), lapply(x$globals, format_value), sep="=", collapse=",\n")
        paste0("list2env(list(", globals, "))")
    }

    paste0("as.function(c(alist(", args, "), ", body,"), envir=", env, ")")
}

#' @export
format_value.default <- function(x, ...) {
    serialize_value(x)
}

#' @title Generate test case code from a trace
#' @description Given a genthat trace it generates a corresponding test case
#'
#' @param trace trace value
#'
#' @export
generate_test_code <- function(trace) {
    UseMethod("generate_test_code")
}

#' @export
generate_test_code.genthat_trace <- function(trace) {
    stopifnot(is.character(trace$fun))
    stopifnot(is.list(trace$args))

    fun <- trace$fun
    args <- format_args(trace$args)
    globals <- paste(names(trace$globals), lapply(trace$globals, format_value), sep=" <- ", collapse="\n")
    retv <- format_value(trace$retv)

    paste0(
        'test_that("', fun, '", {',
        # TODO: only link if there is a function
        if (!is_empty_str(globals)) paste0('\n\t', globals, '\n\tgenthat::link_environments()\n') else '',
        '\n\texpect_equal(', fun, '(', args, '), ', retv, ')\n})'
    )
}

#' @export
generate_test_code.default <- function(trace) {
    NULL
}

#' @title Generates test cases from traces
#'
#' @param traces from which to generate test cases
#' @param output_dir target directory where to place generated test
#'
#' @description Generates tests cases from the captured traces.
#' @export
#'
generate_tests <- function(traces, output_dir="generated_tests") {
    stopifnot(is.list(traces))
    stopifnot(is.character(output_dir))

    if (length(traces) == 0) {
        return(list())
    }

    tests <- lapply(traces, generate_test_code)
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
