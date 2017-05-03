format_args <- function(args) {
    args_str <- if (!is.null(names(args))) {
                   pairs <- zip(name=names(args), value=args)
                   lapply(pairs, function(x) {
                       if(nchar(x$name) > 0) {
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
generate_test_code <- function(trace) {
    UseMethod("generate_test_code")
}

#' @export
generate_test_code.genthat_trace <- function(trace) {
    stopifnot(is.character(trace$fun))
    stopifnot(is.list(trace$args))
    stopifnot(is.character(trace$retv))

    args_str <- format_args(trace$args)
    call_str <- paste0(trace$fun, "(", args_str, ")")

    paste(
        paste0('test_that("', trace$fun, '"',", {"),
        paste0("\texpected <- ", trace$retv),
        paste0("\texpect_equal(", call_str, ", expected)"),
        "})",
        sep="\n"
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
