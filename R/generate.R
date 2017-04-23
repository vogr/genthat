format_args <- function(trace) {
    args <- if (!is.null(names(trace$args))) {
               pairs <- zip(name=names(trace$args), value=trace$args)
               lapply(pairs, function(x) {
                   if(nchar(x$name) > 0) {
                       paste0(x$name, "=", x$value)
                   } else {
                       x$value
                   }
               })
           } else {
               trace$args
           }

    paste(args, collapse=", ")    
}


#' @title Generate test case from trace
#'
#' @description This function generates a test case from the passed trace.
#' @param trace trace value
#'
generate_test <- function(trace) {
    stopifnot(is(trace, "genthat_trace"))
    
    args_str <- format_args(trace)
    call_str <- paste0(trace$fun, "(", args_str,")")
    
    paste(
        paste0('test_that("', trace$fun, '"',", {"),
        paste0("\texpected <- ", trace$retv),
        paste0("\texpect_equal(", call_str, ", expected)"),
        "})",
        sep="\n"
    )
}

#' @title Generates tests from captured information.
#'
#' @description This function takes the tracing information collected by capture and generates
#' testthat compatible testcases.
#'
#' @param output_dir Directory to which the tests should be generated.
#' @export
gen_tests <- function(traces, output_dir="generated_tests") {
    stopifnot(is.list(traces))
    stopifnot(is.character(output_dir))

    if (length(traces) == 0) {
        return(list())
    }
    
    traces <- filter(traces, is, class2="genthat_trace")
    if (length(traces) == 0) {
        return(list())
    }

    if (!dir.exists(output_dir)) {
        if (!dir.create(output_dir)) {
            stop("Couldn't create ", output_dir)
        }
    }

    tests <- lapply(traces, generate_test)
    fnames <- file.path(output_dir, paste0("test-", 1:length(tests), ".R"))
    tests_with_fnames <- zip(fname=fnames, test=tests)
    
    lapply(tests_with_fnames, function(x) write(x$test, file=x$fname))
    
    fnames
}

