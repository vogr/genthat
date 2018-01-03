run_generated_test <- function(test, quiet=TRUE) {
    stopifnot(file.exists(test))

    tryCatch({
        if (!quiet) {
            cat("Running ", test, "... ")
        }

        time <- stopwatch(res <- testthat::test_file(test, reporter="stop", wrap=FALSE))

        if (length(res) == 0) {
            stop("testthat::test_file result was empty")
        }

        time <- as.numeric(time, units="secs")

        if (!quiet) {
            cat("OK (", time, " sec)")
        }

        time
    }, error=function(e) {
        if (!quiet) {
            cat("FAILED")
        }

        as_chr_scalar(e$message)
    })
}

#' @export
run_generated_tests <- function(tests, quiet=TRUE) {
    if (length(tests) == 0) {
        return(numeric())
    }

    result <- pbapply::pblapply(tests, run_generated_test, quiet=quiet)
    names(result) <- tests

    elapsed <- sapply(result, function(x) if (is.numeric(x)) x else NA)
    errors <- sapply(result, function(x) if (is.character(x)) x else NA)

    attr(elapsed, "errors") <- errors
    elapsed
}
