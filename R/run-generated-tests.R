#' @export
#'
run_generated_test <- function(testfile, quiet=TRUE) {
    stopifnot(file.exists(testfile))

    tryCatch({
        output <- testthat:::capture_output(res <- testthat::test_file(testfile), print=!quiet)

        if (length(res) == 0) {
            stop("testthat::test_file result was empty")
        }

        res <- tibble::as_data_frame(res)

        output <- if (res$nb == 1 && res$failed == 0 && res$error == FALSE) {
            NA
        } else {
            output
        }

        res <- dplyr::mutate(res, file=testfile, run_error=NA, elapsed=real, output=output)
        res <- dplyr::select(res, -skipped, -user, -system, -real)
        res
    }, error=function(e) {
        tibble::data_frame(
            file=testfile,
            test=NA,
            nb=NA,
            failed=NA,
            error=NA,
            warning=NA,
            elapsed=NA,
            run_error=e$message,
            output=NA
        )
    })
}

#' @export
run_generated_tests <- function(testfiles, quiet=TRUE) {
    if (is.null(testfiles) || length(testfiles) == 0) {
        return(data_frame(
            file=character(),
            test=character(),
            nb=integer(),
            failed=integer(),
            error=logical(),
            warning=integer(),
            elapsed=numeric(),
            run_error=character(),
            output=character()
        ))
    }

    runs <- pbapply::pblapply(testfiles, run_generated_test)
    dplyr::bind_rows(runs)
}
