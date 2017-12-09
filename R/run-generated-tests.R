#' @export
#'
run_generated_test <- function(file, quiet=TRUE) {
    stopifnot(file.exists(file))

    tryCatch({
        output <- testthat:::capture_output(res <- testthat::test_file(file), print=!quiet)
        res <- tibble::as_data_frame(res)
        res <- dplyr::mutate(res, file=file, run_error=NA, elapsed=real, output=output)
        res <- dplyr::select(res, -skipped, -user, -system, -real)
        res
    }, error=function(e) {
        tibble::data_frame(
            file=file,
            run_error=e$message
        )
    })
}

#' @export
#'
run_generated_tests <- function(files, quiet=TRUE) {
    if (length(files) == 0) {
        return(
            tibble::data_frame(
                file=characted(),
                context=character(),
                test=character(),
                nb=integer(),
                failed=integer(),
                error=logical(),
                warning=integer(),
                elapsed=double(),
                run_error=character()
            )
        )
    }

    purrr::map_dfr(files, function(f) {
        if (is.na(f)) {
            tibble::data_frame(file=NA, run_error=NA)
        } else if (!file.exists(f)) {
            tibble::data_frame(file=f, run_error=paste("File", f, "does not exist"))
        } else {
            run_generated_test(f)
        }
    })
}
