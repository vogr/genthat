#' @export
run_generated_test <- function(code) {
    stopifnot(is.character(code) && length(code) == 1)

    tmp_test_file <- tempfile(pattern="run_generated_test-", fileext=".R")
    on.exit(file.remove(tmp_test_file))
    write(code, file=tmp_test_file)

    result <- list(
        status=NA,
        output=NA,
        elapsed=NA,
        error=NA
    )

    result <- tryCatch({
        run <- capture(testthat_result <- testthat::test_file(tmp_test_file))

        result$output <- paste(c("stdout:", run$stdout, "stderr:", run$stderr), collapse="\n\n")
        result$elapsed <- run$elapsed

        if (length(testthat_result) == 0) {
            result$status <- -1
            result$error <- "testthat::test_file result was empty"
        } else if (!is.null(testthat_result)) {
            testthat_result <- as.data.frame(testthat_result)

            result$status <- if (testthat_result$failed) {
                # test failed
                1
            } else if (testthat_result$error) {
                # exception thrown
                2
            } else if (testthat_result$nb == 0) {
                # no tests were run
                3
            } else {
                # success
                0
            }
        }

        result
    }, expectation_failure=function(e) {
        result$status <- -2
        result$error <- paste("testthat::test_file expectation failure: ", e$message)

        result
    }, error=function(e) {
        result$status <- -3
        result$error <- paste("testthat::test_file error: ", e$message)

        result
    })

    result
}

#' status code:
#'  0: OK
#' -1: Test could not be run - i.e. a syntax error
#'  1: Test failed
#'  2: Test threw an exception
#'  3: Test did not contain any tests (nb field in the testthat_result was 0)
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom utils getTxtProgressBar
#' @export
#'
run_generated_tests <- function(info_file, quiet=TRUE) {
    tests <- readr::read_csv(info_file, col_types=cols(
        fun=col_character(),
        pkg=col_character(),
        test_file=col_character(),
        error=col_character(),
        elapsed=col_double()
    ))

    files <- tests$test_file

    res <- lapply(files, function(f) {
        if (is.na(f) || !file.exists(f)) {
            tibble::data_frame(file=f, exception=paste("File ", f, " does not exist"))
        } else {
            tryCatch({
                out <- capture_output(r <- testthat::test_file(f))
                tibble::as_data_frame(r) %>% bind_cols(data_frame(out=out))
            }, error=function(e) {
                tibble::data_frame(file=f, exception=e$message)
            })
        }
    })

    dplyr::bind_rows(res)
}
