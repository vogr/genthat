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
run_generated_tests <- function(tests, quiet=TRUE, show_progress=isTRUE(getOption("genthat.show_progress"))) {
    stopifnot(is.data.frame(tests))

    if (quiet && show_progress) {
        pb <- utils::txtProgressBar(min=0, max=nrow(tests), initial=0, style=3)

        after_one_run <- function() utils::setTxtProgressBar(pb, utils::getTxtProgressBar(pb) + 1)
        after_all_runs <- function() close(pb)
    } else {
        after_one_run <- function() {}
        after_all_runs <- function() {}
    }

    runs <- apply(tests, 1, function(x) {
        x <- as.list(x)

        result <- list(
            trace_id=x$trace_id,
            trace=x$trace,
            fun=x$fun,
            code=x$code,
            test=NA,
            status=NA,
            error=NA,
            stdout=NA,
            stderr=NA,
            elapsed=NA
        )

        tryCatch({
            if (!is.na(x$code)) {
                if (!quiet) {
                    message("Running test from trace: ", x$trace_id, " (", nchar(x$code, type="bytes"), " bytes)")
                }

                run <- run_generated_test(x$code, quiet)
                result$test <- run$test
                result$status <- run$status
                result$error <- run$error
                result$stdout <- run$stdout
                result$stderr <- run$stderr
                result$elapsed <- run$elapsed
            }
        }, error=function(e) {
            msg <- e$message
            message("Unable to run tests from trace: ", x$trace_id, " - ", msg)
            result$error <- msg
            result$status <- -1
        })

        after_one_run()
        as.data.frame(result, stringsAsFactors=FALSE)
    })

    after_all_runs()

    if (requireNamespace("dplyr", quietly=TRUE)) {
        dplyr::as_data_frame(dplyr::bind_rows(runs))
    } else {
        message("dplyr is not available, which is a pity since it will speed up things")
        do.call(rbind, runs)
    }
}
