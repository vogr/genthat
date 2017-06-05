stopwatch <- function(expr) {
    time <- as.numeric(Sys.time())*1000
    result <- force(expr)
    time <- as.numeric(Sys.time())*1000 - time

    list(result=result, time=time)
}

capture <- function(expr) {
    out <- tempfile()
    err <- tempfile()

    fout = file(out, open="wt")
    ferr = file(err, open="wt")

    sink(type="output", file=fout)
    sink(type="message", file=ferr)

    result <- tryCatch({
        stopwatch(expr)
    }, error=function(e) {
        list(result=e)
    }, finally={
        sink(type="output")
        sink(type="message")
        close(fout)
        close(ferr)
    })

    c(
        result,
        list(
            out=paste(readLines(out), collapse="\n"),
            err=paste(readLines(err), collapse="\n")
        )
    )
}


#' @importFrom methods is
#' @export
#'
run_generated_tests <- function(tests) {
    stopifnot(is.data.frame(tests))

    pb <- txtProgressBar(min=0, max=length(tests), initial=0, style=3)
    result <-
        apply(tests, 1, function(x) {
            x <- as.list(x)
            tmp_test_file <- tempfile()

            write(x$code, file=tmp_test_file)

            r <- capture(testthat::test_file(tmp_test_file))

            setTxtProgressBar(pb, getTxtProgressBar(pb)+1)

            if (methods::is(r$result, "error")) {
                msg <- r$result$message
                if (is_debug_enabled()) {
                    message("Test ", x, " failed: ", msg)
                }

                r$message <- msg
                r[["result"]] <- NULL

                structure(as.data.frame(c(x, r), stringsAsFactors=FALSE), passed=FALSE)
            } else {
                if (is_debug_enabled()) {
                    message("Test ", x, " succeeded")
                }

                r <-
                    cbind(
                        as.data.frame(r$result), # this one is the test result and has to be converted extra
                        as.data.frame(x, stringsAsFactors=FALSE),
                        as.data.frame(list(out=r$out, err=r$err), stringsAsFactors=FALSE))

                structure(r, passed=TRUE)
            }
        })
    close(pb)

    passed <- filter(result,  has_attr, name="passed", value=TRUE)
    passed <- do.call(rbind, passed)

    failed <- filter(result,  has_attr, name="passed", value=FALSE)
    failed <- do.call(rbind, failed)

    list(passed=passed, failed=failed)
}
