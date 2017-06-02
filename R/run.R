capture <- function(expr) {
    out <- tempfile()
    err <- tempfile()

    fout = file(out, open="wt")
    ferr = file(err, open="wt")

    sink(type="output", file=fout)
    sink(type="message", file=ferr)

    time <- as.numeric(Sys.time())*1000
    result <- tryCatch({
        force(expr)
    }, error=function(e) {
        e
    }, finally={
        time <- as.numeric(Sys.time())*1000 - time

        sink(type="output")
        sink(type="message")
        close(fout)
        close(ferr)
    })

    list(
        result=result,
        time=time,
        out=paste(readLines(out), collapse="\n"),
        err=paste(readLines(err), collapse="\n")
    )
}


#' @importFrom methods is
#' @export
#'
run_generated_tests <- function(tests) {
    stopifnot(is.character(tests))

    result <-
        lapply(tests, function(x) {
            n <- tools::file_path_sans_ext(x)
            r <- capture(testthat::test_file(x))

            writeLines(r$out, paste0(n, ".out"))
            writeLines(r$err, paste0(n, ".err"))
            writeLines(as.character(r$time), paste0(n, ".time"))

            if (methods::is(r$result, "error")) {
                if (is_debug_enabled()) {
                    message("Test ", x, " failed: ", r$result$message)
                }
                r <- data.frame(list(test=x, message=r$result$message), stringsAsFactors=FALSE)
                attr(r, "passed") <- FALSE
                r
            } else {
                r <- as.data.frame(r$result)
                attr(r, "passed") <- TRUE
                r
            }
        })

    passed <- filter(result,  has_attr, name="passed", value=TRUE)
    passed <- do.call(rbind, passed)

    failed <- filter(result,  has_attr, name="passed", value=FALSE)
    failed <- do.call(rbind, failed)

    list(passed=passed, failed=failed)
}
