#' @export
#'
run_generated_test <- function(file, quiet=TRUE) {
    stopifnot(file.exists(file))

    tryCatch({
        testthat:::capture_output(res <- testthat::test_file(file), print=!quiet)
        res <- as.data.frame(res)
        res$file <- file
        res
    }, error=function(e) {
        data.frame(
            file=file,
            exception=e$message,
            row.names=NULL,
            stringsAsFactors=FALSE
        )
    })
}

#' @export
#'
run_generated_tests <- function(files, quiet=TRUE) {
    res <- lapply(files, function(f) {
        if (!file.exists(f)) {
            data.frame(
                file=f,
                exception=paste("File", f, "does not exist"),
                row.names=NULL,
                stringsAsFactors=FALSE
            )
        } else {
            run_generated_test(f)
        }
    })

    do.call(rbind, res)
}
