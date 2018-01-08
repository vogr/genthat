#' @importFrom covr package_coverage
#' @export
#
compute_tests_coverage <- function(path, tests, quiet=TRUE) {
    stopifnot(is_chr_scalar(path), file.exists(path))

    tmp=tempfile(pattern="genthat-test-coverage-", fileext=".RDS")
    on.exit(if (file.exists(tmp)) file.remove(tmp))

    code <- sprintf(
        'saveRDS(genthat:::do_compute_tests_coverage(%s), "%s")',
        paste0(deparse(tests, control=c()), collapse=""),
        tmp
    )

    covr::package_coverage(path, type="none", code=code, quiet=quiet)

    result <- readRDS(tmp)
    if (length(result) == 0) {
        return(list())
    }

    names(result) <- tests

    coverage <- sapply(result, function(x) if (is.numeric(x)) x[1] else NA)
    elapsed <- sapply(result, function(x) if (is.numeric(x)) x[2] else NA)
    errors <- sapply(result, function(x) if (is.character(x)) x else NA)

    attr(coverage, "elapsed") <- elapsed
    attr(coverage, "errors") <- errors

    coverage
}

#' @importFrom covr percent_coverage
#
do_compute_tests_coverage <- function(tests) {
    # TODO: accessing the covr:::.coverage is a bit of a hack, yet the most
    # effective way how to do get increasing coverage per tests

    lapply(tests, function(test) {
        old_coverage <- list2env(as.list(covr:::.counters), env=new.env(parent=emptyenv()))

        tryCatch({
            time <- genthat:::stopwatch(testthat::test_file(test, reporter="stop", wrap=FALSE))
            coverage <- structure(as.list(covr:::.counters), class="coverage")

            c(covr::percent_coverage(coverage), as.numeric(time, units="secs"))
        }, error=function(e) {
            # we do not want to capture coverage of failed tests
            # TODO: again not the nicest, we should ask for API to manipulate coverage
            assignInNamespace(".counters", old_coverage, ns="covr")

            as_chr_scalar(e$message)
        })
    })
}
