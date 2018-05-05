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

    total_coverage <- covr::package_coverage(path, type="none", code=code, quiet=quiet)
    total_coverage <- covr::tally_coverage(total_coverage)

    result <- readRDS(tmp)
    if (length(result) == 0) {
        return(list())
    }

    # now we need to normalize the stored coverage
    result <- lapply(result, function(x) {
        if (is.data.frame(x)) {
            coverage <- normalize_coverage(x, full=total_coverage)
            coverage <- (sum(coverage$value > 0) / length(coverage$value)) * 100

            c(coverage, attr(x, "time"))
        } else {
            x
        }
    })
    names(result) <- tests

    errors <- sapply(result, function(x) if (is.character(x)) x else NA)
    coverage <- sapply(result, function(x) if (is.numeric(x)) x[1] else NA)
    elapsed <- sapply(result, function(x) if (is.numeric(x)) x[2] else NA)

    attr(coverage, "elapsed") <- elapsed
    attr(coverage, "errors") <- errors
    attr(coverage, "raw_coverage") <- total_coverage

    coverage
}

#' @importFrom covr percent_coverage
#
do_compute_tests_coverage <- function(tests) {
    # accessing the covr:::.coverage is a bit of a hack, yet the most effective
    # way how to do get increasing coverage per tests

    n <- length(tests)
    i <- 1

    get_coverage <- function() {
        coverage <- structure(as.list(covr:::.counters), class="coverage")
    }

    result <- lapply(tests, function(test) {
        old_coverage <- list2env(as.list(covr:::.counters), env=new.env(parent=emptyenv()))

        log_debug("Running ", i, "/", n, ":", test)
        i <<- i + 1

        tryCatch({
            time <- genthat:::stopwatch(genthat::test_generated_file(test))
            time <- as.numeric(time, units="secs")

            coverage <- covr::tally_coverage(get_coverage())
            coverage <- dplyr::filter(coverage, value > 0)

            log_debug("Finished ", test, " in ", time, " covered lines: ", coverage)

            attr(coverage, "time") <- time
            coverage
        }, error=function(e) {
            # we do not want to capture coverage of failed tests
            # TODO: again not the nicest, we should ask for API to manipulate coverage
            assignInNamespace(".counters", old_coverage, ns="covr")

            log_debug("Failed ", test)
            as_chr_scalar(e$message)
        })
    })

    result
}

normalize_coverage <- function(partial, full) {
    tmp_partial <- dplyr::mutate(partial, filename=sub(".*/R/", "R/", filename))
    tmp <- dplyr::left_join(full, tmp_partial, by=c("filename", "functions", "line"))
    tmp <- dplyr::mutate(tmp, value=ifelse(is.na(value.y), 0, value.y))
    tmp <- dplyr::select(tmp, -value.x, -value.y)

    stopifnot(sum(tmp$value) == sum(partial$value))

    tmp
}
