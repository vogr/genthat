
#' @title Generate tests for given package
#' @description Generates tests from package downloaded from CRAN
#'
#' @param package name of package
#' @export
toString.gen_result <- function(x) {
    yaml::as.yaml(x)
}

#' @title Generate tests for given package
#' @description Generates tests from package downloaded from CRAN
#'
#' @param package name of package
#' @export
print.gen_result <- function(x) {
    cat(toString.gen_result(x))
}

gen_result <- function(fetchTime, genTime, totalTime, testsGenerated, retv_mismatch_count, unparsable_count) {
    vals <- list(
        fetchTime = formatTime(fetchTime),
        genTime = formatTime(genTime),
        totalTime = formatTime(totalTime),
        testsGenerated = testsGenerated,
        retv_mismatch_count = retv_mismatch_count,
        unparsable_count = unparsable_count
    )
    class(vals) <- 'gen_result'
    vals
}
