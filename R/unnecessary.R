
#' @title Generate tests for given package
#' @description Generates tests from package downloaded from CRAN
#'
#' @param package name of package
#' @export
gen_from_cran <- function(package, version, verbose = TRUE, source.dir = "sources", out.dir = "generated_tests", filter = FALSE, clear_capture = FALSE, include.manPages = TRUE, include.vignettes = TRUE, include.tests = TRUE) {
    output.dir <- file.path(out.dir, package)
    if (dir.exists(output.dir))
        stop("Output dir already exists.")
    dir.create(output.dir, recursive = TRUE)
    if (!dir.exists(source.dir))
        dir.create(source.dir)

    t0 <- Sys.time()

    fetchResult <- get_package_from_cran(package, version = version, dest.dir = source.dir, quiet = !verbose)
    t_fetch <- Sys.time()

    result <- gen_from_package(
        file.path(source.dir, package),
        output = output.dir,
        build = FALSE,
        include.manPages = TRUE,
        include.vignettes = TRUE,
        include.tests = TRUE,
        filter = filter,
        verbose = verbose,
        clear_capture = clear_capture
    )
    t_gen <- Sys.time()

    fetchTime <- t_fetch - t0
    genTime <- t_gen - t_fetch
    totalTime <- t_gen - t0

    # TODO change the way the returned value is printed in the console
    gen_result(
        fetchTime = fetchTime,
        genTime = genTime,
        totalTime = totalTime,
        testsGenerated = cache$generated_tests,
        retv_mismatch_count = cache$retv_mismatch_count,
        unparsable_count = cache$unparsable_count
    )
}


#' @title Generate tests for given package
#' @description Generates tests from package downloaded from CRAN
#'
#' @param package name of package
#' @export
gen_from_archive <- function(path, verbose = TRUE, source.dir = "sources", out.dir = "generated_tests", filter = FALSE, clear_capture = FALSE, include.manPages = TRUE, include.vignettes = TRUE, include.tests = TRUE) {
    if (!dir.exists(source.dir))
        dir.create(source.dir)

    t0 <- Sys.time()

    fetchResult <- get_package_from_archive(path, dest.dir = source.dir, quiet = !verbose)
    t_fetch <- Sys.time()

    package <- fetchResult$pkgName
    output.dir <- file.path(out.dir, package)
    if (dir.exists(output.dir))
        stop("Output dir already exists.")
    dir.create(output.dir, recursive = TRUE)

    result <- gen_from_package(
        file.path(source.dir, package),
        output = output.dir,
        build = FALSE,
        include.manPages = TRUE,
        include.vignettes = TRUE,
        include.tests = TRUE,
        filter = filter,
        verbose = verbose,
        clear_capture = clear_capture
    )
    t_gen <- Sys.time()

    fetchTime <- t_fetch - t0
    genTime <- t_gen - t_fetch
    totalTime <- t_gen - t0

    # TODO change the way the returned value is printed in the console
    gen_result(
        fetchTime = fetchTime,
        genTime = genTime,
        totalTime = totalTime,
        testsGenerated = cache$generated_tests,
        retv_mismatch_count = cache$retv_mismatch_count,
        unparsable_count = cache$unparsable_count
    )
}

