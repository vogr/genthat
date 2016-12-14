
#' @title Adds regression tests to specified package.
#'
#' @description Given a package, the code to be executed and a list of functions to capture, the function captures the selected functions from the package, then runs the specified code. It then generates tests from the captured information and using code coverage filters them against existing tests for the package. Those that increase the code coverage will be added to already existing testthat tests for the package.
#'
#' @param package.dir Name/path to the package, uses devtools notation
#' @param code Function (with no arguments) whose code will be executed and its calls in the package captured.
#' @param functions Functions from the package to be captured, if missing, all package functions will be captured (character vector)
#' @param filter T if the generated tests should be filtered
#' @param exclude_existing_tests If TRUE, existing tests will be ignored from the code coverage
#' @param build T if the package will be built beforehand
#' @param timed TRUE if the tests result depends on time, in which case the current date & time will be appended to the output_dir.
#' @param output If used, specifies where should the tests be unfiltered tests be generated (if not specified, they will use a temp directory and clean it afterwards)
#' @param verbose Prints additional information.
#' @export

gen_from_function <- function(package.dir = ".", trace.dir = "traces", code, functions, filter = TRUE, exclude_existing_tests = FALSE, build = TRUE, timed = FALSE, output, verbose = FALSE, clear_capture = TRUE) {
    cleanup <- F

    package <- devtools::as.package(package.dir)
    cache$pkg.name <- package$package
    devtools::load_all(package.dir)
    if (verbose) message(paste0("Package ", package$package, " loaded\n"))

    if (missing(functions)) {
        # get list of all functions defined in the package' R code
        functions <- list_functions(file.path(package$path, "R"))
        if (verbose) message("All functions from package will be decorated\n")
    }
    if (verbose) message(paste("Decorating",length(functions), "functions\n"))
    lapply(functions, function(f) decorate(f, package$package, verbose = verbose))

    # start the capturing
    cache$capture.folder = trace.dir
    cache$capture.arguments <- TRUE
    # run the code
    code()
    # stop capturing
    cache$capture.arguments <- FALSE
    stop_capture_all()

    # generate the tests to the output directory
    if (missing(output)) {
        if (filter) {
            output = "temp"
            cleanup = T
        } else {
            output = file.path(package$path, "tests")
        }
    }
    if (verbose) message(paste("Generating tests to", output, "\n"))
    gen_results <- generate(output, root = trace.dir, verbose = verbose, clear_capture = clear_capture)

    # filter, if enabled
    if (filter) {
        if (verbose)
            cat("Pruning tests - this may take some time...\n")
        filter_tests(output, file.path(package$path, "tests/testthat"), functions, package.dir, compact = T, verbose = verbose)
    }

    # clear the temp folder, if we used a temp folder implicitly
    if (cleanup)
        unlink(output, recursive = T)

    per_function_test_count <- lapply(cache$tid, function(x) x + 1L)
    total_test_count <- sum(unlist(per_function_test_count))

    list(gen_results = gen_results, tests_generated = total_test_count)
}



#' @title Generates tests for a package by running the code associated with it.
#'
#' @description Runs the examples, vignettes and possibly tests associated with the package and captures the usage of package's functions. Creates tests from the captured information, filters it according to the already existing tests and if any new tests are found, adds them to package's tests.
#'
#' @param package.dir Name/path to the package, uses devtools notation.
#' @param include.tests If TRUE, captures also execution of package's tests.
#' @param build if to build package before. Default \code{TRUE}
#' @param timed TRUE if the tests result depends on time, in which case the current date & time will be appended to the output_dir.
#' @param filter TRUE if generated tests should be filteres so that only those adding to a coverage will be used
#' @param output If used, specifies where should the tests be unfiltered tests be generated (if not specified, they will use a temp directory and clean it afterwards)
#' @param verbose Prints additional information.
#' @export
#'
gen_from_package <- function(package.dir = ".", capture.dir = "capture", include.tests = FALSE, include.vignettes = FALSE, include.manPages = FALSE, timed = FALSE, filter = TRUE, build = TRUE, output, verbose = FALSE, clear_capture = TRUE) {
    package = devtools::as.package(package.dir)

    require(utils)
    require(methods)
    devtools::document(package.dir)

    detach(concat("package:", package$package), unload = T, character.only = T)

    f <- function() {
        if (include.vignettes)
        {
            info <- tools::getVignetteInfo(package = package$package)
            vdir <- info[,2]
            vfiles <- info[,6]
            p <- file.path(vdir, "doc", vfiles)
            if (verbose) message(paste("Running vignettes (", length(vfiles), "files)\n"))
            # vignettes are not expected to be runnable, silence errors
            invisible(tryCatch(sapply(p, source), error=function(x) invisible()))
        }
        if (include.manPages)
        {
            # run package examples
            manPath <- file.path(package.dir, "man")
            examples <- list.files(manPath, pattern = "\\.[Rr]d$", no.. = T)
            if (length(examples) != 0) {
                if (verbose)
                    cat(paste("Running examples (", length(examples), "man files)\n"))
                for (f in examples) {
                    code <- example_code(file.path(manPath, f))
                    tryCatch(eval(parse(text = code)), error=function(x) print(x))
                }
            }
        }
        if (include.tests)
        {
            if (verbose) message("Running package tests\n")
            run_package_tests(package.dir, verbose)
        }

    }

    gen_from_function(package.dir, capture.dir = capture.dir, code = f , filter = filter, exclude_existing_tests = include.tests, build = build, timed = timed, output = output, verbose = verbose, clear_capture = clear_capture)
}

#' @title Generate tests for give code
#' @description Generates tests from given code and specific captured functions
#'
#' @param code Code from which the tests will be generated.
#' @param output_dir Directory to which the tests will be generated.
#' @param ... functions to be captured during the code execution (same syntax as capture function)
#' @export
gen_from_code <- function(code, output_dir, trace.dir, ...) {
    code <- substitute(code) # ??? why not just pass it down to eval directly
    start_capture(package.name = NULL, capture.dir = trace.dir)
    eval.parent(code)
    stop_capture_all()
    generate(output_dir, root = trace.dir)
    invisible()
}

#' @title Generate tests for give source
#' @description Generates tests by running given source file.
#'
#' @param src.root Source file to be executed and captured, or directory containing multiple files.
#' @param output_dir Directory to which the tests will be generated.
#' @param ... Functions to be tested.
#' @export
gen_from_source <- function(src.root, output_dir, trace.dir, ...) {
    if (!file.exists(src.root))
        stop("Supplied source does not exist")
    if (file.info(src.root)$isdir)
        src.root <- list.files(src.root, pattern = "\\[rR]", recursive = T, full.names = T)
    cache$capture.arguments <- TRUE
    start_capture(...)
    for (src.file in src.root)
        source(src.file, local = T)
    stop_capture_all()
    generate(output_dir, root = trace.dir)
    invisible()
}

#' Run tests generated by genthat.
#'
#' @param Path to directory containing the generated tests.
#' @export
run_generated_tests <- function(dir) {
    test_files <- Sys.glob(file.path(dir, "/*/test-*.R"))
    results <- list()
    for (f in test_files) {
        retCode <- system(paste0("Rscript ", f))
        results[[f]] <- list(
            funName = sub("^.*/(.*)/.*\\.R", "\1", f),
            passed = if (retCode == 0) TRUE else FALSE
        )
    }
    results
}

#' Returns comparison of two results returned by \code{run_generated_tests()}.
#'
#' @param resA first result
#' @param resB second result
#' @return comparison structure
#' @export
compare_test_results <- function(resA, resB) {
    keys <- names(resA)
    out <- list()
    for (key in keys) {
        valA <- resA[[key]]
        valB <- resB[[key]]
        out[[key]] <- list(
            funName = valA$funName, 
            passedA = valA$passed,
            passedB = valB$passed,
            changed = !identical(valA$passed, valB$passed)
        )
    }
    out
}
