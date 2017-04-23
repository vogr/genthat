#' genthat: A framework for automatic test-case generation from client code
#'
#' @docType package
#' @name genthat
#' @useDynLib genthat
NULL

#' @title Adds regression tests to specified package.
#'
#' @description Given a package, the code to be executed and a list of functions to capture,
#' the function captures the selected functions from the package, then runs the specified code.
#' It then generates tests from the captured information and using code coverage filters them
#' against existing tests for the package. Those that increase the code coverage will be added
#' to already existing testthat tests for the package.
#'
#' @param package_dir Path to the package.
#' @param trace_dir Path where traces are gonna be stored.
#' @param code Function (with no arguments) whose code will be executed and its calls in the package captured.
#' @param functions Functions from the package to be captured, if missing, all package functions will be captured (character vector)
#' @param filter Whether the generated tests should be filtered based on code coverage.
#' @param exclude_existing_tests If TRUE, existing tests will be ignored from the code coverage
#' @param output If used, specifies where should the tests be unfiltered tests be generated (if missing, they will use a temp directory and clean it afterwards)
#' @param verbose Prints additional information.
#' @export
#' 
gen_from_function <-
    function(
        package_dir = ".",
        code,
        trace_dir = "traces",
        functions,
        filter = TRUE,
        exclude_existing_tests = FALSE,
        output_dir,
        verbose = FALSE,
        clear_capture = TRUE
    ) {

    cleanup <- F

    package <- devtools::as.package(package_dir)
    cache$pkg_name <- package$package
    devtools::load_all(package_dir)
    if (verbose) message(paste0("Package ", package$package, " loaded\n"))

    if (missing(functions)) {
        # get list of all functions defined in the package' R code
        functions <- list_functions(file.path(package$path, "R"))
        if (verbose) message("All functions from package will be decorated\n")
    }
    if (verbose) message(paste("Decorating",length(functions), "functions\n"))
    lapply(functions, function(f) decorate(f, package$package, verbose = verbose))

    # start the capturing
    cache$capture_folder = trace_dir
    cache$capture_arguments <- TRUE
    # run the code
    code()
    # stop capturing
    cache$capture_arguments <- FALSE
    stop_capture_all()

    # generate the tests to the output_directory
    if (missing(output_dir)) {
        if (filter) {
            output_dir = "temp"
            cleanup = T
        } else {
            output_dir = file.path(package$path, "tests")
        }
    }
    if (verbose) message(paste("Generating tests to", output_dir, "\n"))
    gen_results <- generate(output_dir, root = trace_dir, verbose = verbose, clear_capture = clear_capture)

    # TODO
    #if (filter) {
    #    if (verbose)
    #        cat("Pruning tests - this may take some time...\n")
    #    filter_tests(output_dir, file.path(package$path, "tests/testthat"), functions, package_dir, compact = T, verbose = verbose)
    #}

    # clear the temp folder, if we used a temp folder implicitly
    if (cleanup)
        unlink(output_dir, recursive = T)

    per_function_test_count <- lapply(cache$tid, function(x) x + 1L)
    total_test_count <- sum(unlist(per_function_test_count))

    list(gen_results = gen_results, tests_generated = total_test_count)
}



#' @title Generates tests for a package by running the code associated with it.
#'
#' @description Runs the examples, vignettes and possibly tests associated with the package and captures the usage of package's functions. Creates tests from the captured information, filters it according to the already existing tests and if any new tests are found, adds them to package's tests.
#'
#' @param package_dir Name/path to the package, uses devtools notation.
#' @param include_tests If TRUE, captures also execution of package's tests.
#' @param build if to build package before. Default \code{TRUE}
#' @param timed TRUE if the tests result depends on time, in which case the current date & time will be appended to the output_dir.
#' @param filter TRUE if generated tests should be filteres so that only those adding to a coverage will be used
#' @param output If used, specifies where should the tests be unfiltered tests be generated (if not specified, they will use a temp directory and clean it afterwards)
#' @param verbose Prints additional information.
#' @export
#'
run_package <- function(package = ".", include_tests = FALSE, include_vignettes = FALSE,
                       include_man_pages = TRUE, envir = new.env(parent = globalenv()), quiet = FALSE) {

  pkg <- devtools::as.package(package)

  if (include_vignettes) {
    info <- tools::getVignetteInfo(package = pkg$package)
    vdir <- info[,2]
    vfiles <- info[,6]
    p <- file.path(vdir, "doc", vfiles)
    if (!quiet) message(paste("Running vignettes (", length(vfiles), "files)\n"))
    # vignettes are not expected to be runnable, silence errors
    invisible(tryCatch(sapply(p, source), error=function(x) invisible()))
  }
  
  if (include_man_pages) {
    # run package examples
    manPath <- file.path(package, "man")
    examples <- list.files(manPath, pattern = "\\.[Rr]d$", no.. = T)
    if (length(examples) != 0) {
      message(paste("Running examples (", length(examples), "man files)\n"))
      for (f in examples) {
        code <- example_code(file.path(manPath, f))
        tryCatch(eval(parse(text = code)), error=function(x) print(x))
      }
    }
  }
  
  if (include_tests) {
    message("Running package tests\n")
    if (devtools::use_testthat(package)) {
      test_path <- devtools:::find_test_dir(package)
      # TODO: what is the difference between test_dir and test_package
      testthat::test_dir(test_path, env=envir)
    } else if (file.exists(file.path(package, "tests"))) {
      run_R_tests(package)
    } else {
      message("Package has no tests!")
    }
  }
}

#' @title Generates tests for a package by running the code associated with it.
#'
#' @description Runs the examples, vignettes and possibly tests associated with the package and captures the usage of package's functions. Creates tests from the captured information, filters it according to the already existing tests and if any new tests are found, adds them to package's tests.
#'
#' @param package_dir Name/path to the package, uses devtools notation.
#' @param include_tests If TRUE, captures also execution of package's tests.
#' @param build if to build package before. Default \code{TRUE}
#' @param timed TRUE if the tests result depends on time, in which case the current date & time will be appended to the output_dir.
#' @param filter TRUE if generated tests should be filteres so that only those adding to a coverage will be used
#' @param output If used, specifies where should the tests be unfiltered tests be generated (if not specified, they will use a temp directory and clean it afterwards)
#' @param verbose Prints additional information.
#' @export
#'
gen_from_package <- function(package=".", output_dir="generated_tests",
                            type=c("tests", "vignettes", "examples"),
                            clean=FALSE, quiet=FALSE) {
    
    pkg <- devtools::as.package(package)

    if (missing(type)) {
        type <- "tests"
    }
    
    tmp_lib <- tempfile("R_LIBS")
    stopifnot(dir.create(tmp_lib))

    if (isTRUE(clean)) {
        on.exit({
            clean_objects(pkg$path)
        }, add=TRUE)
    }

    utils::install.packages(
               repos=NULL,
               lib=tmp_lib,
               pkg$path,
               type="source",
               dependencies=c("Depends", "Imports", "LinkingTo", "Suggests"),
               INSTALL_opts=c("--example", # TODO: only if we need it
                   "--install-tests", # TODO: only if we need it
                   "--with-keep.source",
                   "--no-multiarch"),
               quiet=quiet)

    libs <- env_path(tmp_lib, .libPaths())
    pkg_dir <- file.path(tmp_lib, pkg$package)
    genthat_output <- file.path(pkg_dir, "genthat.RDS")
    
    add_package_hook(
        pkg$package,
        tmp_lib,
        on_load=substitute({
            message("Invoking genthat package loading hook")
            install.packages(repos=NULL, type="source", pkgs=GENTHAT_PATH)
            genthat::decorate_environment(ns)
        }, list(GENTHAT_PATH=find.package("genthat"))),
        on_gc_finalizer=substitute({
            saveRDS(genthat:::cache, file=GENTHAT_OUTPUT)
        }, list(GENTHAT_OUTPUT=genthat_output))
    )

    # TODO: debug trace
    #cat(paste0(readLines(file.path(pkg_dir,"R","stringr")), collapse="\n"))
    
    withr::with_envvar(c(R_LIBS=libs, R_LIBS_USER=libs, R_LIBS_SITE=libs), {
        ## if ("vignettes" %in% type) {
        ##     type <- type[type != "vignettes"]
        ##     run_vignettes(pkg, tmp_lib)
        ## }

        ## if ("examples" %in% type) {
        ##     type <- type[type != "examples"]
        ##     # testInstalledPackage explicitly sets R_LIBS="" on windows, and does
        ##     # not restore it after, so we need to reset it ourselves.
        ##     withr::with_envvar(c(R_LIBS = Sys.getenv("R_LIBS")), {
        ##         result <- tools::testInstalledPackage(pkg$package, outDir = pkg_dir, types = "examples", lib.loc = tmp_lib) # TODO: add ...
        ##         if (result != 0L) {
        ##             show_failures(pkg_dir)
        ##         }
        ##     })
        ## }

        if ("tests" %in% type) {
            result <- tools::testInstalledPackage(pkg$package, outDir=pkg_dir, types="tests", lib.loc=tmp_lib) # TODO: add ...

            if (!quiet) {           
                out <- file.path(pkg_dir, paste0(pkg$package, "-tests"), "testthat.Rout")
                if (file.exists(out)) cat(paste0(readLines(out), collapse="\n"))
            }
            
            if (result != 0L) {
                show_failures(pkg_dir)
            }
        }

#            f <- file.path(pkg_dir, "stringr-tests", "testthat.Rout.fail")
#            if (file.exists(f)) cat(paste0(readLines(f), collapse="\n"))

    })

    if (!file.exists(genthat_output)) {
        stop("genthat output does not exist ", genthat_output)
    }
    
    output <- readRDS(genthat_output)
    stopifnot(is.environment(output))

    tests <- gen_tests(output$traces, output_dir)

    list(
        traces=length(output$traces),
        errors=length(filter(output$traces, is, class2="genthat_trace_error")),
        tests=tests,
        pkg_dir=if (!clean) pkg_dir else NA
    )
}

#' @title Generate tests for given code
#' @description Generates tests from given code and specific captured functions
#'
#' @param code Code from which the tests will be generated.
#' @param output_dir Directory to which the tests will be generated.
#' @param ... functions to be captured during the code execution (same syntax as capture function)
#' @export
#'
gen_from_code <-
    function(
        code,
        output_dir,
        trace_dir,
        ...
    ) {

    code <- substitute(code) # ??? why not just pass it down to eval directly
    start_capture(package_name = NULL, capture_dir = trace_dir)
    eval.parent(code)
    stop_capture_all()
    generate(output_dir, root = trace_dir)
    invisible()
  }

#' @title Generate tests for give source
#' @description Generates tests by running given source file.
#'
#' @param src_root Source file to be executed and captured, or directory containing multiple files.
#' @param output_dir Directory to which the tests will be generated.
#' @param ... Functions to be tested.
#' @export
#'
gen_from_source <-
    function(
        src_root,
        output_dir,
        trace_dir,
        ...
    ) {

    if (!file.exists(src_root))
        stop("Supplied source does not exist")
    if (file.info(src_root)$isdir)
        src_root <- list.files(src_root, pattern = "\\[rR]", recursive = T, full.names = T)
    cache$capture_arguments <- TRUE
    start_capture(...)
    for (src_file in src_root)
        source(src_file, local = T)
    stop_capture_all()
    generate(output_dir, root = trace_dir)
    invisible()
}

#' Run tests generated by genthat.
#'
#' @param Path to directory containing the generated tests.
#' @export
#'
run_generated_tests <-
    function(
        dir
    ) {

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
#'
compare_test_results <-
    function(
        resA,
        resB
    ) {

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
