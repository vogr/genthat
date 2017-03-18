
library(testthat)
library(devtools)
load_all("../..", export_all = FALSE) # genthat

source("./utils.R")

context("Call capturing")

test_that('Can capture arguments of type environment', {
    with_tempdir(function(dir) {
        dir.create("generated_tests")
        dir.create("traces")

        # myget is used because we cannot record native functions at the moment
        myget <- decorate_function_val(function(key, envir) { get(key, envir = envir) }, "myget")
        env1 <- as.environment(list(a = 3, c = 9))
        gen_from_code({ x <- myget("a", envir = env1) }, "generated_tests", "traces")

        testfile <- file.path(dir, 'generated_tests', 'myget', 'test-0.R')

        test_result1 <- run_test_file(testfile, list(
            myget = function(key, envir) { get(key, envir = envir) }
        ))
        expect_equal(test_result1, TRUE)

        test_result2 <- run_test_file(testfile, list(
            myget = function(key, envir) { 42 }
        ))
        expect_equal(test_result2, FALSE)
    })
})

test_that('Can decorate package function.', {
    with_tempdir(function(dir) {
        dir.create("generated_tests")
        dir.create("traces")

        example_package_path <- file.path(get_testthat_folder(), "example-package")
        load_all(example_package_path, TRUE, export_all = FALSE)

        decorate("public_fn", "examplePackage", verbose = FALSE)

        gen_from_code({ examplePackage::public_fn(92) }, "generated_tests", "traces")

        testfile <- file.path(dir, 'generated_tests', 'examplePackage___public_fn', 'test-0.R')

        generatedTest <- get_only(loadTestFile(testfile)$testCases)
        expect_equal(lapply(generatedTest$arguments, eval), list(x = 92))
    })
})

#test_that('Can decorate functions', {
#
#    test_capture_builtin <- function(functions) {
#        testr_options("verbose", F)
#        suppressWarnings(setup_capture(functions))
#        check.dec <- sapply(functions, function(x) {
#            if (!exists(x, envir = getNamespace('base')))
#                return(TRUE)
#            obj <- get(x, envir = getNamespace('base'))
#            if (is.function(obj)) {
#                if (class(obj) == "functionWithTrace")
#                    return(TRUE)
#                if (!testr:::eligible_capture(x))
#                    return(TRUE)
#                testr:::is_s3_generic(x)
#            } else {
#                TRUE
#            }
#        })
#        expect_true(length(testr:::.decorated) > 0)
#        expect_true(all(check.dec))
#        stop_capture_all()
#        check.dec <- sapply(functions, function(x) {
#            if (!exists(x, envir = getNamespace('base')))
#                return(FALSE)
#            obj <- get(x, envir = getNamespace('base'))
#            if (is.function(obj))
#                class(obj) == "functionWithTrace" && x != "library"
#            else
#                FALSE
#        })
#        expect_true(length(testr:::.decorated) == 0)
#        expect_false(any(check.dec))
#    }
#
#    # to get rid of randomness
#    functions <- c("any", "alist", "double", "deparse", "is.logical", "isOpen", "log2", "mean.Date", "pmax", "sort", "sweep", "unsplit")
#    test_capture_builtin(functions)
#})

#test_that('Can decorate functions (long)', {
#    skip_on_cran()
#    # TODO reenable!!
#    #functions <- builtins()
#    #test_decoration(functions)
#})

