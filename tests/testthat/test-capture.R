library(genthat)
library(testthat)

source("./utils.R")

context("Call capturing")

test_that('Can capture arguments of type environment', {
    test_capturing(function(dir) {
        dir.create("generated_tests")
        dir.create("traces")

        myget <- decorate_function_val(function(key, envir) { get(key, envir = envir) }, "myget")
        e1 <- as.environment(list(a = 3, c = 9))
        gen_from_code({ x <- myget("a", envir = e1) }, "generated_tests", "traces")

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
    test_capturing(function(dir) {
        dir.create("generated_tests")
        dir.create("traces")

        example_package_path <- file.path(get_testthat_folder(), "toyProject")
        devtools::load_all(example_package_path)

        decorate("public_fn", "toyProject", verbose = FALSE)

        gen_from_code({ toyProject::public_fn(92) }, "generated_tests", "traces")

        testfile <- file.path(dir, 'generated_tests', 'toyProject___public_fn', 'test-0.R')

        generatedTest <- get_only(loadTestFile(testfile)$testCases)
        expect_equal(lapply(generatedTest$arguments, eval), list(x = 92))
    })
})

test_that('isDecoratedFun FALSE cases.', {
    expect_equal(isDecoratedFun(function() {}), FALSE)
    expect_equal(isDecoratedFun(function() 5), FALSE)
    expect_equal(isDecoratedFun(function(x) x + 1), FALSE)
    expect_equal(isDecoratedFun(function(x) { print(x); x + 1 }), FALSE)
})

test_that('Can decorate functions manually.', {
    f <- function(key, envir) { get(key, envir = envir) }
    expect_equal(isDecoratedFun(f), FALSE)
    f <- decorate_function_val(f, "myget")
    expect_equal(isDecoratedFun(f), TRUE)
})

test_that('Can decorate functions in packages.', {
    expect_equal(isDecoratedFun(base::unlist), FALSE)
    decorate("unlist", "base", verbose = FALSE)
    expect_equal(isDecoratedFun(base::unlist), TRUE)
})

test_that('Can decorate builtin function.', {
    test_capturing(function(dir) {
        dir.create("generated_tests")
        dir.create("traces")

        decorate("unlist", "base", verbose = FALSE)

        gen_from_code({ unlist(list(1,2,3,4)) }, "generated_tests", "traces")

        testfile <- file.path(dir, 'generated_tests', 'base___unlist', 'test-0.R')

        # TODO function is captured but recorded arguments have really weird values
        warning("TODO function is captured but recorded arguments have really weird values")
        #generatedTest <- get_only(loadTestFile(testfile)$testCases)
        #expect_equal(args, list(x = list(1,2,3,4)))
    })
})

test_that('We capture the same calls for testthat:::comparison as base::trace.', {
    runCompareExamples <- function() { capture.output(suppressWarnings(example(compare))) }

    trace_spy <- get_spy_expression()
    trace("comparison", trace_spy$expression, where = asNamespace("testthat"))
    runCompareExamples()
    untrace("comparison", where = asNamespace("testthat"))
    expect_true(trace_spy$getCount() > 1)

    genthat_spy <- get_spy_expression()
    decorate("comparison", "testthat", enter_function = genthat_spy$fn)
    runCompareExamples()
    undecorate("comparison", "testthat")
    expect_true(genthat_spy$getCount() > 1)

    expect_equal(genthat_spy$getCount(), trace_spy$getCount())
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

