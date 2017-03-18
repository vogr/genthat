
library(genthat)
library(testthat)

source("./utils.R")

context("Basic usage of genthat")

test_that("basic gen_from_code usage", {
    test_capturing(function(dir) {
        dir.create("generated_tests")
        dir.create("traces")

        fn1 <- function(a, b) { a + b + 1 }
        fn1 <- decorate_function_val(fn1, "fn1_label")

        gen_from_code({ fn1(32, 81) }, "generated_tests", "traces")

        testfile <- file.path(dir, 'generated_tests', 'fn1_label', 'test-0.R')

        test_result1 <- run_test_file(testfile, list(
            fn1_label = function(a, b) { 1 + a + b }
        ))
        expect_equal(test_result1, TRUE)

        test_result2 <- run_test_file(testfile, list(
            fn1_label = function(a, b) { 2 + a + b }
        ))
        expect_equal(test_result2, FALSE)
    })
})

test_that("basic gen_from_function usage", {
    test_capturing(function(dir) {
        dir.create("generated_tests")
        dir.create("traces")

        example_package_path <- file.path(get_testthat_folder(), "toyProject")
        gen_result <- gen_from_function(example_package_path, function() { suppressMessages({ public_fn(32) }) }, out = "generated_tests", trace.dir = "traces")

        testfile <- file.path(dir, 'generated_tests', 'toyProject___public_fn', 'test-0.R')

        test_result1 <- run_test_file(testfile, list())
        expect_equal(test_result1, TRUE)
    })
})

