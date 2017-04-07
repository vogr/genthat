library(testthat)
library(devtools)

source("./utils.R")

context("Running testcases generated from traces")

test_that("gen_tests() and run_generated_tests() basic usage", {
    with_tempdir(function(dir) {
        fn<- function(a) { a + 1 }
        fn1 <- decorate_functions(fn)
        fn1[[1]](42)
           
        res <- gen_tests(dir)
        expect_equal(res$n_success, 1)
        expect_equal(res$n_failed, 0)

        files <- list.files(dir, no.. = TRUE)
        expect_match(files, c("tc-0.R"));

		res <- run_generated_tests("generated_tests")
		expect_equal(length(res), 1)
		expect_true(res[[1]]$passed)        
    })
})