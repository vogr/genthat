
library(testthat)
library(devtools)

source("./utils.R")

context("Testcase generation from traces")

test_that("generate_tc() basic usage", {
    trace <- list(
        type = "trace",
        func = "pkg1::add",
        args = "list(a = 3L, b = 4L)",
        retv = "8L"
    )

    test_case <- generate_tc(trace)
    expect_match(test_case, "^test_that\\(");
})

test_that("gen_tests() basic usage", {
    with_tempdir(function(dir1) {
        genthat:::push_trace(list(
            type = "trace",
            func = "pkg1::add",
            args = "list(a = 3L, b = 4L)",
            retv = "8L"
        ))

        gen_tests(output_dir = dir1)

        files <- list.files(dir1, no.. = TRUE)
        expect_match(files, c("tc-0.R"));

        contents <- read_file(files[1])
        expect_match(contents, "^test_that\\(");
    })
})

test_that("gen_from_package() basic usage", {
    with_tempdir(function(dir1) {
        gen_from_package(get_example_package_path(), include_tests = TRUE,  output_dir = dir1)

        files <- list.files(dir1, no.. = TRUE)
        expect_equal(length(files), 7);
    })
})
