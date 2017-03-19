
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
    expect_match(test_case$msg, "^test_that\\(");
})
