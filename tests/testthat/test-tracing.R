
library(testthat)
library(devtools)

source("./utils.R")

context("Trace storing and retrieval")

test_that("basic trace pushing", {
    fn1 <- function(a, b) { a + b + 1L }

    decorate_function("fn1")
    fn1(4L, 3L)

    counter <- 0L
    while (traces$has_next()) {
        counter <- counter + 1L
        trace <- traces$get_next()
        expect_equal(trace$func, "fn1")
        expect_equal(trace$args, "structure(list(a=4L,b=3L), .Names=c(\"a\",\"b\"))")
        expect_equal(trace$retv, "8L")
    }
    expect_equal(counter, 1)
})
