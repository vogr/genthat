
library(testthat)

source("./utils.R")

context("Trace storing and retrieval")

test_that("basic trace pushing", {
    fn1 <- function(a, b) { a + b + 1L }

    decorate_functions("fn1")
    fn1(4L, 3L)

    counter <- 0L
    while (traces$has_next()) {
        counter <- counter + 1L
        trace <- traces$get_next()
        expect_equal(trace$func, "fn1")
        expect_equal(trace$args, "list(a=4L,b=3L)")
        expect_equal(trace$retv, "8L")
    }
    expect_equal(counter, 1)
})

test_that("tracing - names of function parameters enclosed in backticks are supported", {
    fn1 <- function(a, `b c`, `function`) { a + `b c` + `function` + 1L }

    decorate_functions("fn1")
    fn1(4L, 3L, 2L)

    counter <- 0L
    while (traces$has_next()) {
        counter <- counter + 1L
        trace <- traces$get_next()
        expect_equal(trace$func, "fn1")
        expect_equal(trace$args, "list(a=4L,`b c`=3L,`function`=2L)")
        expect_equal(trace$retv, "10L")
    }
    expect_equal(counter, 1)
})
