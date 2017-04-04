
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
        expect_equal(trace$args, "list(call=list(4L,3L),vals=list())")
        expect_equal(trace$retv, "8L")
    }
    expect_equal(counter, 1)
})

test_that("basic trace pushing", {
    fn1 <- function(a, b) { a + b + 1L }

    decorate_functions("fn1")

    x <- 42L
    y <- 15L
    fn1(x, y + x)

    counter <- 0L
    while (traces$has_next()) {
        counter <- counter + 1L
        trace <- traces$get_next()
        expect_equal(trace$func, "fn1")
        expect_equal(trace$args, "list(call=list(quote(x),quote(y+x)),vals=list(x=42L,y=15L))")
        expect_equal(trace$retv, "100L")
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
        expect_equal(trace$args, "list(call=list(4L,3L,2L),vals=list())")
        expect_equal(trace$retv, "10L")
    }
    expect_equal(counter, 1)
})
