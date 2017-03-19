
library(testthat)
library(devtools)
load_all("../..", export_all = FALSE, quiet = TRUE) # genthat

source("./utils.R")

context("Function decoration")

test_that('is_decorated FALSE cases.', {
    expect_false(is_decorated(function() {}))
    expect_false(is_decorated(function() 5))
    expect_false(is_decorated(function(x) x + 1))
    expect_false(is_decorated(function(x) { print(x); x + 1 }))
})

test_that('is_decorated TRUE case', {
    expect_true(is_decorated(decorate_function_val(function() {}, "f_label")))
})

test_that('decorate_function_val__() functionality', {
    fn1 <- function(a, b) { a + b + 1 }

    enter_got_called <- FALSE
    exit_got_called <- FALSE

    fn1 <- genthat:::decorate_function_val__(
        fn1,
        "label1",
        enter_function = function(fname, args, call_id) {
            enter_got_called <<- TRUE
            expect_equal(fname, "label1")
            expect_equal(args, list(a = 4, b = 3))
            expect_equal(call_id, 0)
            TRUE
        },
        exit_function = function(call_id) {
            exit_got_called <<- TRUE
            expect_equal(call_id, 0)
            expect_equal(returnValue(), 8)
        }
    )

    fn1(4, 3)

    expect_true(enter_got_called)
    expect_true(exit_got_called)
})

enterFunction_cpp <- function(name, args, call_id) { .Call("genthat_enterFunction_cpp", PACKAGE = "genthat", name, args, call_id) }
exitFunction_cpp <- function(call_id, retv) { .Call("genthat_exitFunction_cpp", PACKAGE = "genthat", call_id, retv) }

test_that("enterFunction_cpp positive", {
    res <- enterFunction_cpp("fn1", list(a = 42, b = "hey!"), 1)
    expect_equal(res, 0)
})

test_that("enterFunction_cpp unserializable", {
    res <- enterFunction_cpp("fn1", list(x = function() {}), 2) # functions cannot be serialized
    expect_type(res, "list")
    expect_equal(res$type, "error")
    expect_match(res$error_description, "^<unserializable:")
})

test_that("exitFunction_cpp positive", {
    enterFunction_cpp("fn1", list(a = 42L), 35)
    res <- exitFunction_cpp(35, 1337L)
    expect_type(res, "list")
    expect_equal(res$type, "trace")
    expect_equal(res$func, "fn1")
    expect_equal(res$args, "structure(list(a=42L), .Names=\"a\")")
    expect_equal(res$retv, "1337L")
})

test_that("exitFunction_cpp unserializable", {
    enterFunction_cpp("fn1", list(a = 42), 249348)
    res <- exitFunction_cpp(249348, function() {}) # functions cannot be serialized
    expect_type(res, "list")
    expect_equal(res$type, "error")
    expect_match(res$error_description, "^<unserializable:")
})

test_that("exitFunction_cpp non-initialized calls", {
    res <- exitFunction_cpp(103984837, 42L) # functions cannot be serialized
    expect_type(res, "list")
    expect_equal(res$type, "error")
    expect_match(res$error_description, "^<Terminated non-initialized call!>$")
})



# TODO
# test_that('We capture the same calls for testthat:::comparison as base::trace.', {
#     runCompareExamples <- function() { capture.output(suppressWarnings(example(compare))) }
#     countTraces <- function() {
#         # TODO
#     }
# 
#     trace_spy <- get_spy_expression()
#     trace("comparison", trace_spy$expression, where = asNamespace("testthat"))
#     runCompareExamples()
#     untrace("comparison", where = asNamespace("testthat"))
#     expect_true(trace_spy$getCount() > 1)
# 
#     genthat_spy <- get_spy_expression()
#     decorate("comparison", "testthat", enter_function = genthat_spy$fn)
#     runCompareExamples()
#     undecorate("comparison", "testthat")
#     expect_true(genthat_spy$getCount() > 1)
# 
#     expect_equal(genthat_spy$getCount(), trace_spy$getCount())
# })

