
library(testthat)
library(devtools)
load_all("../..", export_all = FALSE) # genthat

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

