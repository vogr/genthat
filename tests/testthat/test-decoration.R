
library(testthat)
library(devtools)

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
    cid <- gen_cid()
    res <- enterFunction_cpp("fn1", list(a = 42, b = "hey!"), cid)
    expect_equal(res, 0)
})

test_that("enterFunction_cpp unserializable", {
    cid <- gen_cid()
    res <- enterFunction_cpp("fn1", list(x = function() {}), cid) # functions cannot be serialized
    expect_type(res, "list")
    expect_equal(res$type, "error")
    expect_match(res$error_description, "^<unserializable:")
})

test_that("exitFunction_cpp positive", {
    cid <- gen_cid()
    enterFunction_cpp("fn1", list(a = 42L), cid)
    res <- exitFunction_cpp(cid, 1337L)
    expect_type(res, "list")
    expect_equal(res$type, "trace")
    expect_equal(res$func, "fn1")
    expect_equal(res$args, "structure(list(a=42L), .Names=\"a\")")
    expect_equal(res$retv, "1337L")
})

test_that("exitFunction_cpp unserializable", {
    cid <- gen_cid()
    enterFunction_cpp("fn1", list(a = 42), cid)
    res <- exitFunction_cpp(cid, function() {}) # functions cannot be serialized
    expect_type(res, "list")
    expect_equal(res$type, "error")
    expect_match(res$error_description, "^<unserializable:")
})

test_that("exitFunction_cpp non-initialized calls", {
    cid <- gen_cid()
    res <- exitFunction_cpp(cid, 42L) # functions cannot be serialized
    expect_type(res, "list")
    expect_equal(res$type, "error")
    expect_match(res$error_description, "^<Terminated non-initialized call!>$")
})

test_that("decorate_function_val()", {
    fn1 <- function() {}
    fn2 <- decorate_function_val(fn1, "fn1_label")
    expect_false(is_decorated(fn1))
    expect_true(is_decorated(fn2))
})

test_that("decorate_function_env()", {
    fn1 <- function() {}
    decorate_function_env("fn1", env = environment())
    expect_true(is_decorated(fn1))
})

test_that("decorate_exported()", {
    load_all("./example-package", TRUE, export_all = FALSE, quiet = TRUE)

    decorate_exported("examplePackage", c("my_add", "public_fn"))

    expect_true(is_decorated(examplePackage::my_add))
    expect_true(is_decorated(examplePackage::public_fn))
    expect_false(is_decorated(examplePackage:::private_fn))
})

test_that("decorate_exported()", {
    load_all("./example-package", TRUE, export_all = FALSE, quiet = TRUE)

    decorate_exported("examplePackage", all = TRUE)

    expect_true(is_decorated(examplePackage::my_add))
    expect_true(is_decorated(examplePackage::public_fn))
    expect_false(is_decorated(examplePackage:::private_fn))
})

test_that("decorate_hidden_functions()", {
    load_all("./example-package", TRUE,  export_all = FALSE, quiet = TRUE)

    decorate_hidden_functions("examplePackage")

    expect_false(is_decorated(examplePackage::my_add))
    expect_false(is_decorated(examplePackage::public_fn))
    expect_true(is_decorated(examplePackage:::private_fn))
})

# test_that('We capture the same calls for testthat:::comparison as base::trace.', {
# 
#     runCompareExamples <- function() { capture.output(suppressWarnings(example(compare))) }
# 
#     countTraces <- function() {
#         x <- 0
#         map_iterator(traces, function(trace) { x <<- x + 1 })
#         return(x)
#     }
# 
#     # base trace
#     trace_spy <- get_spy_expression()
#     trace("comparison", trace_spy$expression, where = asNamespace("testthat"))
#     runCompareExamples()
#     untrace("comparison", where = asNamespace("testthat"))
#     base_trace_count <- trace_spy$getCount()
#     expect_true(base_trace_count > 1)
# 
#     # genthat decorate
#     # TODO this decoration break the runCompareExamples call
#     decorate_exported("testthat", "comparison") 
#     runCompareExamples()
#     undecorate_all()
#     genthat_count <- countTraces()
#     expect_true(genthat_count > 1)
# 
#     expect_equal(genthat_count, base_trace_count)
# })

