context("capture")

test_that("on_function_entry correctly evaluates and stores arguments", {
    on.exit(reset_call_traces())

    var <- 1
    f <- function(x) {
        on_function_entry(1, "f", list(x=x))
    }

    f(var + 2)

    expect_equal(get_call_trace(1), create_trace("f", list(x=serialize_value(3))))
})

test_that("on_function_exit records exit value", {
    on.exit(reset_call_traces())

    set_call_trace(1, create_trace("f", list()))

    f <- function() {
        on_function_exit(1, 2)
    }

    f()

    expect_equal(get_call_trace(1), create_trace("f", list(), retv=serialize_value(2)))
})

test_that("create_trace returns entry trace if no return value is specified", {
    expect_true(methods::is(create_trace(fun="f", args=list("")), "genthat_trace_entry"))
    expect_true(methods::is(create_trace(fun="f", args=list(""), retv=""), "genthat_trace"))
})
