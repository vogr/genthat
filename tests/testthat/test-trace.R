context("traces")

test_that("create entry trace", {
    trace <- create_trace("fun")

    expect_is(trace, "genthat_trace_entry")
    expect_null(trace$retv)
    expect_null(trace$error)
})

test_that("create trace", {
    trace <- create_trace("fun", retv=1L)

    expect_is(trace, "genthat_trace")
    expect_equal(trace$retv, 1L)
    expect_null(trace$error)
})

test_that("create error trace", {
    trace <- create_trace("fun", error=simpleError("an error"))

    expect_is(trace, "genthat_trace_error")
    expect_null(trace$retv)
    expect_equal(trace$error$message, "an error")
})

